/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * Parser Class. Handles verifying grammar of language
 **/
#include "kjlc/parser.h"

#include <iostream>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "kjlc/scanner.h"

namespace kjlc {

// Default Constructor
Parser::Parser(std::string filename, bool parser_debug, bool symbol_debug,
               bool codegen_enable, bool codegen_debug)
    : scanner_(filename),
      symbol_table_(symbol_debug),
      error_state_(false),
      end_parse_(false),
      parser_debug_(parser_debug),
      codegen_(codegen_enable),
      codegen_debug_(codegen_debug),
      llvm_module_(nullptr),
      llvm_context_(), // create a new global context
      llvm_current_procedure_(nullptr),
      llvm_builder_(nullptr) {

}

// Deconstructor
Parser::~Parser() {
  if (llvm_builder_ != nullptr) {
    delete llvm_builder_;
  }
  if (llvm_current_procedure_ != nullptr) {
    delete llvm_current_procedure_;
  }
  if (llvm_module_ != nullptr) {
    delete llvm_module_;
  }
}

void Parser::ParseProgram() {
  // immediately increase the scope on the symbol table for a program level
  // scope
  symbol_table_.IncreaseScope();

  ParseProgramHeader();
  ParseProgramBody();

  // no need to decrease scope because the program is over
}

void Parser::BuildProgram() {
  if (!codegen_) {
    std::cout << "Codegen is not enabled, skipping build." << std::endl;
    return;
  }

  // check the generated module for errors
  bool broken = llvm::verifyModule(*llvm_module_, &llvm::errs());
  
  if (!broken) {
    // TODO:codegen Need to actually figure out my build process
    // Module can be safely compiled, it is not broken
    llvm::legacy::PassManager pass_manager;
    // print IR to stdout
    pass_manager.add(llvm::createPrintModulePass(llvm::outs()));
    // run and send to llvm IR to stdout
    pass_manager.run(*llvm_module_);
  } else {
    std::cout << "Error creating LLVM Module. See stderr." << std::endl;
  }
}

void Parser::DebugPrint(std::string parse_function) {
  if (parser_debug_) {
    std::cout << parse_function << std::endl;
  }
}

void Parser::EmitError(Lexeme lexeme) {
  // don't output if still in an error_state_
  if (error_state_) {
    return;
  }
  std::cout << "Line:" << lexeme.line << " Col:" << lexeme.column << std::endl;
  std::cout << std::endl;
  error_state_ = true;
}

void Parser::EmitError(std::string message, Lexeme lexeme) {
  // don't output if still in an error_state_
  if (error_state_) {
    return;
  }
  std::cout << "Line:" << lexeme.line << " Col:" << lexeme.column;
  std::cout << " - " << message << std::endl;
  std::cout << std::endl;
  error_state_ = true;
}

void Parser::EmitExpectedTokenError(std::string expected_token, Lexeme lexeme) {
  // don't output if still in an error_state_
  if (error_state_) {
    return;
  }
  std::cout << "Line:" << lexeme.line << " Col:" << lexeme.column;
  std::cout << " - " << "Expected token '" << expected_token << "'";
  std::cout << std::endl;
  error_state_ = true;
}

void Parser::EmitExpectedTypeError(std::string expected_type,
                                   std::string found_type,
                                   Lexeme lexeme) {
  // Don't output if still in an error_state_
  if (error_state_) {
    return;
  }
  std::cout << "Line:" << lexeme.line << " Col:" << lexeme.column;
  std::cout << " - " << std::endl;
  std::cout << "Expected Type: " << expected_type << std::endl;
  std::cout << "But found: " << found_type << std::endl;
  error_state_ = true;
}

void Parser::EmitOperationTypeCheckingError(std::string operation,
                                            std::string type1,
                                            std::string type2,
                                            Lexeme lexeme) {
  // don't output if still in an error_state_
  if (error_state_) {
    return;
  }
  std::cout << "Line:" << lexeme.line << " Col:" << lexeme.column << " - ";
  std::cout << "Incompatible types for " << operation << ":";
  std::cout << std::endl;
  std::cout << "Type 1: " << type1 << std::endl;
  std::cout << "Type 2: " << type2 << std::endl;
  std::cout << std::endl;
  error_state_ = true;
}

void Parser::EmitWarning(std::string message, Lexeme lexeme) {
  // don't output warnings in an error_state_
  if (error_state_) {
    return;
  }
  std::cout << "Line:" << lexeme.line << " Col:" << lexeme.column << " - ";
  std::cout << "Warning:" << std::endl;
  std::cout << message << std::endl;
  std::cout << std::endl;
  // don't flip error states for warning
}

// consumes tokens until the next token is in the tokens list or a T_PERIOD
void Parser::ResyncOnTokens(Token tokens[], int tokens_length) {
  DebugPrint("ResyncOnTokens");
  // by nature of getting here, error_state_ should be true
  // set it anyways since it's used as an exit condition
  error_state_ = true;
  Lexeme lexeme;
  while (error_state_) {
    lexeme = scanner_.PeekNextLexeme();
    for (int idx = 0; idx < tokens_length; idx++) {
      if (lexeme.token == tokens[idx]) {
        error_state_ = false;
        break;
      }
    }
    if (lexeme.token == T_PERIOD) {
      // stop consuming,
      error_state_ = false;
      // period denotes either the end of file or a marker that indicates the
      // end of file. Regardless, our parse is over even if there are more
      // tokens
      end_parse_ = true;
    } else {
      // consume the lexeme
      lexeme = scanner_.GetNextLexeme();
    }
  }

}

void Parser::LoopDeclarations(Token end_tokens[], int tokens_length) {
  // Peek the first lexeme to determine if there are any declarations
  Lexeme lexeme = scanner_.PeekNextLexeme(); 
  // boolean to indicate no more declarations
  bool stop = false;

  // check to see if there are any declarations at all
  for (int idx = 0; idx < tokens_length; idx++) {
    if (lexeme.token == end_tokens[idx]) {
      stop = true;
      break;
    }
  }

  // loop parsing declarations until an end token is found
  while (!stop) {
    // There are declarations
    ParseDeclaration();

    if (error_state_) {
      // there was an error down the tree

      // add a semi-colon to the declarations for ResyncOnTokens
      Token tokens[tokens_length + 1];
      for (int idx = 0; idx < tokens_length; idx++) {
        tokens[idx] = end_tokens[idx];
      }
      tokens[tokens_length] = T_SEMI_COLON;
      ResyncOnTokens(tokens, tokens_length + 1);

      // check to see if parse should be ended
      if (end_parse_) {
        return;
      }

      // if recovering from an error, we don't consider the semi colon mandatory
      // but, read it if it's the next token so the next declaration can be read
      // correctly
      lexeme = scanner_.PeekNextLexeme();
      if (lexeme.token == T_SEMI_COLON) {
        lexeme = scanner_.GetNextLexeme();
      }
    } else {
      // all declarations are followed by semi colon
      lexeme = scanner_.GetNextLexeme();
      if (lexeme.token != T_SEMI_COLON) {
        EmitExpectedTokenError(";", lexeme);
        return;
      }
    }

    // peek to see if there are more declarations or if it is an end token
    lexeme = scanner_.PeekNextLexeme();
    for (int idx = 0; idx < tokens_length; idx++) {
      if (lexeme.token == end_tokens[idx]) {
        stop = true;
        break;
      }
    }
  }

}

void Parser::LoopStatements(Token end_tokens[], int tokens_length) {
  // no statements are ever required, so peek before loop to allow for no
  // statements
  Lexeme lexeme = scanner_.PeekNextLexeme();
  bool stop = false;

  // check to see if there are any declarations at all
  for (int idx = 0; idx < tokens_length; idx++) {
    if (lexeme.token == end_tokens[idx]) {
      stop = true;
      break;
    }
  }

  // parse statements until we find the end token
  while (!stop) {
    // there are statements
    ParseStatement();

    if (error_state_) {
      // there was an error down the tree

      // add a semi-colon to the end tokens list for call to ResyncOnTokens
      Token tokens[tokens_length + 1];
      for (int idx = 0; idx < tokens_length; idx++) {
        tokens[idx] = end_tokens[idx];
      }
      tokens[tokens_length] = T_SEMI_COLON;
      ResyncOnTokens(tokens, tokens_length + 1);

      // check to see if parse should be ended
      if (end_parse_) {
        return;
      }

      // if recovering from an error, we don't consider the semi colon mandatory
      // but, read it if it's the next token so the next statement can be read
      // correctly
      lexeme = scanner_.PeekNextLexeme();
      if (lexeme.token == T_SEMI_COLON) {
        lexeme = scanner_.GetNextLexeme();
      }
    } else {
      // all statements are followed by semi colon unless there's an error
      lexeme = scanner_.GetNextLexeme();
      if (lexeme.token != T_SEMI_COLON) {
        EmitExpectedTokenError(";", lexeme);
        return;
      }
    }
    
    // check to see if there is another declaration coming or if it is an
    // end token
    lexeme = scanner_.PeekNextLexeme();
    for (int idx = 0; idx < tokens_length; idx++) {
      if (lexeme.token == end_tokens[idx]) {
        stop = true;
        break;
      }
    }
  }
}

void Parser::ParseIndex(Symbol identifier) {
  // Check for open bracket consume token
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_BRACK_LEFT) {
    EmitExpectedTokenError("[", lexeme);
    return;
  }

  // a bound will evaluate to an integer
  Symbol context = Symbol::GenerateAnonymousSymbol();
  context.SetType(TYPE_INT);
  Symbol bound = ParseExpression(context);
    
  // Type checking

  // an index must be an integer
  if (bound.GetType() != TYPE_INT) {
    // lexeme points to '[' which is okay
    EmitError("Array index must evaluate to integer.", lexeme);
    // return invalid symbol
    return;
  }

  // if there's an index, the indexed symbol must be an array
  if (!identifier.IsArray()) {
    EmitError("Can only index array types.", lexeme);
    // return invalid symbol
    return;
  }

  // ending bracket is required if it was opened
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_BRACK_RIGHT) {
    EmitExpectedTokenError("]", lexeme);
    return;
  }
}

Symbol Parser::CheckExpressionParseTypes(Symbol type_context,
                                         Symbol arith_op,
                                         Symbol expression_tail,
                                         Lexeme location,
                                         bool not_operation) {
  // is the tail a valid symbol?
  if (expression_tail.IsValid()) {
    // Generate an anonymous symbol to return
    Symbol symbol = Symbol::GenerateAnonymousSymbol();
    
    bool compatible = false;
    std::string operation_type_string;
    if (type_context.GetType() == TYPE_BOOL) {
      // expected output is a bool so the expression operator ('&', '|', 'not')
      // is a logical operation
      // logical operation is only supported between bools
      operation_type_string = "logical operation";
      compatible = (arith_op.GetType() == TYPE_BOOL &&
                    expression_tail.GetType() == TYPE_BOOL);
      // output type is boolean because that's what's expected
      symbol.SetType(TYPE_BOOL);
    } else if (type_context.GetType() == TYPE_INT ||
               type_context.GetType() == TYPE_FLOAT) {
      // allow for compatibility between ints floats
      // expected output is a int so it's a binary operator
      // binary operators are only compatible between int types
      compatible = (arith_op.GetType() == TYPE_INT &&
                    expression_tail.GetType() == TYPE_INT);  
      operation_type_string = "binary operation";
      // output type is int because that's what's expected
      // even if a float is expected, output a int and let where the float
      // is expected do the conversion
      symbol.SetType(TYPE_INT);
    } else {
      // other types aren't allowed
      EmitError("Invalid type in expression.", location);
      symbol.SetIsValid(false);
      return symbol;
    }
     

    if (!compatible) {
      EmitOperationTypeCheckingError(operation_type_string,
                                     Symbol::GetTypeString(arith_op),
                                     Symbol::GetTypeString(expression_tail),
                                     location);
      symbol.SetIsValid(false);
      return symbol;
    }
    
    return symbol;
  } else {
    // if there's a not operation, we need to check the single operand
    if (not_operation) {
      bool compatible = false;
      if (type_context.GetType() == TYPE_BOOL) {
        // logical not only valid on bool
        compatible = (arith_op.GetType() == TYPE_BOOL);
      } else if (type_context.GetType() == TYPE_INT ||
                 type_context.GetType() == TYPE_FLOAT) {
        // logical not only valid on int
        compatible = (arith_op.GetType() == TYPE_INT);
      }
      if (!compatible) {
        // can't NOT operate on arith_op
        EmitOperationTypeCheckingError("binary operation",
                                       Symbol::GetTypeString(arith_op),
                                       "N/A",
                                       location);
        // Generate anonymous symbol and return it
        Symbol symbol = Symbol::GenerateAnonymousSymbol();
        symbol.SetIsValid(false);
        return symbol;
      }
    }
    // the tail was not a valid symbol, return the arith_op lead as is
    return arith_op;
  }
}

Symbol Parser::CheckRelationParseTypes(Symbol type_context, Symbol term,
                                       Symbol relation_tail,
                                       Lexeme location, bool equality_test) {
  // is the relation_tail a valid symbol?
  if (relation_tail.IsValid()) {
    // Generate an anonymous symbol to return
    Symbol symbol = Symbol::GenerateAnonymousSymbol();

    bool compatible = false;

    if (term.GetType() == TYPE_BOOL) {
      compatible = (relation_tail.GetType() == TYPE_BOOL ||
                    relation_tail.GetType() == TYPE_INT);
    } else if (term.GetType() == TYPE_FLOAT) {
      compatible = (relation_tail.GetType() == TYPE_FLOAT ||
                    relation_tail.GetType() == TYPE_INT);
    } else if (term.GetType() == TYPE_INT) {
      compatible = (relation_tail.GetType() == TYPE_INT ||
                    relation_tail.GetType() == TYPE_BOOL ||
                    relation_tail.GetType() == TYPE_FLOAT);
    } else if (term.GetType() == TYPE_STRING) {
      compatible = (equality_test && relation_tail.GetType() == TYPE_STRING);
    }
    
    if (!compatible) {
      EmitOperationTypeCheckingError("relational operation",
                                     Symbol::GetTypeString(term),
                                     Symbol::GetTypeString(relation_tail),
                                     location);
      symbol.SetIsValid(false);
      return symbol;
    }

    // relational operations always return a boolean
    symbol.SetType(TYPE_BOOL);

    return symbol;
  } else {
    // relation tail was invalid, so just return the term
    // it can be whatever type it wants to be :)
    return term;
  }
}

Symbol Parser::CheckArithmeticParseTypes(Symbol type_context, Symbol lead,
                                         Symbol tail, Lexeme location) {
  // is tail a valid Symbol?
  if (tail.IsValid()) {
    // Generate an anonymous symbol to return
    Symbol symbol = Symbol::GenerateAnonymousSymbol();

    // do a type check between the results
    bool compatible = false;
    if (lead.GetType() == TYPE_INT) {
      compatible = (tail.GetType() == TYPE_INT || tail.GetType() == TYPE_FLOAT);
    } else if (lead.GetType() == TYPE_FLOAT) {
      compatible = (tail.GetType() == TYPE_INT || tail.GetType() == TYPE_FLOAT);
    }
    
    if (!compatible) {
      EmitOperationTypeCheckingError("Arithmetic",
                                     Symbol::GetTypeString(lead),
                                     Symbol::GetTypeString(tail),
                                     location);
      symbol.SetIsValid(false);
      return symbol;
    }

    // cast the output to the type context
    if (type_context.GetType() == TYPE_FLOAT) {
      symbol.SetType(TYPE_FLOAT);
    } else if(type_context.GetType() == TYPE_INT) {
      symbol.SetType(TYPE_INT);
    } else {
      // arithmetic operation in a context that isn't clear what our output
      // type should be. Favor float if one of the types was a float
      if (lead.GetType() == TYPE_FLOAT || tail.GetType() == TYPE_FLOAT) {
        symbol.SetType(TYPE_FLOAT);
      } else {
        symbol.SetType(TYPE_INT);
      }
    }

    // return an anonymous symbol of the operation return type
    return symbol;
  } else {
    // tail was not a valid symbol, so just return the lead symbol
    // allow it to be whatever type it is
    return lead;
  }
}

llvm::Type* Parser::GetRespectiveLLVMType(Symbol symbol) {
  switch(symbol.GetType()) {
    case TYPE_BOOL:
      return llvm_builder_->getInt1Ty();
      break;
    case TYPE_INT:
      return llvm_builder_->getInt32Ty();
      break;
    case TYPE_FLOAT:
      return llvm_builder_->getFloatTy();
      break;
    case TYPE_STRING:
      // strings are implemented as an array of 8 bit integers
      // optional AS param, default is = 0
      return llvm_builder_->getInt8PtrTy();
      break;
    default:
      std::cout << "Unknown type to convert to LLVM." << std::endl;
      return llvm::IntegerType::getInt1Ty(llvm_module_->getContext());
  }
}

void Parser::ParseArgumentList(std::vector<Symbol>::iterator param_current,
                               std::vector<Symbol>::iterator param_end) {
  DebugPrint("ArgumentList");

  // The type expectation is the current parameter
  Symbol expression = ParseExpression(*param_current);

  // peek a lexeme for possible error reporting
  Lexeme lexeme = scanner_.PeekNextLexeme();

  // check that the expression matches
  if (param_current != param_end) {
    if (param_current->GetType() != expression.GetType()) {
      // argument types don't match
      EmitExpectedTypeError(Symbol::GetTypeString(*param_current),
                            Symbol::GetTypeString(expression),
                            lexeme);
      return; 
    }
  } else {
    // the param_current is the end, there should be no more arguments
    EmitError("Argument miss match, too many arguments.", lexeme);
    return;
  }


  // more arguments are optional and indicated by a comma
  lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_COMMA) {
    // consume comma
    lexeme = scanner_.GetNextLexeme();

    // parse the next arguments recursively
    ParseArgumentList(std::next(param_current), param_end);
  } else {
    // it's the end of the argument list
    if (param_current != param_end) {
      if (std::next(param_current) != param_end) {
        // there are more argument that should be present
        EmitError("Not enough arugments for procedure call", lexeme);
        return;
      }
    }
  }
}

void Parser::ParseAssignmentStatement() {
  DebugPrint("AssignmentStatement");

  Symbol destination = ParseDestination();

  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_COL_EQ) {
    EmitExpectedTokenError(":=", lexeme);
    return;
  }

  // the expected type is the destination symbol
  Symbol expression = ParseExpression(destination);

  // destination and expression evaluation must generally be of the same type
  // but ints and bools are interoperable
  // ints and floats are interoperable
  if (destination.GetType() != expression.GetType()) {
    // they do not match
    bool mismatch = true;
    // check that the types are interoperable
    if (destination.GetType() == TYPE_BOOL &&
        expression.GetType() == TYPE_INT) {
      mismatch = false;
      EmitWarning("Coercing int to bool.", lexeme);
    }

    if (destination.GetType() == TYPE_INT) {
      if (expression.GetType() == TYPE_BOOL) {
        mismatch = false;
        EmitWarning("Coercing bool to int.", lexeme); 
      } else if (expression.GetType() == TYPE_FLOAT) {
        mismatch = false;
        EmitWarning("Coercing float to int.", lexeme);
      }
    }

    if (destination.GetType() == TYPE_FLOAT &&
        expression.GetType() == TYPE_INT) {
      mismatch = false;
      EmitWarning("Coercing int to float.", lexeme);
    }

    // couldn't recover from mismatch via interoperability
    if (mismatch) {
      // lexeme refers to equals sign, which is ideal
      EmitExpectedTypeError(Symbol::GetTypeString(destination),
                            Symbol::GetTypeString(expression),
                            lexeme);
      return;
    }
  }

  if (codegen_) {
    // TODO:codegen - this is incomplete, I have no type checking
    // we know what the destination is, update the symbol value to be
    // the expression value
    // TODO:codegen this doesn't consider arrays
    destination.SetLLVMValue(expression.GetLLVMValue());

    // update the symbol table entry for the destination
    symbol_table_.InsertSymbol(destination);
  }
}

// this is a left recursive rule that has been modified to be right recursive
// see docs/language-grammar-modified.txt for more information
Symbol Parser::ParseArithOp(Symbol type_context) {
  DebugPrint("ArithOp");

  Symbol relation = ParseRelation(type_context);

  // Peek location for use in possible error messages
  Lexeme lexeme = scanner_.PeekNextLexeme();

  Symbol arith_op_tail = ParseArithOpTail(type_context);

  return CheckArithmeticParseTypes(type_context, relation, arith_op_tail,
                                   lexeme);
}

// because this is a left recursive rule made right recursive, it is necessary
// to support empty evaluations
Symbol Parser::ParseArithOpTail(Symbol type_context) {
  DebugPrint("ArithOpTail");

  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_PLUS || lexeme.token == T_MINUS) {
    // consume the "+" or "-"
    lexeme = scanner_.GetNextLexeme();

    Symbol relation = ParseRelation(type_context);

    // peek the lexeme location for possible error messages
    Lexeme lexeme = scanner_.PeekNextLexeme();

    // recursive call
    Symbol arith_op_tail = ParseArithOpTail(type_context);

    return CheckArithmeticParseTypes(type_context, relation, arith_op_tail,
                                     lexeme);
  }

  // Empty evaluation of the rule, return an invalid symbol
  Symbol symbol = Symbol::GenerateAnonymousSymbol();
  symbol.SetIsValid(false);
  return symbol;
}

void Parser::ParseBound(Symbol &symbol) {
  DebugPrint("Bound");

  bool negative = false;
  // peek for '-'
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_MINUS) {
    // consume the dash 
    lexeme = scanner_.GetNextLexeme();
    negative = true;
  }

  lexeme = scanner_.PeekNextLexeme();
  // next lexeme must be an integer - floats are not allowed
  if (lexeme.token != T_INT_LITERAL) {
    EmitExpectedTokenError("int literal", lexeme);
    return;
  }
  
  Symbol bound = ParseNumber();

  // safe because it was checked to be int
  int bound_value = lexeme.int_value;

  if (negative) {
    bound_value = bound_value * -1;
  }
  symbol.SetArrayBound(bound_value);
}

void Parser::ParseDeclaration() {
  DebugPrint("Declaration");
  // Declarations are going to declare a symbol so create it
  Symbol symbol;

  // peek because the first terminal could be a number of things, some of
  // which involve reaching down rule evaluations
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_GLOBAL) {
    // there's a global keyword
    // consume the token 
    lexeme = scanner_.GetNextLexeme();
    
    // mark created symbol as global
    symbol.SetIsGlobal(true);
    
    // now prepare lexeme for guessing the next rule
    lexeme = scanner_.PeekNextLexeme();
  }

  // Rule can evaluate to three different rules, so check their FIRST sets
  if (lexeme.token == T_PROCEDURE) {
    // Procedure Declaration rule
    ParseProcedureDeclaration(symbol);
  } else if (lexeme.token == T_VARIABLE) {
    // Variable declaration rule
    ParseVariableDeclaration(symbol);
  } else if (lexeme.token == T_TYPE) {
    // Type declaration rule
    ParseTypeDeclaration(symbol);
  } else {
    EmitError("Could not parse declaration - expected a variable, "
              "type, or procedure", lexeme);
    return;
  }
}

Symbol Parser::ParseDestination() {
  DebugPrint("Destination");

  // peek the lexeme for possible error reporting on the identifier
  Lexeme lexeme = scanner_.PeekNextLexeme();

  std::string id = ParseIdentifier();
  
  Symbol symbol = symbol_table_.FindSymbolByIdentifier(id);

  if (!symbol.IsValid()) {
    // symbol couldn't be looked up
    EmitError("Could not find symbol: " + id, lexeme); 
    // symbol is already invalid, can just report it
    return symbol;
  }

  // Destination needs to be a variable
  if (symbol.GetDeclaration() != DECLARATION_VARIABLE) {
    // symbol isn't a variable which is a problem
    EmitError("Destination must be a variable.", lexeme);
    // return an invalid symbol
    symbol = Symbol::GenerateAnonymousSymbol();
    symbol.SetIsValid(false);
    return symbol;
  }

  // peek to see if there are brackets for an index
  lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_BRACK_LEFT) {
    
    // Parse the index, since there is one -> [ <expression> ]
    ParseIndex(symbol);
  }

  return symbol;
}

// this is a left recursive rule made right recursive
// see `docs/language-gramamr-modified` for notes on the rules
Symbol Parser::ParseExpression(Symbol type_context) {
  DebugPrint("Expression");

  bool not_operation = false;
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_NOT) {
    // consume "not"
    lexeme = scanner_.GetNextLexeme();
    not_operation = true;
  }

  // parse arithOp
  Symbol arith_op = ParseArithOp(type_context);

  // peek location of next symbol for possible error reporting
  lexeme = scanner_.PeekNextLexeme();

  Symbol expression_tail = ParseExpressionTail(type_context);

  return CheckExpressionParseTypes(type_context, arith_op, expression_tail,
                                   lexeme, not_operation);
}

Symbol Parser::ParseExpressionTail(Symbol type_context) {
  DebugPrint("ExpressionTail");

  // there is an lambda(empty) evaluation of this rule, so start with a peek
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_AND || lexeme.token == T_BAR) {
    // consume the token, not empty evaluation
    lexeme = scanner_.GetNextLexeme();
    
    // Next is arith op
    Symbol arith_op = ParseArithOp(type_context);

    // peek location of next symbol for possible error reporting
    lexeme = scanner_.PeekNextLexeme();

    // Right recursive call
    Symbol expression_tail = ParseExpressionTail(type_context);

    // _not_ only leads in expression, so not_operation param will always
    // be false in ParseExpressionTail
    return CheckExpressionParseTypes(type_context, arith_op, expression_tail,
                                     lexeme, false);
  }

  // empty evaluation of this rule, return an invalid anonymous symbol
  Symbol symbol = Symbol::GenerateAnonymousSymbol();
  symbol.SetIsValid(false);
  return symbol;
}

Symbol Parser::ParseFactor(Symbol type_context) {
  DebugPrint("Factor");
  
  // Initially create an anonymous symbol for return - will be replaced if
  // it's not an anonymous symbol later
  Symbol symbol = Symbol::GenerateAnonymousSymbol();

  // Need to use first sets to determine evaluation, so peek
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_PAREN_LEFT) {
    // ( <expression> ) evaluation
    // consume paren
    lexeme = scanner_.GetNextLexeme();
    
    // TODO: Come back here to get the context
    symbol = ParseExpression(type_context);

    // closing paren is required
    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_PAREN_RIGHT) {
      EmitExpectedTokenError(")", lexeme);
      symbol.SetIsValid(false);
      return symbol;
    }
  } else if (lexeme.token == T_ID) {
    // could be a procedure call or name reference
    symbol = ParseReference();
  } else if (lexeme.token == T_MINUS) {
    // consume T_MINUS
    lexeme = scanner_.GetNextLexeme();

    // Parse Name or Number depending on next token
    lexeme = scanner_.PeekNextLexeme();
    if (lexeme.token == T_INT_LITERAL || lexeme.token == T_FLOAT_LITERAL) {
      // number
      symbol = ParseNumber();
    } else if (lexeme.token == T_ID) {
      // name reference
      symbol = ParseReference();
    } else {
      EmitError("Expected numeric literal or identifier reference", lexeme);
      symbol.SetIsValid(false);
      return symbol;
    }
  } else if (lexeme.token == T_INT_LITERAL || lexeme.token == T_FLOAT_LITERAL) {
    // number
    symbol = ParseNumber();
  } else if (lexeme.token == T_STRING_LITERAL) {
    symbol = ParseString();
  } else if (lexeme.token == T_TRUE) {
    // consume true
    symbol.SetType(TYPE_BOOL);
    lexeme = scanner_.GetNextLexeme();
    
    // build the constant for codegen
    if (codegen_) {
      llvm::Value *constant = llvm::ConstantInt::getIntegerValue(
          GetRespectiveLLVMType(symbol),
          llvm::APInt(1, 1, true)); // 1==true
      symbol.SetLLVMValue(constant);
    }
  } else if (lexeme.token == T_FALSE) {
    // consume false
    symbol.SetType(TYPE_BOOL);
    lexeme = scanner_.GetNextLexeme();

    // build the constant for codegen
    if (codegen_) {
      llvm::Value *constant = llvm::ConstantInt::getIntegerValue(
          GetRespectiveLLVMType(symbol),
          llvm::APInt(1, 0, true)); // 0==false
      symbol.SetLLVMValue(constant);
    }
  } else {
    EmitError("Exected valid factor", lexeme);
    symbol.SetIsValid(false);
    return symbol;
  }
  return symbol;
}

std::string Parser::ParseIdentifier() {
  DebugPrint("Identifier");

  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_ID) {
    EmitError("Expected Identifier", lexeme);
    return std::string();
  }

  // return the string value of the identifier, which will be what the
  // identifier is
  return lexeme.str_value;
}

void Parser::ParseIfStatement() {
  DebugPrint("IfStatement");

  Lexeme lexeme = scanner_.GetNextLexeme(); 
  if (lexeme.token != T_IF) {
    EmitExpectedTokenError("if", lexeme);
    return;
  }

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PAREN_LEFT) {
    EmitExpectedTokenError("(", lexeme);
    return;
  }

  // handle parsing the expression
  // Expected type is a boolean
  Symbol context = Symbol::GenerateAnonymousSymbol();
  context.SetType(TYPE_BOOL);
  Symbol expression = ParseExpression(context);

  // expression must evaluate to a boolean
  // lexeme points to '(' which is okay
  if (expression.GetType() == TYPE_INT) {
    EmitWarning("If statement conditional evaluates to integer. "
                "Will be cast to bool.", lexeme);
  } else if (expression.GetType() != TYPE_BOOL) {
    EmitError("If statement conditional must evaluate to bool or int.", lexeme);
    return;
  }

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PAREN_RIGHT) {
    EmitExpectedTokenError(")", lexeme);
    return;
  }

  lexeme = scanner_.GetNextLexeme(); 
  if (lexeme.token != T_THEN) {
    EmitExpectedTokenError("then", lexeme);
    return;
  }

  // Loop statements until one of the end tokens is found
  Token end_tokens[] = {T_ELSE, T_END};
  int tokens_length = 2;
  LoopStatements(end_tokens, tokens_length);

  // the last lexeme was only peeked, read it now
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token == T_ELSE) {

    // Loop statements until the end token is found
    Token else_end_tokens[] = {T_END};
    tokens_length = 1;
    LoopStatements(else_end_tokens, tokens_length);

    // consume the token that was only peeked so that end can proceed
    // as if else wasn't there
    lexeme = scanner_.GetNextLexeme();
  }

  // now at end
  if (lexeme.token != T_END) {
    EmitExpectedTokenError("end", lexeme);
    return;
  }

  // 'if'
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_IF) {
    EmitExpectedTokenError("if", lexeme);
    return;
  }
}

void Parser::ParseLoopStatement() {
  DebugPrint("LoopStatement");

  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_FOR) {
    EmitExpectedTokenError("for", lexeme);
    return;
  }

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PAREN_LEFT) {
    EmitExpectedTokenError("(", lexeme);
    return;
  }

  ParseAssignmentStatement();

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_SEMI_COLON) {
    EmitExpectedTokenError(";", lexeme);
    return;
  }

  // Expected type is a boolean
  Symbol context = Symbol::GenerateAnonymousSymbol();
  context.SetType(TYPE_BOOL);
  Symbol expression = ParseExpression(context);

  // expression must evaluate to a bool or int
  // lexeme points to ';' which is okay
  if (expression.GetType() == TYPE_INT) {
    EmitWarning("Expression in loop evaluates to integer. "
                "Will be cast to bool.",
                lexeme);
  } else if (expression.GetType() != TYPE_BOOL) {
    EmitError("Expression on loop must evaluate to bool or int.", lexeme);
    return;
  }

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PAREN_RIGHT) {
    EmitExpectedTokenError(")", lexeme);
    return;
  }

  // Loop statements until the end token is found
  Token end_tokens[] = {T_END};
  int tokens_length = 1;
  LoopStatements(end_tokens, tokens_length);

  // consume the end for real
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_END) {
    EmitExpectedTokenError("end", lexeme);
    return;
  }

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_FOR) {
    EmitExpectedTokenError("for", lexeme);
    return;
  }
}

Symbol Parser::ParseNumber() {
  DebugPrint("Number");

  Symbol symbol = Symbol::GenerateAnonymousSymbol();

  // consume number token
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_INT_LITERAL && lexeme.token != T_FLOAT_LITERAL) {
    EmitError("Expected numeric literal", lexeme);
    symbol.SetIsValid(false);
    return symbol;
  }

  // Check to see if it's an int or a float to update the symbol
  if (lexeme.token == T_INT_LITERAL) {
    symbol.SetType(TYPE_INT);
  } else if (lexeme.token == T_FLOAT_LITERAL) {
    symbol.SetType(TYPE_FLOAT);
  }

  if (codegen_) {
    
    llvm::Value *constant;
    if (lexeme.token == T_INT_LITERAL) {
      constant = llvm::ConstantInt::getIntegerValue(
          GetRespectiveLLVMType(symbol), // always use function to get type
          llvm::APInt(32, lexeme.int_value, true));
    } else if (lexeme.token == T_FLOAT_LITERAL) {
      constant = llvm::ConstantFP::get(
          GetRespectiveLLVMType(symbol), // use single type def defined in func
          llvm::APFloat(lexeme.float_value));
    }
    symbol.SetLLVMValue(constant);
  }
  return symbol;
}

void Parser::ParseParameter(Symbol &procedure_symbol) {
  Symbol symbol;
  ParseVariableDeclaration(symbol); 
  procedure_symbol.GetParams().push_back(symbol);
}

void Parser::ParseParameterList(Symbol &procedure_symbol) {
  ParseParameter(procedure_symbol);

  // check to see if there are multiple parameters - indicated by ','
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_COMMA) {
    // consume the comma 
    lexeme = scanner_.GetNextLexeme();

    // recursive call to read more parameters
    ParseParameterList(procedure_symbol);
  }
}

void Parser::ParseProcedureBody() {
  DebugPrint("ProcedureBody");
  
  // Loop declarations until begin is found
  Token end_tokens[] = {T_BEGIN};
  int tokens_length = 1;
  LoopDeclarations(end_tokens, tokens_length);
  
  if (codegen_) {
    //
    // create a function definition
    // Do this when entering the body statements so that it isn't overwritten
    // by new functions being declared
    //
    Symbol procedure_symbol = symbol_table_.GetScopeProcedure();
    
    // Go through the list of arguments and create params
    std::vector<llvm::Type *> params;
    for (Symbol symbol : procedure_symbol.GetParams()) {
      params.push_back(GetRespectiveLLVMType(symbol));
    }

    // define the types of the function
    llvm::FunctionType *functionType = llvm::FunctionType::get(
        GetRespectiveLLVMType(procedure_symbol), // return type
        params, // list of args
        false); // is varargs - always no our language doesn't support
        
    // TODO:codegen - need to add a consistent character to avoid
    // name conflicts of "main" - if someone creates a function
    // "main" that will kill it. Alternatively, disallow "main" as procedure id
    // Actually create the function
    llvm::Constant *procedure = llvm_module_->getOrInsertFunction(
        procedure_symbol.GetId(), // name of function
        functionType);

    // cast it to a function and insert it into the current procedure member var
    llvm_current_procedure_ = llvm::cast<llvm::Function>(procedure);

    // set the calling convention of our procedure to that of a C program
    llvm_current_procedure_->setCallingConv(llvm::CallingConv::C);

    // generate the starting block
    llvm::BasicBlock *procedure_entrypoint = llvm::BasicBlock::Create(
        llvm_context_,
        "entrypoint", // first block is always entrypoint
        llvm_current_procedure_); // parent function

    llvm_builder_->SetInsertPoint(procedure_entrypoint);

    // create values for arguments and update their symbol table entries
    llvm::Function::arg_iterator args = llvm_current_procedure_->arg_begin();
    for (Symbol symbol : procedure_symbol.GetParams()) {
      if (args == llvm_current_procedure_->arg_end()) {
        // this shouldn't happen, because the args were generated from this
        // same vector, but play it safe
        EmitError("Error generating IR for arguments.",
            scanner_.PeekNextLexeme());
        return;
      }

      // create value from arg
      llvm::Value *val = &*args++;

      // update symbol and reinsert it into the symbol table
      symbol.SetLLVMValue(val);
      symbol_table_.InsertSymbol(symbol);
    }
  }

  // Parse the 'begin'
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_BEGIN) {
    EmitExpectedTokenError("begin", lexeme);
    return;
  }

  // Parse statements until the end is found
  Token statement_end_tokens[] = {T_END};
  tokens_length = 1;
  LoopStatements(statement_end_tokens, 1);

  // Parse 'end'
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_END) {
    EmitExpectedTokenError("end", lexeme);
    return;
  }

  // Parse 'procedure'
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PROCEDURE) {
    EmitExpectedTokenError("procedure", lexeme);
    return;
  }

  if (codegen_) {
    // procedure is over - get the current block and ensure it is well formed
    // because it has a terminator
    if (llvm_builder_->GetInsertBlock()->getTerminator() == NULL) {
      // there is not a terminator, this is an error and should kill
      // compilation, but leave this for testing
      llvm::Constant *val = llvm::ConstantInt::getIntegerValue(
          llvm_builder_->getInt32Ty(),
          llvm::APInt(32, 0, true));
      llvm_builder_->CreateRet(val);
    }
  }
}

void Parser::ParseProcedureDeclaration(Symbol &procedure_symbol) {
  DebugPrint("ProcedureDeclaration");

  // Increase the scope as we enter a new procedure
  symbol_table_.IncreaseScope();

  // mark the current symbol as a procedure
  procedure_symbol.SetDeclaration(DECLARATION_PROCEDURE);

  // Parse the procedure header and continue to build the symbol
  ParseProcedureHeader(procedure_symbol);
  
  // commit the symbol to the symbol table so that the body can reference itself
  symbol_table_.InsertSymbol(procedure_symbol);

  // mark this procedure as the scope procedure 
  symbol_table_.SetScopeProcedure(procedure_symbol);

  // parse the body of the procedure
  ParseProcedureBody();

  // exiting the procedure, decrease the scope
  symbol_table_.DecreaseScope();

  // now insert the symbol into the scope of the declarer so it can be called
  symbol_table_.InsertSymbol(procedure_symbol);
}

void Parser::ParseProcedureHeader(Symbol &procedure_symbol) {
  DebugPrint("ProcedureHeader");

  // parse procedure keyword
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PROCEDURE) {
    EmitExpectedTokenError("procedure", lexeme);
    return;
  } 

  // read the identifier
  std::string id = ParseIdentifier();
  procedure_symbol.SetId(id);


  // colon required
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_COLON) {
    EmitExpectedTokenError(":", lexeme);
    return;
  }

  // type mark
  ParseTypeMark(procedure_symbol);

  // read left paren
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PAREN_LEFT) {
    EmitExpectedTokenError("(", lexeme);
    return;
  }

  // peek to see if there is an optional parameter list using it's FIRST set
  lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_VARIABLE) {
    // read parameter list
    ParseParameterList(procedure_symbol);
  }

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PAREN_RIGHT) {
    EmitExpectedTokenError(")", lexeme);
    return;
  }


}

void Parser::ParseProgramBody() {
  DebugPrint("ProgramBody");

  // Loop declarations until begin is found
  Token end_tokens[] = {T_BEGIN};
  int end_tokens_length = 1;
  LoopDeclarations(end_tokens, end_tokens_length);

  // Parse 'begin'
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_BEGIN) {
    EmitExpectedTokenError(";", lexeme);
    return;
  }

  if (codegen_) {
    //
    // create main function so it's available for statement creation 
    //
    
    // create empty param list for main
    std::vector<llvm::Type *> params;

    // function returns i32 exit code
    llvm::FunctionType *functionType = llvm::FunctionType::get(
        llvm_builder_->getInt32Ty(),
        params,
        false);

    llvm::Constant *main = llvm_module_->getOrInsertFunction(
        "main", // always name main function main
        functionType);

    // cast it to a function and store it as current procedure
    llvm_current_procedure_ = llvm::cast<llvm::Function>(main);

    // make it interoperable with C
    llvm_current_procedure_->setCallingConv(llvm::CallingConv::C);

    // generate the starting block
    llvm::BasicBlock *procedure_entrypoint = llvm::BasicBlock::Create(
        llvm_context_,
        "entrypoint", // first block is always entrypoint
        llvm_current_procedure_); // parent function

    llvm_builder_->SetInsertPoint(procedure_entrypoint);
  }

  // parse statements until end is found
  Token statement_end_tokens[] = {T_END};
  end_tokens_length = 1;
  LoopStatements(statement_end_tokens, end_tokens_length);

  // parse 'end'
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_END) {
    EmitExpectedTokenError("end", lexeme);
    return;
  }

  // parse 'program'
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PROGRAM) {
    EmitExpectedTokenError("program", lexeme);
    return;
  }

  if (codegen_) {
    // program is over - get the current block and ensure it is well formed
    // because it has a terminator
    if (llvm_builder_->GetInsertBlock()->getTerminator() == NULL) {
      // there is no terminator - add a return of the correct type
      // exit 0 from the program
      llvm::Constant *val = llvm::ConstantInt::getIntegerValue(
          llvm_builder_->getInt32Ty(),
          llvm::APInt(32, 0, true));
      llvm_builder_->CreateRet(val);
    }
  }
  
  // successfully parsed rule
  return;
}

void Parser::ParseProgramHeader() {
  DebugPrint("ProgramHeader");

  // read 'program'
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PROGRAM) {
    EmitExpectedTokenError("program", lexeme);
    return;
  }

  // read the identifier
  std::string program_name = ParseIdentifier();

  if (codegen_) {
    // Create the module in which all this code will go
    llvm_module_ = new llvm::Module(program_name, llvm_context_);
    // create the builder that references this module
    llvm_builder_ = new llvm::IRBuilder<>(llvm_context_);
  }

  // read 'is'
  lexeme = scanner_.GetNextLexeme(); 
  if (lexeme.token != T_IS) {
    EmitExpectedTokenError("is", lexeme);
    return;
  }

  // successful parse
  return;
}

// this rule replaces ParseName and ParseProcedureCall
Symbol Parser::ParseReference() {
  DebugPrint("Reference");

  // generate an anonymous symbol for return, will be replaced later by real
  // symbol later if applicable
  Symbol symbol = Symbol::GenerateAnonymousSymbol();

  // peek the lexeme for identifier to potentially use it for error reporting
  Lexeme lexeme = scanner_.PeekNextLexeme();
  
  // could be a procedure call or name
  // both start with identifiers
  std::string id = ParseIdentifier();

  // find the symbol
  symbol = symbol_table_.FindSymbolByIdentifier(id);

  // check that a valid identifier was found
  if (!symbol.IsValid()) {
    EmitError("Could not find symbol: " + id, lexeme);
    // symbol is already invalid, return it
    return symbol;
  }

  // this is guaranteed to correctly identify it as a name or procedure call
  // because parens are needed for procedure calls
  lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_PAREN_LEFT) {
    // It is a procedure call
    // consume left paren
    lexeme = scanner_.GetNextLexeme();

    // since it's a procedure call, the symbol must be a procedure
    if (symbol.GetDeclaration() != DECLARATION_PROCEDURE) {
      EmitError("Non procedure cannot be called.", lexeme);
      symbol = Symbol::GenerateAnonymousSymbol();
      symbol.SetIsValid(false);
      return symbol;
    }

    // optionally ParseArgumentList
    // need to peek next token and see if it's in the (quite large) first set
    // for argument_list. See docs for derivation of first set 
    lexeme = scanner_.PeekNextLexeme();
    if (lexeme.token == T_NOT || lexeme.token == T_PAREN_LEFT ||
        lexeme.token == T_MINUS || lexeme.token == T_INT_LITERAL ||
        lexeme.token == T_FLOAT_LITERAL || lexeme.token == T_ID ||
        lexeme.token == T_STRING_LITERAL || lexeme.token == T_TRUE ||
        lexeme.token == T_FALSE) {
      // it is an argument!
      // Parameter list needs checked 
      ParseArgumentList(symbol.GetParams().begin(), symbol.GetParams().end());
    }

    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_PAREN_RIGHT) {
      EmitExpectedTokenError(")", lexeme);
      symbol.SetIsValid(false);
      return symbol;
    }
  } else  {
    // it's a name. Could be indexed. Must be variable
    if (symbol.GetDeclaration() != DECLARATION_VARIABLE) {
      EmitError("Reference to name must be a variable type.", lexeme);
      symbol = Symbol::GenerateAnonymousSymbol();
      symbol.SetIsValid(false);
      return symbol;
    }
    
    if (lexeme.token == T_BRACK_LEFT) {
      // it's a name reference with an index operation
      // parse the index operation
      ParseIndex(symbol);
    }

    if (codegen_) {
      // TODO:codegen - re-enable this once codegen is further along
      //if (symbol.GetLLVMValue() == nullptr) {
      //  EmitError("Attempt to use variable before it's initialized.", lexeme);
      //  symbol.SetIsValid(false);
      //  return symbol;
      //}
    }
  }

  return symbol;
}

// this is a left recursive rule made right recursive
// see docs for full write-out of rule
Symbol Parser::ParseRelation(Symbol type_context) {
  DebugPrint("Relation");

  Symbol term = ParseTerm(type_context);

  // peek the next token to see if there is going to be an equality test
  // needed for type checking, this is reaching down into ParseRelationTail
  // lexeme also used for location in possible error reporting
  bool equality_test = false;
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_EQ || lexeme.token == T_NEQ) {
    equality_test = true;
  }

  Symbol relation_tail = ParseRelationTail(type_context);

  return CheckRelationParseTypes(type_context, term, relation_tail,
                                 lexeme, equality_test);
}

// Right recursive portion of the rule
Symbol Parser::ParseRelationTail(Symbol type_context) {
  DebugPrint("RelationTail");

  // Need to allow for empty evaluation so peek
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_LT || lexeme.token == T_GT_EQ ||
      lexeme.token == T_LT_EQ || lexeme.token == T_GT ||
      lexeme.token == T_EQ || lexeme.token == T_NEQ) {
    // consume the token
    lexeme = scanner_.GetNextLexeme();


    Symbol term = ParseTerm(type_context);

    // check to see if the next token is an equality test
    // needed for type checking
    // this is technically reaching down into the next ParseRelationTail
    // lexeme also used for location in possible error reporting
    lexeme = scanner_.PeekNextLexeme();
    bool equality_test = false;
    if (lexeme.token == T_EQ || lexeme.token == T_NEQ) {
      equality_test = true;
    }

    // Recursive call
    Symbol relation_tail = ParseRelationTail(type_context);

    return CheckRelationParseTypes(type_context, term, relation_tail,
                                   lexeme, equality_test);
  }
  
  // empty evaluation of rule, return invalid anonymous symbol
  Symbol symbol = Symbol::GenerateAnonymousSymbol();
  symbol.SetIsValid(false);
  return symbol;
}

void Parser::ParseReturnStatement() {
  DebugPrint("Return");
  
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_RETURN) {
    EmitExpectedTokenError("return", lexeme);
    return;
  }

  // get the symbol for this scope - it will correspond to the return statement
  Symbol procedure = symbol_table_.GetScopeProcedure();

  // check that a return is allowed, it will be if procedure is valid
  if (!procedure.IsValid()) {
    EmitError("Return not valid in this scope.", lexeme);
    return;
  }

  // return type context is the type of the scope procedure
  Symbol expression = ParseExpression(procedure);

  if (expression.GetType() != procedure.GetType()) {
    EmitExpectedTypeError(Symbol::GetTypeString(procedure),
                          Symbol::GetTypeString(expression),
                          lexeme);
    return;
  }

  if (codegen_) {
    // TODO:codegen - is there type coercien that needs to go down?
    // add llvm return statement
    llvm_builder_->CreateRet(expression.GetLLVMValue());
  }
}

void Parser::ParseStatement() {
  DebugPrint("Statement");

  // Parse statement has four possible evaluations so peek and take the one
  // that we can predict using the respective first sets of the possible
  // rule evaluations
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_ID) {
    ParseAssignmentStatement();
  } else if (lexeme.token == T_IF) {
    ParseIfStatement();
  } else if (lexeme.token == T_FOR) {
    ParseLoopStatement();
  } else if (lexeme.token == T_RETURN) {
    ParseReturnStatement();
  } else {
    EmitError("Expected identifier, if, for, or return", lexeme);
    return;
  }
}

Symbol Parser::ParseString() {
  DebugPrint("String");

  // generate anonymous symbol
  Symbol symbol = Symbol::GenerateAnonymousSymbol();
  symbol.SetType(TYPE_STRING);

  // consume the string token
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_STRING_LITERAL) {
    EmitError("Expected string literal", lexeme);
    return symbol;
  }

  if (codegen_) {
    llvm::Value *constant = llvm_builder_->CreateGlobalStringPtr(
        lexeme.str_value);
    symbol.SetLLVMValue(constant);
  }
  return symbol;
}

// a left recursive rule made right recursive. See docs for writeout
Symbol Parser::ParseTerm(Symbol type_context) {
  DebugPrint("Term");

  Symbol factor = ParseFactor(type_context);

  // peek the location of the operator for possible error reporting
  Lexeme lexeme = scanner_.PeekNextLexeme();

  Symbol term_tail = ParseTermTail(type_context);

  return CheckArithmeticParseTypes(type_context, factor, term_tail, lexeme);

}

// Right recursive version of ParseTerm
Symbol Parser::ParseTermTail(Symbol type_context) {
  DebugPrint("TermTail");

  // peek because there can be an empty evaluation
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_DIV || lexeme.token == T_MULT) {
    // consume the token
    lexeme = scanner_.GetNextLexeme();

    Symbol factor = ParseFactor(type_context);

    // peek the location of the operator for possible error reporting
    Lexeme lexeme = scanner_.PeekNextLexeme();

    Symbol term_tail =  ParseTermTail(type_context);

    return CheckArithmeticParseTypes(type_context, factor, term_tail, lexeme);
  }

  // Empty evaluation of the rule, return an invalid symbol
  Symbol symbol = Symbol::GenerateAnonymousSymbol();
  symbol.SetIsValid(false);
  return symbol;
}

void Parser::ParseTypeDeclaration(Symbol &type_symbol) {
  DebugPrint("TypeDeclaration");

  // mark the symbol as a type declaration
  type_symbol.SetDeclaration(DECLARATION_TYPE);

  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_TYPE) {
    EmitExpectedTokenError("type", lexeme);
    return;
  }

  // Read the identifier
  std::string id = ParseIdentifier();
  type_symbol.SetId(id);

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_IS) {
    EmitExpectedTokenError("is", lexeme);
    return;
  }

  // Parse type mark
  ParseTypeMark(type_symbol);

  // commit the symbol to the symbol table
  symbol_table_.InsertSymbol(type_symbol);
}

void Parser::ParseTypeMark(Symbol &symbol) {
  DebugPrint("TypeMark");

  // Need to peek, because it could be an identifier so we can't consume the
  // token
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_BOOL || lexeme.token == T_FLOAT ||
      lexeme.token == T_INT || lexeme.token == T_STRING ||
      lexeme.token == T_ENUM) {
    // read the token because it's consumed here
    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token == T_BOOL) {
      // boolean
      symbol.SetType(TYPE_BOOL);
    } else if (lexeme.token == T_FLOAT) {
      // float
      symbol.SetType(TYPE_FLOAT);
    } else if (lexeme.token == T_INT) {
      // int
      symbol.SetType(TYPE_INT);
    } else if (lexeme.token == T_STRING) {
      // string
      symbol.SetType(TYPE_STRING);
    } else if (lexeme.token == T_ENUM) {
      // enum
      symbol.SetType(TYPE_ENUM);
      
      // there are additional rules
      lexeme = scanner_.GetNextLexeme();
      if (lexeme.token != T_CURLY_LEFT) {
        EmitExpectedTokenError("{", lexeme);
        return;
      }

      // always at least one identifier
      ParseIdentifier();

      // can be multiple identifiers, indicated by comma before next identifier
      lexeme = scanner_.PeekNextLexeme();
      while (lexeme.token == T_COMMA) {
        // check comma - this is redundant
        lexeme = scanner_.GetNextLexeme();
        if (lexeme.token != T_COMMA) {
          EmitExpectedTokenError(",", lexeme);
          return;
        }

        // there's going to be another identifier
        ParseIdentifier();

        // peek for next token to see if more identifiers
        lexeme = scanner_.PeekNextLexeme();
      }

      lexeme = scanner_.GetNextLexeme();
      if (lexeme.token != T_CURLY_RIGHT) {
        EmitExpectedTokenError("}", lexeme);
        return;
      }
    } // end enum if
  } else if(lexeme.token == T_ID) {
    std::string id = ParseIdentifier();
    
    // The referenced identifier must be a type declaration
    Symbol symbol = symbol_table_.FindSymbolByIdentifier(id);

    if (!symbol.IsValid()) {
      EmitError("Symbol could not be found: " + id, lexeme);
      return;
    }

    if (symbol.GetDeclaration() != DECLARATION_TYPE) {
      EmitError("Type mark must be a declared type or built in.", lexeme);
      return;
    }
  } else {
    EmitError("Expected int, string, bool, enum, float, or identifier",
              lexeme);
    return;
  }
}

void Parser::ParseVariableDeclaration() {
  Symbol symbol;
  ParseVariableDeclaration(symbol);
}

void Parser::ParseVariableDeclaration(Symbol &variable_symbol) {
  DebugPrint("VariableDeclaration");

  // mark the symbol as a variable declaration
  variable_symbol.SetDeclaration(DECLARATION_VARIABLE);

  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_VARIABLE) {
    EmitExpectedTokenError("variable", lexeme);
    return;
  }

  std::string id = ParseIdentifier();
  variable_symbol.SetId(id);

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_COLON) {
    EmitExpectedTokenError(":", lexeme);
  }

  ParseTypeMark(variable_symbol);

  // peek to look for optional bound
  lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_BRACK_LEFT) {
    // there are bounds
    // consume left bracket
    lexeme = scanner_.GetNextLexeme();

    // it is an array, so mark the symbol as such
    variable_symbol.SetIsArray(true);

    ParseBound(variable_symbol);

    // read right bracket
    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_BRACK_RIGHT) {
      EmitExpectedTokenError("]", lexeme);
      return;
    }
  }

  // commit the symbol to the symbol table
  symbol_table_.InsertSymbol(variable_symbol);
}

} // namespace kjlc
