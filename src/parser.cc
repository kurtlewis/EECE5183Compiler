/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * Parser Class. Handles verifying grammar of language
 **/
#include "kjlc/parser.h"

#include <iostream>

#include "kjlc/scanner.h"

namespace kjlc {

// Default Constructor
Parser::Parser(std::string filename, bool parser_debug, bool symbol_debug)
    : scanner_(filename),
      symbol_table_(symbol_debug),
      error_state_(false),
      end_parse_(false),
      debug_(parser_debug) {

}

// Deconstructor
Parser::~Parser() {

}

void Parser::ParseProgram() {
  // immediately increase the scope on the symbol table for a program level
  // scope
  symbol_table_.IncreaseScope();

  ParseProgramHeader();
  ParseProgramBody();

  // no need to decrease scope because the program is over
}

void Parser::DebugPrint(std::string parse_function) {
  if (debug_) {
    std::cout << parse_function << std::endl;
  }
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

void Parser::EmitTypeCheckingError(std::string operation, std::string type1,
                           std::string type2, Lexeme lexeme) {
  // don't output if still in an error_state_
  if (error_state_) {
    return;
  }
  std::cout << "Line:" << lexeme.line << " Col:" << lexeme.column << " - ";
  std::cout << "Incompatible types for " << operation << " operation between:";
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

Symbol Parser::CheckExpressionParseTypes(Symbol arith_op,
                                         Symbol expression_tail,
                                         Lexeme location,
                                         bool not_operation) {
  // is the tail a valid symbol?
  if (expression_tail.IsValid()) {
    // Generate an anonymous symbol to return
    Symbol symbol = Symbol::GenerateAnonymousSymbol();
    
    bool compatible = arith_op.CheckTypesForBinaryOp(expression_tail);

    if (!compatible) {
      EmitTypeCheckingError("binary", Symbol::GetTypeString(arith_op),
                            Symbol::GetTypeString(expression_tail),
                            location);
      symbol.SetIsValid(false);
      return symbol;
    }

    // if input type was TYPE_INT, output is TYPE_INT, or if TYPE_BOOL, output
    // is TYPE_BOOL
    if (arith_op.GetType() == TYPE_INT) {
      symbol.SetType(TYPE_INT);
    } else if (arith_op.GetType() == TYPE_BOOL) {
      symbol.SetType(TYPE_BOOL);
    } else {
      // other types aren't allowed
      EmitError("Invalid type in expression.", location);
      symbol.SetIsValid(false);
      return symbol;
    }

    return symbol;

  } else {
    // if there's a not operation, we need to check the single operand
    if (not_operation && !arith_op.CheckTypeForBinaryOp()) {
      // can't NOT operate on arith_op
      EmitTypeCheckingError("binary", Symbol::GetTypeString(arith_op),
                            "N/A", location);
      // Generate anonymous symbol and return it
      Symbol symbol = Symbol::GenerateAnonymousSymbol();
      symbol.SetIsValid(false);
      return symbol;
    }
    // the tail was not a valid symbol, return the arith_op lead as is
    return arith_op;
  }
}

Symbol Parser::CheckRelationParseTypes(Symbol term, Symbol relation_tail,
                                       Lexeme location, bool equality_test) {
  // is the relation_tail a valid symbol?
  if (relation_tail.IsValid()) {
    // Generate an anonymous symbol to return
    Symbol symbol = Symbol::GenerateAnonymousSymbol();

    bool compatible = term.CheckTypesForRelationalOp(relation_tail,
                                                     equality_test);

    if (!compatible) {
      EmitTypeCheckingError("relational", Symbol::GetTypeString(term),
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

Symbol Parser::CheckArithmeticParseTypes(Symbol lead, Symbol tail,
                                   Lexeme location) {
  // is tail a valid Symbol?
  if (tail.IsValid()) {
    // Generate an anonymous symbol to return
    Symbol symbol = Symbol::GenerateAnonymousSymbol();

    // do a type check between the results
    bool compatible = lead.CheckTypesForArithmeticOp(tail);
    
    if (!compatible) {
      EmitTypeCheckingError("Arithmetic", Symbol::GetTypeString(lead),
                            Symbol::GetTypeString(tail),
                            location);
      symbol.SetIsValid(false);
      return symbol;
    }

    // if one of the symbols was a float, that's the return type, otherwise int
    // safe assumption to make because the <term> grammar is for arithmetic
    // operations, and they are only defined for ints and floats
    // if there was a non int/float, it would be cause by term_tail being false
    if (lead.GetType() == TYPE_FLOAT || tail.GetType() == TYPE_FLOAT) {
      symbol.SetType(TYPE_FLOAT);
    } else {
      symbol.SetType(TYPE_INT);
    }

    // return an anonymous symbol of the operation return type
    return symbol;
  } else {
    // tail was not a valid symbol, so just return the lead symbol
    // allow it to be whatever type it is
    return lead;
  }
}

void Parser::ParseArgumentList() {
  DebugPrint("ArgumentList");

  ParseExpression();

  // more arguments are optional and indicated by a comma
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_COMMA) {
    // consume comma
    lexeme = scanner_.GetNextLexeme();

    // parse the next arguments recursively
    ParseArgumentList();
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

  Symbol expression = ParseExpression();

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
    }

    if (destination.GetType() == TYPE_INT &&
        (expression.GetType() == TYPE_BOOL ||
         expression.GetType() == TYPE_FLOAT)) {
      mismatch = false;
    }

    if (destination.GetType() == TYPE_FLOAT &&
        expression.GetType() == TYPE_INT) {
      mismatch = false;
    }

    // couldn't recover from mismatch via interoperability
    if (mismatch) {
      // lexeme refers to equals sign, which is ideal
      EmitError("Destination and expression types do not match.", lexeme);
      return;
    }
  }
}

// this is a left recursive rule that has been modified to be right recursive
// see docs/language-grammar-modified.txt for more information
Symbol Parser::ParseArithOp() {
  DebugPrint("ArithOp");

  Symbol relation = ParseRelation();

  // Peek location for use in possible error messages
  Lexeme lexeme = scanner_.PeekNextLexeme();

  Symbol arith_op_tail = ParseArithOpTail();

  return CheckArithmeticParseTypes(relation, arith_op_tail, lexeme);
}

// because this is a left recursive rule made right recursive, it is necessary
// to support empty evaluations
Symbol Parser::ParseArithOpTail() {
  DebugPrint("ArithOpTail");

  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_PLUS || lexeme.token == T_MINUS) {
    // consume the "+" or "-"
    lexeme = scanner_.GetNextLexeme();

    Symbol relation = ParseRelation();

    // peek the lexeme location for possible error messages
    Lexeme lexeme = scanner_.PeekNextLexeme();

    // recursive call
    Symbol arith_op_tail = ParseArithOpTail();

    return CheckArithmeticParseTypes(relation, arith_op_tail, lexeme);
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

  int bound = ParseNumberInteger();
  if (negative) {
    bound = bound * -1;
  }
  symbol.SetArrayBound(bound);
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

  // peek to see if there are brackets
  lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_BRACK_LEFT) {
    // consume token
    lexeme = scanner_.GetNextLexeme();

    Symbol bound = ParseExpression();
    
    // Type checking
    // TODO:TypeCheck reduce code duplication for array index type checking

    // create invalid symbol for returning in case of errors
    Symbol invalid = Symbol::GenerateAnonymousSymbol();
    invalid.SetIsValid(false);

    // an index must be an integer
    if (bound.GetType() != TYPE_INT) {
      EmitError("Array index must evaluate to integers.", lexeme);
      // return invalid symbol
      return invalid;
    }

    // if there's an index, the indexed symbol must be an array
    if (!symbol.IsArray()) {
      EmitError("Can only index array types.", lexeme);
      // return invalid symbol
      return invalid;
    }
    // TODO:TypeCheck do I need to make a specific symbol for the single 
    // location this is going to if it's an item in an array? I don't know at
    // this point

    // ending bracket is required if it was opened
    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_BRACK_RIGHT) {
      EmitExpectedTokenError("]", lexeme);
      return invalid;
    }
  }

  return symbol;
}

// this is a left recursive rule made right recursive
// see `docs/language-gramamr-modified` for notes on the rules
Symbol Parser::ParseExpression() {
  DebugPrint("Expression");

  bool not_operation = false;
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_NOT) {
    // consume "not"
    lexeme = scanner_.GetNextLexeme();
    not_operation = true;
  }

  // parse arithOp
  Symbol arith_op = ParseArithOp();

  // peek location of next symbol for possible error reporting
  lexeme = scanner_.PeekNextLexeme();

  Symbol expression_tail = ParseExpressionTail();

  return CheckExpressionParseTypes(arith_op, expression_tail, lexeme,
                                   not_operation);
}

Symbol Parser::ParseExpressionTail() {
  DebugPrint("ExpressionTail");

  // there is an lambda(empty) evaluation of this rule, so start with a peek
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_AND || lexeme.token == T_BAR) {
    // consume the token, not empty evaluation
    lexeme = scanner_.GetNextLexeme();
    
    // Next is arith op
    Symbol arith_op = ParseArithOp();

    // peek location of next symbol for possible error reporting
    lexeme = scanner_.PeekNextLexeme();

    // Right recursive call
    Symbol expression_tail = ParseExpressionTail();

    return CheckExpressionParseTypes(arith_op, expression_tail, lexeme, false);
  }

  // empty evaluation of this rule, return an invalid anonymous symbol
  Symbol symbol = Symbol::GenerateAnonymousSymbol();
  symbol.SetIsValid(false);
  return symbol;
}

Symbol Parser::ParseFactor() {
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
    
    symbol = ParseExpression();

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
      // Check to see if it's an int or a float to update the symbol
      if (lexeme.token == T_INT_LITERAL) {
        symbol.SetType(TYPE_INT);
      } else if (lexeme.token == T_FLOAT_LITERAL) {
        symbol.SetType(TYPE_FLOAT);
      }
      ParseNumber();
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
    // Check to see if it's an int or a float to update the symbol
    if (lexeme.token == T_INT_LITERAL) {
      symbol.SetType(TYPE_INT);
    } else if (lexeme.token == T_FLOAT_LITERAL) {
      symbol.SetType(TYPE_FLOAT);
    }
    ParseNumber();
  } else if (lexeme.token == T_STRING_LITERAL) {
    symbol.SetType(TYPE_STRING);
    ParseString();
  } else if (lexeme.token == T_TRUE) {
    // consume true
    symbol.SetType(TYPE_BOOL);
    lexeme = scanner_.GetNextLexeme();
  } else if (lexeme.token == T_FALSE) {
    // consume false
    symbol.SetType(TYPE_BOOL);
    lexeme = scanner_.GetNextLexeme();
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
  Symbol expression = ParseExpression();

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

  Symbol expression = ParseExpression();

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

void Parser::ParseNumber() {
  DebugPrint("Number");

  // consume number token
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_INT_LITERAL && lexeme.token != T_FLOAT_LITERAL) {
    EmitError("Expected numeric literal", lexeme);
    return;
  }
}

float Parser::ParseNumberFloat() {
  DebugPrint("NumberFloat");

  // consume number token, but expect it to be a float
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_FLOAT_LITERAL) {
    EmitError("Expected float literal", lexeme);
    return 0.0;
  }
  return lexeme.float_value;
}

int Parser::ParseNumberInteger() {
  DebugPrint("NumberInteger");

  // consume number token, but expect it to be an integer
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_INT_LITERAL) {
    EmitError("Expected integer literal", lexeme);
    return 0;
  }
  return lexeme.int_value;
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
  ParseIdentifier();

  // read 'is'
  lexeme = scanner_.GetNextLexeme(); 
  if (lexeme.token != T_IS) {
    EmitExpectedTokenError("is", lexeme);
    return;
  }

  // successful parse
  return;
}

// TODO:TypeCheck
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
      ParseArgumentList();
    }

    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_PAREN_RIGHT) {
      EmitExpectedTokenError(")", lexeme);
      symbol.SetIsValid(false);
      return symbol;
    }
  } else if (lexeme.token == T_BRACK_LEFT) {
    // it's a name reference
    // consume left bracket
    lexeme = scanner_.GetNextLexeme();

    Symbol bound = ParseExpression();
    
    // Type checking
    // TODO:TypeCheck - reduce redundant code for parsing indices

    // create invalid symbol for returning in case of errors
    Symbol invalid = Symbol::GenerateAnonymousSymbol();
    invalid.SetIsValid(false);

    // an index must be an integer
    if (bound.GetType() != TYPE_INT) {
      EmitError("Array index must evaluate to integers.", lexeme);
      // return invalid symbol
      return invalid;
    }

    // if there's an index, the indexed symbol must be an array
    if (!symbol.IsArray()) {
      EmitError("Can only index array types.", lexeme);
      // return invalid symbol
      return invalid;
    }
    // TODO:TypeCheck do I need to make a specific symbol for the single 
    // location this is going to if it's an item in an array? I don't know at
    // this point

    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_BRACK_RIGHT) {
      EmitExpectedTokenError("]", lexeme);
      symbol.SetIsValid(false);
      return symbol;
    }
  } else {
    // it's a name, but without the [ <expression> ]
  }

  return symbol;
}

// this is a left recursive rule made right recursive
// see docs for full write-out of rule
Symbol Parser::ParseRelation() {
  DebugPrint("Relation");

  Symbol term = ParseTerm();

  // peek the next token to see if there is going to be an equality test
  // needed for type checking, this is reaching down into ParseRelationTail
  // lexeme also used for location in possible error reporting
  bool equality_test = false;
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_EQ || lexeme.token == T_NEQ) {
    equality_test = true;
  }

  Symbol relation_tail = ParseRelationTail();

  return CheckRelationParseTypes(term, relation_tail, lexeme, equality_test);
}

// Right recursive portion of the rule
Symbol Parser::ParseRelationTail() {
  DebugPrint("RelationTail");

  // Need to allow for empty evaluation so peek
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_LT || lexeme.token == T_GT_EQ ||
      lexeme.token == T_LT_EQ || lexeme.token == T_GT ||
      lexeme.token == T_EQ || lexeme.token == T_NEQ) {
    // consume the token
    lexeme = scanner_.GetNextLexeme();


    Symbol term = ParseTerm();

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
    Symbol relation_tail = ParseRelationTail();

    return CheckRelationParseTypes(term, relation_tail, lexeme, equality_test);
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

  Symbol expression = ParseExpression();

  // get the symbol for this scope - it will correspond to the return statement
  Symbol procedure = symbol_table_.GetScopeProcedure();

  // check that a return is allowed, it will be if procedure is valid
  if (procedure.IsValid()) {
    EmitError("Return not valid in this scope.", lexeme);
    return;
  }

  // TODO:TypeCheck - this probably follows the same type rules as assignment?
  if (expression.GetType() != procedure.GetType()) {
    EmitError("Return type does not match expression type.", lexeme);
    return;
  }
}

// TODO:TypeCheck
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

void Parser::ParseString() {
  DebugPrint("String");

  // consume the string token
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_STRING_LITERAL) {
    EmitError("Expected string literal", lexeme);
  }
}

// a left recursive rule made right recursive. See docs for writeout
Symbol Parser::ParseTerm() {
  DebugPrint("Term");

  Symbol factor = ParseFactor();

  // peek the location of the operator for possible error reporting
  Lexeme lexeme = scanner_.PeekNextLexeme();

  Symbol term_tail = ParseTermTail();

  return CheckArithmeticParseTypes(factor, term_tail, lexeme);

}

// Right recursive version of ParseTerm
Symbol Parser::ParseTermTail() {
  DebugPrint("TermTail");

  // peek because there can be an empty evaluation
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_DIV || lexeme.token == T_MULT) {
    // consume the token
    lexeme = scanner_.GetNextLexeme();

    Symbol factor = ParseFactor();

    // peek the location of the operator for possible error reporting
    Lexeme lexeme = scanner_.PeekNextLexeme();

    Symbol term_tail =  ParseTermTail();

    return CheckArithmeticParseTypes(factor, term_tail, lexeme);
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
    ParseIdentifier();
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
