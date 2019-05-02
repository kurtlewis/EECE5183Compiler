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
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
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
      array_unwrap_(false),
      array_unwrap_bound_(0),
      llvm_module_(nullptr),
      llvm_context_(), // create a new global context
      llvm_current_procedure_(nullptr),
      llvm_builder_(nullptr),
      llvm_array_unwrap_index_(nullptr),
      llvm_array_unwrap_index_address_(nullptr),
      llvm_array_unwrap_loop_header_block_(nullptr),
      llvm_array_unwrap_loop_end_block_(nullptr) {

}

// Deconstructor
Parser::~Parser() {
  if (llvm_builder_ != nullptr) {
    delete llvm_builder_;
  }
  if (llvm_current_procedure_ != nullptr) {
    // TODO:codegen - who owns this pointer? Stop deleting for now.
    // eventually need to do a memory leak test
    //delete llvm_current_procedure_;
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
    std::cout << "Codegen was either disabled or errors were detected. ";
    std::cout << "Skipping build." << std::endl;
    return;
  }

  // if the user requested codegen debug, print the llvm IR
  if (codegen_debug_) {
    llvm_module_->print(llvm::outs(), nullptr);
  }

  // check the generated module for errors
  bool broken = llvm::verifyModule(*llvm_module_, &llvm::errs());
  
  if (!broken) {
    // Module can be safely compiled, it is not broken

    auto target_triple = llvm::sys::getDefaultTargetTriple();

    // initialize all targets for object code
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::string error;
    auto target = llvm::TargetRegistry::lookupTarget(target_triple, error);

    if (!target) {
      // there was an error looking up the target in the registry.
      llvm::errs() << error;
      return;
    }

    std::string cpu = "generic";
    std::string features = "";

    llvm::TargetOptions target_options;
    auto RM = llvm::Optional<llvm::Reloc::Model>();
    // need a PIC_ Relocation model for global strings
    RM = llvm::Reloc::PIC_;
    auto target_machine = target->createTargetMachine(target_triple, cpu,
                                                     features, target_options,
                                                     RM);

    // configure data layout
    llvm_module_->setDataLayout(target_machine->createDataLayout());
    llvm_module_->setTargetTriple(target_triple);

    std::string filename = "out.s";
    std::error_code error_code;
    llvm::raw_fd_ostream dest(filename, error_code, llvm::sys::fs::F_None);

    if (error_code) {
      llvm::errs() << "Could not open output file: " << error_code.message();
      return;
    }

    llvm::legacy::PassManager pass_manager;
    //auto file_type = llvm::TargetMachine::CGFT_ObjectFile;
    auto file_type = llvm::TargetMachine::CGFT_AssemblyFile;

    if (target_machine->addPassesToEmitFile(pass_manager, dest, nullptr, 
                                            file_type)) {
      llvm::errs() << "TargetMachine cannot emit a file of this type.";
      return;
    }

    pass_manager.run(*llvm_module_);
    dest.flush();
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
  codegen_ = false;
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
  codegen_ = false;
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
  codegen_ = false;
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
  codegen_ = false;
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
  codegen_ = false;
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

Symbol Parser::ParseIndex(Symbol identifier) {
  // identifier is being indexed
  identifier.SetIsIndexed(true);
  // Check for open bracket consume token
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_BRACK_LEFT) {
    EmitExpectedTokenError("[", lexeme);
    Symbol symbol = Symbol::GenerateAnonymousSymbol();
    symbol.SetIsValid(false);
    return symbol;
  }

  // a bound will evaluate to an integer
  Symbol context = Symbol::GenerateAnonymousSymbol();
  context.SetType(TYPE_INT);
  Symbol index = ParseExpression(context);
    
  // Type checking
  index = DoAssignmentTypeCheckingAndConversionCodegen(context, index, lexeme);

  // an index must be an integer
  if (index.GetType() != TYPE_INT) {
    // lexeme points to '[' which is okay
    EmitError("Array index must evaluate to integer.", lexeme);
    // return invalid symbol
    identifier.SetIsValid(false);
    return identifier;
  }

  // if there's an index, the indexed symbol must be an array
  if (!identifier.IsArray()) {
    EmitError("Can only index array types.", lexeme);
    // return invalid symbol
    identifier.SetIsValid(false);
    return identifier;
  }

  // ending bracket is required if it was opened
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_BRACK_RIGHT) {
    EmitExpectedTokenError("]", lexeme);
    identifier.SetIsValid(false);
    return identifier;
  }

  if (codegen_) {
    // start off with checks to ensure the index is in bounds
    // compare to make sure >= 0
    llvm::Value *zero_32b = llvm::ConstantInt::getIntegerValue(
        GetRespectiveLLVMType(TYPE_INT),
        llvm::APInt(32, 0, true));

    llvm::Value *greater_than_zero = llvm_builder_->CreateICmpSGE(
        index.GetLLVMValue(),
        zero_32b);

    llvm::Value *less_than_bound = llvm_builder_->CreateICmpSLT(
        index.GetLLVMValue(),
        identifier.GetLLVMBound());

    llvm::Value *correct_index = llvm_builder_->CreateAnd(
        greater_than_zero,
        less_than_bound);

    // conditionally jump to call or continue on
    llvm::BasicBlock *error_block = llvm::BasicBlock::Create(
        llvm_context_,
        "", // no need to name
        llvm_current_procedure_);
    llvm::BasicBlock *continue_block = llvm::BasicBlock::Create(
        llvm_context_,
        "", // no need to name
        llvm_current_procedure_);

    // jump to error block if out of bounds, else continue block
    llvm_builder_->CreateCondBr(correct_index, continue_block, error_block);

    // put call in error block
    llvm_builder_->SetInsertPoint(error_block);

    // look up error function symbol using it's definition - this is ugly
    Symbol error_func = symbol_table_.FindSymbolByIdentifier("_error_func");

    llvm_builder_->CreateCall(error_func.GetLLVMFunction());

    // need to have terminator, even though it won't be reached
    llvm_builder_->CreateBr(continue_block);

    // okay now set insert to continue block and keep on keeping on
    llvm_builder_->SetInsertPoint(continue_block);

    llvm::Value *address = nullptr;
    if (identifier.IsGlobal()) {
      // global arrays need indexed differently from local arrays
      llvm::Value *indices[] = {
          zero_32b,
          index.GetLLVMValue()
      };

      address = llvm_builder_->CreateInBoundsGEP(
          identifier.GetLLVMArrayAddress(),
          indices);
    } else {
      // update address to be address of element
      address = llvm_builder_->CreateGEP(
          identifier.GetLLVMArrayAddress(),
          index.GetLLVMValue());
    }
    
    identifier.SetLLVMAddress(address); 
  }
  return identifier;
}

Symbol Parser::DoExpressionTypeCheckingAndCodegen(Symbol type_context,
                                                  Symbol arith_op,
                                                  Symbol expression_tail,
                                                  Lexeme operation,
                                                  bool not_operation) {
  // is the tail a valid symbol?
  if (expression_tail.IsValid()) {
    // there is a tail, which means there is an operation

    // Generate an anonymous symbol to return
    Symbol symbol = Symbol::GenerateAnonymousSymbol();
    
    bool compatible = false;
    std::string operation_type_string;
    // context is important in determining type compatibility for this
    // Parse rule
    if (type_context.GetType() == TYPE_BOOL) {
      // expected output is a bool so the expression operator ('&', '|', 'not')
      // is a logical operation
      // logical operation is only supported between bools
      operation_type_string = "logical operation";
      compatible = (arith_op.GetType() == TYPE_BOOL &&
                    expression_tail.GetType() == TYPE_BOOL);
      // output type is boolean because that's what's expected and what the
      // result will be
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
      EmitError("Invalid type in expression.", operation);
      symbol.SetIsValid(false);
      return symbol;
    }
     

    if (!compatible) {
      EmitOperationTypeCheckingError(operation_type_string,
                                     Symbol::GetTypeString(arith_op),
                                     Symbol::GetTypeString(expression_tail),
                                     operation);
      symbol.SetIsValid(false);
      return symbol;
    }

    if (codegen_) {
      // despite the fact this rule handles both logical and bitwise operations
      // in llvm, these are they same, because booleans are 1 bit integers!
      llvm::Value *val;
      switch (operation.token) {
        case T_AND:
          val = llvm_builder_->CreateAnd(arith_op.GetLLVMValue(),
                                         expression_tail.GetLLVMValue()); 
          break;
        case T_BAR: // or
          val = llvm_builder_->CreateOr(arith_op.GetLLVMValue(),
                                        expression_tail.GetLLVMValue());
          break;
        default:
          std::cout << "Error matching operation to codegen." << std::endl;
      }
      // update the outgoing symbol
      symbol.SetLLVMValue(val);

      // Check to see if there is a not on the operation
      if (not_operation) {
        val = llvm_builder_->CreateNot(symbol.GetLLVMValue());
        // update outgoing symbol
        symbol.SetLLVMValue(val);
      }
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
                                       operation);
        // Generate anonymous symbol and return it
        Symbol symbol = Symbol::GenerateAnonymousSymbol();
        symbol.SetIsValid(false);
        return symbol;
      }

      if (codegen_) {
        // do the actual not operation on arith_op
        llvm::Value *val = llvm_builder_->CreateNot(arith_op.GetLLVMValue());
        // update arith op before returning it
        arith_op.SetLLVMValue(val);
      }
    }
    // the tail was not a valid symbol, return the arith_op lead as is
    return arith_op;
  }
}

Symbol Parser::DoRelationTypeCheckingAndCodegen(Symbol type_context,
                                                Symbol term,
                                                Symbol relation_tail,
                                                Lexeme operation) {
  // is the relation_tail a valid symbol?
  if (relation_tail.IsValid()) {
    // yes it is valid, so there is a relation operation
    // Generate an anonymous symbol to return
    Symbol symbol = Symbol::GenerateAnonymousSymbol();

    bool compatible = false;


    bool floating_point_comparison = false;

    // confirm that types are compatible. Always do type widening
    // so bool->int->float (but not across unallowable boundaries)
    if (term.GetType() == TYPE_BOOL) {
      compatible = (relation_tail.GetType() == TYPE_BOOL ||
                    relation_tail.GetType() == TYPE_INT);
      if (codegen_ && compatible && relation_tail.GetType() == TYPE_INT) {
        // zero extend the bool term to an int
        llvm::Value *val = llvm_builder_->CreateZExtOrTrunc(
            term.GetLLVMValue(),
            GetRespectiveLLVMType(TYPE_INT));
        // update the term llvm value
        term.SetLLVMValue(val);
      }
    } else if (term.GetType() == TYPE_FLOAT) {
      // comparison is between floats
      floating_point_comparison = true;

      compatible = (relation_tail.GetType() == TYPE_FLOAT ||
                    relation_tail.GetType() == TYPE_INT);
      if (codegen_ && compatible && relation_tail.GetType() == TYPE_INT) {
        // need to widen relation tail to float
        llvm::Value *val = llvm_builder_->CreateSIToFP(
            relation_tail.GetLLVMValue(),
            GetRespectiveLLVMType(TYPE_FLOAT));
        // update the relation tail symbol
        relation_tail.SetLLVMValue(val);
      }
    } else if (term.GetType() == TYPE_INT) {
      compatible = (relation_tail.GetType() == TYPE_INT ||
                    relation_tail.GetType() == TYPE_BOOL ||
                    relation_tail.GetType() == TYPE_FLOAT);

      if (codegen_ && compatible) {
        if (relation_tail.GetType() == TYPE_BOOL) {
          // convert bool -> int
          llvm::Value *val = llvm_builder_->CreateZExtOrTrunc(
              relation_tail.GetLLVMValue(),
              GetRespectiveLLVMType(TYPE_INT));
          //update the tail value
          relation_tail.SetLLVMValue(val);
        } else if (relation_tail.GetType() == TYPE_FLOAT) {
          // widen term int -> float
          llvm::Value *val = llvm_builder_->CreateSIToFP(
              term.GetLLVMValue(),
              GetRespectiveLLVMType(TYPE_FLOAT));
          // update the term value
          term.SetLLVMValue(val);

          // now the comparison is between floats
          floating_point_comparison = true;
        }
      }
    } else if (term.GetType() == TYPE_STRING) {
      // strings are only compatible for equality tests
      compatible = ((operation.token == T_EQ || operation.token == T_NEQ) &&
                    relation_tail.GetType() == TYPE_STRING);
    }
    
    if (!compatible) {
      EmitOperationTypeCheckingError("relational operation",
                                     Symbol::GetTypeString(term),
                                     Symbol::GetTypeString(relation_tail),
                                     operation);
      symbol.SetIsValid(false);
      return symbol;
    }

    if (codegen_) {
      // do actual comparison
      llvm::Value *val;
      if (term.GetType() == TYPE_STRING) {
        // it's a a comparison between strings which is complicated
        // need to explicitly test the entire array of i8's which involves a
        // for loop
        // create an index - unfortunately have to store it to avoid
        // the builder optimizing addition of it away
        llvm::Value *index_address = llvm_builder_->CreateAlloca(
            GetRespectiveLLVMType(TYPE_INT));
        llvm::Value *index = llvm::ConstantInt::getIntegerValue(
            GetRespectiveLLVMType(TYPE_INT),
            llvm::APInt(32, 0, true));
        llvm_builder_->CreateStore(index, index_address);


        llvm::BasicBlock *string_compare_block = llvm::BasicBlock::Create(
            llvm_context_,
            "", // no need to name
            llvm_current_procedure_);

        llvm::BasicBlock *string_compare_end_block = llvm::BasicBlock::Create(
            llvm_context_,
            "", // no need to name
            llvm_current_procedure_);

        // unconditional jump to loop
        llvm_builder_->CreateBr(string_compare_block);
        llvm_builder_->SetInsertPoint(string_compare_block);

        // increment index
        llvm::Value *one_32b = llvm::ConstantInt::getIntegerValue(
            GetRespectiveLLVMType(TYPE_INT),
            llvm::APInt(32, 1, true));

        index = llvm_builder_->CreateLoad(
            GetRespectiveLLVMType(TYPE_INT),
            index_address);

        //index = llvm_builder_->CreateAdd(index, one_32b);
        index = llvm_builder_->CreateBinOp(
            llvm::Instruction::Add,
            index,
            one_32b);

        llvm_builder_->CreateStore(index, index_address);

        // get the current char in the term
        llvm::Value *term_address = llvm_builder_->CreateGEP(
            term.GetLLVMValue(),
            index);
        llvm::Value *term_char = llvm_builder_->CreateLoad(
            llvm_builder_->getInt8Ty(),
            term_address);

        // now get the current char in the tail
        llvm::Value *tail_address = llvm_builder_->CreateGEP(
            relation_tail.GetLLVMValue(),
            index);
        llvm::Value *tail_char = llvm_builder_->CreateLoad(
            llvm_builder_->getInt8Ty(),
            tail_address);
        
        // okay now compare the tails
        llvm::Value *comparison = llvm_builder_->CreateICmpEQ(
            term_char,
            tail_char);

        
        // need to ensure the strings aren't ending
        // if one is ending but the other isn't, they won't be equal and the
        // loop will exit anyways
        // if they both end at the same time this will catch that
        llvm::Value *zero_8b = llvm::ConstantInt::getIntegerValue(
            llvm_builder_->getInt8Ty(),
            llvm::APInt(8, 0, true));
        llvm::Value *term_not_ending = llvm_builder_->CreateICmpNE(
            term_char,
            zero_8b);


        llvm::Value *continue_comparison = llvm_builder_->CreateAnd(
            comparison,
            term_not_ending);

        llvm_builder_->CreateCondBr(continue_comparison, string_compare_block,
                                    string_compare_end_block);

        // output is the end block
        llvm_builder_->SetInsertPoint(string_compare_end_block);

        if (operation.token == T_EQ) {
          val = comparison;
        } else {
          // not equal, so just invert the not
          val = llvm_builder_->CreateNot(comparison);
        }
      } else {
        switch (operation.token) {
          case T_EQ:
              if (floating_point_comparison) {
                val = llvm_builder_->CreateFCmpOEQ(term.GetLLVMValue(),
                                                  relation_tail.GetLLVMValue());
              } else {
                val = llvm_builder_->CreateICmpEQ(term.GetLLVMValue(),
                                                  relation_tail.GetLLVMValue());
              }
            break;
          case T_NEQ:
              if (floating_point_comparison) {
                val = llvm_builder_->CreateFCmpONE(term.GetLLVMValue(),
                                                  relation_tail.GetLLVMValue());
              } else {
                val = llvm_builder_->CreateICmpNE(term.GetLLVMValue(),
                                                  relation_tail.GetLLVMValue());
              }
            break;
          case T_GT:
              if (floating_point_comparison) {
                val = llvm_builder_->CreateFCmpOGT(term.GetLLVMValue(),
                                                  relation_tail.GetLLVMValue());
              } else {
                val = llvm_builder_->CreateICmpSGT(term.GetLLVMValue(),
                                                  relation_tail.GetLLVMValue());
              }
            break;
          case T_GT_EQ:
              if (floating_point_comparison) {
                val = llvm_builder_->CreateFCmpOGE(term.GetLLVMValue(),
                                                  relation_tail.GetLLVMValue());
              } else {
                val = llvm_builder_->CreateICmpSGE(term.GetLLVMValue(),
                                                  relation_tail.GetLLVMValue());
              }
            break;
          case T_LT:
              if (floating_point_comparison) {
                val = llvm_builder_->CreateFCmpOLT(term.GetLLVMValue(),
                                                  relation_tail.GetLLVMValue());
              } else {
                val = llvm_builder_->CreateICmpSLT(term.GetLLVMValue(),
                                                  relation_tail.GetLLVMValue());
              }
            break;
          case T_LT_EQ:
              if (floating_point_comparison) {
                val = llvm_builder_->CreateFCmpOLE(term.GetLLVMValue(),
                                                  relation_tail.GetLLVMValue());
              } else {
                val = llvm_builder_->CreateICmpSLE(term.GetLLVMValue(),
                                                  relation_tail.GetLLVMValue());
              }
            break;
          default:
            std::cout << "Error matching operation to codegen." << std::endl;
        }
      }
      // update the outgoing symbol
      symbol.SetLLVMValue(val);
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

Symbol Parser::DoArithmeticTypeCheckingAndCodegen(Symbol type_context,
                                                  Symbol lead,
                                                  Symbol tail,
                                                  Lexeme operation) {
  // is tail a valid Symbol?
  if (tail.IsValid()) {
    // Generate an anonymous symbol to return
    Symbol symbol = Symbol::GenerateAnonymousSymbol();

    // do a type check between the results
    // if an llvm type conversion ends up being necessary,
    // always do a widening conversion, so whatever is the int goes int->float
    bool compatible = false;
    if (lead.GetType() == TYPE_INT) {
      compatible = (tail.GetType() == TYPE_INT || tail.GetType() == TYPE_FLOAT);

      // if the tail is a float, need to convert lead
      if (codegen_ && compatible && tail.GetType() == TYPE_FLOAT) {
        // do actual type conversion and update symbol
        llvm::Value *val = llvm_builder_->CreateSIToFP(
            lead.GetLLVMValue(),
            GetRespectiveLLVMType(TYPE_FLOAT));
        // update the lead symbol
        lead.SetLLVMValue(val);
      }
    } else if (lead.GetType() == TYPE_FLOAT) {
      compatible = (tail.GetType() == TYPE_INT || tail.GetType() == TYPE_FLOAT);

      // if the tail is an int need to make it a float
      if (codegen_ && compatible && tail.GetType() == TYPE_INT) {
        // do actual type conversion and update symbol
        llvm::Value *val = llvm_builder_->CreateSIToFP(
            tail.GetLLVMValue(),
            GetRespectiveLLVMType(TYPE_FLOAT));
        // update the tail symbol
        tail.SetLLVMValue(val);
      }
    }
    
    if (!compatible) {
      EmitOperationTypeCheckingError("Arithmetic",
                                     Symbol::GetTypeString(lead),
                                     Symbol::GetTypeString(tail),
                                     operation);
      symbol.SetIsValid(false);
      return symbol;
    }

    // codegen requires different instructions if the types are of float
    // we know it's float because either of the params is a float
    // and we always widen type when casting
    bool floating_point_op = (lead.GetType() == TYPE_FLOAT ||
                              tail.GetType() == TYPE_FLOAT); 
    if (codegen_) {
      // do actual operation instruction
      llvm::Value *val;
      switch (operation.token) {
        case T_PLUS:
          if (floating_point_op) {
            val = llvm_builder_->CreateBinOp(llvm::Instruction::FAdd,
                                             lead.GetLLVMValue(),
                                             tail.GetLLVMValue());
          } else {
            val = llvm_builder_->CreateBinOp(llvm::Instruction::Add,
                                             lead.GetLLVMValue(),
                                             tail.GetLLVMValue());
          }
          break;
        case T_MINUS:
          if (floating_point_op) {
            val = llvm_builder_->CreateBinOp(llvm::Instruction::FSub,
                                             lead.GetLLVMValue(),
                                             tail.GetLLVMValue());
          } else {
            val = llvm_builder_->CreateBinOp(llvm::Instruction::Sub,
                                             lead.GetLLVMValue(),
                                             tail.GetLLVMValue());
          }
          break;
        case T_MULT:
          if (floating_point_op) {
            val = llvm_builder_->CreateBinOp(llvm::Instruction::FMul,
                                             lead.GetLLVMValue(),
                                             tail.GetLLVMValue());
          } else {
            val = llvm_builder_->CreateBinOp(llvm::Instruction::Mul,
                                             lead.GetLLVMValue(),
                                             tail.GetLLVMValue());
          }
          break;
        case T_DIV:
          if (floating_point_op) {
            val = llvm_builder_->CreateBinOp(llvm::Instruction::FDiv,
                                             lead.GetLLVMValue(),
                                             tail.GetLLVMValue());
          } else {
            val = llvm_builder_->CreateBinOp(llvm::Instruction::SDiv,
                                             lead.GetLLVMValue(),
                                             tail.GetLLVMValue());
          }
          break;
        default:
          std::cout << "Error matching operation to case." << std::endl;
      }
      // update the outgoing symbol with the new value
      symbol.SetLLVMValue(val);
    }


    // cast the output to the type context
    if (type_context.GetType() == TYPE_FLOAT) {
      symbol.SetType(TYPE_FLOAT);

      if (codegen_ && !floating_point_op) {
        // floating point op is false, so the operation was between ints
        // need to cast to a float
        llvm::Value *val = llvm_builder_->CreateSIToFP(
            symbol.GetLLVMValue(),
            GetRespectiveLLVMType(TYPE_FLOAT));
        // update the output symbol
        symbol.SetLLVMValue(val);
      }
    } else if(type_context.GetType() == TYPE_INT) {
      symbol.SetType(TYPE_INT);

      if (codegen_ && floating_point_op) {
        // it was a floating point op, so need to go back to an int
        llvm::Value *val = llvm_builder_->CreateFPToSI(
            symbol.GetLLVMValue(),
            GetRespectiveLLVMType(TYPE_INT));
        // update the output symbol
        symbol.SetLLVMValue(val);
      }
    } else {
      // arithmetic operation in a context that isn't clear what our output
      // type should be. Favor float if one of the types was a float
      // this is automatically handled in terms of codegen,
      // type conversion is widening so if one was a float the other one
      // was cast to it and the output is a float. If both were ints
      // the result is an int
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

Symbol Parser::DoAssignmentTypeCheckingAndConversionCodegen(Symbol destination,
                                                            Symbol expression,
                                                            Lexeme location) {
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
      expression.SetType(TYPE_BOOL);
      EmitWarning("Coercing int to bool.", location);
      if (codegen_) {
        // update expression
        llvm::Value *val =
            ConvertLLVMIntegerToBoolean(expression.GetLLVMValue());
        expression.SetLLVMValue(val);
      }
    }

    if (destination.GetType() == TYPE_INT) {
      if (expression.GetType() == TYPE_BOOL) {
        expression.SetType(TYPE_INT);
        mismatch = false;
        EmitWarning("Coercing bool to int.", location); 
        if (codegen_) {
          // convert expression from bool to int
          llvm::Value *val = llvm_builder_->CreateZExtOrTrunc(
              expression.GetLLVMValue(),
              GetRespectiveLLVMType(TYPE_INT));

          // update expression
          expression.SetLLVMValue(val);
        }
      } else if (expression.GetType() == TYPE_FLOAT) {
        expression.SetType(TYPE_INT);
        mismatch = false;
        EmitWarning("Coercing float to int.", location);
        if (codegen_) {
          // convert expression from float to int
          llvm::Value *val = llvm_builder_->CreateFPToSI(
              expression.GetLLVMValue(),
              GetRespectiveLLVMType(TYPE_INT));

          // update the expression
          expression.SetLLVMValue(val);
        }
      }
    }

    if (destination.GetType() == TYPE_FLOAT &&
        expression.GetType() == TYPE_INT) {
      mismatch = false;
      EmitWarning("Coercing int to float.", location);
      expression.SetType(TYPE_FLOAT);

      if (codegen_) {
        // extend the int to a float
        llvm::Value *val = llvm_builder_->CreateSIToFP(
            expression.GetLLVMValue(),
            GetRespectiveLLVMType(TYPE_FLOAT));

        // update the expression
        expression.SetLLVMValue(val);
      }
    }

    // couldn't recover from mismatch via interoperability
    if (mismatch) {
      // lexeme refers to equals sign, which is ideal
      EmitExpectedTypeError(Symbol::GetTypeString(destination),
                            Symbol::GetTypeString(expression),
                            location);
      expression.SetIsValid(false);
      return expression;
    }
  }
  return expression;
}

llvm::Type *Parser::GetRespectiveLLVMType(Symbol symbol) {
  return GetRespectiveLLVMType(symbol.GetType());
}

llvm::Type *Parser::GetRespectiveLLVMType(Type type) {
  switch(type) {
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

llvm::Value *Parser::ConvertLLVMIntegerToBoolean(llvm::Value *incoming_val) {
  // Cast expresssion from int to bool
  // llvm doesn't have native support for this as far as I can tell, 
  // but we can do a comparison from true to false and use the select
  // instruction to pick the appropriate value
  // create an int = 0 to compare to
  llvm::Value *zero_32b = llvm::ConstantInt::getIntegerValue(
    GetRespectiveLLVMType(TYPE_INT),
    llvm::APInt(32, 0, true));

  // compare value to zero to see if it will be true
  llvm::Value *comparison = llvm_builder_->CreateICmpEQ(
      incoming_val,
      zero_32b);

  // create some constant options for select statement
  llvm::Value *true_1b = llvm::ConstantInt::getIntegerValue(
      GetRespectiveLLVMType(TYPE_BOOL),
      llvm::APInt(1, 1, true));
  llvm::Value *false_1b = llvm::ConstantInt::getIntegerValue(
      GetRespectiveLLVMType(TYPE_BOOL),
      llvm::APInt(1, 0, true));

  // do actual select statement
  // if expression = 0, use false, otherwise use true per language
  // semantics
  llvm::Value *outgoing_val = llvm_builder_->CreateSelect(comparison, false_1b,
                                                          true_1b);

  return outgoing_val;
}

// this rule is defined recursively, but it makes more sense to use
// iteratively
std::vector<llvm::Value *> Parser::ParseArgumentList(
    std::vector<Symbol>::iterator param_current,
    std::vector<Symbol>::iterator param_end) {
  DebugPrint("ArgumentList");

  // flag to continue iterating
  bool more_args = false;

  std::vector<llvm::Value *> args;
  do {
    // peek a lexeme for possible error reporting
    Lexeme lexeme = scanner_.PeekNextLexeme();

    // make sure there are more arguments allowed to be coming
    if (param_current == param_end) {
      // the param_current is the end, there should be no more arguments
      EmitError("Argument miss match, too many arguments.", lexeme);
      return args;
    }

    // The type expectation is the current parameter
    Symbol expression = ParseExpression(*param_current);

    // peek a lexeme for possible error reporting
    lexeme = scanner_.PeekNextLexeme();

    // do type checking/conversion
    expression = DoAssignmentTypeCheckingAndConversionCodegen(*param_current,
                                                              expression,
                                                              lexeme);

    // some additional checking to ensure if the param is an array the arg is 2
    if (param_current->IsArray()) {
      if (param_current->GetArrayBound() != expression.GetArrayBound() ||
          expression.IsIndexed()) {
        EmitError("Expected array argument.", lexeme);
        return args;
      }
    } else if (expression.IsArray() && !expression.IsIndexed()) {
      // unindexed array as an arg - not allowed
      EmitError("Unexpected array as argument.", lexeme);
      return args;
    }

    if (!expression.IsValid()) {
      return args;
    }


    // argument is okay, add it to the vector of values
    if (expression.IsArray() && !expression.IsIndexed()) {
      if (expression.IsGlobal()) {
        // global arrays are defined differently
        // so need to cast the array type to a pointer type
        // this is really ugly but the easiest solution
        llvm::Value *zero_32b = llvm::ConstantInt::getIntegerValue(
            GetRespectiveLLVMType(TYPE_INT),
            llvm::APInt(32, 0, true));
        llvm::Value *pointer = llvm_builder_->CreateInBoundsGEP(
            expression.GetLLVMArrayAddress(),
            zero_32b);
        pointer = llvm_builder_->CreateBitCast(
            pointer,
            GetRespectiveLLVMType(expression)->getPointerTo());
        args.push_back(pointer);
      } else {
        args.push_back(expression.GetLLVMArrayAddress());
      }
    } else {
      args.push_back(expression.GetLLVMValue());
    }

    // more arguments are optional and indicated by a comma
    lexeme = scanner_.PeekNextLexeme();
    if (lexeme.token == T_COMMA) {
      // consume comma
      lexeme = scanner_.GetNextLexeme();

      // continue parsing more args
      more_args = true;

      // increment current param iterator
      param_current = std::next(param_current);
    } else {
      // it's the end of the argument list
      if (param_current != param_end) {
        if (std::next(param_current) != param_end) {
          // there are more argument that should be present
          EmitError("Not enough arugments for procedure call", lexeme);
          return args;
        }
      }

      more_args = false;
    }
  } while (more_args);

  return args;
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

  // do type checking/conversion
  expression = DoAssignmentTypeCheckingAndConversionCodegen(destination,
                                                            expression,
                                                            lexeme);
  
  if (!expression.IsValid()) {
    return;
  }

  if (codegen_) {
    // store the expression value in the address of the destination
    llvm_builder_->CreateStore(expression.GetLLVMValue(),
                               destination.GetLLVMAddress());
    destination.SetLLVMValue(expression.GetLLVMValue());

    // update the symbol table entry for the destination
    symbol_table_.InsertSymbol(destination);

    // if this assignment block was doing an array unwrap, clean up
    if (array_unwrap_) {
      // increment the index and update the stored value
      // we know it's already been loaded in this context
      llvm::Value *one_32b = llvm::ConstantInt::getIntegerValue(
          GetRespectiveLLVMType(TYPE_INT),
          llvm::APInt(32, 1, true));
      llvm_array_unwrap_index_ = llvm_builder_->CreateAdd(
          llvm_array_unwrap_index_,
          one_32b);
      llvm_builder_->CreateStore(llvm_array_unwrap_index_,
                                 llvm_array_unwrap_index_address_);
      // create a unconditional jump back to the header to test for another
      // go around
      llvm_builder_->CreateBr(llvm_array_unwrap_loop_header_block_);

      // statement is now over, further code will be inserted into the end block
      llvm_builder_->SetInsertPoint(llvm_array_unwrap_loop_end_block_);
      array_unwrap_ = false;
    }
  }
   
  // destination has been written to, to mark that and update it in symbol table
  destination.SetHasBeenInitialized(true);
  symbol_table_.InsertSymbol(destination);
}

// this is a left recursive rule that has been modified to be right recursive
// see docs/language-grammar-modified.txt for more information
Symbol Parser::ParseArithOp(Symbol type_context) {
  DebugPrint("ArithOp");

  Symbol relation = ParseRelation(type_context);

  // Peek ahead - will be used for error reporting, and if there's a tail
  // it is the operation
  Lexeme operation = scanner_.PeekNextLexeme();

  Symbol arith_op_tail = ParseArithOpTail(type_context);

  return DoArithmeticTypeCheckingAndCodegen(type_context, relation,
                                            arith_op_tail, operation);
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

    // Peek ahead - will be used for error reporting, and if there's a tail
    // it is the operation
    Lexeme operation = scanner_.PeekNextLexeme();

    // recursive call
    Symbol arith_op_tail = ParseArithOpTail(type_context);

    return DoArithmeticTypeCheckingAndCodegen(type_context, relation,
                                              arith_op_tail, operation);
  }

  // Empty evaluation of the rule, return an invalid symbol
  Symbol symbol = Symbol::GenerateAnonymousSymbol();
  symbol.SetIsValid(false);
  return symbol;
}

void Parser::ParseBound(Symbol &symbol) {
  DebugPrint("Bound");

  Lexeme lexeme = scanner_.PeekNextLexeme();
  // next lexeme must be an integer - floats are not allowed
  if (lexeme.token != T_INT_LITERAL) {
    EmitExpectedTokenError("int literal", lexeme);
    return;
  }
  
  Symbol bound = ParseNumber();

  // safe because it was checked to be int
  int bound_value = lexeme.int_value;

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
    symbol = ParseIndex(symbol);
  } else {
    // there is not an index
    if (symbol.IsArray()) {
      // but the symbol is an array! this begins an array unwrapping
      symbol.SetIsIndexed(true);
      if (codegen_) {
        array_unwrap_ = true;
        array_unwrap_bound_ = symbol.GetArrayBound();

        // create the index
        llvm::Value *zero_32b = llvm::ConstantInt::getIntegerValue(
            GetRespectiveLLVMType(TYPE_INT),
            llvm::APInt(32, 0, true));
        llvm_array_unwrap_index_ = zero_32b;

        
        // allocate an index storage location and store the value in it
        llvm_array_unwrap_index_address_ = llvm_builder_->CreateAlloca(
            GetRespectiveLLVMType(TYPE_INT));
        llvm_builder_->CreateStore(llvm_array_unwrap_index_,
                                   llvm_array_unwrap_index_address_);

        // create the blocks of the loop
        llvm_array_unwrap_loop_header_block_ = llvm::BasicBlock::Create(
            llvm_context_,
            "", // don't need to name
            llvm_current_procedure_);

        // only referenced here, so doesn't need stored as class instance
        llvm::BasicBlock *unwrap_loop_body_block = llvm::BasicBlock::Create(
            llvm_context_,
            "", // no need to name
            llvm_current_procedure_);
        llvm_array_unwrap_loop_end_block_ = llvm::BasicBlock::Create(
            llvm_context_,
            "", // don't need to name
            llvm_current_procedure_);

        //
        // create the header block which compares the index to the the bound
        // and exits the loop if need be
        //
        // jump to the header
        llvm_builder_->CreateBr(llvm_array_unwrap_loop_header_block_);
        llvm_builder_->SetInsertPoint(llvm_array_unwrap_loop_header_block_);
        // load the updated index 
        llvm_array_unwrap_index_ = llvm_builder_->CreateLoad(
            GetRespectiveLLVMType(TYPE_INT),
            llvm_array_unwrap_index_address_);

        // compare index to bound
        llvm::Value *comparison = llvm_builder_->CreateICmpEQ(
            llvm_array_unwrap_index_,
            symbol.GetLLVMBound());

        // conditional jump - if equal exit loop. Otherwise go to body
        llvm_builder_->CreateCondBr(
            comparison,
            llvm_array_unwrap_loop_end_block_,
            unwrap_loop_body_block);

        // now all code will go into the body until the end of the assignment
        // statement 
        llvm_builder_->SetInsertPoint(unwrap_loop_body_block);
        // load the index for this block
        llvm_array_unwrap_index_ = llvm_builder_->CreateLoad(
            GetRespectiveLLVMType(TYPE_INT),
            llvm_array_unwrap_index_address_);
        // first set the address the destination will eventually go into
        // update address to be address of specific element
        llvm::Value *address = nullptr;
        
        if (symbol.IsGlobal()) {
          // global arrays need accessed differently from local arrays
          llvm::Value *indices[] = {
              zero_32b,
              llvm_array_unwrap_index_
          };

          address = llvm_builder_->CreateInBoundsGEP(
              symbol.GetLLVMArrayAddress(),
              indices);
        } else {
          address = llvm_builder_->CreateGEP(
            symbol.GetLLVMArrayAddress(),
            llvm_array_unwrap_index_);
        }
        
        // address will be used for destination to write back value
        symbol.SetLLVMAddress(address); 
      }
    }
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

  // peek location of next symbol for operation token and possible error
  // reporting
  Lexeme operation = scanner_.PeekNextLexeme();

  Symbol expression_tail = ParseExpressionTail(type_context);

  return DoExpressionTypeCheckingAndCodegen(type_context, arith_op,
                                            expression_tail, operation,
                                            not_operation);
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

    // peek location of next symbol for operation token and possible error
    // reporting
    Lexeme operation = scanner_.PeekNextLexeme();

    // Right recursive call
    Symbol expression_tail = ParseExpressionTail(type_context);

    // _not_ only leads in expression, so not_operation param will always
    // be false in ParseExpressionTail
    return DoExpressionTypeCheckingAndCodegen(type_context, arith_op,
                                              expression_tail, operation,
                                              false);
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

    // make this symbol negative - only works on ints or floats
    if (symbol.GetType() == TYPE_INT) {
      if (codegen_) {
        // make the int negative
        llvm::Value *val = llvm_builder_->CreateNeg(symbol.GetLLVMValue());

        // update the outgoing symbol
        symbol.SetLLVMValue(val);
      }
    } else if (symbol.GetType() == TYPE_FLOAT) {
      if (codegen_) {
        // make the float negative
        llvm::Value *val = llvm_builder_->CreateFNeg(symbol.GetLLVMValue());

        // update the outgoing symbol
        symbol.SetLLVMValue(val);
      }
    } else {
        // this is an error because the '-' operator is only defined for
        // floats and ints
        EmitExpectedTypeError("integer or float",
                              Symbol::GetTypeString(symbol),
                              lexeme);
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
    if (codegen_) {
      // convert it from a int to a bool and restore the value in expression
      llvm::Value *val = ConvertLLVMIntegerToBoolean(expression.GetLLVMValue());

      // update expression
      expression.SetLLVMValue(val);
    }
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

  llvm::BasicBlock *true_block = nullptr;
  llvm::BasicBlock *false_block = nullptr;
  llvm::BasicBlock *end_block = nullptr;
  if (codegen_) {
    // create blocks and conditional jump
    true_block = llvm::BasicBlock::Create(
        llvm_context_,
        "", // don't bother naming block
        llvm_current_procedure_);
    false_block = llvm::BasicBlock::Create(
        llvm_context_,
        "", // don't bother naming block
        llvm_current_procedure_);

    // do a conditional jump based on if statement
    llvm_builder_->CreateCondBr(expression.GetLLVMValue(), true_block,
                                false_block);

    // set the new builder insert point to the true block
    llvm_builder_->SetInsertPoint(true_block);
    // now these statements will be inserted into the true block
  }

  // Loop statements until one of the end tokens is found
  Token end_tokens[] = {T_ELSE, T_END};
  int tokens_length = 2;
  LoopStatements(end_tokens, tokens_length);

  // the last lexeme was only peeked, read it now
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token == T_ELSE) {

    if (codegen_) {
      // there is an else statement

      // start with an unconditional jump to a new block which is what comes
      // after the else since we're wrapping up the true block now
      end_block = llvm::BasicBlock::Create(
          llvm_context_,
          "", // again, don't need to name
          llvm_current_procedure_);
           
      llvm_builder_->CreateBr(end_block);

      // now update insertion point to false_block
      llvm_builder_->SetInsertPoint(false_block);
    }

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

  if (codegen_) {
    // if block is over. If there was no else, just use the false block to
    // continue. If there was an else, use the end block
    if (end_block == nullptr) {
      // need to jump from true block to false block
      llvm_builder_->CreateBr(false_block);
      llvm_builder_->SetInsertPoint(false_block);
    } else {
      // need to jump from false block to end block
      llvm_builder_->CreateBr(end_block);
      llvm_builder_->SetInsertPoint(end_block);
    }
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

  llvm::BasicBlock *loop_header_block = nullptr; // loop condition
  llvm::BasicBlock *loop_body_block = nullptr; // loop statements
  llvm::BasicBlock *loop_end_block = nullptr; // block after loop
  if (codegen_) {
    // beggining loop - a couple of things that need to happen
    // create a new block for the loop - everything here to the end goes in it
    loop_header_block = llvm::BasicBlock::Create(
        llvm_context_,
        "", // doesn't need a name
        llvm_current_procedure_);

    // create a place for the body of the loop
    loop_body_block = llvm::BasicBlock::Create(
        llvm_context_,
        "", // doesn't need a name
        llvm_current_procedure_);

    // create an eventual exit place for the loop
    loop_end_block = llvm::BasicBlock::Create(
        llvm_context_,
        "", // doesn't need a name again
        llvm_current_procedure_);

    // unconditionally jump to the loop header block
    llvm_builder_->CreateBr(loop_header_block);

    // update builder to insert into loop header block 
    llvm_builder_->SetInsertPoint(loop_header_block);
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
    if (codegen_) {
      // cast the int expression to a bool
      llvm::Value *val = ConvertLLVMIntegerToBoolean(expression.GetLLVMValue());
      expression.SetLLVMValue(val);
    }
  } else if (expression.GetType() != TYPE_BOOL) {
    EmitError("Expression on loop must evaluate to bool or int.", lexeme);
    return;
  }

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PAREN_RIGHT) {
    EmitExpectedTokenError(")", lexeme);
    return;
  }

  if (codegen_) {
    // conditionally jump based on the expression to either the loop body
    // or the end of the loop
    llvm_builder_->CreateCondBr(expression.GetLLVMValue(), loop_body_block,
                                loop_end_block);

    // now update the insert point to the body block for the statements
    llvm_builder_->SetInsertPoint(loop_body_block);
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

  if (codegen_) {
    // leaving the for loop - need to unconditionally jump the loop header
    // block to see if another go around of the loop is the move
    llvm_builder_->CreateBr(loop_header_block);

    // but, for the purposes of codegen, we're done with this loop.
    // Set the insertion point to the block after the for loop
    llvm_builder_->SetInsertPoint(loop_end_block);
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
    // Declarations are over, generate the basic block to start inserting
    // statements into
    Symbol procedure_symbol = symbol_table_.GetScopeProcedure();
    llvm_current_procedure_ = procedure_symbol.GetLLVMFunction();

    // generate the starting block
    llvm::BasicBlock *procedure_entrypoint = llvm::BasicBlock::Create(
        llvm_context_,
        "entrypoint", // first block is always entrypoint
        llvm_current_procedure_); // parent function

    llvm_builder_->SetInsertPoint(procedure_entrypoint);

    // create stores for all variables in the local scope so they
    // are ready to use
    for (std::map<std::string, Symbol>::iterator
        it = symbol_table_.GetLocalScopeIteratorBegin();
        it != symbol_table_.GetLocalScopeIteratorEnd();
        ++it) {
      // first it must be a variable declaration
      if (it->second.GetDeclaration() != DECLARATION_VARIABLE) {
        continue;
      }

      // allocate a place for it and update the symbol
      llvm::Value *address = nullptr;
      if (it->second.IsArray()) {
        // create a value for the bound
        llvm::Value *bound = llvm::ConstantInt::getIntegerValue(
            GetRespectiveLLVMType(TYPE_INT),
            llvm::APInt(32, it->second.GetArrayBound(), true));

        it->second.SetLLVMBound(bound);
        
        // allocate array
        address = llvm_builder_->CreateAlloca(
            GetRespectiveLLVMType(it->second.GetType()),
            bound);
        it->second.SetLLVMArrayAddress(address);

        // arrays are also considered to already be initialized
        it->second.SetHasBeenInitialized(true);
      } else {
        address = llvm_builder_->CreateAlloca(
            GetRespectiveLLVMType(it->second.GetType()));
        it->second.SetLLVMAddress(address);
      }


      // it is safe to update the same key we are iterating over the map 
      symbol_table_.InsertSymbol(it->second);
    }
    
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

      // need to store this value into the store for the symbol
      // problem is, the symbol the parameter list represents is out of date
      // C++ is stupid (READ, I AM STUPID). TODO: rearchitect for heap symbols?
      // a poor craftsman blames his tools
      Symbol actual_symbol = symbol_table_.FindSymbolByIdentifier(
          symbol.GetId());

      // update symbol store and reinsert it into the symbol table 
      actual_symbol.SetLLVMValue(val);
      actual_symbol.SetHasBeenInitialized(true);
      // store that value into the symbol's address
      if (actual_symbol.IsArray()) {
        // it is an array, and all values are passed by reference, so store
        // the array new
        actual_symbol.SetLLVMArrayAddress(val);
        // TODO:codegen - need to pass arrays by value which means copying
        // the array instead of just reusing the address
      } else {
        // not an array
        llvm_builder_->CreateStore(val, actual_symbol.GetLLVMAddress());
      }
      symbol_table_.InsertSymbol(actual_symbol);
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
      // there is not a terminator, this is an error and should kill compilation
      EmitError("Procedure must return a value.", lexeme);
      return;
    }
  }
}

void Parser::ParseProcedureDeclaration(Symbol &procedure_symbol) {
  DebugPrint("ProcedureDeclaration");

  // Peek a lexeme for error reporting
  Lexeme lexeme = scanner_.PeekNextLexeme();

  // Increase the scope as we enter a new procedure
  symbol_table_.IncreaseScope();

  // mark the current symbol as a procedure
  procedure_symbol.SetDeclaration(DECLARATION_PROCEDURE);

  // Parse the procedure header and continue to build the symbol
  ParseProcedureHeader(procedure_symbol);

  if (codegen_) {
    //
    // create a function definition
    // Do this when entering the body statements so that it isn't overwritten
    // by new functions being declared
    //
    
    // Go through the list of arguments and create params
    std::vector<llvm::Type *> params;
    for (Symbol symbol : procedure_symbol.GetParams()) {
      if (symbol.IsArray()) {
        params.push_back(GetRespectiveLLVMType(symbol)->getPointerTo());
      } else {
        params.push_back(GetRespectiveLLVMType(symbol));
      }
    }

    // define the types of the function
    llvm::FunctionType *functionType = llvm::FunctionType::get(
        GetRespectiveLLVMType(procedure_symbol), // return type
        params, // list of args
        false); // is varargs - always no our language doesn't support
        
    // Actually create the function
    llvm::Constant *procedure = llvm_module_->getOrInsertFunction(
        procedure_symbol.GetId(), // name of function
        functionType);

    // cast it to a function and insert it into the current procedure member var
    llvm::Function *function = llvm::cast<llvm::Function>(procedure);

    // set the calling convention of our procedure to that of a C program
    function->setCallingConv(llvm::CallingConv::C);

    // now that function has been created, update and reinsert symbol
    procedure_symbol.SetLLVMFunction(function);
    
  }

  // check to make sure this identifier doesn't already exist
  if (symbol_table_.CheckForSymbolCollisions(procedure_symbol.GetId())) {
    EmitError("Identifier collision. Identifier already exists.", lexeme);
    return;
  }
  
  // commit the symbol to the symbol table so that the body can reference itself
  symbol_table_.InsertSymbol(procedure_symbol);

  // mark this procedure as the scope procedure 
  symbol_table_.SetScopeProcedure(procedure_symbol);

  // parse the body of the procedure
  ParseProcedureBody();

  // exiting the procedure, decrease the scope
  symbol_table_.DecreaseScope();

  // check again for the new scope
  if (symbol_table_.CheckForSymbolCollisions(procedure_symbol.GetId())) {
    EmitError("Identifier collision. Identifier already exists.", lexeme);
    return;
  }

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

    // create stores for all variables in the local scope so they
    // are ready to use
    for (std::map<std::string, Symbol>::iterator
        it = symbol_table_.GetLocalScopeIteratorBegin();
        it != symbol_table_.GetLocalScopeIteratorEnd();
        ++it) {
      // first it must be a variable declaration
      if (it->second.GetDeclaration() != DECLARATION_VARIABLE) {
        continue;
      }

      // allocate a place for it and update the symbol
      llvm::Value *address = nullptr;
      if (it->second.IsArray()) {
        // create a value for the bound
        llvm::Value *bound = llvm::ConstantInt::getIntegerValue(
            GetRespectiveLLVMType(TYPE_INT),
            llvm::APInt(32, it->second.GetArrayBound(), true));

        it->second.SetLLVMBound(bound);
        
        // allocate array
        address = llvm_builder_->CreateAlloca(
            GetRespectiveLLVMType(it->second.GetType()),
            bound);

        it->second.SetLLVMArrayAddress(address);

        // arrays are also considered to already be initialized
        it->second.SetHasBeenInitialized(true);
      } else {
        address = llvm_builder_->CreateAlloca(
            GetRespectiveLLVMType(it->second.GetType()));
        it->second.SetLLVMAddress(address);
      }


      // it is safe to update the same key we are iterating over the map 
      symbol_table_.InsertSymbol(it->second);
    }
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

  symbol_table_.InsertBuiltInsIntoGlobalScope(codegen_, llvm_module_, 
                                              llvm_context_, llvm_builder_);

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

    // copy symbol and make an anonymous symbol for outgoing result of call
    Symbol procedure_symbol = symbol;
    symbol = Symbol::GenerateAnonymousSymbol();
    symbol.SetType(procedure_symbol.GetType());

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
      std::vector<llvm::Value *> args = ParseArgumentList(
          procedure_symbol.GetParams().begin(), 
          procedure_symbol.GetParams().end());
      // argument list length is asserted to be correct in ParseArgumentList
      if (codegen_) {
        // make argument call with args
        llvm::Value *val = llvm_builder_->CreateCall(
            procedure_symbol.GetLLVMFunction(),
            args);
        // update outgoing symbol
        symbol.SetLLVMValue(val);
      }
    } else {
      // since ParseArgumentList doesn't assert the correct number of args,
      // assert that there are 0 args before calling function with no args
      if (procedure_symbol.GetParams().size() != 0) {
        EmitError("Missing arguments to procedure call.", lexeme);
        symbol.SetIsValid(false);
        return symbol;
      }
      if (codegen_) {
        // make argument call without args
        llvm::Value *val = llvm_builder_->CreateCall(
            procedure_symbol.GetLLVMFunction());
        // update outgoing symbol
        symbol.SetLLVMValue(val);
      }
    }

    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_PAREN_RIGHT) {
      EmitExpectedTokenError(")", lexeme);
      symbol.SetIsValid(false);
      return symbol;
    }
  } else {
    // it's a name. Could be indexed. Must be variable
    if (symbol.GetDeclaration() != DECLARATION_VARIABLE &&
        symbol.GetDeclaration() != DECLARATION_ENUM) {
      EmitError("Reference to name must be a variable or enum declaration.", 
                lexeme);
      symbol = Symbol::GenerateAnonymousSymbol();
      symbol.SetIsValid(false);
      return symbol;
    }
    
    if (lexeme.token == T_BRACK_LEFT) {
      // it's a name reference with an index operation
      // parse the index operation
      // can't be a enum declaration
      if (symbol.GetDeclaration() != DECLARATION_VARIABLE) {
        EmitError("Only array variables can be indexed.", lexeme);
        symbol = Symbol::GenerateAnonymousSymbol();
        symbol.SetIsValid(false);
        return symbol;
      }
      symbol = ParseIndex(symbol);
    } else if (array_unwrap_) {
      // there wasn't an index on this variable, but there is an array unwrap
      // going on for the current expression tree, so see if this is an array
      // that needs unwrapped
      if (symbol.IsArray()) {
        symbol.SetIsIndexed(true);
        // it is an array unwrap
        // type check to ensure the size of this array is at least bigger than
        // the current bound
        if (symbol.GetArrayBound() < array_unwrap_bound_) {
          EmitError("Array unwrapping failed. Array is smaller than target.",
                    lexeme);
          symbol = Symbol::GenerateAnonymousSymbol();
          symbol.SetIsValid(false);
          return symbol;
        }
        // use the index to set the address to load from
        if (codegen_) {
          llvm::Value *address = llvm_builder_->CreateGEP(
              symbol.GetLLVMArrayAddress(),
              llvm_array_unwrap_index_); // safe - it's been loaded 
              
          symbol.SetLLVMAddress(address);

          // now let the code at the end of ParseReference load it!
        }
      }
    } else {
      // there is neither an array unwrap or an index, so mark it as such
      symbol.SetIsIndexed(false);

    }

    if (codegen_) {
      // only variables and enums get here. Variables need loaded,
      // enums are good to go
      if (symbol.GetDeclaration() == DECLARATION_VARIABLE) {
        // ensure there is a value to get
        if (symbol.HasBeenInitialized() == false) {
          EmitError("Attempt to use variable before it's initialized.", lexeme);
          symbol.SetIsValid(false);
          return symbol;
        }

        // if it's an unindexed array don't need to load
        if (symbol.IsArray() && !symbol.IsIndexed()) {
          // todo: maybe copy the array address into the value to abstract
          // this?
        } else {
          // load the value out of the store and prepare to update the outgoing
          // symbol
          llvm::Value *val = llvm_builder_->CreateLoad(
              GetRespectiveLLVMType(symbol.GetType()),
              symbol.GetLLVMAddress());
          
          // update outgoing symbol
          symbol.SetLLVMValue(val);
        }
      } 
    }
  }

  return symbol;
}

// this is a left recursive rule made right recursive
// see docs for full write-out of rule
Symbol Parser::ParseRelation(Symbol type_context) {
  DebugPrint("Relation");

  Symbol term = ParseTerm(type_context);

  // Peek the next token to get what the operation will be, will also be used
  // for error reporting
  Lexeme operation = scanner_.PeekNextLexeme();

  Symbol relation_tail = ParseRelationTail(type_context);

  return DoRelationTypeCheckingAndCodegen(type_context, term, relation_tail,
                                          operation);
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

    // Peek the next token to get what the operation will be, will also be used
    // for error reporting
    Lexeme operation = scanner_.PeekNextLexeme();

    // Recursive call
    Symbol relation_tail = ParseRelationTail(type_context);

    return DoRelationTypeCheckingAndCodegen(type_context, term, relation_tail,
                                            operation);
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

  // check to see that the types are compatible and do conversion if needed
  expression = DoAssignmentTypeCheckingAndConversionCodegen(procedure,
                                                            expression,
                                                            lexeme);

  if (!expression.IsValid()) {
    return;
  }

  if (codegen_) {
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

  // Peek ahead - it will be used for error reporting and if there's a tail
  // it is the operation
  Lexeme operation = scanner_.PeekNextLexeme();

  Symbol term_tail = ParseTermTail(type_context);

  return DoArithmeticTypeCheckingAndCodegen(type_context, factor, term_tail,
                                            operation);

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

    // Peek ahead - it will be used for error reporting and if there's a tail
    // it is the operation
    Lexeme operation = scanner_.PeekNextLexeme();

    Symbol term_tail =  ParseTermTail(type_context);

    return DoArithmeticTypeCheckingAndCodegen(type_context, factor, term_tail,
                                              operation);
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

  // check that the symbol name doesn't conflict
  if (symbol_table_.CheckForSymbolCollisions(type_symbol.GetId())) {
    EmitError("Identifier collision. Identifier already exists.", lexeme);
    return;
  }

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
      symbol.SetDeclaration(DECLARATION_ENUM);
      
      // there are additional rules
      lexeme = scanner_.GetNextLexeme();
      if (lexeme.token != T_CURLY_LEFT) {
        EmitExpectedTokenError("{", lexeme);
        return;
      }

      int enum_value = 0;

      // always at least one identifier
      std::string id = ParseIdentifier();

      // create a symbol for the enum
      Symbol enum_symbol;
      enum_symbol.SetId(id);
      // it is global if the symbol is global
      enum_symbol.SetIsGlobal(symbol.IsGlobal());
      enum_symbol.SetDeclaration(DECLARATION_ENUM);
      enum_symbol.SetType(TYPE_INT);
      enum_symbol.SetEnumValue(enum_value);
      if (codegen_) {
        // give it the correct value
        llvm::Value *val = llvm::ConstantInt::getIntegerValue(
            GetRespectiveLLVMType(TYPE_INT),
            llvm::APInt(32, enum_value, true));
        enum_symbol.SetLLVMValue(val);
      }
      symbol_table_.InsertSymbol(enum_symbol);
      enum_value++;

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
        id = ParseIdentifier();
        enum_symbol = Symbol();
        enum_symbol.SetId(id);
        // it is global if the overall symbol is global
        enum_symbol.SetIsGlobal(symbol.IsGlobal());
        enum_symbol.SetDeclaration(DECLARATION_ENUM);
        enum_symbol.SetType(TYPE_INT);
        enum_symbol.SetEnumValue(enum_value);
        if (codegen_) {
          // give it the correct llvm value
          llvm::Value *val = llvm::ConstantInt::getIntegerValue(
              GetRespectiveLLVMType(TYPE_INT),
              llvm::APInt(32, enum_value, true));
          enum_symbol.SetLLVMValue(val);
        }
        symbol_table_.InsertSymbol(enum_symbol);
        enum_value++;

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

  if (codegen_) {
    // if it's a global variable, create it in llvm land
    if (variable_symbol.IsGlobal()) {
      // need to declare a constant to initialize it to
      llvm::Type *global_type = nullptr;
      if (variable_symbol.IsArray()) {
        global_type = llvm::ArrayType::get(
            GetRespectiveLLVMType(variable_symbol.GetType()),
            variable_symbol.GetArrayBound());
      } else {
        global_type = GetRespectiveLLVMType(variable_symbol.GetType());
      }
      llvm::Constant *constant_initializer = llvm::Constant::getNullValue(
          global_type);
      llvm::Value *val = new llvm::GlobalVariable(
          *llvm_module_,
          global_type,
          false,
          llvm::GlobalValue::CommonLinkage,
          constant_initializer,
          variable_symbol.GetId());

      // global variables are considered initialized
      variable_symbol.SetHasBeenInitialized(true);

      if (variable_symbol.IsArray()) {
        variable_symbol.SetLLVMArrayAddress(val);
        // also, because it's an array, it expects to have a bound value
        llvm::Value *bound = llvm::ConstantInt::getIntegerValue(
            GetRespectiveLLVMType(TYPE_INT),
            llvm::APInt(32, variable_symbol.GetArrayBound(), true));
        variable_symbol.SetLLVMBound(bound);
      } else {
        variable_symbol.SetLLVMAddress(val);
      }
    }
  }

  // check that the symbol name doesn't conflict
  if (symbol_table_.CheckForSymbolCollisions(variable_symbol.GetId())) {
    EmitError("Identifier collision. Identifier already exists.", lexeme);
    return;
  }

  // commit the symbol to the symbol table
  symbol_table_.InsertSymbol(variable_symbol);
}

} // namespace kjlc
