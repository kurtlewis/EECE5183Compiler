/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * Header file for the Parser Class
 **/
#ifndef EECE5183COMPILER_KJLC_PARSER_H_
#define EECE5183COMPILER_KJLC_PARSER_H_

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "scanner.h"
#include "symbol.h"
#include "symbol_table.h"

namespace kjlc {

class Parser {
  public:
    // Constructor sets things up for parsing.
    // takes a string of the file for parsing.
    Parser(std::string filename, bool parser_debug, bool symbol_debug,
           bool codegen_enable, bool codegen_debug);

    // Deconstructor cleans up any allocated memory.
    ~Parser();

    // Parse Program. Starting point of the parse
    void ParseProgram();

    // Builds LLVM IR
    void BuildProgram();
    
  private:
    // Scanner being used to drive the parse
    Scanner scanner_;
    // Symbol table
    SymbolTable symbol_table_;
    // flag for doing array unwrap 
    // our language supports acting on entire arrays at once
    // which requires behind the scenes unwinding 
    bool array_unwrap_;
    // max index for type checking on an array index to make sure all arrays
    // are the correct size
    int array_unwrap_bound_;
    // count of the number of functions for function naming
    int func_count_;
    // True if error state
    bool error_state_;
    // true if parse should end
    bool end_parse_;
    // flag for debug state
    bool parser_debug_;
    // flag to enable code_gen
    bool codegen_;
    // flag for codegen_debug
    bool codegen_debug_;


    //
    // LLVM Member variables
    //
    // The module that contains all compiled code
    llvm::Module *llvm_module_;
    // global context for compiler - default constructor creates one
    llvm::LLVMContext llvm_context_;
    // current function
    // set in the header for whatever is being added to
    llvm::Function *llvm_current_procedure_;
    // the current builder
    llvm::IRBuilder<> *llvm_builder_;
    // index if an array unwrap is going on
    llvm::Value *llvm_array_unwrap_index_;
    // address of index for load storing
    llvm::Value *llvm_array_unwrap_index_address_;
    // blocks for the for loop for unwrapping the array
    llvm::BasicBlock *llvm_array_unwrap_loop_header_block_;
    llvm::BasicBlock *llvm_array_unwrap_loop_end_block_;

    // Prints out debugging information when requested
    // parse_function: name of parse rule being expanded
    void DebugPrint(std::string parse_function);

    // TODO: I probably need to rethink the implications of having my
    // error output functions set the program to an error state

    //
    // Error Handlers
    // All error handles set an error_state_ (which can be recovered from)
    // and turn off codegen (which there is no recovery from)
    //

    // Error hanlding message that only prints the location of a lexeme
    void EmitError(Lexeme lexeme);

    // Generic Error Handling function
    void EmitError(std::string message, Lexeme lexeme);

    // Error for missing token
    void EmitExpectedTokenError(std::string expected_token, Lexeme lexeme);

    // Error for when types mismatch
    void EmitExpectedTypeError(std::string expected_type,
                               std::string found_type,
                               Lexeme lexeme);

    // Error Handler for incompatible types on operations
    void EmitOperationTypeCheckingError(std::string operation,
                                        std::string type1,
                                        std::string type2,
                                        Lexeme lexeme);

    // Warning Handler - doesn't print if in error_state_, but doesn't 
    // set error_state_ to true
    void EmitWarning(std::string message, Lexeme lexeme);


    // consumes tokens until the next one is in the tokens list
    // does not consume the token that is in the tokens list
    // should only be called in an error_state_ but will handle not being in one
    // always considers T_PERIOD for end of file
    void ResyncOnTokens(Token tokens[], int tokens_length);

    //
    // Parse code deduplication rules
    //

    // Loop Parsing declarations until one of the end tokens is peeked
    // built for code deduplication, everywhere that has (parse_declaration)*
    // is the exact same with possibly different end tokens
    void LoopDeclarations(Token end_tokens[], int tokens_length);

    // Loop parsing statements until one of the end tokens is peeked
    // built for code deduplication, everywhere that has (parse_statement)*
    // is the exact same, with possibly different end tokens
    void LoopStatements(Token end_tokens[], int tokens_length);

    // Parse Index statements, i.e. array [ <expression> ]
    // split out from existing references to reduce code duplication
    // returns an updated symbol to reflect the address that is being
    // referenced
    Symbol ParseIndex(Symbol identifier);

    //
    // Type Check / Codegen Functions for Right Recursive rules
    //

    // Checks the lead and tail symbols to make sure they are type compatible
    // for use in ParseArithOp, ParseTerm, ParseArithOpTail, ParseTermTail
    // if the types are compatible, does appropriate codegen
    // params:
    //   type_context - destination type/expected result
    //   lead - output of ParseFactor or ParseRelation
    //   tail - output of ParseTermTail or ParseArithOp, can be invalid
    //   operation - lexeme of the operation being done. Used for type checking
    //               error reporting as well.
    Symbol DoArithmeticTypeCheckingAndCodegen(Symbol type_context,
                                              Symbol lead,
                                              Symbol tail,
                                              Lexeme operation);
    
    // Checks for the assignment rules for interoperable types
    // and if they are interoperable, automatically converts to the destination
    // type and returns the expression as that type
    // params:
    //   destination - destination symbol to convert type to
    //   expression - the expression we're checking to see if needs converted
    //   location - location for possible warning/error printing
    Symbol DoAssignmentTypeCheckingAndConversionCodegen(Symbol destination,
                                                        Symbol expression,
                                                        Lexeme location);

    // Checks the lead and tail symbols for binary operation compatibility
    // for use in ParseExpression and ParseExpressionTail
    // params:
    //   type_context - destination type/expected result
    //   arith_op - output of ParseArithOp
    //   expression_tail - output of ParseExpressionTail, can be invalid
    //   operation - lexeme before the tail, used to determine operation and
    //               for error reporting 
    //   not_operation - boolean representing if the result or arith_op has
    //                   a not operation being applied to it
    Symbol DoExpressionTypeCheckingAndCodegen(Symbol type_context,
                                              Symbol arith_op,
                                              Symbol expression_tail,
                                              Lexeme operation,
                                              bool not_operation);

    // Checks the term and relation_tail to make sure they are type compatible
    // for use in ParseRelation and ParseRelationTail
    // if the types are compatible, it does codegen
    // params:
    //   type_context - destination type/expected result
    //   term - output of ParseTerm
    //   relation_tail - output of ParseRelationTail, can be invalid
    //   operation - lexeme before the tail, used to determine operation and
    //               for error reporting 
    Symbol DoRelationTypeCheckingAndCodegen(Symbol type_context,
                                            Symbol term,
                                            Symbol relation_tail,
                                            Lexeme operation);


    //
    // Codegen Utility functions 
    //
    llvm::Type* GetRespectiveLLVMType(Symbol symbol);

    llvm::Type* GetRespectiveLLVMType(Type type);

    // converts a llvm integer value to an llvm boolean value
    // following the language semantic rules for conversion
    llvm::Value *ConvertLLVMIntegerToBoolean(llvm::Value *incoming_val);

    //
    // Parse rules functions
    //

    // Handle parsing argument list
    // params:
    //   param_current - iterator to the current parameter, non-recusrive calls
    //                   should be the begin iterator of the vector
    //   param_end - iterator to the end of the vector
    // returns a vector of the values the expressions in the argument list
    // evaluate to
    std::vector<llvm::Value *> ParseArgumentList(
        std::vector<Symbol>::iterator param_current,
        std::vector<Symbol>::iterator param_end);

    // handle parsing arithmetic operators
    Symbol ParseArithOp(Symbol type_context);

    // handle parsing the right recursive ArithOp rule
    Symbol ParseArithOpTail(Symbol type_context);

    // Handle parsing assignment statement
    void ParseAssignmentStatement();

    // Handle parsing bound
    // @params:
    //   symbol - symbol to insert bound information into as the array is
    //            defined
    void ParseBound(Symbol &symbol);

    // Handle parsing declarationr ule
    void ParseDeclaration();

    // Hanlde parsing destination
    Symbol ParseDestination();
    
    // Handle Parsing expression
    // @param:
    //   Symbol type_context - a symbol reperesenting the expected type
    //                         the expression will evaluate to
    Symbol ParseExpression(Symbol type_context);

    // Handle right recursive part of ParseExpression
    // @param:
    //   Symbol type_context - a symbol reperesenting the expected type
    //                         the expression will evaluate to
    Symbol ParseExpressionTail(Symbol type_context);

    // Handle parsing factor rule
    // @param:
    //   Symbol type_context - a symbol reperesenting the expected type
    //                         the expression will evaluate to
    Symbol ParseFactor(Symbol type_context);

    // Handle Parsing Identifier rule
    // @return - string representation of the identifier
    std::string ParseIdentifier();

    // Handle parsing if statement
    void ParseIfStatement();


    // Handle parsing loop statement
    void ParseLoopStatement();

    // Handle parsing number
    // does not return a value for the parsed number
    // see ParseNumberInteger or ParseNumberFloat if a value is needed
    Symbol ParseNumber();
    
    // Handle parsing a parameter
    // @params:
    //    Symbol &procedure_symbol - procedure symbol to place parameters in
    void ParseParameter(Symbol &procedure_symbol);

    // Handle parsing parameter list
    // @params:
    //    Symbol &procedure_symbol - procedure symbol to place parameters in
    void ParseParameterList(Symbol &procedure_symbol);

    // Handle parsing a procedure body
    void ParseProcedureBody();

    // Handle parsing a procedure declaration
    // @params:
    //   Symbol &procedure_symbol - Symbol for procedure being declared
    void ParseProcedureDeclaration(Symbol &procedure_symbol);

    // Handle parsing procedure header
    // @params:
    //   Symbol &procedure_symbol - Symbol for procedure being declared
    void ParseProcedureHeader(Symbol &procedure_symbol);

    // Parse the Program body rule
    void ParseProgramBody();

    // Parse the Program Header Rule
    void ParseProgramHeader();
    
    // Handle parsing references. Combination of <procedure_call> and <name>
    Symbol ParseReference();

    // Handle parsing relation rule
    // @param:
    //   Symbol type_context - a symbol reperesenting the expected type
    //                         the expression will evaluate to
    Symbol ParseRelation(Symbol type_context);

    // Handle parsing right recursive Relation rule
    // @param:
    //   Symbol type_context - a symbol reperesenting the expected type
    //                         the expression will evaluate to
    Symbol ParseRelationTail(Symbol type_context);

    // Handle parsing return statements
    void ParseReturnStatement();
    
    // Handle parsing Statement
    void ParseStatement();

    // Handle parsings tring
    Symbol ParseString();

    // Handle parsing term
    // @param:
    //   Symbol type_context - a symbol reperesenting the expected type
    //                         the expression will evaluate to
    Symbol ParseTerm(Symbol type_context);
    
    // handle parsing right recursive term
    // @param:
    //   Symbol type_context - a symbol reperesenting the expected type
    //                         the expression will evaluate to
    Symbol ParseTermTail(Symbol type_context);

    // Parse Type Declaration
    // @params:
    //   Symbol &type_symbol - Symbol for type being declared
    void ParseTypeDeclaration(Symbol &type_symbol);

    // Parse Type Mark
    // @params:
    //   Symbol &symbol - the symbol to update about type information
    void ParseTypeMark(Symbol &symbol);

    // Parse Variable declaration
    // Overload for calling without a symbol already created
    void ParseVariableDeclaration();

    // Parse Variable Declaration
    // @params
    // Symbol &variable_symbol - symbol for the variable being declared
    void ParseVariableDeclaration(Symbol &variable_symbol);

};

} // namespace kjlc
#endif // EECE5183COMPILER_KJLC_SCANNER_H_
