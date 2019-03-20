/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * Header file for the Parser Class
 **/
#ifndef EECE5138COMPILER_KJLC_PARSER_H_
#define EECE5138COMPILER_KJLC_PARSER_H_

#include "scanner.h"
#include "symbol_table.h"

namespace kjlc {

class Parser {
  public:
    // Constructor sets things up for parsing.
    // takes a string of the file for parsing.
    Parser(std::string filename, bool parser_debug, bool symbol_debug);

    // Deconstructor cleans up any allocated memory.
    ~Parser();

    // Parse Program. Starting point of the parse
    void ParseProgram();
    
  private:
    // Scanner being used to drive the parse
    Scanner scanner_;
    // Symbol table
    SymbolTable symbol_table_;
    // True if error state
    bool error_state_;
    // true if parse should end
    bool end_parse_;
    // flag for debug state
    bool debug_;

    // Prints out debugging information when requested
    // parse_function: name of parse rule being expanded
    void DebugPrint(std::string parse_function);

    // Generic Error Handling function
    void EmitParsingError(std::string message, Lexeme lexeme);

    // Error for missing token
    void EmitExpectedTokenError(std::string expected_token, Lexeme lexeme);

    // consumes tokens until the next one is in the tokens list
    // does not consume the token that is in the tokens list
    // should only be called in an error_state_ but will handle not being in one
    // always considers T_PERIOD for end of file
    void ResyncOnTokens(Token tokens[], int tokens_length);

    // Loop Parsing declarations until one of the end tokens is peeked
    // built for code deduplication, everywhere that has (parse_declaration)*
    // is the exact same with possibly different end tokens
    void LoopDeclarations(Token end_tokens[], int tokens_length);

    // Loop parsing statements until one of the end tokens is peeked
    // built for code deduplication, everywhere that has (parse_statement)*
    // is the exact same, with possibly different end tokens
    void LoopStatements(Token end_tokens[], int tokens_length);

    //
    // Parse rules functions
    //

    // Handle parsing argument list
    void ParseArgumentList();

    // handle parsing arithmetic operators
    void ParseArithOp();

    // handle parsing the right recursive ArithOp rule
    void ParseArithOpTail();

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
    void ParseDestination();
    
    // Handle Parsing expression
    void ParseExpression();

    // Handle right recursive part of ParseExpression
    void ParseExpressionTail();

    // Handle parsing factor rule
    void ParseFactor();

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
    void ParseNumber();
    
    // Parses a numeric literal, but insists it must be a float value
    // returns the float literal
    float ParseNumberFloat();

    // Parses a numeric literal, but insists it must be an int value
    // returns the int literal
    int ParseNumberInteger();

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
    void ParseReference();

    // Handle parsing relation rule
    void ParseRelation();

    // Handle parsing right recursive Relation rule
    void ParseRelationTail();

    // Handle parsing return statements
    void ParseReturnStatement();
    
    // Handle parsing Statement
    void ParseStatement();

    // Handle parsings tring
    void ParseString();

    // Handle parsing term
    void ParseTerm();
    
    // handle parsing right recursive term
    void ParseTermTail();

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
#endif // EECE5138COMPILER_KJLC_SCANNER_H_
