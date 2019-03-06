/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * Header file for the Parser Class
 **/
#ifndef EECE5138COMPILER_KJLC_PARSER_H_
#define EECE5138COMPILER_KJLC_PARSER_H_

#include "scanner.h"

namespace kjlc {

class Parser {
  public:
    // Constructor sets things up for parsing.
    // takes a string of the file for parsing.
    Parser(std::string filename, bool debug);

    // Deconstructor cleans up any allocated memory.
    ~Parser();

    // Parse Program. Starting point of the parse
    void ParseProgram();
    
  private:
    // Scanner being used to drive the parse
    Scanner scanner_;
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
    void LoopDeclarations(Token end_tokens[], int tokens_length);

    // Loop parsing statements until one of the end tokens is peeked
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
    void ParseBound();

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
    void ParseIdentifier();

    // Handle parsing if statement
    void ParseIfStatement();

    // Handle parsing loop statement
    void ParseLoopStatement();

    // Handle parsing number
    void ParseNumber();

    // Handle parsing a parameter
    void ParseParameter();

    // Handle parsing parameter list
    void ParseParameterList();

    // Handle parsing a procedure body
    void ParseProcedureBody();

    // Handle parsing a procedure declaration
    void ParseProcedureDeclaration();

    // Handle parsing procedure header
    void ParseProcedureHeader();

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
    void ParseTypeDeclaration();

    // Parse Type Mark
    void ParseTypeMark();

    // Parse Variable Declaration
    void ParseVariableDeclaration();

};

} // namespace kjlc
#endif // EECE5138COMPILER_KJLC_SCANNER_H_
