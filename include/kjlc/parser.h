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
    Parser(std::string filename);

    // Deconstructor cleans up any allocated memory.
    ~Parser();

    // Parse Program. Starting point of the parse
    void ParseProgram();
    
  private:
    // Scanner being used to drive the parse
    Scanner scanner_;
    // True if error state
    bool error_state_;

    // Generic Error Handling function
    void EmitParsingError(std::string message, Lexeme lexeme);

    // Error for missing token
    void EmitExpectedTokenError(std::string expected_token, Lexeme lexeme);

    //
    // Parse rules functions
    //

    // Handle parsing argument list
    void ParseArgumentList();

    // hadnle parsing arithmetic operators
    void ParseArithOp();

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

    // Handle Parsing Identifier r ule
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

    // Handle parsing a procedure call
    void ParseProcedureCall();
    
    // Handle parsing a procedure declaration
    void ParseProcedureDeclaration();

    // Handle parsing procedure header
    void ParseProcedureHeader();

    // Parse the Program body rule
    void ParseProgramBody();

    // Parse the Program Header Rule
    void ParseProgramHeader();

    // Handle parsing return statements
    void ParseReturnStatement();
    
    // Handle parsing Statement
    void ParseStatement();

    // Parse Type Declaration
    void ParseTypeDeclaration();

    // Parse Type Mark
    void ParseTypeMark();

    // Parse Variable Declaration
    void ParseVariableDeclaration();

};

} // namespace kjlc
#endif // EECE5138COMPILER_KJLC_SCANNER_H_
