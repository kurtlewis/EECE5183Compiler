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
Parser::Parser(std::string filename) : scanner_(filename), error_state_(false) {

}

// Deconstructor
Parser::~Parser() {

}

void Parser::EmitParsingError(std::string message, Lexeme lexeme) {
  std::cout << "Line:" << lexeme.line << " Col:" << lexeme.column;
  std::cout << " - " << message << std::endl;
  error_state_ = true;
}

void Parser::EmitExpectedTokenError(std::string expected_token, Lexeme lexeme) {
  std::cout << "Line:" << lexeme.line << " Col:" << lexeme.column;
  std::cout << " - " << "Expected token '" << expected_token << "'";
  std::cout << std::endl;
  error_state_ = true;
}

void Parser::ParseIfStatement() {
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
  //ReturnType ret = ParseExpression();

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

  // only read statements until we peek a T_ELSE or T_END
  // it is required that there is at least one statement
  while (lexeme.token != T_ELSE && lexeme.token != T_END) {
    // Handle parsing the statement
    // ReturnType ret = ParseStatement();
    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_SEMI_COLON) {
      EmitExpectedTokenError(";", lexeme);
      return;
    }
    lexeme = scanner_.PeekNextLexeme();
  }

  // the last lexeme was only peeked, read it now
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token == T_ELSE) {
    // handle else statement
    while (lexeme.token != T_END) {
      // handle parsing the statements
      // ReturnType ret = ParseStatement();
      lexeme = scanner_.GetNextLexeme();
      if (lexeme.token != T_SEMI_COLON) {
        EmitExpectedTokenError(";", lexeme);
        return;
      }
      lexeme = scanner_.PeekNextLexeme();
    }
    // read the peeeked lexeme
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

} // namespace kjlc
