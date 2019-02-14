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
Parser::Parser(std::string filename) : scanner_(filename), peeked_(false) {

}

// Deconstructor
Parser::~Parser() {

}

Lexeme Parser::GetNextLexeme() {
  if (peeked_) {
    peeked_ = false;
    return next_lexeme_;
  } else {
    return scanner_.ScanNextLexeme();
  }
}

Lexeme Parser::PeekNextLexeme() {
  if (peeked_) {
    return next_lexeme_;
  } else {
    peeked_ = true;
    next_lexeme_ = scanner_.ScanNextLexeme();
    return next_lexeme_;
  }
}
    

void Parser::ParseIfStatement() {
  Lexeme lexeme = GetNextLexeme(); 
  if (lexeme.token != T_IF) {
    // p&d
  }

  lexeme = GetNextLexeme();
  if (lexeme.token != T_PAREN_LEFT) {
    // p&d
  }

  // handle parsing the expression
  //ReturnType ret = ParseExpression();

  lexeme = GetNextLexeme();
  if (lexeme.token != T_PAREN_RIGHT) {
    // p&d
  }

  lexeme = GetNextLexeme(); 
  if (lexeme.token != T_THEN) {
    // p&d
  }

  // only read statements until we peek a T_ELSE or T_END
  // it is required that there is at least one statement
  while (lexeme.token != T_ELSE && lexeme.token != T_END) {
    // Handle parsing the statement
    // ReturnType ret = ParseStatement();
    lexeme = GetNextLexeme();
    if (lexeme.token != T_SEMI_COLON) {
      // p&d
    }
    lexeme = PeekNextLexeme();
  }

  // the last lexeme was only peeked, read it now
  lexeme = GetNextLexeme();
  if (lexeme.token == T_ELSE) {
    // handle else statement
    while (lexeme.token != T_END) {
      // handle parsing the statements
      // ReturnType ret = ParseStatement();
      lexeme = GetNextLexeme();
      if (lexeme.token != T_SEMI_COLON) {
        // p&d
      }
      lexeme = PeekNextLexeme();
    }
    // read the peeeked lexeme
    lexeme = GetNextLexeme();
  }

  // now at end
  if (lexeme.token != T_END) {
    // p&d
  }

  // 'if'
  lexeme = GetNextLexeme();
  if (lexeme.token != T_IF) {
    // p&d
  }
}

} // namespace kjlc
