/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * Scanner handles tokenizing input content
 **/
#include "kjlc/scanner.h"

#include <iostream>
#include <sstream>

namespace kjlc {

Scanner::Scanner(std::string filename)
    : file(filename.c_str(), std::fstream::in) {
  // get token mapping
  tokMap = Scanner::generate_token_mapping();
  // mark the file as incomplete
  fileComplete = false;
}

Scanner::~Scanner() {

}

kjlc::Token Scanner::scanNextLexeme() {
  // pull the char
  char ch = scanNextChar();
  if (ch == '\00' || fileComplete) {
    return T_PERIOD;
  }

  // scan until a non-whitespace character is found
  while (isWhiteSpace(ch)) {
    ch = scanNextChar();
  }
  if (ch == '/') {
    // it could be a comment
    char nCh = peekNextChar();
    if (nCh == '/' || nCh == '*') {
      // it is a comment - how it's handled depends on the type
      if (nCh == '/') {
        // it is a one line comment
        // read until new line
        while (ch != '\n') {
          ch = scanNextChar();
        }
      } else if (nCh == '*') {
        // it is a block comment
        // read until the comment ends
        bool isOver = false;
        while (!isOver) {
          ch = scanNextChar();
          if (ch == '*') {
            // current char is '*', next char could be '/' ending comment
            nCh = peekNextChar();
            if (nCh == '/') {
              // Comment is ending
              isOver = true;
              // move forward to where we peeked
              scanNextChar();
            }
          }
        }
      }
      // start over and return the next Lexeme
      return scanNextLexeme();
    }
    // it was not a comment, so fall through
  }

  if ('A' <= ch && ch <= 'z') {
    // it is going to be an identifier or reserved word
    std::ostringstream wordStream;
    wordStream << ch;
    while (isWhiteSpace(ch)) {
      wordStream << ch;
    }
    return T_ID;
  } else if (ch == '.') {
    return T_PERIOD;
  }
}

std::map<kjlc::Token, std::string> Scanner::generate_token_mapping() {
  std::map<kjlc::Token, std::string> map = std::map<enum Token, std::string>();
  // reserved words
  map[kjlc::T_BEGIN] = "begin"; 
  map[kjlc::T_ELSE] = "else";
  map[kjlc::T_END] = "end";
  map[kjlc::T_FALSE] = "false";
  map[kjlc::T_FOR] = "for";
  map[kjlc::T_GLOBAL] = "global";
  map[kjlc::T_IF] = "if";
  map[kjlc::T_IS] = "is";
  map[kjlc::T_NOT] = "not";
  map[kjlc::T_PROCEDURE] = "procedure";
  map[kjlc::T_PROGRAM] = "program";
  map[kjlc::T_RETURN] = "return";
  map[kjlc::T_THEN] = "then";
  map[kjlc::T_TRUE] = "true";
  map[kjlc::T_TYPE] = "type";
  map[kjlc::T_VARIABLE] = "variable";
  // character tokens
  map[kjlc::T_BRACK_LEFT] = "]";
  map[kjlc::T_BRACK_RIGHT] = "]";
  map[kjlc::T_COMMA] = ",";
  map[kjlc::T_CURLY_LEFT] = "{";
  map[kjlc::T_CURLY_RIGHT] = "}";
  map[kjlc::T_PAREN_LEFT] = "(";
  map[kjlc::T_PAREN_RIGHT] = ")";
  map[kjlc::T_PERIOD] = ".";
  map[kjlc::T_QUOTE] = "\"";
  map[kjlc::T_SEMI_COLON] = ";";
  // operators
  map[kjlc::T_AND] = "&";
  map[kjlc::T_DIV] = "/";
  map[kjlc::T_EQ] = "==";
  map[kjlc::T_GT] = ">";
  map[kjlc::T_GT_EQ] = ">=";
  map[kjlc::T_LT] = "<";
  map[kjlc::T_LT_EQ] = "<=";
  map[kjlc::T_MINUS] = "-";
  map[kjlc::T_MULT] = "*";
  map[kjlc::T_NEQ] = "!=";
  map[kjlc::T_PLUS] = "+";
  // types
  map[kjlc::T_BOOL] = "bool";
  map[kjlc::T_ENUM] = "enum";
  map[kjlc::T_FLOAT] = "float";
  map[kjlc::T_INT] = "integer";
  map[kjlc::T_STRING] = "string";

  // return map
  return map;
}

char Scanner::scanNextChar() {
  // check that the file is open
  if (!this->file.is_open()) {
    fileComplete = true;
    return '\00'; // return null char
  }
  char ch;
  this->file >> std::noskipws >> ch;
  return ch;
}

char Scanner::peekNextChar() {
  // get current position
  //fpos_t position;
  //fgetpos(this->file, &position);
  // get next char
  //char ch = scanNextChar();
  // rewind file pointer
  //fsetpos(this->file, &position);
  //return ch;
  return this->file.peek();
}

bool Scanner::isWhiteSpace(char ch) {
  return (ch == '\n' || ch == ' ' || ch == '\t');
}

} // end kjlc namespace