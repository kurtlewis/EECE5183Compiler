/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * Scanner handles tokenizing input content
 **/
#include "kjlc/scanner.h"

#include <algorithm>
#include <iostream>
#include <sstream>

namespace kjlc {

Scanner::Scanner(std::string filename)
    : file_(filename.c_str(), std::fstream::in) {
  // get token mapping
  this->token_map_ = Scanner::generate_token_mapping();
  // mark the file as incomplete
  this->file_complete_ = false;
  this->line_ = 1;
  this->column_ = 1;
}

Scanner::~Scanner() {

}

kjlc::Lexeme Scanner::ScanNextLexeme() {
  // create return struct
  struct kjlc::Lexeme lexeme;
  // mark the line and column the token starts on
  lexeme.line = this->line_;
  lexeme.column = this->column_;

  // pull the char
  char ch = ScanNextChar();
  if (ch == '\00' || this->file_complete_) {
    lexeme.token = T_PERIOD;
    return lexeme;
  }

  // scan until a non-whitespace character is found
  while (IsWhiteSpace(ch) && !this->file_complete_) {
    ch = ScanNextChar();
  }

  //
  // handle possible comments
  //
  if (ch == '/') {
    // it could be a comment
    char nCh = PeekNextChar();
    if (nCh == '/' || nCh == '*') {
      // it is a comment - how it's handled depends on the type
      if (nCh == '/') {
        // it is a one line comment
        // read until new line
        while (ch != '\n' && !this->file_complete_) {
          ch = ScanNextChar();
        }
      } else if (nCh == '*') {
        // it is a block comment
        // read until the comment ends
        bool isOver = false;
        while (!isOver && !this->file_complete_) {
          ch = ScanNextChar();
          if (ch == '*') {
            // current char is '*', next char could be '/' ending comment
            nCh = PeekNextChar();
            if (nCh == '/') {
              // Comment is ending
              isOver = true;
              // move forward to where we peeked
              ScanNextChar();
            }
          }
        }
      }
      // start over and return the next Lexeme
      return ScanNextLexeme();
    }
    // it was not a comment, so fall through
  }

  //
  // handle quotes
  //
  if (ch == '"') {
    // its the start of a quote. Rules are different, keep all whitespace until
    // the end of the quote is found
    std::ostringstream quote;
    quote << ch;
    ch = '\00'; // set char to null as to not stop while loop execution
    while (ch != '"' && !this->file_complete_) {
      ch = ScanNextChar();
      quote << ch;
    }
    lexeme.token = T_QUOTE;
    lexeme.str_value = quote.str();
    return lexeme;
  }

  // check to make sure it isn't a two character token
  std::string twoCharString = std::string(1, ch) + PeekNextChar();
  std::map<std::string, kjlc::Token>::iterator twoCharResult
    = this->token_map_.find(twoCharString);
  if (twoCharResult != this->token_map_.end()) {
    // it is a two token reserved word, return that token type
    lexeme.token = twoCharResult->second;
    return lexeme;
  }

  // check to see if character is in the reserved token map
  std::map<std::string, kjlc::Token>::iterator singleCharResult
    = this->token_map_.find(std::string(1, ch));
  if (singleCharResult != this->token_map_.end()) {
    // the single character is in the map
    lexeme.token = singleCharResult->second;
    return lexeme;
  }

  if ('A' <= ch && ch <= 'z') {
    // it is going to be an identifier or reserved word
    std::ostringstream wordStream;
    wordStream << ch;
    while (!IsWhiteSpace(ch) && !this->file_complete_) {
      wordStream << ch;
      ch = ScanNextChar();
    }
    std::string word = wordStream.str();
    // convert string to lowercase since language is not case sensitive
    std::transform(word.begin(), word.end(), word.begin(), ::tolower);
    
    // check to see if the word is a reserved word
    std::map<std::string, kjlc::Token>::iterator wordResult = 
      this->token_map_.find(word);
    if (wordResult != this->token_map_.end()) {
      // this word is a reserved word
      lexeme.token = wordResult->second;
      return lexeme;
    }
    // it is not a reserved word
    lexeme.token = T_ID;
    lexeme.str_value = word;
    return lexeme;
  }

  if ('0' <= ch && ch <= '9') {
    // it is a number constant
    std::ostringstream numStream;
    numStream << ch;
    char nextCh = PeekNextChar();
    while ('0' <= nextCh && nextCh <= '9') {
      ch = ScanNextChar();
      numStream << ch;
      nextCh = PeekNextChar();
    }
    lexeme.token =T_NUM;
    lexeme.int_value = T_NUM;
    return lexeme;
  }
  lexeme.token = T_UNKNOWN;
  return lexeme;
}

std::map<std::string, kjlc::Token> Scanner::generate_token_mapping() {
  std::map<std::string, kjlc::Token> map = std::map<std::string, enum Token>();
  // reserved words
  map["begin"] = kjlc::T_BEGIN;
  map["else"] = kjlc::T_ELSE;
  map["end"] = kjlc::T_END;
  map["false"] = kjlc::T_FALSE;
  map["for"] = kjlc::T_FOR;
  map["global"] = kjlc::T_GLOBAL;
  map["if"] = kjlc::T_IF;
  map["is"] = kjlc::T_IS;
  map["not"] = kjlc::T_NOT;
  map["procedure"] = kjlc::T_PROCEDURE;
  map["program"] = kjlc::T_PROGRAM;
  map["return"] = kjlc::T_RETURN;
  map["then"] = kjlc::T_THEN;
  map["true"] = kjlc::T_TRUE;
  map["type"] = kjlc::T_TYPE;
  map["variable"] = kjlc::T_VARIABLE;
  // character tokens
  map["["] = kjlc::T_BRACK_LEFT;
  map["]"] = kjlc::T_BRACK_RIGHT;
  map[","] = kjlc::T_COMMA;
  map["{"] = kjlc::T_CURLY_LEFT;
  map["}"] = kjlc::T_CURLY_RIGHT;
  map["("] = kjlc::T_PAREN_LEFT;
  map[")"] = kjlc::T_PAREN_RIGHT;
  map["."] = kjlc::T_PERIOD;
  map["\""] = kjlc::T_QUOTE;
  map[";"] = kjlc::T_SEMI_COLON;
  // operators
  map["&"] = kjlc::T_AND;
  map["/"] = kjlc::T_DIV;
  map["=="] = kjlc::T_EQ;
  map[">"] = kjlc::T_GT;
  map[">="] = kjlc::T_GT_EQ;
  map["<"] = kjlc::T_LT;
  map["<="] = kjlc::T_LT_EQ;
  map["-"] = kjlc::T_MINUS;
  map["*"] = kjlc::T_MULT;
  map["!="] = kjlc::T_NEQ;
  map["+"] = kjlc::T_PLUS;
  // types
  map["bool"] = kjlc::T_BOOL;
  map["enum"] = kjlc::T_ENUM;
  map["float"] = kjlc::T_FLOAT;
  map["integer"] = kjlc::T_INT;
  map["string"] = kjlc::T_STRING;

  // return map
  return map;
}

char Scanner::ScanNextChar() {
  // check that the file is open
  if (!this->file_.is_open()) {
    this->file_complete_ = true;
    return '\00'; // return null char
  }

  // get the character
  char ch;
  this->file_.get(ch);
  this->file_complete_ = this->file_.eof();

  // handle line and column counting
  this->column_++;
  if (ch == '\n') {
    // if its a newline, reset the line count
    this->line_++;
    this->column_ = 1;
  }
  return ch;
}

char Scanner::PeekNextChar() {
  return this->file_.peek();
}

bool Scanner::IsWhiteSpace(char ch) {
  return (ch == '\n' || ch == ' ' || ch == '\t');
}

} // end kjlc namespace