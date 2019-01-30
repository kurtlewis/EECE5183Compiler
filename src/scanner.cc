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
    : file(filename.c_str(), std::fstream::in) {
  // get token mapping
  this->tokMap = Scanner::generate_token_mapping();
  // mark the file as incomplete
  this->fileComplete = false;
}

Scanner::~Scanner() {

}

kjlc::Token Scanner::scanNextLexeme() {
  // pull the char
  char ch = scanNextChar();
  if (ch == '\00' || this->fileComplete) {
    return T_PERIOD;
  }

  // scan until a non-whitespace character is found
  while (isWhiteSpace(ch) && !this->fileComplete) {
    ch = scanNextChar();
  }

  //
  // handle possible comments
  //
  if (ch == '/') {
    // it could be a comment
    char nCh = peekNextChar();
    if (nCh == '/' || nCh == '*') {
      // it is a comment - how it's handled depends on the type
      if (nCh == '/') {
        // it is a one line comment
        // read until new line
        while (ch != '\n' && !this->fileComplete) {
          ch = scanNextChar();
        }
      } else if (nCh == '*') {
        // it is a block comment
        // read until the comment ends
        bool isOver = false;
        while (!isOver && !this->fileComplete) {
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

  //
  // handle quotes
  //
  if (ch == '"') {
    // its the start of a quote. Rules are different, keep all whitespace until
    // the end of the quote is found
    std::ostringstream quote;
    quote << ch;
    ch = '\00'; // set char to null as to not stop while loop execution
    while (ch != '"' && !this->fileComplete) {
      ch = scanNextChar();
      quote << ch;
    }
    return T_QUOTE;
  }

  // check to see if character is in the reserved token map
  std::map<std::string, kjlc::Token>::iterator singleCharResult
    = this->tokMap.find(std::string(1, ch));
  if (singleCharResult != this->tokMap.end()) {
    // the single character is in the map
    // check to make sure it isn't a two character token
    std::string twoCharString = std::string(1, ch) + peekNextChar();
    std::map<std::string, kjlc::Token>::iterator twoCharResult
      = this->tokMap.find(twoCharString);
    if (twoCharResult != this->tokMap.end()) {
      // it is a two token reserved word, return that token type
      return twoCharResult->second;
    } else {
      // it is only a one character reserved word, return that token type
      return singleCharResult->second;
    }
  }

  if ('A' <= ch && ch <= 'z') {
    // it is going to be an identifier or reserved word
    std::ostringstream wordStream;
    wordStream << ch;
    while (!isWhiteSpace(ch) && !this->fileComplete) {
      wordStream << ch;
      ch = scanNextChar();
    }
    std::string word = wordStream.str();
    // convert string to lowercase since language is not case sensitive
    std::transform(word.begin(), word.end(), word.begin(), ::tolower);
    
    // check to see if the word is a reserved word
    std::map<std::string, kjlc::Token>::iterator wordResult = 
      this->tokMap.find(word);
    if (wordResult != this->tokMap.end()) {
      // this word is a reserved word
      return wordResult->second;
    }
    // it is not a reserved word
    return T_ID;
  }

  if ('0' <= ch && ch <= '9') {
    // it is a number constant
    std::ostringstream numStream;
    numStream << ch;
    char nextCh = peekNextChar();
    while ('0' <= nextCh && nextCh <= '9') {
      ch = scanNextChar();
      numStream << ch;
      nextCh = peekNextChar();
    }
    return T_NUM;
  }

  return T_UNKNOWN;
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

char Scanner::scanNextChar() {
  // check that the file is open
  if (!this->file.is_open()) {
    this->fileComplete = true;
    return '\00'; // return null char
  }
  char ch;
  this->file.get(ch);
  this->fileComplete = this->file.eof();
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