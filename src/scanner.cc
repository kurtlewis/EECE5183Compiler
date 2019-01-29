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
}

Scanner::~Scanner() {

}

kjlc::Token Scanner::scanNext() {
  // check that the file is open
  if (!this->file.is_open()) {
    return kjlc::T_PERIOD;
  }
  //
  // build up the next word
  //
  char ch;
  std::ostringstream wordStream;
  while (this->file >> std::noskipws >> ch) {
    if (ch == '\n' || ch == ' ' || ch == '\t') {
      // if a word has been gathered, break
      if (wordStream.str().length() > 0) {
        break;
      }
    } else {
      // non white-space character, append it to the word
      wordStream << ch;
    }
  }
  //std::cout << wordStream.str() << std::endl;
  std::string word = wordStream.str();
  
  if (word.compare(tokMap.find(kjlc::T_PERIOD)->second) == 0) {
    return kjlc::T_PERIOD;
  } else {
    return  kjlc::T_BEGIN;
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

} // end kjlc namespace