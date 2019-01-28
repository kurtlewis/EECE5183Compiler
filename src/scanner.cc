/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * Scanner handles tokenizing input content
 **/
#include "kjlc/scanner.h"

#include <iostream>

namespace kjlc {

Scanner::Scanner() {
  std::cout << "Hello World" << std::endl;
}

Scanner::~Scanner() {

}

static std::map<kjlc::TOKEN, std::string> generate_token_mapping() {
  std::map<kjlc::TOKEN, std::string> map = std::map<enum TOKEN, std::string>();
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