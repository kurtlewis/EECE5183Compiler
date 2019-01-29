/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * Header file for the Scanner class
 **/
#ifndef EECE5138COMPILER_KJLC_SCANNER_H_
#define EECE5138COMPILER_KJLC_SCANNER_H_

#include <fstream>
#include <map>
#include <string>

namespace kjlc {

enum Token {
  // reserved words
  T_BEGIN, // "begin"
  T_ELSE, // "else"
  T_END, // "end"
  T_FALSE, // "false"
  T_FOR, // "for"
  T_GLOBAL, // "global"
  T_IF, // "if"
  T_IS, // "is"
  T_NOT, // "not"
  T_PROCEDURE, // "procedure"
  T_PROGRAM, // "program"
  T_RETURN, // "return"
  T_THEN, // "then"
  T_TRUE, // "true"
  T_TYPE, // "type"
  T_VARIABLE, // "variable"
  // character tokens
  T_BRACK_LEFT, // "["
  T_BRACK_RIGHT, // "]"
  T_COMMA, // ","
  T_CURLY_LEFT, // "{"
  T_CURLY_RIGHT, // "}"
  T_PAREN_LEFT, // "("
  T_PAREN_RIGHT, // ")"
  T_PERIOD, // "."
  T_QUOTE, // """
  T_SEMI_COLON, // ";"
  // operators
  T_AND, // "&"
  T_DIV, // "/"
  T_EQ, // "=="
  T_GT, // ">"
  T_GT_EQ, // ">="
  T_LT, // "<"
  T_LT_EQ, // "<="
  T_MINUS, // "-"
  T_MULT, // "*"
  T_NEQ, // "!="
  T_PLUS, // "+"
  // types
  T_BOOL, // "bool"
  T_ENUM, // "enum"
  T_FLOAT, // "float"
  T_INT, // "integer"
  T_STRING, // "string"
  // user tokens
  T_ID, // user identifier
  T_UNKNOWN // unknown identifier
};


class Scanner {
  public:
    Scanner(std::string filename);
    ~Scanner();
    Token scanNext();
    // static
    static std::map<Token, std::string> generate_token_mapping();
  private:
    std::fstream file;
    std::map<Token, std::string> tokMap;
};

} // namespace kjlc
#endif // EECE5138COMPILER_KJLC_SCANNER_H_