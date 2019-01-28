/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * Header file for the Scanner class
 **/
#ifndef EECE5138COMPILER_KJLC_SCANNER_H_
#define EECE5138COMPILER_KJLC_SCANNER_H_

namespace kjlc {

enum TOKEN {
  // reserved words
  BEGIN, // "begin"
  ELSE, // "else"
  END, // "end"
  FALSE, // "false"
  FOR, // "for"
  GLOBAL, // "global"
  IF, // "if"
  IS, // "is"
  NOT, // "not"
  PROCEDURE, // "procedure"
  PROGRAM, // "program"
  RETURN, // "return"
  THEN, // "then"
  TRUE, // "true"
  TYPE, // "type"
  VARIABLE, // "variable"
  // character tokens
  BRACK_LEFT, // "["
  BRACK_RIGHT, // "]"
  COMMA, // ","
  CURLY_LEFT, // "{"
  CURLY_RIGHT, // "}"
  PAREN_LEFT, // "("
  PAREN_RIGHT, // ")"
  PERIOD, // "."
  SEMI_COLON, // ";"
  // operators
  AND, // "&"
  DIV, // "/"
  EQ, // "=="
  GT, // ">"
  GT_EQ, // ">="
  LT, // "<"
  LT_EQ, // "<="
  MINUS, // "-"
  MULT, // "*"
  NEQ, // "!="
  PLUS, // "+"
  // types
  BOOL, // "bool"
  ENUM, // "enum"
  FLOAT, // "float"
  INTEGER, // "integer"
  STRING, // "string"
};


class Scanner {
  public:
    Scanner();
    ~Scanner();
};

} // namespace kjlc
#endif // EECE5138COMPILER_KJLC_SCANNER_H_