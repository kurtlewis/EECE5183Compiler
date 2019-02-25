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
#include <utility>

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
  T_BAR, // "|"
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
  T_COL_EQ, // ":="
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
  T_INT_LITERAL, // integer literal
  T_FLOAT_LITERAL, // float literal
  T_STRING_LITERAL, // string literal
  T_UNKNOWN // unknown identifier
};

struct Lexeme {
  Lexeme(): str_value(""), int_value(0), float_value(0.0) {}
  Token token;
  std::string str_value;
  int int_value;
  float float_value;
  int line;
  int column;
};

class Scanner {
  public:
    Scanner(std::string filename);
    ~Scanner();

    // Read the next lexeme irreversably
    Lexeme GetNextLexeme();

    // Peek the next lexeme
    Lexeme PeekNextLexeme();

    // static
    static std::map<std::string, Token> generate_token_mapping();
  private:
    // file the scanner is reading
    std::fstream file_;

    // map of string values that tokens represent to Token enum value
    std::map<std::string, Token> token_map_;

    // flag representing if the file is done being read
    bool file_complete_;

    // counter for line the scanner is on
    int line_;

    // counter for column the scanner is on
    int column_;

    // Lexeme that represents the next lexeme for tracking peeks
    Lexeme next_lexeme_;
    
    // boolean for if there has already been a peek
    bool peeked_;


    // Finds the next Lexeme (complete token)
    Lexeme ScanNextLexeme();

    // scans the next character and returns it
    // moves the file pointer forward
    char ScanNextChar();

    // peeks at the next character and returns it
    // does not move the filer pointer
    char PeekNextChar();
    
    // returns true if the given ch is whitespace
    bool IsWhiteSpace(char ch);

    /* returns true if the given character is a valid word char
     * @param ch - character to check
     * @param apply_first_char_rules - denotes if first char rules should be
     *                                 applied
     */ 
    bool IsValidWordChar(char ch, bool apply_first_char_rules);
};

} // namespace kjlc
#endif // EECE5138COMPILER_KJLC_SCANNER_H_
