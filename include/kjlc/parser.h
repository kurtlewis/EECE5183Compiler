/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * Header file for the Parser Class
 **/
#ifndef EECE5138COMPILER_KJLC_PARSER_H_
#define EECE5138COMPILER_KJLC_PARSER_H_

#include "scanner.h"

namespace kjlc {

class Parser {
  public:
    // Constructor sets things up for parsing.
    // takes a string of the file for parsing.
    Parser(std::string filename);

    // Deconstructor cleans up any allocated memory.
    ~Parser();
    
  private:
    // Scanner being used to drive the parse
    Scanner scanner_;
    Lexeme next_lexeme_;
    bool peeked_;

    // Get next lexeme
    Lexeme GetNextLexeme();

    // Peek next lexeme
    Lexeme PeekNextLexeme();

    // Handle parsing if statement
    void ParseIfStatement();

};

} // namespace kjlc
#endif // EECE5138COMPILER_KJLC_SCANNER_H_
