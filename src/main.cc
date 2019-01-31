/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * This is the main entry point for the compiler
 **/
#include <iostream>

#include "kjlc/scanner.h"
/**
 * Main function of program
 */
int main(int argc, char* argv[]) {
  if (argc < 2) {
    std::cout << "No filename entered." << std::endl;
    return 1;
  }
  std::cout << "Entered filename: " << argv[1] << std::endl;
  kjlc::Scanner scan(argv[1]);
  bool quit = false;
  while (!quit) {
    kjlc::Lexeme lexeme = scan.ScanNextLexeme();
    if (lexeme.token == kjlc::T_ID || lexeme.token == kjlc::T_QUOTE) {
      std::cout << lexeme.str_value;
    } else  if (lexeme.token ==  kjlc::T_INT_LITERAL) {
      std::cout << lexeme.int_value;
    } else if (lexeme.token == kjlc::T_FLOAT_LITERAL) {
      std::cout << lexeme.float_value;
    } else {
      std::cout << "tok: " << lexeme.token;
    }
    if (lexeme.token == kjlc::T_PERIOD) quit = true;
    std::cout << std::endl;
  }
  while (scan.ScanNextLexeme().token != kjlc::T_PERIOD);
  return 0;
}