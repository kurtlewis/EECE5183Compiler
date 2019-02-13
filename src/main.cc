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
    std::cout << "token: " << lexeme.token << std::endl;
    std::cout << "str_value: " << lexeme.str_value << std::endl;
    std::cout << "int_value: " << lexeme.int_value << std::endl;
    std::cout << "float_value: " << lexeme.float_value << std::endl;
    std::cout << "line,column: " << lexeme.line << "," << lexeme.column
        << std::endl;
    if (lexeme.token == kjlc::T_PERIOD) quit = true;
    std::cout << std::endl;
  }
  while (scan.ScanNextLexeme().token != kjlc::T_PERIOD);
  return 0;
}
