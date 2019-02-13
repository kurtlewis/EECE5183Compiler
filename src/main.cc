/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * This is the main entry point for the compiler
 **/
#include <iostream>

#include "kjlc/parser.h"
#include "kjlc/scanner.h"
/**
 * Main function of program
 */
int main(int argc, char* argv[]) {
  // check that a file has been specified
  if (argc < 2) {
    std::cout << "No filename entered." << std::endl;
    return 1;
  }
  // set optional flags to false
  bool scanner_only = false;
  // look through any other arguments for flags and set them
  for (int idx = 2; idx < argc; idx++) {
    if (std::string(argv[idx]).compare("--scanner-only") == 0) {
      scanner_only = true;
    }
  }

  // act depending upon flags
  if (scanner_only) {
    // Only run the scanner in debugging formation
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
  } else {
    // run the whole compiler
    kjlc::Parser parser(argv[1]);

  }
  return 0;
}
