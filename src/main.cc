/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * This is the main entry point for the compiler
 **/
#include <iostream>

#include "kjlc/parser.h"
#include "kjlc/scanner.h"

// forward declare of print help
void PrintHelp();

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
  bool parser_debug = false;
  // look through any other arguments for flags and set them
  for (int idx = 1; idx < argc; idx++) {
    if (std::string(argv[idx]).compare("--help") == 0 ||
        std::string(argv[idx]).compare("--h") == 0) {
      PrintHelp();
      return 0;
    }
    if (std::string(argv[idx]).compare("--scanner-only") == 0) {
      scanner_only = true;
    }
    if (std::string(argv[idx]).compare("--parser-debug") == 0) {
      parser_debug = true;
    }
  }

  // act depending upon flags
  if (scanner_only) {
    // Only run the scanner in debugging formation
    kjlc::Scanner scan(argv[1]);
    bool quit = false;
    while (!quit) {
      kjlc::Lexeme lexeme = scan.GetNextLexeme();
      std::cout << "token: " << lexeme.token << std::endl;
      std::cout << "str_value: " << lexeme.str_value << std::endl;
      std::cout << "int_value: " << lexeme.int_value << std::endl;
      std::cout << "float_value: " << lexeme.float_value << std::endl;
      std::cout << "line,column: " << lexeme.line << "," << lexeme.column
          << std::endl;
      if (lexeme.token == kjlc::T_PERIOD) quit = true;
      std::cout << std::endl;
    }
  } else {
    // run the whole compiler
    kjlc::Parser parser(argv[1], parser_debug);
    parser.ParseProgram();

  }
  return 0;
}

// Prints help information on arguments
void PrintHelp() {
  std::cout << "KJLC: Kurt Lewis' Compiler for EECE5183." << std::endl;
  std::cout << std::endl;
  std::cout << "Usage: kjlc [options] [<file>]" << std::endl;
  std::cout << std::endl;
  std::cout << "Options:" << std::endl;
  std::cout << "  --help            Prints this information." << std::endl;
  std::cout << "  --parser-debug    Enables printing debug information.";
  std::cout << std::endl;
  std::cout << "  --scanner-only    Only runs scanner functionality and prints";
  std::cout << " debugging" << std::endl;
  std::cout << "                    information." << std::endl;
}
