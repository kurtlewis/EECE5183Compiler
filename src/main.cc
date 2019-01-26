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
  return 0;
}