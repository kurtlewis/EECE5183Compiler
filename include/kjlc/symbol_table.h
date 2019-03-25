/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * Header file for the Symbol Table Class
 **/
#ifndef EECE5183COMPILER_KJLC_SYMBOL_TABLE_H
#define EECE5183COMPILER_KJLC_SYMBOL_TABLE_H

#include <map>
#include <string>
#include <vector>

#include "symbol.h"

namespace kjlc {

class SymbolTable {
  public:
    // Constructor initializes symbol table
    SymbolTable(bool debug);

    ~SymbolTable();

    //-------------------------------------------
    // Scope Control Methods
    //-------------------------------------------
    // Pops the topmost scope off the stack, discarding the generated scope
    // at this level
    void DecreaseScope();

    // Pushes a new scope to the sstack, storing the current scope until
    // decrease scope has been called
    void IncreaseScope();


    void InsertSymbol(Symbol &symbol);

    // TODO - as stands this function returns a copied object instead
    // of a reference to it, one way I could fix it is by creating a global
    // invalid symbol
    Symbol FindSymbolByIdentifier(std::string id);


  private:
    // a stack of scope for local scope
    // declaring maps on the stack instead of on the heap saves memory
    // management overhead and should be fine
    std::vector<std::map<std::string, Symbol> > local_scope_stack_;
    // a single map of global scope identifiers
    std::map<std::string, Symbol> global_scope_map_;

    // flag for printing debug information
    bool debug_;
};

} // namespace kjlc
#endif // EECE5183COMPILER_KJLC_SYMBOL_TABLE_H
