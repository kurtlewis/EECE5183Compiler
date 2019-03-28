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


    // inserts a symbol to the appropriate scope, global if it as marked as such
    // or the top of the stack otherwise
    void InsertSymbol(Symbol &symbol);

    // In our language, scopes are created when a new procedure is declared,
    // which means one procedure is the owner of each scope
    void SetScopeProcedure(Symbol procedure);

    // returns the procedure set by SetScopeProcedure, or an invalid symbol
    // if one was not set
    Symbol GetScopeProcedure();

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

    // key used in map for storing scope procedure. Real identifiers can't start
    // with underscores, so it is safe from overlap
    const std::string SCOPE_PROCEDURE_KEY = "__PROCEDURE";

    // flag for printing debug information
    bool debug_;
};

} // namespace kjlc
#endif // EECE5183COMPILER_KJLC_SYMBOL_TABLE_H
