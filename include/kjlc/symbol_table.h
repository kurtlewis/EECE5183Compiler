/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * Header file for the Symbol Table Class
 **/
#ifndef EECE5138COMPILER_KJLC_SYMBOL_TABLE_H
#define EECE5138COMPILER_KJLC_SYMBOL_TABLE_H

#include <map>
#include <string>
#include <vector>

namespace kjlc {

enum Type {
  TYPE_BOOL,
  TYPE_ENUM,
  TYPE_FLOAT,
  TYPE_INT,
  TYPE_STRING,
};

enum Declaration {
  DECLARATION_PROCEDURE,
  DECLARATION_VARIABLE,
  DECLARATION_TYPE
};

struct Symbol {
  // default initializer list for variables that need set
  Symbol() : global(false), params(), valid(true) {}

  // identifier for the symbol
  std::string id;

  // the actual declaration is of this variant
  Declaration declaration;

  // the type of the declaration
  Type type;

  // if the symbol is in the global scope
  bool global;

  // if the symbol is an array
  bool array;
  int bound;

  // if a procedure, it could have argument
  // NOTE - this is NOT a vector of references which means
  // if I make changes it need re-inserted
  // if this proves to be rough, look into std::reference_wrapper
  std::vector<Symbol> params;

  // denotes if this is a valid symbol
  // example invalid symbol: lookup failed
  bool valid;
};

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

    void PrintSymbolDebug(Symbol &symbol);

};

} // namespace kjlc
#endif // EECE5138COMPILER_KJLC_SYMBOL_TABLE_H
