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
  DEClARATION_TYPE
};

struct Symbol {
  // default initializer list for variables that need set
  Symbol() : global(false), valid(true) {}

  // identifier for the symbol
  std::string id;

  // the actual declaration is of this variant
  Declaration declaration;

  // the type of the declaration
  Type type;

  // if the symbol is in the global scope
  bool global;

  // denotes if this is a valid symbol
  // example invalid symbol: lookup failed
  bool valid;
};

class SymbolTable {
  public:
    // Constructor initializes symbol table
    SymbolTable();

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


    void InsertSymbolToGlobalScope(Symbol symbol);

    void InsertSymbolToLocalScope(Symbol symbol);

    Symbol FindSymbolByIdentifier(std::string id);


  private:
    // a stack of scope for local scope
    // declaring maps on the stack instead of on the heap saves memory
    // management overhead and should be fine
    std::vector<std::map<std::string, Symbol> > local_scope_stack_;
    // a single map of global scope identifiers
    std::map<std::string, Symbol> global_scope_map_;

};

} // namespace kjlc
#endif // EECE5138COMPILER_KJLC_SYMBOL_TABLE_H
