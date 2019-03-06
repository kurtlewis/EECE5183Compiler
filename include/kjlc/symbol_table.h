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
  Symbol() : valid(true) {}
  // identifier for the symbol
  std::string id;
  // the actual declaraiton is of this variant
  Declaration declaration;
  // the type of the declaration
  Type type;
  // denotes if this is a valid symbol
  // example invalid symbol: lookup failed
  bool valid;
};

class SymbolTable {
  public:
    // Constructor initializes symbol table
    SymbolTable();

    ~SymbolTable();

    void IncreaseScope();

    void DecreaseScope();

    Symbol FindSymbolByIdentifier(std::string id);


  private:
    // a stack of scope for local scope
    std::vector<std::map<std::string, Symbol> > local_scope_stack_;
    // a single map of global scope identifiers
    std::map<std::string, Symbol> global_scope_map_;

};

} // namespace kjlc
#endif // EECE5138COMPILER_KJLC_SYMBOL_TABLE_H
