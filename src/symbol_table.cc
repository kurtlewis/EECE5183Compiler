/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * Symbol Table manages symbol information
 **/
#include "kjlc/symbol_table.h"

#include <iostream>

namespace kjlc {

SymbolTable::SymbolTable(bool debug)
    : debug_(debug),
      local_scope_stack_(),
      global_scope_map_() {

}

SymbolTable::~SymbolTable() {
}

void SymbolTable::IncreaseScope() {
  if (debug_) {
    std::cout << "Increasing scope stack." << std::endl;
  }

  // push a new map onto the scope
  local_scope_stack_.push_back(std::map<std::string, Symbol>());
}

void SymbolTable::DecreaseScope() {
  if (debug_) {
    std::cout << "Decreasing scope stack." << std::endl;
  }

  // check that the scope can be pushed back
  if (local_scope_stack_.size() > 0) {
    // delete the top scope
    // only declaring maps on the stack so no need to do memory management
    local_scope_stack_.pop_back();
  } else {
    std::cout << "Error popping off scope stack" << std::endl;
  }
}

void SymbolTable::InsertSymbol(Symbol &symbol) {
  if (debug_) {
    std::cout << "Inserting the following symbol:" << std::endl;
    symbol.PrintSymbolDebug();
  }

  if (symbol.IsGlobal()) {
    // it's a global scope symbol, so it goes in the global symbol table
    global_scope_map_[symbol.GetId()] = symbol;
  } else {
    // not global, so put it in the local symbol table
    local_scope_stack_.back()[symbol.GetId()] = symbol;
  }
}

Symbol SymbolTable::FindSymbolByIdentifier(std::string id) {
  // todo - double check that the valid scopes at any given time are the
  // top local scope and the global scope. If it isn't the case I need to update
  // this logic
  std::map<std::string, Symbol>::iterator result
    = local_scope_stack_.back().find(id);
  if (result != local_scope_stack_.back().end()) {
    return result->second;
  }
  result = global_scope_map_.find(id);
  if (result != global_scope_map_.end()) {
    return result->second;
  }
  Symbol symbol;
  symbol.SetIsValid(false);;
  return symbol;
}
} // namespace kjlc
