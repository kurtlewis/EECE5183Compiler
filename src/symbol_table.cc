/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * Symbol Table manages symbol information
 **/
#include "kjlc/symbol_table.h"

#include <iostream>

namespace kjlc {

SymbolTable::SymbolTable() : local_scope_stack_(), global_scope_map_() {

}

SymbolTable::~SymbolTable() {
}

void SymbolTable::IncreaseScope() {
  // push a new map onto the scope
  local_scope_stack_.push_back(std::map<std::string, Symbol>());
}

void SymbolTable::DecreaseScope() {
  // check that the scope can be pushed back
  if (local_scope_stack_.size() > 0) {
    // delete the top scope
    local_scope_stack_.pop_back();
  } else {
    std::cout << "Error popping off scope stack";
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
  symbol.valid = false;
  return symbol;
}

} // namespace kjlc
