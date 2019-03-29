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
  InsertBuiltInsIntoGlobalScope();
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

void SymbolTable::SetScopeProcedure(Symbol procedure) {
  local_scope_stack_.back()[SCOPE_PROCEDURE_KEY] = procedure;
}

Symbol SymbolTable::GetScopeProcedure() {
  std::map<std::string, Symbol>::iterator result
    = local_scope_stack_.back().find(SCOPE_PROCEDURE_KEY);
    if (result != local_scope_stack_.back().end()) {
      return result->second;
    } else {
      // return an invalid symbol
      Symbol symbol = Symbol::GenerateAnonymousSymbol();
      symbol.SetIsValid(false);
      return symbol;
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
  Symbol symbol = Symbol::GenerateAnonymousSymbol();
  symbol.SetIsValid(false);;
  return symbol;
}

void SymbolTable::InsertBuiltInsIntoGlobalScope() {
  //
  // Runtime functions
  //
  
  // getBool() : bool value
  Symbol get_bool;
  get_bool.SetId("getbool");
  get_bool.SetType(TYPE_BOOL);
  get_bool.SetIsGlobal(true);
  get_bool.SetDeclaration(DECLARATION_PROCEDURE);
  InsertSymbol(get_bool);

  // getInteger() : integer value
  Symbol get_integer;
  get_integer.SetId("getinteger");
  get_integer.SetType(TYPE_INT);
  get_integer.SetIsGlobal(true);
  get_integer.SetDeclaration(DECLARATION_PROCEDURE);
  InsertSymbol(get_integer);

  // getFloat() : float value
  Symbol get_float;
  get_float.SetId("getfloat");
  get_float.SetType(TYPE_FLOAT);
  get_float.SetIsGlobal(true);
  get_float.SetDeclaration(DECLARATION_PROCEDURE);
  InsertSymbol(get_float);

  // getString() : string value
  Symbol get_string;
  get_string.SetId("getstring");
  get_string.SetType(TYPE_STRING);
  get_string.SetIsGlobal(true);
  get_string.SetDeclaration(DECLARATION_PROCEDURE);
  InsertSymbol(get_string);

  // putBool(bool value) : bool
  Symbol put_bool;
  put_bool.SetId("putbool");
  put_bool.SetType(TYPE_BOOL);
  Symbol put_bool_arg;
  put_bool_arg.SetId("value");
  put_bool_arg.SetType(TYPE_BOOL);
  put_bool_arg.SetDeclaration(DECLARATION_VARIABLE);
  put_bool.GetParams().push_back(put_bool_arg);
  put_bool.SetIsGlobal(true);
  put_bool.SetDeclaration(DECLARATION_PROCEDURE);
  InsertSymbol(put_bool);

  // putInteger(integer value) : bool
  Symbol put_integer;
  put_integer.SetId("putinteger");
  put_integer.SetType(TYPE_BOOL);
  Symbol put_integer_arg;
  put_integer_arg.SetId("value");
  put_integer_arg.SetType(TYPE_INT);
  put_integer_arg.SetDeclaration(DECLARATION_VARIABLE);
  put_integer.GetParams().push_back(put_integer_arg);
  put_integer.SetIsGlobal(true);
  put_integer.SetDeclaration(DECLARATION_PROCEDURE);
  InsertSymbol(put_integer);

  // putFloat(float value) : bool
  Symbol put_float;
  put_float.SetId("putfloat");
  put_float.SetType(TYPE_BOOL);
  Symbol put_float_arg;
  put_float_arg.SetId("value");
  put_float_arg.SetType(TYPE_FLOAT);
  put_float_arg.SetDeclaration(DECLARATION_VARIABLE);
  put_float.GetParams().push_back(put_float_arg);
  put_float.SetIsGlobal(true);
  put_float.SetDeclaration(DECLARATION_PROCEDURE);
  InsertSymbol(put_float);

  // putString(string value) : bool
  Symbol put_string;
  put_string.SetId("putstring");
  put_string.SetType(TYPE_BOOL);
  Symbol put_string_arg;
  put_string_arg.SetId("value");
  put_string_arg.SetType(TYPE_STRING);
  put_string_arg.SetDeclaration(DECLARATION_VARIABLE);
  put_string.GetParams().push_back(put_string_arg);
  put_string.SetIsGlobal(true);
  put_string.SetDeclaration(DECLARATION_PROCEDURE);
  InsertSymbol(put_string);

  // sqrt(integer value) : float
  Symbol sqrt;
  sqrt.SetId("sqrt");
  sqrt.SetType(TYPE_FLOAT);
  Symbol sqrt_arg;
  sqrt_arg.SetId("value");
  sqrt_arg.SetType(TYPE_INT);
  sqrt_arg.SetDeclaration(DECLARATION_VARIABLE);
  sqrt.GetParams().push_back(sqrt_arg);
  sqrt.SetIsGlobal(true);
  sqrt.SetDeclaration(DECLARATION_PROCEDURE);
  InsertSymbol(sqrt);
}

} // namespace kjlc
