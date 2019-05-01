/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * Symbol Table manages symbol information
 **/
#include "kjlc/symbol_table.h"

#include <iostream>

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

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
    if (debug_) {
      std::cout << "Found the following symbol:" << std::endl;
      result->second.PrintSymbolDebug();
    }
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

bool SymbolTable::CheckForSymbolCollisions(std::string id) {
  // look up in both maps that the string does not return a value
  std::map<std::string, Symbol>::iterator result
      = local_scope_stack_.back().find(id);
  if (result != local_scope_stack_.back().end()) {
    return true;
  }
  result = global_scope_map_.find(id);
  if (result != global_scope_map_.end()) {
    return true;
  }

  // a couple of reserved symbol names
  if (id.compare("main") == 0) {
    return true;
  }

  return false;
}

std::map<std::string, Symbol>::iterator
    SymbolTable::GetLocalScopeIteratorBegin() {
  return local_scope_stack_.back().begin();
}

std::map<std::string, Symbol>::iterator
    SymbolTable::GetLocalScopeIteratorEnd() {
  return local_scope_stack_.back().end();
}

void SymbolTable::InsertBuiltInsIntoGlobalScope(
    bool codegen, 
    llvm::Module *llvm_module,
    llvm::LLVMContext &llvm_context,
    llvm::IRBuilder<> *llvm_builder) {
  //
  // Runtime functions
  //

  // getBool() : bool value
  Symbol get_bool;
  get_bool.SetId("getbool");
  get_bool.SetType(TYPE_BOOL);
  get_bool.SetIsGlobal(true);
  get_bool.SetDeclaration(DECLARATION_PROCEDURE);
  if (codegen) {
    llvm::FunctionType *function_type = llvm::FunctionType::get(
        llvm_builder->getInt1Ty(),
        {},
        false);
    llvm::Function *procedure = llvm::Function::Create(
        function_type,
        llvm::Function::ExternalLinkage,
        get_bool.GetId(),
        llvm_module);
    get_bool.SetLLVMFunction(procedure);
  }
  InsertSymbol(get_bool);

  // getInteger() : integer value
  Symbol get_integer;
  get_integer.SetId("getinteger");
  get_integer.SetType(TYPE_INT);
  get_integer.SetIsGlobal(true);
  get_integer.SetDeclaration(DECLARATION_PROCEDURE);
  if (codegen) {
    llvm::FunctionType *function_type = llvm::FunctionType::get(
      llvm_builder->getInt32Ty(),
      {},
      false);
    llvm::Function *procedure = llvm::Function::Create(
        function_type,
        llvm::Function::ExternalLinkage,
        get_integer.GetId(),
        llvm_module);
    get_integer.SetLLVMFunction(procedure);
  }
  InsertSymbol(get_integer);

  // getFloat() : float value
  Symbol get_float;
  get_float.SetId("getfloat");
  get_float.SetType(TYPE_FLOAT);
  get_float.SetIsGlobal(true);
  get_float.SetDeclaration(DECLARATION_PROCEDURE);
  if (codegen) {
    llvm::FunctionType *function_type = llvm::FunctionType::get(
        llvm_builder->getFloatTy(),
        {},
        false);
    llvm::Function *procedure = llvm::Function::Create(
        function_type,
        llvm::Function::ExternalLinkage,
        get_float.GetId(),
        llvm_module);
    get_float.SetLLVMFunction(procedure);
  }
  InsertSymbol(get_float);

  // getString() : string value
  Symbol get_string;
  get_string.SetId("getstring");
  get_string.SetType(TYPE_STRING);
  get_string.SetIsGlobal(true);
  get_string.SetDeclaration(DECLARATION_PROCEDURE);
  if (codegen) {
    llvm::FunctionType *function_type = llvm::FunctionType::get(
        llvm_builder->getInt8PtrTy(),
        {},
        false);
    llvm::Function *procedure = llvm::Function::Create(
        function_type,
        llvm::Function::ExternalLinkage,
        get_string.GetId(),
        llvm_module);
    get_string.SetLLVMFunction(procedure);
  }
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
  if (codegen) {
    llvm::FunctionType *function_type = llvm::FunctionType::get(
        llvm_builder->getInt1Ty(),
        {llvm_builder->getInt1Ty()},
        false);
    llvm::Function *procedure = llvm::Function::Create(
        function_type,
        llvm::Function::ExternalLinkage,
        put_bool.GetId(),
        llvm_module);
    put_bool.SetLLVMFunction(procedure);
  }
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
  if (codegen) {
    llvm::FunctionType *function_type = llvm::FunctionType::get(
      llvm_builder->getInt1Ty(),
      {llvm_builder->getInt32Ty()},
      false);
    llvm::Function *procedure = llvm::Function::Create(
        function_type,
        llvm::Function::ExternalLinkage,
        put_integer.GetId(),
        llvm_module);
    put_integer.SetLLVMFunction(procedure);
  }
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
  if (codegen) {
    llvm::FunctionType *function_type = llvm::FunctionType::get(
        llvm_builder->getInt1Ty(),
        {llvm_builder->getFloatTy()},
        false);
    llvm::Function *procedure = llvm::Function::Create(
        function_type,
        llvm::Function::ExternalLinkage,
        put_float.GetId(),
        llvm_module);
    put_float.SetLLVMFunction(procedure);
  }
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
  if (codegen) {
    llvm::FunctionType *function_type = llvm::FunctionType::get(
        llvm_builder->getInt1Ty(),
        {llvm_builder->getInt8PtrTy()},
        false);
    llvm::Function *procedure = llvm::Function::Create(
        function_type,
        llvm::Function::ExternalLinkage,
        put_string.GetId(),
        llvm_module);
    put_string.SetLLVMFunction(procedure);
  }
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
  if (codegen) {
    llvm::FunctionType *function_type = llvm::FunctionType::get(
        llvm_builder->getFloatTy(),
        {llvm_builder->getInt32Ty()},
        false);
    llvm::Function *procedure = llvm::Function::Create(
        function_type,
        llvm::Function::ExternalLinkage,
        "mysqrt",
        llvm_module);
    sqrt.SetLLVMFunction(procedure);
  }
  InsertSymbol(sqrt);
  
  // create a runtime function for out of bounds errors
  Symbol error_func;
  error_func.SetId("_error_func"); // hide it with leading '_'
  error_func.SetType(TYPE_BOOL);
  error_func.SetIsGlobal(true);
  error_func.SetDeclaration(DECLARATION_PROCEDURE);
  if (codegen) {
    llvm::FunctionType *function_type = llvm::FunctionType::get(
        llvm_builder->getVoidTy(),
        {},
        false);
    llvm::Function *procedure = llvm::Function::Create(
        function_type,
        llvm::Function::ExternalLinkage,
        "boundsError",
        llvm_module);
    error_func.SetLLVMFunction(procedure);
  }
  InsertSymbol(error_func);
}

} // namespace kjlc
