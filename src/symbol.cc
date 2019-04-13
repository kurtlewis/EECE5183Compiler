/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 **/

#include "kjlc/symbol.h"

#include <ctime>
#include <iostream>
#include <stdlib.h>

namespace kjlc {

Symbol::Symbol() 
  : id_(""),
    global_(false),
    array_(false),
    array_bound_(0),
    params_(),
    valid_(true) {
 
}

Symbol::~Symbol() {

}

Symbol Symbol::GenerateAnonymousSymbol() {
  Symbol symbol;
  std::srand(std::time(nullptr));
  symbol.SetId("_" + std::to_string(std::rand()));
  return symbol;
}

void Symbol::PrintSymbolDebug() {
  std::cout << "Symbol: " << id_ << std::endl;
  std::cout << "  Declaration enum: " << declaration_ << std::endl;
  std::cout << "  Type: " << Symbol::GetTypeString(*this) << std::endl;
  std::cout << "  Global: " << global_ << std::endl;
  std::cout << "  Array: " << array_ << std::endl;
  std::cout << "  Bound: " << array_bound_ << std::endl;
  std::cout << "  Param count: " << params_.size() << std::endl;
  std::cout << "  Valid: " << valid_ << std::endl;
  std::cout << std::endl;
}

bool Symbol::CheckTypesForArithmeticOp(Symbol symbol) {
  // only return true in the switch statement if the types are compatible
  // return false at the end of the function
  switch (type_) {
    case TYPE_BOOL:
      break;
    case TYPE_ENUM:
      break;
    case TYPE_FLOAT: // fall through to TYPE_INT
    case TYPE_INT:
      if (symbol.GetType() == TYPE_INT || symbol.GetType() == TYPE_FLOAT) {
        return true;
      }
      break;
    case TYPE_STRING:
      break;
  }
  // code only reaches here by being incompatible
  return false;
}

bool Symbol::CheckTypesForRelationalOp(Symbol symbol, bool equality_test) {
  // only return true in the switch statement if the types are compatible
  // return false at the end of the function
  switch (type_) {
    case TYPE_BOOL:
      if (symbol.GetType() == TYPE_BOOL || symbol.GetType() == TYPE_INT) {
        return true;
      }
      break;
    case TYPE_ENUM:
      break;
    case TYPE_FLOAT:
      if (symbol.GetType() == TYPE_FLOAT || symbol.GetType() == TYPE_INT) {
        return true;
      }
      break;
    case TYPE_INT:
      if (symbol.GetType() == TYPE_BOOL || symbol.GetType() == TYPE_FLOAT ||
          symbol.GetType() == TYPE_INT) {
        return true;
      }
      break;
    case TYPE_STRING:
      if (equality_test && symbol.GetType() == TYPE_STRING) {
        return true;
      }
      break;
  }
  // code only reaches here by being type incompatible
  return false;
}

std::string Symbol::GetTypeString(Symbol symbol) {
  if (!symbol.IsValid()) {
    return "Invalid symbol";
  }

  switch (symbol.GetType()) {
    case TYPE_BOOL:
      return "boolean";
      break;
    case TYPE_ENUM:
      return "enum";
      break;
    case TYPE_FLOAT:
      return "float";
      break;
    case TYPE_INT:
      return "integer";
      break;
    case TYPE_STRING:
      return "string";
      break;
    default:
      return "unkown type";
  }
}

//
// Getters and Setters 
//

std::string Symbol::GetId() {
  return id_;
}

void Symbol::SetId(std::string id) {
  id_ = id;
}

Declaration Symbol::GetDeclaration() {
  return declaration_;
}

void Symbol::SetDeclaration(Declaration declaration) {
  declaration_ = declaration;
}

Type Symbol::GetType() {
  return type_;
}

void Symbol::SetType(Type type) {
  type_ = type;
}

bool Symbol::IsGlobal() {
  return global_;
}

void Symbol::SetIsGlobal(bool global) {
  global_ = global;
}

bool Symbol::IsArray() {
  return array_;
}

void Symbol::SetIsArray(bool array) {
  array_ = array;
}

int Symbol::GetArrayBound() {
  return array_bound_;
}

void Symbol::SetArrayBound(int bound) {
  array_bound_ = bound;
}

std::vector<Symbol>& Symbol::GetParams() {
  return params_;
}

bool Symbol::IsValid() {
  return valid_;
}

void Symbol::SetIsValid(bool valid) {
  valid_ = valid;
}

} // namespace kjlc
