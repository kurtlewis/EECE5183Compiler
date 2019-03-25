/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 **/

#include "kjlc/symbol.h"

#include <iostream>

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

void Symbol::PrintSymbolDebug() {
  std::cout << "Symbol: " << id_ << std::endl;
  std::cout << "  Declaration enum: " << declaration_ << std::endl;
  std::cout << "  Type enum: " << type_ << std::endl;
  std::cout << "  Global: " << global_ << std::endl;
  std::cout << "  Array: " << array_ << std::endl;
  std::cout << "  Bound: " << array_bound_ << std::endl;
  std::cout << "  Param count: " << params_.size() << std::endl;
  std::cout << "  Valid: " << valid_ << std::endl;
  std::cout << std::endl;
}

//
// Mutators
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
