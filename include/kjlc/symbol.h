/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 **/
#ifndef EECE5183COMPILER_KJLC_SYMBOL_H
#define EECE5183COMPILER_KJLC_SYMBOL_H

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
  DECLARATION_TYPE,
};

class Symbol {
  public:
    // Constructor
    Symbol();

    // Deconstructor
    ~Symbol();

    // Print out information on a given symbol
    void PrintSymbolDebug();

    // Mutators
    std::string GetId();
    void SetId(std::string id);

    Declaration GetDeclaration();
    void SetDeclaration(Declaration declaration);

    Type GetType();
    void SetType(Type type);

    bool IsGlobal();
    void SetIsGlobal(bool global);

    bool IsArray();
    void SetIsArray(bool array);

    int GetArrayBound();
    void SetArrayBound(int bound);
    
    // Params mutated through reference
    // TODO - is this good or bad practice?
    std::vector<Symbol>& GetParams();
    
    bool IsValid();
    void SetIsValid(bool valid);

  private:
    // identifier for the symbol
    std::string id_;

    // the actual declaration is of this variant
    Declaration declaration_;

    // the type of the declaration
    Type type_;

    // if the symbol is in the global scope
    bool global_;

    // if the symbol is an array
    bool array_;
    int array_bound_;

    // if a procedure, it could have an argument
    // NOTE - this is NOT a vector of references which means
    // if I make changes that aren't direct index references it needs
    // re-inserted
    // look into std::reference_wrapper if neeeded
    std::vector<Symbol> params_;

    // denotes if this is a valid symbol
    // example invalid symbol: lookup failed
    bool valid_;
};


} // end namespace kjlc
#endif // EECE5183COMPILER_KJLC_SYMBOL_H

