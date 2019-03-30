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

    // static - generates an anonymous symbol with an identifier
    // that cannot be found in the program
    static Symbol GenerateAnonymousSymbol();

    // Get type from a symbol in a string format
    static std::string GetTypeString(Symbol symbol);

    // Print out information for this symbol
    void PrintSymbolDebug();

    // Compare types for arithmetic operation to make sure the type of
    // param symbol is compatible with current symbol
    bool CheckTypesForArithmeticOp(Symbol symbol);
    
    // Check single type for a binary operation against just this current symbol
    bool CheckTypeForBinaryOp();

    // Compare types for a binary operation to make sure the type of param
    // symbol is compatible with current symbol
    bool CheckTypesForBinaryOp(Symbol symbol);

    // Compare types for a relational operation to make sure the type of the
    // param symbol is compatible with the current symbol
    // params:
    //   symbol - symbol to check for compatability with
    //   equality_test - true if the relational op is a EQ or NEQ
    bool CheckTypesForRelationalOp(Symbol symbol, bool equality_test);

    //
    // Getters and Setters
    //

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
    // declaration, variable, or type
    Declaration declaration_;

    // the type of the declaration
    // could represent variable type, type mark type, or return type of
    // procedure
    Type type_;

    // if the symbol is in the global scope
    bool global_;

    // if the symbol is an array
    bool array_;
    // size of the array if it is an array
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

