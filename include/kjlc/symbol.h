/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 **/
#ifndef EECE5183COMPILER_KJLC_SYMBOL_H
#define EECE5183COMPILER_KJLC_SYMBOL_H

#include <string>
#include <vector>

#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"

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
  DECLARATION_ENUM,
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

    bool IsIndexed();
    void SetIsIndexed(bool indexed);
    
    // Params mutated through reference
    // TODO - is this good or bad practice?
    std::vector<Symbol>& GetParams();
    
    bool IsValid();
    void SetIsValid(bool valid);
    
    int GetEnumValue();
    void SetEnumValue(int value);

    bool HasBeenInitialized();
    void SetHasBeenInitialized(bool initialized);

    // LLVM Mutators
    void SetLLVMValue(llvm::Value *value);
    llvm::Value *GetLLVMValue();

    void SetLLVMFunction(llvm::Function *function);
    llvm::Function *GetLLVMFunction();

    void SetLLVMAddress(llvm::Value *address_ptr);
    llvm::Value *GetLLVMAddress();

    void SetLLVMBound(llvm::Value *bound);
    llvm::Value *GetLLVMBound();

    void SetLLVMArrayAddress(llvm::Value *address_ptr);
    llvm::Value *GetLLVMArrayAddress();

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
    // indicates if the array has been indexed
    bool is_indexed_;

    // if a procedure, it could have an argument
    // NOTE - this is NOT a vector of references which means
    // if I make changes that aren't direct index references it needs
    // re-inserted
    // look into std::reference_wrapper if neeeded
    std::vector<Symbol> params_;

    // denotes if this is a valid symbol
    // example invalid symbol: lookup failed
    bool valid_;

    // denotes that this variable has been assigned to
    bool has_been_initialized_;

    // enum value - indicates the number value of the enum
    int enum_value_;

    //
    // llvm variables
    //
    llvm::Value *llvm_value_;
    llvm::Value *llvm_address_;
    llvm::Value *llvm_array_address_;
    llvm::Function *llvm_function_;
    llvm::Value *llvm_bound_;
};
} // end namespace kjlc
#endif // EECE5183COMPILER_KJLC_SYMBOL_H

