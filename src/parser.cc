/*************************
 * Kurt Lewis
 * https://kurtjlewis.com
 *************************
 * Parser Class. Handles verifying grammar of language
 **/
#include "kjlc/parser.h"

#include <iostream>

#include "kjlc/scanner.h"

namespace kjlc {

// Default Constructor
Parser::Parser(std::string filename, bool debug)
    : scanner_(filename),
      error_state_(false),
      end_parse_(false),
      debug_(debug) {

}

// Deconstructor
Parser::~Parser() {

}

void Parser::ParseProgram() {
  ParseProgramHeader();
  ParseProgramBody();
}

void Parser::DebugPrint(std::string parse_function) {
  if (debug_) {
    std::cout << parse_function << std::endl;
  }
}

void Parser::EmitParsingError(std::string message, Lexeme lexeme) {
  // don't output if still in an error_state_
  if (error_state_) {
    return;
  }
  std::cout << "Line:" << lexeme.line << " Col:" << lexeme.column;
  std::cout << " - " << message << std::endl;
  error_state_ = true;
}

void Parser::EmitExpectedTokenError(std::string expected_token, Lexeme lexeme) {
  // don't output if still in an error_state_
  if (error_state_) {
    return;
  }
  std::cout << "Line:" << lexeme.line << " Col:" << lexeme.column;
  std::cout << " - " << "Expected token '" << expected_token << "'";
  std::cout << std::endl;
  error_state_ = true;
}


// consumes tokens until the next token is in the tokens list or a T_PERIOD
void Parser::ResyncOnTokens(Token tokens[], int tokens_length) {
  DebugPrint("ResyncOnTokens");
  // by nature of getting here, error_state_ should be true
  // set it anyways since it's used as an exit condition
  error_state_ = true;
  Lexeme lexeme;
  while (error_state_) {
    lexeme = scanner_.PeekNextLexeme();
    for (int idx = 0; idx < tokens_length; idx++) {
      if (lexeme.token == tokens[idx]) {
        error_state_ = false;
        break;
      }
    }
    if (lexeme.token == T_PERIOD) {
      // stop consuming,
      error_state_ = false;
      // period denotes either the end of file or a marker that indicates the
      // end of file. Regardless, our parse is over even if there are more
      // tokens
      end_parse_ = true;
    } else {
      // consume the lexeme
      lexeme = scanner_.GetNextLexeme();
    }
  }

}

void Parser::ParseArgumentList() {
  DebugPrint("ArgumentList");

  ParseExpression();

  // more arguments are optional and indicated by a comma
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_COMMA) {
    // consume comma
    lexeme = scanner_.GetNextLexeme();

    // parse the next arguments recursively
    ParseArgumentList();
  }
}

void Parser::ParseAssignmentStatement() {
  DebugPrint("AssignmentStatement");

  ParseDestination();

  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_COL_EQ) {
    EmitExpectedTokenError(":=", lexeme);
    return;
  }

  ParseExpression();
}

// this is a left recursive rule that has been modified to be right recursive
// see docs/language-grammar-modified.txt for more information
void Parser::ParseArithOp() {
  DebugPrint("ArithOp");

  ParseRelation();

  ParseArithOpTail();
}

// because this is a left recursive rule made right recursive, it is necessary
// to support empty evaluations
void Parser::ParseArithOpTail() {
  DebugPrint("ArithOpTail");

  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_PLUS || lexeme.token == T_MINUS) {
    // consume the "+" or "-"
    lexeme = scanner_.GetNextLexeme();

    ParseRelation();

    // recursive call
    ParseArithOpTail();
  }
}

void Parser::ParseBound() {
  DebugPrint("Bound");

  // peek for '-'
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_MINUS) {
    // consume the dash 
    lexeme = scanner_.GetNextLexeme();
  }

  ParseNumber();
}

void Parser::ParseDeclaration() {
  DebugPrint("Declaration");

  // peek because the first terminal could be a number of things, some of
  // which involve reaching down rule evaluations
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_GLOBAL) {
    // there's a global keyword
    // consume the token 
    lexeme = scanner_.GetNextLexeme();
    // TODO: Probably do things with this in the symbol table
    
    // now prepare lexeme for guessing the next rule
    lexeme = scanner_.PeekNextLexeme();
  }

  // Rule can evaluate to three different rules, so check their FIRST sets
  if (lexeme.token == T_PROCEDURE) {
    // Procedure Declaration rule
    ParseProcedureDeclaration();
  } else if (lexeme.token == T_VARIABLE) {
    // Variable declaration rule
    ParseVariableDeclaration();
  } else if (lexeme.token == T_TYPE) {
    // Type declaration rule
    ParseTypeDeclaration();
  } else {
    EmitParsingError("Could not parse declaration - expected a variable, "
                     "type, or procedure",
                     lexeme);
    return;
  }
}

void Parser::ParseDestination() {
  DebugPrint("Destination");

  ParseIdentifier();

  // peek to see if there are brackets
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_BRACK_LEFT) {
    // consume token
    lexeme = scanner_.GetNextLexeme();

    ParseExpression();

    // ending bracket is required if it was opened
    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_BRACK_RIGHT) {
      EmitExpectedTokenError("]", lexeme);
      return;
    }
  }
}

// this is a left recursive rule made right recursive
// see `docs/language-gramamr-modified` for notes on the rules
void Parser::ParseExpression() {
  DebugPrint("Expression");

  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_NOT) {
    // consume "not"
    lexeme = scanner_.GetNextLexeme();
  }

  // parse arithOp
  ParseArithOp();

  ParseExpressionTail();
}

void Parser::ParseExpressionTail() {
  DebugPrint("ExpressionTail");

  // there is an lambda(empty) evaluation of this rule, so start with a peek
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_AND || lexeme.token == T_BAR) {
    // consume the token, not empty evaluation
    lexeme = scanner_.GetNextLexeme();
    
    // Next is arith op
    ParseArithOp();

    // Right recursive call
    ParseExpressionTail();
  }
}

void Parser::ParseFactor() {
  DebugPrint("Factor");

  // Need to use first sets to determine evaluation, so peek
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_PAREN_LEFT) {
    // ( <expression> ) evaluation
    // consume paren
    lexeme = scanner_.GetNextLexeme();
    
    ParseExpression();

    // closing paren is required
    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_PAREN_RIGHT) {
      EmitExpectedTokenError(")", lexeme);
      return;
    }
  } else if (lexeme.token == T_ID) {
    // could be a procedure call or name reference
    ParseReference();
  } else if (lexeme.token == T_MINUS) {
    // consume T_MINUS
    lexeme = scanner_.GetNextLexeme();

    // Parse Name or Number depending on next token
    lexeme = scanner_.PeekNextLexeme();
    if (lexeme.token == T_INT_LITERAL || lexeme.token == T_FLOAT_LITERAL) {
      // number
      ParseNumber();
    } else if (lexeme.token == T_ID) {
      // name reference
      ParseReference();
    } else {
      EmitParsingError("Expected numeric literal or identifier reference",
                      lexeme);
      return;
    }
  } else if (lexeme.token == T_INT_LITERAL || lexeme.token == T_FLOAT_LITERAL) {
    ParseNumber();
  } else if (lexeme.token == T_STRING_LITERAL) {
    ParseString();
  } else if (lexeme.token == T_TRUE) {
    // consume true
    lexeme = scanner_.GetNextLexeme();
  } else if (lexeme.token == T_FALSE) {
    // consume false
    lexeme = scanner_.GetNextLexeme();
  } else {
    EmitParsingError("Exected valid factor", lexeme);
    return;
  }
}

void Parser::ParseIdentifier() {
  DebugPrint("Identifier");

  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_ID) {
    EmitParsingError("Expected Identifier", lexeme);
    return;
  }
  // TODO: probably some symbol table stuff
}

void Parser::ParseIfStatement() {
  DebugPrint("IfStatement");

  Lexeme lexeme = scanner_.GetNextLexeme(); 
  if (lexeme.token != T_IF) {
    EmitExpectedTokenError("if", lexeme);
    return;
  }

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PAREN_LEFT) {
    EmitExpectedTokenError("(", lexeme);
    return;
  }

  // handle parsing the expression
  ParseExpression();

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PAREN_RIGHT) {
    EmitExpectedTokenError(")", lexeme);
    return;
  }

  lexeme = scanner_.GetNextLexeme(); 
  if (lexeme.token != T_THEN) {
    EmitExpectedTokenError("then", lexeme);
    return;
  }

  // only read statements until we peek a T_ELSE or T_END
  // it is required that there is at least one statement so don't peek
  // ahead of time. This way an error will be thrown if there are no statements
  while (lexeme.token != T_ELSE && lexeme.token != T_END) {
    // Handle parsing the statement
    ParseStatement();

    if (error_state_) {
      // there was an error down the tree
      Token tokens[] = {T_SEMI_COLON, T_ELSE, T_END};
      ResyncOnTokens(tokens, 3);

      // check to see if parse should be ended
      if (end_parse_) {
        return;
      }

      // if recovering from an error, we don't consider the semi colon mandatory
      // but, read it if it's the next token so the next statement can be read
      // correctly
      lexeme = scanner_.PeekNextLexeme();
      if (lexeme.token == T_SEMI_COLON) {
        lexeme = scanner_.GetNextLexeme();
      }
    } else {
      // all statements are followed by semi colon
      lexeme = scanner_.GetNextLexeme();
      if (lexeme.token != T_SEMI_COLON) {
        EmitExpectedTokenError(";", lexeme);
        return;
      }
    }
    
    // Peek to know if its another statement or an else or end
    lexeme = scanner_.PeekNextLexeme();
  }

  // the last lexeme was only peeked, read it now
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token == T_ELSE) {
    // handle else statement
    // again, at least one statement is required so don't peek ahead of time
    // because it would let the user get away with no statements
    while (lexeme.token != T_END) {
      // handle parsing the statements
      ParseStatement();

      if (error_state_) {
        // there was an error down the tree
        Token tokens[] = {T_SEMI_COLON, T_END};
        ResyncOnTokens(tokens, 2);

        // check to see if the aprse ended
        if (end_parse_) {
          return;
        }
        
        // if recovering from an error, we don't consider the semi colon 
        // mandatory but, read it if it's the next token so the next statement
        // can be read correctly
        lexeme = scanner_.PeekNextLexeme();
        if (lexeme.token == T_SEMI_COLON) {
          lexeme = scanner_.GetNextLexeme();
        }
      } else {
        // all statements are followed by semi colon
        lexeme = scanner_.GetNextLexeme();
        if (lexeme.token != T_SEMI_COLON) {
          EmitExpectedTokenError(";", lexeme);
          return;
        }
      }
      // peek to see if its another statement or end of else
      lexeme = scanner_.PeekNextLexeme();
    }
    // consume the token that was only peeked so that end can procede
    // as if else wasn't there
    lexeme = scanner_.GetNextLexeme();
  }

  // now at end
  if (lexeme.token != T_END) {
    EmitExpectedTokenError("end", lexeme);
    return;
  }

  // 'if'
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_IF) {
    EmitExpectedTokenError("if", lexeme);
    return;
  }
}

void Parser::ParseLoopStatement() {
  DebugPrint("LoopStatement");

  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_FOR) {
    EmitExpectedTokenError("for", lexeme);
    return;
  }

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PAREN_LEFT) {
    EmitExpectedTokenError("(", lexeme);
    return;
  }

  ParseAssignmentStatement();

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_SEMI_COLON) {
    EmitExpectedTokenError(";", lexeme);
    return;
  }

  ParseExpression();

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PAREN_RIGHT) {
    EmitExpectedTokenError(")", lexeme);
    return;
  }

  // no statements are required, so peek before loop to allow for no statements
  lexeme = scanner_.PeekNextLexeme();
  while (lexeme.token != T_END) {
    ParseStatement();

    if (error_state_) {
      // there was an error down the tree
      Token tokens[] = {T_SEMI_COLON, T_END};
      ResyncOnTokens(tokens, 2);

      // check to see if parse should be ended
      if (end_parse_) {
        return;
      }

      // if recovering from an error, we don't consider the semi colon mandatory
      // but, read it if it's the next token so the next statement can be read
      // correctly
      lexeme = scanner_.PeekNextLexeme();
      if (lexeme.token == T_SEMI_COLON) {
        lexeme = scanner_.GetNextLexeme();
      }
    } else {
      // all statements are followed by semi colon
      lexeme = scanner_.GetNextLexeme();
      if (lexeme.token != T_SEMI_COLON) {
        EmitExpectedTokenError(";", lexeme);
        return;
      }
    }
    lexeme = scanner_.PeekNextLexeme();
  }

  // consume the end for real
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_END) {
    EmitExpectedTokenError("end", lexeme);
    return;
  }

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_FOR) {
    EmitExpectedTokenError("for", lexeme);
    return;
  }
}

void Parser::ParseNumber() {
  DebugPrint("Number");

  // consume number token
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_INT_LITERAL && lexeme.token != T_FLOAT_LITERAL) {
    EmitParsingError("Expected numeric literal", lexeme);
    return;
  }
}

void Parser::ParseParameter() {
  ParseVariableDeclaration(); 
}

void Parser::ParseParameterList() {
  ParseParameter();

  // check to see if there are multiple parameters - indicated by ','
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_COMMA) {
    // consume the comma 
    lexeme = scanner_.GetNextLexeme();

    // recursive call to read more parameters
    ParseParameterList();
  }
}

void Parser::ParseProcedureBody() {
  DebugPrint("ProcedureBody");

  // Peek the first lexeme to determine if there are any declarations
  Lexeme lexeme = scanner_.PeekNextLexeme(); 
  while (lexeme.token != T_BEGIN) {
    // There are declarations
    ParseDeclaration();

    if (error_state_) {
      // there was an error down the tree
      Token tokens[] = {T_SEMI_COLON, T_BEGIN};
      ResyncOnTokens(tokens, 2);

      // check to see if parse should be ended
      if (end_parse_) {
        return;
      }

      // if recovering from an error, we don't consider the semi colon mandatory
      // but, read it if it's the next token so the next declaration can be read
      // correctly
      lexeme = scanner_.PeekNextLexeme();
      if (lexeme.token == T_SEMI_COLON) {
        lexeme = scanner_.GetNextLexeme();
      }
    } else {
      // all declarations are followed by semi colon
      lexeme = scanner_.GetNextLexeme();
      if (lexeme.token != T_SEMI_COLON) {
        EmitExpectedTokenError(";", lexeme);
        return;
      }
    }
    // peek to see if there are more declarations
    lexeme = scanner_.PeekNextLexeme();
  }
  
  // Parse the 'begin'
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_BEGIN) {
    EmitExpectedTokenError("begin", lexeme);
    return;
  }

  // Parse statements until 'end' is found
  // no statements are allowed, so peek before loop
  lexeme = scanner_.PeekNextLexeme();
  while (lexeme.token != T_END) {
    // there are statement(s) to read
    ParseStatement();

    if (error_state_) {
      // there was an error down the tree
      Token tokens[] = {T_SEMI_COLON, T_END};
      ResyncOnTokens(tokens, 2);

      // check to see if parse should be ended
      if (end_parse_) {
        return;
      }

      // if recovering from an error, we don't consider the semi colon mandatory
      // but, read it if it's the next token so the next statement can be read
      // correctly
      lexeme = scanner_.PeekNextLexeme();
      if (lexeme.token == T_SEMI_COLON) {
        lexeme = scanner_.GetNextLexeme();
      }
    } else {
      // all statements are followed by semi colon
      lexeme = scanner_.GetNextLexeme();
      if (lexeme.token != T_SEMI_COLON) {
        EmitExpectedTokenError(";", lexeme);
        return;
      }
    }
    // peek to look for more statements
    lexeme = scanner_.PeekNextLexeme();
  }

  // Parse 'end'
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_END) {
    EmitExpectedTokenError("end", lexeme);
    return;
  }

  // Parse 'procedure'
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PROCEDURE) {
    EmitExpectedTokenError("procedure", lexeme);
    return;
  }
}

void Parser::ParseProcedureDeclaration() {
  DebugPrint("ProcedureDeclaration");

  ParseProcedureHeader();
  ParseProcedureBody();
}

void Parser::ParseProcedureHeader() {
  DebugPrint("ProcedureHeader");

  // parse procedure keyword
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PROCEDURE) {
    EmitExpectedTokenError("procedure", lexeme);
    return;
  } 

  // read the identifier
  ParseIdentifier();

  // colon required
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_COLON) {
    EmitExpectedTokenError(":", lexeme);
    return;
  }

  // type mark
  ParseTypeMark();

  // read left paren
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PAREN_LEFT) {
    EmitExpectedTokenError("(", lexeme);
    return;
  }

  // peek to see if there is an optional parameter list using it's FIRST set
  lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_VARIABLE) {
    // read parameter list
    ParseParameterList();
  }

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PAREN_RIGHT) {
    EmitExpectedTokenError(")", lexeme);
    return;
  }
}

void Parser::ParseProgramBody() {
  DebugPrint("ProgramBody");

  // Parse leading declarations
  // none are required so peek first
  Lexeme lexeme = scanner_.PeekNextLexeme();
  while (lexeme.token != T_BEGIN) {
    // parse declaration
    ParseDeclaration();

    if (error_state_) {
      // there was an error down the tree
      Token tokens[] = {T_SEMI_COLON, T_BEGIN};
      ResyncOnTokens(tokens, 2);

      // check to see if parse should be ended
      if (end_parse_) {
        return;
      }

      // if recovering from an error, we don't consider the semi colon mandatory
      // but, read it if it's the next token so the next declaration can be read
      // correctly
      lexeme = scanner_.PeekNextLexeme();
      if (lexeme.token == T_SEMI_COLON) {
        lexeme = scanner_.GetNextLexeme();
      }
    } else {
      // all declarations are followed by semi colon
      lexeme = scanner_.GetNextLexeme();
      if (lexeme.token != T_SEMI_COLON) {
        EmitExpectedTokenError(";", lexeme);
        return;
      }
    }
    // peek to see if declarations continue
    lexeme = scanner_.PeekNextLexeme();
  }

  // Parse 'begin'
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_BEGIN) {
    EmitExpectedTokenError(";", lexeme);
    return;
  }

  // parse statements
  // none required so peek first
  lexeme = scanner_.PeekNextLexeme();
  while (lexeme.token != T_END) {
    // parse statements
    ParseStatement();

    if (error_state_) {
      // there was an error down the tree
      Token tokens[] = {T_SEMI_COLON, T_END};
      ResyncOnTokens(tokens, 2);

      // check to see if parse should be ended
      if (end_parse_) {
        return;
      }

      // if recovering from an error, we don't consider the semi colon mandatory
      // but, read it if it's the next token so the next statement can be read
      // correctly
      lexeme = scanner_.PeekNextLexeme();
      if (lexeme.token == T_SEMI_COLON) {
        lexeme = scanner_.GetNextLexeme();
      }
    } else {
      // all statements are followed by semi colon
      lexeme = scanner_.GetNextLexeme();
      if (lexeme.token != T_SEMI_COLON) {
        EmitExpectedTokenError(";", lexeme);
        return;
      }
    }
    // peek to see if statements continue
    lexeme = scanner_.PeekNextLexeme();
  }

  // parse 'end'
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_END) {
    EmitExpectedTokenError("end", lexeme);
    return;
  }

  // parse 'program'
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PROGRAM) {
    EmitExpectedTokenError("program", lexeme);
    return;
  }
  
  // successfully parsed rule
  return;
}

void Parser::ParseProgramHeader() {
  DebugPrint("ProgramHeader");

  // read 'program'
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PROGRAM) {
    EmitExpectedTokenError("program", lexeme);
    return;
  }

  // read the identifier
  ParseIdentifier();

  // read 'is'
  lexeme = scanner_.GetNextLexeme(); 
  if (lexeme.token != T_IS) {
    EmitExpectedTokenError("is", lexeme);
    return;
  }

  // successful parse
  return;
}

// this rule replaces ParseName and ParseProcedureCall
void Parser::ParseReference() {
  DebugPrint("Reference");
  
  // could be a procedure call or name
  // both start with identifiers
  ParseIdentifier();

  // this is guaranteed to correctly identify it as a name or procedure call
  // because parens are needed for procedure calls
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_PAREN_LEFT) {
    // It is a procedure call
    // consume left paren
    lexeme = scanner_.GetNextLexeme();

    // optionally ParseArgumentList
    // need to peek next token and see if it's in the (quite large) first set
    // for argument_list. See docs for derivation of first set 
    lexeme = scanner_.PeekNextLexeme();
    if (lexeme.token == T_NOT || lexeme.token == T_PAREN_LEFT ||
        lexeme.token == T_MINUS || lexeme.token == T_INT_LITERAL ||
        lexeme.token == T_FLOAT_LITERAL || lexeme.token == T_ID ||
        lexeme.token == T_STRING_LITERAL || lexeme.token == T_TRUE ||
        lexeme.token == T_FALSE) {
      // it is an argument!
      ParseArgumentList();
    }

    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_PAREN_RIGHT) {
      EmitExpectedTokenError(")", lexeme);
      return;
    }
  } else if (lexeme.token == T_BRACK_LEFT) {
    // it's a name reference
    // consume left bracket
    lexeme = scanner_.GetNextLexeme();

    ParseExpression();

    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_BRACK_RIGHT) {
      EmitExpectedTokenError("]", lexeme);
      return;
    }
  } else {
    // it's a name, but without the [ <expression> ]
  }
}

// this is a left recursive rule made right recursive
// see docs for full write-out of rule
void Parser::ParseRelation() {
  DebugPrint("Relation");

  ParseTerm();

  ParseRelationTail();
}

// Right recursive portion of the rule
void Parser::ParseRelationTail() {
  DebugPrint("RelationTail");

  // Need to allow for empty evaluation so peek
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_LT || lexeme.token == T_GT_EQ ||
      lexeme.token == T_LT_EQ || lexeme.token == T_GT ||
      lexeme.token == T_EQ || lexeme.token == T_NEQ) {
    // consume the token
    lexeme = scanner_.GetNextLexeme();

    ParseTerm();

    // Recursive call
    ParseRelationTail();
  }
}

void Parser::ParseReturnStatement() {
  DebugPrint("Return");
  
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_RETURN) {
    EmitExpectedTokenError("return", lexeme);
    return;
  }

  ParseExpression();
}

void Parser::ParseStatement() {
  DebugPrint("Statement");

  // Parse statement has four possible evaluations so peek and take the one
  // that we can predict using the respective first sets of the possible
  // rule evaluations
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_ID) {
    ParseAssignmentStatement();
  } else if (lexeme.token == T_IF) {
    ParseIfStatement();
  } else if (lexeme.token == T_FOR) {
    ParseLoopStatement();
  } else if (lexeme.token == T_RETURN) {
    ParseReturnStatement();
  } else {
    EmitParsingError("Expected identifier, if, for, or return", lexeme);
    return;
  }
}

void Parser::ParseString() {
  DebugPrint("String");

  // consume the string token
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_STRING_LITERAL) {
    EmitParsingError("Expected string literal", lexeme);
  }
}

// a left recursive rule made right recursive. See docs for writeout
void Parser::ParseTerm() {
  DebugPrint("Term");

  ParseFactor();

  ParseTermTail();
}

// Right recursive version of ParseTerm
void Parser::ParseTermTail() {
  DebugPrint("TermTail");

  // peek because there can be an empty evaluation
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_DIV || lexeme.token == T_MULT) {
    // consume the token
    lexeme = scanner_.GetNextLexeme();

    ParseFactor();

    ParseTermTail();
  }
}

void Parser::ParseTypeDeclaration() {
  DebugPrint("TypeDeclaration");

  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_TYPE) {
    EmitExpectedTokenError("type", lexeme);
    return;
  }

  // Read the identifier
  ParseIdentifier();

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_IS) {
    EmitExpectedTokenError("is", lexeme);
    return;
  }

  // Parse type mark
  ParseTypeMark();
}

void Parser::ParseTypeMark() {
  DebugPrint("TypeMark");

  // Need to peek, because it could be an identifier so we can't consume the
  // token
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_BOOL || lexeme.token == T_FLOAT ||
      lexeme.token == T_INT || lexeme.token == T_STRING ||
      lexeme.token == T_ENUM) {
    // read the token because it's consumed here
    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token == T_BOOL) {
      // boolean
    } else if (lexeme.token == T_FLOAT) {
      // float
    } else if (lexeme.token == T_INT) {
      // int
    } else if (lexeme.token == T_STRING) {
      // string
    } else if (lexeme.token == T_ENUM) {
      // enum - additional rules
      lexeme = scanner_.GetNextLexeme();
      if (lexeme.token != T_CURLY_LEFT) {
        EmitExpectedTokenError("{", lexeme);
        return;
      }

      // always at least one identifier
      ParseIdentifier();

      // can be multiple identifiers, indicated by comma before next identifier
      lexeme = scanner_.PeekNextLexeme();
      while (lexeme.token == T_COMMA) {
        // check comma - this is redundant
        lexeme = scanner_.GetNextLexeme();
        if (lexeme.token != T_COMMA) {
          EmitExpectedTokenError(",", lexeme);
          return;
        }

        // there's going to be another identifier
        ParseIdentifier();

        // peek for next token to see if more identifiers
        lexeme = scanner_.PeekNextLexeme();
      }

      lexeme = scanner_.GetNextLexeme();
      if (lexeme.token != T_CURLY_RIGHT) {
        EmitExpectedTokenError("}", lexeme);
        return;
      }
    } // end enum if
  } else if(lexeme.token == T_ID) {
    ParseIdentifier();
  } else {
    EmitParsingError("Expected int, string, bool, enum, float, or identifier",
                     lexeme);
    return;
  }
}

void Parser::ParseVariableDeclaration() {
  DebugPrint("VariableDeclaration");

  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_VARIABLE) {
    EmitExpectedTokenError("variable", lexeme);
    return;
  }

  ParseIdentifier();

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_COLON) {
    EmitExpectedTokenError(":", lexeme);
  }

  ParseTypeMark();

  // peek to look for optional bound
  lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_BRACK_LEFT) {
    // there are bounds
    // consume left bracket
    lexeme = scanner_.GetNextLexeme();

    ParseBound();

    // read right bracket
    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_BRACK_RIGHT) {
      EmitExpectedTokenError("]", lexeme);
      return;
    }
  }
}

} // namespace kjlc
