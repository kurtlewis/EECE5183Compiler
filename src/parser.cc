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
Parser::Parser(std::string filename) : scanner_(filename), error_state_(false) {

}

// Deconstructor
Parser::~Parser() {

}

void Parser::ParseProgram() {
  ParseProgramHeader();
  ParseProgramBody();
}

void Parser::EmitParsingError(std::string message, Lexeme lexeme) {
  std::cout << "Line:" << lexeme.line << " Col:" << lexeme.column;
  std::cout << " - " << message << std::endl;
  error_state_ = true;
}

void Parser::EmitExpectedTokenError(std::string expected_token, Lexeme lexeme) {
  std::cout << "Line:" << lexeme.line << " Col:" << lexeme.column;
  std::cout << " - " << "Expected token '" << expected_token << "'";
  std::cout << std::endl;
  error_state_ = true;
}

void Parser::ParseArgumentList() {
  ParseExpression();

  // more arguments are optional
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_COMMA) {
    // consume comma
    lexeme = scanner_.GetNextLexeme();
    ParseArgumentList();
  }
}

void Parser::ParseAssignmentStatement() {
  ParseDestination();

  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token == T_COL_EQ) {
    EmitExpectedTokenError(":=", lexeme);
    return;
  }

  ParseExpression();
}

void Parser::ParseBound() {
  // peek for '-'
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_MINUS) {
    // read the dash off the input 
    lexeme = scanner_.GetNextLexeme();
  }

  ParseNumber();
}

void Parser::ParseDeclaration() {
  // peek because the first terminal could be a number of things, some of
  // which involve reaching down rule evaluations
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_GLOBAL) {
    // there's a global keyword
    // scan it for real
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
  }
}

void Parser::ParseDestination() {
  ParseIdentifier();

  // peek to see if there are brackets
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_BRACK_LEFT) {
    // consume token
    lexeme = scanner_.GetNextLexeme();

    ParseExpression();

    // ending bracket is required
    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token == T_BRACK_RIGHT) {
      EmitExpectedTokenError("]", lexeme);
      return;
    }
  }
}

void Parser::ParseExpression() {
  // TODO
}

void Parser::ParseIdentifier() {
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_ID) {
    EmitParsingError("Expected Identifier", lexeme);
    return;
  }
  // TODO: probably some symbol table stuff
}

void Parser::ParseIfStatement() {
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
  //ReturnType ret = ParseExpression();

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
  // it is required that there is at least one statement
  while (lexeme.token != T_ELSE && lexeme.token != T_END) {
    // Handle parsing the statement
    // ReturnType ret = ParseStatement();
    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_SEMI_COLON) {
      EmitExpectedTokenError(";", lexeme);
      return;
    }
    lexeme = scanner_.PeekNextLexeme();
  }

  // the last lexeme was only peeked, read it now
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token == T_ELSE) {
    // handle else statement
    while (lexeme.token != T_END) {
      // handle parsing the statements
      // ReturnType ret = ParseStatement();
      lexeme = scanner_.GetNextLexeme();
      if (lexeme.token != T_SEMI_COLON) {
        EmitExpectedTokenError(";", lexeme);
        return;
      }
      lexeme = scanner_.PeekNextLexeme();
    }
    // read the peeeked lexeme
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
  // TODO
}

void Parser::ParseNumber() {
  // TODO
}

void Parser::ParseParameter() {
  ParseVariableDeclaration(); 
}

void Parser::ParseParameterList() {
  ParseParameter();

  // check to see if there are multiple parameters - indicated by ','
  Lexeme lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_COMMA) {
    // read the lexeme for real
    lexeme = scanner_.GetNextLexeme();
    // recursive call to read more parameters
    ParseParameterList();
  }
}

void Parser::ParseProcedureBody() {
  // Peek the first lexeme to determine if there are any declarations
  Lexeme lexeme = scanner_.PeekNextLexeme(); 
  while (lexeme.token != T_BEGIN) {
    // There are declarations
    ParseDeclaration();
    // Parse ';'
    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_SEMI_COLON) {
      EmitExpectedTokenError(";", lexeme);
      return;
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
  lexeme = scanner_.PeekNextLexeme();
  while (lexeme.token != T_END) {
    // there are statement(s) to read
    ParseStatement();
    // Parse ';'
    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_SEMI_COLON) {
      EmitExpectedTokenError(";", lexeme);
      return;
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

void Parser::ParseProcedureCall() {
  ParseIdentifier();

  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PAREN_LEFT) {
    EmitExpectedTokenError("(", lexeme);
    return;
  }

  ParseArgumentList();

  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PAREN_RIGHT) {
    EmitExpectedTokenError(")", lexeme);
    return;
  }
}

void Parser::ParseProcedureDeclaration() {
  ParseProcedureHeader();
  ParseProcedureBody();
}

void Parser::ParseProcedureHeader() {
  // parse procedure keyword
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PROCEDURE) {
    EmitExpectedTokenError("procedure", lexeme);
    return;
  }
  
  // read the identifier
  ParseIdentifier();

  // type mark - todo
  ParseTypeMark();

  // read left paren
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_PAREN_LEFT) {
    EmitExpectedTokenError("(", lexeme);
    return;
  }

  // peek to see if there is an optional parameter list
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
  // Parse leading declarations
  Lexeme lexeme = scanner_.PeekNextLexeme();
  while (lexeme.token != T_BEGIN) {
    // parse declaration
    // TODO: declaration parse
    // retType ret = ParseDeclaration();
    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_SEMI_COLON) {
      EmitExpectedTokenError(";", lexeme);
      return;
    }
    lexeme = scanner_.PeekNextLexeme();
  }

  // Parse 'begin'
  lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_BEGIN) {
    EmitExpectedTokenError(";", lexeme);
    return;
  }

  // parse statements
  lexeme = scanner_.PeekNextLexeme();
  while (lexeme.token != T_END) {
    // parse statements
    // TODO: statement parse
    // retType ret = ParseStatement();
    lexeme = scanner_.GetNextLexeme();
    if (lexeme.token != T_SEMI_COLON) {
      EmitExpectedTokenError(";", lexeme);
      return;
    }
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

void Parser::ParseReturnStatement() {
  // TODO
}

void Parser::ParseStatement() {
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

void Parser::ParseTypeDeclaration() {
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

      lexeme = scanner_.PeekNextLexeme();
      while (lexeme.token == T_COMMA) {
        lexeme = scanner_.GetNextLexeme();
        if (lexeme.token != T_COMMA) {
          EmitExpectedTokenError(",", lexeme);
          return;
        }

        // there's going to be another identifier
        ParseIdentifier();

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
  Lexeme lexeme = scanner_.GetNextLexeme();
  if (lexeme.token != T_VARIABLE) {
    EmitExpectedTokenError("variable", lexeme);
    return;
  }

  ParseIdentifier();

  ParseTypeMark();

  // peek to look for optional bound
  lexeme = scanner_.PeekNextLexeme();
  if (lexeme.token == T_BRACK_LEFT) {
    // there are bounds
    lexeme = scanner_.GetNextLexeme();
    // we already know it's a left bracket

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
