%skeleton "lalr1.cc"
%require  "3.0.1"

%defines 
%define api.namespace {IPL}
%define api.parser.class {Parser}

%define parse.trace

%code requires{
   namespace IPL {
      class Scanner;
   }

  // # ifndef YY_NULLPTR
  // #  if defined __cplusplus && 201103L <= __cplusplus
  // #   define YY_NULLPTR nullptr
  // #  else
  // #   define YY_NULLPTR 0
  // #  endif
  // # endif

}

%printer { std::cerr << $$; } IDENTIFIER
%printer { std::cerr << $$; } INTEGER_CONST
%printer { std::cerr << $$; } FLOAT_CONST
%printer { std::cerr << $$; } STRING_LIT
%printer { std::cerr << $$; } STRUCT_KWD
%printer { std::cerr << $$; } VOID_KWD
%printer { std::cerr << $$; } INT_KWD
%printer { std::cerr << $$; } FLOAT_KWD
%printer { std::cerr << $$; } WHILE_KWD
%printer { std::cerr << $$; } FOR_KWD
%printer { std::cerr << $$; } IF_KWD
%printer { std::cerr << $$; } ELSE_KWD
%printer { std::cerr << $$; } RETURN_KWD
%printer { std::cerr << $$; } INC_OP
%printer { std::cerr << $$; } PTR_OP
%printer { std::cerr << $$; } LE_OP
%printer { std::cerr << $$; } GE_OP
%printer { std::cerr << $$; } EQ_OP
%printer { std::cerr << $$; } NE_OP
%printer { std::cerr << $$; } AND_OP
%printer { std::cerr << $$; } OR_OP


%parse-param { Scanner  &scanner  }
%locations
%code{
   #include <iostream>
   #include <cstdlib>
   #include <fstream>
   #include <string>
   
   
   #include "scanner.hh"
   int nodeCount = 0;

#undef yylex
#define yylex IPL::Parser::scanner.yylex

}




%define api.value.type variant
%define parse.assert

%start translation_unit



%token '\n'
%token <std::string> STRUCT_KWD VOID_KWD INT_KWD FLOAT_KWD WHILE_KWD FOR_KWD IF_KWD ELSE_KWD RETURN_KWD OTHERS
%token <std::string> IDENTIFIER INTEGER_CONST FLOAT_CONST STRING_LIT
%token <std::string> INC_OP PTR_OP LE_OP GE_OP EQ_OP NE_OP AND_OP OR_OP
%token '>' '<' '=' '!' '&' ',' '(' ')' '{' '}' '[' ']' ';' '.' ':'
%left '+' '-'
%left '*' '/'

%nterm <int> translation_unit struct_specifier function_definition type_specifier fun_declarator parameter_list parameter_declaration declarator_arr declarator compound_statement statement_list statement assignment_expression assignment_statement procedure_call expression logical_and_expression equality_expression relational_expression additive_expression unary_expression multiplicative_expression postfix_expression primary_expression expression_list unary_operator selection_statement iteration_statement declaration_list declaration declarator_list

%%
translation_unit: 
          struct_specifier {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"translation_unit\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    function_definition {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"translation_unit\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    translation_unit struct_specifier {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"translation_unit\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << $$ << " -> " << $2 << "\n";
          }
     |    translation_unit function_definition {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"translation_unit\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << $$ << " -> " << $2 << "\n";
          }
     ;

struct_specifier: 
          STRUCT_KWD IDENTIFIER '{' declaration_list '}' ';' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"struct_specifier\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" struct \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\"" << $2 << "\"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" { \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $4 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" } \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ; \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     ;

function_definition: 
          type_specifier fun_declarator compound_statement {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"function_definition\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << $$ << " -> " << $2 << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
          }
     ;

type_specifier: 
          VOID_KWD {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"type_specifier\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" void \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    INT_KWD {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"type_specifier\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" int \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    FLOAT_KWD {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"type_specifier\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" float \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    STRUCT_KWD IDENTIFIER {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"type_specifier\"]\n";               
               std::cout << ++nodeCount << "[label=" << "\" struct \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\"" << $2 << "\"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     ;

fun_declarator: 
          IDENTIFIER '(' parameter_list ')' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"fun_declarator\"]\n";
               std::cout << ++nodeCount << "[label=" << "\"" << $1 << "\"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ( \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ) \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    IDENTIFIER '(' ')' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"fun_declarator\"]\n";
               std::cout << ++nodeCount << "[label=" << "\"" << $1 << "\"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ( \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ) \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     ;

parameter_list: 
          parameter_declaration {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"parameter_list\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    parameter_list ',' parameter_declaration {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"parameter_list\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" , \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
          }
     ;

parameter_declaration: 
          type_specifier declarator {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"parameter_declaration\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << $$ << " -> " << $2 << "\n";
          }
     ;

declarator_arr: 
          IDENTIFIER {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"declarator_arr\"]\n";
               std::cout << ++nodeCount << "[label=" << "\"" << $1 << "\"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    declarator_arr '[' INTEGER_CONST ']' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"declarator_arr\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" [ \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\"" << $3 << "\"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ] \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     ;

declarator: 
          declarator_arr {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"declarator\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    '*' declarator {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"declarator\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" * \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $2 << "\n";
          }
     ;

compound_statement: 
          '{' '}' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"compound_statement\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" { \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" } \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    '{' statement_list '}' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"compound_statement\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" { \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $2 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" } \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    '{' declaration_list statement_list '}' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"compound_statement\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" { \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $2 << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" } \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     ;

statement_list: 
          statement {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"statement_list\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    statement_list statement {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"statement_list\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << $$ << " -> " << $2 << "\n";
          }
     ;

statement: 
          ';' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"statement\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" ; \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    '{' statement_list '}' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"statement\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" { \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $2 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" } \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    selection_statement {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"statement\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    iteration_statement {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"statement\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    assignment_statement {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"statement\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    procedure_call {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"statement\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    RETURN_KWD expression ';' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"statement\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" return \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $2 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ; \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     ;

assignment_expression: 
          unary_expression '=' expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"assignment_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" = \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
          }
     ;

assignment_statement: 
          assignment_expression ';' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"assignment_statement\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ; \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     ;

procedure_call: 
          IDENTIFIER '(' ')' ';' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"procedure_call\"]\n";
               std::cout << ++nodeCount << "[label=" << "\"" << $1 << "\"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ( \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ) \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ; \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    IDENTIFIER '(' expression_list ')' ';' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"procedure_call\"]\n";
               std::cout << ++nodeCount << "[label=" << "\"" << $1 << "\"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ( \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ) \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ; \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     ;

expression: 
          logical_and_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    expression OR_OP logical_and_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" || \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
          }
     ;

logical_and_expression: 
          equality_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"logical_and_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    logical_and_expression AND_OP equality_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"logical_and_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" && \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
          }
     ;

equality_expression: 
          relational_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"equality_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    equality_expression EQ_OP relational_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"equality_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" == \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
          }
     |    equality_expression NE_OP relational_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"equality_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" != \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
          }
     ;

relational_expression: 
          additive_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"relational_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    relational_expression '<' additive_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"relational_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" < \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
          }
     |    relational_expression '>' additive_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"relational_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" > \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
          }
     |    relational_expression LE_OP additive_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"relational_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" <= \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
          }
     |    relational_expression GE_OP additive_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"relational_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" >= \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
          }
     ;

additive_expression: 
          multiplicative_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"additive_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    additive_expression '+' multiplicative_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"additive_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" + \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
          }
     |    additive_expression '-' multiplicative_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"additive_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" - \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
          }
     ;

unary_expression: 
          postfix_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"unary_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    unary_operator unary_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"unary_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << $$ << " -> " << $2 << "\n";
          }
     ;

multiplicative_expression: 
          unary_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"multiplicative_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    multiplicative_expression '*' unary_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"multiplicative_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" * \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
          }
     |    multiplicative_expression '/' unary_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"multiplicative_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" / \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
          }
     ;

postfix_expression: 
          primary_expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"postfix_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    postfix_expression '[' expression ']' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"postfix_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" [ \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ] \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    IDENTIFIER '(' ')' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"postfix_expression\"]\n";
               std::cout << ++nodeCount << "[label=" << "\"" << $1 << "\"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ( \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ) \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    IDENTIFIER '(' expression_list ')' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"postfix_expression\"]\n";
               std::cout << ++nodeCount << "[label=" << "\"" << $1 << "\"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ( \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ) \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    postfix_expression '.' IDENTIFIER {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"postfix_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" . \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\"" << $3 << "\"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    postfix_expression PTR_OP IDENTIFIER {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"postfix_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" -> \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\"" << $3 << "\"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    postfix_expression INC_OP {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"postfix_expression\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ++ \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     ;

primary_expression: 
          IDENTIFIER {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"primary_expression\"]\n";
               std::cout << ++nodeCount << "[label=" << "\"" << $1 << "\"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    INTEGER_CONST {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"primary_expression\"]\n";
               std::cout << ++nodeCount << "[label=" << "\"" << $1 << "\"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    FLOAT_CONST {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"primary_expression\"]\n";
               std::cout << ++nodeCount << "[label=" << "\"" << $1 << "\"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    STRING_LIT {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"primary_expression\"]\n";
               std::cout << ++nodeCount << "[label=" << " \"\\\"\" + " << $1 << " + \"\\\"\"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    '(' expression ')' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"primary_expression\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" ( \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $2 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ) \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     ;

expression_list: 
          expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"expression_list\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    expression_list ',' expression {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"expression_list\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" , \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
          }
     ;

unary_operator: 
          '-' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"unary_operator\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" - \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    '!' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"unary_operator\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" ! \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    '&' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"unary_operator\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" & \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     |    '*' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"unary_operator\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" * \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     ;

selection_statement: 
          IF_KWD '(' expression ')' statement ELSE_KWD statement {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"selection_statement\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" if \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ( \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ) \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $5 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" else \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $7 << "\n";
          }
     ;

iteration_statement: 
          WHILE_KWD '(' expression ')' statement {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"iteration_statement\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" while \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ( \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ) \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $5 << "\n";
          }
     |    FOR_KWD '(' assignment_expression ';' expression ';' assignment_expression ')' statement {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"iteration_statement\"]\n";
               std::cout << ++nodeCount << "[label=" << "\" for \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ( \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ; \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $5 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ; \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $7 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ) \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $9 << "\n";
          }
     ;

declaration_list: 
          declaration {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"declaration_list\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    declaration_list declaration {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"declaration_list\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << $$ << " -> " << $2 << "\n";
          }
     ;

declaration: 
          type_specifier declarator_list ';' {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"declaration\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << $$ << " -> " << $2 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" ; \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
          }
     ;

declarator_list: 
          declarator {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"declarator_list\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
          }
     |    declarator_list ',' declarator {
               $$ = ++nodeCount;
               std::cout << $$ << "[label=\"declarator_list\"]\n";
               std::cout << $$ << " -> " << $1 << "\n";
               std::cout << ++nodeCount << "[label=" << "\" , \"" << "]\n";
               std::cout << $$ << " -> " << nodeCount << "\n";
               std::cout << $$ << " -> " << $3 << "\n";
          }
     ;
%%
void IPL::Parser::error( const location_type &l, const std::string &err_message )
{
   std::cerr << "Error: " << err_message << " at " << l << "\n";
}


