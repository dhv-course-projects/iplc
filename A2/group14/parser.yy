%skeleton "lalr1.cc"
%require  "3.0.1"

%defines 
%define api.namespace {IPL}
%define api.parser.class {Parser}

%define parse.trace

%locations

%code requires{
     #include "symbtab.hh"
     #include "type.hh"
     #include "ast.hh"
     #include "decl.hh"
     #include "location.hh"

     namespace IPL {
          class Scanner;
     }
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
%code{
    #include <iostream>
    #include <cstdlib>
    #include <fstream>
    #include <string>
    #include <map>


    #include "scanner.hh"

    // Returns the ST entry corresponding to the declaration of this identifier
    // Return value is such that Local ST entry if present will override global ST entry
    // Can resolve acc to localST only, if globalST is passed as NULL
    // TODO
    // Return an entry with type void for printf and scanf
    st_entry *resolveIdentifier(std::string name, SymbolTable *globalST, SymbolTable *localST) {
        st_entry *location = NULL;
        if (localST)
            location = localST->getEntry(name);
        if (location)
            return location;
        if (globalST)
            location = globalST->getEntry(name);
        return location;
    }

    // Used in actions for funcall and procedure_call
    // Returns non-zero if the types are compatible else 0
    // return 2 for TO_FLOAT and 3 for TO_INT
    int checkTypeCompatibility(Type *lType, Exp_astnode *rexp) {
        // TODO
        Type *rType = rexp->getType();
        std::string l, r;
        l = lType->getTypeName();
        r = rType->getTypeName();
        if (l == "float" && r == "int")
            return 2;
        else if (l == "int" && r == "float")
            return 3;
        else {
            bool valid = (l == r);
            valid = valid || (
                lType->isImplicitPointer() && 
                (rexp->astnode_type == kIntconst_astnode) && 
                (((Intconst_astnode*) rexp)->getVal() == 0)
            );
            if (!valid && lType->isImplicitPointer() && rType->isImplicitPointer()) {
                std::string l_cast_name, r_cast_name;
                Type *lCastType, *rCastType;
                lCastType = lType->implicitCastPtr();
                rCastType = rType->implicitCastPtr();
                l_cast_name = lCastType->getTypeName();
                r_cast_name = rCastType->getTypeName();
                delete lCastType;
                delete rCastType;
                valid = valid || (l_cast_name == r_cast_name);
                valid = valid || (l_cast_name == "void*");
                valid = valid || (r_cast_name == "void*");
            }
            return valid;
        }
    }

    SymbolTable *globalST = new SymbolTable();
    SymbolTable *currentST = NULL;
    int globalOffset;
    int globalWidth;
    enum Processing {kStructProcessing, kFunctionProcessing};
    Processing currentlyProcessing;
    Type *currentFuncReturnType;
    std::string currentStructName;

    Op_binary_astnode* relational_operation(std::string op_name, Exp_astnode* lhs, Exp_astnode* rhs){
        std::string l,r;
        l = lhs->getType()->getTypeName();
        r = rhs->getType()->getTypeName();
        if(l == "float" && r == "float"){
            return new Op_binary_astnode(op_name + "_FLOAT", lhs, rhs, new Type("int", 4));
        }else
        if(l == "float" && r == "int"){
            return new Op_binary_astnode(op_name + "_FLOAT", lhs, new Op_unary_astnode("TO_FLOAT", rhs, new Type("float", 4)), new Type("int", 4));
        }else
        if(l == "int" && r == "float"){
            return new Op_binary_astnode(op_name + "_FLOAT", new Op_unary_astnode("TO_FLOAT", lhs, new Type("float", 4)), rhs, new Type("int", 4));
        }else
        if(l == "int" && r == "int"){
            return new Op_binary_astnode(op_name + "_INT", lhs, rhs, new Type("int", 4));
        }else{
            int valid = lhs->getType()->isImplicitPointer() && rhs->getType()->isImplicitPointer();            
            if(valid){
                Type* l = lhs->getType()->implicitCastPtr();
                Type* r = rhs->getType()->implicitCastPtr();
                valid = (l->getTypeName() == r->getTypeName());
                delete l;
                delete r;
            }
            if(valid){
                return new Op_binary_astnode(op_name + "_INT", lhs, rhs, new Type("int", 4));
            }else{
                return NULL;
            }
        }
    }

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

%nterm translation_unit struct_specifier function_definition declaration_list declaration declarator_list
%nterm <Fun_decln*> fun_declarator
%nterm <Type*> type_specifier
%nterm <Declarator*> declarator_arr declarator
%nterm <Param_list*> parameter_list parameter_declaration
%nterm <Statement_astnode*> compound_statement statement assignment_statement procedure_call selection_statement iteration_statement
%nterm <Seq_astnode*> statement_list
/* %nterm compound_statement statement_list statement assignment_statement procedure_call selection_statement iteration_statement */
%nterm <Exp_astnode*> expression postfix_expression primary_expression assignment_expression logical_and_expression equality_expression relational_expression additive_expression unary_expression multiplicative_expression
%nterm <ExpList*> expression_list
%nterm <std::string> unary_operator

%%
translation_unit: 
    struct_specifier
    |
    function_definition
    |
    translation_unit struct_specifier
    |
    translation_unit function_definition
;

struct_specifier: 
    STRUCT_KWD IDENTIFIER '{'
    {
        currentST = new SymbolTable();
        globalOffset = 0;
        globalWidth = 0;
        currentlyProcessing = kStructProcessing;
        currentStructName = "struct " + $2;
    } 
    declaration_list '}' ';'
    {
        if (!globalST->addEntry(("struct " + $2), kStruct, kGlobal, globalWidth, 0, NULL, currentST))
            error(@$, "struct " + $2 + " has multiple struct defintions");
        currentST = globalST;
        currentStructName = "";
    }
;

function_definition: 
    type_specifier 
    {
        currentST = new SymbolTable();
        globalOffset = 0;
    } 
    fun_declarator 
    {
        globalOffset = 0;
        currentlyProcessing = kFunctionProcessing;
        currentFuncReturnType = $1;
        if (!globalST->addEntry($3->getName(), kFunction, kGlobal, 0, 0, $1, currentST))
            error(@$, $3->getName() + " has multiple function defintions");
    }
    compound_statement 
    {        
        currentST = globalST;
        globalST->getEntry($3->getName())->set_ast($5);
    }
;

type_specifier: 
    VOID_KWD
    {
        $$ = new Type("void", 0);
    }
    |
    INT_KWD
    {
        $$ = new Type("int", 4);
    }
    |
    FLOAT_KWD
    {
        $$ = new Type("float", 4);
    }
    |
    STRUCT_KWD IDENTIFIER
    {
        if (currentlyProcessing == kStructProcessing && currentStructName == ("struct " + $2))
            $$ = new Type("struct " + $2, 0);
        else {
            int width = globalST->getWidth("struct " + $2);
            if (!width)
                error(@2, "struct " + $2 + " is used but not defined");
            $$ = new Type("struct " + $2, width);
        }
    }
;

fun_declarator: 
    IDENTIFIER '(' 
    <Param_list*> {
        $$ = new Param_list();
    }
    parameter_list
    {
        std::string msg = $4->modifyOffsets(currentST);
        if (msg.length()) {
            error(@4, msg);
            // error(@4, std::to_string(msg.length()));
        }
    }
    ')' 
    {
        $$ = new Fun_decln();
        $$->modifyOrAddName($1);
    }
    |
    IDENTIFIER '(' ')' 
    {
        $$ = new Fun_decln();
        $$->modifyOrAddName($1);
    }
;

parameter_list: 
    <Param_list*> {
        $$ = $<Param_list*>0;
    }
    parameter_declaration
    |
    parameter_list ',' 
    <Param_list*> {
        $$ = $<Param_list*>0;
    }
    parameter_declaration
;

parameter_declaration: 
    type_specifier 
    <Type*> {
        // For parameter declaration we can use Type object of type_specifier itself
        // Can't do this for variable declaration Ex: int a, b; etc.
        $$ = $1;
    }
    declarator
    {
        // Error handling for void type
        // Not needed, is carried out in actions for nterm declarator
        // if (!$3->getSize())
        //     error(@3, $3->getDeclName() + " parameter declaration has size 0, possible reasons: " 
        //         + "void type or array with 0 as one of it's size parameters");
        // Add the parameter to the current symbol table
        if (!currentST->addEntry($3->getDeclName(), kVariable, kParameter, $3->getSize(), 0, (Type *)$3, NULL))
            error(@3, $3->getDeclName() + " parameter has multiple declarations");
        ($<Param_list*>0)->addParam($3);
    }
;

declarator_arr: 
    IDENTIFIER
    {
        $$ = $<Declarator*>0;
        $$->addDeclName($1);
    }
    |
    declarator_arr '[' INTEGER_CONST ']'
    {
        $$ = $1;
        $$->addArray(std::stoi($3));
    }
;

declarator: 
    <Declarator*> {
        if ($<Type*>0->getSize())
            $$ = new Declarator($<Type*>0);
        else if (currentlyProcessing == kStructProcessing && $<Type*>0->getBasicTypeName() == currentStructName)
            error(@$, " Cannot have non-pointer derived types of \"" + currentStructName + "\"");
        else
            error(@$, " Cannot have zero size variable of type \"void\"");
    }
    declarator_arr
    {
        $$ = $2;
    }
    |
    '*'
    <Type*> {
        $$ = $<Type*>0;
        $$->addPointer();
    } 
    declarator
    {
        $$ = $3;
    }
;

compound_statement: 
    '{' '}' 
    {
        $$ = new Seq_astnode();
    }
    |
    '{' statement_list '}' 
    {
        $$ = $2;
    }
    |    
    '{' declaration_list '}' 
    {
        $$ = new Seq_astnode();
    }
    |    
    '{' declaration_list statement_list '}' 
    {
        $$ = $3;
    }
;

statement_list: 
    statement 
    {
        $$ = new Seq_astnode(); 
        $$->add_statement($1);
    }
    |
    statement_list statement
    {
        $1->add_statement($2); 
        $$ = $1;
    }
;

statement: 
    ';'
    {
        $$ = new Empty_astnode();
    }
    |    
    '{' statement_list '}' 
    {
        $$ = $2;
    }
    |    
    selection_statement 
    {
        $$ = $1;
    }
    |    
    iteration_statement 
    {
        $$ = $1;
    }
    |
    assignment_statement 
    {
        $$ = $1;
    }
    |
    procedure_call 
    {
        $$ = $1;
    }
    |
    RETURN_KWD expression ';' 
    {
        // Check type compatibility with function signature and cast if needed
        // Return type of function is in currentFuncReturnType global
        std::string l, r;
        l = currentFuncReturnType->getTypeName();
        r = $2->getType()->getTypeName();
        // If the parser executes this action $2 can't be of void type
        // If it was of void type, the error would have been detected earlier itself
        if (l == r)
            $$ = new Return_astnode($2);
        else if (l == "float" && r == "int")
            $$ = new Return_astnode(new Op_unary_astnode("TO_FLOAT", $2, new Type(currentFuncReturnType)));
        else if (l == "int" && r == "float")
            $$ = new Return_astnode(new Op_unary_astnode("TO_INT", $2, new Type(currentFuncReturnType)));
        else {
            // Error Checking
            if (l == "void")
                error(@1, "Can't return a value for a function with void return type");
            else
                error(@1, "Function return type " + l + " is not type compatible with type of return value " + r );
        }
    }
;

assignment_expression: 
    unary_expression '=' expression 
    {
        // check if can be assigned?
        // TODO
        // Array analysis (using lvalue)
        enum TypeExp ast_type = $1->astnode_type;
        bool hasLvalue = (ast_type == kIdentifier_astnode) || (ast_type == kArrayref_astnode) || (ast_type == kMember_astnode) || (ast_type == kArrow_astnode);
        hasLvalue = hasLvalue || (
            ast_type == kOp_unary_astnode && (
                ((Op_unary_astnode*)$1)->get_op_type() == "DEREF"
            )
        );
        if(!hasLvalue){
            error(@1, "Left operand does not have an lvalue");
        }
        Type *lType, *rType;
        lType = $1->getType();
        rType = $3->getType();
        std::string l, r;
        l = lType->getTypeName();
        r = rType->getTypeName();
        if(lType->isArray()){
            error(@1, "Incompatible assignment when assigning to type \"" + l + "\" from type \"" + r + "\", lhs array is pointer-const");
        }
        // If the parser executes this action $2 can't be of void type
        // If it was of void type, the error would have been detected earlier itself
        if (l == "float" && r == "int")
            $$ = new AssignE_astnode($1, new Op_unary_astnode("TO_FLOAT", $3, $1->getType()), $1->getType());
        else if (l == "int" && r == "float")
            $$ = new AssignE_astnode($1, new Op_unary_astnode("TO_INT", $3, $1->getType()), new Type($1->getType()));
        else {
            // Pointer analysis
            // Identify pointer using isImplicitPointer() != 0
            // Void * to any pointer and any pointer to Void * is allowed
            // lhs_type == implicitCastPointer(rhs_type)
            bool valid = (l == r);
            valid = valid || (
                lType->isImplicitPointer() && ($3->astnode_type == kIntconst_astnode) && (((Intconst_astnode*)$3)->getVal() == 0)
            );
            if(!valid && lType->isImplicitPointer() && rType->isImplicitPointer()){
                Type* r_cast = rType->implicitCastPtr();
                std::string r_name = r_cast->getTypeName();
                delete r_cast;
                valid = valid || (l == r_name);
                valid = valid || (r_name == "void*");
                valid = valid || (l == "void*");
            }
            if (valid){
                $$ = new AssignE_astnode($1, $3, $1->getType());
            }else {
                error (@2, "Incompatible assignment for lexp type " + l + " and rexp type " + r);
            }
        }
    }
;

assignment_statement: 
    assignment_expression ';' 
    {
        // deletes $1 in the process, makes AssignS from AssignE
        $$ = new AssignS_astnode((AssignE_astnode*)$1); 
    }
;

procedure_call: 
    IDENTIFIER '(' ')' ';' 
    {
        // TODO
        // Should do this for funcall too
        // Check number and type of parameters being passed against function signature
        st_entry *entry = resolveIdentifier($1, globalST, NULL);
        if (!entry)
            error(@1, "Procedure call to undefined function named " + $1);
        std::vector<Type*> param_types = entry->getLocalSymbolTable()->getParams();
        if (param_types.size() != 0)
            error(@1, "Too few arguments passed for function call of " + $1);
        $$ = new Proccall_astnode($1);
    }
    |
    IDENTIFIER '(' expression_list ')' ';' 
    {
        // TODO
        // Should do this for funcall too
        // Check number and type of parameters being passed against function signature
        // Store a vector of parameter types (to get in order sort according to offset size in decreasing order)
        // Check type compatibility as done in assignment_expression
        /* 
            OR 
        */
        // Can write a global function to cover both proc_call and func_call
        st_entry *entry = resolveIdentifier($1, globalST, NULL);
        if (!entry){
            if($1=="printf" || $1=="scanf"){
                $$ = new Proccall_astnode($1, $3);
            }else{
                error(@1, "Procedure call to undefined function named " + $1);
            }
        }else{
            std::vector<Type*> param_types = entry->getLocalSymbolTable()->getParams();
            if ((int) param_types.size() < $3->getListSize()){
                error(@1, "Too many arguments passed for function call of " + $1);
            }else
            if ((int) param_types.size() > $3->getListSize()){
                error(@1, "Too few arguments passed for function call of " + $1);
            }
            for (int i = 0; i < (int) param_types.size(); i++) {
                // Check type compatibility
                int compatible = checkTypeCompatibility(param_types[i], $3->getExpAt(i));
                if (!compatible){
                    error(@1, "Expected argument type " + param_types[i]->getTypeName() + 
                    " but passed parameter of type " + $3->getExpAt(i)->getType()->getTypeName() + 
                    " for function call of " + $1);
                }else
                if(compatible != 1){ // need to cast int to float or float to int
                    $3->castExpAt(i, (compatible==2 ? "FLOAT" : "INT"));
                }
            }
            $$ = new Proccall_astnode($1, $3);
        }
    }
;

expression: 
    logical_and_expression
    {
        $$ = $1;
    }
    |
    expression OR_OP logical_and_expression
    {
        if($1->getType()->isTypeStruct() || $3->getType()->isTypeStruct()){
            error(@2, "Invalid operand types for binary OR_OP ,\"" + $1->getType()->getTypeName() + "\" and \"" + $3->getType()->getTypeName() + "\"");
        }
        $$ = new Op_binary_astnode("OR_OP", $1, $3, new Type("int", 4));
    }
;

logical_and_expression: 
    equality_expression
    {
        $$ = $1;
    }
    |
    logical_and_expression AND_OP equality_expression
    {
        if($1->getType()->isTypeStruct() || $3->getType()->isTypeStruct()){
            error(@2, "Invalid operand types for binary AND_OP ,\"" + $1->getType()->getTypeName() + "\" and \"" + $3->getType()->getTypeName() + "\"");
        }
        $$ = new Op_binary_astnode("AND_OP", $1, $3, new Type("int", 4));
    }
;

equality_expression: 
    relational_expression
    {
        $$ = $1;
    }
    |
    equality_expression EQ_OP relational_expression
    {
        $$ = relational_operation("EQ_OP", $1, $3);
        if(!$$){error(@2, "Invalid operand types for binary EQ_OP ,\"" + $1->getType()->getTypeName() + "\" and \"" + $3->getType()->getTypeName() + "\"");}
    }
    |
    equality_expression NE_OP relational_expression
    {
        $$ = relational_operation("NE_OP", $1, $3);
        if(!$$){error(@2, "Invalid operand types for binary NE_OP ,\"" + $1->getType()->getTypeName() + "\" and \"" + $3->getType()->getTypeName() + "\"");}
    }
;

relational_expression: 
    additive_expression
    {
        $$ = $1;
    }
    |
    relational_expression '<' additive_expression
    {
        $$ = relational_operation("LT_OP", $1, $3);
        if(!$$){error(@2, "Invalid operand types for binary LT_OP ,\"" + $1->getType()->getTypeName() + "\" and \"" + $3->getType()->getTypeName() + "\"");}
    }
    |
    relational_expression '>' additive_expression
    {
        $$ = relational_operation("GT_OP", $1, $3);
        if(!$$){error(@2, "Invalid operand types for binary GT_OP ,\"" + $1->getType()->getTypeName() + "\" and \"" + $3->getType()->getTypeName() + "\"");}
    }
    |
    relational_expression LE_OP additive_expression
    {
        $$ = relational_operation("LE_OP", $1, $3);
        if(!$$){error(@2, "Invalid operand types for binary LE_OP ,\"" + $1->getType()->getTypeName() + "\" and \"" + $3->getType()->getTypeName() + "\"");}
    }
    |
    relational_expression GE_OP additive_expression
    {
        $$ = relational_operation("GE_OP", $1, $3);
        if(!$$){error(@2, "Invalid operand types for binary GE_OP ,\"" + $1->getType()->getTypeName() + "\" and \"" + $3->getType()->getTypeName() + "\"");}
    }
;

additive_expression: 
    multiplicative_expression
    {
        $$ = $1;
    }
    |
    additive_expression '+' multiplicative_expression
    {
        std::string l,r;
        l = $1->getType()->getTypeName();
        r = $3->getType()->getTypeName();
        if(l == "float" && r == "float"){
            $$ = new Op_binary_astnode("PLUS_FLOAT", $1, $3, new Type("float", 4));
        }else
        if(l == "float" && r == "int"){
            $$ = new Op_binary_astnode("PLUS_FLOAT", $1, new Op_unary_astnode("TO_FLOAT", $3, new Type("float", 4)), new Type("float", 4));
        }else
        if(l == "int" && r == "float"){
            $$ = new Op_binary_astnode("PLUS_FLOAT", new Op_unary_astnode("TO_FLOAT", $1, new Type("float", 4)), $3, new Type("float", 4));
        }else
        if(l == "int" && r == "int"){
            $$ = new Op_binary_astnode("PLUS_INT", $1, $3, new Type("int", 4));
        }else{
            Type* output_type;
            int valid = 0;
            if(l=="int"){
                if($3->getType()->isImplicitPointer()){
                    valid = 1;
                    output_type = $3->getType()->implicitCastPtr();
                }
            }else
            if(r=="int"){
                if($1->getType()->isImplicitPointer()){
                    valid = 1;
                    output_type = $1->getType()->implicitCastPtr();
                }
            }
            if(valid){
                $$ = new Op_binary_astnode("PLUS_INT", $1, $3, output_type);
            }else{
                error(@3, "Invalid operand types for binary + ,\"" + l + "\" and \"" + r + "\"");
            }
        }
        /*$$ = NULL;*/
    }
    |
    additive_expression '-' multiplicative_expression
    {
        std::string l,r;
        l = $1->getType()->getTypeName();
        r = $3->getType()->getTypeName();
        if(l == "float" && r == "float"){
            $$ = new Op_binary_astnode("MINUS_FLOAT", $1, $3, new Type("float", 4));
        }else
        if(l == "float" && r == "int"){
            $$ = new Op_binary_astnode("MINUS_FLOAT", $1, new Op_unary_astnode("TO_FLOAT", $3, new Type("float", 4)), new Type("float", 4));
        }else
        if(l == "int" && r == "float"){
            $$ = new Op_binary_astnode("MINUS_FLOAT", new Op_unary_astnode("TO_FLOAT", $1, new Type("float", 4)), $3, new Type("float", 4));
        }else
        if(l == "int" && r == "int"){
            $$ = new Op_binary_astnode("MINUS_INT", $1, $3, new Type("int", 4));
        }else
        if(r == "int" && $1->getType()->isImplicitPointer()){
            $$ = new Op_binary_astnode("MINUS_INT", $1, $3, $1->getType()->implicitCastPtr());
        }else
        if($1->getType()->isImplicitPointer() && $3->getType()->isImplicitPointer()){
            Type* _l = $1->getType()->implicitCastPtr();
            Type* _r = $3->getType()->implicitCastPtr();
            bool valid = (_l->getTypeName() == _r->getTypeName());
            delete _l; delete _r;
            if(valid){
                $$ = new Op_binary_astnode("MINUS_INT", $1, $3, new Type("int", 4));
            }else{
                error(@3, "Invalid operand types for binary - ,\"" + l + "\" and \"" + r + "\"");
            }
        }else{
            error(@3, "Invalid operand types for binary - ,\"" + l + "\" and \"" + r + "\"");
        }
        /*$$ = NULL;*/
    }
;

unary_expression: 
    postfix_expression
    {
        $$ = $1;
    }
    |
    unary_operator unary_expression
    {
        Type* output_type = NULL;
        if($1 == "UMINUS"){
            std::string type_name = $2->getType()->getTypeName();
            bool valid = (type_name == "int" || type_name == "float");
            output_type = (valid ? $2->getType() : NULL);
        }
        else if($1 == "NOT"){
            output_type = ( $2->getType()->isTypeStruct()) ? NULL : new Type("int", 4);}
        else if($1 == "ADDRESS"){
            enum TypeExp ast_type = $2->astnode_type;
            bool hasLvalue = (ast_type == kIdentifier_astnode) || (ast_type == kArrayref_astnode) || (ast_type == kMember_astnode) || (ast_type == kArrow_astnode);
            hasLvalue = hasLvalue || (
                ast_type == kOp_unary_astnode && (
                    ((Op_unary_astnode*)$2)->get_op_type() == "DEREF"
                )
            );
            output_type = ( hasLvalue ? $2->getType()->addressOf() : NULL);
        }
        else if($1 == "DEREF"){output_type = $2->getType()->dereference();}
        
        if(output_type){
            $$ =  new Op_unary_astnode($1, $2, output_type);
        } else {
            error(@1, "invalid type " + $2->getType()->getTypeName() + " for unary operator " + $1);
        }
    }
;

multiplicative_expression: 
    unary_expression
    {
        $$ = $1;
    }
    |
    multiplicative_expression '*' unary_expression
    {
        std::string l,r;
        l = $1->getType()->getTypeName();
        r = $3->getType()->getTypeName();
        if(l == "float" && r == "float"){
            $$ = new Op_binary_astnode("MULT_FLOAT", $1, $3, new Type("float", 4));
        }else
        if(l == "float" && r == "int"){
            $$ = new Op_binary_astnode("MULT_FLOAT", $1, new Op_unary_astnode("TO_FLOAT", $3, new Type("float", 4)), new Type("float", 4));
        }else
        if(l == "int" && r == "float"){
            $$ = new Op_binary_astnode("MULT_FLOAT", new Op_unary_astnode("TO_FLOAT", $1, new Type("float", 4)), $3, new Type("float", 4));
        }else
        if(l == "int" && r == "int"){
            $$ = new Op_binary_astnode("MULT_INT", $1, $3, new Type("int", 4));
        }else{
            error(@3, "Invalid operand types for binary * ,\"" + l + "\" and \"" + r + "\"");
        }
    }
    |
    multiplicative_expression '/' unary_expression
    {
        std::string l,r;
        l = $1->getType()->getTypeName();
        r = $3->getType()->getTypeName();
        if(l == "float" && r == "float"){
            $$ = new Op_binary_astnode("DIV_FLOAT", $1, $3, new Type("float", 4));
        }else
        if(l == "float" && r == "int"){
            $$ = new Op_binary_astnode("DIV_FLOAT", $1, new Op_unary_astnode("TO_FLOAT", $3, new Type("float", 4)), new Type("float", 4));
        }else
        if(l == "int" && r == "float"){
            $$ = new Op_binary_astnode("DIV_FLOAT", new Op_unary_astnode("TO_FLOAT", $1, new Type("float", 4)), $3, new Type("float", 4));
        }else
        if(l == "int" && r == "int"){
            $$ = new Op_binary_astnode("DIV_INT", $1, $3, new Type("int", 4));
        }else{
            error(@3, "Invalid operand types for binary / ,\"" + l + "\" and \"" + r + "\"");
        }
    }
;

postfix_expression: 
    primary_expression
    {
        $$ = $1;
    }
    |
    postfix_expression '[' expression ']'
    {
        Type *type = $1->getType();
        Type* new_type = type->indexInto();
        if (!new_type)
            error(@1, "Not an array, cannot index into");
        // Also further need to check if the expression returns an integer or not
        if($3->getType()->getTypeName() != "int"){
            error(@1, "Array subscript is not an integer");
        }
        $$ = new Arrayref_astnode($1, $3, new_type);
    }
    |
    IDENTIFIER '(' ')'
    {
        // Function call
        auto entry = resolveIdentifier($1, globalST, NULL);
        if (!entry)
            error(@1, $1 + " has no function declaration");
        // TODO
        // Add the return type to the constructor
        std::vector<Type*> param_types = entry->getLocalSymbolTable()->getParams();
        if (param_types.size() != 0)
            error(@1, "Too few arguments passed for function call of " + $1);
        $$ = new Funcall_astnode($1, NULL, entry->getType());
    }
    |
    IDENTIFIER '(' expression_list ')'
    {
        // Function call
        auto entry = resolveIdentifier($1, globalST, NULL);
        if (!entry){
            if($1=="printf" || $1=="scanf"){
                error(@1, "tried to use void expression");
            }
            error(@1, $1 + " has no function declaration");
        }
        // TODO
        // Add the return type to the constructor
        // Check number and type of parameters being passed against function signature
        // Store a vector of parameter types (to get in order sort according to offset size in decreasing order)
        // Check type compatibility as done in assignment_expression
        // Return type can't be void
        std::vector<Type*> param_types = entry->getLocalSymbolTable()->getParams();
        if ((int) param_types.size() < $3->getListSize()){
            error(@1, "Too many arguments passed for function call of " + $1);
        }else
        if ((int) param_types.size() > $3->getListSize()){
            error(@1, "Too few arguments passed for function call of " + $1);
        }
        for (int i = 0; i < (int) param_types.size(); i++) {
            // Check type compatibility
            int compatible = checkTypeCompatibility(param_types[i], $3->getExpAt(i));
            if (!compatible){
                error(@1, "Expected argument type " + param_types[i]->getTypeName() + 
                " but passed parameter of type " + $3->getExpAt(i)->getType()->getTypeName() + 
                " for function call of " + $1);
            }else
            if(compatible != 1){ // need to cast int to float or float to int
                $3->castExpAt(i, (compatible==2 ? "FLOAT" : "INT"));
            }
        }
        if (entry->getType()->getTypeName() == "void") {
            error(@1, "tried to use void expression");
        }
        $$ = new Funcall_astnode($1, $3, entry->getType());
    }
    |
    postfix_expression '.' IDENTIFIER
    {
        std::string type_name = ($1->getType())->getTypeName();
        auto entry = resolveIdentifier(type_name, globalST, NULL);
        if (!entry || !entry->isTypeStruct())
            error(@1, "Performing member access operation on a non-struct " + type_name);
        entry = resolveIdentifier($3, NULL, entry->getLocalSymbolTable());
        if (!entry)
            error(@3, $3 + " is not a valid field for " + type_name);
        $$ = new Member_astnode($1, new Identifier_astnode($3, entry->getType()), entry->getType());
    }
    |
    postfix_expression PTR_OP IDENTIFIER
    {
        // Refer above action for ideas
        // Array and Pointer handling is not yet completely implemented
        Type* lhs_ptr = $1->getType();
        Type* lhs = lhs_ptr->dereference();
        if(lhs){
            std::string type_name = lhs->getTypeName();
            auto entry = resolveIdentifier(type_name, globalST, NULL);
            if (!entry || !entry->isTypeStruct())
                error(@1, "Performing member access operation on a non-struct " + type_name);
            entry = resolveIdentifier($3, NULL, entry->getLocalSymbolTable());
            if (!entry)
                error(@3, $3 + " is not a valid field for " + type_name);
            $$ = new Arrow_astnode($1, new Identifier_astnode($3, entry->getType()), entry->getType());
        }
    }
    |
    postfix_expression INC_OP
    {
        if($1->getType()->isTypeStruct()){
            error(@2, "Invalid operand types for unary INC_OP ,\"" + $1->getType()->getTypeName() + "\"");
        }
        $$ = new Op_unary_astnode("PP", $1, $1->getType());
    }
;

primary_expression: 
    IDENTIFIER
    {
        auto x = resolveIdentifier($1, NULL, currentST);
        if (!x)
            error(@1, $1 + " is used but not declared");
        $$ = new Identifier_astnode($1, x->getType());
    }
    |
    INTEGER_CONST
    {
        $$ = new Intconst_astnode(std::stoi($1));
    }
    |
    FLOAT_CONST
    {
        $$ = new Floatconst_astnode(std::stof($1));
    }
    |
    STRING_LIT
    {
        $$ = new Stringconst_astnode($1);
    }
    |
    '(' expression ')'
    {
        $$ = $2;
    }
;

expression_list: 
    expression
    {
        // Temporarily
        $$ = new ExpList($1);
    }
    |
    expression_list ',' expression
    {
        // Temporarily
        $1->addExp($3);
        $$ = $1;
    }
;

unary_operator: 
    '-' 
    {
        $$ = "UMINUS";
    }
    |
    '!' 
    {
        $$ = "NOT";
    }
    |
    '&' 
    {
        $$ = "ADDRESS";
    }
    |
    '*' 
    {
        $$ = "DEREF";
    }
;

selection_statement: 
    IF_KWD '(' expression ')' statement ELSE_KWD statement 
    {
        $$ = new If_astnode($3, $5, $7);
    }
;

iteration_statement: 
    WHILE_KWD '(' expression ')' statement 
    {
        $$ = new While_astnode($3, $5);
    }
    |
    FOR_KWD '(' assignment_expression ';' expression ';' assignment_expression ')' statement 
    {
        $$ = new For_astnode($3, $5, $7, $9);
    }
;

declaration_list: 
    declaration
    |
    declaration_list declaration
;

declaration: 
    type_specifier 
    <Type*> {
        $$ = $1;
    }
    declarator_list ';'
;

declarator_list:
    <Type*> {
        // Each element of the list needs to have seperate Type object
        $$ = new Type($<Type*>0);
    }
    declarator
    {
        // if (!$2) {
        //     error(@2, "Cause of segmentation fault at declarator_list is $2(declarator) is a null pointer");
        // }
        if (currentlyProcessing == kStructProcessing) {
            if (!currentST->addEntry($2->getDeclName(), kVariable, kLocal, $2->getSize(), globalOffset, (Type *)$2, NULL))
                error(@2, $2->getDeclName() + " has multiple declarations");
            globalOffset += $2->getSize();
            globalWidth += $2->getSize();
        } else {
            globalOffset -= $2->getSize();
            if (!currentST->addEntry($2->getDeclName(), kVariable, kLocal, $2->getSize(), globalOffset, (Type *)$2, NULL))
                error(@2, $2->getDeclName() + " has multiple declarations");
        }
    }
    |
    declarator_list ',' 
    <Type*> {
        $$ = new Type($<Type*>0);
    }
    declarator
    {
        if (currentlyProcessing == kStructProcessing) {
            if (!currentST->addEntry($4->getDeclName(), kVariable, kLocal, $4->getSize(), globalOffset, (Type *)$4, NULL))
                error(@4, $4->getDeclName() + " has multiple declarations");
            globalOffset += $4->getSize();
            globalWidth += $4->getSize();
        } else {
            globalOffset -= $4->getSize();
            if (!currentST->addEntry($4->getDeclName(), kVariable, kLocal, $4->getSize(), globalOffset, (Type *)$4, NULL))
                error(@4, $4->getDeclName() + " has multiple declarations");
        }
    }
;
%%
void IPL::Parser::error( const location_type &l, const std::string &err_message )
{
    std::cout << "Error at line " << l.begin.line << ": " << err_message << "\n";
    exit(1);
}


