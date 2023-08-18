/*
    Following Google's c++ style guide
*/
#include "type.hh"


#ifndef AST_H
#define AST_H

// genCode globals
extern std::vector<std::string> ro_consts;
extern int label_count; // start at 0
extern std::vector<int> avlbl_reg; // start at {6,5,4,3,2,1}

class st_entry;
class SymbolTable;

extern SymbolTable* globalST;
extern SymbolTable* currentST;

int resolveIdentifierOffset(std::string name, SymbolTable *globalST, SymbolTable *localST);
Type* resolveIdentifierType(std::string name, SymbolTable *globalST, SymbolTable *localST);
int isIdentifierParam(std::string name, SymbolTable *globalST, SymbolTable *localST);
int structMemberOffset(std::string structName, SymbolTable *globalST, std::string fieldName);
int functionReturnTypeWidth(std::string name, SymbolTable *globalST, SymbolTable *localST);
Type* functionReturnType(std::string name, SymbolTable *globalST, SymbolTable *localST);
int pushl_struct(std::string reg_addr, int offset, std::string structName);
void popl_struct(std::string reg_addr, int offset, std::string structName);
void return_struct(std::string reg_addr, int offset, int dest, std::string structName);
int getCurrentParamStackSize();

std::string reg_no_to_name(int i);
void swap_rstack();

void genASM(std::string op1, std::string op2, std::string op_type);

/*
    Abstract classes
*/
class Abstract_astnode {
  protected:
    // Type *type;  
  public:
    enum TypeExp astnode_type;
    virtual void print(int blanks) = 0;
    virtual ~Abstract_astnode(){}
};

class Statement_astnode: public Abstract_astnode {
  protected:
    //
  public:
    virtual void genCode() = 0;
    // truelist falselist here later probably
};

class Exp_astnode: public Abstract_astnode {
  protected:
    // When can type be NULL?
    // 1) For Funcall_astnode
    // 2) For (Int/Float/String)const_astnode
    Type *type;
  public:
    Exp_astnode(Type *type) {
        this->type = type;
    }
    Type *getType() {
        return type;
    }
    int label;
    int isRightLeaf = 0;
    virtual int genLabel(int isRightChild) = 0;
    virtual void genValue() = 0;
    virtual void genAddr() = 0; // only called for ast_nodes with l-value
    // genValue puts the value of the expression in register at top of stack
    // genAddr puts the address of the expression-lvalue in register at top of stack
};

class Ref_astnode: public Exp_astnode {
  protected:
    // 
  public:
    Ref_astnode(Type *type) : Exp_astnode(type) {}
};

class Intconst_astnode: public Exp_astnode {
  protected:
    int val;
  public:
    Intconst_astnode(int val) : Exp_astnode(new Type("int", 4)) {
        astnode_type = kIntconst_astnode;
        this->val = val;
    }
    int genLabel(int isRightChild){
        label = !isRightChild;
        isRightLeaf = isRightChild;
        return label;
    }
    void genValue(){
        std::cout << "\tmovl\t$" << val << ", %" << reg_no_to_name(avlbl_reg.back()) << "\n";
    }
    void genAddr(){}
    int genCode(int rvalue_reg){
        // rvalue_reg = 0 expected
        int reg = avlbl_reg.back(); avlbl_reg.pop_back();
        std::cout << "\tmovl\t$" << val << ", %" << reg_no_to_name(reg) << "\n";
        return reg;
    }
    void print(int blanks) {
        std::cout << "{\"intconst\": " << val << "}";
    }
    int getVal(){
        return val;
    }
};
class Stringconst_astnode: public Exp_astnode {
  protected:
    std::string val;
  public:
    Stringconst_astnode(std::string val) : Exp_astnode(new Type("string_literal", 4)) {
        astnode_type = kStringconst_astnode;
        this->val = val;
    }
    int genLabel(int isRightChild){
        label = 0;
        return label;
    }
    void genValue(){}
    void genAddr(){}
    void print(int blanks) {
        std::cout << "{\"stringconst\": " << val << "}";
    }
    std::string getVal(){
        return val;
    }
};
class Identifier_astnode: public Ref_astnode {
  protected:
    std::string name;
  public:
    Identifier_astnode(std::string name, Type *type) : Ref_astnode(type) {
        astnode_type = kIdentifier_astnode;
        this->name = name;
    }
    int genLabel(int isRightChild){
        label = !isRightChild;
        isRightLeaf = isRightChild;
        return label;
    }
    void genValue(){
        if(resolveIdentifierType(name, globalST, currentST)->isArray()){genAddr(); return;}
        int offset = resolveIdentifierOffset(name, globalST, currentST);
        std::cout << "\tmovl\t" << offset << "(%ebp), %" << reg_no_to_name(avlbl_reg.back()) << "\n";
    }
    void genAddr(){
        int offset = resolveIdentifierOffset(name, globalST, currentST);
        std::cout << "\tleal\t" << offset << "(%ebp), %" << reg_no_to_name(avlbl_reg.back()) << "\n";
        if(resolveIdentifierType(name, globalST, currentST)->isArray() && isIdentifierParam(name, globalST, currentST)){
            std::cout << "\tmovl\t(%" << reg_no_to_name(avlbl_reg.back()) << "), %" << reg_no_to_name(avlbl_reg.back()) << "\n";
        }
    }
    void print(int blanks) {
        std::cout << "{\"identifier\": \"" << name << "\"}";
    }
    std::string getName() {
        return name;
    }
};

class Op_unary_astnode: public Exp_astnode {
  protected:
    // Follows a format, refer page 5 of assignment.pdf
    std::string resolved_op_type;
    Exp_astnode *exp;
  public:
    Op_unary_astnode(std::string resolved_op_type, Exp_astnode *exp, Type *type) 
    : Exp_astnode(type) {
        astnode_type = kOp_unary_astnode;
        this->resolved_op_type = resolved_op_type;
        this->exp = exp;
    }
    int genLabel(int isRightChild){
        label = exp->genLabel(0);
        return label;
    }
    void genValue(){
        if(resolved_op_type == "ADDRESS"){exp->genAddr(); return;}
        if(resolved_op_type == "DEREF"){
            exp->genValue(); 
            std::cout << "\tmovl\t(%" << reg_no_to_name(avlbl_reg.back()) << "), %" << reg_no_to_name(avlbl_reg.back()) << "\n" ;
            return;
        }
        if(resolved_op_type == "NOT"){
            exp->genValue();
            if(avlbl_reg.back() != 1){std::cout << "\tpushl\t%eax\n";}
            std::cout << "\tcmpl\t$0, %" << reg_no_to_name(avlbl_reg.back()) << "\n";
            std::cout << "\tsete\t%al\n\tmovzbl\t%al, %" << reg_no_to_name(avlbl_reg.back()) << "\n";
            if(avlbl_reg.back() != 1){std::cout << "\tpopl\t%eax\n";}
            return;
        }
        if(resolved_op_type == "UMINUS"){
            exp->genValue();
            std::cout << "\tnegl\t%" << reg_no_to_name(avlbl_reg.back()) << "\n";
            return;
        }
        if(resolved_op_type == "PP"){
            int inc = 1;
            if(exp->getType()->isImplicitPointer()){
                inc = exp->getType()->_dereference().sizeOf();
            }
            exp->genAddr();
            std::cout << "\tpushl\t(%" << reg_no_to_name(avlbl_reg.back()) << ")\n";
            
            if(inc==1){std::cout << "\tincl\t(%" << reg_no_to_name(avlbl_reg.back()) << ")\n";}
            else{std::cout << "\taddl\t$" << inc << ", (%" << reg_no_to_name(avlbl_reg.back()) << ")\n";}
            
            std::cout << "\tpopl\t%" << reg_no_to_name(avlbl_reg.back()) << "\n";
            return;
        }
    }
    void genAddr(){
        // only called for DEREF op_type
        exp->genValue();
    }
    void print(int blanks) {
        std::cout << "{\"op_unary\": {\"op\": \""<<resolved_op_type<<"\", \"child\": ";
        exp->print(blanks);
        std::cout<<"}}";
    }
    std::string get_op_type(){
      return resolved_op_type;
    }
};

/*
    Helper classes
*/
class ExpList {
  protected:
    std::vector<Exp_astnode*> list;
  public:
    ExpList(Exp_astnode *exp) {
        addExp(exp);
    }
    ExpList(std::vector<Exp_astnode*> list) {
        this->list = list;
    }
    void addExp(Exp_astnode *exp) {
        list.push_back(exp);
    }
    int getListSize() {
        return list.size();
    }
    Exp_astnode *getExpAt(int i) {
        if (i < 0)
            return NULL;
        if (i >= (int) list.size())
            return NULL;
        return list[i];
    }
    void castExpAt(int i, std::string cast_type) {
        if (i < 0)
            return ;
        if (i >= (int) list.size())
            return ;
        list[i] = new Op_unary_astnode("TO_"+cast_type, list[i], new Type((cast_type=="FLOAT" ? "float" : "int"), 4));
    }
    friend class Funcall_astnode;
    friend class Proccall_astnode;
};

/*
    Child classes of Statement_astnode
*/
class Empty_astnode: public Statement_astnode {
  protected:
    //
  public:
    Empty_astnode() {
        astnode_type = kEmpty_astnode;
    }
    void genCode(){}
    void print(int blanks) {
        std::cout << "\"empty\"";
    }
};

class Seq_astnode: public Statement_astnode {
  protected:
    std::vector<Statement_astnode*> sequence;
  public:
    Seq_astnode() {
        astnode_type = kSeq_astnode;
    }
    void add_statement(Statement_astnode* statement){
      sequence.push_back(statement);
    }
    void genCode(){
      for(auto statement : sequence){
        statement->genCode();
      }
    }
    void print(int blanks) {
        // Do something
        std::cout << "{\n\"seq\": [\n";
        std::string delimiter = "";
        for(auto statement : sequence){
          std::cout << delimiter;
          statement->print(blanks);
          delimiter = ",\n";
        }
        std::cout << "\n]}";
    }
};

class Return_astnode: public Statement_astnode {
  protected:
    Exp_astnode *r_exp;
  public:
    Return_astnode(Exp_astnode* r_exp) {
        astnode_type = kReturn_astnode;
        this->r_exp = r_exp;
    }
    void genCode(){
        r_exp->genLabel(0);
        if(r_exp->getType()->isTypeStruct()){
            // TODO if r_exp is a struct we assume caller left space in stack that we now fill
            r_exp->genAddr();
            return_struct(reg_no_to_name(avlbl_reg.back()), 0, 32+getCurrentParamStackSize(), r_exp->getType()->getTypeName());
        }else{
            r_exp->genValue();
            std::cout << "\tmovl\t%" << reg_no_to_name(avlbl_reg.back()) << ", %eax\n"; 
        }
        std::cout << "\tleave\n\tret\n" ;
    }
    void print(int blanks) {
        std::cout << "{\"return\" : ";
        r_exp->print(blanks);
        std::cout << "}";
    }
};

class Proccall_astnode: public Statement_astnode {
  protected:
    std::string name;
    std::vector<Exp_astnode*> p_exp;
  public:
    Proccall_astnode(std::string name) {
        astnode_type = kProccall_astnode;
        this->name = name;
    }
    Proccall_astnode(std::string name, ExpList *list) {
        astnode_type = kProccall_astnode;
        this->name = name;
        this->p_exp = list->list;
    }
    void genCode(){
        int w = functionReturnTypeWidth(name, globalST, currentST);
        if(name != "printf"){std::cout << "\tsubl\t$" << w << ", %esp\n";}
        int n = p_exp.size();
        int arguments_stack_size = 0;
        
        int begin = (name=="printf") ? n-1 : 0;
        int end = (name=="printf") ? -1 : n;
        int step = (name=="printf") ? -1 : 1;
        for(int i = begin; i!=end; i=i+step){
            if((p_exp[i])->astnode_type == kStringconst_astnode){
                std::cout << "\tpushl\t$.LC" << ro_consts.size() << "\n";
                ro_consts.push_back(((Stringconst_astnode*)(p_exp[i]))->getVal());
                arguments_stack_size += 4;
                continue; // or break;
            }
            (p_exp[i])->genLabel(0);
            if((p_exp[i])->getType()->isTypeStruct()){
                (p_exp[i])->genAddr();
                arguments_stack_size += pushl_struct(reg_no_to_name(avlbl_reg.back()), 0, (p_exp[i])->getType()->getBasicTypeName());
            }
            // else if((p_exp[i])->getType()->isArray()){}
            else{
                (p_exp[i])->genValue(); // if isArray() this will call genAddr() which is how arrays are passed
                std::cout << "\tpushl\t%" << reg_no_to_name(avlbl_reg.back()) << "\n";
                arguments_stack_size += 4;
            }
        }
        
        if(name != "printf"){
            // save reg
            std::cout << "\tpushl\t%eax\n";
            std::cout << "\tpushl\t%ebx\n";
            std::cout << "\tpushl\t%ecx\n";
            std::cout << "\tpushl\t%edx\n";
            std::cout << "\tpushl\t%esi\n";
            std::cout << "\tpushl\t%edi\n";
        }

        std::cout << "\tcall\t" << name << "\n";
        
        // put eax into return value if was int return type
        if((name != "printf") && functionReturnType(name, globalST, NULL)->getTypeName() == "int"){
            std::cout << "\tmovl\t%eax\t24(%ebp)\n";
        }
        
        if(name != "printf"){
            // restore reg
            std::cout << "\tpopl\t%edi\n";
            std::cout << "\tpopl\t%esi\n";
            std::cout << "\tpopl\t%edx\n";
            std::cout << "\tpopl\t%ecx\n";
            std::cout << "\tpopl\t%ebx\n";
            std::cout << "\tpopl\t%eax\n";
        }

        std::cout << "\taddl\t$" << arguments_stack_size << ", %esp\n";

        // access return value
        // --no action in proccall--
        if(name != "printf"){std::cout << "\taddl\t$" << w << ", %esp\n";}
    }
    void print(int blanks) {
        std::cout << "{\"proccall\": { \"fname\": { \"identifier\": \"" << name << "\"},\"params\": [";
        std::string delimiter = "";
        for(auto param : p_exp){
          std::cout<<delimiter;
          param->print(blanks);
          delimiter = ",";
        }
        std::cout<<"]}}";
    }
};

class If_astnode: public Statement_astnode {
  protected:
    Exp_astnode *exp;
    Statement_astnode *if_body, *else_body;
  public:
    If_astnode(Exp_astnode* exp, Statement_astnode* if_body, Statement_astnode* else_body) {
        astnode_type = kIf_astnode;
        this->exp = exp;
        this->if_body = if_body;
        this->else_body = else_body;
    }
    void genCode(){
        exp->genLabel(0);
        exp->genValue();
        std::cout << "\tcmpl\t$0, %" << reg_no_to_name(avlbl_reg.back()) << "\n";
        
        int l1 = label_count; ++label_count;
        std::cout << "\tje\t.L" << l1 << "\n";
        
        if_body->genCode();
        
        int l2 = label_count; ++label_count;
        std::cout << "\tjmp\t.L" << l2 << "\n";
        
        std::cout << ".L" << l1 << ":\n";
        else_body->genCode();
        std::cout << ".L" << l2 << ":\n";
        
    }
    void print(int blanks) {
        std::cout << "{\"if\": {\"cond\": ";
        exp->print(blanks);
        std::cout << ", \"then\": ";
        if_body->print(blanks);
        std::cout << ", \"else\": ";
        else_body->print(blanks);
        std::cout<<"}}";
    }
};

class While_astnode: public Statement_astnode {
  protected:
    Exp_astnode *exp;
    Statement_astnode *body;
  public:
    While_astnode(Exp_astnode* exp, Statement_astnode* body) {
        astnode_type = kWhile_astnode;
        this->exp = exp;
        this->body = body;
    }
    void genCode(){
        exp->genLabel(0);
        
        int l1 = label_count; ++label_count;
        std::cout << ".L" << l1 << ":\n";
        exp->genValue();
        std::cout << "\tcmpl\t$0, %" << reg_no_to_name(avlbl_reg.back()) << "\n";
        int l2 = label_count; ++label_count;
        std::cout << "\tje\t.L" << l2 << "\n";
        body->genCode();
        std::cout << "\tjmp\t.L" << l1 << "\n";
        std::cout << ".L" << l2 << ":\n";
    }
    void print(int blanks) {
        std::cout << "{\"while\": {\"cond\": ";
        exp->print(blanks);
        std::cout << ", \"stmt\": ";
        body->print(blanks);
        std::cout<<"}}";
    }
};

class For_astnode: public Statement_astnode {
  protected:
    Exp_astnode *init_exp, *guard_exp, *step_exp;
    Statement_astnode *body;
  public:
    For_astnode(Exp_astnode* init_exp, Exp_astnode* guard_exp, Exp_astnode* step_exp, Statement_astnode *body) {
        astnode_type = kFor_astnode;
        this->init_exp = init_exp;
        this->guard_exp = guard_exp;
        this->step_exp = step_exp;
        this->body = body;
    }
    void genCode(){
        init_exp->genLabel(0);
        guard_exp->genLabel(0);
        step_exp->genLabel(0);

        init_exp->genValue();
        
        int l1 = label_count; ++label_count;
        std::cout << ".L" << l1 << ":\n";
        guard_exp->genValue();
        std::cout << "\tcmpl\t$0, %" << reg_no_to_name(avlbl_reg.back()) << "\n";
        int l2 = label_count; ++label_count;
        std::cout << "\tje\t.L" << l2 << "\n";
        body->genCode();
        step_exp->genValue();
        std::cout << "\tjmp\t.L" << l1 << "\n";
        std::cout << ".L" << l2 << ":\n";
    }
    void print(int blanks) {
        std::cout << "{\"for\": {\"init\": ";
        init_exp->print(blanks);
        std::cout << ", \"guard\": ";
        guard_exp->print(blanks);
        std::cout << ", \"step\": ";
        step_exp->print(blanks);
        std::cout << ", \"body\": ";
        body->print(blanks);
        std::cout<<"}}";
    }
};

/*
    Child classes of Ref_astnode
*/


class Arrayref_astnode: public Ref_astnode {
  protected:
    Exp_astnode *arr_exp, *index_exp;
  public:
    Arrayref_astnode(Exp_astnode *arr_exp, Exp_astnode *index_exp, Type *type) 
    : Ref_astnode(type) {
        astnode_type = kArrayref_astnode;
        this->arr_exp = arr_exp;
        this->index_exp = index_exp;
    }
    int genLabel(int isRightChild){
        int l1 = arr_exp->genLabel(0);
        int l2 = index_exp->genLabel(1);
        if(l1>l2){label = l1;}else
        if(l1<l2){label = l2;}else{label = l1+1;}
        return label;
    }
    void genValue(){
        genAddr();
        if(!getType()->isArray()){
            std::cout << "\tmovl\t(%" << reg_no_to_name(avlbl_reg.back()) << "), %" << reg_no_to_name(avlbl_reg.back()) << "\n";
        }
    }
    void genAddr(){
        int multiplier = arr_exp->getType()->_dereference().sizeOf();
        if(index_exp->isRightLeaf){
            arr_exp->genValue();
            if(index_exp->astnode_type == kIdentifier_astnode){
                int offset = resolveIdentifierOffset(((Identifier_astnode*)index_exp)->getName(), globalST, currentST);

                std::cout << "\tpushl\t%" << reg_no_to_name(avlbl_reg.back()) << "\n";
                std::cout << "\timull\t$" << multiplier << ", " << offset << "(%ebp), %" << reg_no_to_name(avlbl_reg.back()) << "\n";
                std::cout << "\tpushl\t%" << reg_no_to_name(avlbl_reg.back()) << "\n";
                std::cout << "\tmovl\t4(%esp), %" << reg_no_to_name(avlbl_reg.back()) << "\n";
                std::cout << "\taddl\t(%esp), %" << reg_no_to_name(avlbl_reg.back()) << "\n";
                std::cout << "\taddl\t$4, %esp\n";
     
            }
            if(index_exp->astnode_type == kIntconst_astnode){
                std::cout << "\taddl\t$" << (((Intconst_astnode*)index_exp)->getVal())*multiplier << ", %" << reg_no_to_name(avlbl_reg.back()) << "\n";   
            }
            return;
        }
        if(arr_exp->label >= 6 && index_exp->label >=6){
            index_exp->genValue();
            std::cout << "\timull\t$" << multiplier << ", %" << reg_no_to_name(avlbl_reg.back()) << "\n";
            std::cout << "\tpushl\t%" << reg_no_to_name(avlbl_reg.back()) << "\n";
            arr_exp->genValue();
            std::cout << "\taddl\t(%esp), %" << reg_no_to_name(avlbl_reg.back()) << "\n";
            std::cout << "\taddl\t$4, (%esp)\n";
            return;
        }
        if(arr_exp->label < index_exp->label){
            swap_rstack();
            index_exp->genValue();
            std::cout << "\timull\t$" << multiplier << ", %" << reg_no_to_name(avlbl_reg.back()) << "\n";
            int R = avlbl_reg.back(); avlbl_reg.pop_back();
            arr_exp->genValue();
            std::cout << "\taddl\t%" << reg_no_to_name(R) << ", %" << reg_no_to_name(avlbl_reg.back()) << "\n";
            avlbl_reg.push_back(R);
            swap_rstack();
            return;
        }
        if(arr_exp->label >= index_exp->label){
            arr_exp->genValue();
            int R = avlbl_reg.back(); avlbl_reg.pop_back();
            index_exp->genValue();
            std::cout << "\timull\t$" << multiplier << ", %" << reg_no_to_name(avlbl_reg.back()) << "\n";
            std::cout << "\taddl\t%" << reg_no_to_name(avlbl_reg.back()) << ", %" << reg_no_to_name(R) << "\n";
            avlbl_reg.push_back(R);
            return;
        }
    }
    void print(int blanks) {
        std::cout << "{\"arrayref\": {\"array\": ";
        arr_exp->print(blanks);
        std::cout<<", \"index\": ";
        index_exp->print(blanks);
        std::cout << "}}";
    }
};

class Member_astnode: public Ref_astnode {
  protected:
    Exp_astnode *exp;
    Identifier_astnode *field;
    // Type of the resulting expression can be inferred from type of field attribute
  public:
    Member_astnode(Exp_astnode *exp, Identifier_astnode *field, Type *type)
    : Ref_astnode(type) {
        astnode_type = kMember_astnode;
        this->exp = exp;
        this->field = field;
    }
    int genLabel(int isRightChild){
        label = exp->genLabel(0);
        return label;
    }
    void genValue(){
        genAddr();
        if(!field->getType()->isArray()){
            std::cout << "\tmovl\t(%" << reg_no_to_name(avlbl_reg.back()) << "), %" << reg_no_to_name(avlbl_reg.back()) << "\n";
        }
    }
    void genAddr(){
        exp->genAddr();
        int offset = structMemberOffset(exp->getType()->getTypeName(), globalST, field->getName());
        std::cout << "\taddl\t$" << offset << ", %" << reg_no_to_name(avlbl_reg.back()) << "\n";
    }
    void print(int blanks) {
        std::cout << "{\"member\": {\"struct\": ";
        exp->print(blanks);
        std::cout << ", \"field\": ";
        field->print(blanks);
        std::cout<<"}}";
    }
};

class Arrow_astnode: public Ref_astnode {
  protected:
    Exp_astnode *ptr_exp;
    Identifier_astnode *field;
    // Type of the resulting expression can be inferred from type of field attribute
  public:
    Arrow_astnode(Exp_astnode *ptr_exp, Identifier_astnode *field, Type *type) 
    : Ref_astnode(type) {
        astnode_type = kArrow_astnode;
        this->ptr_exp = ptr_exp;
        this->field = field;
    }
    int genLabel(int isRightChild){
        label = ptr_exp->genLabel(0);
        return label;
    }
    void genValue(){
        genAddr();
        if(!field->getType()->isArray()){
            std::cout << "\tmovl\t(%" << reg_no_to_name(avlbl_reg.back()) << "), %" << reg_no_to_name(avlbl_reg.back()) << "\n";
        }
    }
    void genAddr(){
        ptr_exp->genValue();
        int offset = structMemberOffset(ptr_exp->getType()->_dereference().getTypeName(), globalST, field->getName());
        std::cout << "\taddl\t$" << offset << ", %" << reg_no_to_name(avlbl_reg.back()) << "\n";
    }
    void print(int blanks) {
        std::cout << "{\"arrow\": {\"pointer\": ";
        ptr_exp->print(blanks);
        std::cout << ", \"field\": ";
        field->print(blanks);
        std::cout<<"}}";
    }
};

/*
    Child classes of Exp_astnode
*/
class Op_binary_astnode: public Exp_astnode {
  protected:
    // Follows a format, refer page 5 of assignment.pdf
    std::string resolved_op_type;
    Exp_astnode *lhs, *rhs;
  public:
    Op_binary_astnode(std::string op_type, Exp_astnode* lhs, Exp_astnode* rhs, Type *type) : Exp_astnode(type) {
        astnode_type = kOp_binary_astnode;
        this->resolved_op_type = op_type;
        this->lhs = lhs;
        this->rhs = rhs;
    }
    int genLabel(int isRightChild){
        int l1 = lhs->genLabel(0);
        int l2 = rhs->genLabel(1);
        if(l1>l2){label = l1;}else
        if(l1<l2){label = l2;}else{label = l1+1;}
        return label;
    }
    void genValue(){
        int multiplier = 1;
        int flag_l = 0;
        int flag_r = 0;
        if(lhs->getType()->isImplicitPointer() && rhs->getType()->getTypeName()=="int"){
            flag_r = 1;
            multiplier = lhs->getType()->_dereference().sizeOf();
        }
        if(rhs->getType()->isImplicitPointer() && lhs->getType()->getTypeName()=="int"){
            flag_l = 1;
            multiplier = rhs->getType()->_dereference().sizeOf();
        }

        if(rhs->isRightLeaf){
            lhs->genValue();
            if(rhs->astnode_type == kIdentifier_astnode){
                int offset = resolveIdentifierOffset(((Identifier_astnode*)rhs)->getName(), globalST, currentST);
                
                if(flag_l){
                    genASM("%"+reg_no_to_name(avlbl_reg.back()), "$" + std::to_string(multiplier), "MULT_INT");
                    genASM("%"+reg_no_to_name(avlbl_reg.back()), std::to_string(offset)+"(%ebp)", resolved_op_type);
                }else
                if(flag_r){
                    std::cout << "\tpushl\t%" << reg_no_to_name(avlbl_reg.back()) << "\n";
                    std::cout << "\tmovl\t" << offset << "(%ebp), %" << reg_no_to_name(avlbl_reg.back()) << "\n";
                    
                    if(resolveIdentifierType(((Identifier_astnode*)rhs)->getName(), globalST, currentST)->isArray() && isIdentifierParam(((Identifier_astnode*)rhs)->getName(), globalST, currentST)){
                        std::cout << "\tmovl\t(%" << reg_no_to_name(avlbl_reg.back()) << "), %" << reg_no_to_name(avlbl_reg.back()) << "\n";
                    }
                    
                    std::cout << "\timull\t$" << multiplier << ", %" << reg_no_to_name(avlbl_reg.back()) << "\n";
                    std::cout << "\tpushl\t%" << reg_no_to_name(avlbl_reg.back()) << "\n";
                    std::cout << "\tmovl\t4(%esp), %" << reg_no_to_name(avlbl_reg.back()) << "\n";
                    genASM("%"+reg_no_to_name(avlbl_reg.back()), "(%esp)", resolved_op_type);
                    std::cout << "\taddl\t$4, %esp\n";
                }else{
                    genASM("%"+reg_no_to_name(avlbl_reg.back()), std::to_string(offset)+"(%ebp)", resolved_op_type);
                }                
            }
            if(rhs->astnode_type == kIntconst_astnode){
                int num = ((Intconst_astnode*)rhs)->getVal();              
                if(flag_r){num *= multiplier;}
                std::string operand2 = "$"+std::to_string(num);
                genASM("%"+reg_no_to_name(avlbl_reg.back()), operand2, resolved_op_type);
            }
            return;
        }
        if(lhs->label >= 6 && rhs->label >=6){
            rhs->genValue();
            std::cout << "\tpushl\t%" << reg_no_to_name(avlbl_reg.back()) << "\n";
            lhs->genValue();
            
            if(flag_l){genASM("%"+reg_no_to_name(avlbl_reg.back()), "$" + std::to_string(multiplier), "MULT_INT");}
            if(flag_r){genASM("(%esp)", "$" + std::to_string(multiplier), "MULT_INT");}

            genASM("%"+reg_no_to_name(avlbl_reg.back()), "(%esp)", resolved_op_type);
            std::cout << "\taddl\t$4, %esp\n";
            return;
        }
        if(lhs->label < rhs->label){
            swap_rstack();
            rhs->genValue();
            int R = avlbl_reg.back(); avlbl_reg.pop_back();
            lhs->genValue();
            
            if(flag_l){genASM("%"+reg_no_to_name(avlbl_reg.back()), "$" + std::to_string(multiplier), "MULT_INT");}
            if(flag_r){genASM("%"+reg_no_to_name(R), "$" + std::to_string(multiplier), "MULT_INT");}

            genASM("%"+reg_no_to_name(avlbl_reg.back()), "%"+reg_no_to_name(R), resolved_op_type);
            avlbl_reg.push_back(R);
            swap_rstack();
            return; 
        }
        if(lhs->label >= rhs->label){
            lhs->genValue();
            int R = avlbl_reg.back(); avlbl_reg.pop_back();
            rhs->genValue();
                        
            if(flag_l){genASM("%"+reg_no_to_name(R), "$" + std::to_string(multiplier), "MULT_INT");}
            if(flag_r){genASM("%"+reg_no_to_name(avlbl_reg.back()), "$" + std::to_string(multiplier), "MULT_INT");}

            genASM("%"+reg_no_to_name(R), "%"+reg_no_to_name(avlbl_reg.back()), resolved_op_type);
            avlbl_reg.push_back(R);
            return; 
        }
    }
    void genAddr(){}
    void print(int blanks) {
        std::cout << "{\"op_binary\": {\"op\": \""<<resolved_op_type<<"\", \"left\": ";
        lhs->print(blanks);
        std::cout << ", \"right\": ";
        rhs->print(blanks);
        std::cout<<"}}";
    }
};


class AssignE_astnode: public Exp_astnode {
  protected:
    Exp_astnode *lhs, *rhs;
  public:
    AssignE_astnode(Exp_astnode*lhs, Exp_astnode* rhs, Type *type) : Exp_astnode(type) {
        astnode_type = kAssignE_astnode;
        this->lhs = lhs;
        this->rhs = rhs;
    }
    int genLabel(int isRightChild){
        int l1 = lhs->genLabel(0);
        int l2 = rhs->genLabel(1);
        if(l1>l2){label = l1;}else
        if(l1<l2){label = l2;}else{label = l1+1;}
        return label;
    }
    void genValue(){
        lhs->genLabel(0);
        rhs->genLabel(!rhs->getType()->isTypeStruct());
        if(rhs->getType()->isTypeStruct()){
            rhs->genAddr();
            pushl_struct(reg_no_to_name(avlbl_reg.back()), 0, rhs->getType()->getBasicTypeName());
            lhs->genAddr();
            popl_struct(reg_no_to_name(avlbl_reg.back()), 0, rhs->getType()->getBasicTypeName());
            return;
        }
        if(rhs->isRightLeaf){
            lhs->genAddr();
            if(rhs->astnode_type == kIdentifier_astnode){
                int offset = resolveIdentifierOffset(((Identifier_astnode*)rhs)->getName(), globalST, currentST);
                
                // obv better to use a register as intermediate instead of the stack. but can't be bothered to do all the cases
                std::cout << "\tpushl\t" << offset << "(%ebp)\n";
                std::cout << "\tpopl\t(%" << reg_no_to_name(avlbl_reg.back()) << ")\n";
                // std::cout << "\tmovl\t" << offset << "(%ebp), (%" << reg_no_to_name(avlbl_reg.back()) << ")\n";   
            }
            if(rhs->astnode_type == kIntconst_astnode){
                std::cout << "\tmovl\t$" << ((Intconst_astnode*)rhs)->getVal() << ", (%" << reg_no_to_name(avlbl_reg.back()) << ")\n";   
            }
            return;
        }
        if(lhs->label >= 6 && rhs->label >=6){
            rhs->genValue();
            std::cout << "\tpushl\t%" << reg_no_to_name(avlbl_reg.back()) << "\n";
            lhs->genAddr();
            std::cout << "\tpopl\t(%" << reg_no_to_name(avlbl_reg.back()) << ")\n";
            return;
        }
        if(lhs->label < rhs->label){
            swap_rstack();
            rhs->genValue();
            int R = avlbl_reg.back(); avlbl_reg.pop_back();
            lhs->genAddr();
            std::cout << "\tmovl\t%" << reg_no_to_name(R) << ", (%" << reg_no_to_name(avlbl_reg.back()) << ")\n";   
            avlbl_reg.push_back(R);
            swap_rstack();
            return; // top of stack now contains address of lhs-lvalue. ideally we would want it to be value of rhs (swap at end) but it doesn't matter for our smaller grammer
        }
        if(lhs->label >= rhs->label){
            lhs->genAddr();
            int R = avlbl_reg.back(); avlbl_reg.pop_back();
            rhs->genValue();
            std::cout << "\tmovl\t%" << reg_no_to_name(avlbl_reg.back()) << ", (%" << reg_no_to_name(R) << ")\n";   
            avlbl_reg.push_back(R);
            return; // top of stack now contains address of lhs-lvalue. ideally we would want it to be value of rhs (swap at end) but it doesn't matter for our smaller grammer
        }
    }
    void genAddr(){}
    void print(int blanks) {
        std::cout << "{\"assignE\": {\"left\": ";
        lhs->print(blanks);
        std::cout << ", \"right\": ";
        rhs->print(blanks);
        std::cout<<"}}";
    }
    friend class AssignS_astnode;
};

class AssignS_astnode: public Statement_astnode {
  protected:
    Exp_astnode *lhs, *rhs;
  public:
    AssignS_astnode(AssignE_astnode* assign_exp) {
        astnode_type = kAssignS_astnode;
        this->lhs = assign_exp->lhs;
        this->rhs = assign_exp->rhs;
        //delete assign_exp;
    }
    void genCode(){
        lhs->genLabel(0);
        rhs->genLabel(!rhs->getType()->isTypeStruct());
        if(rhs->getType()->isTypeStruct()){
            rhs->genAddr();
            pushl_struct(reg_no_to_name(avlbl_reg.back()), 0, rhs->getType()->getBasicTypeName());
            lhs->genAddr();
            popl_struct(reg_no_to_name(avlbl_reg.back()), 0, rhs->getType()->getBasicTypeName());
            return;
        }
        if(rhs->isRightLeaf){
            lhs->genAddr();
            if(rhs->astnode_type == kIdentifier_astnode){
                int offset = resolveIdentifierOffset(((Identifier_astnode*)rhs)->getName(), globalST, currentST);
                
                // obv better to use a register as intermediate instead of the stack. but can't be bothered to do all the cases
                std::cout << "\tpushl\t" << offset << "(%ebp)\n";
                std::cout << "\tpopl\t(%" << reg_no_to_name(avlbl_reg.back()) << ")\n";
                // std::cout << "\tmovl\t" << offset << "(%ebp), (%" << reg_no_to_name(avlbl_reg.back()) << ")\n";   
            }
            if(rhs->astnode_type == kIntconst_astnode){
                std::cout << "\tmovl\t$" << ((Intconst_astnode*)rhs)->getVal() << ", (%" << reg_no_to_name(avlbl_reg.back()) << ")\n";   
            }
            return;
        }
        if(lhs->label >= 6 && rhs->label >=6){
            rhs->genValue();
            std::cout << "\tpushl\t%" << reg_no_to_name(avlbl_reg.back()) << "\n";
            lhs->genAddr();
            std::cout << "\tpopl\t(%" << reg_no_to_name(avlbl_reg.back()) << ")\n";
            return;
        }
        if(lhs->label < rhs->label){
            swap_rstack();
            rhs->genValue();
            int R = avlbl_reg.back(); avlbl_reg.pop_back();
            lhs->genAddr();
            std::cout << "\tmovl\t%" << reg_no_to_name(R) << ", (%" << reg_no_to_name(avlbl_reg.back()) << ")\n";   
            avlbl_reg.push_back(R);
            swap_rstack();
            return; // top of stack now contains address of lhs-lvalue. ideally we would want it to be value of rhs (swap at end) but it doesn't matter for our smaller grammer
        }
        if(lhs->label >= rhs->label){
            lhs->genAddr();
            int R = avlbl_reg.back(); avlbl_reg.pop_back();
            rhs->genValue();
            std::cout << "\tmovl\t%" << reg_no_to_name(avlbl_reg.back()) << ", (%" << reg_no_to_name(R) << ")\n";   
            avlbl_reg.push_back(R);
            return; // top of stack now contains address of lhs-lvalue. ideally we would want it to be value of rhs (swap at end) but it doesn't matter for our smaller grammer
        }
    }
    void print(int blanks) {
        std::cout << "{\"assignS\": {\"left\": ";
        lhs->print(blanks);
        std::cout << ", \"right\": ";
        rhs->print(blanks);
        std::cout<<"}}";
    }
};

class Funcall_astnode: public Exp_astnode {
  protected:
    std::vector<Exp_astnode*> p_exp;
    std::string name;
  public:
    Funcall_astnode(std::string name, ExpList *list, Type* type) : Exp_astnode(type) {
        astnode_type = kFuncall_astnode;
        this->name = name;
        if (list)
            p_exp = list->list;
    }
    int genLabel(int isRightChild){
        label = 1; // need atleast 1 label, for final output
        int tmp;
        for(auto exp: p_exp){
            tmp = exp->genLabel(0);
            if(tmp > label){label = tmp;}
        }
        return label;
    }
    void genValue(){
        int w = functionReturnTypeWidth(name, globalST, currentST);
        std::cout << "\tsubl\t$" << w << ", %esp\n";
        int n = p_exp.size();
        int arguments_stack_size = 0;
        for(int i = 0; i<n; ++i){
            if((p_exp[i])->astnode_type == kStringconst_astnode){
                std::cout << "\tpushl\t$.LC" << ro_consts.size() << "\n";
                ro_consts.push_back(((Stringconst_astnode*)(p_exp[i]))->getVal());
                arguments_stack_size += 4;
                continue; // or break;
            }
            (p_exp[i])->genLabel(0);
            if((p_exp[i])->getType()->isTypeStruct()){
                (p_exp[i])->genAddr();
                arguments_stack_size += pushl_struct(reg_no_to_name(avlbl_reg.back()), 0, (p_exp[i])->getType()->getBasicTypeName());
            }
            // else if((p_exp[i])->getType()->isArray()){}
            else{
                (p_exp[i])->genValue();
                std::cout << "\tpushl\t%" << reg_no_to_name(avlbl_reg.back()) << "\n";
                arguments_stack_size += 4;
            }
        }
        // save reg
        std::cout << "\tpushl\t%eax\n";
        std::cout << "\tpushl\t%ebx\n";
        std::cout << "\tpushl\t%ecx\n";
        std::cout << "\tpushl\t%edx\n";
        std::cout << "\tpushl\t%esi\n";
        std::cout << "\tpushl\t%edi\n";

        std::cout << "\tcall\t" << name << "\n";
        
        // put eax into return value if was int return type
        if(functionReturnType(name, globalST, NULL)->getTypeName() == "int"){
            std::cout << "\tmovl\t%eax, " << (24+arguments_stack_size) << "(%esp)\n";
        }
        
        // restore reg
        std::cout << "\tpopl\t%edi\n";
        std::cout << "\tpopl\t%esi\n";
        std::cout << "\tpopl\t%edx\n";
        std::cout << "\tpopl\t%ecx\n";
        std::cout << "\tpopl\t%ebx\n";
        std::cout << "\tpopl\t%eax\n";

        std::cout << "\taddl\t$" << arguments_stack_size << ", %esp\n";

        // access return value
        if(functionReturnType(name, globalST, NULL)->getTypeName() == "int"){
            std::cout << "\tpopl\t%" << reg_no_to_name(avlbl_reg.back()) << "\n";
        }else{
            // return type can't be void, it is struct
            // only place funcall with return type struct can be found is 
            //          rhs of assignment
            //          passed as a parameter
            //          lhs of member_astnode
            // since it is an rvalue, we trust that the parent ast_node will add 'w' to %esp after reading the value
            // and so we keep in the top register the address of our return value
            
            std::cout << "\tmovl\t%esp, %" << reg_no_to_name(avlbl_reg.back()) << "\n";
            // std::cout << "\taddl\t$" << w << ", %esp\n"; // the parent ast_node must call this
        }
    }
    void genAddr(){genValue();return;} // never supposed to be called except as a hack when a struct is returned by function
    void print(int blanks) {
        std::cout << "{\"funcall\": { \"fname\": { \"identifier\": \"" << name << "\"},\"params\": [";
        std::string delimiter = "";
        for(auto param : p_exp){
          std::cout<<delimiter;
          param->print(blanks);
          delimiter = ",";
        }
        std::cout<<"]}}";
    }
};



class Floatconst_astnode: public Exp_astnode {
  protected:
    float val;
  public:
    Floatconst_astnode(float val) : Exp_astnode(new Type("float", 4)) {
        astnode_type = kFloatconst_astnode;
        this->val = val;
    }
    int genLabel(int isRightChild){
        label = !isRightChild;
        isRightLeaf = isRightChild;
        return label;
    }
    void genValue(){}
    void genAddr(){}
    void print(int blanks) {
        std::cout << "{\"floatconst\": " << val << "}";
    }
};


#endif