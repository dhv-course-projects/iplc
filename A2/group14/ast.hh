/*
    Following Google's c++ style guide
*/
#include "type.hh"


#ifndef AST_H
#define AST_H

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
    // 
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
};

class Ref_astnode: public Exp_astnode {
  protected:
    // 
  public:
    Ref_astnode(Type *type) : Exp_astnode(type) {}
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
class Identifier_astnode: public Ref_astnode {
  protected:
    std::string name;
  public:
    Identifier_astnode(std::string name, Type *type) : Ref_astnode(type) {
        astnode_type = kIdentifier_astnode;
        this->name = name;
    }
    void print(int blanks) {
        std::cout << "{\"identifier\": \"" << name << "\"}";
    }
    std::string getName() {
        return name;
    }
};

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

class Intconst_astnode: public Exp_astnode {
  protected:
    int val;
  public:
    Intconst_astnode(int val) : Exp_astnode(new Type("int", 4)) {
        astnode_type = kIntconst_astnode;
        this->val = val;
    }
    void print(int blanks) {
        std::cout << "{\"intconst\": " << val << "}";
    }
    int getVal(){
        return val;
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
    void print(int blanks) {
        std::cout << "{\"floatconst\": " << val << "}";
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
    void print(int blanks) {
        std::cout << "{\"stringconst\": " << val << "}";
    }
};

#endif