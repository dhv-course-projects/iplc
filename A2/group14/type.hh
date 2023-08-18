/*
    Following Google's c++ style guide
*/
#include <string>
#include <vector>
#include <map>
#include <iostream>
#include <deque>
#include <utility>
#include <algorithm>

#ifndef TYPE_H
#define TYPE_H

class Type {
  private:
    std::string basic_type_name;
    // Field useful for parameter processing
    int ptr_cnt;
    std::deque<int> arr_sizes;
    int size;
  public:
    int ptr_to_arr = 0;
    Type (std::string basic_type_name, int size) {
        this->basic_type_name = basic_type_name;
        this->size = size;
        ptr_cnt = 0;
    }
    Type (Type *copy) {
        this->basic_type_name = copy->basic_type_name;
        this->size = copy->size;
        this->ptr_cnt = copy->ptr_cnt;
        this->arr_sizes = copy->arr_sizes;
        this->ptr_to_arr = copy->ptr_to_arr;
    }
    int getSize() {
        return size;
    }
    void setSize(int size) {
        this->size = size;
    }
    std::string getTypeName() {
        std::string type_name = basic_type_name;
        for (int i = 0; i < ptr_cnt; i++)
            type_name += "*";
        if(ptr_to_arr){
            type_name += "(";
            for (int i = 0; i < ptr_to_arr; i++) type_name += "*";
            type_name += ")";
        }
        for (int e: arr_sizes)
            type_name += ("[" + std::to_string(e) + "]");
        return type_name;
    }
    // Changes typename and size appropriately
    void addPointer() {
        size = 4; // Necessary for void and struct types
        ptr_cnt++;
    }
    void addArray(int size) {
        this->size *= size;
        arr_sizes.push_back(size);
    }
    // Type is modified for an array indexing operation
    // Returns 1 if successful else 0(If no further indexing can be performed)
    // int indexInto() {
    //     if (arr_sizes.size())
    //         arr_sizes.pop_front();
    //     else
    //         return 0;
    //     return 1;
    // }
    Type* indexInto() {
        if (arr_sizes.size()){
            Type* new_type = new Type(this);
            new_type->arr_sizes.pop_front();
            return new_type;
        }else
        if(ptr_cnt && getTypeName()!="void*"){
            Type* new_type = new Type(this);
            --(new_type->ptr_cnt);
            return new_type;
        }
        else{
            return NULL;
        }
    }

    // returns 1 if can be implicitly converted to a pointer
    int isImplicitPointer(){
        return ptr_cnt || arr_sizes.size();
    }
    // This function must only be called when the above function returns True
    Type* implicitCastPtr(){
        Type* ptr = new Type(this);
        if(arr_sizes.size() == 0 || ptr_to_arr){return ptr;}
        if(arr_sizes.size() == 1){
            ptr->arr_sizes.clear(); 
            ptr->ptr_cnt += 1; 
            ptr->size = 4; 
            return ptr;
        }else{
            ptr->arr_sizes.pop_front();
            ptr->ptr_to_arr += 1;
            ptr->size = 4;
            return ptr;
        }
    }
    Type* addressOf(){
        Type* result = new Type(this);
        if(arr_sizes.size() == 0){result->addPointer(); return result;}
        else{result->ptr_to_arr += 1; result->size = 4; return result;}
    }

    Type* dereference(){        
        if(arr_sizes.size() == 0 && ptr_cnt){
            Type* result = new Type(this);
            --(result->ptr_cnt);
            if(result->ptr_cnt == 0){
                //result->size = ;//resize
            }
            return result;
        }else if(ptr_to_arr){
            Type* result = new Type(this);
            --(result->ptr_to_arr);
            //result->size = ;//resize
            return result;
        }else{
            if(!isImplicitPointer()){return NULL;}
            Type* cast = implicitCastPtr();
            Type* result = cast->dereference();
            delete cast;
            return result;
        }
    }
    int isArray(){
        return arr_sizes.size();
    }

    std::string getBasicTypeName() {
        return basic_type_name;
    }

    // Returns 1 if type is of struct else 0
    int isTypeStruct() {
        if (!arr_sizes.size() && !ptr_cnt && basic_type_name.length() >= 6){
            return (basic_type_name.substr(0,6) == "struct");
        }
        return 0;
    }
};

enum TypeExp {
    kAbstract_astnode, 
        kStatement_astnode,
            kEmpty_astnode, kSeq_astnode, kAssignS_astnode, kReturn_astnode, kIf_astnode, kWhile_astnode, kFor_astnode, kProccall_astnode, 
        kExp_astnode, 
            kRef_astnode,
                kIdentifier_astnode, kArrayref_astnode, kMember_astnode, kArrow_astnode,
            kOp_binary_astnode, kOp_unary_astnode, kAssignE_astnode, kFuncall_astnode, kIntconst_astnode, kFloatconst_astnode, kStringconst_astnode
};

#endif