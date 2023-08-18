/*
    Following Google's c++ style guide
*/
#include "type.hh"
#include "symbtab.hh"

#ifndef DECL_H
#define DECL_H

#define LEAST_PARAM_OFFSET 12

class Fun_decln {
  private:
    std::string name;
  public:
    void modifyOrAddName(std::string name) {
        this->name = name;
    }
    std::string getName() {
        return name;
    }
};

class Declarator: public Type {
  private:
    std::string decl_name;
  public:
    Declarator(Type *type): Type(type) {};
    void addDeclName(std::string decl_name) {
        this->decl_name = decl_name;
    }
    std::string getDeclName() {
        return decl_name;
    }
};

class Param_list {
  private:
    std::vector<Declarator*> list;
    int size;
  public:
    Param_list() {
        size = LEAST_PARAM_OFFSET;
    }
    void addParam(Declarator* param) {
        list.push_back(param);
        size += param->getSize();
    }
    // Modifies the offsets of all parameters added to an object of this class 
    // Return value is an error message if failed else an empty string
    std::string modifyOffsets(SymbolTable* table) {
        for (auto e: list) {
            size -= e->getSize();
            if (!table->setOffset(e->getDeclName(), size))
                return ("Failed to modify offset for parameter " + e->getDeclName());
        }
        // for (auto e: list)
        //     delete e;
        // list.clear();
        // std::cout << "-----------------------------------------------------------------------\n";
        // std::cout << "Modification done on ST: " << table << "\n";
        // std::cout << "-----------------------------------------------------------------------\n";
        return "";
    }
};

#endif