/*
    Following Google's c++ style guide
*/
#include "type.hh"
#include "ast.hh"

#ifndef ROOT_SYMBTAB_H
#define ROOT_SYMBTAB_H

class st_entry;
class SymbolTable;
st_entry *resolveIdentifier(std::string name, SymbolTable *globalST, SymbolTable *localST);

enum EntryType {kFunction = 0, kStruct = 1, kVariable = 2};
enum Scope {kGlobal, kLocal, kParameter};

class st_entry {
  private:
    std::string name;
    EntryType entry_type;
    Scope scope;
    int width;
    int offset;
    Type *type;
    SymbolTable *local_symbol_table; // structs
    Statement_astnode* seq_node = NULL; // functions
  public:
    st_entry (std::string name, EntryType entry_type, Scope scope, int width, int offset, Type *type, SymbolTable *local_symbol_table) {
        this->name = name;
        this->entry_type = entry_type;
        this->scope = scope;
        this->width = width;
        this->offset = offset;
        this->type = type;
        this->local_symbol_table = local_symbol_table;
    }
    int getWidth() {
        return width;
    }
    void setOffset(int offset) {
        this->offset = offset;
    }
    SymbolTable *getLocalSymbolTable() {
        return local_symbol_table;
    }
    std::string getName() {
        return name;
    }
    // For debugging purpose
    void printEntry() {
        std::string entry_type_str;
        if(entry_type == kFunction){entry_type_str = "fun";}
        if(entry_type == kStruct){entry_type_str = "struct";}
        if(entry_type == kVariable){entry_type_str = "var";}

        std::string scope_str;
        if(scope == kGlobal){scope_str = "global";}
        if(scope == kLocal){scope_str = "local";}
        if(scope == kParameter){scope_str = "param";}

        std::cout << "[\"" << name << "\", \"" << entry_type_str << "\", \"" << scope_str << "\", " << width << ", ";
        if(entry_type == kStruct){
            std::cout << "\"-\"";
        }else{
            std::cout << offset;
        }
        std::cout << ", \"" << (type? type->getTypeName() : "-") << "\"]";
    }
    Type *getType() {
        return type;
    }
    int isTypeStruct() {
        if (entry_type == kStruct)
            return 1;
        return 0;
    }
    void set_ast(Statement_astnode* seq_node){
        this->seq_node = seq_node;
    }
    Statement_astnode* get_ast(){
        return seq_node;
    }
    Scope getScope() {
        return scope;
    }
    int getOffset() {
        return offset;
    }
};

class SymbolTable {
  private:
    // Function overloading is not part of the assignment so we can use name of the entry as a key
    // Exs for name of the entry: `int f1(int a) {...}`->`f1`, `struct r{...};`->`struct r`, `int x;`->`x` etc.
    std::map<std::string, st_entry*> table;
    static bool getParamsComp(const std::pair<Type*, int> &a, const std::pair<Type*, int> &b) {
        // Want descending sort
        return a.second >= b.second;
    }
  public:
    // Searches if an entry is present in the table
    // Return value is 1 if already present else 0
    int findEntry(std::string key) {
        if (table.find(key) == table.end())
          return 0;
        else
          return 1;
    }
    // Returns a pointer(NULL) to the entry matching(If no match) the parameters
    st_entry *getEntry(std::string key) {
        if (!findEntry(key))
            return NULL;
        return table[key];
    }
    // Inserts a new entry in the table unless it's a duplicate
    // Return value is 1 if successful else 0
    int addEntry(std::string name, EntryType entry_type, Scope scope, int width, int offset, Type *type, SymbolTable *local_symbol_table) {
        if (findEntry(name))
            return 0;
        st_entry *entry = new st_entry(name, entry_type, scope, width, offset, type, local_symbol_table);
        table.insert({name, entry});
        return 1;
    }
    // Returns the size attribute of the entry corresponding to the parameters
    // Return value is 0 if the entry doesn't exist or is of kFunction enumeral
    int getWidth(std::string name) {
        st_entry *entry = getEntry(name);
        if (entry)
            return entry->getWidth();
        else
            return 0;
    }
    // Sets the offset of the entry corresponding to the param
    // Useful for modifying parameter offsets after inserting them
    // Return value is 1 if successful and 0 if such an entry doesn't exist
    int setOffset(std::string name, int offset) {
        st_entry *entry = getEntry(name);
        if (entry)
            entry->setOffset(offset);
        else
            return 0;
        return 1;
    }
    // For debugging purpose
    void printTable(int isGlobal) {
        if(isGlobal){std::cout<<"{\n\"globalST\":[\n";}
        std::string delimiter = "";
        for (auto it: table) {
            std::cout << delimiter;
            std::cout << "\t";
            it.second->printEntry();
            delimiter = ",\n";
        }
        std::cout<<"\n";
        if(!isGlobal){return;}
        std::cout<<"],";

        std::vector<st_entry*> structs;
        std::vector<st_entry*> funcs;
        for (auto it: table) {
            if(it.second->isTypeStruct()){
                structs.push_back(it.second);
            }else{
                funcs.push_back(it.second);
            }
        }

        std::cout<<"\n\"structs\": [\n";
        delimiter = "";
        for(auto entry : structs){
            std::cout << delimiter;
            std::cout<<"{\"name\": \"" << entry->getName() << "\", \"localST\": [\n";
            auto local_symbol_table = entry->getLocalSymbolTable();
            local_symbol_table->printTable(0);
            std::cout << "]}";
            delimiter = ",\n";
        }
        std::cout<<"\n],";
        std::cout<<"\n\"functions\": [\n";
        delimiter = "";
        for(auto entry : funcs){
            std::cout << delimiter;
            std::cout<<"{\"name\": \"" << entry->getName() << "\", \"localST\": [\n";
            auto local_symbol_table = entry->getLocalSymbolTable();
            local_symbol_table->printTable(0);
            std::cout << "], \"ast\": ";
            entry->get_ast()->print(0);
            std::cout<<"}";
            delimiter = ",\n";
        }
        std::cout<<"\n]}";
    }
    // Returns Types of parameters in sorted order for a function
    std::vector<Type*> getParams() {
        std::vector<std::pair<Type*, int>> parameters;
        st_entry *entry;
        for (auto it: table) {
            entry = it.second;
            if (entry->getScope() == kParameter)
                parameters.push_back({new Type(entry->getType()), entry->getOffset()});
        }
        std::sort(parameters.begin(), parameters.end(), getParamsComp);
        std::vector<Type*> result;
        for (auto it: parameters)
            result.push_back(it.first);
        return result;
    }
};
#endif