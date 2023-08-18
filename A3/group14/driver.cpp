#include "scanner.hh"
#include "parser.tab.hh"
#include <fstream>
using namespace std;

extern SymbolTable *globalST;
string filename;
// extern std::map<string,abstract_astnode*> ast;
// std::map<std::string, datatype> predefined {
//             {"printf", createtype(VOID_TYPE)},
//             {"scanf", createtype(VOID_TYPE)},
//             {"mod", createtype(INT_TYPE)}
//         };
int main(int argc, char **argv)
{
	using namespace std;
	fstream in_file, out_file;
	in_file.open(argv[1], ios::in);
	IPL::Scanner scanner(in_file);
	IPL::Parser parser(scanner);
#ifdef YYDEBUG
	parser.set_debug_level(1);
#endif
	parser.parse();

	// globalST->printTable(1);
	globalST->genCode();
}

