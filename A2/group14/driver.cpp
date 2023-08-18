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

/*
	Debugging purpose code
*/
	globalST->printTable(1);

// create gstfun with function entries only

// for (const auto &entry : gst.Entries)
// {
// 	if (entry.second.varfun == "fun")
// 	gstfun.Entries.insert({entry.first, entry.second});
// }
// // create gststruct with struct entries only

// for (const auto &entry : gst.Entries)
// {
// 	if (entry.second.varfun == "struct")
// 	gststruct.Entries.insert({entry.first, entry.second});
// }
// // start the JSON printing

// cout << "{\"globalST\": " << endl;
// gst.printgst();
// cout << "," << endl;

// cout << "  \"structs\": [" << endl;
// for (auto it = gststruct.Entries.begin(); it != gststruct.Entries.end(); ++it)

// {   cout << "{" << endl;
// 	cout << "\"name\": " << "\"" << it->first << "\"," << endl;
// 	cout << "\"localST\": " << endl;
// 	it->second.symbtab->print();
// 	cout << "}" << endl;
// 	if (next(it,1) != gststruct.Entries.end()) 
// 	cout << "," << endl;
// }
// cout << "]," << endl;
// cout << "  \"functions\": [" << endl;

// for (auto it = gstfun.Entries.begin(); it != gstfun.Entries.end(); ++it)

// {
// 	cout << "{" << endl;
// 	cout << "\"name\": " << "\"" << it->first << "\"," << endl;
// 	cout << "\"localST\": " << endl;
// 	it->second.symbtab->print();
// 	cout << "," << endl;
// 	cout << "\"ast\": " << endl;
// 	ast[it->first]->print(0);
// 	cout << "}" << endl;
// 	if (next(it,1) != gstfun.Entries.end()) cout << "," << endl;
	
// }
// 	cout << "]" << endl;
// 	cout << "}" << endl;

// 	fclose(stdout);
// }
// void printAst(const char *astname, const char *fmt...) // fmt is a format string that tells about the type of the arguments.
// {   
// 	typedef vector<abstract_astnode *>* pv;
// 	va_list args;
// 	va_start(args, fmt);
// 	if ((astname != NULL) && (astname[0] != '\0'))
// 	{
// 		cout << "{ ";
// 		cout << "\"" << astname << "\"" << ": ";
// 	}
// 	cout << "{" << endl;
// 	while (*fmt != '\0')
// 	{
// 		if (*fmt == 'a')
// 		{
// 			char * field = va_arg(args, char *);
// 			abstract_astnode *a = va_arg(args, abstract_astnode *);
// 			cout << "\"" << field << "\": " << endl;
			
// 			a->print(0);
// 		}
// 		else if (*fmt == 's')
// 		{
// 			char * field = va_arg(args, char *);
// 			char *str = va_arg(args, char *);
// 			cout << "\"" << field << "\": ";

// 			cout << str << endl;
// 		}
// 		else if (*fmt == 'i')
// 		{
// 			char * field = va_arg(args, char *);
// 			int i = va_arg(args, int);
// 			cout << "\"" << field << "\": ";

// 			cout << i;
// 		}
// 		else if (*fmt == 'f')
// 		{
// 			char * field = va_arg(args, char *);
// 			double f = va_arg(args, double);
// 			cout << "\"" << field << "\": ";
// 			cout << f;
// 		}
// 		else if (*fmt == 'l')
// 		{
// 			char * field = va_arg(args, char *);
// 			pv f =  va_arg(args, pv);
// 			cout << "\"" << field << "\": ";
// 			cout << "[" << endl;
// 			for (int i = 0; i < (int)f->size(); ++i)
// 			{
// 				(*f)[i]->print(0);
// 				if (i < (int)f->size() - 1)
// 					cout << "," << endl;
// 				else
// 					cout << endl;
// 			}
// 			cout << endl;
// 			cout << "]" << endl;
// 		}
// 		++fmt;
// 		if (*fmt != '\0')
// 			cout << "," << endl;
// 	}
// 	cout << "}" << endl;
// 	if ((astname != NULL) && (astname[0] != '\0'))
// 		cout << "}" << endl;
// 	va_end(args);
}

