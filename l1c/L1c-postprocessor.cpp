#include <iostream>
using namespace std;

const char * header = ".text\n.globl go\n.type go, @function\n\ngo:\npushl %ebp\nmovl %esp, %ebp\npushl %ebx\npushl %esi\npushl %edi\npushl %ebp\nmovl %esp, %ebp";
const char * footer = ".size go, .-go\n.section .note.GNU-stack,\"\",@progbits";

int main() {
	
	cout << header <<  endl;

	while(cin) {
		std::string input_line;
		getline(cin, input_line);
		cout << input_line << endl;
	}

	cout << footer << endl;

	return 0;
}
