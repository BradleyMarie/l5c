#include <fstream>
#include <string>
#include <iostream>
using namespace std;

int main (int argc, char ** argv) {
  if ( argc != 2 ) {
    std::cout << "usage: L1-preprocessor file" << std::endl;
    return -1;
  }

  std::ifstream file( argv[1] );
  std::string line;

  while( std::getline(file, line) )
  {
    for ( size_t i = 0; i < line.size(); i++ )
    {
      if ( line[i] == ';' )
        break;

      std::cout << line[i];
    }

    std::cout << std::endl;
  }

  file.close();
}
