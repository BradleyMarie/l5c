all:
	make -C runtime
	g++ -Wall -Werror -O2 -o L1c-preprocessor L1c-preprocessor.cpp
	raco exe -o L1c-bin l1c.rkt

clean:
	@rm -f runtime/runtime.o L1c-bin prog.* a.out L1c-preprocessor