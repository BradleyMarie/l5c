all:
	make -C runtime
	raco exe -o L1c-bin l1c.rkt

clean:
	@rm -f runtime/runtime.o L1c-bin prog.* a.out L1c-preprocessor
