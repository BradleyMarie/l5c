all: runtime/runtime.o
	raco exe -o L1c-bin l1c.rkt

runtime:
	make -C runtime

clean:
	@rm -f runtime/runtime.o L1c-bin prog.* a.out
