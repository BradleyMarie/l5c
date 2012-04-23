all: graph liveness spill

graph:
	raco exe -o graph graph.rkt

liveness:
	raco exe -o liveness liveness.rkt

spill:
	raco exe -o spill spill.rkt

clean:
	@rm -f graph liveness spill