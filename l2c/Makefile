all: l2c graph liveness spill

l2c:
	raco exe -o L2c l2c.rkt

graph:
	raco exe -o graph graph.rkt

liveness:
	raco exe -o liveness liveness.rkt

spill:
	raco exe -o spill spill.rkt

clean:
	@rm -f L2c graph liveness spill
