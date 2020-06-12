all: raclette

RKT := main.rkt $(wildcard private/*.rkt)

raclette: $(RKT)
	raco exe --vv -o $@ main.rkt

.PHONY: release
release: raclette
	raco distribute -v raclette-v`sed -nr 's/\(define version "([0-9]+\.[0-9]+)"\)/\1/p' info.rkt` $<
