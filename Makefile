BENCH_CSV = bench.csv

all: pre-run perf-c perf-cyclone perf-gambit perf-julia perf-python perf-pypython

pre-run:
ifneq ("$(wildcard $(BENCH_CSV))","")
	mv -f $(BENCH_CSV) $(BENCH_CSV)-`date +%F-%N`
endif

perf-c:
	cc -o perf-c perf.c
	./perf-c >> $(BENCH_CSV)

perf-cyclone:
	cyclone cyclone/printf.sld
	cyclone perf-cyclone.scm
	./perf-cyclone >> $(BENCH_CSV)

perf-gambit:
	gsc -o perf-gambit -exe perf-gambit.scm
	./perf-gambit >> $(BENCH_CSV)

perf-julia:
	julia perf.jl >> $(BENCH_CSV)

perf-python: # cpython-config
	python3 perf.py >> $(BENCH_CSV)

perf-pypython: # pypy-config
	pypy3 perf.py >> $(BENCH_CSV)

cpython-config:
	sudo apt install python3
	pip install numpy

pypy-config:
	sudo apt install pypy3
	pypy3 -m pip install numpy

clean:
	rm -f perf-c perf-cyclone perf-cyclone.{c,o} cyclone/printf.{meta,o,so,c} perf-gambit

.PHONY: all pre-run perf-c perf-cyclone perf-gambit perf-julia perf-python perf-pypython cpython-config pypy-config clean
