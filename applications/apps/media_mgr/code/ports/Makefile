.SUFFIXES: .erl .beam .yrl

.erl.beam:

	erlc -W $<

MODS = example1 example1_lid

all: 	 ${MODS:%=%.beam} example1 example1_drv.so

example1:  example1.c erl_comm.c example1_driver.c
	gcc -o example1 example1.c erl_comm.c example1_driver.c

example1_drv.so: example1_lid.c example1.c
	gcc -o example1_drv.so -fpic -shared example1.c example1_lid.c

clean:
	rm example1 example1_drv.so *.beam

