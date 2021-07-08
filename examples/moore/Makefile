.PHONY : all
all : rtl/incrementer.vhd tb_incrementer.vhd

.PHONY : clean
clean :
	rm -f rtl/incrementer.vhd tb_incrementer.vhd
	rm -rf vunit_out

.PHONY : test
test : rtl/incrementer.vhd tb_incrementer.vhd
	./run.py

rtl/incrementer.vhd tb_incrementer.vhd : src/Main.hs
	stack run moore
