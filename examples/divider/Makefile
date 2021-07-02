RUNGHC = stack runghc

.PHONY : all
all : rtl/divider.vhd tb_divider.vhd

.PHONY : clean
clean :
	rm -f rtl/divider.vhd tb_divider.vhd
	rm -rf vunit_out

rtl/divider.vhd tb_divider.vhd : Main.hs
	stack run divider -- -o rtl/divider.vhd -t tb_divider.vhd || rm -f $@

data/in.csv data/out.csv : Main.hs
	stack run divider -- -n 100 --tvinput data/in.csv --tvoutput data/out.csv || rm -f $@

.PHONY : test
test : rtl/divider.vhd tb_divider.vhd data/in.csv data/out.csv
	./run.py
