RUNGHC = stack runghc

.PHONY : all
all : rtl/divider.vhd tb_divider.vhd

.PHONY : clean
clean :
	rm -f rtl/divider.vhd tb_divider.vhd
	rm -rf vunit_out

rtl/divider.vhd tb_divider.vhd : Main.hs
	$(RUNGHC) $< -- -o rtl/divider.vhd -t tb_divider.vhd || rm -f rtl/divider.vhd tb_divider.vhd

.PHONY : test
test : rtl/divider.vhd tb_divider.vhd
	./run.py
