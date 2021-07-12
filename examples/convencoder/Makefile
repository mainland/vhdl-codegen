GENERATED = rtl/encoder.vhd tb_encoder.vhd

.PHONY : all
all : $(GENERATED)

.PHONY : clean
clean :
	rm -f $(GENERATED)
	rm -rf vunit_out

.PHONY : test
test : $(GENERATED)
	./run.py

$(GENERATED) : src/Main.hs
	stack run convencoder
