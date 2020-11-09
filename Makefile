all:
	$(MAKE) -C src all
	cp src/insc_jvm src/insc_llvm .
