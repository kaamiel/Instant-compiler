all:
	$(MAKE) -C src all
	mv src/insc_jvm src/insc_llvm .
