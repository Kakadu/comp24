.PHONY: copydemo

all:

copydemo:
	$(RM) -rf $(DEST)
	cp -r demo $(DEST)
	mv $(DEST)/demo.opam $(DEST)/$(DEST).opam
	sed 's/demo/$(DEST)/g' -i $(DEST)/dune-project $(DEST)/bin/dune

