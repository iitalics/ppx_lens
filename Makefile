
build:
	jbuilder build

ppx:
	jbuilder build -p ppx_lens

lib:
	jbuilder build -p lenslib

test:
	jbuilder runtest

clean:
	jbuilder clean

.PHONY: build ppx lib test clean
