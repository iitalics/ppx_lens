PKG=ppx_lens

build:
	jbuilder build -p $(PKG)

test:
	jbuilder runtest -p $(PKG)

clean:
	jbuilder clean

.PHONY: build test clean
