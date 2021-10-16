.PHONY: fmt cabal-fmt src-fmt
.PHONY: install

ROOTDIR := .
APP_HS_FILES := $(shell find $(ROOTDIR)/app -type f -name "*.hs")
SRC_HS_FILES := $(shell find $(ROOTDIR)/src -type f -name "*.hs")

fmt: cabal-fmt src-fmt

cabal-fmt: *.cabal
	cabal-fmt -i $^

src-fmt: $(APP_HS_FILES) $(SRC_HS_FILES)
	stylish-haskell -ri $^

notion-cli: $(APP_HS_FILES) $(SRC_HS_FILES)
	cabal update
	cabal build
	cabal install --installdir=. --install-method=copy exe:notion-cli
