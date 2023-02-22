.PHONY: fmt cabal-fmt src-fmt
.PHONY: install

ROOTDIR := .
APP_HS_FILES := $(shell find $(ROOTDIR)/app -type f -name "*.hs")
SRC_HS_FILES := $(shell find $(ROOTDIR)/src -type f -name "*.hs")
GHC_VERSION := 9.2.5
CABAL_VERSION := 3.6.2.0

fmt: cabal-fmt src-fmt

cabal-fmt: *.cabal
	cabal-fmt -i $^

src-fmt: $(APP_HS_FILES) $(SRC_HS_FILES)
	stylish-haskell -ri $^

ghc:
	ghcup install ghc $(GHC_VERSION)

cabal:
	ghcup install cabal $(CABAL_VERSION)

notion-cli: ghc cabal $(APP_HS_FILES) $(SRC_HS_FILES)
	cabal update
	cabal build -w ghc-$(GHC_VERSION)
	cabal install \
		--installdir=. \
		--install-method=copy \
		--overwrite-policy=always \
		exe:notion-cli
