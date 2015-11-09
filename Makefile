ROOT_DIR = $(realpath .)
INTERACTIVE    = -it --rm
CABAL_DIR = $(realpath ./.cabal)
GHC_DIR = $(realpath ./.ghc)

all: dirs docker update install sdist

docker:
	sudo docker build -t telegram .

repl:
	docker run \
		$(INTERACTIVE) \
		-v $(HOME):$(HOME) \
		-v $(PWD):/src \
		-v $(CABAL_DIR):/root/.cabal \
		-v $(GHC_DIR):/root/.ghc \
		telegram \
		cabal repl

bash:
	docker run \
		$(INTERACTIVE) \
		-v $(HOME):$(HOME) \
		-v $(PWD):/src \
		-v $(CABAL_DIR):/root/.cabal \
		-v $(GHC_DIR):/root/.ghc \
		telegram \
		/bin/bash

dirs:
	mkdir -p .cabal
	mkdir -p .ghc

update:
	docker run \
		$(INTERACTIVE) \
		-v $(PWD):/src \
		-v $(CABAL_DIR):/root/.cabal \
		-v $(GHC_DIR):/root/.ghc \
		telegram \
		cabal update

install:
	docker run \
		$(INTERACTIVE) \
		-v $(PWD):/src \
		-v $(CABAL_DIR):/root/.cabal \
		-v $(GHC_DIR):/root/.ghc \
		telegram \
		cabal install -j
