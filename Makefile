format:
	find src -type f -name "*.hs" | xargs ormolu --ghc-opt -XArrows --mode inplace