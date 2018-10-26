# OSDIR 				:=
# 	UNAME_S := $(shell uname -s)
# 	ifeq ($(UNAME_S),Linux)
# 		OSDIR = linux
# 	endif
# 	ifeq ($(UNAME_S),Darwin)
# 		OSDIR = osx
# 	endif
#EXE := .stack-work/install/x86_64-$(OSDIR)/lts-7.19/ghcjs-0.2.1.9007019_ghc-8.0.1/bin/test.jsexe/all.js

EXE := $(shell stack --verbosity none path --local-install-root)/bin/test.jsexe/all.js

tst:
	stack build :test;ls -LR .stack-work | grep jsexe;node $(EXE)

dev:
	stack build :test --fast --file-watch --exec 'node $(EXE)'
