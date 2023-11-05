CC ?= cc

PKG_CONFIG=$(shell command -v pkg-config)

OS = $(shell uname -o)

ifeq ($(OS), Darwin)
SOEXT = .dylib
DYNFLAG= -dynamiclib
else
SOEXT = .so
DYNFLAG= -shared
endif

ifneq ("$(PKG_CONFIG)", "")
# the ?= will only compute the RHS if the variable
# is not bound already
TREE_SITTER_INCLUDE ?= $(shell $(PKG_CONFIG) --cflags tree-sitter)
TREE_SITTER_LIB ?= $(shell $(PKG_CONFIG) --libs tree-sitter)
endif

CFLAGS = -g -O2 -Wall -Wno-unused-value -fPIC $(DYNFLAG)
CFLAGS+= $(TREE_SITTER_INCLUDE)
LDFLAGS = $(TREE_SITTER_LIB)
FILE = tree-sitter-wrapper.c
SO = tree-sitter-wrapper$(SOEXT)

all: $(SO)

$(SO): $(FILE)
	$(CC) $(CFLAGS) $(FILE) -o $(SO) $(LDFLAGS)

clean:
	-rm $(SO)
