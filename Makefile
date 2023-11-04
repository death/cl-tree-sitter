PKG_CONFIG=$(shell command -v pkg-config)

ifneq ("$(PKG_CONFIG)", "")
# the ?= will only compute the RHS if the variable
# is not bound already
TREE_SITTER_INCLUDE ?= $(shell $(PKG_CONFIG) --cflags tree-sitter)
TREE_SITTER_LIB ?= $(shell $(PKG_CONFIG) --libs tree-sitter)
endif

CFLAGS = -g -O2 -Wall -Wno-unused-value -fPIC -shared
CFLAGS+= $(TREE_SITTER_INCLUDE)
LDFLAGS = $(TREE_SITTER_LIB)
FILE = tree-sitter-wrapper.c
SO = tree-sitter-wrapper.so

all: $(SO)

$(SO): $(FILE)
	cc $(CFLAGS) $(FILE) -o $(SO) $(LDFLAGS)

clean:
	-rm $(SO)
