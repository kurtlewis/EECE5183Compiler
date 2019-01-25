# $@ = target, $^ = preqrequisites
# Compiler
CC=g++
# Include directory
IDIR=./include
# Source directory
SDIR=./src
# Out directory
ODIR=./out
# Name of outputed compiler
OUT_NAME=kjlc

# header files that are depended upon
DEPS = $(INCLUDE_DIR)/*
# Object files for creating final executable
OBJS = $(addprefix $(ODIR)/, main.o)

# Create the compiler
$(OUT_NAME): $(OBJS)
	$(CC) $^ -o $@ 

$(ODIR)/main.o : $(SDIR)/main.cc
	$(CC) -c $^ -o $@

# mark the Out directory as a dependency only if it doesn't exist
$(OBJS): | $(ODIR)

# Create out directory if it does not exist
$(ODIR):
	mkdir $(ODIR)

clean:
	rm -f $(ODIR)/*.o
	rm $(OUT_NAME)