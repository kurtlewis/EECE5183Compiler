# $@ = target, $^ = preqrequisites
# Compiler
CXX=g++
# CPPFLAGS = 
CXXFLAGS = -I $(IDIR) -std=c++11
# LLVM command
LLVM = `llvm-config --cxxflags --ldflags --libs --system-libs`
# Include directory
IDIR=./include
# Look for *.h files in IDIR
vpath %.h $(IDIR)
# Source directory
SDIR=./src
# Look for *.cc files in SDIR
vpath %.cc $(SDIR)
# Out directory
ODIR=./out
#vpath %.o $(ODIR)
# Name of outputed compiler
OUT_NAME=kjlc

# header files that are depended upon
DEPS = $(addprefix $(IDIR)/, $(addprefix kjlc/, scanner.h parser.h symbol.h symbol_table.h))
# Object files for creating final executable
OBJS = $(addprefix $(ODIR)/, main.o scanner.o parser.o symbol.o symbol_table.o)

# Create the compiler
$(ODIR)/$(OUT_NAME): $(OBJS)
	$(CXX) -o $@ $(CXXFLAGS) $^ $(LLVM) 

# Rule for implicitly creating object files when they're found to be out of date
$(ODIR)/%.o : %.cc $(DEPS)
	$(CXX) -o $@ -c $(CPPFLAGS) $(CXXFLAGS) $< $(LLVM)

# mark the Out directory as a dependency only if it doesn't exist
$(OBJS): | $(ODIR)

# Create out directory if it does not exist
$(ODIR):
	mkdir $(ODIR)

# test control
test: $(ODIR)/$(OUT_NAME)
	python3 testPgms/test.py

clean:
	rm -f $(ODIR)/*.o
	rm $(ODIR)/$(OUT_NAME)
