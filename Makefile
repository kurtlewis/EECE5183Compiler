# $@ = target, $^ = preqrequisites
# Compiler
CXX=g++
# CPPFLAGS = 
# CXXFLAGS = 
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
DEPS = $(addprefix $(IDIR)/, )
# Object files for creating final executable
OBJS = $(addprefix $(ODIR)/, main.o)

# Create the compiler
$(OUT_NAME): $(OBJS)
	$(CXX) -o $@ $^

# Rule for implicitly creating object files when they're found to be out of date
$(ODIR)/%.o : %.cc $(DEPS)
	$(CXX) -c $(CPPFLAGS) $(CXXFLAGS) $< -o $@

# mark the Out directory as a dependency only if it doesn't exist
$(OBJS): | $(ODIR)

# Create out directory if it does not exist
$(ODIR):
	mkdir $(ODIR)

clean:
	rm -f $(ODIR)/*.o
	rm $(OUT_NAME)