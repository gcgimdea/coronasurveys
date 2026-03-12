SNAPDIR=snap
#../../../../snap

CC=g++
LINKER   = g++

TARGET   = graphSampler


# compiling flags here
CFLAGS=-c -O3 -std=c++14
LFLAGS   = -Wall -I. -lm #-fopenmp

SRCDIR   = src
OBJDIR   = obj
BINDIR   = bin

SOURCES  := $(wildcard $(SRCDIR)/*.cpp)
INCLUDES= -I $(SNAPDIR)/snap-core -I $(SNAPDIR)/glib-core
OBJECTS  := $(SOURCES:$(SRCDIR)/%.cpp=$(OBJDIR)/%.o)


EXECUTABLE=graphSampler
SNAPLIB=$(SNAPDIR)/snap-core/Snap.o

rm       = rm -f

all:	$(SNAPLIB) $(SOURCES) $(BINDIR)/$(TARGET)

$(SNAPLIB):	
	$(MAKE) -C $(SNAPDIR) all

$(BINDIR)/$(TARGET): $(OBJECTS)
	mkdir -p bin
	@$(LINKER) $(OBJECTS) $(LFLAGS) -o $@ $(SNAPLIB)
	@echo "Linking complete!"

$(OBJECTS): $(OBJDIR)/%.o : $(SRCDIR)/%.cpp
	mkdir -p obj
	@$(CC) $(CFLAGS) -c $< $(INCLUDES) -o $@
	@echo "Compiled "$<" successfully!"

.PHONY: clean
clean:
	@$(rm) $(OBJECTS)s
	@echo "Cleanup complete!"

.PHONY: remove
remove: clean
	@$(rm) $(BINDIR)/$(TARGET)
	@echo "Executable removed!"


