SNAPDIR=snap
#../../../../snap

CC=g++
LINKER   = g++

TARGET   = scaleUpDirectSurveyComparator


# compiling flags here
#CFLAGS=-c -O3 -std=c++14 -ggdb
CFLAGS=-c -std=c++14 -ggdb
LFLAGS   = -Wall -I. -lm -fopenmp -ggdb

SRCDIR   = src
OBJDIR   = obj
BINDIR   = bin

SOURCES  := $(wildcard $(SRCDIR)/*.cpp)
INCLUDES= -I $(SNAPDIR)/snap-core -I $(SNAPDIR)/glib-core
OBJECTS  := $(SOURCES:$(SRCDIR)/%.cpp=$(OBJDIR)/%.o)


#SOURCES= abstractGraphBasedSurveyDiffusion.cpp abstractSurvey.cpp directEstimationSurvey.cpp  socialGraphSurveyDiffusion.cpp  uniformRandomSpreader.cpp abstractSpreader.cpp scaleUpDirectSurveyComparator.cpp  networkScaleUpSurvey.cpp testGraphSampler.cpp util.cpp
# OBJECTS=$(SOURCES:.cpp=.o)
EXECUTABLE=scaleUpDirectSurveyComparator
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


