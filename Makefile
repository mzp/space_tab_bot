MODULES = \
	Main \
	Base \
 	Uri \
 	Pathname \
	Github \
 	Setting \
 	Detector

TARGET = space_tab_bot

TEST_MODULES = \
	Main \
	BaseTest \
	PathnameTest \
	SettingTest \
	DetectorTest

TEST_TARGET = testRunner

SMLSHARP = smlsharp
SMLSHARP_FLAGS = -O2

# ------------------------------------------------------------
sources := $(addprefix src/,$(MODULES:=.sml))
objects := $(sources:.sml=.o)

test_sources := $(addprefix test/, $(TEST_MODULES:=.sml))
test_objects := $(test_sources:.sml=.o)
# ------------------------------------------------------------

$(TARGET): $(objects)
	$(SMLSHARP) $(SMLSHARP_FLAGS) -o $@ src/Main.smi

$(TEST_TARGET): $(objects) $(test_objects)
	$(SMLSHARP) $(SMLSHARP_FLAGS) -o $@ test/Main.smi

# ------------------------------------------------------------
#  Build rules
# ------------------------------------------------------------
%.o: %.sml
	$(SMLSHARP) $(SMLSHARP_FLAGS) -c -o $@ $<

# ------------------------------------------------------------
#  Phony target
# ------------------------------------------------------------
.PHONY: clean depend check

check: $(TEST_TARGET)
	./$(TEST_TARGET)

clean:
	rm -rf $(TARGET) src/*.o test/*.o

depend:
	$(SMLSHARP) -MM $(sources) $(test_sources) > .depend
-include .depend

