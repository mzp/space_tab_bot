MODULES = \
	Main \
	Base \
	Pathname \
	Github \
	Http \
	Setting \
	Detector \
	Message \
	Json

C_MODULES = \
	libjansson/janssonext \
	liburiparser/uriparserext

TARGET = space_tab_bot

TEST_MODULES = \
	Main \
	BaseTest \
	UriTest \
	PathnameTest \
	SettingTest \
	DetectorTest \
	JanssonTest \
	JsonTest

TEST_TARGET = testRunner

LIB_MODULES = \
	smlsharp-lib/GetOpt \
	libcurl/Curl \
	liburiparser/Uri \
	libjansson/Jansson \

SMLSHARP = smlsharp
SMLSHARP_CFLAGS = -O2
SMLSHARP_LDFLAGS = -lcurl -ljansson -luriparser

CFLAGS += -m32

# ------------------------------------------------------------
sources := $(addprefix src/,$(MODULES:=.sml))
objects := $(sources:.sml=.o)

c_sources = $(addprefix lib/,$(C_MODULES:=.c))
c_objects := $(c_sources:.c=.o)

test_sources := $(addprefix test/, $(TEST_MODULES:=.sml))
test_objects := $(test_sources:.sml=.o)

lib_sources := $(addprefix lib/,$(LIB_MODULES:=.sml))
lib_objects := $(lib_sources:.sml=.o)
# ------------------------------------------------------------

$(TARGET): $(objects) $(c_objects) $(lib_objects)
	$(SMLSHARP) $(SMLSHARP_LDFLAGS) -o $@ src/Main.smi $(c_objects)

$(TEST_TARGET): $(objects) $(c_objects) $(lib_objects) $(test_objects)
	$(SMLSHARP) $(SMLSHARP_LDFLAGS) -o $@ test/Main.smi $(c_objects)

# ------------------------------------------------------------
#  Build rules
# ------------------------------------------------------------
%.o: %.sml
	$(SMLSHARP) $(SMLSHARP_CFLAGS) -c -o $@ $<

.c.o:
	$(CC) $(CFLAGS) $(DEFS) $(CPPFLAGS) -c -o $@ $<

# ------------------------------------------------------------
#  Phony target
# ------------------------------------------------------------
.PHONY: clean depend check

check: $(TEST_TARGET)
	./$(TEST_TARGET)

clean:
	rm -rf $(TARGET) src/*.o test/*.o lib/*/*.o

depend:
	$(SMLSHARP) -MM $(sources) $(test_sources) $(lib_sources) > .depend
-include .depend

