.PHONY: all clean run

CC = clang-16
CFLAGS = -Wall -Wextra -O2 -I. -I/Library/Developer/CommandLineTools/SDKs/MacOSX14.5.sdk/usr/include/ffi
LDFLAGS = -r

SRCS = taggedValue.c basicFunctions.c closures.c binaryOperations.c unaryOperations.c printers.c

BIN_DIR = bin

LL_FILE = $(BIN_DIR)/arml.ll

OBJS = $(addprefix $(BIN_DIR)/, $(SRCS:.c=.o))

TARGET = $(BIN_DIR)/runtime.o

all: $(TARGET)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

$(TARGET): $(OBJS) | $(BIN_DIR)
	$(LD) $(LDFLAGS) -o $@ $^

$(BIN_DIR)/%.o: %.c | $(BIN_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f $(OBJS) $(TARGET)
	rmdir $(BIN_DIR) 2>/dev/null || true

run: $(TARGET)
	$(CC) -Wno-override-module $(CFLAGS) $(LLFILE) $(TARGET) -o a.out
