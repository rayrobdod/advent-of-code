.PHONY: all clean tidy tpp

OBJ_DIR := build
OBJ_NAMES := day1.o
ROM := day1.gb

COMPILE_FLAGS := -Wunmapped-char=0 -Weverything -I build

OBJS := $(OBJ_NAMES:%=$(OBJ_DIR)/%)
OBJ_DEPS := $(OBJS:%.o=%.d)

all: $(ROM)

include $(OBJ_DEPS)

$(OBJ_DIR):
	mkdir $@

$(OBJ_DIR)/day1.d $(OBJ_DIR)/main.o: $(OBJ_DIR)/oldschool.2bpp

$(OBJ_DIR)/%.d: %.asm | $(OBJ_DIR)
	rgbasm $(COMPILE_FLAGS) -M "$@" -MP -MQ "$@" -MQ "$(@:%.d=%.o)" $<

$(OBJ_DIR)/%.o: %.asm | $(OBJ_DIR)
	rgbasm $(COMPILE_FLAGS) -o $@ $<

$(ROM): $(OBJS) Makefile
	rgblink -n $(addprefix $(OBJ_DIR)/,$(addsuffix .sym,$(basename $(ROM)))) -m $(addprefix $(OBJ_DIR)/,$(addsuffix .map,$(basename $(ROM)))) -o $@ $(OBJS)
	rgbfix -jv -k 01 -l 0x33 -m MBC5+RAM -p 0 -r 02 -t "RRD_AOC" -i RAOC $@

tidy:
	rm -f $(ROM) $(OBJS) $(OBJ_DEPS) $(addprefix $(OBJ_DIR)/,$(addsuffix .sym,$(basename $(ROM)))) $(addprefix $(OBJ_DIR)/,$(addsuffix .map,$(basename $(ROM))))

clean: tidy
	find . \( -iname '*.1bpp' -o -iname '*.2bpp' \) -delete

$(OBJ_DIR)/%.2bpp: %.png
	rgbgfx -o $@ $<

$(OBJ_DIR)/%.1bpp: %.png
	rgbgfx -d1 -o $@ $<
