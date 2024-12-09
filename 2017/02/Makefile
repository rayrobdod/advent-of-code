.PHONY: all clean tidy tpp

OBJ_DIR := build
OBJ_NAMES := day2.o copy.o print.o parse.o compare_word.o
ROM := day2.gb

COMPILE_FLAGS := -Wunmapped-char=0 -Weverything -I build -I ../lib/include

OBJS := $(OBJ_NAMES:%=$(OBJ_DIR)/%)
OBJ_DEPS := $(OBJS:%.o=%.d)

all: $(ROM)

include $(OBJ_DEPS)

$(OBJ_DIR):
	mkdir $@

$(OBJ_DIR)/day2.d: $(OBJ_DIR)/oldschool.2bpp

$(OBJ_DIR)/%.d: %.asm | $(OBJ_DIR)
	rgbasm $(COMPILE_FLAGS) -M "$@" -MP -MQ "$@" -MQ "$(@:%.d=%.o)" $<

$(OBJ_DIR)/%.o: %.asm | $(OBJ_DIR)
	rgbasm $(COMPILE_FLAGS) -o $@ $<

$(OBJ_DIR)/%.d: ../lib/src/%.asm | $(OBJ_DIR)
	rgbasm $(COMPILE_FLAGS) -M "$@" -MP -MQ "$@" -MQ "$(@:%.d=%.o)" $<

$(OBJ_DIR)/%.o: ../lib/src/%.asm | $(OBJ_DIR)
	rgbasm $(COMPILE_FLAGS) -o $@ $<

$(ROM): $(OBJS) Makefile
	rgblink -n $(addprefix $(OBJ_DIR)/,$(addsuffix .sym,$(basename $(ROM)))) -m $(addprefix $(OBJ_DIR)/,$(addsuffix .map,$(basename $(ROM)))) -o $@ $(OBJS)
	rgbfix -jv -k 01 -l 0x33 -m ROM -p 0 -r 0 -t "RRD_AOC_01" -i RAOC $@

tidy:
	rm -f $(ROM) $(OBJS) $(OBJ_DEPS) $(addprefix $(OBJ_DIR)/,$(addsuffix .sym,$(basename $(ROM)))) $(addprefix $(OBJ_DIR)/,$(addsuffix .map,$(basename $(ROM))))

clean: tidy
	find . \( -iname '*.2bpp' \) -delete
	rm -r build

$(OBJ_DIR)/%.2bpp: ../lib/image/%.png
	rgbgfx -o $@ $<
