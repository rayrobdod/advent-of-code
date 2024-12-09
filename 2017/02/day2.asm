	INCLUDE "hardware.inc/hardware.inc"
	INCLUDE "debug_prints.inc"

SECTION "Header", ROM0[$100]
	nop
	jp EntryPoint
	ds $150 - @, 0

SECTION "Interrupt_Vblank", ROM0[$0040]
	reti
SECTION "Interrupt_LCDC", ROM0[$0048]
	reti
SECTION "Interrupt_Timer_Overflow", ROM0[$0050]
	reti
SECTION "Interrupt_Serial", ROM0[$0058]
	reti
SECTION "Interrupt_p1thru4", ROM0[$0060]
	reti


SECTION "EntryPoint", ROM0
EntryPoint:
	; Do not turn the LCD off outside of VBlank
.waitVBlank:
	ldh a, [rLY]
	cp SCRN_Y
	jr c, .waitVBlank

	; Turn the LCD off
	ld a, 0
	ldh [rLCDC], a

	ld sp, Stack

	; Copy tile data
	ld de, Font
	ld hl, $9200
	ld bc, SIZEOF("Font")
	rst Copy

	call Part1

	; Print the resut of Part 1
	ld de, Part1String
	ld hl, $9821
	ld bc, Part1String.end - Part1String
	rst Copy

	ld a, [Accumulator + 3]
	ld b, a
	ld a, [Accumulator + 2]
	ld c, a
	ld a, [Accumulator + 1]
	ld d, a
	ld a, [Accumulator]
	ld e, a
	call PrintHex_BCDE

	; Puzzle Solution End

	; Turn the LCD on
	ld a, LCDCF_ON | LCDCF_BGON
	ldh [rLCDC], a

	; During the first (blank) frame, initialize display registers
	ld a, %11100100
	ldh [rBGP], a

.done:
	halt
	jr .done

SECTION "Part1", ROM0
Part1:
	ld hl, Input

	ASSERT(Accumulator & $FF00 == $FF00)
	ld c, LOW(Accumulator)
	ld a, 0
	REPT 4
	ldh [c], a
	inc c
	ENDR

.forEachLine
	ld a, [hl]
	cp a, "\n"
	ret z

	call Parse_BCD16_BC
	ld a, b
	ldh [LineMax + 1], a
	ldh [LineMin + 1], a
	ld a, c
	ldh [LineMax], a
	ldh [LineMin], a

.forEachCell
	ld a, [hl+]
	cp a, "\n"
	jp z, .lineEnd

	call Parse_BCD16_BC

	; LineMax = max(LineMax, bc)
	ld a, [LineMax + 1]
	ld d, a
	ld a, [LineMax]
	ld e, a
	call Compare_BC_DE
	jp c, .maxUpdateDone
.maxUpdateApply
	ld a, b
	ldh [LineMax + 1], a
	ld a, c
	ldh [LineMax], a
.maxUpdateDone

	; LineMin = min(LineMin, bc)
	ld a, [LineMin + 1]
	ld d, a
	ld a, [LineMin]
	ld e, a
	call Compare_BC_DE
	jp nc, .minUpdateDone
.minUpdateApply
	ld a, b
	ldh [LineMin + 1], a
	ld a, c
	ldh [LineMin], a
.minUpdateDone

	jp .forEachCell

.lineEnd
	; Accumulator += LineMax - LineMin
	ldh a, [LineMin]
	ld e, a
	ldh a, [LineMin + 1]
	ld d, a
	ldh a, [LineMax]
	ld c, a
	ldh a, [LineMax + 1]
	ld b, a

	ld a, c
	sub a, e
	daa
	ld c, a
	ld a, b
	sbc a, d
	daa
	ld b, a

	ldh a, [Accumulator]
	add a, c
	daa
	ldh [Accumulator], a
	ldh a, [Accumulator + 1]
	adc a, b
	daa
	ldh [Accumulator + 1], a
	ld b, 0
	ldh a, [Accumulator + 2]
	adc a, b
	daa
	ldh [Accumulator + 2], a
	ldh a, [Accumulator + 3]
	adc a, b
	daa
	ldh [Accumulator + 3], a

	jp .forEachLine

	ret

SECTION "Part1_Vars", HRAM
Accumulator:
	DL
LineMax:
	DW
LineMin:
	DW

SECTION "Font", ROM0
Font:
	INCBIN "build/oldschool.2bpp"

SECTION "Input", ROM0
Input:
	INCBIN "input.txt"
	DB "\n"

SECTION "Part Strings", ROM0
Part1String:
	DB "Part 1: "
.end
Part2String:
	DB "Part 2: "
.end

SECTION "Stack", WRAM0
	DS $FF
Stack:
	DS 1
