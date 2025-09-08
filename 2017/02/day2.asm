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

	ld a, [Accumulator1 + 3]
	ld b, a
	ld a, [Accumulator1 + 2]
	ld c, a
	ld a, [Accumulator1 + 1]
	ld d, a
	ld a, [Accumulator1]
	ld e, a
	call PrintHex_BCDE

	call Part2

	; Print the resut of Part 2
	ld de, Part2String
	ld hl, $9841
	ld bc, Part2String.end - Part2String
	rst Copy

	ld a, [Accumulator2 + 3]
	ld b, a
	ld a, [Accumulator2 + 2]
	ld c, a
	ld a, [Accumulator2 + 1]
	ld d, a
	ld a, [Accumulator2]
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

	ASSERT(Accumulator1 & $FF00 == $FF00)
	ld c, LOW(Accumulator1)
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
	; Accumulator1 += LineMax - LineMin
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

	ldh a, [Accumulator1]
	add a, c
	daa
	ldh [Accumulator1], a
	ldh a, [Accumulator1 + 1]
	adc a, b
	daa
	ldh [Accumulator1 + 1], a
	ld b, 0
	ldh a, [Accumulator1 + 2]
	adc a, b
	daa
	ldh [Accumulator1 + 2], a
	ldh a, [Accumulator1 + 3]
	adc a, b
	daa
	ldh [Accumulator1 + 3], a

	jp .forEachLine

	ret

SECTION "Part1_Vars", HRAM, ALIGN[4]
Accumulator1:
	DL
LineMax:
	DW
LineMin:
	DW

SECTION "Part2", ROM0
Part2:
	ld hl, Sample

	ASSERT(Accumulator2 & $FF00 == $FF00)
	ld c, LOW(Accumulator2)
	ld a, 0
	REPT 4
	ldh [c], a
	inc c
	ENDR

.forEachLine
	ld a, [hl]
	cp a, "\n"
	ret z

	call Parse_16_BC
	ld a, c
	ldh [NumbersInLine], a
	ld a, b
	ldh [NumbersInLine+1], a

	ld a, 1
	ldh [NumbersInLineCount], a

.parseCell
	ld a, [hl+]
	cp a, "\n"
	jp z, .parseCellsEnd

	call Parse_16_BC
	ldh a, [NumbersInLineCount]
	ld d, b
	ld e, c
	ld c, LOW(NumbersInLine)
	sla a
	add a, c
	ld c, a
	ld a, e
	ld [c], a
	inc c
	ld a, d
	ld [c], a

	ldh a, [NumbersInLineCount]
	inc a
	ldh [NumbersInLineCount], a
	jr .parseCell

.parseCellsEnd


	jp .forEachLine

SECTION "Part2_Vars", HRAM, ALIGN[4]
Accumulator2:
	DL
NumbersInLine:
	REPT 20
	DW
	ENDR
NumbersInLineCount:
	DB
Ptr1:
	DB
Ptr2:
	DB

SECTION "Font", ROM0
Font:
	INCBIN "build/oldschool.2bpp"

SECTION "Input", ROM0
Input:
	INCBIN "input.txt"
	DB "\n"

SECTION "Sample", ROM0
Sample:
	INCBIN "sample.txt"
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
