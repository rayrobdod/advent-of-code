	INCLUDE "lib/hardware.inc/hardware.inc"

	; basically a debug print
MACRO PRINT_REGISTER ; position, 16-bit register
	push hl
	push bc
	push de

	push \2
	pop de
	ld hl, \1
	call PrintHex_DE

	pop de
	pop bc
	pop hl
	ENDM

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

	; Copy tile data
	ld de, Font
	ld hl, $9200
	ld bc, Font.end - Font
	rst Copy


	;;;;; Part 1 Solution Start

	; hl - current spot in input
	; de - accumulator

	ld de, 0
	ld hl, Input

	; compare each item to next item
	; add item to accumulator if equal
.forEachInput1
	ld a, [hl+]
	ld b, a
	ld a, [hl]
	cp a, $0A ; '\n' terminates input
	jr z, .endOfInput1

	cp a, b
	jr nz, .forEachInput1

	and a, $0F
	add a, e
	daa
	ld e, a
	adc a, d
	daa
	sub a, e
	daa
	ld d, a
	jr .forEachInput1

.endOfInput1
	; compare last item to first item
	ld a, [Input]

	cp a, b
	jr nz, .processInputDone1

	and a, $0F
	add a, e
	daa
	ld e, a
	adc a, d
	daa
	sub a, e
	daa
	ld d, a

.processInputDone1

	push de

	ld hl, $9821
	ld de, Part1String
	ld bc, Part1String.end - Part1String
	rst Copy

	pop de
	call PrintHex_DE

	;;;;;; Part 2 Solution Start

	ld hl, Input
	ld bc, 0

.forEachInput2Count
	ld a, [hl+]
	inc bc
	cp a, $0A ; '\n' terminates input
	jr nz, .forEachInput2Count

	srl b
	rr c
	; now, `bc` is half of the length of the input
	; hl is at end of input

	push hl
	ld hl, Input
	add hl, bc
	ld b, h
	ld c, l
	pop bc

	inc hl
	; now, `hl` just past the midpoint of the input,
	; and `bc` is just past the end of the input

	ld de, 0 ; accumulator
	push de

	; compare the first half of the items to the item at `hl + bc`
	; add item to accumulator twice if equal
.forEachInput2
	dec hl
	dec bc

	ld d, [hl]
	ld a, [bc]
	cp a, d
	jr nz, .inputsAreNotEqual

	pop de
	and a, $0F
	add a, a
	daa
	add a, e
	daa
	ld e, a
	ld a, 0
	adc a, d
	daa
	ld d, a
	push de

.inputsAreNotEqual
	ASSERT(Input & $0FFF == 0)
	ASSERT((Input.end - Input) < $1000)
	; thus, reached start of input when `hl & $0FFF == 0`

	ld a, h
	and a, $0F
	or a, l
	jr nz, .forEachInput2

.processInputDone2

	ld hl, $9861
	ld de, Part2String
	ld bc, Part2String.end - Part2String
	rst Copy

	pop de
	call PrintHex_DE

	;;;;; Puzzle Solution End

	; Turn the LCD on
	ld a, LCDCF_ON | LCDCF_BGON
	ldh [rLCDC], a

	; During the first (blank) frame, initialize display registers
	ld a, %11100100
	ldh [rBGP], a

Done:
	halt
	jr Done

SECTION "Copy", ROM0, ALIGN[3]
; hl - [in] pointer to target - [out] hl + bc
; de - [in] pointer to source - [out] de + bc
; bc - [in] number of bytes to copy - [out] 0
Copy::
	; 9 bytes; could fit in two rsts
	ld a, [de]
	ld [hl+], a
	inc de
	dec bc
	ld a, b
	or a, c
	jr nz, Copy
	ret

SECTION "Print", ROM0
; hl - the start of where to print to
; de - value to print
PrintHex_DE:
	ASSERT(Digits & $0F == 0)
	REPT 3
	inc hl
	ENDR

	ld a, e
	and a, $0F
	or a, LOW(Digits)
	ld b, HIGH(Digits)
	ld c, a
	ld a, [bc]
	ld [hl-], a

	ld a, e
	and a, $F0
	swap a
	or a, LOW(Digits)
	;ld b, HIGH(Digits)
	ld c, a
	ld a, [bc]
	ld [hl-], a

	ld a, d
	and a, $0F
	or a, LOW(Digits)
	;ld b, HIGH(Digits)
	ld c, a
	ld a, [bc]
	ld [hl-], a

	ld a, d
	and a, $F0
	swap a
	or a, LOW(Digits)
	;ld b, HIGH(Digits)
	ld c, a
	ld a, [bc]
	ld [hl], a

	REPT 4
	dec hl
	ENDR

	ret

SECTION "Font", ROM0
Font:
	INCBIN "build/oldschool.2bpp"
.end

SECTION "Input", ROM0, ALIGN[12]
Input:
	INCBIN "input.txt"
.end

SECTION "Part Strings", ROM0
Part1String:
	DB "Part 1: "
.end
Part2String:
	DB "Part 2: "
.end

Section "Digits", ROM0, ALIGN[4]
Digits:
	DB "0123456789ABCDEF"
