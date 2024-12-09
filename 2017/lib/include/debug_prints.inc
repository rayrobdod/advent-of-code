; Macros that will, without observably changing any registers,
; write a value to memory (probably VRAM)

; For print debugging


MACRO DEBUGPRINT_CHAR ; position, value
	push af
	push bc
	push de
	push hl

	ld hl, \1
	ld a, \2
	ld [hl], a

	pop hl
	pop de
	pop bc
	pop af
	ENDM

MACRO DEBUGPRINT_CLEAR ; position, length
	push af
	push bc
	push de
	push hl

	ld a, " "
	ld c, \2
	ld hl, \1
.forEach\@
	ld [hli], a
	dec c
	jr nz, .forEach\@

	pop hl
	pop de
	pop bc
	pop af
	ENDM

MACRO DEBUGPRINT_MEM16 ; position, memory address
	push af
	push bc
	push de
	push hl

	ld a, [\2 + 1]
	ld d, a
	ld a, [\2]
	ld e, a
	ld hl, \1
	call PrintHex_DE

	pop hl
	pop de
	pop bc
	pop af
	ENDM

MACRO DEBUGPRINT_MEM32 ; position, memory address
	push af
	push bc
	push de
	push hl

	ld a, [\2 + 3]
	ld b, a
	ld a, [\2 + 2]
	ld c, a
	ld a, [\2 + 1]
	ld d, a
	ld a, [\2]
	ld e, a
	ld hl, \1
	call PrintHex_DE

	pop hl
	pop de
	pop bc
	pop af
	ENDM


MACRO DEBUGPRINT_REGISTER_BYTE ; position, 8-bit register
	push af
	push bc
	push de
	push hl

	ld e, \2
	ld hl, \1
	call PrintHex_E

	pop hl
	pop de
	pop bc
	pop af
	ENDM

MACRO DEBUGPRINT_REGISTER ; position, 16-bit register
	push af
	push bc
	push de
	push hl

	push \2
	pop de
	ld hl, \1
	call PrintHex_DE

	pop hl
	pop de
	pop bc
	pop af
	ENDM

MACRO DEBUGPRINT_FLAGS ; position
	push af
	push bc
	push de
	push hl

	push af
	pop de
	ld hl, \1

	bit 7, e
	jr z, .notZ\@
	ld a, "Z"
	jr .endZ\@
.notZ\@
	ld a, "-"
.endZ\@
	ld [hl+], a

	bit 6, e
	jr z, .notN\@
	ld a, "N"
	jr .endN\@
.notN\@
	ld a, "-"
.endN\@
	ld [hl+], a

	bit 5, e
	jr z, .notH\@
	ld a, "H"
	jr .endH\@
.notH\@
	ld a, "-"
.endH\@
	ld [hl+], a

	bit 4, e
	jr z, .notC\@
	ld a, "C"
	jr .endC\@
.notC\@
	ld a, "-"
.endC\@
	ld [hl+], a

	pop hl
	pop de
	pop bc
	pop af
	ENDM
