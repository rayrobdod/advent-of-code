; For all of these,
; hl - [in] the start of where to print to [out] the end of the print

SECTION "PrintHex_BCDE", ROM0
; hl - the start of where to print to
; bcde - the value to print
PrintHex_BCDE::
	ASSERT(Digits & $0F == 0)
	push bc
	push de
	ld d, b
	ld e, c

	call PrintHex_DE
	pop de
	call PrintHex_DE

	pop bc
	ret

SECTION "PrintHex_DE", ROM0
; hl - the start of where to print to
; de - value to print
PrintHex_DE::
	ASSERT(Digits & $0F == 0)
	push bc
	ld b, HIGH(Digits)

	REPT 3
	inc hl
	ENDR

	ld a, e
	and a, $0F
	or a, LOW(Digits)
	ld c, a
	ld a, [bc]
	ld [hl-], a

	ld a, e
	and a, $F0
	swap a
	or a, LOW(Digits)
	ld c, a
	ld a, [bc]
	ld [hl-], a

	ld a, d
	and a, $0F
	or a, LOW(Digits)
	ld c, a
	ld a, [bc]
	ld [hl-], a

	ld a, d
	and a, $F0
	swap a
	or a, LOW(Digits)
	ld c, a
	ld a, [bc]
	ld [hl], a

	REPT 4
	inc hl
	ENDR

	pop bc
	ret

SECTION "PrintHex_E", ROM0
; hl - the start of where to print to
; e - value to print
PrintHex_E::
	ASSERT(Digits & $0F == 0)
	push bc
	ld b, HIGH(Digits)

	ld a, e
	and a, $F0
	jr c, .leftIsZero
	swap a
	or a, LOW(Digits)
	ld c, a
	ld a, [bc]
	ld [hl+], a
	jr .leftEnd
.leftIsZero
	ld a, " "
	ld [hl+], a
.leftEnd

	ld a, e
	and a, $0F
	or a, LOW(Digits)
	ld c, a
	ld a, [bc]
	ld [hl+], a

	pop bc
	ret

Section "Digits", ROM0, ALIGN[4]
Digits:
	DB "0123456789ABCDEF"
