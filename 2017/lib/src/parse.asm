SECTION "Parse_BCD16_BC", ROM0

; hl [in] start of string [out] end of string
; bc [out] parsed number
Parse_BCD16_BC::
	ld bc, 0
.forEachDigit
	ld a, [hl+]
	cp a, "0"
	jp c, .done
	cp a, "9" + 1
	jp nc, .done
	and a, $0F
	FOR N, 4
	sla c
	rl b
	ENDR
	or a, c
	ld c, a
	jp .forEachDigit
.done
	dec hl
	ret

SECTION "Parse_16_BC", ROM0

; hl [in] start of string [out] end of string
; bc [out] parsed number
Parse_16_BC::
	push de
	ld bc, 0
.forEachDigit
	ld a, [hl+]
	cp a, "0"
	jp c, .done
	cp a, "9" + 1
	jp nc, .done
	and a, $0F

	; bc = bc * 10
	push hl
	sla c
	rl b
	ld l, c
	ld h, b
	sla c
	rl b
	sla c
	rl b
	add hl, bc
	ld c, l
	ld b, h
	pop hl

	; bc += a
	add a, c
	ld c, a
	ld a, 0
	adc a, b
	ld b, a

	jp .forEachDigit
.done
	dec hl
	pop de
	ret
