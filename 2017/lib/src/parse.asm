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
