SECTION "Compare_BC_DE", ROM0
; F - [out] Z iff bc == de, C iff bc < de
Compare_BC_DE::
	ld a, b
	cp a, d
	ret nz
	ld a, c
	cp a, e
	ret
