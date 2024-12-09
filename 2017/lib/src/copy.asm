; hl - [in] pointer to target - [out] hl + bc
; de - [in] pointer to source - [out] de + bc
; bc - [in] number of bytes to copy - [out] 0

SECTION "Copy", ROM0, ALIGN[3]
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
