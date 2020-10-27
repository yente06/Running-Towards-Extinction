IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT


SCRWIDTH EQU 320	; Pixels scherm breedte
SCRHEIGHT EQU 200   ;Pixels scherm hoogte
VMEMADR EQU 0A0000h

CODESEG
PROC setVideoMode
ARG @@mode:word
USES eax
mov AX, [@@mode]         ;Kies video mode
int 10h
ret
ENDP setVideoMode

PROC terminateProcess
	USES eax
	call setVideoMode, 03h
	mov	ax,04C00h
	int 21h
	ret
ENDP terminateProcess

PROC drawTrex
ARG @@width:word, @@array:dword
USES EDI, EBX,ecx, eax, esi
mov ECX, 9	;Breedte
mov EBX, [@@array]	;pointer naar gegeugen adres
mov EDI, VMEMADR

@@govertical: 		;gaat door alle verticale vakjes
mov EAX, [EBX]		;Zet waarde in eax
mov esi, ecx
mov edx, eax
;================================================================
;Horizontaal
;================================================================
@@gohorizontal: 	;gaat door alle horizontale vakjes
mov ecx, 8
@@loopdrawhorizontal:;teken op het scherm
shl edx, 1          ;bitshift naar links voor volgende getal
and edx, 80h
cmp edx, 80h
mov AL, 07
mov [EDI], AL       ;Teken op het scherm

cmp edx,0
jz @@nextblock
add edi, 1          ;Verhoog edi met 1
loop @@loopdrawhorizontal

@@nextblock:		;Volgende horizontale block
add edi, 8
mov eax, [ebx]
loop @@gohorizontal
;====================================================================
;Vertikaal
;====================================================================
mov ecx, esi
mov edi, esi
add edi, 464		; Ga 1 lijnen naar beneden voor volgend vakje
loop @@govertical
ret
ENDP drawTrex


PROC main
	sti
	cld

	call setVideoMode, 12h
    call drawTrex, offset size, offset logo

    mov	ah,00h
	int	16h

ENDP main

DATASEG
size DW  8, 8
logo DB 0FFH, 00H
	 DB 00H, 00H
	 DB 00H, 00H
	 DB 00H, 00H
	 DB 00H, 00H
	 DB 00H, 00H
	 DB 00H, 00H
	 DB 00H, 0FFH

STACK 100h

END main
