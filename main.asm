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
ARG @@array:dword, @@Size:dword
USES EDI, EBX, ECX, ESI, EDX
mov EAX, [@@Size]
mov CX, [EAX]				;Aantal verikale loops
mov EBX, [@@array]	;pointer naar gegeugen adres
mov EDI, VMEMADR+640*40+5		;Begin video geheugen

@@govertical:
mov EDX, ECX				;Hou ctr bij
mov CX, [EAX+2]			;Aantal hex horizontaal
mov ESI, EDI				;Hou EDI bij

@@gohorizontal:
mov AL, [EBX]				;Zet hex in AL
mov [EDI], AL				;Zet AL in videogeheugen
add edi, 1					;Volgende reeks in videogeheugen
add ebx, 1					;Volgende hex
loop @@gohorizontal

mov EAX, [@@Size]		;Zet ctr voor inner loop terug
mov ECX, EDX				;Zet ctr terug
mov EDI, ESI				;Zet EDI terug
add EDI, 80				;Volgende rij
loop @@govertical
ret
ENDP drawTrex


PROC main
	sti
	cld

	call setVideoMode, 12h
    call drawTrex, offset Trex, offset Size

    mov	ah,00h
	int	16h

ENDP main

DATASEG
Size DW  32, 4
Trex DB 00H, 00H, 00H, 00H
	 	 DB 00H, 00H, 03FH, 0FCH
	 	 DB 00H, 00H, 03FH, 0FCH
	 	 DB 00H, 00H, 0F3H, 0FFH
	 	 DB 00H, 00H, 0F3H, 0FFH
	   DB 00H, 00H, 0FFH, 0FFH
		 DB 00H, 00H, 0FFH, 0FFH
	 	 DB 00H, 00H, 0FFH, 00H
		 DB 00H, 00H, 0FFH, 00H
		 DB 00H, 00H, 0FFH, 0F0H
		 DB 00H, 00H, 0FFH, 0F0H
		 DB 0CH, 03H, 0FCH, 00H
		 DB 0CH, 03H, 0FCH, 00H
		 DB 0FH, 0FH, 0FFH, 0C0H
		 DB 0FH, 0FH, 0FFH, 0C0H
		 DB 0FH, 0FFH, 0FCH, 0C0H
		 DB 0FH, 0FFH, 0FCH, 0C0H
		 DB 0FH, 0FFH, 0FCH, 00H
		 DB 0FH, 0FFH, 0FCH, 00H
		 DB 03H, 0FFH, 0FCH, 00H
		 DB 03H, 0FFH, 0FCH, 00H
		 DB 00H, 0FFH, 0F0H, 00H
		 DB 00H, 03FH, 0F0H, 00H
		 DB 00H, 03FH, 0C0H, 00H
		 DB 00H, 03FH, 0C0H, 00H
		 DB 00H, 03CH, 0C0H, 00H
		 DB 00H, 03CH, 0C0H, 00H
		 DB 00H, 030H, 0C0H, 00H
		 DB 00H, 030H, 0C0H, 00H
		 DB 00H, 03CH, 0F0H, 00H
		 DB 00H, 03CH, 0F0H, 00H
		 DB 00H, 00H, 00H, 00H

STACK 100h

END main
