IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT


SCRWIDTH EQU 320	; Pixels scherm breedte
SCRHEIGHT EQU 200   ;Pixels scherm hoogte
VMEMADR EQU 0A0000h

CODESEG
;##FOR DEBUGGING##
PROC printUnsignedInteger
	ARG @@printval:dword
	USES eax, ebx, ecx, edx
	sti  ; Set the interrupt flag => enable interrupts.
    cld  ; Clear the direction flag => auto-increment source
          ; and destination indices.

	mov	eax, [@@printval]  ; eax holds input integer
	mov	ebx, 10		; divider
	xor ecx, ecx	; counter for digits to be printed

	; Store digits on stack
@@getNextDigit:
	inc	ecx         ; increase digit counter
	xor edx, edx
	div	ebx   		; divide by 10
	push dx			; store remainder on stack
	test eax, eax	; {cmp eax, 0} check whether zero?
	jnz	@@getNextDigit

    ; Write all digits to the standard output
	mov	ah, 2h 		; Function for printing single characters.
@@printDigits:
	pop dx
	add	dl,'0'      	; Add 30h => code for a digit in the ASCII table, ...
	int	21h            	; Print the digit to the screen, ...
	loop @@printDigits	; Until digit counter = 0.

	ret
ENDP printUnsignedInteger
;##FOR DEBUGGING##


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

PROC generateRandomNumber ; Het gegenereerde getal komt terecht in [RandomState], gebaseerd op https://en.wikipedia.org/wiki/Xorshift, min val: 0, max val: 4294967295
USES EAX, EDX, ECX
; Systeem tijd als random start punt
cmp [RandomState], 0
jne @@SkipSeedSetup
MOV AH, 2Ch    ; Systeem tijd opvragen
INT 21H
XOR ECX, EDX   ; Nemen xor van de 2 delen van de tijd
mov [RandomState], ECX
@@SkipSeedSetup:
mov EAX, [RandomState]
mov EDX, [RandomState]
SHL EDX, 13							; Left shift met 13
XOR EAX, EDX						; Sla de xor van EAX en EDX op in EAX
mov EDX, EAX
SHR EDX, 17							; Right shift met 17
XOR EAX, EDX						; Sla de xor van EAX en EDX op in EAX
mov EDX, EAX
SHL EDX, 5							; Left shift met 5
XOR EAX, EDX						; Sla de xor van EAX en EDX op in EAX
mov [RandomState], EAX
;DEBUGGING
;call printUnsignedInteger, [RandomState]
;mov ah, 09h ;lets int 21h know it needs to print a string
;mov edx, offset NewLine
;int 21h ; interrupts to print string pointed to by edx
;;
ret
ENDP generateRandomNumber


PROC main
	sti
	cld
	;call generateRandomNumber
	;call setVideoMode, 12h
  ;call drawTrex, offset Trex, offset Size
	;call drawTrex, offset Floor, offset SizeFloor

	mov ah,0h		; wait for keystroke
	int 16h
	mov	ax,4C00h 	; terminate
	int 21h

ENDP main

DATASEG
; Drawing sprites
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

;SizeFloor DW 2, 80
;Floor DB 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH
;			DB 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH
;Random Generation
RandomState DD 0; 1957386613 Binary: 111 0100 1010 1011 0101 1001 0111 0101
NewLine db ' ', 13, 10, '$' ; 13, 10: newline, $: eindigd interrupt

STACK 100h

END main
