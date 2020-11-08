IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT


SCRWIDTH EQU 640	; Pixels scherm breedte
SCRHEIGHT EQU 480   ;Pixels scherm hoogte
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

PROC drawFloor
ARG @@symb:dword, @@width:dword, @@heightOffset:dword
USES EDI, EBX, EAX, ECX
mov EDI, [@@heightOffset] ; Voegen heightOffset toe
imul EDI, SCRWIDTH				; HeightOffset maal SCRWIDTH om naar beneden te gaan
add EDI, VMEMADR	;Geheugen adres
mov EBX, [@@symb]	;adres Hex waarde
mov eax, [EBX]		; Hex waarde
mov EBX, [@@width]
mov CX, [EBX]				;grote
; Nu zit in AL: waarde en BX: Grootte

;REP STOSB

fillingMemory:
cmp CX, 0
je done
mov [EDI], EAX
add EDI, 1
sub CX, 1
jmp fillingMemory
done:
ret
ENDP drawFloor

PROC drawSprite
ARG @@array:dword, @@Size:dword, @@height:dword, @@offset:dword
USES EDI, EBX, ECX, ESI, EDX
mov EAX, [@@Size]
mov CX, [EAX]				;Aantal verikale loops
mov EBX, [@@array]	;pointer naar gegeugen adres
xor EDI, EDI
mov EDI, [@@height]
mov EDI, [EDI]      ; beacause edi is a pointer to a height
imul EDI, SCRWIDTH
add EDI, VMEMADR
add EDI, [@@offset]

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
ENDP drawSprite

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

PROC updateJump
ARG @@speed:dword
USES EAX, EDX, ECX, EBX
xor EAX, EAX
mov AX, [JumpState]
cmp AX, 0     ; If the jump state is not null, aka we are already jumping
jne @@update  ; skip button check if already in the air
mov ah,01h		; wait for keystroke
int 16h
jz @@end      ; Skip if there is no keyboard input
mov ah, 00h
int 16h       ; Get the key and clear the keyboard buffer
cmp al, 32    ; Check if it is space
jne @@end			; Skip if it's not space

@@update:
xor EAX, EAX  ; Set EAX to zero before division
xor EDX, EDX  ; Set EDX to zero before division
inc [JumpState]
mov AX, [JumpState]
mov ECX, [@@speed]
idiv ECX
; EAX is the round division, use it as X in (x-2)^(2)-4
cmp EAX, 4    ; Y=0 on X=4, so this is the end of the jump
jne @@skipReset
; If the jump is done, we reset the jump state
mov [JumpState], 0
@@skipReset:
sub EAX, 2    ; (x-2)
imul EAX, EAX ; (x-2)^(2)
sub EAX, 4    ; (x-2)^(2)-4
; EAX now contains a negative number that we add to the current player height
xor EDX, EDX  ; Reset EDX beacause the remainder was stored here
mov DX, [PlayerGroundHeight]
add EDX, EAX
mov [PlayerHeight], DX ; Updating the player height
@@end:
ret
ENDP updateJump


PROC main
	sti
	cld
	;call generateRandomNumber
	call setVideoMode, 12h ; Reset the screen
	call drawFloor, offset Floor, offset SizeFloor, 50
	gameLoop:
		call updateJump, 200
		call drawSprite, offset Trex, offset Size, offset PlayerGroundHeight, 5
		; For testing
		;mov ah,0h		; wait for keystroke
		;int 16h
		;cmp al, 27    ; Check if it is escape
		;je terminate
	jmp gameLoop

	mov ah,0h		; wait for keystroke
	int 16h
	terminate:
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

SizeFloor DW 80
Floor DB 0ffH
;Random Generation
RandomState DD 0; 1957386613 Binary: 111 0100 1010 1011 0101 1001 0111 0101
NewLine db ' ', 13, 10, '$' ; 13, 10: newline, $: eindigd interrupt
;Jumping
PlayerGroundHeight DW 46
PlayerHeight DW 46
JumpState DW 0

STACK 100h

END main
