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
add EDI, VMEMADR					;Geheugen adres
mov EBX, [@@symb]					;adres Hex waarde
mov eax, [EBX]						; Hex waarde
mov EBX, [@@width]
mov CX, [EBX]							;grote
; Nu zit in AL: waarde en BX: Grootte
REP STOSB
ret
ENDP drawFloor

PROC drawSprite
ARG @@array:dword, @@height:dword, @@offset:dword
USES EAX, EDI, EBX, ECX, ESI, EDX
mov EAX, [@@array]
mov CX, [EAX]				;Aantal verikale loops
mov EBX, EAX				;pointer naar gegeugen adres
add EBX, 4					;Skip de lijn met de groottes
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

mov EAX, [@@array]		;Zet ctr voor inner loop terug
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
mov ah,01h		; wait for keystroke
int 16h
jz @@checkIfInAir      ; Skip if there is no keyboard input
mov ah, 00h
int 16h       ; Get the key and clear the keyboard buffer
cmp al, 32    ; Check if it is space
je @@update			; Skip if it's not space

@@checkIfInAir:
mov EAX, [JumpState]
cmp EAX, 0     ; If the jump state is 0, we are not jumping
je @@end

@@update:
inc [JumpState]
mov EAX, [JumpState] ; Jump state is X of function
; EAX is the round division, use it as X in (x/[@@speed]-3)^(2)-9
mov ECX, 6
imul ECX, [@@speed] ; Y=0 on X=6*[@@speed]
cmp EAX, ECX    ; Y=0 on X=6*[@@speed], so this is the end of the jump
jne @@skipReset
; If the jump is done, we reset the jump state
mov [JumpState], 0
@@skipReset:
xor EDX, EDX  ; Set EDX to zero before division
idiv [@@speed]; (x/[@@speed])
sub EAX, 3    ; (x/[@@speed]-3)
imul EAX, EAX ; (x/[@@speed]-3)^(2)
sub EAX, 9    ; (x/[@@speed]-3)^(2)-9
; EAX now contains a negative number that we add to the current player height
mov EDX, [PlayerGroundHeight]
add EDX, EAX
mov [PlayerHeight], EDX ; Updating the player height
@@end:
ret
ENDP updateJump


PROC main
	sti
	cld
	;call generateRandomNumber
	push ds
	call setVideoMode, 12h
	pop es
	; Draw the floor and T-REX once
	call drawFloor, offset Floor, offset SizeFloor, 50
	call drawSprite, offset Trex, offset PlayerHeight, 5
	call drawSprite, offset SmallCactus, offset CactusHeight, 70
	call drawSprite, offset LargeCactus, offset CactusHeight, 67
	call drawSprite, offset Pterodactyl, offset PterodactylHeight, 67

	gameLoop:
		mov EAX, [PlayerHeight]
		call updateJump, 25000
		cmp EAX, [PlayerHeight]
		je @@skipScreenUpdate
		; Update the screen only when necessary
		push ds
		call setVideoMode, 12h
		pop es
		call drawFloor, offset Floor, offset SizeFloor, 50
		call drawSprite, offset Trex, offset PlayerHeight, 5
		call drawSprite, offset SmallCactus,offset CactusHeight, 70
		call drawSprite, offset LargeCactus, offset CactusHeight, 67
		call drawSprite, offset Pterodactyl, offset PterodactylHeight, 67
		@@skipScreenUpdate:

		; For testing
		mov ah,01h		; wait for keystroke
		int 16h
		cmp al, 27    ; Check if it is 'escape'
		je terminate	; Terminate if it is 'escape'
	jmp gameLoop

	mov ah,0h		; wait for keystroke
	int 16h
	terminate:
	call terminateProcess

ENDP main

DATASEG
; Drawing sprites
Trex 	DW  32, 4
			DB 00H, 00H, 00H, 00H
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

SmallCactus DW  32, 4
						DB 00H, 00H, 00H, 00H
	 	 			 	DB 00H, 00H, 00H, 00H
	 	 	 		 	DB 00H, 00H, 00H, 00H
	 	 	 		 	DB 00H, 00H, 00H, 00H
	 	 	 		 	DB 00H, 00H, 00H, 00H
	   	 		 	DB 00H, 00H, 00H, 00H
		 		 	 	DB 00H, 00H, 00H, 00H
	 	 	 		 	DB 00H, 00H, 00H, 00H
		 		 	 	DB 00H, 00H, 00H, 00H
		 		 	 	DB 00H, 00H, 00H, 00H
		 		 	 	DB 00H, 03H, 0C0H, 00H
		 		 	 	DB 00H, 03H, 0C0H, 00H
		 		 	 	DB 00H, 03H, 0CCH, 00H
		 		 	 	DB 00H, 03H, 0CCH, 00H
		 		 	 	DB 00H, 03H, 0CCH, 00H
		 		 	 	DB 00H, 03H, 0CCH, 00H
		 		 	 	DB 00H, 033H, 0CCH, 00H
		 		 	 	DB 00H, 033H, 0CCH, 00H
		 		 	 	DB 00H, 033H, 0FCH, 00H
		 		 	 	DB 00H, 033H, 0FCH, 00H
		 		 	 	DB 00H, 033H, 0C0H, 00H
		 		 	 	DB 00H, 033H, 0C0H, 00H
		 		 	 	DB 00H, 3FH, 0C0H, 00H
		 		 	 	DB 00H, 3FH, 0C0H, 00H
		 		 	 	DB 00H, 03H, 0C0H, 00H
				 	 	DB 00H, 03H, 0C0H, 00H
				 	 	DB 00H, 03H, 0C0H, 00H
				 	 	DB 00H, 03H, 0C0H, 00H
				 	 	DB 00H, 03H, 0C0H, 00H
				 	 	DB 00H, 03H, 0C0H, 00H
				 	 	DB 00H, 03H, 0C0H, 00H
				 	 	DB 00H, 03H, 0C0H, 00H

LargeCactus DW  32, 4
						DB 00H, 00H, 00H, 00H
					 	DB 00H, 00H, 00H, 00H
					 	DB 00H, 00H, 00H, 00H
					 	DB 00H, 00H, 00H, 00H
					 	DB 00H, 03H, 0C0H, 00H
					 	DB 00H, 03H, 0C0H, 00H
					 	DB 00H, 03H, 0C0H, 00H
					 	DB 00H, 03H, 0C0H, 00H
					 	DB 00H, 03H, 0C0H, 00H
					 	DB 00H, 03H, 0C0H, 00H
					 	DB 00H, 0F3H, 0CFH, 00H
					 	DB 00H, 0F3H, 0CFH, 00H
					 	DB 00H, 0F3H, 0CFH, 00H
					 	DB 00H, 0F3H, 0CFH, 00H
					 	DB 00H, 0F3H, 0CFH, 00H
					 	DB 00H, 0F3H, 0CFH, 00H
					 	DB 00H, 0F3H, 0CFH, 00H
					 	DB 00H, 0F3H, 0CFH, 00H
					 	DB 00H, 0FFH, 0FFH, 00H
					 	DB 00H, 0FFH, 0FFH, 00H
					 	DB 00H, 03FH, 0FCH, 00H
					 	DB 00H, 03FH, 0FCH, 00H
					 	DB 00H, 03H, 0C0H, 00H
					 	DB 00H, 03H, 0C0H, 00H
					 	DB 00H, 03H, 0C0H, 00H
					 	DB 00H, 03H, 0C0H, 00H
					 	DB 00H, 03H, 0C0H, 00H
					 	DB 00H, 03H, 0C0H, 00H
					 	DB 00H, 03H, 0C0H, 00H
					 	DB 00H, 03H, 0C0H, 00H
					 	DB 00H, 03H, 0C0H, 00H
					 	DB 00H, 03H, 0C0H, 00H

Pterodactyl DW  32, 4
						DB 00H, 00H, 00H, 00H
						DB 00H, 00H, 00H, 00H
						DB 00H, 00H, 00H, 00H
						DB 00H, 00H, 00H, 00H
						DB 00H, 00H, 00H, 00H
						DB 00H, 00H, 00H, 00H
						DB 00H, 00H, 00H, 00H
						DB 00H, 00H, 00H, 00H
						DB 00H, 00H, 00H, 00H
						DB 00H, 00H, 00H, 00H
						DB 00H, 00H, 00H, 00H
						DB 00H, 00H, 00H, 00H
						DB 00H, 020H, 00H, 00H
						DB 00H, 030H, 00H, 00H
						DB 00H, 038H, 00H, 00H
						DB 00H, 03CH, 00H, 00H
						DB 00H, 03EH, 00H, 00H
						DB 00H, 03FH, 00H, 00H
						DB 03H, 01FH, 080H, 00H
						DB 07H, 01FH, 0C0H, 00H
						DB 0FH, 09FH, 0E0H, 00H
						DB 01FH, 0DFH, 0F0H, 00H
						DB 03FH, 0DFH, 0F8H, 00H
						DB 07FH, 0FFH, 0F8H, 00H
						DB 07FH, 0FFH, 0FCH, 00H
						DB 00H, 07FH, 0FFH, 0F8H
						DB 00H, 03FH, 0FFH, 0FCH
						DB 00H, 01FH, 0FFH, 080H
						DB 00H, 0FH, 0FFH, 0F8H
						DB 00H, 07H, 0FFH, 0E0H
						DB 00H, 03H, 0FFH, 0C0H
						DB 00H, 00H, 00H, 00H
;Pterodactyl DB "Sprites/SizePterodactyl.bin", 0


SizeFloor DW 80
Floor DB 0ffH
;Random Generation
RandomState DD 0; 1957386613 Binary: 111 0100 1010 1011 0101 1001 0111 0101
NewLine db ' ', 13, 10, '$' ; 13, 10: newline, $: eindigd interrupt
;Jumping
PlayerGroundHeight DD 46
PlayerHeight DD 46
CactusHeight DD 46
PterodactylHeight DD 42
JumpState DD 0

STACK 100h

END main
