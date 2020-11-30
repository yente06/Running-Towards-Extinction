IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT


SCRWIDTH EQU 640	; Pixels scherm breedte
SCRHEIGHT EQU 480   ;Pixels scherm hoogte
VMEMADR EQU 0A0000h
SPRITESIZE EQU 32*4+2+2

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
mov EBX, EAX				;pointer naar sprite adres
add EBX, 4					;Skip de lijn met de groottes
mov EDI, [@@height]
;mov EDI, [EDI]      ; beacause edi is a pointer to a height
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

; opens file, returns file handle in ax
PROC openFile
ARG @@file:dword
	USES eax, ebx, ecx, edx
	mov al, 2 ; read only
	mov edx, [@@file]
	mov ah, 3dh
	int 21h

	jnc @@no_error ; carry flag is set if error occurs

	call setVideoMode, 03h
	mov  ah, 09h
	mov  edx, offset openErrorMsg
	int  21h

	mov	ah,00h
	int	16h
	call terminateProcess

@@no_error:
	mov [filehandle], ax
	ret
ENDP openFile

; closes file
PROC closeFile
	USES eax, ebx, ecx, edx
	mov bx, [filehandle]
	mov ah, 3Eh
	int 21h

	jnc @@no_error ; carry flag is set if error occurs

	call setVideoMode, 03h
	mov  ah, 09h
	mov  edx, offset closeErrorMsg
	int  21h

	mov	ah,00h
	int	16h
	call terminateProcess

@@no_error:
	ret
ENDP closeFile

; reads chunk to buffer
PROC readChunk
ARG @@var:dword
	USES eax, ebx, ecx, edx
	mov bx, [filehandle]
	mov cx, SPRITESIZE
	mov edx, [@@var]
	mov ah, 3fh
	int 21h

	jnc @@no_error ; carry flag is set if error occurs

	call setVideoMode, 03h
	mov  ah, 09h
	mov  edx, offset readErrorMsg
	int  21h

	mov	ah,00h
	int	16h
	call terminateProcess

@@no_error:
	ret
ENDP readChunk

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

PROC updateCrouching
ARG @@player:dword
USES eax
mov ah,01h		; wait for keystroke
int 16h
jz @@checkToUncrouch
mov ah, 00h
cmp al, 08
jne @@gotoEnd
cmp [player.crouching], 0 ; was niet aan het bukken
je @@changeTocrouching
cmp [player.crouching], 1 ; was nog al aan het bukken
je @@checkToUncrouch

@@changeTocrouching:
mov [player.crouching], 1
mov [player.sprite], offset TrexCrouching
jmp @@gotoEnd

@@checkToUncrouch:
cmp [player.crouching], 1 ; check if it is crouching
jne @@gotoEnd
cmp [player.crouchTime], 15
je @@changeToUncrouch    ;check if it needs to stop crouching
mov eax, [player.crouchTime]
add eax, 1
mov [player.crouchTime], eax
jmp @@gotoEnd

@@changeToUncrouch:
mov [player.crouching], 0
mov [player.crouchTime], 0
mov [player.sprite], offset Trex

@@gotoEnd:
ret
ENDP updateCrouching

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
mov [player.y], EDX ; Updating the player height
@@end:
ret
ENDP updateJump

PROC drawPlayer
ARG @@player:dword
USES EAX
mov EAX, [@@player]
call drawSprite, [EAX + Player.sprite], [EAX + Player.y], [EAX + Player.x]
ret
ENDP drawPlayer

PROC drawEnemy
ARG @@enemy:dword
USES EAX
mov EAX, [@@enemy]
call drawSprite, [EAX + Enemy.sprite], [EAX + Enemy.y], [EAX + Enemy.x]
ret
ENDP drawEnemy

PROC updateEnemies ; Moves the enemies left and draws them if their x is not 0
ARG @@rarity:dword ; Rarity of spawning a new enemy
USES EAX, EBX, ECX, EDX
xor EAX, EAX ; We use EAX to detect if we already printed 1

mov ECX, [enemiesLen] ; Amount of enemies
mov EDX, offset enemies

@@update:

cmp [EDX + Enemy.x], 0
je @@enemyNotUsed
; Move the enemy
dec [EDX + Enemy.x]
call drawEnemy, EDX
; Check for collision
mov EBX, [player.x]
cmp [EDX + Enemy.x], EBX
jne @@skip
; The enemy and the player have the same x coordinate
; Check if high enough
mov EBX, [player.y]
cmp EBX, [EDX + Enemy.top]
jl @@skip ; high enough
;dec [player.lives]
;jmp @@skip

; Check if low enoughg
@@checkBottom:
mov EBX, [player.y]
sub EBX, [player.heightOffset]
cmp EBX, [EDX + Enemy.bottom]
jg @@skip ; The player is low enough
dec [player.lives]
jmp @@skip
@@enemyNotUsed:
; This enemy is not used (off the screen)
mov EBX, [EDX + Enemy.score]
add [player.score], EBX
mov [EDX + Enemy.score], 0 ; So the score isn't added the next time if the object is not reused as another enemy
cmp EAX, 0
jne @@skip
mov EAX, 1
call decideToSpawnEnemy, EDX, [@@rarity]

@@skip:
add EDX, 24 ; Size of the Enemy struct
loop @@update
ret
ENDP updateEnemies

PROC decideToSpawnEnemy ; Generates 2 random numbers, 1 to decide if it want's to spawn an enemy and 1 to decide what enemy
ARG @@enemy:dword, @@chance:dword ; Chance: 20 = 1 in 20
USES EAX, EDX
call generateRandomNumber

mov EAX, 4294967295 ; Max value of generateRandomNumber
xor EDX, EDX  ; Set EDX to zero before division
div [@@chance]
cmp [RandomState], EAX
ja @@skip ; above for unsigned integers

; The random number was within out chance margin
mov EAX, [@@enemy]
mov [EAX + Enemy.x], 76 ; Begin point
; Randomly choosing from 3 sprites
call generateRandomNumber
cmp [RandomState], 1431655765
jae @@sprite2 ; Above or equal for unsigned integers
; Set sprite 1
mov [EAX + Enemy.sprite], offset SmallCactus
mov [EAX + Enemy.y], 46
mov [EAX + Enemy.top], 43
mov [EAX + Enemy.bottom], 46
mov [EAX + Enemy.score], 50
jmp @@skip
@@sprite2:
cmp [RandomState], 2863311530
jae @@sprite3 ; Above or equal for unsigned integers
; Set sprite 2
mov [EAX + Enemy.sprite], offset LargeCactus
mov [EAX + Enemy.y], 46
mov [EAX + Enemy.top], 43
mov [EAX + Enemy.bottom], 46
mov [EAX + Enemy.score], 100
jmp @@skip
@@sprite3:
; Set sprite 3
mov [EAX + Enemy.sprite], offset Pterodactyl
mov [EAX + Enemy.y], 40
mov [EAX + Enemy.top], 38
mov [EAX + Enemy.bottom], 40
mov [EAX + Enemy.score], 200
@@skip:
ret
ENDP decideToSpawnEnemy

PROC waitForFrame
USES EAX, EDX
MOV  DX, 03DAH      ; VGA status port
@@waitForEnd:
IN  AL, DX
AND AL, 8           ; third bit is set during VBI
JNZ @@waitForEnd
@@waitForBegin:
IN  AL, DX
AND AL, 8
JZ  @@waitForBegin
ret
ENDP waitForFrame

PROC displayScore
USES EAX, EDX
call printUnsignedInteger, [player.score]
mov ah, 09h
mov edx, offset NewLine
int 21h
ret
ENDP displayScore


PROC main
	sti
	cld
	;call generateRandomNumber
	push ds
	call setVideoMode, 12h
	pop es

	; laadt alle sprites

	;Trex
	call openFile, offset TrexFile
	call readChunk, offset Trex
	call closeFile
	mov [player.sprite], offset Trex

	;TrexCrouching
	call openFile, offset TrexCrouchingFile
	call readChunk, offset TrexCrouching
	call closeFile

	;SmallCactus
	call openFile, offset SmallCactusFile
	call readChunk, offset SmallCactus
	call closeFile

	;LargeCactus
	call openFile, offset LargeCactusFile
	call readChunk, offset LargeCactus
	call closeFile

	;Pterodactyl
	call openFile, offset PterodactylFile
	call readChunk, offset Pterodactyl
	call closeFile

	; Start het spel met alle sprites die altijd op het scherm staan al te tekenen
	call drawFloor, offset Floor, offset SizeFloor, 50
	call drawPlayer, offset player

  ; Start de game loop
	gameLoop:
		call updateCrouching, offset player
		call updateJump, 4
		push ds
		call setVideoMode, 12h
		pop es
		call drawFloor, offset Floor, offset SizeFloor, 50
		call updateEnemies, 50
		call drawPlayer, offset player

		cmp [player.lives], 0 ; Stop the game if the player has no lives left
		je gameOver

		;Wait for the current frame to be drawn
		call waitForFrame

		mov ah,01h		; wait for keystroke
		int 16h
		cmp al, 27    ; Check if it is 'escape'
		je terminate	; Terminate if it is 'escape'
	jmp gameLoop

	gameOver:
	call displayScore
	mov ah,0h		; wait for keystroke
	int 16h
	terminate:
	call terminateProcess

ENDP main

DATASEG
; Drawing sprites
PterodactylFile db "ptero.bin", 0
TrexCrouchingFile db "trcrouch.bin", 0
TrexFile db "trex.bin", 0
SmallCactusFile db "smallcac.bin", 0
LargeCactusFile db "largecac.bin", 0
openErrorMsg db "could not open file", 13, 10, '$'
readErrorMsg db "could not read data", 13, 10, '$'
closeErrorMsg db "error during file closing", 13, 10, '$'


SizeFloor DW 80
Floor DB 0ffH
;Random Generation
RandomState DD 0; 1957386613 Binary: 111 0100 1010 1011 0101 1001 0111 0101
NewLine db ' ', 13, 10, '$' ; 13, 10: newline, $: eindigd interrupt
;Jumping
PlayerGroundHeight DD 46
JumpState DD 0

struc Player
	x   dd 5  	  ; X position
	y   dd 46     ; Y position
	sprite dd ?   ; Pointer to the sprite
	heightOffset dd 2 ; Amount of blank space above the player sprite
	score dd 0
	crouching dd 0
	crouchTime dd 0
	lives dd 1
ends Player

player Player <>

struc Enemy
	x   dd 0  	 ; X position
	y   dd ?     ; Y position
	sprite dd ?   ; Pointer to the sprite
	top  dd ?    ; How high you need to be to dodge
	bottom dd ?  ; How low you need to be to dodge
	score  dd ?  ; Score to gain when jumped
ends Enemy

enemiesLen dd 4
enemies Enemy 4 DUP(<>)

UDATASEG
filehandle dw ?

Pterodactyl DW ?, ?
		 				DB 128 DUP(?)
Trex DW ?, ?
		 DB 128 DUP(?)

TrexCrouching DW ?,?
							DB 128 DUP(?)

SmallCactus DW ?, ?
		 				DB 128 DUP(?)

LargeCactus	DW ?, ?
		 				DB 128 DUP(?)

STACK 100h

END main
