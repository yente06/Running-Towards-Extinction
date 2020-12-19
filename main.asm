IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT


SCRWIDTH EQU 640	; Pixels scherm breedte
SCRHEIGHT EQU 480   ;Pixels scherm hoogte
VMEMADR EQU 0A0000h
KEY_BUFFER	EQU 60h
SPRITEWIDTH EQU 4
SPRITESIZE EQU 32*SPRITEWIDTH+2+2
SCORESIZE EQU 4 ; DD has a size of 4 bytes

INCLUDE "keyb.inc"

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
cmp AL, 0           ;We don't display black pixels, otherwise there would be a black border around the sprite
je @@allBlack
mov [EDI], AL				;Zet AL in videogeheugen
@@allBlack:
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
ARG @@var:dword, @@size: dword
	USES eax, ebx, ecx, edx
	mov bx, [filehandle]
	mov ecx, [@@size]
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
ret
ENDP generateRandomNumber

PROC isKeyPressed				;Checks if the key is pressed
ARG @@keyHex: dword
USES EBX
mov EAX, [@@keyHex]
mov bl, [offset __keyb_keyboardState + EAX]
xor eax, eax
sub ax, bx
ret
ENDP isKeyPressed

PROC updateCrouching
ARG @@player:dword
USES eax, ebx, ecx
mov ecx, [player.crouching]
call isKeyPressed, 1DH

cmp eax, 0											;Uncrouch when key is not pressed
je @@unCrouch
cmp ecx, 1											;if 1, already crouching
je @@gotoEnd
cmp [JumpState], 0
jne @@gotoEnd ; If he's jumping he can't crouch
; We make him crouch
mov [player.crouching], 1				; Moet bukken
mov EBX, [player.crouchOffset]
mov [player.heightOffset], EBX
jmp @@gotoEnd

@@unCrouch:
cmp ecx, 0											;Release crouch, already standing up
je @@gotoEnd
mov [player.crouching], 0				;Mag niet bukken
mov EBX, [player.defaultOffset]
mov [player.heightOffset], EBX

@@gotoEnd:
ret
ENDP updateCrouching

PROC updateJump
ARG @@speed:dword
USES EAX, EDX, ECX, EBX

call isKeyPressed, 39h
cmp eax,0
jne @@update

@@checkIfInAir:
mov EAX, [JumpState]
cmp EAX, 0     ; If the jump state is 0, we are not jumping
je @@end
mov [player.crouching], 0

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
USES EAX, EBX
mov EAX, [@@player]
mov EBX, [EAX + Player.crouching]

cmp EBX, 1
je @@drawCrouch
call drawSprite, [EAX + Player.defaultSprite], [EAX + Player.y], [EAX + Player.x]		;draw sprite default
jmp @@end

@@drawCrouch:
call drawSprite, [EAX + Player.crouchSprite], [EAX + Player.y], [EAX + Player.x]		;draw crouch sprite

@@end:
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
add [scoreSinceChange], EBX ; Used for updating the difficulty
mov [EDX + Enemy.score], 0 ; So the score isn't added the next time if the object is not reused as another enemy
cmp EAX, 0
jne @@skip ; Already one call made
mov EAX, 1
call decideToSpawnEnemy, EDX, [@@rarity]

@@skip:
add EDX, 24 ; Size of the Enemy struct
loop @@update
ret
ENDP updateEnemies

PROC decideToSpawnEnemy ; Generates 2 random numbers, 1 to decide if it want's to spawn an enemy and 1 to decide what enemy
ARG @@enemy:dword, @@chance:dword ; Chance: 20 = 1 in 20
USES EAX, ECX, EDX
call generateRandomNumber
mov ECX, 1
mov EDX, [lastEnemySpawn]
cmp EDX, [minEnemyDistance]
jl @@checkAdjacent
mov [lastEnemySpawn], 0
jmp @@start
@@checkAdjacent:
mov EAX, SPRITEWIDTH
sub EAX, 2 ; We want the sprite to be a bit closer, because they have black bars
cmp EDX, EAX
jne @@notEnoughDistance
mov EDX, [lastEnemyPtery]
cmp EDX, 0
jne @@notEnoughDistance
inc [lastEnemySpawn]
mov ECX, 0 ; Rembere that they are adjacent


@@start:
xor EAX, EAX
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
mov [lastEnemyPtery], 0
; Set sprite 1
mov [EAX + Enemy.sprite], offset SmallCactus
mov [EAX + Enemy.y], 46
mov [EAX + Enemy.top], 43
mov [EAX + Enemy.bottom], 46
mov [EAX + Enemy.score], 50
jmp SHORT @@skip
@@sprite2:
cmp [RandomState], 2863311530
jae @@sprite3 ; Above or equal for unsigned integers
@@forceSprite2:
mov [lastEnemyPtery], 0
; Set sprite 2
mov [EAX + Enemy.sprite], offset LargeCactus
mov [EAX + Enemy.y], 46
mov [EAX + Enemy.top], 43
mov [EAX + Enemy.bottom], 46
mov [EAX + Enemy.score], 100
jmp @@skip
@@sprite3:
; We don't want to spawn a Pterodactyl adjacent to a cactus, so we spawn a large cactus instead
cmp ECX, 0
je @@forceSprite2
mov [lastEnemyPtery], 1
; Set sprite 3
mov [EAX + Enemy.sprite], offset Pterodactyl
mov [EAX + Enemy.y], 43
mov [EAX + Enemy.top], 41
mov [EAX + Enemy.bottom], 43
mov [EAX + Enemy.score], 200
jmp @@skip

@@notEnoughDistance:
inc [lastEnemySpawn]

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
USES EAX, EBX, EDX
mov ah, 09h
mov edx, offset scoreText
int 21h

call printUnsignedInteger, [player.score]
ret
ENDP displayScore

PROC displayHighScore
USES EAX, EBX, EDX
mov ah, 09h

mov edx, offset NewLine
int 21h

mov edx, offset highestScoreText
int 21h

mov ebx, [player.highscore]
cmp ebx, [player.score]
jge @@printHighScore ; Print highscore if the score is smaller than the highscore
call printUnsignedInteger, [player.score]
jmp @@stop
@@printHighScore:
call printUnsignedInteger, [player.highscore]
@@stop:
ret
ENDP displayHighScore

PROC saveHighScore
USES EAX, EBX, ECX, EDX
mov ebx, [player.highscore]
cmp ebx, [player.score]
jge @@skip ; Don't save if the score is smaller than the highscore
xor ebx, ebx
call openFile, offset HighestScoreFile
mov  ah, 40h
mov  bx, [filehandle]
mov  cx, SCORESIZE  ;STRING LENGTH.
mov  edx, offset player.score
int  21h
call closeFile
@@skip:
ret
ENDP saveHighScore

PROC updateDifficulty
USES EAX
mov EAX, [enemySpawnRate]
cmp EAX, 1
je @@skip
mov EAX, [scoreSinceChange]
cmp EAX, 2500
jl @@skip
mov [scoreSinceChange], 0
; If enemySpawnRate is equal to 1, minEnemyDistance will be equal to 15
sub [minEnemyDistance], 2
dec [enemySpawnRate]
@@skip:
ret
ENDP updateDifficulty


PROC main
	sti
	cld
	;call generateRandomNumber
	push ds
	call setVideoMode, 12h
	pop es

	call __keyb_installKeyboardHandler
	; laadt alle sprites

	;Trex
	call openFile, offset TrexFile
	call readChunk, offset Trex, SPRITESIZE
	call closeFile
	mov [player.defaultSprite], offset Trex

	;TrexCrouching
	call openFile, offset TrexCrouchingFile
	call readChunk, offset TrexCrouching, SPRITESIZE
	call closeFile
	mov [player.crouchSprite], offset TrexCrouching

	;SmallCactus
	call openFile, offset SmallCactusFile
	call readChunk, offset SmallCactus, SPRITESIZE
	call closeFile

	;LargeCactus
	call openFile, offset LargeCactusFile
	call readChunk, offset LargeCactus,SPRITESIZE
	call closeFile

	;Pterodactyl
	call openFile, offset PterodactylFile
	call readChunk, offset Pterodactyl, SPRITESIZE
	call closeFile

	;Highestscore
	call openFile, offset HighestScoreFile
	call readChunk, offset player.highscore, SCORESIZE
	call closeFile

	; Start het spel met alle sprites die altijd op het scherm staan al te tekenen
	call drawFloor, offset Floor, offset SizeFloor, 50
	call drawPlayer, offset player

  ; Start de game loop
	@@gameLoop:
		call updateCrouching, offset player
		call updateJump, 4
		push ds
		call setVideoMode, 12h
		pop es
		call displayScore
		call updateDifficulty
		call drawFloor, offset Floor, offset SizeFloor, 50
		call updateEnemies, [enemySpawnRate]
		call drawPlayer, offset player
		cmp [player.lives], 0 ; Stop the game if the player has no lives left
		je @@gameOver

		;Wait for the current frame to be drawn
		call waitForFrame


		call isKeyPressed, 01h
		cmp eax, 0
		jne @@terminate
	jmp @@gameLoop

	@@gameOver:
	call displayHighScore
	;Checking if user presses escape and ending program when he did
	@@checkEsc:
	call isKeyPressed, 01h
	cmp eax, 0
	je @@checkEsc

	@@terminate:
	call saveHighScore
	call __keyb_uninstallKeyboardHandler
	call terminateProcess

ENDP main

DATASEG
; Drawing sprites
PterodactylFile db "ptero.bin", 0
TrexCrouchingFile db "trcrouch.bin", 0
TrexFile db "trex.bin", 00
SmallCactusFile db "smallcac.bin", 0
LargeCactusFile db "largecac.bin", 0
HighestScoreFile db "higscore.bin", 0
openErrorMsg db "could not open file", 13, 10, '$'
readErrorMsg db "could not read data", 13, 10, '$'
closeErrorMsg db "error during file closing", 13, 10, '$'
scoreText db "Score: ", 0, '$'
highestScoreText db "High score: ", 0, '$'


SizeFloor DW 80
Floor DB 0ffH
;Random Generation
RandomState DD 0; 1957386613 Binary: 111 0100 1010 1011 0101 1001 0111 0101
NewLine db ' ', 13, 10, '$' ; 13, 10: newline, $: eindigd interrupt
;Jumping
PlayerGroundHeight DD 46
JumpState DD 0 ; It's not a boolean, it's the x coordinate of a function that calculates the height

struc Player
	x   dd 5  	  ; X position
	y   dd 46     ; Y position
	defaultSprite dd ?   ; Pointer to the sprite
	crouchSprite dd ?
	heightOffset dd 4 ; Amount of blank space above the player sprite
	defaultOffset dd 4
	crouchOffset dd 2
	score dd 0
	highscore dd ?
	crouching dd 0
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
lastEnemySpawn dd 0
minEnemyDistance dd 30 ; Hardest is 20, easisest is 30
enemySpawnRate dd 6 ; Hardest is 1, easiest is 6
lastEnemyPtery dd 0 ; We don't want to spawn an enemy adjacent to a pterodactyl

scoreSinceChange dd 0 ; The score gained after the last difficulty change

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
