.8086

;
; Takes a word register (and references to its upper and lower byte), and the
; location of a character coordinate pair, and converts them into an offset to
; the board table/array. Pushes flags to stack - pop them and use the carry flag
; to determine which of the two nibbles to read (CF = rightmost).
;

CRD2OFST MACRO reg, regH, regL, loc
         mov reg, WORD PTR loc ; Get 2 char string of form AB where A is a letter and B is a number
         sub reg, 3161h        ; Subtract 'a' from lower byte and '1' from upper byte
         ; The operations up until the shl, are giving us the result of 7 - regH
         sub regH, 7           ; Subtract 7 (translates 0-7 to 249-255)
         not regH              ; Invert number (translates 249-255 to 6-255)
         inc regH              ; Add 1 (translates 6-255 to 7-0)
         shl regH, 1           ; Multiply row by 4 bytes
         shl regH, 1
         shr regL, 1           ; Divide column by 2 (4 bytes long not 8)
         pushf                 ; Store flags (we want CF later)
         add regL, regH        ; Add column
         xor regH, regH        ; Clear upper byte
         ENDM

CLRCOORD MACRO
         mov WORD PTR coords[0], 2020h ; Clear source coordinate
         mov WORD PTR coords[2], 2020h ; Clear destination coordinate
         ENDM

Stack    SEGMENT STACK
theStack DB 8 DUP ("(C) Matthew R.  ") ; 8 * 16 bytes
Stack    ENDS

Data     SEGMENT PUBLIC
board    DB 32 DUP (0)  ; 32 bytes, one nibble per board tile
                        ; 000 empty space
                        ; 001 pawn
                        ; 010 rook
                        ; 011 knight
                        ; 100 bishop
                        ; 101 queen
                        ; 110 king
                        ; fourth bit for player
player   DB 0,0         ; Current player
pieces   DB " PRNBQK?", ; White pieces
            " prnbqk?"  ; Black pieces
message0 DB "Enter move > ",0
message1 DB "Illegal move! ",0
msgTable DW message0, message1
status   DW 0
coords   DB "    "        ; 4 bytes for two sets of chess board coordinates
prompts  DW promptLetter, ; Next parse source column
            promptNumber, ; Next parse source row
            promptLetter, ; Next parse destination column
            promptNumber, ; Next parse destination row
            promptCheck   ; Next try move and reset
move     DB 0             ; Current step for player move selection
                          ; 0 - not started
                          ; 1 - got first letter
                          ; 2 - got first number (source tile selected)
                          ; 3 - got second letter
                          ; 4 - got second number (destination tile selected) - may not be used
valTable DW validEmpty,   ; Trying to move an empty space (ERROR)
            validPawn,    ; Moving a pawn
            validRook,    ; Moving a rook
            validKnight,  ; Moving a knight
            validBishop,  ; Moving a bishop
            validQueen,   ; Moving a queen
            validKing     ; Moving a king
nibble   DB 0Fh, 0F0h     ; Mask to get nibble out of byte
          ;
          ; Pawn related constants
          ;
pawnStart DB 32h, 37h     ; (ASCII) coordinates for pawn starting row for each player (don't change this)
pawnNormalMove DB 1, -1   ; DST-SRC difference per player
pawnDoubleMove DB 2, -2   ; DST-SRC difference per player, double move
Data     ENDS

Code SEGMENT PUBLIC

  assume CS:Code,DS:Data

main PROC
start:
     mov AX, Data
     mov DS, AX

     ;
     ; Initialize Board
     ;
     mov AX, 9999h  ; Four black pawns
     lea BX, board  ; Get address of board
     add BX, 4      ; (BX = board + 4) Second row
     mov [BX], AX   ; Place pawns
     add BX, 2      ; BX = board + 6
     mov [BX], AX   ; Place remaining pawns
     and AX, 7777h  ; Change to four white pawns
     add BX, 18     ; BX = board + 24
     mov [BX], AX   ; Place pawns
     add BX, 2      ; BX = board + 26
     mov [BX], AX   ; Place remaining pawns
     mov AX, 0DCBAh ; Black rook, knight, bishop, and queen
     sub BX, 26     ; BX = board
     mov [BX], AX   ; Place black pieces
     and AX, 7777h  ; Change to white pieces
     add BX, 28     ; (BX = board + 28) Eighth row
     mov [BX], AX   ; Place white pieces
     mov AX, 2346h  ; White king, bishop, knight, and rook
     add BX, 2      ; BX = board + 30
     mov [BX], AX   ; Place remaining white pieces
     or  AX, 8888h  ; Change to black pieces
     sub BX, 28     ; (BX = board + 2) First row, second half
     mov [BX], AX   ; Place remaining black pieces
     ; Player should already be set to white by assembler

     ;
     ; Game loop
     ;
game:
     CLRCOORD       ; Clear coordinates
     call checkmate ; Check if player is checkmated
     jnz gameOver   ; If they are, show message, and then exit
     call check     ; Check if player is in check
     jz prompt      ; If not, skip the following instructions
     ;if they are, tell them

     ;
     ; User input
     ;
prompt:
     call drawBoard     ; Draw the chess board
     ; prompt player for move
     xor AH, AH         ; Keyboard function 0, get keystroke
     int 16h            ; Keyboard
     cmp AL, 71h        ; 71h = 'q'
     je gameOver        ; Quit
     ; Read a move
     mov BL, move       ; Get current move step (how far player is in making their move)
     xor BH, BH         ; Clear BX
     shl BX, 1          ; Multiply by 2 since addresses are words not bytes
     jmp prompts[BX]    ; Jump based on current step

promptLetter:
     ; Read in a letter
     sub AL, 61h        ; 61h = 'a'
     jb prompt          ; Try again
     sub AL, 8          ; AL -= 8
     jae prompt         ; Try again
     add AL, 69h        ; Restore column character
     shr BX, 1          ; Unshift from earlier jump
     mov coords[BX], AL ; Store in coordinate variable
     inc BL             ; Next step
     mov move, BL       ; Update in memory
     jmp prompt

promptNumber:
     ; Read in a number
     sub AL, 31h        ; 31h = '1'
     jb prompt          ; Try again
     sub AL, 8          ; AL -= 8
     jae prompt         ; Try again
     add AL, 39h        ; Restore row character
     shr BX, 1          ; Unshift from earlier jump
     inc BL             ; Next step
     mov move, BL       ; Update in memory
     dec BL             ; DL is either 2 or 4, so now it will be 1 or 3
     mov coords[BX], AL ; Store in coordinate variable
     jmp prompt

promptCheck:
     ; Confirm move
     mov move, 0        ; Set move step thingy to 0
     call valid         ; Check if move is valid (and if player is in check, make sure move gets them out of check)
     jnz moveExecute    ; If so, execute the move, otherwise:
     CLRCOORD           ; Clear coordinates
     jmp prompt         ; Start move over

     ;
     ; Remove the player's piece from its current position and store the piece in the stack
     ;
moveExecute:
     CRD2OFST BX, BH, BL, coords[0] ; Get source coordinates and convert to board offset
     mov DL, board[BX]      ; Get two pieces from coordinates
     xor AX, AX             ; Clear AX
     popf                   ; Restore flags
     pushf                  ; Backup flags again
     jnc moveReadSource     ; Skip if we want leftmost piece
     inc AX                 ; +1 to get rightmost piece
moveReadSource:
                            ; Get piece being moved
     push BX                ; Backup source offset
     mov BX, AX             ; Move to base register
     and DL, nibble[BX]     ; Mask out the piece we want (i.e. 1011 0101 -> 1011 0000)
     pop BX                 ; Restore source offset
     xor board[BX], DL      ; Remove selected piece from board
     popf                   ; Restore flags again
     jnc moveReadSourceDone ; Skip if moving leftmost piece
     shr DL, 1              ; Shift out leftmost piece so we have rightmost instead
     shr DL, 1
     shr DL, 1
     shr DL, 1
moveReadSourceDone:
     push DX                ; Backup piece being moved

     ;
     ; Grab the destination byte, merge the piece that isn't being replaced with the one being moved, and write back to memory
     ;
     CRD2OFST BX, BH, BL, coords[2] ; Get destination coordinates and convert to board offset
     mov DL, board[BX]      ; Get two pieces from coordinates
     xor AX, AX             ; Clear AX
     popf                   ; Restore flags
     pushf                  ; Backup flags again
     jc moveReadDestination ; Skip if we are replacing leftmost piece?
     inc AX                 ; +1 to replace rightmost piece
moveReadDestination:
     ; Get piece not being replaced
     push BX                ; Backup destination offset
     mov BX, AX             ; Move to base register
     and DL, nibble[BX]     ; Mask out the piece we are replacing (so we can or this with the piece being moved)
     pop BX                 ; Restore destination offset
     popf                   ; Restore flags again
     pop AX                 ; Restore piece being moved into AX (pushed from DX)
     jnc moveMerge          ; Skip if replacing the leftmost piece
     shl AL, 1              ; We are replacing rightmost piece
     shl AL, 1
     shl AL, 1
     shl AL, 1
moveMerge:
     ; At this point, AL contains the piece being moved (in the correct nibble), and DL contains the piece that isn't being replaced
     or DL, AL              ; Merge the two pieces together
     mov board[BX], DL      ; Commit to memory

     ;
     ; Update and continue game
     ;
     xor player, 1 ; change to other player's turn
     jmp game

     ;
     ; Game end
     ;
gameOver:
     mov AX, 4C00h
     int 21h
main ENDP

; optional: draw board from current player's perspective
drawBoard PROC
          ;
          ; Clear Screen
          ;
          mov AH, 0Fh ; BIOS video function F, get video mode
          xor BH, BH  ; BH = 0
          int 10h     ; BIOS video
          xor AH, AH  ; BIOS video function 0, set video mode
          int 10h     ; BIOS video

          ;
          ; Move cursor to top left
          ;
          mov AH, 1   ; BIOS video function 1, cursor control
          mov CH, 1Fh ; Bit 5 disables cursor, 0-4 control cursor shape
          int 10h     ; BIOS video
          mov AH, 2   ; BIOS video function 2, set cursor position
          xor BH, BH  ; Page 0
          xor DX, DX  ; Row 0, column 0
          int 10h     ; BIOS video

          ;
          ; Print board
          ;
          mov AX, 0E20h ; BIOS video function E, write single char and move cursor, AL = 20h = ' '
          xor BX, BX    ; Page 0
          push AX       ; Backup function
          int 10h       ; BIOS video
          pop AX        ; Restore function
          int 10h       ; BIOS video
          mov AX, 0E60h ; BIOS video function E, write single char and move cursor, AL = 61h = 'a'
          mov CX, 8     ; Count = 8
drawAH:
          inc AL        ; AL++
          mov AH, 0Eh   ; BIOS video function E, write single char and move cursor
          push CX       ; Backup count
          int 10h       ; BIOS video
          pop CX        ; Restore count
          dec CX        ; CX-- (Printed one char)
          jnz drawAH    ; While CX != 0
          mov AX, 0E0Dh ; BIOS video function E, write single char and move cursor, AL = Carriage Return
          ;xor BX, BX    ; Page 0
          int 10h       ; BIOS video
          mov AX, 0E0Ah ; BIOS video function E, write single char and move cursor, AL = Line Feed
          ;xor BX, BX    ; Page 0
          int 10h       ; BIOS video
          mov AX, 0E0Ah ; BIOS video function E, write single char and move cursor, AL = Line Feed
          ;xor BX, BX    ; Page 0
          int 10h       ; BIOS video

          mov CX, -2    ; Count = -2
drawLoop:
          add CX, 2     ; Increment count by 2
          cmp CX, 32    ; 32 / 4 = 8, 8 rows
          je drawEnd    ; If we've drawn 8 rows, stop
          push CX       ; Backup count
          ; Check part of row
          test CX, 3    ; Check if CX is a not a multiple of 4
          jnz drawRow   ; If so, continue loop, otherwise:
          ; Row coordinate character
          mov AX, 0E38h ; BIOS video function E, write single character and move cursor, print character 38h ('8')
          shr CX, 1     ; Divide count by 2
          shr CX, 1     ; DIvide count by 2 (now 4)
          sub AL, CL    ; (char)AL -= count / 4
          ;xor BX, BX    ; Page 0
          int 10h       ; BIOS video
          mov AX, 0E20h ; BIOS video function E, write single char and move cursor, AL = ' '
          ;xor BX, BX    ; Page 0
          int 10h       ; BIOS video
          pop CX        ; Rest of drawLoop needs CX
          push CX       ; But we still have to push it to maintain push/pop balance
          ;
          ; Main draw, 8 characters per row
          ;
          ; First half of row CX
drawRow:
          mov BX, CX                  ; Use count as our offset TODO: possibly optimize so that count is already in BX?
          mov DX, WORD PTR board[BX]  ; Get pieces (board data)
          ; Print first piece
          mov BX, DX                  ; Copy DX to BX
          and BX, 0Fh                 ; Get first nibble
          mov AL, pieces[BX]          ; Get character for piece, from array
          xor BX, BX                  ; Page 0
          mov AH, 0Eh                 ; BIOS video function E, write single char and move cursor
          push DX                     ; Backup data from memory
          int 10h                     ; BIOS video
          pop DX                      ; Restore data from memory
          shr DX, 1
          shr DX, 1
          shr DX, 1
          shr DX, 1                   ; Next piece (4 bits per piece)
          ; Print second piece
          mov BX, DX                  ; Copy DX to BX
          and BX, 0Fh                 ; Get first nibble
          mov AL, pieces[BX]          ; Get character for piece, from array
          xor BX, BX                  ; Page 0
          mov AH, 0Eh                 ; BIOS video function E, write single char and move cursor
          push DX                     ; Backup data from memory
          int 10h                     ; BIOS video
          pop DX                      ; Restore data from memory
          shr DX, 1
          shr DX, 1
          shr DX, 1
          shr DX, 1                   ; Next piece (4 bits per piece)
          ; Print third piece
          mov BX, DX                  ; Copy DX to BX
          and BX, 0Fh                 ; Get first nibble
          mov AL, pieces[BX]          ; Get character for piece, from array
          xor BX, BX                  ; Page 0
          mov AH, 0Eh                 ; BIOS video function E, write single char and move cursor
          push DX                     ; Backup data from memory
          int 10h                     ; BIOS video
          pop BX                      ; Restore data from memory
          shr BX, 1
          shr BX, 1
          shr BX, 1
          shr BX, 1                   ; Next piece (4 bits per piece)
          ; Print fourth piece
          mov AL, pieces[BX]          ; Get character for piece, from array
          xor BX, BX                  ; Page 0
          mov AH, 0Eh                 ; BIOS video function E, write single char and move cursor
          int 10h                     ; BIOS video
          ; Back to loop
          pop CX                      ; Restore count
          test CX, 2                  ; Check if CX is a multiple of 2, but not 4 (aka 2, 6, 10, etc)
          jz drawLoop                 ; If not, continue loop
          push CX                     ; Otherwise, backup count again, and print CRLF
          mov AL, 0Dh                 ; Carriage Return
          ;xor BX, BX                  ; Page 0
          mov AH, 0Eh                 ; BIOS video function E, write single char and move cursor
          int 10h                     ; BIOS video
          mov AL, 0Ah                 ; Line Feed
          ;xor BX, BX                  ; Page 0
          mov AH, 0Eh                 ; BIOS video function E, write single char and move cursor
          int 10h                     ; BIOS video
          pop CX                      ; Restore count
          jmp drawLoop                ; Back to top

          ;
          ; Display currently staged move
          ;
drawEnd:
          xor BX, BX        ; Page 0
          mov AX, 0E0Dh     ; BIOS video 0E, write char - 0x0D = '\r'
          int 10h           ; BIOS video
          mov AX, 0E0Ah     ; 0x0A = '\n'
          int 10h
          mov AH, 0Eh
          mov AL, coords[0] ; Source Column
          int 10h
          mov AH, 0Eh
          mov AL, coords[1] ; Source Row
          int 10h
          mov AX, 0E20h     ; 0x20 = ' '
          int 10h
          mov AX, 0E74h     ; 0x74 = 't'
          int 10h
          mov AX, 0E6Fh     ; 0x6F = 'o'
          int 10h
          mov AX, 0E20h     ; 0x20 = ' '
          int 10h
          mov AH, 0Eh
          mov AL, coords[2] ; Destination Column
          int 10h
          mov AH, 0Eh
          mov AL, coords[3] ; Destination Row
          int 10h
          mov AX, 0E0Dh     ; 0x0D = '\r'
          int 10h
          mov AX, 0E0Ah     ; 0x0A = '\n'
          int 10h

          ;
          ; Display a message based on last status, and reset status
          ;
          mov BX, [status]     ; Get current status
          mov BX, msgTable[BX] ; Get address of relevant message
          mov AL, [BX]         ; Get next character to print
drawString:
          mov AH, 0Eh          ; BIOS video function E, write character
          push BX              ; Backup address
          xor BX, BX           ; Page 0
          int 10h              ; BIOS video
          pop BX               ; Restore address
          inc BX               ; Next character
          mov AL, [BX]         ; Get next character to print
          test AL, 0FFh        ; Apparently mov doesn't set/clear the Zero Flag...
          jnz drawString       ; Loop until null terminator
          mov status, 0        ; Reset status

          ret
drawBoard ENDP

checkmate PROC
          call check ; See if king is in check
          jnz moves  ; If in check, jump to checking if there are safe moves
          ; Check how many pieces the player has
          ; Compare that to 1
          je king    ; If they have other pieces, and aren't in check, they can move those pieces
          xor AX, AX ; AX = 0
          ret
moves:    ; for each piece (besides the king) check if any moves will save the king
king:     ; check if king can make a move that is safe
          ; put results in AX (0 means not checkmated)
          ret
checkmate ENDP

check PROC
      ; for each of the 8 directions, go through empty board spaces
      ; until you find a piece (either player)
      ; check if that piece can attack the king
      xor AX, AX ; TEMP: hardcoded that the player was not found to be in check
      ; if so, clear the zero flag in some way and
      jz knight ; Lastly, check if any knights can attack
      ret
knight: ; check if there are knights in any of the 8 attacking positions
      xor AX, AX ; TEMP: hardcoded that the player was not found to be in check
      ret
check ENDP

;
; Check if a move is valid
;
valid PROC
      ;
      ; Check if source is owned by current player
      ;
      CRD2OFST BX, BH, BL, coords[0] ; Get source coordinates and convert to board offset
      mov DL, board[BX]   ; Get two pieces from coordinates
      popf                ; Restore flags
      jnc validSourceTest ; Continue to valid source test if no carry
      shr DL, 1           ; Shift out leftmost of the 2 pieces
      shr DL, 1
      shr DL, 1
      shr DL, 1
validSourceTest:
      and DL, 0Fh         ; Clear upper nibble
      ; Check if piece is actually a play piece and not an empty space
      jz validReturn      ; That part of the board is not a valid piece (hopefully meaning it's a space ' ')

      mov DH, DL          ; Copy piece to DH
      and DH, 08h         ; Isolate player bit
      shr DH, 1           ; Move player bit to LSB
      shr DH, 1
      shr DH, 1
      ; Check player owns piece
      xor DH, 1           ; Invert player (so cmp/jmp logic is inverted!)
      cmp DH, [player]    ; Check if this piece is owned by the current player
      je validReturn      ; If not, then this move is not legal
      xor DH, DH          ; Clear upper byte
      push DX             ; Backup source piece

      ;
      ; Check if destination is now owned by current player
      ;
      CRD2OFST BX, BH, BL, coords[2] ; Get destination coordinates and convert to board offset
      mov DL, board[BX]        ; Get two pieces from coordinates
      popf                     ; Restore flags
      jnc validDestinationTest ; Continue to v alid destination test if no carry
      shr DL, 1                ; Shift out leftmost of the 2 pieces
      shr DL, 1
      shr DL, 1
      shr DL, 1
validDestinationTest:
      and DL, 0Fh              ; Clear upper nibble
      ;push DL                 ; Backup destination piece
      ; Check if space is blank
      jz validPieceJump        ; If so, skip to chess logic

      mov DH, DL               ; Copy piece to DH
      and DH, 08h              ; Isolate player bit
      shr DH, 1                ; Move player bit to LSB
      shr DH, 1
      shr DH, 1
      ; Check player doesn't own piece
      cmp DH, [player]         ; Check if this piece is owned by the current player
      je validPopAndReturn     ; If so, then this move is not legal (otherwise this is a blank space, or an opponent's piece)

      ;
      ; Check if move is a valid chess play
      ;
validPieceJump:
      pop DX           ; Restore source piece
      mov BX, DX       ; Place in base register
      and BX, 7        ; Only use first 3 bits
      shl BX, 1        ; Multiply by 2, so it becomes a word offset
      jmp valTable[BX] ; Allowing us to jump to the correct check in the code

      ;
      ; An empty space was owned by the second player (binary 1000) - this should NEVER happen
      ;
validEmpty:   
      xor DX, DX      ; Set Zero Flag (probably not necessary since the shl BX, 1 would have set the zero flag but this is for errors anyway so whatever)
      ret
      ;jmp validReturn ; Finish

      ;
      ; Check if play is valid for pawn
      ;
      ; Currently implemented:
      ; - Move forward one space
      ; - Move forward two spaces if in starting row
      ; - Attack diagonally
      ;
validPawn:
      mov AX, WORD PTR coords[2]     ; Get destination coordinates
      mov BX, WORD PTR player        ; Get current player as offset

      ; Check if moving within same column
      sub AH, coords[1]              ; Subtract source y coordinate
      cmp AH, pawnNormalMove[BX]     ; Check if pawn is moving single space forward
      jne validPawnDouble            ; If not, check for double
      sub AL, coords[0]              ; Subtract source x coordinate
      jne validPawnSpecial           ; For diagonal attacks
      ; Make sure destination doesn't have enemy piece since pawns can only attack diagonally
      CRD2OFST BX, BH, BL, coords[2] ; Get destination coordinates and convert to board offset
      mov DL, board[BX]              ; Get two pieces from coordinates
      popf                           ; Restore flags
      jnc validPawnAttack            ; Continue to valid destination test if no carry
      shr DL, 1                      ; Shift out leftmost of the 2 pieces
      shr DL, 1
      shr DL, 1
      shr DL, 1
validPawnAttack:
      and DL, 07h                    ; Clear upper nibble and player bit
      jnz validSetAndReturn          ; If not zero, then a piece is impeding the progress of this pawn
      or DX, 0FFFFh                  ; Something to clear the Zero Flag
      ret

validPawnSpecial:
      ; Test if pawn is moving one space diagonally
      cmp AL, 1                      ; Check if moving right one space or less
      jg validSetAndReturn           ; If not, the move is invalid
      cmp AL, -1                     ; Check if moving left one space or less
      jl validSetAndReturn           ; If not, the move is invalid
      shl AX, 1                      ; Something to clear the Zero Flag
      CRD2OFST BX, BH, BL, coords[2] ; Get offset of destination piece
      mov DL, board[BX]              ; Place pieces in DL
      popf                           ; Restore flags
      jnc validPawnSpecialEnemy      ; If no carry, skip (we want leftmost piece)
      shr DL, 1                      ; Shift out leftmost piece to get rightmost
      shr DL, 1
      shr DL, 1
      shr DL, 1
validPawnSpecialEnemy:
      and DL, 0Fh                    ; Mask out upper nibble
      jz validSetAndReturn           ; If nothing remains, the piece is empty and the move is invalid
      shr DL, 1                      ; Move player bit to LSB (and shift out actual piece)
      shr DL, 1
      shr DL, 1
      cmp DL, player[0]              ; Check if piece is owned by other player
      ret

validPawnDouble:
      ; Test if pawn is moving two spaces, from starting position
      cmp AH, pawnDoubleMove[BX]     ; Check if moving two spaces forward
      jne validSetAndReturn          ; If not, the move isn't valid
      mov DL, pawnStart[BX]          ; Get starting row for this player's pawns
      cmp DL, coords[1]              ; Check if pawn is in said row
      jne validSetAndReturn          ; If not, the move isn't valid
      ; Make sure path is not blocked
      CRD2OFST BX, BH, BL, coords[0] ; Get offset of source piece
      mov AX, WORD PTR player        ; Get current player
      shl AX, 1                      ; Shift player bit into AX (so AX is now 0 or 8)
      shl AX, 1
      shl AX, 1
      sub AX, 4                      ; Subtract 4 (so now AX is -4 or 4)
      ; Check if first space in front of pawn is empty
      add BX, AX                     ; Add this to the base register so we get the next row
      mov DL, board[BX]              ; Place pieces in DL
      popf                           ; Restore flags
      pushf                          ; Backup flags (again)
      jnc validPawnDoubleSafeCheck1  ; If no carry, skip (we want leftmost piece)
      shr DL, 1                      ; Shift out leftmost piece to get rightmost
      shr DL, 1
      shr DL, 1
      shr DL, 1
validPawnDoubleSafeCheck1:
      and DL, 07h                    ; Clear upper nibble and player bit
      jnz validPopSetAndReturn       ; If not zero, then a piece is impeding the progress of this pawn
      ; Check if second space in front of pawn is empty
      add BX, AX                     ; Get next row (again)
      mov DL, board[BX]              ; Place pieces in DL
      popf                           ; Restore flags (again)
      jnc validPawnDoubleSafeCheck2  ; If no carry, skip (we want leftmost piece)
      shr DL, 1                      ; Shift out leftmost piece to get rightmost
      shr DL, 1
      shr DL, 1
      shr DL, 1
validPawnDoubleSafeCheck2:
      and DL, 07h                    ; Clear upper nibble and player bit
      jnz validSetAndReturn          ; If not zero, then a piece is impeding the progress of this pawn
      ; Path is clear, move is valid
      or DX, 0FFFFh                  ; Something to clear the Zero Flag
      ret

      ;
      ; Check if play is valid for rook
      ;
validRook:
      xor DX, DX      ; Set Zero Flag
      jmp validReturn ; Finish

      ;
      ; Check if play is valid for knight
      ;
validKnight:
      mov AX, WORD PTR coords[2] ; Get destination coordinates
      ;sub AX, 3161h            ; Subtract 'a' from lower byte and '1' from upper byte

      ; Verify valid X coordinate (-2, -1, 1, or 2)
      sub AL, coords[0]        ; Subtract source x coordinate
      je validReturn           ; Knight's destination cannot be same column as source
      jg validKnightPositiveX  ; Jump over this code if the difference is positive
      dec AL                   ; Subtract 1
      not AL                   ; Then invert to get 2's compliment
validKnightPositiveX:
      cmp AL, 2                ; Check difference against 2
      jg validSetAndReturn     ; Knights cannot move more than 2 spaces in any direction

      ; Verify valid Y coordinate (-2, -1, 1, or 2)
      sub AH, coords[1]        ; Subtract source y coordinate
      je validReturn           ; Knight's destination cannot be same row as source
      jg validKnightPositiveY  ; Jump over this code if the difference is positive
      dec AH                   ; Subtract 1
      not AH                   ; Then invert to get 2's compliment
validKnightPositiveY:
      cmp AH, 2                ; Check difference against 2
      jg validSetAndReturn     ; Knights cannot move more than 2 spaces in any direction

      ; Verify valid X,Y coordinates (+-2,+-1 or +-1,+-2 - in other words, the absolute value of the difference cannot be the same for X and Y)
      cmp AH, AL               ; Check X and Y differences - ZF means invalid, NZ means valid, because:
      jmp validReturn          ; Knights move in L shapes - 2 spaces in one direction, and 1 in the other

      ;
      ; Check if play is valid for bishop
      ;
validBishop:
      xor DX, DX      ; Set Zero Flag
      jmp validReturn ; Finish

      ;
      ; Check if play is valid for queen
      ;
validQueen:
      xor DX, DX      ; Set Zero Flag
      jmp validReturn ; Finish

      ;
      ; Check if play is valid for king
      ;
      ; Currently implemented:
      ; - Move any direction one space
      ;
validKing: ; TODO: check if move would place king in check...
      mov AX, WORD PTR coords[2] ; Get destination coordinates

      ; Check if moving within same column and row
      sub AH, coords[1]          ; Subtract source y coordinate
      cmp AH, 1                  ; Check against 1
      jg validSetAndReturn       ; Invalid if more than one space vertically
      cmp AH, -1                 ; Check against -1
      jl validSetAndReturn       ; See above
      sub AL, coords[0]          ; Subtract source x coordinate
      cmp AL, 1                  ; Check against 1
      jg validSetAndReturn       ; Invalid if more than one space horizontally
      cmp AL, -2                 ; Check against -2
      jle validSetAndReturn      ; See above
      ret                        ; Unless AL is -2, the Zero Flag will be cleared

      ;
      ; Return
      ;
validPopSetAndReturn:
      pop DX             ; Maintain stack balance...
validSetAndReturn:
      xor DX, DX         ; Set Zero Flag
      mov status, 2      ; Otherwise go to status 1 (byte 2)
      ret
validClearAndReturn:
      xor DX, DX         ; Set to 0
      inc DX             ; Clear Zero Flag
      ret
validPopAndReturn:
      pop DX             ; Maintain stack balance...
validReturn:
      pushf              ; Backup flags
      jnz validReturnPop ; Skip if move was valid
      mov status, 2      ; Otherwise go to status 1 (byte 2)
validReturnPop:
      popf               ; Restore flags
      ret
valid ENDP

Code ENDS

  END start
