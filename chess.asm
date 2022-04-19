.8086

;
; Takes a word register (and references to its upper and lower byte), and the
; location of a character coordinate pair, and converts them into an offset to
; the board table/array. Pushes flags to stack - pop them and use the carry flag
; to determine which of the two nibbles to read (CF = rightmost).
;

CRD2OFST MACRO W, loc
         mov &W&X, WORD PTR loc ; Get 2 char string of form AB where A is a letter and B is a number
         sub &W&X, 3161h        ; Subtract 'a' from lower byte and '1' from upper byte
         ; The operations up until the shl, are giving us the result of 7 - regH
         sub &W&H, 7           ; Subtract 7 (translates 0-7 to 249-255)
         not &W&H              ; Invert number (translates 249-255 to 6-255)
         inc &W&H              ; Add 1 (translates 6-255 to 7-0)
         shl &W&H, 1           ; Multiply row by 8 bytes
         shl &W&H, 1
         shl &W&H, 1
         add &W&L, &W&H        ; Add column
         xor &W&H, &W&H        ; Clear upper byte
         ENDM

CLRCOORD MACRO
         mov WORD PTR coords[0], 2020h ; Clear source coordinate
         mov WORD PTR coords[2], 2020h ; Clear destination coordinate
         ENDM

;
; Quick putc (assumes BH is already 0)
; Assumes BX is the address of an array, and AL is the index.
; Translates (xlat) that index and returns the value into AL
; Then sets AH and prints
;
XLATPUTC MACRO
         xlat        ; Use AL index into BX and return value into AL
         mov AH, 0Eh ; BIOS video function E, TTY write character
         int 10h     ; Call BIOS
         ENDM

;
; Quick putc (assumes AH is already set to 0Eh and BH is already 0)
; Must provide AL value
;
LPUTC MACRO char
      mov AL, char ; Place character in AL
      int 10h      ; BIOS video
      ENDM

Stack    SEGMENT STACK
theStack byte 8 DUP ("(C) Matthew R.  ") ; 8 * 16 bytes
Stack    ENDS

Data     SEGMENT PUBLIC

;
; Game board data
;
; Piece secification
;   Lower nibble - data
;     000 empty space
;     001 pawn
;     010 rook
;     011 knight
;     100 bishop
;     101 queen
;     110 king
;   Upper nibble - header
;     bit 0 - owner of piece
;     bit 1 - piece has not been moved (should be set at start of the game, then once moved, remain cleared)
;
board byte 64 DUP (0) ; 64 bytes, one byte per board tile

; Initial game state (top and bottom 2 rows only)
starting_board byte \
  32h, 33h, 34h, 35h, 36h, 34h, 33h, 32h, ; rnbqkbnr
  8 DUP (31h),                            ; pppppppp
  8 DUP (21h),                            ; PPPPPPPP
  22h, 23h, 24h, 25h, 26h, 24h, 23h, 22h  ; RNBQKBNR

;
; Current player (second byte just for saving on cycles when moving into a
; 16-bit register - so we don't need to clear the upper byte)
;
player byte 0,0 ; Current player

; ASCII characters for each player's pieces
pieces byte \
  " PRNBQK?", ; White pieces
  "PADDING!", ; Dummy data for padding (so we don't need to move the player bit
  \           ; to the lower-byte - saves cycles on the screen draw routine
  \           ; which is called the most)
  " prnbqk?"  ; Black pieces

; Various in-game messages
move_prompt byte "Enter move > ",0 ; Shown while waiting for a move to be input
illegal_move byte "Illegal move! ",0 ; Shown when a move is attempted but it's
                                     ; not valid
message_table word \
  move_prompt,
  illegal_move
; Current input status
status word 0 ; Index for table above

; Currently staged move
coords byte "    " ; First 2 bytes are source, second 2 are destination
; Labels to jump to upon user input, dependent on what has already been input
prompts word \
  promptLetter, ; Next parse source column
  promptNumber, ; Next parse source row
  promptLetter, ; Next parse destination column
  promptNumber, ; Next parse destination row
  promptCheck   ; Next try move and reset
;
; Current step for player move selection
;
; 0 - not started
; 1 - got first letter
; 2 - got first number (source tile selected)
; 3 - got second letter
; 4 - got second number (destination tile selected)
;
move byte 0, 0 ; Index for table above (extra byte for saving cycles - no need to clear upper byte of register)

;
; Jump table for valid move check, based on 3-bit piece data
valid_table word \
  validEmpty,  ; Trying to move an empty space (ERROR)
  validPawn,   ; Moving a pawn
  validRook,   ; Moving a rook
  validKnight, ; Moving a knight
  validBishop, ; Moving a bishop
  validQueen,  ; Moving a queen
  validKing,   ; Moving a king
  validEmpty   ; Undefined 111 piece

; Simple mask for getting a nibble from a byte
nibble byte 0Fh, 0F0h ; 0 is lower 1 is upper - may not be used...

;
; Pawn related constants
;
pawnStart byte 32h, 37h ; (ASCII) coordinates for pawn starting row for each
                        ; player (don't change this)
pawnNormalMove byte 1, -1 ; DST-SRC difference per player
pawnDoubleMove byte 2, -2 ; DST-SRC difference per player, double move

; 197 bytes of RAM up to this point - try to keep all printable text below 256 to optimize int 10h (AH=0Eh)

Data     ENDS

Code SEGMENT PUBLIC

  assume CS:Code,DS:Data

main PROC
start:
     mov AX, Data
     mov DS, AX
     mov ES, AX
     cld          ; Clear Direction

     ;
     ; Initialize Game
     ;
reset:
     ; Init Board
     lea SI, starting_board ; Source      = starting_board
     lea DI, board          ; Destination = board
     mov CX, 8              ; 16 bytes (black pieces)
     rep movsw              ; Copy 16 bytes from starting_board to board
     add DI, 32             ; 8 * 4 (from row 3 to row 7)
     mov CX, 8              ; 16 bytes (white pieces)
     rep movsw              ; Copy 16 bytes from starting_board to board
     ; Init State
     mov player, 0          ; White goes first
     mov status, 0          ; Awaiting input
     mov move, 0            ; No input yet

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
     call drawBoard        ; Draw the chess board
     ; prompt player for move
     xor AH, AH            ; Keyboard function 0, get keystroke
     int 16h               ; Keyboard
     cmp AL, 71h           ; 71h = 'q'
     je gameOver           ; Quit
     ; Read a move
     mov BX, WORD PTR move ; Get current move step (how far player is in making their move)
     shl BX, 1             ; Multiply by 2 since addresses are words not bytes
     jmp prompts[BX]       ; Jump based on current step

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
     ; Execute player's move
     ;
moveExecute:
     ; Get piece being moved and remove it from the board
     CRD2OFST B, coords[0] ; Get source coordinates and convert to board offset
     mov DL, board[BX]              ; Get two pieces from coordinates
     mov board[BX], 0               ; Remove selected piece from board
     ; Get the destination and replace whatever is there with the piece
     CRD2OFST B, coords[2] ; Get destination coordinates and convert to board offset
     mov board[BX], DL              ; Place piece in new location

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
          int 10h       ; BIOS video (print a-h)
          loop drawAH   ; while (--count)
          mov AL, 0Dh   ; BIOS video function E, write single char and move cursor, AL = Carriage Return
          ;xor BX, BX    ; Page 0
          int 10h       ; BIOS video
          mov AL, 0Ah   ; BIOS video function E, write single char and move cursor, AL = Line Feed
          ;xor BX, BX    ; Page 0
          int 10h       ; BIOS video
          ;mov AX, 0E0Ah ; BIOS video function E, write single char and move cursor, AL = Line Feed
          ;xor BX, BX    ; Page 0
          int 10h       ; BIOS video

          ;
          ; Draw row loop + vertical coordinates
          ;
          mov CX, 8     ; Count = 8 (rows to print)
          lea SI, board ; SI = address of board data
          lea BX, pieces ; ASCII data
          mov AH, 0Eh   ; BIOS video E, write character
drawLoop:
          ; Row coordinate character
          mov AL, 30h   ; BIOS video function E, write single character and move cursor, print character 38h ('8')
          add AL, CL    ; (char)AL += count
          int 10h       ; BIOS video
          LPUTC 20h     ; BIOS video, write ' '
          ;
          ; Draw one row of the board
          ;
drawRow:
          ; Next pair
          lodsw          ; Get 2 pieces from board
          ; Print first piece
          push AX        ; Backup pieces
          and AL, 17h    ; Get owner and piece type
          XLATPUTC       ; Get character and write
          pop AX         ; Restore pieces
          xchg AL, AH    ; Swap first with second piece
          ; Print second piece
          and AX, 17h    ; Get owner and piece type
          XLATPUTC       ; Get character and write
          ; Next pair
          lodsw          ; Get 2 pieces from board
          ; Print third piece
          push AX        ; Backup pieces
          and AL, 17h    ; Get owner and piece type
          XLATPUTC       ; Get character and write
          pop AX         ; Restore pieces
          xchg AL, AH    ; Swap first with second piece
          ; Print fourth piece
          and AX, 17h    ; Get owner and piece type
          XLATPUTC       ; Get character and write
          ; Next pair
          lodsw          ; Get 2 pieces from board
          ; Print fifth piece
          push AX        ; Backup pieces
          and AL, 17h    ; Get owner and piece type
          XLATPUTC       ; Get character and write
          pop AX         ; Restore pieces
          xchg AL, AH    ; Swap first with second piece
          ; Print sixth piece
          and AX, 17h    ; Get owner and piece type
          XLATPUTC       ; Get character and write
          ; Next pair
          lodsw          ; Get 2 pieces from board
          ; Print seventh piece
          push AX        ; Backup pieces
          and AL, 17h    ; Get owner and piece type
          XLATPUTC       ; Get character and write
          pop AX         ; Restore pieces
          xchg AL, AH    ; Swap first with second piece
          ; Print eighth piece
          and AX, 17h    ; Get owner and piece type
          XLATPUTC       ; Get character and write
          ; Back to loop
          LPUTC 0Dh      ; Write Carriage Return
          LPUTC 0Ah      ; Write Line Feed
          ;dec CX         ; --count
          ;jnz drawLoop   ; Next row if unfinished
          loop drawLoop  ; Next row if unfinished (decrements CX)

          ;
          ; Display currently staged move
          ;
drawEnd:
          ; TODO: switch to MS-DOS calls, and stage characters in temp buffer so we can print a single string
          LPUTC 0Dh      ; Write Carriage Return
          LPUTC 0Ah      ; Write Line Feed
          LPUTC coords[0] ; Write source column
          LPUTC coords[1] ; Write source row
          LPUTC 20h      ; Write ' '
          LPUTC 74h      ; Write 't'
          LPUTC 6Fh      ; Write 'o'
          LPUTC 20h      ; Write ' '
          LPUTC coords[2] ; Write destination column
          LPUTC coords[3] ; Write destination row
          LPUTC 0Dh      ; Write Carriage Return
          LPUTC 0Ah      ; Write Line Feed

          ;
          ; Display a message based on last status, and reset status
          ;
          mov BX, [status]          ; Get current status
          mov BX, message_table[BX] ; Get address of relevant message
          mov AL, [BX]              ; Get next character to print
          mov AH, 0Eh               ; BIOS video function E, write character
drawString:
          int 10h                   ; BIOS video
          inc BX                    ; Next character (index)
          mov AL, [BX]              ; Get next character to print
          test AL, 0FFh             ; Apparently mov doesn't set/clear the Zero Flag...
          jnz drawString            ; Loop until null terminator
          mov status, 0             ; Reset status

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
      CRD2OFST B, coords[0] ; Get source coordinates and convert to board offset
      mov DL, board[BX]     ; Get source pieces from coordinates
      test DL, 07h          ; Get piece info
      jz validReturn        ; Invalid move if empty space
      mov DH, DL            ; Copy piece to DH
      and DH, 10h           ; Isolate player bit
      shr DH, 1             ; Move player bit to LSB
      shr DH, 1
      shr DH, 1
      shr DH, 1
      ; Check player owns piece
      cmp DH, [player]      ; Check if this piece is owned by the current player
      jne validSetAndReturn ; If not, then this move is not legal
      xor DH, DH            ; Clear upper byte
      push DX               ; Backup source piece

      ;
      ; Check if destination is now owned by current player
      ;
      CRD2OFST B, coords[2] ; Get destination coordinates and convert to board offset
      mov DL, board[BX]        ; Get two pieces from coordinates
      and DL, 07h              ; Get piece info
      ;push DL                  ; Backup destination piece
      jz validPieceJump        ; If destination is empty, skip following logic

      mov DH, DL               ; Copy piece to DH
      and DH, 10h              ; Isolate player bit
      shr DH, 1                ; Move player bit to LSB
      shr DH, 1
      shr DH, 1
      shr DH, 1
      ; Check player doesn't own piece
      cmp DH, [player]         ; Check if this piece is owned by the current player
      je validPopAndReturn     ; If so, then this move is not legal (otherwise this is a blank space, or an opponent's piece)

      ;
      ; Check if move is a valid chess play
      ;
validPieceJump:
      pop DX              ; Restore source piece
      mov BX, DX          ; Place in base register
      and BX, 7           ; Only use first 3 bits
      shl BX, 1           ; Multiply by 2, so it becomes a word offset
      jmp valid_table[BX] ; Allowing us to jump to the correct check in the code

      ;
      ; An empty space was owned by the second player (binary 1000) - this
      ; should NEVER happen!
      ; Or a space contained 111 (undefined piece) - which also shouldn't happen
      ;
validEmpty:
      xor DX, DX ; Set Zero Flag
      ret

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
      CRD2OFST B, coords[2] ; Get destination coordinates and convert to board offset
      mov DL, board[BX]              ; Get destination piece
      and DL, 07h                    ; Get piece data
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
      CRD2OFST B, coords[2] ; Get offset of destination piece
      mov DL, board[BX]              ; Place pieces in DL
      test DL, 07h                   ; Check if space has piece
      jz validReturn                 ; If not, the move is invalid
      ;shr DL, 1                      ; Move player bit to LSB (and shift out actual piece)
      ;shr DL, 1
      ;shr DL, 1
      ;shr DL, 1
      ;cmp DL, player[0]              ; Check if piece is owned by other player
      ret                            ; We can assume the destination is not
                                     ; owned by the player due to the global
                                     ; validation checks

validPawnDouble:
      ; Test if pawn is moving two spaces, from starting position
      cmp AH, pawnDoubleMove[BX]     ; Check if moving two spaces forward
      jne validSetAndReturn          ; If not, the move isn't valid
      mov DL, pawnStart[BX]          ; Get starting row for this player's pawns
      cmp DL, coords[1]              ; Check if pawn is in said row
      jne validSetAndReturn          ; If not, the move isn't valid
      ; Make sure path is not blocked
      CRD2OFST B, coords[0] ; Get offset of source piece
      mov AX, WORD PTR player        ; Get current player
      shl AX, 1                      ; Shift player bit into AX (so AX is now 0 or 16)
      shl AX, 1
      shl AX, 1
      shl AX, 1
      sub AX, 8                      ; Subtract 8 (so now AX is -8 or 8)
      ; Check if first space in front of pawn is empty
      add BX, AX                     ; Add this to the base register so we get the next row
      test board[BX], 07h            ; Check if space is empty
      jnz validSetAndReturn          ; If not zero, then a piece is impeding the progress of this pawn
      ; Check if second space in front of pawn is empty
      add BX, AX                     ; Get next row (again)
      test board[BX], 07h            ; Check if space is empty
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
      ; Currently implemented:
      ; - Move in valid L shape
      ;
validKnight:
      mov AX, WORD PTR coords[2] ; Get destination coordinates

      ; Verify valid X coordinate (-2, -1, 1, or 2)
      sub AL, coords[0]        ; Subtract source x coordinate
      je validReturn           ; Knight's destination cannot be same column as source
      jg validKnightPositiveX  ; Jump over this code if the difference is positive
      neg AL                   ; Turn negative into positive
validKnightPositiveX:
      cmp AL, 2                ; Check difference against 2
      jg validSetAndReturn     ; Knights cannot move more than 2 spaces in any direction

      ; Verify valid Y coordinate (-2, -1, 1, or 2)
      sub AH, coords[1]        ; Subtract source y coordinate
      je validReturn           ; Knight's destination cannot be same row as source
      jg validKnightPositiveY  ; Jump over this code if the difference is positive
      neg AH                   ; Turn negative into positive
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
