.8086

Stack    SEGMENT STACK
theStack DB 8 DUP ("(C) Matthew R.  ") ; 8 * 16 bytes
Stack    ENDS

Data    SEGMENT PUBLIC
board   DB 32 DUP (0)  ; 32 bytes, one nibble per board tile
                       ; 000 empty space
                       ; 001 pawn
                       ; 010 rook
                       ; 011 knight
                       ; 100 bishop
                       ; 101 queen
                       ; 110 king
                       ; fourth bit for player
player  DB 0           ; Current player
pieces  DB " PRNBQK?", ; White pieces
           " prnbqk?"  ; Black pieces
coords  DB "    "      ; 4 bytes for two sets of chess board coordinates
prompts DW prompt1,    ; Next parse source column
           prompt2,    ; Next parse source row
           prompt1,    ; Next parse destination column
           prompt2,    ; Next parse destination row
           prompt3     ; Next try move and reset
move    DB 0           ; Current step for player move selection
                       ; 0 - not started
                       ; 1 - got first letter
                       ; 2 - got first number (source tile selected)
                       ; 3 - got second letter
                       ; 4 - got second number (destination tile selected) - may not be used
Data    ENDS

Code SEGMENT PUBLIC

  assume CS:Code,DS:Data

main      PROC
start:    mov AX, Data
          mov DS, AX

          ;
          ; Initialize Board
          ;
          mov AX, 9999h      ; Four black pawns
          lea BX, board      ; Get address of board
          add BX, 4          ; (BX = board + 4) Second row
          mov [BX], AX       ; Place pawns
          add BX, 2          ; BX = board + 6
          mov [BX], AX       ; Place remaining pawns
          and AX, 7777h      ; Change to four white pawns
          add BX, 18         ; BX = board + 24
          mov [BX], AX       ; Place pawns
          add BX, 2          ; BX = board + 26
          mov [BX], AX       ; Place remaining pawns
          mov AX, 0DCBAh     ; Black rook, knight, bishop, and queen
          sub BX, 26         ; BX = board
          mov [BX], AX       ; Place black pieces
          and AX, 7777h      ; Change to white pieces
          add BX, 28         ; (BX = board + 28) Eighth row
          mov [BX], AX       ; Place white pieces
          mov AX, 2346h      ; White king, bishop, knight, and rook
          add BX, 2          ; BX = board + 30
          mov [BX], AX       ; Place remaining white pieces
          or  AX, 8888h      ; Change to black pieces
          sub BX, 28         ; (BX = board + 2) First row, second half
          mov [BX], AX       ; Place remaining black pieces
          ; Player should already be set to white by assembler

          ;
          ; Game loop
          ;
game:     call checkmate ; Check if player is checkmated
          jnz gameOver   ; If they are, show message, and then exit
          call check     ; Check if player is in check
          jz prompt      ; If not, skip the following instructions
          ;if they are, tell them

          ;
          ; User input
          ;
prompt:   call drawBoard ; Draw the chess board
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
          ; Read in a letter
prompt1:  sub AL, 61h        ; 61h = 'a'
          jl prompt          ; Try again
          sub AL, 8          ; AL -= 8
          jge prompt         ; Try again
          add AL, 69h        ; Restore column character
          shr BX, 1          ; Unshift from earlier jump
          mov coords[BX], AL ; Store in coordinate variable
          inc BL             ; Next step
          mov move, BL       ; Update in memory
          jmp prompt
          ; Read in a number
prompt2:  sub AL, 31h        ; 31h = '1'
          jl prompt          ; Try again
          sub AL, 8          ; AL -= 8
          jge prompt         ; Try again
          add AL, 39h        ; Restore row character
          shr BX, 1          ; Unshift from earlier jump
          inc BL             ; Next step
          mov move, BL       ; Update in memory
          dec BL             ; DL is either 2 or 4, so now it will be 1 or 3
          mov coords[BX], AL ; Store in coordinate variable
          jmp prompt
          ; Confirm move
prompt3:  mov move, 0        ; Set move step thingy to 0
          call valid         ; Check if move is valid (and if player is in check, make sure move gets them out of check)
          jz prompt
          ; make move
          xor [player], 1    ; change to other player's turn
          jmp game

          ;
          ; Game end
          ;
gameOver: mov AX, 4C00h
          int 21h
          ; EXIT 0
main      ENDP

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
drawAH:   inc AL        ; AL++
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
drawLoop: add CX, 2     ; Increment count by 2
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
drawRow:  mov BX, CX                  ; Use count as our offset TODO: possibly optimize so that count is already in BX?
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
drawEnd:  xor BX, BX        ; Page 0
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
      ; Invalid move
      mov WORD PTR coords[0], 2020h ; Clear source coordinate ("  ")
      mov WORD PTR coords[2], 2020h ; Clear destination coordinate
      xor DX, DX                    ; Set to 0 (to set Zero Flag)
      ret
valid ENDP

Code ENDS

  END start
