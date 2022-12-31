MAIN = chess

ASM_DIR = src
OBJ_DIR = build
SOURCES =

AFLAGS = #/Imacros

all: $(OBJ_DIR) $(MAIN).exe

$(OBJ_DIR):
	@-mkdir $@ > nul

# MS-DOS 1+, IBM PC Compatible Program
$(MAIN).exe: $(OBJ_DIR)\$$(@B).obj #$(OBJ_DIR)\$(SOURCES:.asm=.obj)
	LINK $**,$@;

{$(ASM_DIR)}.asm{$(OBJ_DIR)}.obj:
	$(AS) $(AFLAGS) /Fo$@ /c $<

clean:
	-del $(MAIN).exe > nul
	-del $(OBJ_DIR)\*.obj > nul
