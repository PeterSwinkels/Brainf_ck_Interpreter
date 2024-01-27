'''OPTION EXPLICIT
DEFINT A-Z
CONST MEMORY_SIZE = &H7FFF

TYPE LoopStr
 LoopEnd AS INTEGER
 LoopStart AS INTEGER
END TYPE

DECLARE FUNCTION Escape (Text AS STRING) AS STRING
DECLARE FUNCTION LoadCode (Path AS STRING) AS STRING
DECLARE FUNCTION Loops (LoopList() AS LoopStr, InstructionP AS INTEGER, Backwards AS INTEGER) AS INTEGER
DECLARE FUNCTION NextArgument (Arguments AS STRING) AS INTEGER
DECLARE FUNCTION PopArgument (Arguments AS STRING, Argument AS STRING) AS STRING
DECLARE FUNCTION Unescape (Text AS STRING) AS STRING
DECLARE SUB Execute (Code AS STRING, InputLineBreak AS STRING, OutputLineBreak AS STRING)
DECLARE SUB GetLoopList (Code AS STRING, LoopList() AS LoopStr)
DECLARE SUB Main ()

CALL Main

FUNCTION Escape (Text AS STRING) AS STRING
DIM Character AS STRING * 1
DIM Escaped AS STRING
DIM Hexed AS STRING
DIM Position AS INTEGER

 Escaped = ""

 FOR Position = 1 TO LEN(Text)
  Character = MID$(Text, Position, 1)
  SELECT CASE Character
   CASE "/"
    Escaped = Escaped + "//"
   CASE " " TO "~"
    Escaped = Escaped + Character
   CASE ELSE
    Hexed = HEX$(ASC(Character))
    IF LEN(Hexed) = 1 THEN Hexed = "0" + Hexed
    Escaped = Escaped + "/" + Hexed
  END SELECT
 NEXT Position

 Escape = Escaped
END FUNCTION

SUB Execute (Code AS STRING, InputLineBreak AS STRING, OutputLineBreak AS STRING)
DIM Character AS STRING * 1
DIM InputBuffer AS STRING
DIM InstructionP AS INTEGER
DIM LoopList(0 TO 0) AS LoopStr
DIM Memory AS STRING
DIM MemoryP AS INTEGER
DIM OutputBuffer AS STRING
DIM UserInput AS STRING

 InstructionP = &H0
 GetLoopList Code, LoopList()
 Memory = STRING$(MEMORY_SIZE, &H0)
 MemoryP = &H0

 DO
  SELECT CASE MID$(Code, InstructionP + &H1, &H1)
   CASE ">"
    IF MemoryP >= LEN(Memory) THEN MemoryP = &H0 ELSE MemoryP = MemoryP + &H1
   CASE "<"
    IF MemoryP = &H0 THEN MemoryP = LEN(Memory) - &H1 ELSE MemoryP = MemoryP - &H1
   CASE "+"
    IF ASC(MID$(Memory, MemoryP + &H1, &H1)) = &HFF THEN
     MID$(Memory, MemoryP + &H1, &H1) = CHR$(&H0)
    ELSE
     MID$(Memory, MemoryP + &H1, &H1) = CHR$(ASC(MID$(Memory, MemoryP + &H1, &H1)) + &H1)
    END IF
   CASE "-"
    IF ASC(MID$(Memory, MemoryP + &H1, &H1)) = &H0 THEN
     MID$(Memory, MemoryP + &H1, &H1) = CHR$(&HFF)
    ELSE
     MID$(Memory, MemoryP + &H1, &H1) = CHR$(ASC(MID$(Memory, MemoryP + &H1, &H1)) - &H1)
    END IF
   CASE "."
    Character = MID$(Memory, MemoryP + &H1, &H1)
   
    IF OutputLineBreak = "" THEN
     PRINT Escape(Character);
    ELSE
     OutputBuffer = OutputBuffer + Character
     
     IF NOT LEFT$(OutputLineBreak, LEN(OutputBuffer)) = OutputBuffer THEN
      PRINT Escape(OutputBuffer);
      OutputBuffer = ""
     ELSEIF OutputBuffer = OutputLineBreak THEN
      PRINT
      OutputBuffer = ""
     END IF
    END IF
   CASE ","
    IF InputBuffer = "" THEN
     LINE INPUT UserInput
     InputBuffer = Unescape(UserInput) + InputBuffer + InputLineBreak
    END IF
    
    IF NOT InputBuffer = "" THEN
     MID$(Memory, MemoryP + &H1, &H1) = LEFT$(InputBuffer, 1)
     InputBuffer = MID$(InputBuffer, 2)
    END IF
   CASE "["
    IF ASC(MID$(Memory, MemoryP + &H1, &H1)) = &H0 THEN
     InstructionP = Loops(LoopList(), InstructionP, 0)
    END IF
   CASE "]"
    IF NOT ASC(MID$(Memory, MemoryP + &H1, &H1)) = &H0 THEN
     InstructionP = Loops(LoopList(), InstructionP, -1)
    END IF
  END SELECT

  InstructionP = InstructionP + &H1
 LOOP WHILE InstructionP > &H0 AND InstructionP < LEN(Code)
END SUB

SUB GetLoopList (Code AS STRING, LoopList() AS LoopStr)
DIM Character AS STRING * 1
DIM LoopCount AS INTEGER
DIM LoopStack(0 TO 0) AS INTEGER
DIM LoopStackSize AS INTEGER
DIM Position AS INTEGER
DIM StartOfLoop AS INTEGER

 LoopCount = 0
 LoopStackSize = 0
 FOR Position = 1 TO LEN(Code)
  Character = MID$(Code, Position, 1)
  SELECT CASE Character
   CASE "["
    REDIM PRESERVE LoopStack(LBOUND(LoopStack) TO LoopStackSize) AS INTEGER
    LoopStack(UBOUND(LoopStack)) = Position - 1
    LoopStackSize = LoopStackSize + 1
   CASE "]"
    IF LoopStackSize = 0 THEN
     PRINT "End of loop without start."
     EXIT FOR
    ELSE
     StartOfLoop = LoopStack(UBOUND(LoopStack))
     LoopStackSize = LoopStackSize - 1
     REDIM PRESERVE LoopStack(LBOUND(LoopStack) TO LoopStackSize) AS INTEGER
     LoopCount = LoopCount + 1
     REDIM PRESERVE LoopList(LBOUND(LoopList) TO LoopCount) AS LoopStr
     LoopList(UBOUND(LoopList)).LoopStart = StartOfLoop
     LoopList(UBOUND(LoopList)).LoopEnd = Position - 1
    END IF
  END SELECT
 NEXT Position

 IF NOT LoopStackSize = 0 THEN
  PRINT "Loop without end."
 END IF

END SUB

FUNCTION LoadCode (Path AS STRING) AS STRING
DIM Code AS STRING
DIM FileH AS INTEGER

 IF DIR$(Path) = "" THEN ERROR 53

 FileH = FREEFILE
 OPEN Path FOR BINARY LOCK READ WRITE AS FileH
  Code = INPUT$(LOF(FileH), FileH)
 CLOSE FileH

 LoadCode = Code
END FUNCTION

FUNCTION Loops (LoopList() AS LoopStr, InstructionP AS INTEGER, Backwards AS INTEGER) AS INTEGER
DIM NewInstructionP AS INTEGER
DIM Position AS INTEGER

 FOR Position = LBOUND(LoopList) TO UBOUND(LoopList)
  SELECT CASE Backwards
   CASE 0
    IF InstructionP = LoopList(Position).LoopStart THEN
     NewInstructionP = LoopList(Position).LoopEnd
     EXIT FOR
    END IF
   CASE -1
    IF InstructionP = LoopList(Position).LoopEnd THEN
     NewInstructionP = LoopList(Position).LoopStart
     EXIT FOR
    END IF
  END SELECT
 NEXT Position

 Loops = NewInstructionP
END FUNCTION

SUB Main ()
DIM Arguments AS STRING
DIM InputLineBreak AS STRING
DIM OutputLineBreak AS STRING
DIM Path AS STRING

 Arguments = LTRIM$(RTRIM$(COMMAND$))
 InputLineBreak = CHR$(13)
 OutputLineBreak = CHR$(13)

 IF NOT Arguments = "" THEN
  Arguments = PopArgument(Arguments, Path)
  IF NOT Arguments = "" THEN
   Arguments = PopArgument(Arguments, InputLineBreak)
   IF NOT Arguments = "" THEN
    Arguments = PopArgument(Arguments, OutputLineBreak)
   END IF
  END IF
 END IF

 IF Path = "" THEN
  PRINT "Brainf*ck Interpreter v1.00, by: Peter Swinkels, ***2023***"
  PRINT
  PRINT "Usage:"
  PRINT "BFInterp.exe PATH LINE_BREAK_IN LINE_BREAK_OUT"
 ELSE
  Execute LoadCode(Path), Unescape(InputLineBreak), Unescape(OutputLineBreak)
 END IF
END SUB

FUNCTION NextArgument (Arguments AS STRING)
DIM Position AS INTEGER
 
 Position = INSTR(1, Arguments, " ")
 IF Position = 0 THEN Position = LEN(Arguments) + 1
 IF Position = 1 THEN Position = 0

 NextArgument = Position
END FUNCTION

FUNCTION PopArgument (Arguments AS STRING, Argument AS STRING) AS STRING
DIM NextPosition AS INTEGER

 Arguments = LTRIM$(RTRIM$(Arguments))
 NextPosition = NextArgument(Arguments)
 IF NextPosition > 0 THEN
  Argument = LEFT$(Arguments, NextPosition - 1)
  Arguments = MID$(Arguments, NextPosition + 1)
 END IF

 PopArgument = Arguments
END FUNCTION

FUNCTION Unescape (Text AS STRING) AS STRING
DIM Character AS STRING * 1
DIM Position AS INTEGER
DIM Unescaped AS STRING

 Position = 1
 Unescaped = ""
 DO UNTIL Position > LEN(Text)
  Character = MID$(Text, Position, 1)
  IF Character = "/" THEN
   IF MID$(Text, Position + 1, 1) = "/" THEN
    Unescaped = Unescaped + Character
    Position = Position + 2
   ELSE
    Unescaped = Unescaped + CHR$(VAL("&H" + MID$(Text, Position + 1, 2) + "%"))
    Position = Position + 3
   END IF
  ELSE
   Unescaped = Unescaped + Character
   Position = Position + 1
  END IF
 LOOP

 Unescape = Unescaped
END FUNCTION

