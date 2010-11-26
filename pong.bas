DECLARE SUB CalcDrawSpeed ()
DECLARE SUB HelpDisplay ()
DECLARE SUB InfoDisplay ()
DECLARE SUB ProcessBoundaries ()
DECLARE SUB ProcessPaddles ()
DECLARE SUB GameConstants ()
DECLARE SUB DottedLineX (x1%, x2%, y%)
DECLARE SUB ProcessWalls ()
DECLARE SUB GetScreenLines (i%, kind%, x1%, y1%, x2%, y2%)
DECLARE FUNCTION ScreenLines% (i%, kind%, x1%, y1%, x2%, y2%)
DECLARE SUB MoveBall2 ()
DECLARE SUB DrawScreen ()
DECLARE SUB FixBounceMode ()
DECLARE SUB InitGame ()
DECLARE SUB ExitProggy ()
DECLARE SUB PaddleSound ()
DECLARE SUB DrawLines ()
DECLARE SUB DrawPaddles ()
DECLARE SUB DrawPaddle (x%, y%)
DECLARE SUB EachCycle ()
DECLARE SUB ParseKeys ()
DECLARE SUB ResetButton ()
DECLARE SUB ScorePoint ()
DECLARE SUB PrintNumber (x%, y%, number$)
DECLARE SUB DrawScores ()
DECLARE SUB DottedLineY (x%, y1%, y2%)
DECLARE SUB Delay (length#)
DECLARE SUB PrintDigit (x%, y%, digit%)
DECLARE SUB DrawBall (x AS SINGLE, y AS SINGLE)
DECLARE SUB CalcDelayLoop ()
DECLARE SUB MoveBall ()
DECLARE SUB WallSound ()
DECLARE SUB ScoreSound ()
DECLARE SUB ClearBall ()
' ********************
' History:
' **********
' 1.0.3  - Increased Rate range.
' **********
' 1.0.2  - Some bug fixes
'        - Added compensation for slow graphics.  Calculates drawing speed
'          and adjusts *Vel[XY] accordingly.
' **********
' 1.0.1  - Some bug fixes
' **********
' 1.0.0  - First Release
' **********
' 0.7.01 - Set advanced ball mode as default
'        - Added Hockey (GM4)
'        - Rearranged drawing order to fit reasonable priorities
'        - Added variable MaxScore
' **********
' 0.5.04 - Added Sound/Mute option
'        - Added Keyboard Help
' **********
' 0.5.03 - Fixed GM2 scoring for player 2
' **********
' 0.5.02 - Fixed GM3 reverse scoring
' **********
' 0.5.01 - Major rewrite of key routines to reduce redundancy of code
'        - Added pause key with status display
'        - Added different ball types
'        - Added alternate score font
'        - I think I've fixed most bugs
' **********
' 0.3.05 - Fixed scoring bug with game mode switch
'        - Fixed GM3 double reset
'        - Fixed Hockey-like deflection in table tennis
'        - Fixed bouncing on top & bottom walls
' **********
' 0.3.04 - Started this log.
'        - Fixed a retarded bug in handball(GM3) that caused the ball to not
'          bounce properly
' ********************

DEFINT A-Z
CONST True = -1
CONST False = 0

CONST LoopAccuracy = 2
CONST BallSize = 8
CONST LineSize = 5
CONST ProPaddleThresh = 0


CONST LineSolidXTop = 1     'a reflective wall
CONST LineSolidXBottom = 2     'a reflective wall
CONST LineSolidYRight = 3     'a reflective wall
CONST LineSolidYLeft = 4
CONST LineDottedY = 5   'Has no effect of the ball
CONST LineDottedX = 6

CONST bitsperpixelperplane = 1
CONST planes = 4
CONST ScrWidth = 320
CONST ScrHeight = 200
CONST ScrMax = 108

BallArrayBytes = 4 + INT(((BallSize - 0 + 1) * (bitsperpixelperplane) + 7) / 8) * planes * ((BallSize - 0) + 1)
DIM SHARED NormalVelX AS SINGLE
DIM SHARED NormalVelY AS SINGLE
DIM SHARED BigVelX AS SINGLE
DIM SHARED BigVelY AS SINGLE
DIM SHARED LastBallX AS SINGLE, LastBallY AS SINGLE
DIM SHARED LoopsPerSecond AS LONG
DIM SHARED DrawsPerSecond AS LONG
DIM SHARED BallX AS SINGLE, BallY AS SINGLE, BallVisible AS INTEGER, BallType AS INTEGER
DIM SHARED Velocity AS SINGLE, VelX AS SINGLE, VelY AS SINGLE
DIM SHARED GameMode
DIM SHARED Score(1), WhoScores
DIM SHARED BallArray(BallArrayBytes * 2) AS INTEGER
DIM SHARED Paddle(1), PaddleX(1), PaddleX2(1), PaddleSize, PaddleJump, JustInAPaddle AS INTEGER
DIM SHARED BounceMode
DIM SHARED GamePaused AS INTEGER, HelpShown AS INTEGER
DIM SHARED FontType AS INTEGER
DIM SHARED SoundEnabled AS INTEGER
DIM SHARED MaxScore AS INTEGER

'BounceMode = 0  'Simple
BounceMode = 1  'Advanced

GameMode = 1            'Table Tennis
'GameMode = 2            'HandBall - Single Player
'GameMode = 3            'HandBall - Two Player
'GameMode = 4          'Hockey

BallType = 0            'Classic
'BallType = 1            'Classic Outline
'BallType = 2            'Round
'BallType = 3            'Round Outline
'BallType = 4            'Triangle
'BallType = 5            'Triangle Outline

FontType = 0            ' Original
'FontType = 1            ' Alternate

MaxScore = 15

NormalVelX = .8
NormalVelY = .2
BigVelX = .6
BigVelY = .9

SoundEnabled = True
HelpShown = False
RANDOMIZE TIMER
SCREEN 0
CLS
COLOR 9
PRINT "PONG v1.0.3"
PRINT "Copyright (c) 1998-2000, Dwayne Litzenberger"
PRINT
COLOR 4
PRINT "This program is free software; you can redistribute it and/or modify"
PRINT "it under the terms of the GNU General Public License as published by"
PRINT "the Free Software Foundation; either version 2 of the License, or"
PRINT "(at your option) any later version."
PRINT
PRINT "This program is distributed in the hope that it will be useful,"
PRINT "but WITHOUT ANY WARRANTY; without even the implied warranty of"
PRINT "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
PRINT "GNU General Public License for more details."
PRINT ""
PRINT "You should have received a copy of the GNU General Public License"
PRINT "along with this program; if not, write to the Free Software"
PRINT "Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA"
PRINT "or, see http://www.fsf.org/copyleft/gpl.html"
PRINT
COLOR 7
PRINT "- Press any key - "
WHILE INKEY$ = "": WEND
SCREEN 7
COLOR 7
CLS
PRINT "Calculating delay loop...";
CALL CalcDelayLoop
PRINT "done"
PRINT "Calculating drawing speed...";
CALL CalcDrawSpeed
PRINT "done"
SLEEP 4
COLOR 15
CLS
GamePaused = False
Velocity = 1
PaddleSize = 40
PaddleJump = 10
WhoScores = 0
Score(0) = MaxScore: Score(1) = MaxScore - 3
Paddle(0) = ScrHeight / 2 - 1
Paddle(1) = ScrHeight / 2 - 1
CALL InitGame
t# = TIMER
WHILE 1         ' Infinite loop
 IF (NOT GamePaused) AND (NOT HelpShown) THEN CALL MoveBall
 IF (NOT GamePaused) AND (NOT HelpShown) THEN Delay .003 / Velocity
 CALL ParseKeys
 IF (NOT GamePaused) AND (NOT HelpShown) AND (TIMER - t# > 3) THEN CALL DrawScores: t# = TIMER     'Do a bit of screen refresh every few seconds
WEND
END


numfont:
DATA "XXX"
DATA "X X"
DATA "X X"
DATA "X X"
DATA "XXX"

DATA "  X"
DATA "  X"
DATA "  X"
DATA "  X"
DATA "  X"

DATA "XXX"
DATA "  X"
DATA "XXX"
DATA "X  "
DATA "XXX"

DATA "XXX"
DATA "  X"
DATA " XX"
DATA "  X"
DATA "XXX"

DATA "X  "
DATA "X X"
DATA "XXX"
DATA "  X"
DATA "  X"

DATA "XXX"
DATA "X  "
DATA "XXX"
DATA "  X"
DATA "XXX"

DATA "X  "
DATA "X  "
DATA "XXX"
DATA "X X"
DATA "XXX"

DATA "XXX"
DATA "  X"
DATA "  X"
DATA "  X"
DATA "  X"

DATA "XXX"
DATA "X X"
DATA "XXX"
DATA "X X"
DATA "XXX"

DATA "XXX"
DATA "X X"
DATA "XXX"
DATA "  X"
DATA "  X"

altnumfont:
DATA " X "
DATA "X X"
DATA "X X"
DATA "X X"
DATA " X "

DATA "  X"
DATA " XX"
DATA "  X"
DATA "  X"
DATA "  X"

DATA "XX "
DATA "  X"
DATA " X "
DATA "X  "
DATA "XXX"

DATA "XX "
DATA "  X"
DATA " X "
DATA "  X"
DATA "XX "

DATA "X X"
DATA "X X"
DATA "XXX"
DATA "  X"
DATA "  X"

DATA "XXX"
DATA "X  "
DATA "XX "
DATA "  X"
DATA "XX "

DATA " X "
DATA "X  "
DATA "XX "
DATA "X X"
DATA " X "

DATA "XXX"
DATA "  X"
DATA " X "
DATA " X "
DATA " X "

DATA " X "
DATA "X X"
DATA " X "
DATA "X X"
DATA " X "

DATA " X "
DATA "X X"
DATA " XX"
DATA "  X"
DATA "  X"

SUB CalcDelayLoop
 t# = TIMER
 tt& = 0
 WHILE TIMER - t# < LoopAccuracy
  tt& = tt& + 1
 WEND
 LoopsPerSecond = tt& / LoopAccuracy
END SUB

SUB CalcDrawSpeed
 t# = TIMER
 tt& = 0
 WHILE TIMER - t# < LoopAccuracy
  LINE (50, 50)-(90, 90), 0, BF
  tt& = tt& + 1
 WEND
 DrawsPerSecond = tt& / LoopAccuracy
 Multiplier! = 30 * (170 / tt&)
 NormalVelX = NormalVelX * Multiplier!
 NormalVelY = NormalVelY * Multiplier!
 BigVelX = BigVelX * Multiplier!
 BigVelY = BigVelY * Multiplier!
END SUB

SUB ClearBall
 IF BallVisible THEN
  xx = LastBallX - BallSize / 2: yy = LastBallY - BallSize / 2
  IF xx >= 0 AND yy >= 0 THEN
'  IF xx >= 0 AND yy >= 0 AND xx <= ScrWidth - 1 - BallSize AND yy <= ScrHeight - 1 - BallSize THEN
   IF xx <= ScrWidth - 1 AND yy <= ScrHeight - 1 THEN PUT (xx, yy), BallArray, PSET
'  ELSEIF xx >= 0 AND yy >= 0 THEN
   'LINE (xx, yy)-STEP(BallSize, BallSize), 0, BF
  END IF
 END IF
 BallVisible = False
END SUB

SUB Delay (length#)
 tt& = 0
 WHILE (length# * LoopsPerSecond) > tt&
  dummy# = TIMER
  tt& = tt& + 1
 WEND
END SUB

SUB DottedLineX (x1, x2, y)
 FOR i = x1 TO x2 STEP 20
  LINE (i, y - 5 / 2)-STEP(10, 5), 15, BF
 NEXT
END SUB

SUB DottedLineY (x, y1, y2)
 FOR i = y1 TO y2 STEP 20
  LINE (x - 5 / 2, i)-STEP(5, 10), 15, BF
 NEXT
END SUB

SUB DrawBall (x AS SINGLE, y AS SINGLE)
 CALL ClearBall
 xx = x - BallSize / 2: yy = y - BallSize / 2
 IF xx >= 0 AND yy >= 0 THEN
  xd = xx + BallSize: yd = yy + BallSize
  IF xd > ScrWidth - 1 THEN xd = ScrWidth - 1
  IF yd > ScrHeight - 1 THEN yd = ScrHeight - 1
  IF xx <= ScrWidth - 1 AND yy <= ScrHeight - 1 THEN GET (xx, yy)-(xd, yd), BallArray
  SELECT CASE BallType
   CASE 0
    LINE (xx, yy)-STEP(BallSize, BallSize), 15, BF
   CASE 1
    LINE (xx, yy)-STEP(BallSize, BallSize), 15, B
   CASE 2
    CIRCLE (x, y), BallSize / 2, 15, , , .88
    PAINT (x, y), 15, 15
   CASE 3
    CIRCLE (x, y), BallSize / 2, 15, , , .88
   CASE 4
    LINE (x, y - BallSize / 2)-(x - BallSize / 2, y + BallSize / 2), 15
    LINE -(x + BallSize / 2, y + BallSize / 2), 15
    LINE -(x, y - BallSize / 2), 15
    PAINT (x, y), 15, 15
   CASE 5
    LINE (x, y - BallSize / 2)-(x - BallSize / 2, y + BallSize / 2), 15
    LINE -(x + BallSize / 2, y + BallSize / 2), 15
    LINE -(x, y - BallSize / 2), 15
  END SELECT
 END IF
 LastBallX = x: LastBallY = y
 BallVisible = True
END SUB

SUB DrawLines
 DIM i, k, x1, y1, x2, y2
 i = 0
 DO
  GetScreenLines i, k, x1, y1, x2, y2
  SELECT CASE k
   CASE LineSolidX, LineSolidXTop, LineSolidXBottom, LineSolidYLeft
    LINE (x1, y1)-(x2, y2), 15, BF
   CASE LineDottedY
    CALL DottedLineY(x1, y1, y2)
   CASE LineDottedX
    CALL DottedLineX(x1, x2, y1)
  END SELECT
  i = i + 1
 LOOP UNTIL k = 0
END SUB

SUB DrawPaddle (x%, y%)
        LINE (x% - LineSize / 2, 0)-STEP(LineSize, ScrHeight), 0, BF
        LINE (x% - LineSize / 2, y% - PaddleSize / 2)-STEP(LineSize, PaddleSize), 15, BF
        CALL DrawLines
END SUB

SUB DrawPaddles
 SELECT CASE GameMode
  CASE 1, 3
   CALL DrawPaddle(PaddleX(0), Paddle(0))
   CALL DrawPaddle(PaddleX(1), Paddle(1))
  CASE 2
   CALL DrawPaddle(PaddleX(0), Paddle(0))
  CASE 4
   CALL DrawPaddle(PaddleX(0), Paddle(0))
   CALL DrawPaddle(PaddleX2(0), Paddle(0))
   CALL DrawPaddle(PaddleX(1), Paddle(1))
   CALL DrawPaddle(PaddleX2(1), Paddle(1))
 END SELECT
END SUB

SUB DrawScores
 n$ = RTRIM$(LTRIM$(STR$(Score(0))))
 IF Score(0) < 10 THEN n$ = "*" + n$
 CALL PrintNumber((ScrWidth - 1) / 2 + 14, 10, n$)
 IF GameMode <> 2 THEN
  n$ = RTRIM$(LTRIM$(STR$(Score(1))))
  IF Score(1) < 10 THEN n$ = "*" + n$
  CALL PrintNumber((ScrWidth - 1) / 2 - 5 * 7 - 10, 10, n$)
 END IF
END SUB

SUB DrawScreen
 CALL DrawLines
 CALL DrawPaddles
 CALL DrawBall(BallX, BallY)
 CALL DrawScores
END SUB

SUB ExitProggy
SCREEN 2
SCREEN 0
COLOR 7, 0
CLS
COLOR 9, 0
PRINT "Thank you for playing PONG!"
PRINT "Have a nice day!"
COLOR 7, 0
Delay 1.5
END

END SUB

SUB FixBounceMode
 SELECT CASE BounceMode
  CASE 0
   VelX = NormalVelX * SGN(VelX)
   VelY = NormalVelY * SGN(VelY)
  CASE 1
   IF Score(0) >= MaxScore OR Score(1) >= MaxScore THEN
    VelX = BigVelX * SGN(VelX)
    VelY = BigVelY * SGN(VelY)
   END IF
 END SELECT
END SUB

SUB GameConstants
 SELECT CASE GameMode
  CASE 1
   PaddleX(0) = ScrWidth - 1 - 8
   PaddleX(1) = 8
  CASE 2
   PaddleX(0) = ScrWidth - 1 - 20
  CASE 3
   PaddleX(0) = ScrWidth - 1 - 20
   PaddleX(1) = ScrWidth - 1 - 20 - (LineSize * 2)
  CASE 4
   PaddleX(0) = ScrWidth - 1 - 8
   PaddleX(1) = 8
   PaddleX2(0) = ScrWidth / 2 - 1 + 50
   PaddleX2(1) = ScrWidth / 2 - 1 - 50

 END SELECT
END SUB

SUB GetScreenLines (i, kind, x1, y1, x2, y2)
 SELECT CASE GameMode
  CASE 1, 4       ' Table Tennis & Hockey
   SELECT CASE i
    CASE 0
     kind = LineSolidXTop
     x1 = 0 - ScrMax
     y1 = 0
     x2 = ScrWidth - 1 + ScrMax
     y2 = LineSize
    CASE 1
     kind = LineSolidXBottom
     x1 = 0
     y1 = ScrHeight - LineSize - 1
     x2 = ScrWidth - 1
     y2 = y1 + LineSize
    CASE 2
     kind = LineDottedY
     x1 = (ScrWidth - 1) / 2
     y1 = 0
     y2 = ScrHeight - 1
     x2 = 0 'Unused
    CASE ELSE
     kind = 0 ' Finished
     x1 = 0: y1 = x1: x2 = x1: y2 = x1 ' Null Variables
   END SELECT
 
  CASE 2, 3     ' Handball
   SELECT CASE i
    CASE 0
     kind = LineSolidXTop
     x1 = 0
     y1 = 0
     x2 = ScrWidth - 1
     y2 = LineSize
    CASE 1
     kind = LineSolidXBottom
     x1 = 0
     y1 = ScrHeight - LineSize - 1
     x2 = ScrWidth - 1
     y2 = y1 + LineSize
    CASE 2
     kind = LineSolidYLeft
     x1 = 0
     y1 = 0
     x2 = LineSize
     y2 = ScrHeight - 1
    CASE ELSE
     kind = 0 ' Finished
     x1 = 0: y1 = x1: x2 = x1: y2 = x1 ' Null Variables
   END SELECT
 END SELECT

END SUB

SUB HelpDisplay
 LOCATE 1, 2
 PRINT "ÕÍÍÍÍÍÍÍÍÍÍµKeyboard HelpÆÍÍÍÍÍÍÍÍÍÍÍ¸"
 FOR i = 2 TO 23
  LOCATE i, 2
  PRINT "³"; SPACE$(36); "³"
 NEXT
 LOCATE , 2: PRINT "À"; STRING$(36, "Ä"); "Ù";

 LOCATE 2, 1
 LOCATE , 4: PRINT "(A) Left Paddle Up"
 LOCATE , 4: PRINT "(Z) Left Paddle Down"
 LOCATE , 4: PRINT "(S) Left Paddle Up Faster"
 LOCATE , 4: PRINT "(X) Left Paddle Down Faster"
 PRINT
 LOCATE , 4: PRINT "(-) Right Paddle Up"
 LOCATE , 4: PRINT "(+) Right Paddle Down"
 LOCATE , 4: PRINT "(*) Right Paddle Up Faster"
 LOCATE , 4: PRINT "(9)/(6) Right Paddle Down Faster"
 PRINT
 LOCATE , 4: PRINT "(H) Pause / Keyboard Help"
 LOCATE , 4: PRINT "(P) Pause / Show Status"
 LOCATE , 4: PRINT "(SPACE) Reset/Start Game"
 LOCATE , 4: PRINT "(ESC) Quit"
 PRINT
 LOCATE , 4: PRINT "(G) Game Mode"
 LOCATE , 4: PRINT "(B) Bounce Mode"
 LOCATE , 4: PRINT "(T) Ball Type"
 LOCATE , 4: PRINT "(F) Font Type"
 LOCATE , 4: PRINT "(R) Ball Rate / Game Speed"
 LOCATE , 4: PRINT "(O) Sound On/Off"
 LOCATE , 4: PRINT "(M) Change Maximum Score"

END SUB

SUB InfoDisplay
LOCATE 7, 3
PRINT "***************PAUSED****************"
FOR i = 8 TO 17
 LOCATE i, 3
 PRINT "*"; SPACE$(35); "*"
NEXT
LOCATE , 3: PRINT STRING$(37, "*")
LOCATE , 3: PRINT "*"; SPACE$(35); "*"; : LOCATE , 5: PRINT "Press <H> for help"
LOCATE , 3: PRINT STRING$(37, "*")

LOCATE 8, 1

LOCATE , 5: PRINT "PONG"
LOCATE , 5: PRINT "(c) 1998-2000, Dwayne Litzenberger"
LOCATE , 4: PRINT STRING$(35, "*")

LOCATE , 5: PRINT "GameMode = ";
SELECT CASE GameMode
 CASE 1
  PRINT "Table Tennis"
 CASE 2
  PRINT "Handball - One Player"
 CASE 3
  PRINT "Handball - Two Players"
 CASE 4
  PRINT "Hockey"
END SELECT

LOCATE , 5: PRINT "BounceMode = ";
SELECT CASE BounceMode
 CASE 0
  PRINT "Simple"
 CASE 1
  PRINT "Advanced"
END SELECT

LOCATE , 5: PRINT USING "BallRate = #.#"; Velocity

LOCATE , 5: PRINT "BallType = ";
SELECT CASE BallType
 CASE 0
  PRINT "Classic"
 CASE 1
  PRINT "Classic Outline"
 CASE 2
  PRINT "Round"
 CASE 3
  PRINT "Round Outline"
 CASE 4
  PRINT "Triangle"
 CASE 5
  PRINT "Triangle Outline"
END SELECT

LOCATE , 5: PRINT "Font = ";
SELECT CASE FontType
 CASE 0
  PRINT "Original"
 CASE 1
  PRINT "Alternate"
END SELECT

LOCATE , 5: PRINT "Sound = ";
SELECT CASE SoundEnabled
 CASE True
  PRINT "On"
 CASE False
  PRINT "Muted"
END SELECT

LOCATE , 5: PRINT USING "MaxScore = ##"; MaxScore

END SUB

SUB InitGame
 DEF SEG = 0
 POKE 1047, PEEK(1047) OR 32
 DEF SEG
 CALL GameConstants
 SELECT CASE GameMode
  CASE 1, 2, 3, 4
   VelX = -NormalVelX
   VelY = -NormalVelY
   BallX = 250: BallY = 110
   WhoScores = 0
   JustInAPaddle = False
 END SELECT
 CALL DrawScreen
END SUB

SUB MoveBall
 DIM OldVelX AS SINGLE, OldVelY AS SINGLE
 BallX = BallX + Velocity * VelX
 BallY = BallY + Velocity * VelY
 CALL ProcessWalls
 CALL ProcessPaddles
 CALL ProcessBoundaries
 CALL DrawBall(BallX, BallY)
END SUB

SUB PaddleSound
 IF SoundEnabled THEN SOUND 750, .3
END SUB

SUB ParseKeys
 i$ = ""
 iterations = 0
 DO
  i$ = UCASE$(INKEY$)
  SELECT CASE i$
   CASE CHR$(27)
    CALL ExitProggy
   CASE CHR$(32)
    CALL ResetButton
   CASE "G"
    IF SoundEnabled THEN SOUND 2000, .1
    GameMode = GameMode + 1
    IF GameMode > 4 THEN GameMode = 1
    CALL GameConstants: CLS : CALL DrawScreen
   CASE "B"
    IF SoundEnabled THEN SOUND 2000, .1
    BounceMode = BounceMode + 1
    IF BounceMode > 1 THEN BounceMode = 0
    CALL FixBounceMode
   CASE "R"
    IF SoundEnabled THEN SOUND 2000, .1
    Velocity = Velocity + .2
    IF Velocity > 3 THEN Velocity = .2
   CASE "T"
    IF SoundEnabled THEN SOUND 2000, .1
    BallType = BallType + 1
    IF BallType > 5 THEN BallType = 0
   CASE "F"
    IF SoundEnabled THEN SOUND 2000, .1
    FontType = FontType + 1
    IF FontType > 1 THEN FontType = 0
    CALL DrawScores
   CASE "M"
    IF SoundEnabled THEN SOUND 2000, .1
    MaxScore = MaxScore + 1
    IF MaxScore > 30 THEN MaxScore = 1
   CASE "P"
    IF SoundEnabled THEN SOUND 1200, 1: SOUND 600, 1
    IF GamePaused THEN
     CLS : CALL DrawScreen
    END IF
    GamePaused = True - GamePaused  'Reverse Pause status
   CASE "H"
    IF SoundEnabled THEN SOUND 2000, .1
    IF HelpShown THEN
     CLS : CALL DrawScreen
    END IF
    HelpShown = True - HelpShown
   CASE "O"
    SoundEnabled = True - SoundEnabled
    IF SoundEnabled THEN SOUND 2000, .1
  END SELECT
  SELECT CASE GameMode
   CASE 1, 2, 3, 4
    SELECT CASE i$
     CASE "A"
      Paddle(1) = Paddle(1) - PaddleJump
      CALL DrawPaddles
     CASE "Z"
      Paddle(1) = Paddle(1) + PaddleJump
      CALL DrawPaddles
     CASE "S"
      Paddle(1) = Paddle(1) - PaddleJump * 2
      CALL DrawPaddles
     CASE "X"
      Paddle(1) = Paddle(1) + PaddleJump * 2
      CALL DrawPaddles
      
     CASE "-"
      Paddle(0) = Paddle(0) - PaddleJump
      CALL DrawPaddles
     CASE "+"
      Paddle(0) = Paddle(0) + PaddleJump
      CALL DrawPaddles
     CASE "*"
      Paddle(0) = Paddle(0) - PaddleJump * 2
      CALL DrawPaddles
     CASE "6", "9", CHR$(0) + "M", CHR$(0) + "I"
      Paddle(0) = Paddle(0) + PaddleJump * 2
      CALL DrawPaddles
     
    END SELECT
  END SELECT
  IF GamePaused AND i$ <> "" THEN
   CALL DrawScreen
  END IF
  IF i$ <> "" AND GameMode = 4 THEN CALL DrawScores
  IF i$ <> "" AND GamePaused THEN CALL InfoDisplay
  IF i$ <> "" AND HelpShown THEN CALL HelpDisplay
  iterations = iterations + 1
 LOOP UNTIL i$ = "" OR iterations > 1
END SUB

SUB PrintDigit (x%, y%, digit%)
 SELECT CASE FontType
  CASE 0
   RESTORE numfont
  CASE 1
   RESTORE altnumfont
 END SELECT
 FOR i = 1 TO digit%                    ' Skip unwanted numbers
  READ z$, z$, z$, z$, z$
 NEXT
 FOR yy = 1 TO 5
  READ n$
  FOR xx = 1 TO 3
   a$ = MID$(n$, xx, 1)
   IF a$ = "X" THEN c = 15 ELSE c = 0
   LINE (x% + (xx - 1) * 5, y% + (yy - 1) * 5)-STEP(4, 4), c, BF
  NEXT
 NEXT
END SUB

SUB PrintNumber (x%, y%, number$)
 n$ = RTRIM$(LTRIM$(number$))
 FOR i = 1 TO LEN(n$)
  d$ = MID$(n$, i, 1)
  IF d$ >= "0" AND d$ <= "9" THEN
   digit% = VAL(d$)
   CALL PrintDigit(x% + 5 * 4 * (i - 1), y%, digit%)
  ELSE
   LINE (x% + 5 * 4 * (i - 1), y%)-STEP(5 * 3, 5 * 5), 0, BF
  END IF

 NEXT
END SUB

SUB ProcessBoundaries
 IF (BallX + BallSize / 2 >= 0) AND (BallX - BallSize / 2 <= ScrWidth - 1) AND (BallY + BallSize / 2 >= 0) AND (BallY - BallSize / 2 <= ScrHeight - 1) THEN EXIT SUB' Do nothing if inside boundaries
 IF BallX + BallSize / 2 < -(ScrMax - BallSize * 2) AND SGN(VelX) = -1 THEN            ' Left Boundary
  SELECT CASE GameMode
   CASE 1, 4
    WhoScores = 0
    CALL ScorePoint
    CALL ScoreSound
    BallX = ScrWidth + (ScrMax - BallSize * 2)
   CASE 2, 3    'This shouldn't happen unless game mode changed
    CALL WallSound
    VelX = -VelX
  END SELECT
 END IF
 IF BallX - BallSize / 2 > ScrWidth - 1 + (ScrMax - BallSize * 2) AND SGN(VelX) = 1 THEN ' Right Boundary
  SELECT CASE GameMode
   CASE 1, 4
    WhoScores = 1
    CALL ScorePoint
    CALL ScoreSound
    BallX = -(ScrMax - BallSize * 2)
   CASE 2
    WhoScores = 0
    CALL ScorePoint
    CALL ScoreSound
    BallX = -(ScrMax - BallSize * 2)
   CASE 3
    CALL ScorePoint
    CALL ScoreSound
    BallX = -(ScrMax - BallSize * 2)
  END SELECT
 END IF
 IF BallY + BallSize / 2 < 0 AND SGN(VelY) = -1 THEN            ' Top Boundary
  SELECT CASE GameMode
   CASE 1, 2, 3, 4        'This shouldn't happen unless game mode changed in game
    'CALL WallSound
    VelY = -VelY
    'BallY = ScrHeight + (ScrMax - BallSize * 2)
  END SELECT
 END IF
 IF BallY - BallSize / 2 > ScrHeight - 1 AND SGN(VelY) = 1 THEN ' Bottom Boundary
  SELECT CASE GameMode
   CASE 1, 2, 3, 4        'This shouldn't happen unless game mode changed in game
    'CALL WallSound
    VelY = -VelY
    'BallY = -(ScrMax - BallSize * 2)
  END SELECT
 END IF
END SUB

'ProcessPaddles:
' You wouldn't believe how hard it is to implement Hockey over top of the
' other game modes, just because of the second paddle, and the "passing"
' behaviour of the Hockey paddles.
SUB ProcessPaddles
 JIAP = False
 IF Score(0) >= MaxScore OR Score(1) >= MaxScore THEN JustInAPaddle = JIAP: EXIT SUB' Paddles have no effect when game is over
 IF (BallX + BallSize / 2 < 0) OR (BallX - BallSize / 2 > ScrWidth - 1) OR (BallY + BallSize / 2 < 0) OR (BallY - BallSize / 2 > ScrHeight - 1) THEN EXIT SUB    ' Don't do anything if the ball is beyond the screen
 i = 0
 p = 1
 IF GameMode = 2 THEN p = 0
 FOR i = 0 TO p
  x1 = PaddleX(i) - LineSize / 2: x2 = PaddleX(i) + LineSize / 2
  xx1 = PaddleX2(i) - LineSize / 2: xx2 = PaddleX2(i) + LineSize / 2
  y1 = Paddle(i) - PaddleSize / 2: y2 = Paddle(i) + PaddleSize / 2
  IF Paddle(i) + PaddleSize / 2 >= 0 AND Paddle(i) - PaddleSize / 2 <= ScrHeight THEN         ' Don't process the paddle if it's off the screen
   PaddlePass123 = (BallX + BallSize / 2 >= x1) AND (BallX - BallSize / 2 <= x2) AND (BallY + BallSize / 2 >= y1) AND (BallY - BallSize / 2 <= y2)
   PaddlePass4 = ((BallX + BallSize / 2 >= xx1) AND (BallX - BallSize / 2 <= xx2) AND (BallY + BallSize / 2 >= y1) AND (BallY - BallSize / 2 <= y2))
   GM1Pass = PaddlePass123 AND (GameMode = 1 AND ((SGN(VelX) = 1 AND i = 0) OR (SGN(VelX) = -1 AND i = 1)))
   GM2Pass = PaddlePass123 AND ((GameMode = 2) AND SGN(VelX) = 1)
   GM3Pass = PaddlePass123 AND ((GameMode = 3) AND SGN(VelX) = 1 AND WhoScores = 1 - i)
   GM4Pass = (PaddlePass4 AND GameMode = 4) OR (PaddlePass123 AND (GameMode = 4 AND ((SGN(VelX) = 1 AND i = 0) OR (SGN(VelX) = -1 AND i = 1))))
   IF GM1Pass OR GM2Pass OR GM3Pass OR GM4Pass THEN
    SELECT CASE BounceMode
     CASE 0
      IF PaddlePass123 OR (GM4Pass AND PaddlePass4 AND ((i = 0 AND SGN(VelX) = -1 AND ABS(VelX) = NormalVelX) OR (i = 1 AND SGN(VelX) = 1 AND ABS(VelX) = NormalVelX))) THEN
       VelX = -VelX: JIAP = True
      END IF
     CASE 1
      TopOfPaddle = Paddle(i) - PaddleSize / 2
      BottomOfPaddle = Paddle(i) + PaddleSize / 2
      WhereOnPaddle = (BallY - TopOfPaddle)
      IF WhereOnPaddle <= BallSize + ProPaddleThresh THEN
       IF PaddlePass123 OR (GM4Pass AND PaddlePass4 AND ((i = 0 AND SGN(VelX) = -1 AND ABS(VelX) = NormalVelX) OR (i = 1 AND SGN(VelX) = 1 AND ABS(VelX) = NormalVelX))) THEN
        VelX = BigVelX * -SGN(VelX): JIAP = True
       END IF
       VelY = -BigVelY
      ELSEIF WhereOnPaddle >= PaddleSize - BallSize - ProPaddleThresh THEN
       IF PaddlePass123 OR (GM4Pass AND PaddlePass4 AND ((i = 0 AND SGN(VelX) = -1 AND ABS(VelX) = NormalVelX) OR (i = 1 AND SGN(VelX) = 1 AND ABS(VelX) = NormalVelX))) THEN
        VelX = BigVelX * -SGN(VelX): JIAP = True
       END IF
       VelY = BigVelY
      ELSE
       IF PaddlePass123 OR (GM4Pass AND PaddlePass4 AND ((i = 0 AND SGN(VelX) = -1 AND ABS(VelX) = NormalVelX) OR (i = 1 AND SGN(VelX) = 1 AND ABS(VelX) = NormalVelX))) THEN
        VelX = NormalVelX * -SGN(VelX): JIAP = True
       END IF
       VelY = NormalVelY * SGN(VelY)
      END IF
    END SELECT
    IF PaddlePass4 AND i = 0 THEN VelX = -ABS(VelX): JIAP = True
    IF PaddlePass4 AND i = 1 THEN VelX = ABS(VelX): JIAP = True
    WhoScores = 1 - WhoScores
    IF NOT JustInAPaddle THEN CALL PaddleSound
   END IF
  END IF
 NEXT i
 JustInAPaddle = JIAP
END SUB

SUB ProcessWalls
 DIM i, k, x1, y1, x2, y2, jb AS INTEGER
 DIM WithinBoundaries AS INTEGER
 WithinBoundaries = True
 IF (BallX + BallSize / 2 < 0) OR (BallX - BallSize / 2 > ScrWidth - 1) OR (BallY + BallSize / 2 < 0) OR (BallY - BallSize / 2 > ScrHeight - 1) THEN WithinBoundaries = False
 i = 0
 DO
  GetScreenLines i, k, x1, y1, x2, y2
  IF x1 > x2 THEN SWAP x1, x2
  IF y1 > y2 THEN SWAP y1, y2
  SELECT CASE k
   CASE LineSolidXTop, LineSolidXBottom, LineSolidYLeft
    IF (BallX + BallSize / 2 >= x1) AND (BallX - BallSize / 2 <= x2) AND (BallY + BallSize / 2 >= y1) AND (BallY - BallSize / 2 <= y2) THEN
     SELECT CASE k
      CASE LineSolidXTop
       IF SGN(VelY) = -1 THEN
        VelY = ABS(VelY)
        IF WithinBoundaries THEN CALL WallSound
       END IF
      CASE LineSolidXBottom
       IF SGN(VelY) = 1 THEN
        VelY = -ABS(VelY)
        IF WithinBoundaries THEN CALL WallSound
       END IF
      CASE LineSolidYLeft
       IF SGN(VelX) = -1 THEN
        VelX = ABS(VelX)
        IF WithinBoundaries THEN CALL WallSound
       END IF
     END SELECT
    END IF
  END SELECT
  i = i + 1
 LOOP UNTIL k = 0
END SUB

SUB ResetButton
 CLS
 Delay 1
 Score(0) = 0: Score(1) = 0
 WhoScores = 0
 CALL InitGame
 Delay 1
END SUB

SUB ScorePoint
 IF Score(0) < MaxScore AND Score(1) < MaxScore THEN
  Score(WhoScores) = Score(WhoScores) + 1
 END IF
 CALL DrawScores
END SUB

SUB ScoreSound
 IF SoundEnabled THEN SOUND 1000, .3
END SUB

SUB WallSound
 IF SoundEnabled THEN SOUND 500, .3
END SUB

