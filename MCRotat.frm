VERSION 5.00
Begin VB.Form Form1 
   Appearance      =   0  'Flat
   BackColor       =   &H80000006&
   Caption         =   "Machine Code Spirals"
   ClientHeight    =   5595
   ClientLeft      =   165
   ClientTop       =   735
   ClientWidth     =   6885
   FillColor       =   &H00808080&
   FillStyle       =   0  'Solid
   Icon            =   "MCRotat.frx":0000
   LinkTopic       =   "Form1"
   PaletteMode     =   2  'Custom
   ScaleHeight     =   373
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   459
   StartUpPosition =   3  'Windows Default
   Begin VB.Menu Shapes 
      Caption         =   "Shapes"
      Begin VB.Menu Spiral 
         Caption         =   "Spiral"
         Checked         =   -1  'True
      End
      Begin VB.Menu Spring1 
         Caption         =   "Spring1"
      End
      Begin VB.Menu Spring2 
         Caption         =   "Spring2"
      End
      Begin VB.Menu BurningSpiral 
         Caption         =   "Burning spiral"
      End
   End
   Begin VB.Menu Rotation 
      Caption         =   "Rotation"
      Begin VB.Menu RotateAntiClockwise 
         Caption         =   "Anti-ClockWise"
         Checked         =   -1  'True
      End
      Begin VB.Menu RotateClockwise 
         Caption         =   "Clockwise"
      End
      Begin VB.Menu Stretcher 
         Caption         =   "Stretcher"
      End
      Begin VB.Menu CycleColors 
         Caption         =   "Cycle colors"
      End
   End
   Begin VB.Menu Palette 
      Caption         =   "Palette"
      Begin VB.Menu Step7PAL 
         Caption         =   "Step7"
      End
      Begin VB.Menu BluesPAL 
         Caption         =   "Blues"
      End
      Begin VB.Menu HalfSinePAL 
         Caption         =   "HalfSine"
      End
      Begin VB.Menu FirePAL 
         Caption         =   "Fire"
      End
   End
   Begin VB.Menu StopLoop 
      Caption         =   "Stop"
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'MCRotat.frm by Robert Rayment

'Spiral maths from Chris'

'See 'Tunnels & Things for description of techniques'

'USING MACHINE CODE

'Using NASM. Netwide Assembler freeware from
'www.phoenix.gb/net
'www.cryogen.com/Nasm
'& other sites
'see also
'www.geocities.com/SunsetStrip/Stage/8513/assembly.html


Option Base 1

DefInt A-T
DefSng U-Z

'Constants for StretchDIBits
'Public Const DIB_RGB_COLORS = 0
'Public Const SRCCOPY = &HCC0020

''Byte front surface
'Global FrontSurf() As Byte
''Byte back surface
'Global BackSurf() As Byte
'Global PathSpec$        'App path
'Global PalSpec$         'PAL file

Dim Done As Boolean  'To exit loop
Dim Counter#

Private Sub RunCode()
'Global SpiralShape, OpCode&, PalSpec$

Cls

ReadPAL PalSpec$

'Zero color number to surfaces
For iy = 1 To 256
For ix = 1 To 256
   BackSurf(ix, iy) = 0
   FrontSurf(ix, iy) = 0
Next ix
Next iy

Randomize
Counter# = 1
Select Case SpiralShape
Case 1: DrawSpiral
Case 2: DrawSpring1
Case 3: DrawSpring2
Case 4: 'DrawBurningSpiral 'MCode will draw it
Case Else: Exit Sub
End Select
'FrontSurf to BackSurf
CopyMemory BackSurf(1, 1), FrontSurf(1, 1), 256& * 256&

Done = False
zA = 36  '180 1 deg rot, 18 10 deg rot, 36 5 deg rot
zangs(1) = pi# / zA
ptzang& = VarPtr(zangs(1))
'Set CallWindowProc parameters
ptmc& = VarPtr(InCode(1))
ptbuff& = VarPtr(bmp.bmWidth)

'OpCode& = 1    'Rotation anti-clockwise
'OpCode& = 2    'Rotation clockwise
'OpCode& = 4    'Stretcher
'OpCode& = 8    'Color cyling
'OpCode& =16    'Burning Spriral

Randomize
rand& = 10 * Rnd
op& = PrevOpCode&
   
Do
   res& = CallWindowProc(ptmc&, ptbuff&, rand&, ptzang&, OpCode&)
   rand& = res&   'Random number seed
   
   'zv = zangs(2) 'testing
   
   'Show FrontSurf
   'Get Form dimensions in pixels
   FormWidth& = Me.Width \ Screen.TwipsPerPixelX
   FormHeight& = Me.Height \ Screen.TwipsPerPixelY
   
   'Stretch byte-array to Form
   'NB The ByVal is critical in this! Otherwise big memory leak!
   succ& = StretchDIBits(Me.hdc, _
   0, 0, _
   FormWidth& - 8, FormHeight& - 40, _
   0, 0, _
   256&, 256&, _
   ByVal bmp.ptrFrontSurf, bm, _
   DIB_RGB_COLORS, SRCCOPY)
   
   DoEvents
   
   If OpCode& < 8 Then
      zangs(1) = zangs(1) + pi# / zA
      If zangs(1) > 2 * pi# Then zangs(1) = pi# / zA
   End If

Loop Until Done

End Sub



Private Sub Form_Load()

Counter# = 0
KeyPreview = True

'Set form up
ScaleMode = vbPixels
WindowState = vbNormal
AutoRedraw = False
Top = 1000
Left = 1000

'Width = 640 * Screen.TwipsPerPixelX    'ie * 15
'Height = 480 * Screen.TwipsPerPixelY   'ie * 15

Width = 400 * Screen.TwipsPerPixelX    'ie * 15
Height = 400 * Screen.TwipsPerPixelY   'ie * 15


'Get app path
PathSpec$ = App.Path
If Right$(PathSpec$, 1) <> "\" Then PathSpec$ = PathSpec$ & "\"

'Starting palette
'PalSpec$ = PathSpec$ & "Step7.PAL"
'PalSpec$ = PathSpec$ & "Blues.PAL"
'PalSpec$ = PathSpec$ & "HalfSine.PAL"
PalSpec$ = PathSpec$ & "Fire.PAL"   'Start palette

ReadPAL PalSpec$
FirePAL.Checked = True

ReDim BackSurf(256, 256)  'Edged Drawing byte surface
ReDim FrontSurf(256, 256)  'Rendering byte surface


'Fill BITDATA structure for machine code
bmp.bmWidth = 256          'Pixel width
bmp.bmHeight = 256         'Pixel height
bmp.ptrFrontSurf = VarPtr(FrontSurf(1, 1))  'Pointer to front surface
bmp.ptrBackSurf = VarPtr(BackSurf(1, 1))  'Pointer to back surface


'Fill BITMAPINFO.BITMAPINFOHEADER FOR StretchDIBits
bm.bmiH.biSize = 40
bm.bmiH.biwidth = bmp.bmWidth
bm.bmiH.biheight = bmp.bmHeight
bm.bmiH.biPlanes = 1
bm.bmiH.biBitCount = 8
bm.bmiH.biCompression = 0
bm.bmiH.biSizeImage = bmp.bmWidth * bmp.bmHeight '0 not used here
bm.bmiH.biXPelsPerMeter = 0
bm.bmiH.biYPelsPerMeter = 0
bm.bmiH.biClrUsed = 0
bm.bmiH.biClrImportant = 0

'Load mcode
InFile$ = PathSpec$ & "MCRotat.bin"
Loadmcode (InFile$)

'Starting shape
SpiralShape = 1
ClrShapeChecks
Spiral.Checked = True
'Starting rotation
OpCode& = 1
PrevOpCode& = 1
ClrRotationChecks
RotateAntiClockwise.Checked = True

ReDim zangs(2)

End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)

'Caption = Str$(X) + Str$(Y) & "      Maximize -->"

End Sub

Private Sub Form_Unload(Cancel As Integer)
'Over-done exit !!?
Done = True    'end loop
Erase BackSurf, FrontSurf
DoEvents
Unload Me
End
End Sub


'####  MENUS ##########

'####  SHAPES #########
Private Sub Spiral_Click()
Done = True
SpiralShape = 1
ClrShapeChecks
Spiral.Checked = True
If OpCode& = 16 Then
   OpCode& = PrevOpCode& 'Stop fire
   EnableRots
End If
RunCode
End Sub

Private Sub Spring1_Click()
Done = True
SpiralShape = 2
ClrShapeChecks
Spring1.Checked = True
If OpCode& = 16 Then
   OpCode& = PrevOpCode& 'Stop fire
   EnableRots
End If
RunCode
End Sub

Private Sub Spring2_Click()
Done = True
SpiralShape = 3
ClrShapeChecks
Spring2.Checked = True
If OpCode& = 16 Then
   OpCode& = PrevOpCode& 'Stop fire
   EnableRots
End If
RunCode
End Sub
Private Sub BurningSpiral_Click()
Done = True
SpiralShape = 4
ClrShapeChecks
BurningSpiral.Checked = True
If OpCode& <> 16 Then PrevOpCode& = OpCode&
OpCode& = 16
DisableRots
RunCode
End Sub

Private Sub ClrShapeChecks()
Spiral.Checked = False
Spring1.Checked = False
Spring2.Checked = False
BurningSpiral.Checked = False
End Sub

'####  ROTATIONS #########
Private Sub RotateAntiClockwise_Click()
OpCode& = 1
ClrRotationChecks
RotateAntiClockwise.Checked = True
RunCode
End Sub
Private Sub RotateClockwise_Click()
OpCode& = 2
ClrRotationChecks
RotateClockwise.Checked = True
RunCode
End Sub
Private Sub Stretcher_Click()
OpCode& = 4
ClrRotationChecks
Stretcher.Checked = True
RunCode
End Sub
Private Sub CycleColors_Click()
OpCode& = 8
ClrRotationChecks
CycleColors.Checked = True
RunCode
End Sub
Private Sub ClrRotationChecks()
RotateAntiClockwise.Checked = False
RotateClockwise.Checked = False
Stretcher.Checked = False
CycleColors.Checked = False
End Sub
Private Sub DisableRots()
RotateAntiClockwise.Enabled = False
RotateClockwise.Enabled = False
Stretcher.Enabled = False
CycleColors.Enabled = False
End Sub
Private Sub EnableRots()
RotateAntiClockwise.Enabled = True
RotateClockwise.Enabled = True
Stretcher.Enabled = True
CycleColors.Enabled = True
End Sub

'#### PALETTES ########
Private Sub Step7PAL_Click()
PalSpec$ = PathSpec$ & "Step7.PAL"
ClrPALChecks
Step7PAL.Checked = True
RunCode
End Sub
Private Sub BluesPAL_Click()
PalSpec$ = PathSpec$ & "Blues.PAL"
ClrPALChecks
BluesPAL.Checked = True
RunCode
End Sub
Private Sub HalfSinePAL_Click()
PalSpec$ = PathSpec$ & "HalfSine.PAL"
ClrPALChecks
HalfSinePAL.Checked = True
RunCode
End Sub
Private Sub FirePAL_Click()
PalSpec$ = PathSpec$ & "Fire.PAL"
ClrPALChecks
FirePAL.Checked = True
RunCode
End Sub
Private Sub ClrPALChecks()
Step7PAL.Checked = False
BluesPAL.Checked = False
HalfSinePAL.Checked = False
FirePAL.Checked = False
End Sub

'#####  DRAW SHAPES ##############

Private Sub DrawSpiral()
'Log spiral R = A x zang
'x = xc + zA zang cos(zang)
'y = yc + zA zang sin(zang)

'Select zA & zang limit to stay in bounds

ixc = 128: iyc = 128
zA = 0.6
cul = 1
For zang = 0 To 70 * pi# Step 0.01
   ix = ixc + zA * zang * Cos(zang)
   iy = iyc + zA * zang * Sin(zang)
   If ix >= 1 And iy >= 1 And ix <= 256 And iy <= 256 Then
      FrontSurf(ix, iy) = cul
   End If
   If cul = 255 Then cul = 0
   cul = cul + 1
Next zang

End Sub

Private Sub DrawSpring1()
'Log spiral R = A x zang
'x = xc(+) + zA zang cos(zang)
'y = yc(+) + zA zang sin(zang)

'Select zA & zang limit to stay in bounds

zixc = 40: ziyc = 40
zA = 0.6
cul = 1
For zang = 0 To 30 * pi# Step 0.005
   ix = zixc + zA * zang * Cos(zang)
   iy = ziyc + zA * zang * Sin(zang)
   If ix >= 1 And iy >= 1 And ix <= 256 And iy <= 256 Then
      FrontSurf(ix, iy) = cul
   End If
   If cul = 255 Then cul = 0
   cul = cul + 1
   'Spring x-> & y->
   ziyc = ziyc + 0.005
   zixc = zixc + 0.005
Next zang

End Sub

Private Sub DrawSpring2()
'Log spiral R = A x zang
'x = xc(+) + zA zang cos(zang)
'y = yc + zA zang sin(zang)

'Select zA & zang limit to stay in bounds

zixc = 40: ziyc = 128
zA = 0.6
cul = 1
For zang = 0 To 30 * pi# Step 0.005
   ix = zixc + zA * zang * Cos(zang)
   iy = ziyc + zA * zang * Sin(zang)
   If ix >= 1 And iy >= 1 And ix <= 256 And iy <= 256 Then
      FrontSurf(ix, iy) = cul
   End If
   If cul = 255 Then cul = 0
   cul = cul + 1
   'Spring x->
   If zang < 25 Then
      zixc = zixc + 0.03
   Else
      zixc = zixc - 0.005
   End If
Next zang
End Sub

Private Sub DrawBurningSpiral()
'Log spiral R = A x zang
'x = xc + zA zang cos(zang)
'y = yc + zA zang sin(zang)

'Select zA & zang limit to stay in bounds
LCounter& = 0
ixc = 128: iyc = 128
zA = 2   'More open then other spiral
cul = 1
For zang = 0 To 16 * pi# Step 0.01
   ix = ixc + zA * zang * Cos(zang)
   iy = iyc + zA * zang * Sin(zang)
   If ix >= 1 And iy >= 1 And ix <= 256 And iy <= 256 Then
      FrontSurf(ix, iy) = cul
   End If
   If cul = 255 Then cul = 0
   cul = cul + 1
   LCounter& = LCounter& + 1
Next zang

zang = 10
ix = ixc + zA * zang * Cos(zang) '111
iy = iyc + zA * zang * Sin(zang) '117


End Sub


Private Sub StopLoop_Click()
Done = True
End Sub

