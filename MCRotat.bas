Attribute VB_Name = "Module1"
'MCRotat.bas by Robert Rayment

Option Base 1

DefInt A-T
DefSng U-Z


'Copy one array to another of same number of bytes
Public Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" _
(Destination As Any, Source As Any, ByVal Length As Long)


'Structures for StretchDIBits
Public Type BITMAPINFOHEADER '40 bytes
   biSize As Long
   biwidth As Long
   biheight As Long
   biPlanes As Integer
   biBitCount As Integer
   biCompression As Long
   biSizeImage As Long
   biXPelsPerMeter As Long
   biYPelsPerMeter As Long
   biClrUsed As Long
   biClrImportant As Long
End Type

Public Type RGBQUAD
        rgbBlue As Byte
        rgbGreen As Byte
        rgbRed As Byte
        rgbReserved As Byte
End Type

Public Type BITMAPINFO
   bmiH As BITMAPINFOHEADER
   Colors(0 To 255) As RGBQUAD
End Type

Public bm As BITMAPINFO


'For transferring drawing in byte array to Form
Public Declare Function StretchDIBits Lib "gdi32" (ByVal hdc As Long, _
ByVal X As Long, ByVal Y As Long, _
ByVal DesW As Long, ByVal DesH As Long, _
ByVal SrcX As Long, ByVal SrcY As Long, _
ByVal SrcW As Long, ByVal SrcH As Long, _
lpBits As Any, lpBitsInfo As BITMAPINFO, _
ByVal wUsage As Long, ByVal dwRop As Long) As Long

'Set alternative actions for StretchDIBits (not used here)
Public Declare Function SetStretchBltMode Lib "gdi32" _
(ByVal hdc As Long, ByVal nStretchMode As Long) As Long
'nStretchMode
Public Const STRETCH_ANDSCANS = 1    'default
Public Const STRETCH_ORSCANS = 2
Public Const STRETCH_DELETESCANS = 3
Public Const STRETCH_HALFTONE = 4

'For calling machine code
Public Declare Function CallWindowProc Lib "user32" Alias "CallWindowProcA" _
(ByVal lpMCode As Long, _
ByVal Long1 As Long, ByVal Long2 As Long, _
ByVal Long3 As Long, ByVal OpCode As Long) As Long


'Structure for input to mcode
Public Type BITDATA
   bmWidth As Long         ' Pixel width
   bmHeight As Long        ' Pixel height
   ptrFrontSurf As Long     ' Pointer to front surface
   ptrBackSurf As Long     ' Pointer to back surface
End Type

Public bmp As BITDATA

'Constants for StretchDIBits
Public Const DIB_PAL_COLORS = 1 '  color table in palette indices
Public Const DIB_RGB_COLORS = 0 '  color table in RGBs
Public Const SRCCOPY = &HCC0020
Public Const SRCINVERT = &H660046



'Byte front surface
Global FrontSurf() As Byte
'Byte back surface
Global BackSurf() As Byte
Global PathSpec$        'App path
Global PalSpec$         'PAL file

Global InCode() As Byte 'To hold mcode
Global zangs()          'Rotation angle (rad) & testing
Global SpiralShape      'LogSpiral, Spring1, Spring2, Ellipse
Global OpCode&, PrevOpCode&  'Rotation kind or color cycling
Global Const pi# = 3.1415927


Public Sub Loadmcode(InFile$)
'Load machine code into InCode() byte array
On Error GoTo InFileErr
If Dir$(InFile$) = "" Then
   MsgBox (InFile$ & " missing")
   Erase FrontSurf, BackSurf
   DoEvents
   Unload Form1
   End
End If
Open InFile$ For Binary As #1
MCSize& = LOF(1)
If MCSize& = 0 Then
InFileErr:
   MsgBox (InFile$ & " missing")
   Erase FrontSurf, BackSurf
   DoEvents
   Unload Form1
   End
End If
ReDim InCode(MCSize&)
Get #1, , InCode
Close #1
On Error GoTo 0
End Sub

Public Sub ReadPAL(PalSpec$)
'Read JASC-PAL palette file
'Any error shown by PalSpec$ = ""
'Else RGB into Colors(i) Long

Dim RED As Byte, GREEN As Byte, BLUE As Byte
On Error GoTo palerror
Open PalSpec$ For Input As #1
Line Input #1, a$
p = InStr(1, a$, "JASC")
If p = 0 Then PalSpec$ = "": Exit Sub
   
   'JASC-PAL
   '0100
   '256
   Line Input #1, a$
   Line Input #1, a$

   For n = 0 To 255
      If EOF(1) Then Exit For
      Line Input #1, a$
      ParsePAL a$, RED, GREEN, BLUE
      bm.Colors(n).rgbBlue = BLUE
      bm.Colors(n).rgbGreen = GREEN
      bm.Colors(n).rgbRed = RED
      bm.Colors(n).rgbReserved = 0
   Next n
   Close #1

Exit Sub
'===========
palerror:
PalSpec$ = ""
Exit Sub
End Sub

Public Sub ParsePAL(ain$, RED As Byte, GREEN As Byte, BLUE As Byte)
'Input string ain$, with 3 numbers(R G B) with
'space separators and then any text
ain$ = LTrim(ain$)
lena = Len(ain$)
r$ = ""
g$ = ""
B$ = ""
num = 0 'R
nt = 0
For i = 1 To lena
   c$ = Mid$(ain$, i, 1)
   
   If c$ <> " " Then
      If nt = 0 Then num = num + 1
      nt = 1
      If num = 4 Then Exit For
      If Asc(c$) < 48 Or Asc(c$) > 57 Then Exit For
      If num = 1 Then r$ = r$ + c$
      If num = 2 Then g$ = g$ + c$
      If num = 3 Then B$ = B$ + c$
   Else
      nt = 0
   End If
Next i
RED = Val(r$): GREEN = Val(g$): BLUE = Val(B$)
End Sub
