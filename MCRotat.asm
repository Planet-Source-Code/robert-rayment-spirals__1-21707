;MCRotat.asm  by Robert Rayment

;VB
;res& = CallWindowProc(ptmc&, ptbuff&, rand&, ptzang, OpCode&)
;                             8        12     16      20
;Fill BITDATA structure
;Private Type BITDATA
; bmp.bmWidth = 256          'Pixel width
; bmp.bmHeight = 256         'Pixel height
; bmp.ptrFrontSurf = VarPtr(FrontSurf(1,1)) 'Pointer to Front surface
; bmp.ptrBackSurf = VarPtr(BackSurf(1,1))   ' Pointer to Back surface
;Dim bmp As BITDATA

%macro movab 2		;name & num of parameters
  push dword %2		;2nd param
  pop dword %1		;1st param
%endmacro			;use  movab %1,%2
;Allows eg	movab bmW,[ebx+4]

%define bmW   [ebp-4]	;Surfaces width eg 256
%define bmH   [ebp-8]	;Surfaces height eg 256
%define bmFS  [ebp-12]	;pointer to Front Surface
%define bmBS  [ebp-16]	;pointer to Back Surface

%define lo32 [ebp-20]
%define hi32 [ebp-24]

%define irand [ebp-28]	;random number 0-255

%define ix    [ebp-32]
%define iy    [ebp-36]
%define zA    [ebp-40]
%define zang  [ebp-44]
%define ixc   [ebp-48]
%define iyc   [ebp-52]
%define ixs   [ebp-56]
%define iys   [ebp-60]
%define zCos  [ebp-64]
%define zSin  [ebp-68]
%define zangStep  [ebp-72]
%define zangLim   [ebp-76]
%define intzang   [ebp-80]	;Int(zang)

[bits 32]

	push ebp
	mov ebp,esp
	sub esp,80
	push edi
	push esi
	push ebx

	;Fill bmp structure
	mov ebx,[ebp+8]
	movab bmW,[ebx]
	movab bmH,[ebx+4]
	movab bmFS,[ebx+8]
	movab bmBS,[ebx+12]
	
	mov eax,bmW			
	shr eax,1
	mov ixc,eax
	
	mov eax,bmH			
	shr eax,1
	mov iyc,eax
	
	;Random number seed
	mov eax,[ebp+12]	;rand
	mov irand,eax

	;Angle
	mov ebx,[ebp+16]	;ptzang
	movab zang,[ebx]
	
;---------------------------------------------
	mov eax,[ebp+20]	;Get OpCode	1,2,4,8,16,,
	cmp eax,4
	ja OpCode8
	
	;OpCodes = 1,2 or 4
	CALL RotateBytes
	jmp GETOUT

OpCode8:
	
	rcr eax,4
	jnc OpCode16

	CALL DecrementColor
	jmp GETOUT
	
OpCode16:
	rcr eax,1
	jnc OpCode32

	CALL BurningSpiral
	jmp GETOUT
	
OpCode32:

GETOUT:

	mov eax,irand	;Send random number seed back
	pop ebx
	pop esi
	pop edi
	mov esp,ebp
	pop ebp
	ret 16

;--- Retrace  Slows things down
;--- but avoids inteference sometimes
;--- mov dx,03DAh in NT doubtful.

;mov   dx,03DAh    ;Status register 
;WaitForOFF:      
;in    al,dx     
;test  al,8	  ;Is scan OFF
;jnz   WaitForOFF  ;No, keep waiting

;WaitForON:
;in    al,dx     
;test  al,8	  ;Is scan ON
;jz    WaitForON  ;No, keep waiting

;WaitForOFF2:      
;in    al,dx     
;test  al,8	  ;Is scan OFF
;jnz   WaitForOFF2  ;No, keep waiting

;############################################################

RotateBytes:

;zCos = Cos(zang)
;zSin = Sin(zang)
;ixc = bmW/2: iyc = bmH/2
;For iy = bmH To 1 Step -1
;For ix = bmW To 1 Step -1
;   FrontSurf(ix, iy) = 0
;   'For each FrontSurf point find rotated BackSurf source point
;   ixs = ixc + (ix - ixc) * zCos -/+ (iy - iyc) * zSin
;   If ixs < 1 Then Goto nexix
;   If ixs > bmW Then Goto nexix
;   iys = iyc + (iy - iyc) * zCos +/- (ix - ixc) * zSin
;   If iys < 1 Then Goto nexix
;   If iys > bmH Then Goto nexix
;   'Move valid rotated BackSurf source points to FrontSurf
;   FrontSurf(ix, iy) = BackSurf(ixs, iys)
;nexix:
;Next ix
;Next iy

	fld DWORD zang
	fsincos			;sin,cos
	fstp DWORD zSin
	fstp DWORD zCos
	
	mov ebx,bmH		;iy
Nextiy:
	mov iy,ebx
	mov ecx,bmW		;ix
Nextix:
	mov ix,ecx
	
	mov edi,bmFS	;->FrontSurf
	mov eax,iy
	dec eax
	mov edx,bmH
	mul edx
	mov edx,ix
	dec edx
	add eax,edx
	add edi,eax		;FrontSurf(ix,iy)
	mov [edi],BYTE 0

;   ixs = ixc + (ix - ixc) * zCos -/+ (iy - iyc) * zSin
	fild DWORD ixc	;ixc
	fild DWORD ix	;ix,ixc
	fsubp ST1		;ix-ixc
	fld DWORD zCos
	fmulp ST1		;(ix-ixc)zCos
	
	fild DWORD iyc	;iyc
	fild DWORD iy	;iy,iyc
	fsubp ST1		;iy-iyc
	fld DWORD zSin
	fmulp ST1		;(iy-iyc)zSin
	
	mov eax,[ebp+20]	;Get OpCode	1,2,4,8,16,,
	rcr eax,1
	jnc AOp2
	fsubp ST1			;OpCode=1 Rotate anti-clockwise
	jmp ADO
AOp2:
	rcr eax,1
	jnc AOp4	
	faddp ST1			;OpCode=2 Rotate clockwise
	jmp ADO
AOp4:
	faddp ST1			;OpCode=4 Stretcher
ADO:
	fild DWORD ixc
	faddp ST1
	fistp DWORD ixs
	
	cmp ixs,DWORD 1
	jb Decix
	mov eax,bmW
	cmp ixs,eax
	ja Decix

;   iys = iyc + (iy - iyc) * zCos +/- (ix - ixc) * zSin
	fild DWORD iyc	;iyc
	fild DWORD iy	;iy,iyc
	fsubp ST1		;iy-iyc
	fld DWORD zCos
	fmulp ST1		;(iy-iyc)zCos

	fild DWORD ixc	;ixc
	fild DWORD ix	;ix,ixc
	fsubp ST1		;ix-ixc
	fld DWORD zSin
	fmulp ST1		;(ix-ixc)zSin

	mov eax,[ebp+20]	;Get OpCode	1,2,4,8,16,,
	rcr eax,1
	jnc BOp2
	faddp ST1			;OpCode=1 Rotate anti-clockwise
	jmp BDO
BOp2:
	rcr eax,1
	jnc BOp4	
	fsubp ST1			;OpCode=2 Rotate clockwise
	jmp BDO
BOp4:
	faddp ST1			;OpCode=4 Stretcher
BDO:
	fild DWORD iyc
	faddp ST1
	fistp DWORD iys

	cmp iys,DWORD 1
	jb Decix
	mov eax,bmH
	cmp iys,eax
	ja Decix

;   FrontSurf(ix, iy) = BackSurf(ixs, iys)
	mov esi,bmBS	;->BackSurf
	mov eax,iys
	dec eax
	mov edx,bmH
	mul edx
	mov edx,ixs
	dec edx
	add eax,edx
	add esi,eax		;BackSurf(ixs,iys)
	movsb			;[edi] <- [esi]
	
Decix:
	dec ecx
	jnz NEAR Nextix
	dec ebx
	jnz NEAR Nextiy
ret

;############################################################

;	CPU DECREMENT COLOR NUMBER EVERWHERE ON FRONT SURFACE
;	OpCode = 8

DecrementColor:
	mov edi,bmFS	;->Front surface
	mov eax,bmH
	mov ebx,bmW
	mul ebx			;Number of bytes in FrontSurf array
	mov ecx,eax
	mov bl,255		;For changing color number 1
nextedi:
	mov dl,[edi]	;Pick up color number
	cmp dl,0		;Leave color number = 0 alone
	je skip0
	
	cmp dl,4
	ja decit
	
	mov [edi],bl	;Make color number2 1 to 4 = 255
	jmp skip0
decit:
	dec dl			;Decrement color number by 2
	dec dl			;Can decrement by 1 but slower
	mov [edi],dl
skip0:	
	inc edi
	dec ecx
	jnz nextedi
ret

;############################################################

;	BURNING SPIRAL
ALIGN

BurningSpiral:

;SEEDING ALONG SPIRAL
;ixc = 128: iyc = 128
;zA = 2   'More open then other spiral
;cul = 1
;For zang = 0 To 16 * pi# Step 0.01
;   ix = ixc + zA * zang * Cos(zang)
;	If ix < 1 or ix > 256 goto nexzang
;   iy = iyc + zA * zang * Sin(zang)
;	If iy < 1 or iy > 256 goto nexzang
;   FrontSurf(ix, iy) = cul
;   If cul = 255 Then cul = 0
;   cul = cul + 1
;nexzang
;Next zang

;Available
;%define ix    [ebp-20]
;%define iy    [ebp-24]
;%define zA    [ebp-28]	;/ Int(2)
;%define zang  [ebp-32]	;/ 0 start
;%define ixc   [ebp-36]	;/
;%define iyc   [ebp-40]	;/
;%define ixs   [ebp-44]
;%define iys   [ebp-48]
;%define zCos  [ebp-52]
;%define zSin  [ebp-56]
;%define zangStep  [ebp-60]	;.01
;%define zangLim   [ebp-64]	;16*pi# =~ Int(50)
;%define intzang   [ebp-68]	;Int(zang)

	mov eax,2
	mov zA,eax

	mov eax,100
	mov zangStep,eax
	fld1					;1
	fild DWORD zangStep		;100,1
	fdivp ST1				;.01
	fstp DWORD zangStep

	mov eax,50
	mov zangLim,eax
	
;For zang = 0 To zangLim Step zangStep
;   ix = ixc + zA * zang * Cos(zang)
;	If ix < 1 or ix > 256 goto nexzang
;   iy = iyc + zA * zang * Sin(zang)
;	If iy < 1 or iy > 256 goto nexzang
;   FrontSurf(ix, iy) = cul
;   If cul = 255 Then cul = 0
;   cul = cul + 1
;nexzang
;Next zang


	mov eax,DWORD 1
	mov lo32,eax
	
	CALL DrawSpiral
	;mov eax,iyc
	;inc eax
	;mov iyc,eax
	;CALL DrawSpiral
	;mov eax,iyc
	;inc eax
	;mov iyc,eax
	;CALL DrawSpiral
	
	mov eax,DWORD 2
	mov lo32,eax

	CALL DrawSpiral
	
	
mov   dx,03DAh    ;Status register 
WaitForOFF:      
in    al,dx     
test  al,8	  ;Is scan OFF
jnz   WaitForOFF  ;No, keep waiting

WaitForON:
in    al,dx     
test  al,8	  ;Is scan ON
jz    WaitForON	  ;No, keep waiting


ret

;############################################################
;	RANDOM NUMBERS IN EAX SEED IN irand

Rando:
	mov eax,01180Bh
	imul DWORD irand
	add eax, 0AB209h
	rcr eax,1
	jc ok
	bswap eax
	rol eax,1
ok:
	mov irand,eax
ret

;############################################################
DrawSpiral:
	fldz
	fstp DWORD zang

NextZang:	
	fld DWORD zang
	fcos
	fild DWORD zA
	fmulp ST1
	fld DWORD zang
	fmulp ST1
	
	fild DWORD ixc
	faddp ST1
	fistp DWORD ix	;ix = ixc + zA * zang * Cos(zang)
	
	mov eax,ix
	cmp eax,DWORD 1	;If ix < 1 or ix > bmW goto nexzang
	jl NEAR nexzang
	cmp eax,bmW
	jg NEAR nexzang
	
	fld DWORD zang
	fsin
	fild DWORD zA
	fmulp ST1
	fld DWORD zang
	fmulp ST1

	fild DWORD iyc
	faddp ST1
	fistp DWORD iy	;iy = iyc + zA * zang * Sin(zang)

	mov eax,iy
	cmp eax,DWORD 1	;If iy < 1 or iy > bmH goto nexzang
	jl NEAR nexzang
	cmp eax,bmH
	jg NEAR nexzang
	

	mov edi,bmFS	;->FrontSurf
	mov eax,iy
	dec eax
	mov ebx,bmH
	mul ebx
	mov ebx,ix
	dec ebx
	add eax,ebx
	add edi,eax		;FrontSurf(ix,iy)

	mov eax,lo32
	cmp eax,1
	jne Burn

	CALL Rando		;ran num in eax
	mov [edi],al
	jmp nexzang
	
Burn:
	mov ecx,DWORD 11
Burn2:	
	mov eax,bmW
	add edi,eax
	
  	xor dx,dx             ;Sum up 4 colors
 	
  	mov dl,[edi-256]      ;Line below
  	mov ax,dx             ;ax = dx
  	mov dl,[edi-257]      ;Line below right
  	add ax,dx             ;sum
  	mov dl,[edi-255]      ;Line below left
  	add ax,dx             ;sum
  	mov dl,[edi-512]      ;2 Lines below
  	add ax,dx             ;sum
  	shr ax,2              ;/4 average
  	jnc StoreCul
  	shr ax,1              ;/2 average
  
StoreCul:

  	mov [edi],al          ;put the new pixel value back in the array.
	dec ecx
	jnz Burn2

nexzang:

	fld DWORD zang
	fld DWORD zangStep
	faddp ST1
	fst DWORD zang

	fistp DWORD intzang
	mov eax,intzang
	cmp eax,zangLim

	jl NEAR NextZang
	
	finit

RET
