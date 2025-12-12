{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of Cast256 ******************************}
{******************************************************************************}
{* Copyright (c) 1999-2002 David Barton                                       *}
{* Permission is hereby granted, free of charge, to any person obtaining a    *}
{* copy of this software and associated documentation files (the "Software"), *}
{* to deal in the Software without restriction, including without limitation  *}
{* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *}
{* and/or sell copies of the Software, and to permit persons to whom the      *}
{* Software is furnished to do so, subject to the following conditions:       *}
{*                                                                            *}
{* The above copyright notice and this permission notice shall be included in *}
{* all copies or substantial portions of the Software.                        *}
{*                                                                            *}
{* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *}
{* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *}
{* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *}
{* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *}
{* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *}
{* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *}
{* DEALINGS IN THE SOFTWARE.                                                  *}
{******************************************************************************}
unit DCPcast256;

interface
uses
  Classes, Sysutils, DCPcrypt2, DCPblockciphers;

type
  TDCP_cast256= class(TDCP_blockcipher128)
  protected
    Kr, Km: array[0..11,0..3] of DWord;
    procedure InitKey(const Key; Size: longword); override;
  public
    class function GetAlgorithm: string; override;
    class function GetMaxKeySize: integer; override;
    procedure Burn; override;
    procedure EncryptECB(const InData; var OutData); override;
    procedure DecryptECB(const InData; var OutData); override;
  end;


{******************************************************************************}
{******************************************************************************}
implementation
{$R-}{$Q-}
{$I DCPcast256.inc}

function LRot32(a, n: dword): dword;
begin
  Result:= (a shl n) or (a shr (32-n));
end;

function SwapDword(a: dword): dword;
begin
  Result:= ((a and $FF) shl 24) or ((a and $FF00) shl 8) or ((a and $FF0000) shr 8) or ((a and $FF000000) shr 24);
end;

function F1(a,rk,mk: DWord): DWord;
var
  t: DWord;
begin
  t:= LRot32(mk + a,rk);
  Result:= ((S1[t shr 24] xor S2[(t shr 16) and $FF]) - S3[(t shr 8) and $FF]) + S4[t and $FF];
end;
function F2(a,rk,mk: DWord): DWord;
var
  t: DWord;
begin
  t:= LRot32(mk xor a,rk);
  Result:= ((S1[t shr 24] - S2[(t shr 16) and $FF]) + S3[(t shr 8) and $FF]) xor S4[t and $FF];
end;
function F3(a,rk,mk: DWord): DWord;
var
  t: DWord;
begin
  t:= LRot32(mk - a,rk);
  Result:= ((S1[t shr 24] + S2[(t shr 16) and $FF]) xor S3[(t shr 8) and $FF]) - S4[t and $FF];
end;

class function TDCP_cast256.GetMaxKeySize: integer;
begin
  Result:= 256;
end;

class function TDCP_cast256.GetAlgorithm: string;
begin
  Result:= 'Cast256';
end;

procedure TDCP_cast256.InitKey(const Key; Size: longword);
var
  x: array[0..7] of DWord;
  cm, cr: DWord;
  i, j: longword;
  tr, tm: array[0..7] of DWord;
begin
  Size:= Size div 8;

  FillChar(x,Sizeof(x),0);
  Move(Key,x,Size);

  cm:= $5a827999;
  cr:= 19;
  for i:= 0 to 7 do
    x[i]:= (x[i] shl 24) or ((x[i] shl 8) and $FF0000) or ((x[i] shr 8) and $FF00) or (x[i] shr 24);
  for i:= 0 to 11 do
  begin
    for j:= 0 to 7 do
    begin
      tm[j]:= cm;
      Inc(cm,$6ed9eba1);
      tr[j]:= cr;
      Inc(cr,17);
    end;
    x[6]:= x[6] xor f1(x[7],tr[0],tm[0]);
    x[5]:= x[5] xor f2(x[6],tr[1],tm[1]);
    x[4]:= x[4] xor f3(x[5],tr[2],tm[2]);
    x[3]:= x[3] xor f1(x[4],tr[3],tm[3]);
    x[2]:= x[2] xor f2(x[3],tr[4],tm[4]);
    x[1]:= x[1] xor f3(x[2],tr[5],tm[5]);
    x[0]:= x[0] xor f1(x[1],tr[6],tm[6]);
    x[7]:= x[7] xor f2(x[0],tr[7],tm[7]);

    for j:= 0 to 7 do
    begin
      tm[j]:= cm;
      Inc(cm,$6ed9eba1);
      tr[j]:= cr;
      Inc(cr,17);
    end;
    x[6]:= x[6] xor f1(x[7],tr[0],tm[0]);
    x[5]:= x[5] xor f2(x[6],tr[1],tm[1]);
    x[4]:= x[4] xor f3(x[5],tr[2],tm[2]);
    x[3]:= x[3] xor f1(x[4],tr[3],tm[3]);
    x[2]:= x[2] xor f2(x[3],tr[4],tm[4]);
    x[1]:= x[1] xor f3(x[2],tr[5],tm[5]);
    x[0]:= x[0] xor f1(x[1],tr[6],tm[6]);
    x[7]:= x[7] xor f2(x[0],tr[7],tm[7]);

    Kr[i,0]:= x[0] and 31;
    Kr[i,1]:= x[2] and 31;
    Kr[i,2]:= x[4] and 31;
    Kr[i,3]:= x[6] and 31;
    Km[i,0]:= x[7];
    Km[i,1]:= x[5];
    Km[i,2]:= x[3];
    Km[i,3]:= x[1];
  end;
  FillChar(x,Sizeof(x),$FF);
end;

procedure TDCP_cast256.Burn;
begin
  FillChar(Kr,Sizeof(Kr),$FF);
  FillChar(Km,Sizeof(Km),$FF);
  inherited Burn;
end;

procedure TDCP_cast256.EncryptECB(const InData; var OutData);
var
  A: array[0..3] of DWord;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  A[0]:= PDWord(@InData)^;
  A[1]:= PDWord(longword(@InData)+4)^;
  A[2]:= PDWord(longword(@InData)+8)^;
  A[3]:= PDWord(longword(@InData)+12)^;

  A[0]:= SwapDWord(A[0]);
  A[1]:= SwapDWord(A[1]);
  A[2]:= SwapDWord(A[2]);
  A[3]:= SwapDWord(A[3]);
  A[2]:= A[2] xor f1(A[3],kr[0,0],km[0,0]);
  A[1]:= A[1] xor f2(A[2],kr[0,1],km[0,1]);
  A[0]:= A[0] xor f3(A[1],kr[0,2],km[0,2]);
  A[3]:= A[3] xor f1(A[0],kr[0,3],km[0,3]);
  A[2]:= A[2] xor f1(A[3],kr[1,0],km[1,0]);
  A[1]:= A[1] xor f2(A[2],kr[1,1],km[1,1]);
  A[0]:= A[0] xor f3(A[1],kr[1,2],km[1,2]);
  A[3]:= A[3] xor f1(A[0],kr[1,3],km[1,3]);
  A[2]:= A[2] xor f1(A[3],kr[2,0],km[2,0]);
  A[1]:= A[1] xor f2(A[2],kr[2,1],km[2,1]);
  A[0]:= A[0] xor f3(A[1],kr[2,2],km[2,2]);
  A[3]:= A[3] xor f1(A[0],kr[2,3],km[2,3]);
  A[2]:= A[2] xor f1(A[3],kr[3,0],km[3,0]);
  A[1]:= A[1] xor f2(A[2],kr[3,1],km[3,1]);
  A[0]:= A[0] xor f3(A[1],kr[3,2],km[3,2]);
  A[3]:= A[3] xor f1(A[0],kr[3,3],km[3,3]);
  A[2]:= A[2] xor f1(A[3],kr[4,0],km[4,0]);
  A[1]:= A[1] xor f2(A[2],kr[4,1],km[4,1]);
  A[0]:= A[0] xor f3(A[1],kr[4,2],km[4,2]);
  A[3]:= A[3] xor f1(A[0],kr[4,3],km[4,3]);
  A[2]:= A[2] xor f1(A[3],kr[5,0],km[5,0]);
  A[1]:= A[1] xor f2(A[2],kr[5,1],km[5,1]);
  A[0]:= A[0] xor f3(A[1],kr[5,2],km[5,2]);
  A[3]:= A[3] xor f1(A[0],kr[5,3],km[5,3]);

  A[3]:= A[3] xor f1(A[0],kr[6,3],km[6,3]);
  A[0]:= A[0] xor f3(A[1],kr[6,2],km[6,2]);
  A[1]:= A[1] xor f2(A[2],kr[6,1],km[6,1]);
  A[2]:= A[2] xor f1(A[3],kr[6,0],km[6,0]);
  A[3]:= A[3] xor f1(A[0],kr[7,3],km[7,3]);
  A[0]:= A[0] xor f3(A[1],kr[7,2],km[7,2]);
  A[1]:= A[1] xor f2(A[2],kr[7,1],km[7,1]);
  A[2]:= A[2] xor f1(A[3],kr[7,0],km[7,0]);
  A[3]:= A[3] xor f1(A[0],kr[8,3],km[8,3]);
  A[0]:= A[0] xor f3(A[1],kr[8,2],km[8,2]);
  A[1]:= A[1] xor f2(A[2],kr[8,1],km[8,1]);
  A[2]:= A[2] xor f1(A[3],kr[8,0],km[8,0]);
  A[3]:= A[3] xor f1(A[0],kr[9,3],km[9,3]);
  A[0]:= A[0] xor f3(A[1],kr[9,2],km[9,2]);
  A[1]:= A[1] xor f2(A[2],kr[9,1],km[9,1]);
  A[2]:= A[2] xor f1(A[3],kr[9,0],km[9,0]);
  A[3]:= A[3] xor f1(A[0],kr[10,3],km[10,3]);
  A[0]:= A[0] xor f3(A[1],kr[10,2],km[10,2]);
  A[1]:= A[1] xor f2(A[2],kr[10,1],km[10,1]);
  A[2]:= A[2] xor f1(A[3],kr[10,0],km[10,0]);
  A[3]:= A[3] xor f1(A[0],kr[11,3],km[11,3]);
  A[0]:= A[0] xor f3(A[1],kr[11,2],km[11,2]);
  A[1]:= A[1] xor f2(A[2],kr[11,1],km[11,1]);
  A[2]:= A[2] xor f1(A[3],kr[11,0],km[11,0]);
  A[0]:= SwapDWord(A[0]);
  A[1]:= SwapDWord(A[1]);
  A[2]:= SwapDWord(A[2]);
  A[3]:= SwapDWord(A[3]);

  PDWord(@OutData)^:= A[0];
  PDWord(longword(@OutData)+4)^:= A[1];
  PDWord(longword(@OutData)+8)^:= A[2];
  PDWord(longword(@OutData)+12)^:= A[3];
end;

procedure TDCP_cast256.DecryptECB(const InData; var OutData);
var
  A: array[0..3] of DWord;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  A[0]:= PDWord(@InData)^;
  A[1]:= PDWord(longword(@InData)+4)^;
  A[2]:= PDWord(longword(@InData)+8)^;
  A[3]:= PDWord(longword(@InData)+12)^;

  A[0]:= SwapDWord(A[0]);
  A[1]:= SwapDWord(A[1]);
  A[2]:= SwapDWord(A[2]);
  A[3]:= SwapDWord(A[3]);
  A[2]:= A[2] xor f1(A[3],kr[11,0],km[11,0]);
  A[1]:= A[1] xor f2(A[2],kr[11,1],km[11,1]);
  A[0]:= A[0] xor f3(A[1],kr[11,2],km[11,2]);
  A[3]:= A[3] xor f1(A[0],kr[11,3],km[11,3]);
  A[2]:= A[2] xor f1(A[3],kr[10,0],km[10,0]);
  A[1]:= A[1] xor f2(A[2],kr[10,1],km[10,1]);
  A[0]:= A[0] xor f3(A[1],kr[10,2],km[10,2]);
  A[3]:= A[3] xor f1(A[0],kr[10,3],km[10,3]);
  A[2]:= A[2] xor f1(A[3],kr[9,0],km[9,0]);
  A[1]:= A[1] xor f2(A[2],kr[9,1],km[9,1]);
  A[0]:= A[0] xor f3(A[1],kr[9,2],km[9,2]);
  A[3]:= A[3] xor f1(A[0],kr[9,3],km[9,3]);
  A[2]:= A[2] xor f1(A[3],kr[8,0],km[8,0]);
  A[1]:= A[1] xor f2(A[2],kr[8,1],km[8,1]);
  A[0]:= A[0] xor f3(A[1],kr[8,2],km[8,2]);
  A[3]:= A[3] xor f1(A[0],kr[8,3],km[8,3]);
  A[2]:= A[2] xor f1(A[3],kr[7,0],km[7,0]);
  A[1]:= A[1] xor f2(A[2],kr[7,1],km[7,1]);
  A[0]:= A[0] xor f3(A[1],kr[7,2],km[7,2]);
  A[3]:= A[3] xor f1(A[0],kr[7,3],km[7,3]);
  A[2]:= A[2] xor f1(A[3],kr[6,0],km[6,0]);
  A[1]:= A[1] xor f2(A[2],kr[6,1],km[6,1]);
  A[0]:= A[0] xor f3(A[1],kr[6,2],km[6,2]);
  A[3]:= A[3] xor f1(A[0],kr[6,3],km[6,3]);

  A[3]:= A[3] xor f1(A[0],kr[5,3],km[5,3]);
  A[0]:= A[0] xor f3(A[1],kr[5,2],km[5,2]);
  A[1]:= A[1] xor f2(A[2],kr[5,1],km[5,1]);
  A[2]:= A[2] xor f1(A[3],kr[5,0],km[5,0]);
  A[3]:= A[3] xor f1(A[0],kr[4,3],km[4,3]);
  A[0]:= A[0] xor f3(A[1],kr[4,2],km[4,2]);
  A[1]:= A[1] xor f2(A[2],kr[4,1],km[4,1]);
  A[2]:= A[2] xor f1(A[3],kr[4,0],km[4,0]);
  A[3]:= A[3] xor f1(A[0],kr[3,3],km[3,3]);
  A[0]:= A[0] xor f3(A[1],kr[3,2],km[3,2]);
  A[1]:= A[1] xor f2(A[2],kr[3,1],km[3,1]);
  A[2]:= A[2] xor f1(A[3],kr[3,0],km[3,0]);
  A[3]:= A[3] xor f1(A[0],kr[2,3],km[2,3]);
  A[0]:= A[0] xor f3(A[1],kr[2,2],km[2,2]);
  A[1]:= A[1] xor f2(A[2],kr[2,1],km[2,1]);
  A[2]:= A[2] xor f1(A[3],kr[2,0],km[2,0]);
  A[3]:= A[3] xor f1(A[0],kr[1,3],km[1,3]);
  A[0]:= A[0] xor f3(A[1],kr[1,2],km[1,2]);
  A[1]:= A[1] xor f2(A[2],kr[1,1],km[1,1]);
  A[2]:= A[2] xor f1(A[3],kr[1,0],km[1,0]);
  A[3]:= A[3] xor f1(A[0],kr[0,3],km[0,3]);
  A[0]:= A[0] xor f3(A[1],kr[0,2],km[0,2]);
  A[1]:= A[1] xor f2(A[2],kr[0,1],km[0,1]);
  A[2]:= A[2] xor f1(A[3],kr[0,0],km[0,0]);
  A[0]:= SwapDWord(A[0]);
  A[1]:= SwapDWord(A[1]);
  A[2]:= SwapDWord(A[2]);
  A[3]:= SwapDWord(A[3]);

  PDWord(@OutData)^:= A[0];
  PDWord(longword(@OutData)+4)^:= A[1];
  PDWord(longword(@OutData)+8)^:= A[2];
  PDWord(longword(@OutData)+12)^:= A[3];
end;


end.
