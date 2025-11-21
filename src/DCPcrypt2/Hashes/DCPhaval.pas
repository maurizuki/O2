{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of Haval ********************************}
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
unit DCPhaval;

interface
uses
  Classes, Sysutils, DCPcrypt2;

type
  TDCP_haval= class(TDCP_hash)
  protected
    LenHi, LenLo: longword;
    Index: DWord;
    CurrentHash: array[0..7] of DWord;
    HashBuffer: array[0..127] of byte;
    procedure Compress;
  public
    class function GetAlgorithm: string; override;
    class function GetHashSize: integer; override;
    class function SelfTest: boolean; override;
    procedure Init; override;
    procedure Burn; override;
    procedure Update(const Buffer; Size: longword); override;
    procedure Final(var Digest); override;
  end;



{******************************************************************************}
{******************************************************************************}
implementation
{$R-}{$Q-}

procedure TDCP_haval.Compress;
var
  t7, t6, t5, t4, t3, t2, t1, t0: DWord;
  W: array[0..31] of DWord;
  Temp: dword;
begin
  t0:= CurrentHash[0];
  t1:= CurrentHash[1];
  t2:= CurrentHash[2];
  t3:= CurrentHash[3];
  t4:= CurrentHash[4];
  t5:= CurrentHash[5];
  t6:= CurrentHash[6];
  t7:= CurrentHash[7];
  Move(HashBuffer,W,Sizeof(W));

  {$INCLUDE DCPhaval.inc}

  Inc(CurrentHash[0],t0);
  Inc(CurrentHash[1],t1);
  Inc(CurrentHash[2],t2);
  Inc(CurrentHash[3],t3);
  Inc(CurrentHash[4],t4);
  Inc(CurrentHash[5],t5);
  Inc(CurrentHash[6],t6);
  Inc(CurrentHash[7],t7);
  FillChar(W,Sizeof(W),0);
  Index:= 0;
  FillChar(HashBuffer,Sizeof(HashBuffer),0);
end;

class function TDCP_haval.GetHashSize: integer;
begin
  Result:= 256;
end;

class function TDCP_haval.GetAlgorithm: string;
begin
  Result:= 'Haval (256bit, 5 passes)';
end;

class function TDCP_haval.SelfTest: boolean;
const
  Test1Out: array[0..31] of byte=
    ($EA,$B9,$2E,$C2,$9E,$FC,$CB,$E0,$19,$D8,$31,$F2,$AB,$03,$0B,$10,
     $7E,$A9,$B8,$4B,$76,$86,$47,$33,$3B,$2D,$8C,$A5,$53,$1F,$01,$12);
  Test2Out: array[0..31] of byte=
    ($17,$22,$C5,$DC,$88,$D7,$77,$08,$D4,$78,$7D,$28,$3D,$7E,$26,$38,
     $57,$E9,$84,$AE,$5A,$6F,$9A,$18,$54,$96,$39,$06,$34,$3A,$BB,$F8);
var
  TestHash: TDCP_haval;
  TestOut: array[0..31] of byte;
begin
  TestHash:= TDCP_haval.Create;
  TestHash.Init;
  TestHash.UpdateStr('abc');
  TestHash.Final(TestOut);
  Result:= CompareMem(@TestOut,@Test1Out,Sizeof(Test1Out));
  TestHash.Init;
  TestHash.UpdateStr('abcdefghijklmnopqrstuvwxyz');
  TestHash.Final(TestOut);
  Result:= CompareMem(@TestOut,@Test2Out,Sizeof(Test2Out)) and Result;
  TestHash.Free;
end;

procedure TDCP_haval.Init;
begin
  Burn;
  CurrentHash[0]:= $243F6A88;
  CurrentHash[1]:= $85A308D3;
  CurrentHash[2]:= $13198A2E;
  CurrentHash[3]:= $03707344;
  CurrentHash[4]:= $A4093822;
  CurrentHash[5]:= $299F31D0;
  CurrentHash[6]:= $082EFA98;
  CurrentHash[7]:= $EC4E6C89;
  fInitialized:= true;
end;

procedure TDCP_haval.Burn;
begin
  LenHi:= 0; LenLo:= 0;
  Index:= 0;
  FillChar(HashBuffer,Sizeof(HashBuffer),0);
  FillChar(CurrentHash,Sizeof(CurrentHash),0);
  fInitialized:= false;
end;

procedure TDCP_haval.Update(const Buffer; Size: longword);
var
  PBuf: ^byte;
begin
  if not fInitialized then
    raise EDCP_hash.Create('Hash not initialized');

  Inc(LenHi,Size shr 29);
  Inc(LenLo,Size*8);
  if LenLo< (Size*8) then
    Inc(LenHi);

  PBuf:= @Buffer;
  while Size> 0 do
  begin
    if (Sizeof(HashBuffer)-Index)<= DWord(Size) then
    begin
      Move(PBuf^,HashBuffer[Index],Sizeof(HashBuffer)-Index);
      Dec(Size,Sizeof(HashBuffer)-Index);
      Inc(PBuf,Sizeof(HashBuffer)-Index);
      Compress;
    end
    else
    begin
      Move(PBuf^,HashBuffer[Index],Size);
      Inc(Index,Size);
      Size:= 0;
    end;
  end;
end;

procedure TDCP_haval.Final(var Digest);
begin
  if not fInitialized then
    raise EDCP_hash.Create('Hash not initialized');
  HashBuffer[Index]:= $80;
  if Index>= 118 then
    Compress;
  HashBuffer[118]:= ((256 and 3) shl 6) or (5 shl 3) or 1;
  HashBuffer[119]:= (256 shr 2) and $FF;
  PDWord(@HashBuffer[120])^:= LenLo;
  PDWord(@HashBuffer[124])^:= LenHi;
  Compress;
  Move(CurrentHash,Digest,256 div 8);
  Burn;
end;

end.
