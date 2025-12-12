{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of Rijndael *****************************}
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
unit DCPrijndael;

interface
uses
  Classes, Sysutils, DCPcrypt2, DCPblockciphers;

const
  BC= 4;
  MAXROUNDS= 14;

type
  TDCP_rijndael= class(TDCP_blockcipher128)
  protected
    numrounds: longword;
    rk, drk: array[0..MAXROUNDS,0..7] of DWord;
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
{$I DCPrijndael.inc}

class function TDCP_rijndael.GetMaxKeySize: integer;
begin
  Result:= 256;
end;

class function TDCP_rijndael.GetAlgorithm: string;
begin
  Result:= 'Rijndael';
end;

procedure InvMixColumn(a: PByteArray; BC: byte);
var
  j: longword;
begin
  for j:= 0 to (BC-1) do
    PDWord(@(a^[j*4]))^:= PDWord(@U1[a^[j*4+0]])^ xor
                       PDWord(@U2[a^[j*4+1]])^ xor
                       PDWord(@U3[a^[j*4+2]])^ xor
                       PDWord(@U4[a^[j*4+3]])^;
end;

procedure TDCP_rijndael.InitKey(const Key; Size: longword);
var
  KC, ROUNDS, j, r, t, rconpointer: longword;
  tk: array[0..MAXKC-1,0..3] of byte;
begin
  Size:= Size div 8;

  FillChar(tk,Sizeof(tk),0);
  Move(Key,tk,Size);
  if Size<= 16 then
  begin
    KC:= 4;
    Rounds:= 10;
  end
  else if Size<= 24 then
  begin
    KC:= 6;
    Rounds:= 12;
  end
  else
  begin
    KC:= 8;
    Rounds:= 14;
  end;
  numrounds:= rounds;
  r:= 0;
  t:= 0;
  j:= 0;
  while (j< KC) and (r< (rounds+1)) do
  begin
    while (j< KC) and (t< BC) do
    begin
      rk[r,t]:= PDWord(@tk[j])^;
      Inc(j);
      Inc(t);
    end;
    if t= BC then
    begin
      t:= 0;
      Inc(r);
    end;
  end;
  rconpointer:= 0;
  while (r< (rounds+1)) do
  begin
    tk[0,0]:= tk[0,0] xor S[tk[KC-1,1]];
    tk[0,1]:= tk[0,1] xor S[tk[KC-1,2]];
    tk[0,2]:= tk[0,2] xor S[tk[KC-1,3]];
    tk[0,3]:= tk[0,3] xor S[tk[KC-1,0]];
    tk[0,0]:= tk[0,0] xor rcon[rconpointer];
    Inc(rconpointer);
    if KC<> 8 then
    begin
      for j:= 1 to (KC-1) do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
    end
    else
    begin
      for j:= 1 to ((KC div 2)-1) do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
      tk[KC div 2,0]:= tk[KC div 2,0] xor S[tk[KC div 2 - 1,0]];
      tk[KC div 2,1]:= tk[KC div 2,1] xor S[tk[KC div 2 - 1,1]];
      tk[KC div 2,2]:= tk[KC div 2,2] xor S[tk[KC div 2 - 1,2]];
      tk[KC div 2,3]:= tk[KC div 2,3] xor S[tk[KC div 2 - 1,3]];
      for j:= ((KC div 2) + 1) to (KC-1) do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
    end;
    j:= 0;
    while (j< KC) and (r< (rounds+1)) do
    begin
      while (j< KC) and (t< BC) do
      begin
        rk[r,t]:= PDWord(@tk[j])^;
        Inc(j);
        Inc(t);
      end;
      if t= BC then
      begin
        Inc(r);
        t:= 0;
      end;
    end;
  end;
  Move(rk,drk,Sizeof(rk));
  for r:= 1 to (numrounds-1) do
    InvMixColumn(@drk[r],BC);
end;

procedure TDCP_rijndael.Burn;
begin
  numrounds:= 0;
  FillChar(rk,Sizeof(rk),0);
  FillChar(drk,Sizeof(drk),0);
  inherited Burn;
end;

procedure TDCP_rijndael.EncryptECB(const InData; var OutData);
var
  r: longword;
  tempb: array[0..MAXBC-1,0..3] of byte;
  a: array[0..MAXBC,0..3] of byte;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  PDword(@a[0,0])^:= PDword(@InData)^;
  PDword(@a[1,0])^:= PDword(dword(@InData)+4)^;
  PDword(@a[2,0])^:= PDword(dword(@InData)+8)^;
  PDword(@a[3,0])^:= PDword(dword(@InData)+12)^;
  for r:= 0 to (numrounds-2) do
  begin
    PDWord(@tempb[0])^:= PDWord(@a[0])^ xor rk[r,0];
    PDWord(@tempb[1])^:= PDWord(@a[1])^ xor rk[r,1];
    PDWord(@tempb[2])^:= PDWord(@a[2])^ xor rk[r,2];
    PDWord(@tempb[3])^:= PDWord(@a[3])^ xor rk[r,3];
    PDWord(@a[0])^:= PDWord(@T1[tempb[0,0]])^ xor
                     PDWord(@T2[tempb[1,1]])^ xor
                     PDWord(@T3[tempb[2,2]])^ xor
                     PDWord(@T4[tempb[3,3]])^;
    PDWord(@a[1])^:= PDWord(@T1[tempb[1,0]])^ xor
                     PDWord(@T2[tempb[2,1]])^ xor
                     PDWord(@T3[tempb[3,2]])^ xor
                     PDWord(@T4[tempb[0,3]])^;
    PDWord(@a[2])^:= PDWord(@T1[tempb[2,0]])^ xor
                     PDWord(@T2[tempb[3,1]])^ xor
                     PDWord(@T3[tempb[0,2]])^ xor
                     PDWord(@T4[tempb[1,3]])^;
    PDWord(@a[3])^:= PDWord(@T1[tempb[3,0]])^ xor
                     PDWord(@T2[tempb[0,1]])^ xor
                     PDWord(@T3[tempb[1,2]])^ xor
                     PDWord(@T4[tempb[2,3]])^;
  end;
  PDWord(@tempb[0])^:= PDWord(@a[0])^ xor rk[numrounds-1,0];
  PDWord(@tempb[1])^:= PDWord(@a[1])^ xor rk[numrounds-1,1];
  PDWord(@tempb[2])^:= PDWord(@a[2])^ xor rk[numrounds-1,2];
  PDWord(@tempb[3])^:= PDWord(@a[3])^ xor rk[numrounds-1,3];
  a[0,0]:= T1[tempb[0,0],1];
  a[0,1]:= T1[tempb[1,1],1];
  a[0,2]:= T1[tempb[2,2],1];
  a[0,3]:= T1[tempb[3,3],1];
  a[1,0]:= T1[tempb[1,0],1];
  a[1,1]:= T1[tempb[2,1],1];
  a[1,2]:= T1[tempb[3,2],1];
  a[1,3]:= T1[tempb[0,3],1];
  a[2,0]:= T1[tempb[2,0],1];
  a[2,1]:= T1[tempb[3,1],1];
  a[2,2]:= T1[tempb[0,2],1];
  a[2,3]:= T1[tempb[1,3],1];
  a[3,0]:= T1[tempb[3,0],1];
  a[3,1]:= T1[tempb[0,1],1];
  a[3,2]:= T1[tempb[1,2],1];
  a[3,3]:= T1[tempb[2,3],1];
  PDWord(@a[0])^:= PDWord(@a[0])^ xor rk[numrounds,0];
  PDWord(@a[1])^:= PDWord(@a[1])^ xor rk[numrounds,1];
  PDWord(@a[2])^:= PDWord(@a[2])^ xor rk[numrounds,2];
  PDWord(@a[3])^:= PDWord(@a[3])^ xor rk[numrounds,3];

  PDword(@OutData)^:= PDword(@a[0,0])^;
  PDword(dword(@OutData)+4)^:= PDword(@a[1,0])^;
  PDword(dword(@OutData)+8)^:= PDword(@a[2,0])^;
  PDword(dword(@OutData)+12)^:= PDword(@a[3,0])^;
end;

procedure TDCP_rijndael.DecryptECB(const InData; var OutData);
var
  r: longword;
  tempb: array[0..MAXBC-1,0..3] of byte;
  a: array[0..MAXBC,0..3] of byte;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  PDword(@a[0,0])^:= PDword(@InData)^;
  PDword(@a[1,0])^:= PDword(dword(@InData)+4)^;
  PDword(@a[2,0])^:= PDword(dword(@InData)+8)^;
  PDword(@a[3,0])^:= PDword(dword(@InData)+12)^;
  for r:= NumRounds downto 2 do
  begin
    PDWord(@tempb[0])^:= PDWord(@a[0])^ xor drk[r,0];
    PDWord(@tempb[1])^:= PDWord(@a[1])^ xor drk[r,1];
    PDWord(@tempb[2])^:= PDWord(@a[2])^ xor drk[r,2];
    PDWord(@tempb[3])^:= PDWord(@a[3])^ xor drk[r,3];
    PDWord(@a[0])^:= PDWord(@T5[tempb[0,0]])^ xor
                     PDWord(@T6[tempb[3,1]])^ xor
                     PDWord(@T7[tempb[2,2]])^ xor
                     PDWord(@T8[tempb[1,3]])^;
    PDWord(@a[1])^:= PDWord(@T5[tempb[1,0]])^ xor
                     PDWord(@T6[tempb[0,1]])^ xor
                     PDWord(@T7[tempb[3,2]])^ xor
                     PDWord(@T8[tempb[2,3]])^;
    PDWord(@a[2])^:= PDWord(@T5[tempb[2,0]])^ xor
                     PDWord(@T6[tempb[1,1]])^ xor
                     PDWord(@T7[tempb[0,2]])^ xor
                     PDWord(@T8[tempb[3,3]])^;
    PDWord(@a[3])^:= PDWord(@T5[tempb[3,0]])^ xor
                     PDWord(@T6[tempb[2,1]])^ xor
                     PDWord(@T7[tempb[1,2]])^ xor
                     PDWord(@T8[tempb[0,3]])^;
  end;
  PDWord(@tempb[0])^:= PDWord(@a[0])^ xor drk[1,0];
  PDWord(@tempb[1])^:= PDWord(@a[1])^ xor drk[1,1];
  PDWord(@tempb[2])^:= PDWord(@a[2])^ xor drk[1,2];
  PDWord(@tempb[3])^:= PDWord(@a[3])^ xor drk[1,3];
  a[0,0]:= S5[tempb[0,0]];
  a[0,1]:= S5[tempb[3,1]];
  a[0,2]:= S5[tempb[2,2]];
  a[0,3]:= S5[tempb[1,3]];
  a[1,0]:= S5[tempb[1,0]];
  a[1,1]:= S5[tempb[0,1]];
  a[1,2]:= S5[tempb[3,2]];
  a[1,3]:= S5[tempb[2,3]];
  a[2,0]:= S5[tempb[2,0]];
  a[2,1]:= S5[tempb[1,1]];
  a[2,2]:= S5[tempb[0,2]];
  a[2,3]:= S5[tempb[3,3]];
  a[3,0]:= S5[tempb[3,0]];
  a[3,1]:= S5[tempb[2,1]];
  a[3,2]:= S5[tempb[1,2]];
  a[3,3]:= S5[tempb[0,3]];
  PDWord(@a[0])^:= PDWord(@a[0])^ xor drk[0,0];
  PDWord(@a[1])^:= PDWord(@a[1])^ xor drk[0,1];
  PDWord(@a[2])^:= PDWord(@a[2])^ xor drk[0,2];
  PDWord(@a[3])^:= PDWord(@a[3])^ xor drk[0,3];
  PDword(@OutData)^:= PDword(@a[0,0])^;
  PDword(dword(@OutData)+4)^:= PDword(@a[1,0])^;
  PDword(dword(@OutData)+8)^:= PDword(@a[2,0])^;
  PDword(dword(@OutData)+12)^:= PDword(@a[3,0])^;
end;


end.
