{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2024 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uPasswordStrengthIndicator;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls;

type
  TPasswordStrengthIndicator = class(TFrame)
    PaintBox: TPaintBox;
    procedure PaintBoxPaint(Sender: TObject);
  private
    FPasswordScore: Integer;
    function GetPasswordStrengthInfo: string;
    procedure SetPasswordScore(const Value: Integer);
    procedure SetPasswordStrengthInfo(const Value: string);
  public
    property PasswordScore: Integer read FPasswordScore write SetPasswordScore;
    property PasswordStrengthInfo: string read GetPasswordStrengthInfo
      write SetPasswordStrengthInfo;
  end;

implementation

uses
  uGlobal;

{$R *.dfm}

{ TPasswordStrengthIndicator }

function TPasswordStrengthIndicator.GetPasswordStrengthInfo: string;
begin
  Result := PaintBox.Hint;
end;

procedure TPasswordStrengthIndicator.SetPasswordScore(const Value: Integer);
begin
  if FPasswordScore <> Value then
  begin
    FPasswordScore := Value;
    PaintBox.Invalidate;
    PaintBox.ShowHint := FPasswordScore in [0..5];
  end;
end;

procedure TPasswordStrengthIndicator.SetPasswordStrengthInfo(
  const Value: string);
begin
  PaintBox.Hint := Value;
end;

procedure TPasswordStrengthIndicator.PaintBoxPaint(Sender: TObject);
var
  ARect: TRect;
begin
  PaintBox.Canvas.Brush.Color := clWhite;

  if not (FPasswordScore in [0..5]) then
  begin
    PaintBox.Canvas.Brush.Style := bsClear;
    PaintBox.Canvas.FillRect(PaintBox.ClientRect);
    PaintBox.Canvas.Rectangle(PaintBox.ClientRect);

    Exit;
  end;

  PaintBox.Canvas.Brush.Style := bsSolid;
  PaintBox.Canvas.FillRect(PaintBox.ClientRect);
  PaintBox.Canvas.Rectangle(PaintBox.ClientRect);

  PaintBox.Canvas.Brush.Color := PasswordScoreColors[FPasswordScore];
  ARect := PaintBox.ClientRect;
  ARect.Right := Round(ARect.Left	+ ARect.Width * (FPasswordScore + 1) / 5);
  ARect.Inflate(-1, -1);
  PaintBox.Canvas.FillRect(ARect);
end;

end.
