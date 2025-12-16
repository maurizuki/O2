{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2026 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uRTFViewer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls;

type
  TRTFViewer = class(TForm)
    RichEdit: TRichEdit;
  public
    class procedure Execute(AOwner: TComponent; const FileName: string);
  end;

var
  RTFViewer: TRTFViewer;

implementation

{$R *.dfm}

{ TRTFViewer }

class procedure TRTFViewer.Execute(AOwner: TComponent; const FileName: string);
var
  Form: TRTFViewer;
begin
  Form := TRTFViewer.Create(AOwner);
  try
    Form.Caption := ExtractFileName(FileName);
    Form.RichEdit.Lines.LoadFromFile(FileName);
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

end.
