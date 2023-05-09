object FilePropsDlg: TFilePropsDlg
  Left = 253
  Top = 110
  BorderStyle = bsDialog
  Caption = 'Propriet'#224' file'
  ClientHeight = 330
  ClientWidth = 293
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    293
    330)
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 24
    Height = 13
    Caption = '&Titolo:'
    FocusControl = edTitle
  end
  object Label2: TLabel
    Left = 8
    Top = 66
    Width = 57
    Height = 13
    Caption = '&Descrizione:'
    FocusControl = edDescription
  end
  object Label3: TLabel
    Left = 8
    Top = 120
    Width = 37
    Height = 13
    Caption = '&Autore:'
    FocusControl = edAuthor
  end
  object Label4: TLabel
    Left = 8
    Top = 174
    Width = 55
    Height = 13
    Caption = '&Cifratura:'
    FocusControl = edCipher
  end
  object Label5: TLabel
    Left = 8
    Top = 228
    Width = 75
    Height = 13
    Caption = 'Algoritmo di &hash:'
    FocusControl = edHash
  end
  object edTitle: TEdit
    Left = 8
    Top = 31
    Width = 277
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    ExplicitWidth = 273
  end
  object edDescription: TEdit
    Left = 8
    Top = 85
    Width = 277
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    ExplicitWidth = 273
  end
  object edAuthor: TEdit
    Left = 8
    Top = 139
    Width = 277
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    ExplicitWidth = 273
  end
  object edCipher: TEdit
    Left = 8
    Top = 193
    Width = 277
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 3
    ExplicitWidth = 273
  end
  object edHash: TEdit
    Left = 8
    Top = 247
    Width = 277
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
    ExplicitWidth = 273
  end
  object btOk: TButton
    Left = 129
    Top = 297
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = btOkClick
    ExplicitLeft = 125
    ExplicitTop = 296
  end
  object btCancel: TButton
    Left = 210
    Top = 297
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Annulla'
    ModalResult = 2
    TabOrder = 6
    ExplicitLeft = 206
    ExplicitTop = 296
  end
end
