object ReplaceDlg: TReplaceDlg
  Left = 191
  Top = 108
  BorderStyle = bsDialog
  ClientHeight = 166
  ClientWidth = 257
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    257
    166)
  PixelsPerInch = 96
  TextHeight = 13
  object lbReplaceValue: TLabel
    Left = 12
    Top = 68
    Width = 72
    Height = 13
    Caption = 'lbReplaceValue'
    FocusControl = cbReplaceValue
  end
  object lbSearchValue: TLabel
    Left = 12
    Top = 16
    Width = 67
    Height = 13
    Caption = 'lbSearchValue'
    FocusControl = cbSearchValue
  end
  object btOk: TButton
    Left = 50
    Top = 131
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 2
    ExplicitTop = 112
  end
  object btCancel: TButton
    Left = 131
    Top = 131
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Annulla'
    ModalResult = 2
    TabOrder = 3
    ExplicitTop = 112
  end
  object cbSearchValue: TComboBox
    Left = 12
    Top = 35
    Width = 233
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = cbSearchValueChange
  end
  object cbReplaceValue: TComboBox
    Left = 12
    Top = 87
    Width = 233
    Height = 21
    TabOrder = 1
    OnChange = cbReplaceValueChange
  end
end
