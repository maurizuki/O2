object ReplaceDlg: TReplaceDlg
  Left = 191
  Top = 108
  BorderStyle = bsDialog
  ClientHeight = 165
  ClientWidth = 253
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    253
    165)
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
    Left = 48
    Top = 130
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 2
  end
  object btCancel: TButton
    Left = 129
    Top = 130
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
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
