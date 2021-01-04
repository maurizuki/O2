object SetPasswordDlg: TSetPasswordDlg
  Left = 191
  Top = 108
  ActiveControl = cbEncryption
  Anchors = [akRight, akBottom]
  BorderStyle = bsDialog
  Caption = 'Cifratura'
  ClientHeight = 275
  ClientWidth = 257
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    257
    275)
  PixelsPerInch = 96
  TextHeight = 13
  object lbPassword: TLabel
    Left = 12
    Top = 122
    Width = 50
    Height = 13
    Caption = '&Password:'
    FocusControl = edPassword
  end
  object lbConfPassword: TLabel
    Left = 12
    Top = 176
    Width = 112
    Height = 13
    Caption = 'C&onferma password:'
    FocusControl = edConfPassword
  end
  object Label1: TLabel
    Left = 12
    Top = 16
    Width = 55
    Height = 13
    Caption = '&Cifratura:'
    FocusControl = cbEncryption
  end
  object lbHash: TLabel
    Left = 12
    Top = 68
    Width = 75
    Height = 13
    Caption = 'Algoritmo di &hash:'
    FocusControl = cbHash
  end
  object edPassword: TJvEdit
    Left = 12
    Top = 141
    Width = 233
    Height = 21
    ProtectPassword = True
    ThemedPassword = True
    TabOrder = 2
    OnChange = edPasswordChange
  end
  object edConfPassword: TJvEdit
    Left = 12
    Top = 195
    Width = 233
    Height = 21
    ProtectPassword = True
    ThemedPassword = True
    TabOrder = 3
    OnChange = edPasswordChange
  end
  object btOk: TButton
    Left = 50
    Top = 240
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 4
    ExplicitTop = 200
  end
  object btCancel: TButton
    Left = 131
    Top = 240
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Annulla'
    ModalResult = 2
    TabOrder = 5
    ExplicitTop = 200
  end
  object cbEncryption: TComboBox
    Left = 12
    Top = 35
    Width = 233
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = cbEncryptionChange
  end
  object cbHash: TComboBox
    Left = 12
    Top = 87
    Width = 233
    Height = 21
    Style = csDropDownList
    TabOrder = 1
  end
end
