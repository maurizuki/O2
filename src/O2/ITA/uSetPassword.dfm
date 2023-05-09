object SetPasswordDlg: TSetPasswordDlg
  Left = 191
  Top = 108
  ActiveControl = cbEncryption
  Anchors = [akRight, akBottom]
  BorderStyle = bsDialog
  Caption = 'Cifratura'
  ClientHeight = 274
  ClientWidth = 253
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    253
    274)
  TextHeight = 13
  object lbPassword: TLabel
    Left = 8
    Top = 122
    Width = 50
    Height = 13
    Caption = '&Password:'
    FocusControl = edPassword
  end
  object lbConfPassword: TLabel
    Left = 8
    Top = 176
    Width = 112
    Height = 13
    Caption = 'C&onferma password:'
    FocusControl = edConfPassword
  end
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 55
    Height = 13
    Caption = '&Cifratura:'
    FocusControl = cbEncryption
  end
  object lbHash: TLabel
    Left = 8
    Top = 68
    Width = 75
    Height = 13
    Caption = 'Algoritmo di &hash:'
    FocusControl = cbHash
  end
  object edPassword: TJvEdit
    Left = 8
    Top = 141
    Width = 237
    Height = 21
    ProtectPassword = True
    ThemedPassword = True
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = ''
    OnChange = edPasswordChange
  end
  object edConfPassword: TJvEdit
    Left = 8
    Top = 195
    Width = 237
    Height = 21
    ProtectPassword = True
    ThemedPassword = True
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = ''
    OnChange = edPasswordChange
  end
  object btOk: TButton
    Left = 89
    Top = 241
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 4
  end
  object btCancel: TButton
    Left = 170
    Top = 241
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Annulla'
    ModalResult = 2
    TabOrder = 5
  end
  object cbEncryption: TComboBox
    Left = 8
    Top = 35
    Width = 237
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = cbEncryptionChange
  end
  object cbHash: TComboBox
    Left = 8
    Top = 87
    Width = 237
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = cbHashChange
  end
end
