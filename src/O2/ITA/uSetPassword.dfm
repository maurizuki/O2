object SetPasswordDlg: TSetPasswordDlg
  Left = 191
  Top = 108
  ActiveControl = cbEncryption
  Anchors = [akRight, akBottom]
  BorderStyle = bsDialog
  Caption = 'Cifratura'
  ClientHeight = 274
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    430
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
    Width = 178
    Height = 21
    ProtectPassword = True
    ThemedPassword = True
    TabOrder = 2
    Text = ''
    OnChange = edPasswordChange
  end
  object edConfPassword: TJvEdit
    Left = 8
    Top = 195
    Width = 178
    Height = 21
    ProtectPassword = True
    ThemedPassword = True
    TabOrder = 3
    Text = ''
    OnChange = edPasswordChange
  end
  object btOk: TButton
    Left = 266
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
    Left = 347
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
    Width = 178
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = cbEncryptionChange
  end
  object cbHash: TComboBox
    Left = 8
    Top = 87
    Width = 178
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = cbHashChange
  end
  object gbPasswordStrength: TGroupBox
    Left = 192
    Top = 16
    Width = 230
    Height = 200
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Sicurezza della password'
    ParentBackground = False
    TabOrder = 6
    DesignSize = (
      230
      200)
    object pbPasswordStrength: TPaintBox
      Left = 3
      Top = 19
      Width = 224
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      OnPaint = pbPasswordStrengthPaint
      ExplicitWidth = 232
    end
    object PasswordStrengthMemo: TMemo
      Left = 3
      Top = 52
      Width = 224
      Height = 145
      TabStop = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      ExplicitWidth = 216
    end
  end
end
