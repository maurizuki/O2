object GetPasswordDlg: TGetPasswordDlg
  Left = 191
  Top = 108
  ActiveControl = edPassword
  BorderStyle = bsDialog
  Caption = 'Password'
  ClientHeight = 87
  ClientWidth = 253
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  DesignSize = (
    253
    87)
  TextHeight = 13
  object edPassword: TJvEdit
    Left = 8
    Top = 16
    Width = 237
    Height = 21
    ProtectPassword = True
    ThemedPassword = True
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = ''
    OnChange = edPasswordChange
    ExplicitWidth = 233
  end
  object btOk: TButton
    Left = 89
    Top = 54
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
    ExplicitLeft = 85
    ExplicitTop = 53
  end
  object btCancel: TButton
    Left = 170
    Top = 54
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Annulla'
    ModalResult = 2
    TabOrder = 2
    ExplicitLeft = 166
    ExplicitTop = 53
  end
end
