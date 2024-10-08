object RelationPropsDlg: TRelationPropsDlg
  Left = 0
  Top = 0
  ActiveControl = cbRole1
  BorderStyle = bsDialog
  Caption = 'Relation properties'
  ClientHeight = 166
  ClientWidth = 333
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  DesignSize = (
    333
    166)
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 45
    Height = 13
    Caption = 'Object 1:'
    FocusControl = edObject1
  end
  object Label2: TLabel
    Left = 8
    Top = 64
    Width = 45
    Height = 13
    Caption = 'Object 2:'
    FocusControl = edObject2
  end
  object Label3: TLabel
    Left = 167
    Top = 12
    Width = 34
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Role &1:'
    FocusControl = cbRole1
  end
  object Label4: TLabel
    Left = 167
    Top = 64
    Width = 34
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Role &2:'
    FocusControl = cbRole2
  end
  object edObject1: TEdit
    Left = 8
    Top = 31
    Width = 153
    Height = 21
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
  end
  object edObject2: TEdit
    Left = 8
    Top = 83
    Width = 153
    Height = 21
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 2
  end
  object cbRole1: TComboBox
    Left = 167
    Top = 31
    Width = 158
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 1
    OnChange = cbRole1Change
  end
  object cbRole2: TComboBox
    Left = 167
    Top = 83
    Width = 158
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 3
    OnChange = cbRole2Change
  end
  object btOk: TButton
    Left = 167
    Top = 133
    Width = 76
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = btOkClick
  end
  object btCancel: TButton
    Left = 249
    Top = 133
    Width = 76
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
