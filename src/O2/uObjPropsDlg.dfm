object ObjPropsDlg: TObjPropsDlg
  Left = 203
  Top = 111
  BorderIcons = [biSystemMenu]
  Caption = 'Object properties'
  ClientHeight = 417
  ClientWidth = 357
  Color = clBtnFace
  Constraints.MinHeight = 452
  Constraints.MinWidth = 354
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
    357
    417)
  TextHeight = 13
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 341
    Height = 370
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'General'
      DesignSize = (
        333
        342)
      object Label1: TLabel
        Left = 3
        Top = 12
        Width = 31
        Height = 13
        Caption = '&Name:'
        FocusControl = edName
      end
      object Label2: TLabel
        Left = 3
        Top = 113
        Width = 27
        Height = 13
        Caption = '&Tags:'
        FocusControl = cbTag
      end
      object edName: TEdit
        Left = 3
        Top = 31
        Width = 327
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = edNameChange
        ExplicitWidth = 319
      end
      object cbTag: TComboBox
        Left = 3
        Top = 132
        Width = 241
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        ExplicitWidth = 233
      end
      object lbxTags: TListBox
        Left = 3
        Top = 159
        Width = 241
        Height = 180
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        Sorted = True
        TabOrder = 3
        ExplicitWidth = 233
        ExplicitHeight = 179
      end
      object Button4: TButton
        Left = 250
        Top = 132
        Width = 80
        Height = 25
        Action = AddTag
        Anchors = [akTop, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        ExplicitLeft = 242
      end
      object Button5: TButton
        Left = 250
        Top = 159
        Width = 80
        Height = 25
        Action = DeleteTag
        Anchors = [akTop, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        ExplicitLeft = 242
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Fields'
      ImageIndex = 1
      DesignSize = (
        333
        342)
      object pbPasswordStrength: TPaintBox
        Left = 152
        Top = 290
        Width = 178
        Height = 17
        Cursor = crHelp
        Anchors = [akLeft, akRight, akBottom]
        ParentShowHint = False
        ShowHint = True
        Visible = False
        OnPaint = pbPasswordStrengthPaint
        ExplicitTop = 292
      end
      object FieldsView: TListView
        Left = 3
        Top = 3
        Width = 327
        Height = 255
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Field'
            Width = 140
          end
          item
            Caption = 'Value'
            Width = 160
          end>
        ColumnClick = False
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnResize = FieldsViewResize
        OnSelectItem = FieldsViewSelectItem
      end
      object cbFieldName: TComboBox
        Left = 3
        Top = 264
        Width = 143
        Height = 21
        Hint = 'Field name'
        Anchors = [akLeft, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
      object cbFieldValue: TComboBox
        Left = 152
        Top = 264
        Width = 178
        Height = 21
        Hint = 'Field value'
        Anchors = [akLeft, akRight, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnChange = cbFieldValueChange
        OnEnter = cbFieldValueEnter
      end
      object Button1: TButton
        Left = 3
        Top = 314
        Width = 75
        Height = 25
        Action = AddField
        Anchors = [akLeft, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
      end
      object Button2: TButton
        Left = 84
        Top = 314
        Width = 75
        Height = 25
        Action = ReplaceField
        Anchors = [akLeft, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
      end
      object Button3: TButton
        Left = 165
        Top = 314
        Width = 75
        Height = 25
        Action = DeleteField
        Anchors = [akLeft, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
      end
      object Button6: TButton
        Left = 258
        Top = 314
        Width = 33
        Height = 25
        Action = MoveUp
        Anchors = [akRight, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
      end
      object Button7: TButton
        Left = 297
        Top = 314
        Width = 33
        Height = 25
        Action = MoveDown
        Anchors = [akRight, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
      end
      object ckDisplayPasswordStrength: TCheckBox
        Left = 3
        Top = 290
        Width = 143
        Height = 17
        Anchors = [akLeft, akRight, akBottom]
        Caption = '&Password strength'
        TabOrder = 8
        OnClick = ckDisplayPasswordStrengthClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Notes'
      ImageIndex = 2
      DesignSize = (
        333
        342)
      object Label3: TLabel
        Left = 222
        Top = 323
        Width = 108
        Height = 13
        Cursor = crHandPoint
        Hint = 'https://commonmark.org/help/'
        Alignment = taRightJustify
        Anchors = [akRight, akBottom]
        Caption = 'Markdown syntax help'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = LinkClick
      end
      object Memo: TMemo
        Left = 3
        Top = 3
        Width = 327
        Height = 313
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
        WantTabs = True
      end
      object ckMarkdown: TCheckBox
        Left = 3
        Top = 322
        Width = 213
        Height = 17
        Anchors = [akLeft, akRight, akBottom]
        Caption = '&Markdown syntax'
        TabOrder = 1
      end
    end
  end
  object btOk: TButton
    Left = 193
    Top = 384
    Width = 75
    Height = 25
    Action = OK
    Anchors = [akRight, akBottom]
    Default = True
    TabOrder = 1
  end
  object btCancel: TButton
    Left = 274
    Top = 384
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ActionList: TActionList
    Left = 8
    Top = 384
    object OK: TAction
      Caption = 'OK'
      OnExecute = OKExecute
      OnUpdate = OKUpdate
    end
    object AddField: TAction
      Category = 'Fields'
      Caption = '&Add'
      Hint = 'Add field'
      ShortCut = 16429
      OnExecute = AddFieldExecute
      OnUpdate = AddFieldUpdate
    end
    object ReplaceField: TAction
      Category = 'Fields'
      Caption = '&Replace'
      Hint = 'Replace field'
      ShortCut = 24621
      OnExecute = ReplaceFieldExecute
      OnUpdate = ReplaceFieldUpdate
    end
    object DeleteField: TAction
      Category = 'Fields'
      Caption = '&Delete'
      Hint = 'Delete field'
      ShortCut = 16430
      OnExecute = DeleteFieldExecute
      OnUpdate = DeleteFieldUpdate
    end
    object MoveUp: TAction
      Category = 'Fields'
      Caption = #9650
      Hint = 'Move up'
      ShortCut = 24614
      OnExecute = MoveUpExecute
      OnUpdate = MoveUpUpdate
    end
    object MoveDown: TAction
      Category = 'Fields'
      Caption = #9660
      Hint = 'Move down'
      ShortCut = 24616
      OnExecute = MoveDownExecute
      OnUpdate = MoveDownUpdate
    end
    object AddTag: TAction
      Category = 'Tags'
      Caption = '&Add'
      Hint = 'Add tag'
      ShortCut = 8237
      OnExecute = AddTagExecute
      OnUpdate = AddTagUpdate
    end
    object DeleteTag: TAction
      Category = 'Tags'
      Caption = '&Delete'
      Hint = 'Delete tag'
      ShortCut = 8238
      OnExecute = DeleteTagExecute
      OnUpdate = DeleteTagUpdate
    end
  end
end
