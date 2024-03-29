object ObjPropsDlg: TObjPropsDlg
  Left = 203
  Top = 111
  BorderIcons = [biSystemMenu]
  Caption = 'Object properties'
  ClientHeight = 442
  ClientWidth = 388
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 400
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  DesignSize = (
    388
    442)
  TextHeight = 13
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 372
    Height = 395
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ExplicitWidth = 368
    ExplicitHeight = 394
    object TabSheet1: TTabSheet
      Caption = 'General'
      DesignSize = (
        364
        367)
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
        Width = 358
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = edNameChange
        ExplicitWidth = 354
      end
      object cbTag: TComboBox
        Left = 3
        Top = 132
        Width = 272
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        ExplicitWidth = 268
      end
      object lbxTags: TListBox
        Left = 3
        Top = 159
        Width = 272
        Height = 205
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        Sorted = True
        TabOrder = 3
        ExplicitWidth = 268
        ExplicitHeight = 204
      end
      object Button4: TButton
        Left = 281
        Top = 132
        Width = 80
        Height = 25
        Action = AddTag
        Anchors = [akTop, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        ExplicitLeft = 277
      end
      object Button5: TButton
        Left = 281
        Top = 159
        Width = 80
        Height = 25
        Action = DeleteTag
        Anchors = [akTop, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        ExplicitLeft = 277
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Fields'
      ImageIndex = 1
      DesignSize = (
        364
        367)
      object FieldsView: TListView
        Left = 3
        Top = 3
        Width = 358
        Height = 280
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Field'
            Width = 155
          end
          item
            Caption = 'Value'
            Width = 155
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
        Top = 289
        Width = 156
        Height = 21
        Hint = 'Field name'
        Anchors = [akLeft, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnChange = cbFieldNameChange
      end
      object cbFieldValue: TComboBox
        Left = 165
        Top = 289
        Width = 196
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
        Top = 339
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
        Top = 339
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
        Top = 339
        Width = 75
        Height = 25
        Action = DeleteField
        Anchors = [akLeft, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
      end
      object Button6: TButton
        Left = 289
        Top = 339
        Width = 33
        Height = 25
        Action = MoveUp
        Anchors = [akRight, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
      end
      object Button7: TButton
        Left = 328
        Top = 339
        Width = 33
        Height = 25
        Action = MoveDown
        Anchors = [akRight, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
      end
      inline PasswordStrengthIndicator: TPasswordStrengthIndicator
        Left = 165
        Top = 315
        Width = 196
        Height = 9
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 8
        ExplicitLeft = 165
        ExplicitTop = 315
        ExplicitWidth = 196
        ExplicitHeight = 9
        inherited PaintBox: TPaintBox
          Width = 196
          Height = 9
          ExplicitWidth = 196
          ExplicitHeight = 10
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Notes'
      ImageIndex = 2
      DesignSize = (
        364
        367)
      object Label3: TLabel
        Left = 253
        Top = 348
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
        ExplicitLeft = 222
        ExplicitTop = 323
      end
      object Memo: TMemo
        Left = 3
        Top = 3
        Width = 358
        Height = 338
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
        Top = 347
        Width = 244
        Height = 17
        Anchors = [akLeft, akRight, akBottom]
        Caption = '&Markdown syntax'
        TabOrder = 1
        OnClick = ckMarkdownClick
      end
    end
  end
  object btOk: TButton
    Left = 224
    Top = 409
    Width = 75
    Height = 25
    Action = OK
    Anchors = [akRight, akBottom]
    Default = True
    ModalResult = 1
    TabOrder = 1
    ExplicitLeft = 220
    ExplicitTop = 408
  end
  object btCancel: TButton
    Left = 305
    Top = 409
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    ExplicitLeft = 301
    ExplicitTop = 408
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
