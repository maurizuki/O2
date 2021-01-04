object ObjPropsDlg: TObjPropsDlg
  Left = 203
  Top = 111
  BorderIcons = [biSystemMenu]
  Caption = 'Propriet'#224' oggetto'
  ClientHeight = 418
  ClientWidth = 361
  Color = clBtnFace
  Constraints.MinHeight = 452
  Constraints.MinWidth = 369
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    361
    418)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 6
    Top = 8
    Width = 349
    Height = 372
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Generale'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        341
        344)
      object Label1: TLabel
        Left = 8
        Top = 12
        Width = 31
        Height = 13
        Caption = '&Nome:'
        FocusControl = edName
      end
      object Label2: TLabel
        Left = 8
        Top = 113
        Width = 27
        Height = 13
        Caption = 'E&tichette:'
        FocusControl = cbTag
      end
      object edName: TEdit
        Left = 8
        Top = 31
        Width = 325
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = edNameChange
      end
      object cbTag: TComboBox
        Left = 8
        Top = 132
        Width = 244
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object lbxTags: TListBox
        Left = 8
        Top = 159
        Width = 244
        Height = 174
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        Sorted = True
        TabOrder = 3
      end
      object Button4: TButton
        Left = 258
        Top = 132
        Width = 75
        Height = 25
        Action = AddTag
        Anchors = [akTop, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
      end
      object Button5: TButton
        Left = 258
        Top = 159
        Width = 75
        Height = 25
        Action = DeleteTag
        Anchors = [akTop, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Campi'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        341
        344)
      object FieldsView: TListView
        Left = 8
        Top = 9
        Width = 325
        Height = 261
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Campo'
            Width = 140
          end
          item
            Caption = 'Valore'
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
        Left = 8
        Top = 276
        Width = 138
        Height = 21
        Hint = 'Nome campo'
        Anchors = [akLeft, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
      object cbFieldValue: TComboBox
        Left = 152
        Top = 276
        Width = 181
        Height = 21
        Hint = 'Valore campo'
        Anchors = [akLeft, akRight, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnEnter = cbFieldValueEnter
      end
      object Button1: TButton
        Left = 8
        Top = 311
        Width = 75
        Height = 25
        Action = AddField
        Anchors = [akLeft, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
      end
      object Button2: TButton
        Left = 89
        Top = 311
        Width = 75
        Height = 25
        Action = ReplaceField
        Anchors = [akLeft, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
      end
      object Button3: TButton
        Left = 170
        Top = 311
        Width = 75
        Height = 25
        Action = DeleteField
        Anchors = [akLeft, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
      end
      object Button6: TButton
        Left = 261
        Top = 311
        Width = 33
        Height = 25
        Action = MoveUp
        Anchors = [akRight, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
      end
      object Button7: TButton
        Left = 300
        Top = 311
        Width = 33
        Height = 25
        Action = MoveDown
        Anchors = [akRight, akBottom]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Note'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        341
        344)
      object Memo: TMemo
        Left = 8
        Top = 9
        Width = 325
        Height = 324
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
    end
  end
  object btOk: TButton
    Left = 199
    Top = 387
    Width = 75
    Height = 25
    Action = OK
    Anchors = [akRight, akBottom]
    Default = True
    TabOrder = 1
  end
  object btCancel: TButton
    Left = 280
    Top = 387
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Annulla'
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
      Caption = '&Aggiungi'
      Hint = 'Aggiungi campo'
      ShortCut = 16429
      OnExecute = AddFieldExecute
      OnUpdate = AddFieldUpdate
    end
    object ReplaceField: TAction
      Category = 'Fields'
      Caption = '&Sostituisci'
      Hint = 'Sostituisci campo'
      ShortCut = 24621
      OnExecute = ReplaceFieldExecute
      OnUpdate = ReplaceFieldUpdate
    end
    object DeleteField: TAction
      Category = 'Fields'
      Caption = '&Elimina'
      Hint = 'Elimina campo'
      ShortCut = 16430
      OnExecute = DeleteFieldExecute
      OnUpdate = DeleteFieldUpdate
    end
    object MoveUp: TAction
      Category = 'Fields'
      Caption = #9650
      Hint = 'Sposta in alto'
      ShortCut = 24614
      OnExecute = MoveUpExecute
      OnUpdate = MoveUpUpdate
    end
    object MoveDown: TAction
      Category = 'Fields'
      Caption = #9660
      Hint = 'Sposta in basso'
      ShortCut = 24616
      OnExecute = MoveDownExecute
      OnUpdate = MoveDownUpdate
    end
    object AddTag: TAction
      Category = 'Tags'
      Caption = '&Aggiungi'
      Hint = 'Aggiungi etichetta'
      ShortCut = 8237
      OnExecute = AddTagExecute
      OnUpdate = AddTagUpdate
    end
    object DeleteTag: TAction
      Category = 'Tags'
      Caption = '&Elimina'
      Hint = 'Elimina etichetta'
      ShortCut = 8238
      OnExecute = DeleteTagExecute
      OnUpdate = DeleteTagUpdate
    end
  end
end
