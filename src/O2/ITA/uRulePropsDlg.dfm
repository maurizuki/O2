object RulePropsDlg: TRulePropsDlg
  Left = 192
  Top = 107
  Caption = 'Propriet'#224' regola'
  ClientHeight = 427
  ClientWidth = 357
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
    357
    427)
  TextHeight = 13
  object btOk: TButton
    Left = 193
    Top = 394
    Width = 75
    Height = 25
    Action = OK
    Anchors = [akRight, akBottom]
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btCancel: TButton
    Left = 274
    Top = 394
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Annulla'
    ModalResult = 2
    TabOrder = 2
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 341
    Height = 380
    ActivePage = tsGeneral
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsGeneral: TTabSheet
      Caption = 'Generale'
      DesignSize = (
        333
        352)
      object Label1: TLabel
        Left = 3
        Top = 12
        Width = 31
        Height = 13
        Caption = '&Nome:'
        FocusControl = edName
      end
      object Label2: TLabel
        Left = 169
        Top = 12
        Width = 28
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '&Tipo:'
        FocusControl = cbType
      end
      object Label3: TLabel
        Left = 3
        Top = 66
        Width = 82
        Height = 13
        Caption = 'Maschera nome &campo:'
        FocusControl = edFieldName
      end
      object Label4: TLabel
        Left = 169
        Top = 66
        Width = 82
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Maschera &valore campo:'
        FocusControl = edFieldValue
      end
      object edName: TEdit
        Left = 3
        Top = 31
        Width = 160
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = edNameChange
        ExplicitWidth = 152
      end
      object cbType: TComboBox
        Left = 169
        Top = 31
        Width = 161
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        TabOrder = 1
        OnChange = cbTypeChange
        ExplicitLeft = 161
      end
      object edFieldName: TEdit
        Left = 3
        Top = 85
        Width = 160
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = edFieldNameChange
        ExplicitWidth = 152
      end
      object edFieldValue: TEdit
        Left = 169
        Top = 85
        Width = 161
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 3
        OnChange = edFieldValueChange
        ExplicitLeft = 161
      end
      object MaskHelpMemo: TMemo
        Left = 3
        Top = 128
        Width = 327
        Height = 221
        TabStop = False
        Anchors = [akLeft, akTop, akRight, akBottom]
        ParentColor = True
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 4
        ExplicitWidth = 319
        ExplicitHeight = 220
      end
    end
    object tsDisplay: TTabSheet
      Caption = 'Aspetto'
      ImageIndex = 5
      DesignSize = (
        333
        352)
      object Label12: TLabel
        Left = 3
        Top = 12
        Width = 28
        Height = 13
        Caption = '&Maschera:'
        FocusControl = edDisplayMask
      end
      object edDisplayMask: TEdit
        Left = 3
        Top = 31
        Width = 327
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        HideSelection = False
        TabOrder = 0
        OnChange = edDisplayMaskChange
        ExplicitWidth = 323
      end
      object btDisplayMacros: TButton
        Left = 255
        Top = 58
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Macros'
        TabOrder = 1
        OnClick = btDisplayMacrosClick
        ExplicitLeft = 251
      end
    end
    object tsHyperLink: TTabSheet
      Caption = 'Collegamento internet'
      ImageIndex = 2
      DesignSize = (
        333
        352)
      object Label7: TLabel
        Left = 3
        Top = 12
        Width = 28
        Height = 13
        Caption = '&Maschera:'
        FocusControl = edMask
      end
      object edMask: TEdit
        Left = 3
        Top = 31
        Width = 327
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        HideSelection = False
        TabOrder = 0
        OnChange = edMaskChange
        ExplicitWidth = 323
      end
      object btHyperLinkMacros: TButton
        Left = 255
        Top = 58
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Macros'
        TabOrder = 1
        OnClick = btHyperLinkMacrosClick
        ExplicitLeft = 251
      end
    end
    object tsDateFormat: TTabSheet
      Caption = 'Formato data'
      ImageIndex = 3
      DesignSize = (
        333
        352)
      object Label8: TLabel
        Left = 3
        Top = 12
        Width = 62
        Height = 13
        Caption = '&Formato data:'
        FocusControl = cbDateFormat
      end
      object Label9: TLabel
        Left = 169
        Top = 12
        Width = 77
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '&Separatore data:'
        FocusControl = edDateSeparator
      end
      object edDateSeparator: TEdit
        Left = 169
        Top = 31
        Width = 161
        Height = 21
        Anchors = [akTop, akRight]
        MaxLength = 1
        TabOrder = 1
        OnChange = edDateSeparatorChange
        ExplicitLeft = 165
      end
      object cbDateFormat: TComboBox
        Left = 3
        Top = 31
        Width = 160
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = cbDateFormatChange
        ExplicitWidth = 156
      end
    end
    object tsCustomFilter: TTabSheet
      Caption = 'Intervallo personalizzato'
      ImageIndex = 4
      DesignSize = (
        333
        352)
      object Label10: TLabel
        Left = 3
        Top = 12
        Width = 94
        Height = 13
        Caption = 'Giorni &prima di oggi:'
        FocusControl = edDaysBefore
      end
      object Label11: TLabel
        Left = 169
        Top = 12
        Width = 86
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Giorni &dopo di oggi:'
        FocusControl = edDaysAfter
      end
      object edDaysBefore: TJvSpinEdit
        Left = 3
        Top = 31
        Width = 160
        Height = 21
        ButtonKind = bkStandard
        MaxValue = 365
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = edDaysBeforeChange
        ExplicitWidth = 156
      end
      object edDaysAfter: TJvSpinEdit
        Left = 169
        Top = 31
        Width = 161
        Height = 21
        ButtonKind = bkStandard
        MaxValue = 365
        Anchors = [akTop, akRight]
        TabOrder = 1
        OnChange = edDaysAfterChange
        ExplicitLeft = 165
      end
    end
    object tsHighlight: TTabSheet
      Caption = 'Evidenziatura'
      ImageIndex = 1
      DesignSize = (
        333
        352)
      object Label5: TLabel
        Left = 3
        Top = 12
        Width = 71
        Height = 13
        Caption = 'Colore &sfondo:'
        FocusControl = cbHighlightColor
      end
      object Label6: TLabel
        Left = 169
        Top = 12
        Width = 52
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Colore &testo:'
        FocusControl = cbHighlightTextColor
      end
      object cbHighlightColor: TColorBox
        Left = 3
        Top = 31
        Width = 160
        Height = 22
        Style = [cbStandardColors, cbCustomColor, cbPrettyNames]
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = cbHighlightColorChange
        ExplicitWidth = 156
      end
      object cbHighlightTextColor: TColorBox
        Left = 169
        Top = 31
        Width = 161
        Height = 22
        Style = [cbStandardColors, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        TabOrder = 1
        OnChange = cbHighlightTextColorChange
        ExplicitLeft = 165
      end
    end
    object tsPassword: TTabSheet
      Caption = 'Password'
      ImageIndex = 6
      DesignSize = (
        333
        352)
      object ckDisplayPasswordStrength: TCheckBox
        Left = 3
        Top = 11
        Width = 327
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Mostra sicurezza delle password'
        TabOrder = 0
        OnClick = ckDisplayPasswordStrengthClick
        ExplicitWidth = 323
      end
    end
  end
  object ActionList: TActionList
    Left = 8
    Top = 392
    object OK: TAction
      Caption = 'OK'
      OnExecute = OKExecute
      OnUpdate = OKUpdate
    end
    object HyperLinkFieldNameMacro: TAction
      Category = 'HyperLink'
      Caption = 'Nome campo'
      OnExecute = HyperLinkFieldNameMacroExecute
    end
    object HyperLinkFieldValueMacro: TAction
      Category = 'HyperLink'
      Caption = 'Valore campo'
      OnExecute = HyperLinkFieldValueMacroExecute
    end
    object DisplayFieldNameMacro: TAction
      Category = 'Display'
      Caption = 'Nome campo'
      OnExecute = DisplayFieldNameMacroExecute
    end
    object DisplayFieldValueMacro: TAction
      Category = 'Display'
      Caption = 'Valore campo'
      OnExecute = DisplayFieldValueMacroExecute
    end
    object DisplayYearsMacro: TAction
      Category = 'Display'
      Caption = 'Anni'
      OnExecute = DisplayYearsMacroExecute
    end
    object DisplayMonthsOfYearMacro: TAction
      Category = 'Display'
      Caption = 'Mesi dell'#39'anno'
      OnExecute = DisplayMonthsOfYearMacroExecute
    end
    object DisplayDaysOfMonthMacro: TAction
      Category = 'Display'
      Caption = 'Giorni del mese'
      OnExecute = DisplayDaysOfMonthMacroExecute
    end
    object DisplayMonthsMacro: TAction
      Category = 'Display'
      Caption = 'Mesi'
      OnExecute = DisplayMonthsMacroExecute
    end
    object DisplayDaysMacro: TAction
      Category = 'Display'
      Caption = 'Giorni'
      OnExecute = DisplayDaysMacroExecute
    end
  end
  object HyperLinkMacrosMenu: TPopupMenu
    Left = 72
    Top = 392
    object Fieldname1: TMenuItem
      Action = HyperLinkFieldNameMacro
    end
    object Fieldvalue1: TMenuItem
      Action = HyperLinkFieldValueMacro
    end
  end
  object DisplayMacrosMenu: TPopupMenu
    Left = 40
    Top = 392
    object Fieldname2: TMenuItem
      Action = DisplayFieldNameMacro
    end
    object Fieldvalue2: TMenuItem
      Action = DisplayFieldValueMacro
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Years1: TMenuItem
      Action = DisplayYearsMacro
    end
    object Monthsofyear1: TMenuItem
      Action = DisplayMonthsOfYearMacro
    end
    object Daysofmonth1: TMenuItem
      Action = DisplayDaysOfMonthMacro
    end
    object Months1: TMenuItem
      Action = DisplayMonthsMacro
    end
    object Days1: TMenuItem
      Action = DisplayDaysMacro
    end
  end
end
