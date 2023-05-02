object RulePropsDlg: TRulePropsDlg
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Rule properties'
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
    Top = 393
    Width = 75
    Height = 25
    Action = OK
    Anchors = [akRight, akBottom]
    Default = True
    TabOrder = 1
    ExplicitLeft = 189
    ExplicitTop = 392
  end
  object btCancel: TButton
    Left = 274
    Top = 393
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    ExplicitLeft = 270
    ExplicitTop = 392
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 341
    Height = 379
    ActivePage = tsGeneral
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ExplicitWidth = 337
    ExplicitHeight = 378
    object tsGeneral: TTabSheet
      Caption = 'General'
      object Label1: TLabel
        Left = 3
        Top = 12
        Width = 31
        Height = 13
        Caption = '&Name:'
        FocusControl = edName
      end
      object Label2: TLabel
        Left = 169
        Top = 12
        Width = 28
        Height = 13
        Caption = '&Type:'
        FocusControl = cbType
      end
      object Label3: TLabel
        Left = 3
        Top = 66
        Width = 82
        Height = 13
        Caption = '&Field name mask:'
        FocusControl = edFieldName
      end
      object Label4: TLabel
        Left = 169
        Top = 66
        Width = 82
        Height = 13
        Caption = 'Field &value mask:'
        FocusControl = edFieldValue
      end
      object edName: TEdit
        Left = 3
        Top = 31
        Width = 160
        Height = 21
        TabOrder = 0
        OnChange = edNameChange
      end
      object cbType: TComboBox
        Left = 169
        Top = 31
        Width = 161
        Height = 21
        Style = csDropDownList
        TabOrder = 1
        OnChange = cbTypeChange
      end
      object edFieldName: TEdit
        Left = 3
        Top = 85
        Width = 160
        Height = 21
        TabOrder = 2
      end
      object edFieldValue: TEdit
        Left = 169
        Top = 85
        Width = 161
        Height = 21
        TabOrder = 3
      end
      object MaskHelpMemo: TMemo
        Left = 3
        Top = 128
        Width = 327
        Height = 214
        TabStop = False
        ParentColor = True
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 4
      end
    end
    object tsDisplay: TTabSheet
      Caption = 'Display'
      ImageIndex = 5
      object Label12: TLabel
        Left = 3
        Top = 12
        Width = 28
        Height = 13
        Caption = '&Mask:'
        FocusControl = edDisplayMask
      end
      object edDisplayMask: TEdit
        Left = 3
        Top = 31
        Width = 327
        Height = 21
        HideSelection = False
        TabOrder = 0
      end
      object btDisplayMacros: TButton
        Left = 255
        Top = 58
        Width = 75
        Height = 25
        Caption = 'Macros'
        TabOrder = 1
        OnClick = btDisplayMacrosClick
      end
    end
    object tsHyperLink: TTabSheet
      Caption = 'Internet link'
      ImageIndex = 2
      object Label7: TLabel
        Left = 3
        Top = 12
        Width = 28
        Height = 13
        Caption = '&Mask:'
        FocusControl = edMask
      end
      object edMask: TEdit
        Left = 3
        Top = 31
        Width = 327
        Height = 21
        HideSelection = False
        TabOrder = 0
      end
      object btHyperLinkMacros: TButton
        Left = 255
        Top = 58
        Width = 75
        Height = 25
        Caption = 'Macros'
        TabOrder = 1
        OnClick = btHyperLinkMacrosClick
      end
    end
    object tsDateFormat: TTabSheet
      Caption = 'Date format'
      ImageIndex = 3
      object Label8: TLabel
        Left = 3
        Top = 12
        Width = 62
        Height = 13
        Caption = 'Date &format:'
        FocusControl = cbDateFormat
      end
      object Label9: TLabel
        Left = 169
        Top = 12
        Width = 77
        Height = 13
        Caption = 'Date &separator:'
        FocusControl = edDateSeparator
      end
      object edDateSeparator: TEdit
        Left = 169
        Top = 31
        Width = 161
        Height = 21
        MaxLength = 1
        TabOrder = 1
      end
      object cbDateFormat: TComboBox
        Left = 3
        Top = 31
        Width = 160
        Height = 21
        Style = csDropDownList
        TabOrder = 0
      end
    end
    object tsCustomFilter: TTabSheet
      Caption = 'Custom interval'
      ImageIndex = 4
      object Label10: TLabel
        Left = 3
        Top = 12
        Width = 94
        Height = 13
        Caption = 'Days &before today:'
        FocusControl = edDaysBefore
      end
      object Label11: TLabel
        Left = 169
        Top = 12
        Width = 86
        Height = 13
        Caption = 'Days &after today:'
        FocusControl = edDaysAfter
      end
      object edDaysBefore: TJvSpinEdit
        Left = 3
        Top = 31
        Width = 160
        Height = 21
        ButtonKind = bkStandard
        MaxValue = 365
        TabOrder = 0
      end
      object edDaysAfter: TJvSpinEdit
        Left = 169
        Top = 31
        Width = 161
        Height = 21
        ButtonKind = bkStandard
        MaxValue = 365
        TabOrder = 1
      end
    end
    object tsHighlight: TTabSheet
      Caption = 'Highlight'
      ImageIndex = 1
      object Label5: TLabel
        Left = 3
        Top = 12
        Width = 71
        Height = 13
        Caption = '&Highlight color:'
        FocusControl = cbHighlightColor
      end
      object Label6: TLabel
        Left = 169
        Top = 12
        Width = 52
        Height = 13
        Caption = '&Text color:'
        FocusControl = cbHighlightTextColor
      end
      object cbHighlightColor: TColorBox
        Left = 3
        Top = 31
        Width = 160
        Height = 22
        Style = [cbStandardColors, cbCustomColor, cbPrettyNames]
        TabOrder = 0
      end
      object cbHighlightTextColor: TColorBox
        Left = 169
        Top = 31
        Width = 161
        Height = 22
        Style = [cbStandardColors, cbCustomColor, cbPrettyNames]
        TabOrder = 1
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
      Caption = 'Field name'
      OnExecute = HyperLinkFieldNameMacroExecute
    end
    object HyperLinkFieldValueMacro: TAction
      Category = 'HyperLink'
      Caption = 'Field value'
      OnExecute = HyperLinkFieldValueMacroExecute
    end
    object DisplayFieldNameMacro: TAction
      Category = 'Display'
      Caption = 'Field name'
      OnExecute = DisplayFieldNameMacroExecute
    end
    object DisplayFieldValueMacro: TAction
      Category = 'Display'
      Caption = 'Field value'
      OnExecute = DisplayFieldValueMacroExecute
    end
    object DisplayYearsMacro: TAction
      Category = 'Display'
      Caption = 'Years'
      OnExecute = DisplayYearsMacroExecute
    end
    object DisplayMonthsOfYearMacro: TAction
      Category = 'Display'
      Caption = 'Months of year'
      OnExecute = DisplayMonthsOfYearMacroExecute
    end
    object DisplayDaysOfMonthMacro: TAction
      Category = 'Display'
      Caption = 'Days of month'
      OnExecute = DisplayDaysOfMonthMacroExecute
    end
    object DisplayMonthsMacro: TAction
      Category = 'Display'
      Caption = 'Months'
      OnExecute = DisplayMonthsMacroExecute
    end
    object DisplayDaysMacro: TAction
      Category = 'Display'
      Caption = 'Days'
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
