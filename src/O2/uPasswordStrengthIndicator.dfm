object PasswordStrengthIndicator: TPasswordStrengthIndicator
  Left = 0
  Top = 0
  Width = 300
  Height = 30
  TabOrder = 0
  object PaintBox: TPaintBox
    Left = 0
    Top = 0
    Width = 300
    Height = 30
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    OnPaint = PaintBoxPaint
    ExplicitWidth = 321
    ExplicitHeight = 129
  end
end
