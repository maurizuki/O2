object RTFViewer: TRTFViewer
  Left = 0
  Top = 0
  Caption = 'RTFViewer'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 15
  object RichEdit: TRichEdit
    Left = 0
    Top = 0
    Width = 628
    Height = 442
    TabStop = False
    Align = alClient
    EnableURLs = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitWidth = 624
    ExplicitHeight = 441
  end
end
