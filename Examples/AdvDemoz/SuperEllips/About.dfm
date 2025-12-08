object AboutBox: TAboutBox
  Left = 913
  Height = 218
  Top = 347
  Width = 298
  ActiveControl = OKButton
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 218
  ClientWidth = 298
  Color = clBtnFace
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Position = poScreenCenter
  LCLVersion = '1.8.0.1'
  object Panel1: TPanel
    Left = 8
    Height = 161
    Top = 8
    Width = 281
    BevelOuter = bvLowered
    ClientHeight = 161
    ClientWidth = 281
    TabOrder = 0
    object ProgramIcon: TImage
      Left = 8
      Height = 57
      Top = 8
      Width = 65
    end
    object ProductName: TLabel
      Left = 88
      Height = 13
      Top = 16
      Width = 68
      Caption = 'Product Name'
      ParentColor = False
    end
    object Version: TLabel
      Left = 88
      Height = 13
      Top = 40
      Width = 35
      Caption = 'Version'
      ParentColor = False
    end
    object Copyright: TLabel
      Left = 8
      Height = 13
      Top = 80
      Width = 44
      Caption = 'Copyright'
      ParentColor = False
    end
    object Comments: TLabel
      Left = 8
      Height = 13
      Top = 104
      Width = 49
      Caption = 'Comments'
      ParentColor = False
      WordWrap = True
    end
  end
  object OKButton: TButton
    Left = 120
    Height = 33
    Top = 178
    Width = 65
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
