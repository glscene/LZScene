object DerivativesForm: TDerivativesForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 372
  ClientWidth = 318
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 2
    Top = 3
    Width = 312
    Height = 366
    Caption = '  Plot Range  '
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    DesignSize = (
      312
      366)
    object Label1: TLabel
      Left = 54
      Top = 15
      Width = 40
      Height = 13
      Caption = 'Minimum'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 142
      Top = 15
      Width = 44
      Height = 13
      Caption = 'Maximum'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 232
      Top = 15
      Width = 49
      Height = 13
      Alignment = taRightJustify
      Caption = 'Increment'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 12
      Top = 31
      Width = 13
      Height = 13
      Alignment = taRightJustify
      Caption = 'X :'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 12
      Top = 53
      Width = 13
      Height = 13
      Alignment = taRightJustify
      Caption = 'Y :'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 3
      Top = 75
      Width = 20
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Z :'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label8: TLabel
      Left = 13
      Top = 128
      Width = 38
      Height = 13
      Alignment = taRightJustify
      Caption = 'Modes :'
    end
    object zCountLabel: TLabel
      Left = 7
      Top = 150
      Width = 297
      Height = 22
      Alignment = taCenter
      AutoSize = False
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
    end
    object GridValues: TSpeedButton
      Left = 212
      Top = 98
      Width = 90
      Height = 22
      Cursor = crHandPoint
      Hint = 'Set Plot Range values to the function'#39's grid values'
      Caption = 'Grid Values'
      Flat = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      NumGlyphs = 2
      ParentFont = False
      OnClick = GridValuesClick
      OnMouseUp = GridValuesMouseUp
    end
    object PlotValues: TSpeedButton
      Left = 212
      Top = 73
      Width = 90
      Height = 21
      Cursor = crHandPoint
      Hint = 'Set Plot Range values to the function'#39's plot values'
      Caption = 'Plot Values'
      Flat = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      NumGlyphs = 2
      ParentFont = False
      OnClick = PlotValuesClick
      OnMouseUp = PlotValuesMouseUp
    end
    object Label7: TLabel
      Left = 9
      Top = 287
      Width = 56
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Line width:'
    end
    object ColorButton: TSpeedButton
      Left = 181
      Top = 280
      Width = 100
      Height = 29
      Caption = 'Line Color...'
      Glyph.Data = {
        F6060000424DF606000000000000360000002800000018000000180000000100
        180000000000C00600000000000000000000000000000000000082C9F77FC8F7
        86CBF78CCEF792D0F898D3F89FD6F8A5D8F9AADBF9B1DDFAB7E0FABCE3FBC2E5
        FBC7E7FBCDE9FCD2ECFCD7EEFCDCF0FDE1F3FDE6F5FDE9F7FEEDF8FEF2F9FEF5
        FBFE70C3F570C2F677C5F67EC8F784CBF78BCDF792D0F898D3F89ED6F9A6D9F9
        ACDBFAB2DEFAB8E1FBBFE3FBC4E6FBCAE9FCCFEBFCD5EDFCDAEFFDDFF2FDE4F4
        FDE8F6FEEDF7FEF2F9FE6DC1F66ABFF571C2F678C5F6FF0000FF0000FF0000FF
        0000FF00009FD6F9FF0000FF0000FF0000B8E1FAFF0000C4E6FBFF0000FF0000
        D5EDFCDAF0FDDFF2FDE4F4FDE8F6FEEEF8FE67BEF563BDF56BC0F571C3F6FF00
        007EC8F785CBF78CCDF892D0F7FF00009FD6F9A6D9F9FF0000B2DEFAFF0000BF
        E4FBFF0000CAE8FCCFEBFCD5EDFCDAF0FDDFF2FDE4F4FDEAF7FE60BBF55EBAF4
        64BDF56BC0F5FF000077C5F67EC8F785CBF78CCEF7FF000098D3F89FD6F9FF00
        00ABDCFAFF0000B9E1FBFF0000C4E6FBCAE9FCCFEBFCD5EDFCDAEFFDDFF2FDE6
        F5FD5BB9F557B7F45EBAF464BDF5FF0000FF0000FF0000FF0000FF0000FF0000
        C0C0C099D4F8FF0000C0C0C0FF0000C0C0C0FF0000BFE3FBC4E6FBCAE9FCD0EA
        FCD5EDFCDAF0FDE2F3FD56B7F451B5F458B8F45EBAF5FF00006BC0F5FF00FFFF
        00FFFF00FFFF00FFFF0000FF0000FF000000FFFFFF0000FF0000FF0000FF0000
        BFE4FBC4E6FBCAE9FCD0EBFCD6EEFCDDF1FD50B5F44AB2F351B5F457B7F4FF00
        00FF00FFFF00FFFF00FFFF00FFFF00FF0000FF0000FFFF000000FFFF00FFFF00
        FFFFFF000000FFFF00FFFFBFE3FBC5E6FBCAE9FCCFEBFCD8EFFC49B2F344B0F3
        4BB2F352B5F4FF0000FF0000FF0000FF0000FF0000FF00FF0000FF0000FFFF00
        000000FFFF000000FFFF00FFFF00FFFF00FFFFB9E1FBBFE4FBC5E6FBCAE9FCD3
        ECFC45AFF33FADF346AFF34CB2F3FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        0000FF0000FF0000FF0000FF00FFFF00FFFF00FFFF00FFFF00FFFFC0C0C0B9E1
        FBBFE4FBC5E6FBCEE9FC3FADF339AAF240ADF346B0F3FF00FFFF00FFFF00FFFF
        00FFFF00FF80008000008000000000000000008000808000FFFF00FFFF00FFFF
        00FFFF00FFFFB3DEFAB9E1FBBFE4FBC8E7FB3AABF234A8F239ABF23FADF2FF00
        FFFF00FFFF00FFFF00FFFF00FF80000000000000000000000000000000800000
        FF0000FFFF00FFFF00FFFFC0C0C0ADDCFAB3DFFABAE1FBC4E6FB36A8F22EA6F1
        34A8F23AAAF240ADF2FF00FFFF00FFFF00FFFF0000FF00000000000000000000
        0000000000FF0000FF0000FFFF00FFFF00FFFFA0D6F9A7D9F9ADDCFAB4DFFABE
        E3FB30A7F229A3F12FA6F134A9F239ABF2FF00FFFF00FFFF0000FF0000FF0000
        80000000000000000000800000FF0000FF0000FF0000FFFF00FFFF99D4F8A0D7
        F9A6D9F9ADDCFAB8E1FA2CA6F124A2F029A4F12FA6F134A8F23AABF2FF00FFFF
        0000FF0000FF0000FF000000000000800000FF0000FF0000FF0000FF0000FFFF
        8DCEF894D1F89AD4F8A1D7F9A7DAF9B3DEFA28A3F11F9FF024A1F129A3F12FA5
        F134A9F239ABF2FFFF00FF0000FF0000FF0000FFFF00FFFF0000FF0000FF0000
        FF00FFFF0080C8F786CCF78DCEF894D1F89AD4F9A1D7F9ACDBF924A1F11A9DF0
        1FA0F024A2F129A4F12FA6F134A8F2FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF0079C6F680C9F786CBF78DCEF893D1F89AD4F8A7
        D9F920A0F0169BF01A9EF00000FF0000FF2AA3F12FA7F1C0C0C00000FF0000FF
        FFFF00FFFF000000FFFFFF00FFFF000000FF0000FF72C3F67AC6F60000FF87CC
        F78DCFF893D1F8A1D7F81C9DF01299EF0000FF1A9EF020A0F00000FF29A4F100
        00FFFFFF00FFFF000000FFFFFF000000FFFFFF000000FFFFFF0066BEF50000FF
        73C3F60000FF80C9F787CCF78DCFF89AD4F8189CF00E98EF0000FF169BF01B9E
        F01F9FF025A1F10000FF2FA6F1FFFF000000FFFFFF000000FFFFFF000000FF59
        B8F45FBBF50000FF6DC1F60000FF79C6F680C9F787CBF794D1F8159BF00A97EF
        0000FF129AEF169CF00000FF1FA0F00000FF2AA4F12FA7F10000FFC0C0C00000
        FF47B0F30000FF53B6F459B8F40000FF66BEF50000FF73C3F67AC6F681C9F78E
        CFF71299EF0695EF0A97EF0000FF0000FF169CF01B9DF01FA0F00000FF0000FF
        2FA6F134A8F20000FF41AEF347B0F30000FF0000FF59B8F45FBBF50000FF0000
        FF0000FF7AC6F688CCF70C97EF0394EE0795EF0A96EF0E98EF1299EF169CF01B
        9DF0209FF025A1F12AA4F12FA7F10000FF3AABF240AEF347B0F34CB3F352B6F4
        59B8F460BBF566BEF56DC1F573C3F680C8F61E9FF01098EF129AEF159BF0189D
        F01C9EF020A1F024A1F128A3F12DA6F132A7F237A9F20000FF41AEF346B0F34C
        B3F452B5F457B7F45DBAF563BCF569BFF66FC2F676C5F683CBF7}
      OnClick = ColorButtonClick
    end
    object VolumeLabel: TLabel
      Left = 4
      Top = 240
      Width = 302
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object PosVolLabel: TLabel
      Left = 4
      Top = 200
      Width = 302
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object NegVolLabel: TLabel
      Left = 4
      Top = 220
      Width = 302
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object TotalLabel: TLabel
      Left = 4
      Top = 260
      Width = 302
      Height = 16
      Alignment = taCenter
      Anchors = [akLeft, akBottom]
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object EditMinX: TEdit
      Left = 28
      Top = 29
      Width = 90
      Height = 21
      Hint = 'Minimum x axis value'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnKeyDown = EditKeyDown
      OnKeyPress = FloatKeyPress
      OnKeyUp = EditMinXKeyUp
    end
    object EditMaxX: TEdit
      Left = 120
      Top = 29
      Width = 90
      Height = 21
      Hint = 'Maximum x axis value'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnKeyDown = EditKeyDown
      OnKeyPress = FloatKeyPress
      OnKeyUp = EditMaxXKeyUp
    end
    object EditdX: TEdit
      Left = 212
      Top = 29
      Width = 90
      Height = 21
      Hint = 'Increment x '
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnKeyDown = EditKeyDown
      OnKeyPress = IncKeyPress
      OnKeyUp = EditdXKeyUp
    end
    object EditMinY: TEdit
      Left = 28
      Top = 51
      Width = 90
      Height = 21
      Hint = 'Minimum y axis value'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnKeyDown = EditKeyDown
      OnKeyPress = FloatKeyPress
      OnKeyUp = EditMinYKeyUp
    end
    object EditMaxY: TEdit
      Left = 120
      Top = 51
      Width = 90
      Height = 21
      Hint = 'Maximum y axis value'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnKeyDown = EditKeyDown
      OnKeyPress = FloatKeyPress
      OnKeyUp = EditMaxYKeyUp
    end
    object EditdY: TEdit
      Left = 212
      Top = 51
      Width = 90
      Height = 21
      Hint = 'Increment y'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnKeyDown = EditKeyDown
      OnKeyPress = IncKeyPress
      OnKeyUp = EditdYKeyUp
    end
    object EditMinZ: TEdit
      Left = 28
      Top = 73
      Width = 90
      Height = 21
      Hint = 'Minimum z axis value'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnKeyDown = EditKeyDown
      OnKeyPress = FloatKeyPress
      OnKeyUp = EditMinZKeyUp
    end
    object EditMaxZ: TEdit
      Left = 120
      Top = 73
      Width = 90
      Height = 21
      Hint = 'Maximum z axis value'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      OnKeyDown = EditKeyDown
      OnKeyPress = FloatKeyPress
      OnKeyUp = EditMaxZKeyUp
    end
    object zLimitCB: TCheckBox
      Left = 21
      Top = 102
      Width = 97
      Height = 17
      Cursor = crHandPoint
      Hint = 'Plot z values between minZ <= z <= maxZ  '
      Caption = 'Apply z Limit'
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 8
      OnClick = zLimitCBClick
    end
    object ModeComboBox: TComboBox
      Left = 54
      Top = 125
      Width = 121
      Height = 21
      Cursor = crHandPoint
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 10
      Text = 'Ambient'
      OnChange = ModeComboBoxChange
      Items.Strings = (
        'Ambient'
        'Ambient and Diffuse'
        'Diffuse'
        'Emission'
        'None')
    end
    object StyleComboBox: TComboBox
      Left = 181
      Top = 125
      Width = 121
      Height = 21
      Cursor = crHandPoint
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 11
      Text = 'Fill'
      OnChange = StyleComboBoxChange
      Items.Strings = (
        'Fill'
        'Lines'
        'Points')
    end
    object zCapCB: TCheckBox
      Left = 120
      Top = 102
      Width = 90
      Height = 17
      Cursor = crHandPoint
      Hint = 'Cap z value to minZ <= z <= maxZ  '
      Caption = 'Apply z cap'
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 9
      OnClick = zCapCBClick
    end
    object ApplyBtn: TBitBtn
      Left = 35
      Top = 320
      Width = 90
      Height = 27
      Cursor = crHandPoint
      Hint = 'Update any changes made'
      Caption = 'Apply'
      Default = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FF936035936035936035936035936035936035FF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF936035936035C6923DE3B445F7
        CE4AF7CF4BE3BB47C79940936035936035FF00FFFF00FFFF00FFFF00FFFF00FF
        936035B57A36DD9838F0AD38F3B538F5BC3BF6BE3CF5BB3BF3B63BE1A33DB87F
        38936035FF00FFFF00FFFF00FF936035B27233DC872FE4922DDCA457D7BC8FD7
        C097D8C096D8C199D5B582E69E38E09233B47634936035FF00FFFF00FF936035
        CD6F28DA7B25D2A374F9FDFEF9FDFEF9FDFEF9FDFEF9FDFEF9FDFEDF8E32E08A
        2FD0782C936035FF00FF936035B4652BD3661FD27A37F9FDFED5B69ADA8229DE
        892DDF8A2FD9852DDC8227DD832DDA7B2AD67226B5682C936035936035C05A21
        CE5A17D07D47F9FDFED18C57DA711CDB7B29DC7822F9FDFED2905BD76D1CD56E
        25D26722C25E24936035936035C64C18CA5015CB6A36F9FDFED8AD91CD5F16D0
        641BCF5E0FF9FDFEF9FDFECD7F4CCF5A15CD5B1EC7521B936035936035C24212
        C54613C5480FD5A38CF5FCFFF9FDFEF9FDFEF9FDFEF9FDFEF9FDFEF9FDFECB70
        43C84B12C34917936035936035C15F39C9552DC95124C54918D08464E4BDAEE7
        C8BCE6C5B8F9FDFEF9FDFEF9FDFEC7663FC33E0CB84618936035936035B5704B
        D3765CD4785BD47555D06842CD6239CC5E33C74D1DF9FDFEF7F5F4CA7252C23D
        0FC24119AD5428936035FF00FF936035CD7C64D6816AD7836AD8846BD8856BD8
        846AD77F63F9FDFED59B89D27154D4775CC86F55936035FF00FFFF00FF936035
        B17855D88B7ADA8E7BDA8E7BDA8E7ADA8D79DB8C78D58974D78570D98974D483
        70B07450936035FF00FFFF00FFFF00FF936035B37F5DD79886E09E90E09E8FDF
        9D8EDF9C8CDF9A8ADE9888D38F7DB27B59936035FF00FFFF00FFFF00FFFF00FF
        FF00FF936035936035BE8E71D6A28FE4AEA1E4ADA0D5A08CBE8B6E9360359360
        35FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF93603593603593
        6035936035936035936035FF00FFFF00FFFF00FFFF00FFFF00FF}
      TabOrder = 16
      Visible = False
      OnClick = ApplyBtnClick
    end
    object CloseBtn: TBitBtn
      Left = 181
      Top = 320
      Width = 100
      Height = 27
      Caption = '&Close'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        040000000000800000000000000000000000100000001000000000000000FFFF
        000000008400FF00FF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00330000000000
        0333330244444444033333022444444403333302224444440333330222044444
        0333330222044444033333022204444403333302220444440333330222044444
        0333330222044444033333022240444403333302220444440333330222044444
        0333330212044444033333021104444403333300000000000333}
      TabOrder = 17
      OnClick = CloseBtnClick
    end
    object DerivXRB: TRadioButton
      Left = 13
      Top = 170
      Width = 59
      Height = 25
      Cursor = crHandPoint
      Caption = 'dz/dx'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Sylfaen'
      Font.Style = []
      ParentFont = False
      TabOrder = 12
      OnClick = DerivXRBClick
    end
    object DerivYRB: TRadioButton
      Left = 104
      Top = 170
      Width = 60
      Height = 25
      Cursor = crHandPoint
      Caption = 'dz/dy'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Sylfaen'
      Font.Style = []
      ParentFont = False
      TabOrder = 13
      OnClick = DerivYRBClick
    end
    object VolumeRB: TRadioButton
      Left = 191
      Top = 170
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Caption = #8747#8747'f(x,y)dydx'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Sylfaen'
      Font.Style = []
      ParentFont = False
      TabOrder = 14
      OnClick = VolumeRBClick
    end
    object EditAddLineWidth: TEdit
      Tag = 1
      Left = 69
      Top = 284
      Width = 56
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 15
      OnKeyPress = EditAddLineWidthKeyPress
      OnKeyUp = EditAddLineWidthKeyUp
    end
  end
  object ColorDialog: TColorDialog
    Options = [cdFullOpen]
    Left = 264
    Top = 307
  end
end
