object GridOptionsForm: TGridOptionsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 351
  ClientWidth = 320
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    320
    351)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxXY: TGroupBox
    Left = 4
    Top = 2
    Width = 312
    Height = 81
    Anchors = [akTop, akRight]
    Caption = '  X, Y Grid Range  '
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object Label7: TLabel
      Left = 41
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
    object Label8: TLabel
      Left = 106
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
    object Label9: TLabel
      Left = 172
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
    object Label11: TLabel
      Left = -2
      Top = 31
      Width = 20
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'x :'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label12: TLabel
      Left = -2
      Top = 53
      Width = 20
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'y :'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label10: TLabel
      Left = 242
      Top = 15
      Width = 59
      Height = 13
      Caption = 'Position ( z )'
    end
    object EditxyGridMinx: TEdit
      Left = 21
      Top = 29
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = EditxyGridMinxChange
      OnKeyPress = FloatKeyPress
    end
    object EditxyGridMaxx: TEdit
      Left = 92
      Top = 29
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnChange = EditxyGridMaxxChange
      OnKeyPress = FloatKeyPress
    end
    object EditxyGridStpx: TEdit
      Left = 164
      Top = 29
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnChange = EditxyGridStpxChange
      OnKeyPress = PositiveKeyPress
    end
    object EditxyGridMiny: TEdit
      Left = 21
      Top = 51
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnChange = EditxyGridMinyChange
      OnKeyPress = FloatKeyPress
    end
    object EditxyGridMaxy: TEdit
      Left = 92
      Top = 51
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnChange = EditxyGridMaxyChange
      OnKeyPress = FloatKeyPress
    end
    object EditxyGridStpy: TEdit
      Left = 164
      Top = 51
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnChange = EditxyGridStpyChange
      OnKeyPress = PositiveKeyPress
    end
    object EditxyGridPosz: TEdit
      Left = 236
      Top = 29
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnChange = EditxyGridPoszChange
      OnKeyPress = FloatKeyPress
    end
    object xyLock: TCheckBox
      Left = 241
      Top = 53
      Width = 66
      Height = 17
      Cursor = crHandPoint
      Caption = 'Lock x,y'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      State = cbChecked
      TabOrder = 7
      OnClick = xyLockClick
    end
  end
  object GroupBoxXZ: TGroupBox
    Left = 4
    Top = 84
    Width = 312
    Height = 81
    Anchors = [akTop, akRight]
    Caption = '  X, Z Grid Range  '
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object Label13: TLabel
      Left = 41
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
    object Label18: TLabel
      Left = 106
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
    object Label20: TLabel
      Left = 172
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
    object Label21: TLabel
      Left = -2
      Top = 31
      Width = 20
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'x :'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label22: TLabel
      Left = -2
      Top = 53
      Width = 20
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'z :'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label23: TLabel
      Left = 242
      Top = 15
      Width = 60
      Height = 13
      Caption = 'Position ( y )'
    end
    object EditxzGridMinx: TEdit
      Left = 21
      Top = 29
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = EditxzGridMinxChange
      OnKeyPress = FloatKeyPress
    end
    object EditxzGridMaxx: TEdit
      Left = 92
      Top = 29
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnChange = EditxzGridMaxxChange
      OnKeyPress = FloatKeyPress
    end
    object EditxzGridStpx: TEdit
      Left = 164
      Top = 29
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnChange = EditxzGridStpxChange
      OnExit = EditxzGridStpxExit
      OnKeyPress = PositiveKeyPress
    end
    object EditxzGridMinz: TEdit
      Left = 21
      Top = 51
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnChange = EditxzGridMinzChange
      OnKeyPress = FloatKeyPress
    end
    object EditxzGridMaxz: TEdit
      Left = 92
      Top = 51
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnChange = EditxzGridMaxzChange
      OnKeyPress = FloatKeyPress
    end
    object EditxzGridStpz: TEdit
      Left = 164
      Top = 51
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnChange = EditxzGridStpzChange
      OnKeyPress = PositiveKeyPress
    end
    object EditxzGridPosy: TEdit
      Left = 236
      Top = 29
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnChange = EditxzGridPosyChange
      OnKeyPress = FloatKeyPress
    end
    object zLock: TCheckBox
      Left = 241
      Top = 56
      Width = 66
      Height = 17
      Cursor = crHandPoint
      Caption = 'Lock z'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      State = cbChecked
      TabOrder = 7
      OnClick = zLockClick
    end
  end
  object GroupBoxYZ: TGroupBox
    Left = 4
    Top = 166
    Width = 312
    Height = 81
    Anchors = [akTop, akRight]
    Caption = '  Y, Z Grid Range  '
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    object Label24: TLabel
      Left = 41
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
    object Label25: TLabel
      Left = 106
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
    object Label26: TLabel
      Left = 172
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
    object Label27: TLabel
      Left = -1
      Top = 32
      Width = 20
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'y :'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label28: TLabel
      Left = -2
      Top = 53
      Width = 20
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'z :'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label29: TLabel
      Left = 242
      Top = 15
      Width = 60
      Height = 13
      Caption = 'Position ( x )'
    end
    object EdityzGridMiny: TEdit
      Left = 21
      Top = 29
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = EdityzGridMinyChange
      OnKeyPress = FloatKeyPress
    end
    object EdityzGridMaxy: TEdit
      Left = 92
      Top = 29
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnChange = EdityzGridMaxyChange
      OnKeyPress = FloatKeyPress
    end
    object EdityzGridStpy: TEdit
      Left = 164
      Top = 29
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnChange = EdityzGridStpyChange
      OnExit = EdityzGridStpyExit
      OnKeyPress = PositiveKeyPress
    end
    object EdityzGridMinz: TEdit
      Left = 21
      Top = 51
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnChange = EdityzGridMinzChange
      OnKeyPress = FloatKeyPress
    end
    object EdityzGridMaxz: TEdit
      Left = 92
      Top = 51
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnChange = EdityzGridMaxzChange
      OnKeyPress = FloatKeyPress
    end
    object EdityzGridStpz: TEdit
      Left = 164
      Top = 51
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnChange = EdityzGridStpzChange
      OnExit = EdityzGridStpzExit
      OnKeyPress = PositiveKeyPress
    end
    object EdityzGridPosx: TEdit
      Left = 236
      Top = 29
      Width = 73
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnChange = EdityzGridPosxChange
      OnKeyPress = FloatKeyPress
    end
    object MinLock: TCheckBox
      Left = 241
      Top = 53
      Width = 66
      Height = 17
      Cursor = crHandPoint
      Caption = 'Minimum'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      State = cbChecked
      TabOrder = 7
      OnClick = MinLockClick
    end
  end
  object GroupBoxOp: TGroupBox
    Left = 4
    Top = 246
    Width = 312
    Height = 99
    Anchors = [akTop, akRight]
    Caption = '  Options  '
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    object Label14: TLabel
      Left = 4
      Top = 15
      Width = 36
      Height = 13
      AutoSize = False
      Caption = 'Show :'
    end
    object Label19: TLabel
      Left = 191
      Top = 40
      Width = 61
      Height = 13
      Caption = 'View Depth :'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Colors: TSpeedButton
      Left = 172
      Top = 64
      Width = 54
      Height = 21
      Cursor = crHandPoint
      Caption = '&Colours'
      Flat = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      NumGlyphs = 2
      ParentFont = False
      OnClick = ColorsClick
    end
    object Label1: TLabel
      Left = 4
      Top = 40
      Width = 40
      Height = 13
      Caption = 'Scale z :'
    end
    object Centre: TSpeedButton
      Left = 89
      Top = 64
      Width = 78
      Height = 21
      Cursor = crHandPoint
      Caption = 'Grids Centre '
      Flat = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      NumGlyphs = 2
      ParentFont = False
      OnClick = CentreClick
    end
    object Label4: TLabel
      Left = 97
      Top = 40
      Width = 47
      Height = 13
      Caption = 'Box Line :'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object PlotValues: TSpeedButton
      Left = 7
      Top = 64
      Width = 74
      Height = 21
      Cursor = crHandPoint
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
    end
    object xyGridCB: TCheckBox
      Left = 41
      Top = 14
      Width = 55
      Height = 17
      Cursor = crHandPoint
      Caption = 'xy Grid'
      TabOrder = 0
      OnClick = xyGridCBClick
    end
    object xzGridCB: TCheckBox
      Left = 104
      Top = 14
      Width = 55
      Height = 17
      Cursor = crHandPoint
      Caption = 'xz Grid'
      TabOrder = 1
      OnClick = xzGridCBClick
    end
    object yzGridCB: TCheckBox
      Left = 167
      Top = 14
      Width = 55
      Height = 17
      Cursor = crHandPoint
      Caption = 'yz Grid'
      TabOrder = 2
      OnClick = yzGridCBClick
    end
    object EditViewDepth: TEdit
      Left = 254
      Top = 37
      Width = 52
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      Text = '10000'
      OnChange = EditViewDepthChange
      OnKeyPress = PositiveKeyPress
    end
    object EditzScale: TEdit
      Left = 47
      Top = 37
      Width = 45
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnKeyPress = PositiveKeyPress
      OnKeyUp = EditzScaleKeyUp
    end
    object BoxOutlineCB: TCheckBox
      Left = 230
      Top = 12
      Width = 76
      Height = 17
      Cursor = crHandPoint
      Caption = 'Box Outline'
      TabOrder = 3
      OnClick = BoxOutlineCBClick
    end
    object EditBoxLnWidth: TEdit
      Tag = 1
      Left = 145
      Top = 37
      Width = 43
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnKeyPress = IntKeyPress
      OnKeyUp = EditBoxLnWidthKeyUp
    end
    object BitBtn1: TBitBtn
      Left = 230
      Top = 61
      Width = 75
      Height = 29
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
      TabOrder = 7
      OnClick = BitBtn1Click
    end
  end
end
