object MainForm: TMainForm
  Left = 561
  Height = 597
  Top = 235
  Width = 1065
  Caption = 'MainForm'
  ClientHeight = 597
  ClientWidth = 1065
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '1.6.4.0'
  Visible = True
  object Panel1: TPanel
    Left = 0
    Height = 597
    Top = 0
    Width = 432
    Align = alLeft
    ClientHeight = 597
    ClientWidth = 432
    TabOrder = 0
    object Label54: TLabel
      Left = 148
      Height = 13
      Top = 502
      Width = 32
      Caption = 'Object'
      ParentColor = False
    end
    object PageControl1: TPageControl
      Left = 1
      Height = 496
      Top = 1
      Width = 430
      ActivePage = TabSheet1
      Align = alTop
      MultiLine = True
      ParentShowHint = False
      ShowHint = True
      TabIndex = 0
      TabOrder = 0
      Options = [nboMultiLine]
      object TabSheet1: TTabSheet
        Caption = 'Fur'
        ClientHeight = 470
        ClientWidth = 422
        object lblFurDistance: TLabel
          Left = 3
          Height = 13
          Top = 53
          Width = 55
          Caption = 'Fur Length '
          ParentColor = False
        end
        object Label10: TLabel
          Left = 3
          Height = 13
          Top = 205
          Width = 51
          Caption = 'Blend Dest'
          ParentColor = False
        end
        object Label11: TLabel
          Left = 3
          Height = 13
          Top = 179
          Width = 44
          Caption = 'Blend Src'
          ParentColor = False
        end
        object lblFurPassCount1: TLabel
          Left = 3
          Height = 13
          Top = 28
          Width = 57
          Caption = 'Pass Count '
          ParentColor = False
        end
        object lblFurLength: TLabel
          Left = 287
          Height = 13
          Top = 53
          Width = 16
          Caption = '0.3'
          ParentColor = False
        end
        object Label7: TLabel
          Left = 3
          Height = 13
          Top = 75
          Width = 59
          Caption = 'Max Length '
          ParentColor = False
        end
        object lblFurMaxLength: TLabel
          Left = 287
          Height = 13
          Top = 76
          Width = 16
          Caption = '3.0'
          ParentColor = False
        end
        object lblFurPassCount: TLabel
          Left = 287
          Height = 13
          Top = 28
          Width = 12
          Caption = '16'
          ParentColor = False
        end
        object Label12: TLabel
          Left = 3
          Height = 13
          Top = 97
          Width = 36
          Caption = 'Density'
          ParentColor = False
        end
        object lblFurDensity: TLabel
          Left = 287
          Height = 13
          Top = 97
          Width = 16
          Caption = '1.0'
          ParentColor = False
        end
        object Label6: TLabel
          Left = 3
          Height = 13
          Top = 233
          Width = 56
          Caption = 'Light Power'
          ParentColor = False
        end
        object lblFurLightPower: TLabel
          Left = 287
          Height = 13
          Top = 233
          Width = 16
          Caption = '2.5'
          ParentColor = False
        end
        object Label8: TLabel
          Left = 3
          Height = 13
          Top = 263
          Width = 56
          Caption = 'Color Scale '
          ParentColor = False
        end
        object Label9: TLabel
          Left = 3
          Height = 13
          Top = 285
          Width = 67
          Caption = 'Ambient Color'
          ParentColor = False
        end
        object Shape1: TShape
          Left = 83
          Height = 15
          Top = 262
          Width = 64
          OnMouseDown = Shape1MouseDown
        end
        object Shape2: TShape
          Left = 83
          Height = 15
          Top = 283
          Width = 64
          OnMouseDown = Shape2MouseDown
        end
        object Label63: TLabel
          Left = 2
          Height = 13
          Top = 129
          Width = 59
          Caption = 'Gravity XYZ '
          ParentColor = False
        end
        object chkFurShader: TCheckBox
          Left = 3
          Height = 19
          Top = 3
          Width = 58
          Caption = 'Enabled'
          OnClick = chkFurShaderClick
          TabOrder = 0
        end
        object tbFurLength: TTrackBar
          Left = 65
          Height = 26
          Top = 49
          Width = 216
          Frequency = 10
          Max = 100
          Min = 1
          OnChange = tbFurLengthChange
          PageSize = 10
          Position = 30
          TickStyle = tsNone
          TabOrder = 1
        end
        object cbxFurBlendSrc: TComboBox
          Left = 72
          Height = 21
          Top = 176
          Width = 145
          ItemHeight = 13
          ItemIndex = 3
          Items.Strings = (
            'ZERO'
            'ONE'
            'SRC COLOR'
            'ONE MINUS SRC COLOR'
            'DST COLOR'
            'ONE MINUS DST COLOR'
            'SRC ALPHA'
            'MINUS SRC ALPHA'
            'DST ALPHA'
            'MINUS DST ALPHA'
            'SRC ALPHA SATURATE'
            'CONSTANT COLOR'
            'ONE MINUS CONSTANT COLOR'
            'CONSTANT ALPHA'
            'ONE MINUS CONSTATNT ALPHA'
          )
          OnChange = cbxFurBlendSrcChange
          Style = csDropDownList
          TabOrder = 2
          Text = 'ONE MINUS SRC COLOR'
        end
        object cbxFurBlendDest: TComboBox
          Left = 72
          Height = 21
          Top = 203
          Width = 145
          ItemHeight = 13
          ItemIndex = 7
          Items.Strings = (
            'ZERO'
            'ONE'
            'SRC COLOR'
            'ONE MINUS SRC COLOR'
            'DST COLOR'
            'ONE MINUS DST COLOR'
            'SRC ALPHA'
            'MINUS SRC ALPHA'
            'DST ALPHA'
            'MINUS DST ALPHA'
            'SRC ALPHA SATURATE'
            'CONSTANT COLOR'
            'ONE MINUS CONSTANT COLOR'
            'CONSTANT ALPHA'
            'ONE MINUS CONSTATNT ALPHA'
          )
          OnChange = cbxFurBlendDestChange
          Style = csDropDownList
          TabOrder = 3
          Text = 'MINUS SRC ALPHA'
        end
        object chkAnimateFur: TCheckBox
          Left = 72
          Height = 19
          Top = 3
          Width = 78
          Caption = 'Animate Fur'
          TabOrder = 4
        end
        object tbFurPassCount: TTrackBar
          Left = 65
          Height = 23
          Top = 24
          Width = 216
          Frequency = 8
          Max = 200
          Min = 1
          OnChange = tbFurPassCountChange
          PageSize = 10
          Position = 16
          TickStyle = tsNone
          TabOrder = 5
        end
        object tbFurMaxLength: TTrackBar
          Left = 65
          Height = 26
          Top = 72
          Width = 216
          Frequency = 10
          Max = 500
          Min = 1
          OnChange = tbFurMaxLengthChange
          PageSize = 10
          Position = 300
          TickStyle = tsNone
          TabOrder = 6
        end
        object chkFurRandomLength: TCheckBox
          Left = 72
          Height = 19
          Top = 154
          Width = 114
          Caption = 'Random Fur Length'
          OnClick = chkFurRandomLengthClick
          TabOrder = 7
        end
        object tbFurDensity: TTrackBar
          Left = 65
          Height = 26
          Top = 94
          Width = 216
          Frequency = 10
          Max = 200
          Min = 1
          OnChange = tbFurDensityChange
          PageSize = 10
          Position = 100
          TickStyle = tsNone
          TabOrder = 8
        end
        object tbFurLightPower: TTrackBar
          Left = 65
          Height = 26
          Top = 230
          Width = 216
          Frequency = 10
          Max = 1000
          Min = 1
          OnChange = tbFurLightPowerChange
          PageSize = 10
          Position = 250
          TickStyle = tsNone
          TabOrder = 9
        end
        object Button8: TButton
          Left = 3
          Height = 25
          Top = 315
          Width = 184
          Caption = 'Load Main Texture'
          OnClick = Button8Click
          TabOrder = 10
        end
        object Button9: TButton
          Left = 3
          Height = 25
          Top = 346
          Width = 184
          Caption = 'Load Noise Texture'
          OnClick = Button9Click
          TabOrder = 11
        end
        object edtFurGravityX: TEdit
          Left = 71
          Height = 21
          Top = 124
          Width = 57
          OnChange = edtFurGravityXChange
          OnKeyPress = EditFloatKeyPress
          TabOrder = 12
          Text = '0.0'
        end
        object edtFurGravityY: TEdit
          Left = 141
          Height = 21
          Top = 124
          Width = 57
          OnChange = edtFurGravityYChange
          OnKeyPress = EditFloatKeyPress
          TabOrder = 13
          Text = '-2.0'
        end
        object edtFurGravityZ: TEdit
          Left = 216
          Height = 21
          Top = 124
          Width = 57
          OnChange = edtFurGravityZChange
          OnKeyPress = EditFloatKeyPress
          TabOrder = 14
          Text = '0.0'
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Lattice'
        ClientHeight = 470
        ClientWidth = 343
        ImageIndex = 1
        object lblLatticeScaleX: TLabel
          Left = 291
          Height = 13
          Top = 41
          Width = 12
          Caption = '10'
          ParentColor = False
        end
        object lblLatticeThresholdX: TLabel
          Left = 289
          Height = 13
          Top = 93
          Width = 22
          Caption = '0.15'
          ParentColor = False
        end
        object lblLatticeScaleY: TLabel
          Left = 290
          Height = 13
          Top = 67
          Width = 12
          Caption = '40'
          ParentColor = False
        end
        object Label4: TLabel
          Left = 16
          Height = 13
          Top = 147
          Width = 74
          Caption = 'Specular Power'
          ParentColor = False
        end
        object Label5: TLabel
          Left = 16
          Height = 13
          Top = 172
          Width = 56
          Caption = 'Light Power'
          ParentColor = False
        end
        object lblLatticeThresholdY: TLabel
          Left = 290
          Height = 13
          Top = 118
          Width = 22
          Caption = '0.30'
          ParentColor = False
        end
        object lblLatticeSpecularPower: TLabel
          Left = 290
          Height = 13
          Top = 147
          Width = 16
          Caption = '8.0'
          ParentColor = False
        end
        object lblLatticeLightPower: TLabel
          Left = 290
          Height = 13
          Top = 173
          Width = 16
          Caption = '1.0'
          ParentColor = False
        end
        object Label23: TLabel
          Left = 10
          Height = 13
          Top = 41
          Width = 34
          Caption = 'Scale X'
          ParentColor = False
        end
        object Label29: TLabel
          Left = 10
          Height = 13
          Top = 67
          Width = 34
          Caption = 'Scale Y'
          ParentColor = False
        end
        object Label31: TLabel
          Left = 10
          Height = 13
          Top = 93
          Width = 56
          Caption = 'Threshold X'
          ParentColor = False
        end
        object Label33: TLabel
          Left = 10
          Height = 13
          Top = 122
          Width = 56
          Caption = 'Threshold Y'
          ParentColor = False
        end
        object Label35: TLabel
          Left = 10
          Height = 13
          Top = 200
          Width = 62
          Caption = 'Diffuse Color'
          ParentColor = False
        end
        object Shape10: TShape
          Left = 90
          Height = 15
          Top = 198
          Width = 64
          OnMouseDown = Shape10MouseDown
        end
        object Label38: TLabel
          Left = 10
          Height = 13
          Top = 223
          Width = 67
          Caption = 'Ambient Color'
          ParentColor = False
        end
        object Shape11: TShape
          Left = 90
          Height = 15
          Top = 222
          Width = 64
          Brush.Color = 1381653
          OnMouseDown = Shape11MouseDown
        end
        object Label39: TLabel
          Left = 10
          Height = 13
          Top = 244
          Width = 69
          Caption = 'Specular Color'
          ParentColor = False
        end
        object Shape12: TShape
          Left = 90
          Height = 15
          Top = 244
          Width = 64
          OnMouseDown = Shape12MouseDown
        end
        object tbLatticeScaleX: TTrackBar
          Left = 73
          Height = 30
          Top = 37
          Width = 215
          Frequency = 10
          Max = 100
          Min = 1
          OnChange = tbLatticeScaleXChange
          PageSize = 10
          Position = 10
          TickStyle = tsNone
          TabOrder = 0
        end
        object tbLatticeThresholdX: TTrackBar
          Left = 73
          Height = 27
          Top = 89
          Width = 215
          Frequency = 10
          Max = 100
          Min = 1
          OnChange = tbLatticeThresholdXChange
          PageSize = 10
          Position = 15
          TickStyle = tsNone
          TabOrder = 1
        end
        object chkLatticeShader: TCheckBox
          Left = 16
          Height = 19
          Top = 12
          Width = 58
          Caption = 'Enabled'
          OnClick = chkLatticeShaderClick
          TabOrder = 2
        end
        object tbLatticeScaleY: TTrackBar
          Left = 73
          Height = 32
          Top = 63
          Width = 215
          Frequency = 10
          Max = 100
          Min = 1
          OnChange = tbLatticeScaleYChange
          PageSize = 10
          Position = 40
          TickStyle = tsNone
          TabOrder = 3
        end
        object tbLatticeThresholdY: TTrackBar
          Left = 73
          Height = 27
          Top = 114
          Width = 211
          Frequency = 10
          Max = 100
          Min = 1
          OnChange = tbLatticeThresholdYChange
          PageSize = 10
          Position = 30
          TickStyle = tsNone
          TabOrder = 4
        end
        object tbLatticeSpecularPower: TTrackBar
          Left = 96
          Height = 32
          Top = 143
          Width = 188
          Frequency = 10
          Max = 1000
          OnChange = tbLatticeSpecularPowerChange
          PageSize = 10
          Position = 800
          TickStyle = tsNone
          TabOrder = 5
        end
        object tbLatticeLightPower: TTrackBar
          Left = 96
          Height = 32
          Top = 166
          Width = 188
          Frequency = 10
          Max = 500
          OnChange = tbLatticeLightPowerChange
          PageSize = 10
          Position = 100
          TickStyle = tsNone
          TabOrder = 6
        end
        object Button7: TButton
          Left = 16
          Height = 25
          Top = 275
          Width = 184
          Caption = 'Load Main Texture'
          OnClick = Button3Click
          TabOrder = 7
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Erosion'
        ClientHeight = 470
        ClientWidth = 343
        ImageIndex = 2
        object Label1: TLabel
          Left = 8
          Height = 13
          Top = 46
          Width = 67
          Caption = 'Erosion factor'
          ParentColor = False
        end
        object lblErosionFactor: TLabel
          Left = 295
          Height = 13
          Top = 47
          Width = 22
          Caption = '0.35'
          ParentColor = False
        end
        object Label3: TLabel
          Left = 8
          Height = 13
          Top = 79
          Width = 63
          Caption = 'Erosion Scale'
          ParentColor = False
        end
        object lblErosionScale: TLabel
          Left = 295
          Height = 13
          Top = 79
          Width = 22
          Caption = '0.03'
          ParentColor = False
        end
        object Label25: TLabel
          Left = 8
          Height = 13
          Top = 111
          Width = 84
          Caption = 'Intensity factor 1'
          ParentColor = False
        end
        object lblErosionIFactor1: TLabel
          Left = 295
          Height = 13
          Top = 111
          Width = 22
          Caption = '0.75'
          ParentColor = False
        end
        object Label28: TLabel
          Left = 8
          Height = 13
          Top = 142
          Width = 86
          Caption = 'Intensity Factor 2'
          ParentColor = False
        end
        object lblerosionIFactor2: TLabel
          Left = 295
          Height = 13
          Top = 143
          Width = 22
          Caption = '1.95'
          ParentColor = False
        end
        object Label2: TLabel
          Left = 8
          Height = 13
          Top = 200
          Width = 71
          Caption = 'Ambient factor'
          ParentColor = False
        end
        object lblErosionAmbientF: TLabel
          Left = 295
          Height = 13
          Top = 200
          Width = 22
          Caption = '0.80'
          ParentColor = False
        end
        object Label27: TLabel
          Left = 8
          Height = 13
          Top = 172
          Width = 66
          Caption = 'Diffuse factor'
          ParentColor = False
        end
        object lblErosionDiffuseF: TLabel
          Left = 295
          Height = 13
          Top = 173
          Width = 22
          Caption = '0.90'
          ParentColor = False
        end
        object Label30: TLabel
          Left = 8
          Height = 13
          Top = 228
          Width = 73
          Caption = 'Specular factor'
          ParentColor = False
        end
        object lblErosionSpecularF: TLabel
          Left = 295
          Height = 13
          Top = 228
          Width = 22
          Caption = '0.90'
          ParentColor = False
        end
        object Label32: TLabel
          Left = 8
          Height = 13
          Top = 260
          Width = 97
          Caption = 'Specular Roughness'
          ParentColor = False
        end
        object lblErosionSpecularR: TLabel
          Left = 297
          Height = 13
          Top = 260
          Width = 22
          Caption = '0.45'
          ParentColor = False
        end
        object Label34: TLabel
          Left = 8
          Height = 13
          Top = 292
          Width = 109
          Caption = 'Anisotropic Roughness'
          ParentColor = False
        end
        object lblErosionAnisoR: TLabel
          Left = 297
          Height = 13
          Top = 292
          Width = 22
          Caption = '0.35'
          ParentColor = False
        end
        object Label36: TLabel
          Left = 6
          Height = 13
          Top = 327
          Width = 67
          Caption = 'Ambient Color'
          ParentColor = False
        end
        object Shape8: TShape
          Left = 86
          Height = 15
          Top = 325
          Width = 64
          Brush.Color = 2105376
          OnMouseDown = Shape8MouseDown
        end
        object Shape9: TShape
          Left = 86
          Height = 15
          Top = 346
          Width = 64
          Brush.Color = 14540253
          OnMouseDown = Shape9MouseDown
        end
        object Label37: TLabel
          Left = 6
          Height = 13
          Top = 346
          Width = 69
          Caption = 'Specular Color'
          ParentColor = False
        end
        object chkErosionShader: TCheckBox
          Left = 24
          Height = 19
          Top = 20
          Width = 58
          Caption = 'Enabled'
          OnClick = chkErosionShaderClick
          TabOrder = 0
        end
        object tbErosionFactor: TTrackBar
          Left = 98
          Height = 26
          Top = 43
          Width = 191
          Frequency = 10
          Max = 100
          Min = 1
          OnChange = tbErosionFactorChange
          PageSize = 10
          Position = 35
          TickStyle = tsNone
          TabOrder = 1
        end
        object tberosionScale: TTrackBar
          Left = 98
          Height = 26
          Top = 75
          Width = 191
          Frequency = 10
          Max = 100
          Min = 1
          OnChange = tberosionScaleChange
          PageSize = 10
          Position = 3
          TickStyle = tsNone
          TabOrder = 2
        end
        object tbErosionIFactor1: TTrackBar
          Left = 98
          Height = 26
          Top = 107
          Width = 191
          Frequency = 10
          Max = 200
          Min = 1
          OnChange = tbErosionIFactor1Change
          PageSize = 10
          Position = 75
          TickStyle = tsNone
          TabOrder = 3
        end
        object tbErosionIFactor2: TTrackBar
          Left = 100
          Height = 26
          Top = 139
          Width = 189
          Frequency = 10
          Max = 200
          Min = 1
          OnChange = tbErosionIFactor2Change
          PageSize = 10
          Position = 195
          TickStyle = tsNone
          TabOrder = 4
        end
        object tbErosionAmbientF: TTrackBar
          Left = 98
          Height = 28
          Top = 194
          Width = 191
          Frequency = 10
          Max = 100
          OnChange = tbErosionAmbientFChange
          PageSize = 10
          Position = 80
          TickStyle = tsNone
          TabOrder = 5
        end
        object tbErosionDiffuseF: TTrackBar
          Left = 99
          Height = 26
          Top = 169
          Width = 190
          Frequency = 10
          Max = 100
          OnChange = tbErosionDiffuseFChange
          PageSize = 10
          Position = 90
          TickStyle = tsNone
          TabOrder = 6
        end
        object tbErosionSpecularF: TTrackBar
          Left = 98
          Height = 26
          Top = 224
          Width = 191
          Frequency = 10
          Max = 100
          OnChange = tbErosionSpecularFChange
          PageSize = 10
          Position = 90
          TickStyle = tsNone
          TabOrder = 7
        end
        object tbErosionSpecularR: TTrackBar
          Left = 111
          Height = 26
          Top = 256
          Width = 180
          Frequency = 10
          Max = 500
          OnChange = tbErosionSpecularRChange
          PageSize = 10
          Position = 45
          TickStyle = tsNone
          TabOrder = 8
        end
        object tbErosionAnisoR: TTrackBar
          Left = 123
          Height = 26
          Top = 288
          Width = 166
          Frequency = 10
          Max = 500
          OnChange = tbErosionAnisoRChange
          PageSize = 10
          Position = 35
          TickStyle = tsNone
          TabOrder = 9
        end
        object Button5: TButton
          Left = 3
          Height = 25
          Top = 367
          Width = 184
          Caption = 'Load Main Texture'
          OnClick = Button5Click
          TabOrder = 10
        end
        object Button6: TButton
          Left = 3
          Height = 25
          Top = 398
          Width = 184
          Caption = 'Load Erosion Texture'
          OnClick = Button6Click
          TabOrder = 11
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'Ivory'
        ClientHeight = 470
        ClientWidth = 343
        ImageIndex = 3
        object chkIvoryShader: TCheckBox
          Left = 16
          Height = 19
          Top = 16
          Width = 58
          Caption = 'Enabled'
          OnClick = chkIvoryShaderClick
          TabOrder = 0
        end
      end
      object TabSheet5: TTabSheet
        Caption = 'Gootch'
        ClientHeight = 470
        ClientWidth = 422
        ImageIndex = 4
        object Label13: TLabel
          Left = 16
          Height = 13
          Top = 47
          Width = 62
          Caption = 'Diffuse Color'
          ParentColor = False
        end
        object Shape3: TShape
          Left = 96
          Height = 15
          Top = 46
          Width = 64
          Brush.Color = clSilver
          OnMouseDown = Shape3MouseDown
        end
        object Label14: TLabel
          Left = 16
          Height = 13
          Top = 67
          Width = 56
          Caption = 'Warm Color'
          ParentColor = False
        end
        object Shape4: TShape
          Left = 96
          Height = 15
          Top = 67
          Width = 64
          Brush.Color = clFuchsia
          OnMouseDown = Shape4MouseDown
        end
        object Label15: TLabel
          Left = 16
          Height = 13
          Top = 88
          Width = 49
          Caption = 'Cool Color'
          ParentColor = False
        end
        object Shape5: TShape
          Left = 96
          Height = 15
          Top = 88
          Width = 64
          Brush.Color = 1145343
          OnMouseDown = Shape5MouseDown
        end
        object Label16: TLabel
          Left = 16
          Height = 13
          Top = 110
          Width = 67
          Caption = 'Ambient Color'
          ParentColor = False
        end
        object Shape6: TShape
          Left = 96
          Height = 15
          Top = 109
          Width = 64
          Brush.Color = 3158064
          OnMouseDown = Shape6MouseDown
        end
        object Label17: TLabel
          Left = 16
          Height = 13
          Top = 131
          Width = 69
          Caption = 'Specular Color'
          ParentColor = False
        end
        object Shape7: TShape
          Left = 96
          Height = 15
          Top = 130
          Width = 64
          OnMouseDown = Shape7MouseDown
        end
        object Label18: TLabel
          Left = 16
          Height = 13
          Top = 163
          Width = 66
          Caption = 'Diffuse factor'
          ParentColor = False
        end
        object lblGoochDFactor: TLabel
          Left = 303
          Height = 13
          Top = 164
          Width = 22
          Caption = '0.80'
          ParentColor = False
        end
        object Label20: TLabel
          Left = 16
          Height = 13
          Top = 195
          Width = 60
          Caption = 'Warm factor'
          ParentColor = False
        end
        object lblGoochWFactor: TLabel
          Left = 303
          Height = 13
          Top = 196
          Width = 22
          Caption = '0.55'
          ParentColor = False
        end
        object Label22: TLabel
          Left = 16
          Height = 13
          Top = 227
          Width = 53
          Caption = 'Cool factor'
          ParentColor = False
        end
        object lblGoochCFactor: TLabel
          Left = 303
          Height = 13
          Top = 228
          Width = 22
          Caption = '0.30'
          ParentColor = False
        end
        object Label24: TLabel
          Left = 16
          Height = 13
          Top = 259
          Width = 71
          Caption = 'Ambient factor'
          ParentColor = False
        end
        object lblGoochAFactor: TLabel
          Left = 303
          Height = 13
          Top = 260
          Width = 16
          Caption = '1.0'
          ParentColor = False
        end
        object Label26: TLabel
          Left = 16
          Height = 13
          Top = 292
          Width = 73
          Caption = 'Specular factor'
          ParentColor = False
        end
        object lblGoochSFactor: TLabel
          Left = 303
          Height = 13
          Top = 292
          Width = 22
          Caption = '0.90'
          ParentColor = False
        end
        object Label40: TLabel
          Left = 16
          Height = 13
          Top = 323
          Width = 55
          Caption = 'Blend Mode'
          ParentColor = False
        end
        object Label41: TLabel
          Left = 164
          Height = 13
          Top = 47
          Width = 27
          Caption = 'Alpha'
          ParentColor = False
        end
        object lblGoochAlpha: TLabel
          Left = 309
          Height = 13
          Top = 47
          Width = 16
          Caption = '1.0'
          ParentColor = False
        end
        object chkGoochShader: TCheckBox
          Left = 16
          Height = 19
          Top = 16
          Width = 58
          Caption = 'Enabled'
          OnClick = chkGoochShaderClick
          TabOrder = 0
        end
        object tbGoochDFactor: TTrackBar
          Left = 96
          Height = 26
          Top = 160
          Width = 201
          Frequency = 10
          Max = 500
          Min = 1
          OnChange = tbGoochDFactorChange
          PageSize = 10
          Position = 80
          TickStyle = tsNone
          TabOrder = 1
        end
        object tbGoochWFactor: TTrackBar
          Left = 96
          Height = 26
          Top = 192
          Width = 201
          Frequency = 10
          Max = 500
          Min = 1
          OnChange = tbGoochWFactorChange
          PageSize = 10
          Position = 55
          TickStyle = tsNone
          TabOrder = 2
        end
        object tbGoochCFactor: TTrackBar
          Left = 96
          Height = 26
          Top = 224
          Width = 201
          Frequency = 10
          Max = 500
          Min = 1
          OnChange = tbGoochCFactorChange
          PageSize = 10
          Position = 30
          TickStyle = tsNone
          TabOrder = 3
        end
        object tbGoochAFactor: TTrackBar
          Left = 96
          Height = 26
          Top = 256
          Width = 201
          Frequency = 10
          Max = 500
          Min = 1
          OnChange = tbGoochAFactorChange
          PageSize = 10
          Position = 100
          TickStyle = tsNone
          TabOrder = 4
        end
        object tbGoochSFactor: TTrackBar
          Left = 96
          Height = 26
          Top = 288
          Width = 201
          Frequency = 10
          Max = 500
          Min = 1
          OnChange = tbGoochSFactorChange
          PageSize = 10
          Position = 90
          TickStyle = tsNone
          TabOrder = 5
        end
        object cbxGootchBlendMode: TComboBox
          Left = 104
          Height = 21
          Top = 320
          Width = 193
          ItemHeight = 13
          ItemIndex = 0
          Items.Strings = (
            'Opaque'
            'Transparency'
            'Additive'
            'AlphaTest50'
            'AlphaTest100'
            'Modulate'
            'DestColorOne'
            'DestAlphaOne'
          )
          OnChange = cbxGootchBlendModeChange
          Style = csDropDownList
          TabOrder = 6
          Text = 'Opaque'
        end
        object tbGoochAlpha: TTrackBar
          Left = 196
          Height = 26
          Top = 44
          Width = 114
          Frequency = 10
          Max = 100
          OnChange = tbGoochAlphaChange
          PageSize = 10
          Position = 100
          TickStyle = tsNone
          TabOrder = 7
        end
      end
      object TabSheet6: TTabSheet
        Caption = 'S.E.M'
        ClientHeight = 470
        ClientWidth = 343
        ImageIndex = 5
        object Label19: TLabel
          Left = 16
          Height = 13
          Top = 44
          Width = 66
          Caption = 'Diffuse factor'
          ParentColor = False
        end
        object lblSemDiffuseF: TLabel
          Left = 303
          Height = 13
          Top = 45
          Width = 22
          Caption = '0.90'
          ParentColor = False
        end
        object Label42: TLabel
          Left = 16
          Height = 13
          Top = 72
          Width = 71
          Caption = 'Ambient factor'
          ParentColor = False
        end
        object lblSemAmbientF: TLabel
          Left = 303
          Height = 13
          Top = 71
          Width = 22
          Caption = '0.80'
          ParentColor = False
        end
        object Label44: TLabel
          Left = 16
          Height = 13
          Top = 100
          Width = 73
          Caption = 'Specular factor'
          ParentColor = False
        end
        object lblSemSpecularF: TLabel
          Left = 303
          Height = 13
          Top = 100
          Width = 22
          Caption = '0.90'
          ParentColor = False
        end
        object Label46: TLabel
          Left = 14
          Height = 13
          Top = 130
          Width = 67
          Caption = 'Ambient Color'
          ParentColor = False
        end
        object Shape13: TShape
          Left = 94
          Height = 15
          Top = 128
          Width = 64
          Brush.Color = 2105376
          OnMouseDown = Shape13MouseDown
        end
        object Label47: TLabel
          Left = 14
          Height = 13
          Top = 149
          Width = 69
          Caption = 'Specular Color'
          ParentColor = False
        end
        object Shape14: TShape
          Left = 94
          Height = 15
          Top = 149
          Width = 64
          Brush.Color = 14540253
          OnMouseDown = Shape14MouseDown
        end
        object tbSemDiffuseF: TTrackBar
          Left = 107
          Height = 26
          Top = 39
          Width = 190
          Frequency = 10
          Max = 100
          OnChange = tbSemDiffuseFChange
          PageSize = 10
          Position = 90
          TickStyle = tsNone
          TabOrder = 0
        end
        object tbSemAmbientF: TTrackBar
          Left = 106
          Height = 28
          Top = 66
          Width = 191
          Frequency = 10
          Max = 100
          OnChange = tbSemAmbientFChange
          PageSize = 10
          Position = 80
          TickStyle = tsNone
          TabOrder = 1
        end
        object tbSemSpecularF: TTrackBar
          Left = 106
          Height = 26
          Top = 96
          Width = 191
          Frequency = 10
          Max = 100
          OnChange = tbSemSpecularFChange
          PageSize = 10
          Position = 90
          TickStyle = tsNone
          TabOrder = 2
        end
        object chkSEMShader: TCheckBox
          Left = 24
          Height = 19
          Top = 16
          Width = 58
          Caption = 'Enabled'
          OnClick = chkSEMShaderClick
          TabOrder = 3
        end
        object Button4: TButton
          Left = 16
          Height = 25
          Top = 179
          Width = 184
          Caption = 'Load MatCap Texture'
          OnClick = Button4Click
          TabOrder = 4
        end
      end
      object Displacement: TTabSheet
        Caption = 'Displacement'
        ClientHeight = 470
        ClientWidth = 422
        ImageIndex = 6
        object Label21: TLabel
          Left = 24
          Height = 13
          Top = 52
          Width = 66
          Caption = 'Diffuse factor'
          ParentColor = False
        end
        object Label43: TLabel
          Left = 24
          Height = 13
          Top = 80
          Width = 71
          Caption = 'Ambient factor'
          ParentColor = False
        end
        object Label45: TLabel
          Left = 24
          Height = 13
          Top = 108
          Width = 73
          Caption = 'Specular factor'
          ParentColor = False
        end
        object lblVDSpecularF: TLabel
          Left = 311
          Height = 13
          Top = 108
          Width = 22
          Caption = '0.90'
          ParentColor = False
        end
        object lblVDAmbientF: TLabel
          Left = 311
          Height = 13
          Top = 79
          Width = 22
          Caption = '0.80'
          ParentColor = False
        end
        object lblVDDiffuseF: TLabel
          Left = 311
          Height = 13
          Top = 53
          Width = 22
          Caption = '0.90'
          ParentColor = False
        end
        object Label51: TLabel
          Left = 22
          Height = 13
          Top = 138
          Width = 67
          Caption = 'Ambient Color'
          ParentColor = False
        end
        object Shape15: TShape
          Left = 102
          Height = 15
          Top = 136
          Width = 64
          Brush.Color = 2105376
          OnMouseDown = Shape13MouseDown
        end
        object Label52: TLabel
          Left = 22
          Height = 13
          Top = 157
          Width = 69
          Caption = 'Specular Color'
          ParentColor = False
        end
        object Shape16: TShape
          Left = 102
          Height = 15
          Top = 157
          Width = 64
          Brush.Color = 14540253
          OnMouseDown = Shape14MouseDown
        end
        object Label48: TLabel
          Left = 22
          Height = 13
          Top = 188
          Width = 26
          Caption = 'Noise'
          ParentColor = False
        end
        object lblVDNoise: TLabel
          Left = 311
          Height = 13
          Top = 188
          Width = 22
          Caption = '10.0'
          ParentColor = False
        end
        object Label49: TLabel
          Left = 22
          Height = 13
          Top = 211
          Width = 30
          Caption = 'Period'
          ParentColor = False
        end
        object lblVDPeriod: TLabel
          Left = 311
          Height = 13
          Top = 211
          Width = 16
          Caption = '5.0'
          ParentColor = False
        end
        object Label53: TLabel
          Left = 22
          Height = 13
          Top = 236
          Width = 54
          Caption = 'Noise Scale'
          ParentColor = False
        end
        object lblVDNScale: TLabel
          Left = 311
          Height = 13
          Top = 237
          Width = 22
          Caption = '0.05'
          ParentColor = False
        end
        object Label55: TLabel
          Left = 22
          Height = 13
          Top = 260
          Width = 53
          Caption = 'Turbulence'
          ParentColor = False
        end
        object lblVDTurb: TLabel
          Left = 311
          Height = 13
          Top = 261
          Width = 16
          Caption = '0.5'
          ParentColor = False
        end
        object Label57: TLabel
          Left = 22
          Height = 13
          Top = 284
          Width = 91
          Caption = 'Displacement Scale'
          ParentColor = False
        end
        object lblVDDispScale: TLabel
          Left = 311
          Height = 13
          Top = 284
          Width = 16
          Caption = '1.0'
          ParentColor = False
        end
        object Label50: TLabel
          Left = 22
          Height = 13
          Top = 307
          Width = 56
          Caption = 'Time Factor'
          ParentColor = False
        end
        object lblVDTimeF: TLabel
          Left = 311
          Height = 13
          Top = 307
          Width = 22
          Caption = '0.05'
          ParentColor = False
        end
        object chkVDShader: TCheckBox
          Left = 32
          Height = 19
          Top = 24
          Width = 58
          Caption = 'Enabled'
          OnClick = chkVDShaderClick
          TabOrder = 0
        end
        object tbVDDiffuseF: TTrackBar
          Left = 115
          Height = 26
          Top = 47
          Width = 190
          Frequency = 10
          Max = 100
          OnChange = tbVDDiffuseFChange
          PageSize = 10
          Position = 90
          TickStyle = tsNone
          TabOrder = 1
        end
        object tbVDAmbientF: TTrackBar
          Left = 114
          Height = 28
          Top = 74
          Width = 191
          Frequency = 10
          Max = 100
          OnChange = tbVDAmbientFChange
          PageSize = 10
          Position = 80
          TickStyle = tsNone
          TabOrder = 2
        end
        object tbVDSpecularF: TTrackBar
          Left = 114
          Height = 26
          Top = 104
          Width = 191
          Frequency = 10
          Max = 100
          OnChange = tbVDSpecularFChange
          PageSize = 10
          Position = 90
          TickStyle = tsNone
          TabOrder = 3
        end
        object chkVDAnimate: TCheckBox
          Left = 128
          Height = 19
          Top = 24
          Width = 59
          Caption = 'Animate'
          TabOrder = 4
        end
        object tbVDNoise: TTrackBar
          Left = 114
          Height = 26
          Top = 184
          Width = 191
          Frequency = 10
          Max = 5000
          OnChange = tbVDNoiseChange
          PageSize = 10
          Position = 1000
          TickStyle = tsNone
          TabOrder = 5
        end
        object tbVDPeriod: TTrackBar
          Left = 114
          Height = 26
          Top = 207
          Width = 191
          Frequency = 10
          Max = 500
          OnChange = tbVDPeriodChange
          PageSize = 10
          Position = 500
          TickStyle = tsNone
          TabOrder = 6
        end
        object tbVDNScale: TTrackBar
          Left = 114
          Height = 26
          Top = 230
          Width = 191
          Frequency = 10
          Max = 100
          OnChange = tbVDNScaleChange
          PageSize = 10
          Position = 5
          TickStyle = tsNone
          TabOrder = 7
        end
        object tbVDTurb: TTrackBar
          Left = 114
          Height = 26
          Top = 256
          Width = 191
          Frequency = 10
          Max = 200
          OnChange = tbVDTurbChange
          PageSize = 10
          Position = 50
          TickStyle = tsNone
          TabOrder = 8
        end
        object tbVDDispScale: TTrackBar
          Left = 114
          Height = 26
          Top = 280
          Width = 191
          Frequency = 10
          Max = 1000
          OnChange = tbVDDispScaleChange
          PageSize = 10
          Position = 100
          TickStyle = tsNone
          TabOrder = 9
        end
        object tbVDTimeF: TTrackBar
          Left = 114
          Height = 26
          Top = 303
          Width = 191
          Frequency = 10
          Max = 1000
          OnChange = tbVDTimeFChange
          PageSize = 10
          Position = 5
          TickStyle = tsNone
          TabOrder = 10
        end
        object Button3: TButton
          Left = 16
          Height = 25
          Top = 344
          Width = 113
          Caption = 'Reset Time'
          OnClick = Button3Click
          TabOrder = 11
        end
        object Button1: TButton
          Left = 16
          Height = 25
          Top = 375
          Width = 184
          Caption = 'Load Main Texture'
          OnClick = Button1Click
          TabOrder = 12
        end
      end
      object TabSheet7: TTabSheet
        Caption = 'Glass'
        ClientHeight = 470
        ClientWidth = 422
        ImageIndex = 7
        object Label56: TLabel
          Left = 11
          Height = 13
          Top = 36
          Width = 29
          Caption = 'Depth'
          ParentColor = False
        end
        object Label58: TLabel
          Left = 24
          Height = 13
          Top = 66
          Width = 16
          Caption = 'Mix'
          ParentColor = False
        end
        object Label59: TLabel
          Left = 14
          Height = 13
          Top = 146
          Width = 62
          Caption = 'Diffuse Color'
          ParentColor = False
        end
        object Shape17: TShape
          Left = 87
          Height = 15
          Top = 145
          Width = 64
          Brush.Color = 2105376
          OnMouseDown = Shape17MouseDown
        end
        object lblGlassDepth: TLabel
          Left = 285
          Height = 13
          Top = 36
          Width = 16
          Caption = '0.1'
          ParentColor = False
        end
        object lblGlassMix: TLabel
          Left = 283
          Height = 13
          Top = 66
          Width = 16
          Caption = '1.0'
          ParentColor = False
        end
        object Label60: TLabel
          Left = 10
          Height = 13
          Top = 99
          Width = 27
          Caption = 'Alpha'
          ParentColor = False
        end
        object lblGlassAlpha: TLabel
          Left = 282
          Height = 13
          Top = 99
          Width = 16
          Caption = '1.0'
          ParentColor = False
        end
        object Label61: TLabel
          Left = 18
          Height = 13
          Top = 179
          Width = 44
          Caption = 'Blend Src'
          ParentColor = False
        end
        object Label62: TLabel
          Left = 18
          Height = 13
          Top = 205
          Width = 51
          Caption = 'Blend Dest'
          ParentColor = False
        end
        object tbGlassDepth: TTrackBar
          Left = 46
          Height = 26
          Top = 33
          Width = 235
          Frequency = 10
          Max = 100
          OnChange = tbGlassDepthChange
          PageSize = 10
          Position = 10
          TickStyle = tsNone
          TabOrder = 0
        end
        object tbGlassMix: TTrackBar
          Left = 45
          Height = 28
          Top = 62
          Width = 236
          Frequency = 10
          Max = 200
          OnChange = tbGlassMixChange
          PageSize = 10
          Position = 100
          TickStyle = tsNone
          TabOrder = 1
        end
        object Button10: TButton
          Left = 3
          Height = 25
          Top = 254
          Width = 184
          Caption = 'Load Refraction Texture'
          OnClick = Button10Click
          TabOrder = 2
        end
        object chkGlassShader: TCheckBox
          Left = 24
          Height = 19
          Top = 8
          Width = 58
          Caption = 'Enabled'
          OnClick = chkGlassShaderClick
          TabOrder = 3
        end
        object tbGlassAlpha: TTrackBar
          Left = 45
          Height = 26
          Top = 96
          Width = 235
          Frequency = 10
          Max = 100
          OnChange = tbGlassAlphaChange
          PageSize = 10
          Position = 100
          TickStyle = tsNone
          TabOrder = 4
        end
        object cbxGlassBlendSrc: TComboBox
          Left = 87
          Height = 21
          Top = 176
          Width = 145
          ItemHeight = 13
          ItemIndex = 6
          Items.Strings = (
            'ZERO'
            'ONE'
            'SRC COLOR'
            'ONE MINUS SRC COLOR'
            'DST COLOR'
            'ONE MINUS DST COLOR'
            'SRC ALPHA'
            'MINUS SRC ALPHA'
            'DST ALPHA'
            'MINUS DST ALPHA'
            'SRC ALPHA SATURATE'
            'CONSTANT COLOR'
            'ONE MINUS CONSTANT COLOR'
            'CONSTANT ALPHA'
            'ONE MINUS CONSTATNT ALPHA'
          )
          OnChange = cbxGlassBlendSrcChange
          Style = csDropDownList
          TabOrder = 5
          Text = 'SRC ALPHA'
        end
        object cbxGlassBlendDst: TComboBox
          Left = 87
          Height = 21
          Top = 203
          Width = 145
          ItemHeight = 13
          ItemIndex = 8
          Items.Strings = (
            'ZERO'
            'ONE'
            'SRC COLOR'
            'ONE MINUS SRC COLOR'
            'DST COLOR'
            'ONE MINUS DST COLOR'
            'SRC ALPHA'
            'MINUS SRC ALPHA'
            'DST ALPHA'
            'MINUS DST ALPHA'
            'SRC ALPHA SATURATE'
            'CONSTANT COLOR'
            'ONE MINUS CONSTANT COLOR'
            'CONSTANT ALPHA'
            'ONE MINUS CONSTATNT ALPHA'
          )
          OnChange = cbxGlassBlendDstChange
          Style = csDropDownList
          TabOrder = 6
          Text = 'DST ALPHA'
        end
      end
      object TabSheet8: TTabSheet
        Caption = 'Toon'
        ClientHeight = 470
        ClientWidth = 422
        ImageIndex = 8
        object Label64: TLabel
          Left = 7
          Height = 13
          Top = 51
          Width = 66
          Caption = 'HighLight Size'
          ParentColor = False
        end
        object lblToonHighlightSize: TLabel
          Left = 290
          Height = 13
          Top = 51
          Width = 22
          Caption = '0.95'
          ParentColor = False
        end
        object Label66: TLabel
          Left = 7
          Height = 13
          Top = 73
          Width = 38
          Caption = 'Mid Size'
          ParentColor = False
        end
        object lblToonMidSize: TLabel
          Left = 290
          Height = 13
          Top = 73
          Width = 16
          Caption = '0.5'
          ParentColor = False
        end
        object Label68: TLabel
          Left = 7
          Height = 13
          Top = 95
          Width = 60
          Caption = 'Shadow Size'
          ParentColor = False
        end
        object lblToonShadowSize: TLabel
          Left = 290
          Height = 13
          Top = 95
          Width = 22
          Caption = '0.25'
          ParentColor = False
        end
        object Label70: TLabel
          Left = 7
          Height = 13
          Top = 117
          Width = 65
          Caption = 'Outline Width'
          ParentColor = False
        end
        object lblToonOutlineWidth: TLabel
          Left = 290
          Height = 13
          Top = 117
          Width = 22
          Caption = '0.25'
          ParentColor = False
        end
        object Label72: TLabel
          Left = 8
          Height = 13
          Top = 146
          Width = 69
          Caption = 'Highlight Color'
          ParentColor = False
        end
        object Shape18: TShape
          Left = 118
          Height = 15
          Top = 146
          Width = 64
          Brush.Color = 15658734
          OnMouseDown = Shape18MouseDown
        end
        object Label73: TLabel
          Left = 8
          Height = 13
          Top = 167
          Width = 44
          Caption = 'Mid Color'
          ParentColor = False
        end
        object Shape19: TShape
          Left = 118
          Height = 15
          Top = 167
          Width = 64
          Brush.Color = 13421772
          OnMouseDown = Shape19MouseDown
        end
        object Label74: TLabel
          Left = 8
          Height = 13
          Top = 188
          Width = 104
          Caption = 'Lighten Shadow Color'
          ParentColor = False
        end
        object Shape20: TShape
          Left = 118
          Height = 15
          Top = 188
          Width = 64
          Brush.Color = clGray
          OnMouseDown = Shape20MouseDown
        end
        object Label75: TLabel
          Left = 8
          Height = 13
          Top = 209
          Width = 103
          Caption = 'Darken Shadow Color'
          ParentColor = False
        end
        object Shape21: TShape
          Left = 118
          Height = 15
          Top = 209
          Width = 64
          Brush.Color = 3158064
          OnMouseDown = Shape21MouseDown
        end
        object Label76: TLabel
          Left = 7
          Height = 13
          Top = 230
          Width = 62
          Caption = 'Outline Color'
          ParentColor = False
        end
        object Shape22: TShape
          Left = 118
          Height = 15
          Top = 230
          Width = 64
          Brush.Color = clBlack
          OnMouseDown = Shape22MouseDown
        end
        object chkToonShader: TCheckBox
          Left = 8
          Height = 19
          Top = 16
          Width = 58
          Caption = 'Enabled'
          OnClick = chkToonShaderClick
          TabOrder = 0
        end
        object tbToonHighlightSize: TTrackBar
          Left = 75
          Height = 26
          Top = 48
          Width = 213
          Frequency = 10
          Max = 100
          OnChange = tbToonHighlightSizeChange
          PageSize = 10
          Position = 95
          TickStyle = tsNone
          TabOrder = 1
        end
        object tbToonMidSize: TTrackBar
          Left = 75
          Height = 26
          Top = 70
          Width = 213
          Frequency = 10
          Max = 100
          OnChange = tbToonMidSizeChange
          PageSize = 10
          Position = 50
          TickStyle = tsNone
          TabOrder = 2
        end
        object tbToonShadowSize: TTrackBar
          Left = 75
          Height = 26
          Top = 92
          Width = 213
          Frequency = 10
          Max = 100
          OnChange = tbToonShadowSizeChange
          PageSize = 10
          Position = 25
          TickStyle = tsNone
          TabOrder = 3
        end
        object tbToonOutlineWidth: TTrackBar
          Left = 75
          Height = 26
          Top = 114
          Width = 213
          Frequency = 10
          Max = 100
          OnChange = tbToonOutlineWidthChange
          PageSize = 10
          Position = 25
          TickStyle = tsNone
          TabOrder = 4
        end
      end
    end
    object chkAnimScene: TCheckBox
      Left = 13
      Height = 19
      Top = 499
      Width = 91
      Caption = 'Animate Scene'
      TabOrder = 1
    end
    object chkLightmoving: TCheckBox
      Left = 13
      Height = 19
      Top = 522
      Width = 80
      Caption = 'Light moving'
      TabOrder = 2
    end
    object cbxObjects: TComboBox
      Left = 187
      Height = 21
      Top = 497
      Width = 145
      ItemHeight = 13
      ItemIndex = 0
      Items.Strings = (
        'Suzanne'
        'Knot'
        'Spoutnik'
        'Rectangle Spirale'
        'Geode'
        'Syamil'
        'GLTorus'
        'GLSphere'
      )
      OnChange = cbxObjectsChange
      Style = csDropDownList
      TabOrder = 3
      Text = 'Suzanne'
    end
    object Button2: TButton
      Left = 148
      Height = 25
      Top = 524
      Width = 184
      Caption = 'Choose Background Color'
      OnClick = Button2Click
      TabOrder = 4
    end
    object chkBackgroundImg: TCheckBox
      Left = 128
      Height = 19
      Hint = 'Show Background Texture'
      Top = 555
      Width = 20
      OnClick = chkBackgroundImgClick
      TabOrder = 5
    end
    object Button11: TButton
      Left = 148
      Height = 25
      Top = 551
      Width = 184
      Caption = 'Load Background Texture'
      Enabled = False
      OnClick = Button11Click
      TabOrder = 6
    end
  end
  object Viewer: TGLSceneViewer
    Left = 432
    Height = 597
    Top = 0
    Width = 633
    Camera = Camera
    Buffer.BackgroundColor = clBlack
    Buffer.AmbientColor.Red = 0.200000002980232
    Buffer.AmbientColor.Green = 0.200000002980232
    Buffer.AmbientColor.Blue = 0.200000002980232
    Buffer.AmbientColor.Alpha = 1
    Buffer.AntiAliasing = aa2x
    FieldOfView = 160.981994628906
    Align = alClient
    TabOrder = 1
  end
  object MaterialLibrary: TGLMaterialLibrary
    Materials = <    
      item
        Name = 'MainTexture'
        Tag = 0
        Material.BackProperties.Ambient.Red = 0.200000002980232
        Material.BackProperties.Ambient.Green = 0.200000002980232
        Material.BackProperties.Ambient.Blue = 0.200000002980232
        Material.BackProperties.Ambient.Alpha = 1
        Material.BackProperties.Diffuse.Red = 0.800000011920929
        Material.BackProperties.Diffuse.Green = 0.800000011920929
        Material.BackProperties.Diffuse.Blue = 0.800000011920929
        Material.BackProperties.Diffuse.Alpha = 1
        Material.BackProperties.Emission.Red = 0
        Material.BackProperties.Emission.Green = 0
        Material.BackProperties.Emission.Blue = 0
        Material.BackProperties.Emission.Alpha = 1
        Material.BackProperties.Shininess = 10
        Material.BackProperties.Specular.Red = 0
        Material.BackProperties.Specular.Green = 0
        Material.BackProperties.Specular.Blue = 0
        Material.BackProperties.Specular.Alpha = 1
        Material.FrontProperties.Ambient.Red = 0.200000002980232
        Material.FrontProperties.Ambient.Green = 0.200000002980232
        Material.FrontProperties.Ambient.Blue = 0.200000002980232
        Material.FrontProperties.Ambient.Alpha = 1
        Material.FrontProperties.Diffuse.Red = 0.800000011920929
        Material.FrontProperties.Diffuse.Green = 0.800000011920929
        Material.FrontProperties.Diffuse.Blue = 0.800000011920929
        Material.FrontProperties.Diffuse.Alpha = 1
        Material.FrontProperties.Emission.Red = 0
        Material.FrontProperties.Emission.Green = 0
        Material.FrontProperties.Emission.Blue = 0
        Material.FrontProperties.Emission.Alpha = 1
        Material.FrontProperties.Shininess = 10
        Material.FrontProperties.Specular.Red = 0.298039227724075
        Material.FrontProperties.Specular.Green = 0.298039227724075
        Material.FrontProperties.Specular.Blue = 0.298039227724075
        Material.FrontProperties.Specular.Alpha = 1
        Material.FrontProperties.Specular.Color = {
          9998983E9998983E9998983E0000803F
        }
        Material.BlendingMode = bmTransparency
        Material.Texture.TextureMode = tmModulate
        Material.Texture.EnvColor.Red = 0
        Material.Texture.EnvColor.Green = 0
        Material.Texture.EnvColor.Blue = 0
        Material.Texture.EnvColor.Alpha = 0
        Material.Texture.BorderColor.Red = 0
        Material.Texture.BorderColor.Green = 0
        Material.Texture.BorderColor.Blue = 0
        Material.Texture.BorderColor.Alpha = 0
        Material.Texture.Disabled = False
      end    
      item
        Name = 'NoiseTexture'
        Tag = 0
        Material.BackProperties.Ambient.Red = 0.200000002980232
        Material.BackProperties.Ambient.Green = 0.200000002980232
        Material.BackProperties.Ambient.Blue = 0.200000002980232
        Material.BackProperties.Ambient.Alpha = 1
        Material.BackProperties.Diffuse.Red = 0.800000011920929
        Material.BackProperties.Diffuse.Green = 0.800000011920929
        Material.BackProperties.Diffuse.Blue = 0.800000011920929
        Material.BackProperties.Diffuse.Alpha = 1
        Material.BackProperties.Emission.Red = 0
        Material.BackProperties.Emission.Green = 0
        Material.BackProperties.Emission.Blue = 0
        Material.BackProperties.Emission.Alpha = 1
        Material.BackProperties.Specular.Red = 0
        Material.BackProperties.Specular.Green = 0
        Material.BackProperties.Specular.Blue = 0
        Material.BackProperties.Specular.Alpha = 1
        Material.FrontProperties.Ambient.Red = 0.200000002980232
        Material.FrontProperties.Ambient.Green = 0.200000002980232
        Material.FrontProperties.Ambient.Blue = 0.200000002980232
        Material.FrontProperties.Ambient.Alpha = 1
        Material.FrontProperties.Diffuse.Red = 0.800000011920929
        Material.FrontProperties.Diffuse.Green = 0.800000011920929
        Material.FrontProperties.Diffuse.Blue = 0.800000011920929
        Material.FrontProperties.Diffuse.Alpha = 1
        Material.FrontProperties.Emission.Red = 0
        Material.FrontProperties.Emission.Green = 0
        Material.FrontProperties.Emission.Blue = 0
        Material.FrontProperties.Emission.Alpha = 1
        Material.FrontProperties.Specular.Red = 0
        Material.FrontProperties.Specular.Green = 0
        Material.FrontProperties.Specular.Blue = 0
        Material.FrontProperties.Specular.Alpha = 1
        Material.Texture.EnvColor.Red = 0
        Material.Texture.EnvColor.Green = 0
        Material.Texture.EnvColor.Blue = 0
        Material.Texture.EnvColor.Alpha = 0
        Material.Texture.BorderColor.Red = 0
        Material.Texture.BorderColor.Green = 0
        Material.Texture.BorderColor.Blue = 0
        Material.Texture.BorderColor.Alpha = 0
        Material.Texture.Disabled = False
      end    
      item
        Name = 'ShaderMaterial'
        Tag = 0
        Material.BackProperties.Ambient.Red = 0.200000002980232
        Material.BackProperties.Ambient.Green = 0.200000002980232
        Material.BackProperties.Ambient.Blue = 0.200000002980232
        Material.BackProperties.Ambient.Alpha = 1
        Material.BackProperties.Diffuse.Red = 0.800000011920929
        Material.BackProperties.Diffuse.Green = 0.800000011920929
        Material.BackProperties.Diffuse.Blue = 0.800000011920929
        Material.BackProperties.Diffuse.Alpha = 1
        Material.BackProperties.Emission.Red = 0
        Material.BackProperties.Emission.Green = 0
        Material.BackProperties.Emission.Blue = 0
        Material.BackProperties.Emission.Alpha = 1
        Material.BackProperties.Specular.Red = 0
        Material.BackProperties.Specular.Green = 0
        Material.BackProperties.Specular.Blue = 0
        Material.BackProperties.Specular.Alpha = 1
        Material.FrontProperties.Ambient.Red = 0.200000002980232
        Material.FrontProperties.Ambient.Green = 0.200000002980232
        Material.FrontProperties.Ambient.Blue = 0.200000002980232
        Material.FrontProperties.Ambient.Alpha = 1
        Material.FrontProperties.Diffuse.Red = 0.800000011920929
        Material.FrontProperties.Diffuse.Green = 0.800000011920929
        Material.FrontProperties.Diffuse.Blue = 0.800000011920929
        Material.FrontProperties.Diffuse.Alpha = 1
        Material.FrontProperties.Emission.Red = 0.0780000016093254
        Material.FrontProperties.Emission.Green = 0.0710000023245811
        Material.FrontProperties.Emission.Blue = 0.063000001013279
        Material.FrontProperties.Emission.Alpha = 1
        Material.FrontProperties.Emission.Color = {
          77BE9F3D7368913D2506813D0000803F
        }
        Material.FrontProperties.Specular.Red = 0.866999983787537
        Material.FrontProperties.Specular.Green = 0.859000027179718
        Material.FrontProperties.Specular.Blue = 0.859000027179718
        Material.FrontProperties.Specular.Alpha = 1
        Material.FrontProperties.Specular.Color = {
          B6F35D3F6DE75B3F6DE75B3F0000803F
        }
        Material.Texture.TextureMode = tmModulate
        Material.Texture.EnvColor.Red = 0
        Material.Texture.EnvColor.Green = 0
        Material.Texture.EnvColor.Blue = 0
        Material.Texture.EnvColor.Alpha = 0
        Material.Texture.BorderColor.Red = 0
        Material.Texture.BorderColor.Green = 0
        Material.Texture.BorderColor.Blue = 0
        Material.Texture.BorderColor.Alpha = 0
        Material.Texture.Disabled = False
        Material.Texture.KeepImageAfterTransfer = True
      end    
      item
        Name = 'ErosionNoiseTexture'
        Tag = 0
        Material.BackProperties.Ambient.Red = 0.200000002980232
        Material.BackProperties.Ambient.Green = 0.200000002980232
        Material.BackProperties.Ambient.Blue = 0.200000002980232
        Material.BackProperties.Ambient.Alpha = 1
        Material.BackProperties.Diffuse.Red = 0.800000011920929
        Material.BackProperties.Diffuse.Green = 0.800000011920929
        Material.BackProperties.Diffuse.Blue = 0.800000011920929
        Material.BackProperties.Diffuse.Alpha = 1
        Material.BackProperties.Emission.Red = 0
        Material.BackProperties.Emission.Green = 0
        Material.BackProperties.Emission.Blue = 0
        Material.BackProperties.Emission.Alpha = 1
        Material.BackProperties.Specular.Red = 0
        Material.BackProperties.Specular.Green = 0
        Material.BackProperties.Specular.Blue = 0
        Material.BackProperties.Specular.Alpha = 1
        Material.FrontProperties.Ambient.Red = 0.200000002980232
        Material.FrontProperties.Ambient.Green = 0.200000002980232
        Material.FrontProperties.Ambient.Blue = 0.200000002980232
        Material.FrontProperties.Ambient.Alpha = 1
        Material.FrontProperties.Diffuse.Red = 0.800000011920929
        Material.FrontProperties.Diffuse.Green = 0.800000011920929
        Material.FrontProperties.Diffuse.Blue = 0.800000011920929
        Material.FrontProperties.Diffuse.Alpha = 1
        Material.FrontProperties.Emission.Red = 0
        Material.FrontProperties.Emission.Green = 0
        Material.FrontProperties.Emission.Blue = 0
        Material.FrontProperties.Emission.Alpha = 1
        Material.FrontProperties.Specular.Red = 0
        Material.FrontProperties.Specular.Green = 0
        Material.FrontProperties.Specular.Blue = 0
        Material.FrontProperties.Specular.Alpha = 1
        Material.Texture.EnvColor.Red = 0
        Material.Texture.EnvColor.Green = 0
        Material.Texture.EnvColor.Blue = 0
        Material.Texture.EnvColor.Alpha = 0
        Material.Texture.BorderColor.Red = 0
        Material.Texture.BorderColor.Green = 0
        Material.Texture.BorderColor.Blue = 0
        Material.Texture.BorderColor.Alpha = 0
        Material.Texture.Disabled = False
      end    
      item
        Name = 'ErosionMainTexture'
        Tag = 0
        Material.BackProperties.Ambient.Red = 0.200000002980232
        Material.BackProperties.Ambient.Green = 0.200000002980232
        Material.BackProperties.Ambient.Blue = 0.200000002980232
        Material.BackProperties.Ambient.Alpha = 1
        Material.BackProperties.Diffuse.Red = 0.800000011920929
        Material.BackProperties.Diffuse.Green = 0.800000011920929
        Material.BackProperties.Diffuse.Blue = 0.800000011920929
        Material.BackProperties.Diffuse.Alpha = 1
        Material.BackProperties.Emission.Red = 0
        Material.BackProperties.Emission.Green = 0
        Material.BackProperties.Emission.Blue = 0
        Material.BackProperties.Emission.Alpha = 1
        Material.BackProperties.Specular.Red = 0
        Material.BackProperties.Specular.Green = 0
        Material.BackProperties.Specular.Blue = 0
        Material.BackProperties.Specular.Alpha = 1
        Material.FrontProperties.Ambient.Red = 0.200000002980232
        Material.FrontProperties.Ambient.Green = 0.200000002980232
        Material.FrontProperties.Ambient.Blue = 0.200000002980232
        Material.FrontProperties.Ambient.Alpha = 1
        Material.FrontProperties.Diffuse.Red = 0.800000011920929
        Material.FrontProperties.Diffuse.Green = 0.800000011920929
        Material.FrontProperties.Diffuse.Blue = 0.800000011920929
        Material.FrontProperties.Diffuse.Alpha = 1
        Material.FrontProperties.Emission.Red = 0
        Material.FrontProperties.Emission.Green = 0
        Material.FrontProperties.Emission.Blue = 0
        Material.FrontProperties.Emission.Alpha = 1
        Material.FrontProperties.Specular.Red = 0
        Material.FrontProperties.Specular.Green = 0
        Material.FrontProperties.Specular.Blue = 0
        Material.FrontProperties.Specular.Alpha = 1
        Material.BlendingMode = bmModulate
        Material.Texture.EnvColor.Red = 0
        Material.Texture.EnvColor.Green = 0
        Material.Texture.EnvColor.Blue = 0
        Material.Texture.EnvColor.Alpha = 0
        Material.Texture.BorderColor.Red = 0
        Material.Texture.BorderColor.Green = 0
        Material.Texture.BorderColor.Blue = 0
        Material.Texture.BorderColor.Alpha = 0
        Material.Texture.Disabled = False
      end    
      item
        Name = 'ErosionTexture'
        Tag = 0
        Material.BackProperties.Ambient.Red = 0.200000002980232
        Material.BackProperties.Ambient.Green = 0.200000002980232
        Material.BackProperties.Ambient.Blue = 0.200000002980232
        Material.BackProperties.Ambient.Alpha = 1
        Material.BackProperties.Diffuse.Red = 0.800000011920929
        Material.BackProperties.Diffuse.Green = 0.800000011920929
        Material.BackProperties.Diffuse.Blue = 0.800000011920929
        Material.BackProperties.Diffuse.Alpha = 1
        Material.BackProperties.Emission.Red = 0
        Material.BackProperties.Emission.Green = 0
        Material.BackProperties.Emission.Blue = 0
        Material.BackProperties.Emission.Alpha = 1
        Material.BackProperties.Specular.Red = 0
        Material.BackProperties.Specular.Green = 0
        Material.BackProperties.Specular.Blue = 0
        Material.BackProperties.Specular.Alpha = 1
        Material.FrontProperties.Ambient.Red = 0.200000002980232
        Material.FrontProperties.Ambient.Green = 0.200000002980232
        Material.FrontProperties.Ambient.Blue = 0.200000002980232
        Material.FrontProperties.Ambient.Alpha = 1
        Material.FrontProperties.Diffuse.Red = 0.800000011920929
        Material.FrontProperties.Diffuse.Green = 0.800000011920929
        Material.FrontProperties.Diffuse.Blue = 0.800000011920929
        Material.FrontProperties.Diffuse.Alpha = 1
        Material.FrontProperties.Emission.Red = 0
        Material.FrontProperties.Emission.Green = 0
        Material.FrontProperties.Emission.Blue = 0
        Material.FrontProperties.Emission.Alpha = 1
        Material.FrontProperties.Specular.Red = 0
        Material.FrontProperties.Specular.Green = 0
        Material.FrontProperties.Specular.Blue = 0
        Material.FrontProperties.Specular.Alpha = 1
        Material.BlendingMode = bmModulate
        Material.Texture.EnvColor.Red = 0
        Material.Texture.EnvColor.Green = 0
        Material.Texture.EnvColor.Blue = 0
        Material.Texture.EnvColor.Alpha = 0
        Material.Texture.BorderColor.Red = 0
        Material.Texture.BorderColor.Green = 0
        Material.Texture.BorderColor.Blue = 0
        Material.Texture.BorderColor.Alpha = 0
        Material.Texture.Disabled = False
      end    
      item
        Name = 'MatCapTexture'
        Tag = 0
        Material.BackProperties.Ambient.Red = 0.200000002980232
        Material.BackProperties.Ambient.Green = 0.200000002980232
        Material.BackProperties.Ambient.Blue = 0.200000002980232
        Material.BackProperties.Ambient.Alpha = 1
        Material.BackProperties.Diffuse.Red = 0.800000011920929
        Material.BackProperties.Diffuse.Green = 0.800000011920929
        Material.BackProperties.Diffuse.Blue = 0.800000011920929
        Material.BackProperties.Diffuse.Alpha = 1
        Material.BackProperties.Emission.Red = 0
        Material.BackProperties.Emission.Green = 0
        Material.BackProperties.Emission.Blue = 0
        Material.BackProperties.Emission.Alpha = 1
        Material.BackProperties.Specular.Red = 0
        Material.BackProperties.Specular.Green = 0
        Material.BackProperties.Specular.Blue = 0
        Material.BackProperties.Specular.Alpha = 1
        Material.FrontProperties.Ambient.Red = 0.200000002980232
        Material.FrontProperties.Ambient.Green = 0.200000002980232
        Material.FrontProperties.Ambient.Blue = 0.200000002980232
        Material.FrontProperties.Ambient.Alpha = 1
        Material.FrontProperties.Diffuse.Red = 0.800000011920929
        Material.FrontProperties.Diffuse.Green = 0.800000011920929
        Material.FrontProperties.Diffuse.Blue = 0.800000011920929
        Material.FrontProperties.Diffuse.Alpha = 1
        Material.FrontProperties.Emission.Red = 0
        Material.FrontProperties.Emission.Green = 0
        Material.FrontProperties.Emission.Blue = 0
        Material.FrontProperties.Emission.Alpha = 1
        Material.FrontProperties.Specular.Red = 0
        Material.FrontProperties.Specular.Green = 0
        Material.FrontProperties.Specular.Blue = 0
        Material.FrontProperties.Specular.Alpha = 1
        Material.Texture.EnvColor.Red = 0
        Material.Texture.EnvColor.Green = 0
        Material.Texture.EnvColor.Blue = 0
        Material.Texture.EnvColor.Alpha = 0
        Material.Texture.BorderColor.Red = 0
        Material.Texture.BorderColor.Green = 0
        Material.Texture.BorderColor.Blue = 0
        Material.Texture.BorderColor.Alpha = 0
        Material.Texture.Disabled = False
      end    
      item
        Name = 'ExplosionTexture'
        Tag = 0
        Material.BackProperties.Ambient.Red = 0.200000002980232
        Material.BackProperties.Ambient.Green = 0.200000002980232
        Material.BackProperties.Ambient.Blue = 0.200000002980232
        Material.BackProperties.Ambient.Alpha = 1
        Material.BackProperties.Diffuse.Red = 0.800000011920929
        Material.BackProperties.Diffuse.Green = 0.800000011920929
        Material.BackProperties.Diffuse.Blue = 0.800000011920929
        Material.BackProperties.Diffuse.Alpha = 1
        Material.BackProperties.Emission.Red = 0
        Material.BackProperties.Emission.Green = 0
        Material.BackProperties.Emission.Blue = 0
        Material.BackProperties.Emission.Alpha = 1
        Material.BackProperties.Specular.Red = 0
        Material.BackProperties.Specular.Green = 0
        Material.BackProperties.Specular.Blue = 0
        Material.BackProperties.Specular.Alpha = 1
        Material.FrontProperties.Ambient.Red = 0.200000002980232
        Material.FrontProperties.Ambient.Green = 0.200000002980232
        Material.FrontProperties.Ambient.Blue = 0.200000002980232
        Material.FrontProperties.Ambient.Alpha = 1
        Material.FrontProperties.Diffuse.Red = 0.800000011920929
        Material.FrontProperties.Diffuse.Green = 0.800000011920929
        Material.FrontProperties.Diffuse.Blue = 0.800000011920929
        Material.FrontProperties.Diffuse.Alpha = 1
        Material.FrontProperties.Emission.Red = 0
        Material.FrontProperties.Emission.Green = 0
        Material.FrontProperties.Emission.Blue = 0
        Material.FrontProperties.Emission.Alpha = 1
        Material.FrontProperties.Specular.Red = 0
        Material.FrontProperties.Specular.Green = 0
        Material.FrontProperties.Specular.Blue = 0
        Material.FrontProperties.Specular.Alpha = 1
        Material.Texture.EnvColor.Red = 0
        Material.Texture.EnvColor.Green = 0
        Material.Texture.EnvColor.Blue = 0
        Material.Texture.EnvColor.Alpha = 0
        Material.Texture.BorderColor.Red = 0
        Material.Texture.BorderColor.Green = 0
        Material.Texture.BorderColor.Blue = 0
        Material.Texture.BorderColor.Alpha = 0
        Material.Texture.Disabled = False
      end    
      item
        Name = 'EnvMap'
        Tag = 0
        Material.BackProperties.Ambient.Red = 0.200000002980232
        Material.BackProperties.Ambient.Green = 0.200000002980232
        Material.BackProperties.Ambient.Blue = 0.200000002980232
        Material.BackProperties.Ambient.Alpha = 1
        Material.BackProperties.Diffuse.Red = 0.800000011920929
        Material.BackProperties.Diffuse.Green = 0.800000011920929
        Material.BackProperties.Diffuse.Blue = 0.800000011920929
        Material.BackProperties.Diffuse.Alpha = 1
        Material.BackProperties.Emission.Red = 0
        Material.BackProperties.Emission.Green = 0
        Material.BackProperties.Emission.Blue = 0
        Material.BackProperties.Emission.Alpha = 1
        Material.BackProperties.Specular.Red = 0
        Material.BackProperties.Specular.Green = 0
        Material.BackProperties.Specular.Blue = 0
        Material.BackProperties.Specular.Alpha = 1
        Material.FrontProperties.Ambient.Red = 0.200000002980232
        Material.FrontProperties.Ambient.Green = 0.200000002980232
        Material.FrontProperties.Ambient.Blue = 0.200000002980232
        Material.FrontProperties.Ambient.Alpha = 1
        Material.FrontProperties.Diffuse.Red = 0.800000011920929
        Material.FrontProperties.Diffuse.Green = 0.800000011920929
        Material.FrontProperties.Diffuse.Blue = 0.800000011920929
        Material.FrontProperties.Diffuse.Alpha = 1
        Material.FrontProperties.Emission.Red = 0
        Material.FrontProperties.Emission.Green = 0
        Material.FrontProperties.Emission.Blue = 0
        Material.FrontProperties.Emission.Alpha = 1
        Material.FrontProperties.Specular.Red = 0
        Material.FrontProperties.Specular.Green = 0
        Material.FrontProperties.Specular.Blue = 0
        Material.FrontProperties.Specular.Alpha = 1
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.EnvColor.Red = 0
        Material.Texture.EnvColor.Green = 0
        Material.Texture.EnvColor.Blue = 0
        Material.Texture.EnvColor.Alpha = 0
        Material.Texture.BorderColor.Red = 0
        Material.Texture.BorderColor.Green = 0
        Material.Texture.BorderColor.Blue = 0
        Material.Texture.BorderColor.Alpha = 0
        Material.Texture.Disabled = False
      end    
      item
        Name = 'RefractMap'
        Tag = 0
        Material.BackProperties.Ambient.Red = 0.200000002980232
        Material.BackProperties.Ambient.Green = 0.200000002980232
        Material.BackProperties.Ambient.Blue = 0.200000002980232
        Material.BackProperties.Ambient.Alpha = 1
        Material.BackProperties.Diffuse.Red = 0.800000011920929
        Material.BackProperties.Diffuse.Green = 0.800000011920929
        Material.BackProperties.Diffuse.Blue = 0.800000011920929
        Material.BackProperties.Diffuse.Alpha = 1
        Material.BackProperties.Emission.Red = 0
        Material.BackProperties.Emission.Green = 0
        Material.BackProperties.Emission.Blue = 0
        Material.BackProperties.Emission.Alpha = 1
        Material.BackProperties.Specular.Red = 0
        Material.BackProperties.Specular.Green = 0
        Material.BackProperties.Specular.Blue = 0
        Material.BackProperties.Specular.Alpha = 1
        Material.FrontProperties.Ambient.Red = 0.200000002980232
        Material.FrontProperties.Ambient.Green = 0.200000002980232
        Material.FrontProperties.Ambient.Blue = 0.200000002980232
        Material.FrontProperties.Ambient.Alpha = 1
        Material.FrontProperties.Diffuse.Red = 0.800000011920929
        Material.FrontProperties.Diffuse.Green = 0.800000011920929
        Material.FrontProperties.Diffuse.Blue = 0.800000011920929
        Material.FrontProperties.Diffuse.Alpha = 1
        Material.FrontProperties.Emission.Red = 0
        Material.FrontProperties.Emission.Green = 0
        Material.FrontProperties.Emission.Blue = 0
        Material.FrontProperties.Emission.Alpha = 1
        Material.FrontProperties.Specular.Red = 0
        Material.FrontProperties.Specular.Green = 0
        Material.FrontProperties.Specular.Blue = 0
        Material.FrontProperties.Specular.Alpha = 1
        Material.Texture.EnvColor.Red = 0
        Material.Texture.EnvColor.Green = 0
        Material.Texture.EnvColor.Blue = 0
        Material.Texture.EnvColor.Alpha = 0
        Material.Texture.BorderColor.Red = 0
        Material.Texture.BorderColor.Green = 0
        Material.Texture.BorderColor.Blue = 0
        Material.Texture.BorderColor.Alpha = 0
        Material.Texture.Disabled = False
      end    
      item
        Name = 'BackgroundTex'
        Tag = 0
        Material.BackProperties.Ambient.Red = 0.200000002980232
        Material.BackProperties.Ambient.Green = 0.200000002980232
        Material.BackProperties.Ambient.Blue = 0.200000002980232
        Material.BackProperties.Ambient.Alpha = 1
        Material.BackProperties.Diffuse.Red = 0.800000011920929
        Material.BackProperties.Diffuse.Green = 0.800000011920929
        Material.BackProperties.Diffuse.Blue = 0.800000011920929
        Material.BackProperties.Diffuse.Alpha = 1
        Material.BackProperties.Emission.Red = 0
        Material.BackProperties.Emission.Green = 0
        Material.BackProperties.Emission.Blue = 0
        Material.BackProperties.Emission.Alpha = 1
        Material.BackProperties.Specular.Red = 0
        Material.BackProperties.Specular.Green = 0
        Material.BackProperties.Specular.Blue = 0
        Material.BackProperties.Specular.Alpha = 1
        Material.FrontProperties.Ambient.Red = 0.200000002980232
        Material.FrontProperties.Ambient.Green = 0.200000002980232
        Material.FrontProperties.Ambient.Blue = 0.200000002980232
        Material.FrontProperties.Ambient.Alpha = 1
        Material.FrontProperties.Diffuse.Red = 0.800000011920929
        Material.FrontProperties.Diffuse.Green = 0.800000011920929
        Material.FrontProperties.Diffuse.Blue = 0.800000011920929
        Material.FrontProperties.Diffuse.Alpha = 1
        Material.FrontProperties.Emission.Red = 0
        Material.FrontProperties.Emission.Green = 0
        Material.FrontProperties.Emission.Blue = 0
        Material.FrontProperties.Emission.Alpha = 1
        Material.FrontProperties.Specular.Red = 0
        Material.FrontProperties.Specular.Green = 0
        Material.FrontProperties.Specular.Blue = 0
        Material.FrontProperties.Specular.Alpha = 1
        Material.Texture.EnvColor.Red = 0
        Material.Texture.EnvColor.Green = 0
        Material.Texture.EnvColor.Blue = 0
        Material.Texture.EnvColor.Alpha = 0
        Material.Texture.BorderColor.Red = 0
        Material.Texture.BorderColor.Green = 0
        Material.Texture.BorderColor.Blue = 0
        Material.Texture.BorderColor.Alpha = 0
        Material.Texture.Disabled = False
      end    
      item
        Name = 'LibMaterial'
        Tag = 0
        Material.BackProperties.Ambient.Red = 0.200000002980232
        Material.BackProperties.Ambient.Green = 0.200000002980232
        Material.BackProperties.Ambient.Blue = 0.200000002980232
        Material.BackProperties.Ambient.Alpha = 1
        Material.BackProperties.Diffuse.Red = 0.800000011920929
        Material.BackProperties.Diffuse.Green = 0.800000011920929
        Material.BackProperties.Diffuse.Blue = 0.800000011920929
        Material.BackProperties.Diffuse.Alpha = 1
        Material.BackProperties.Emission.Red = 0
        Material.BackProperties.Emission.Green = 0
        Material.BackProperties.Emission.Blue = 0
        Material.BackProperties.Emission.Alpha = 1
        Material.BackProperties.Specular.Red = 0
        Material.BackProperties.Specular.Green = 0
        Material.BackProperties.Specular.Blue = 0
        Material.BackProperties.Specular.Alpha = 1
        Material.FrontProperties.Ambient.Red = 0.200000002980232
        Material.FrontProperties.Ambient.Green = 0.200000002980232
        Material.FrontProperties.Ambient.Blue = 0.200000002980232
        Material.FrontProperties.Ambient.Alpha = 1
        Material.FrontProperties.Diffuse.Red = 0.800000011920929
        Material.FrontProperties.Diffuse.Green = 0.800000011920929
        Material.FrontProperties.Diffuse.Blue = 0.800000011920929
        Material.FrontProperties.Diffuse.Alpha = 1
        Material.FrontProperties.Emission.Red = 0
        Material.FrontProperties.Emission.Green = 0
        Material.FrontProperties.Emission.Blue = 0
        Material.FrontProperties.Emission.Alpha = 1
        Material.FrontProperties.Specular.Red = 0
        Material.FrontProperties.Specular.Green = 0
        Material.FrontProperties.Specular.Blue = 0
        Material.FrontProperties.Specular.Alpha = 1
        Material.Texture.EnvColor.Red = 0
        Material.Texture.EnvColor.Green = 0
        Material.Texture.EnvColor.Blue = 0
        Material.Texture.EnvColor.Alpha = 0
        Material.Texture.BorderColor.Red = 0
        Material.Texture.BorderColor.Green = 0
        Material.Texture.BorderColor.Blue = 0
        Material.Texture.BorderColor.Alpha = 0
      end>
    left = 416
    top = 24
  end
  object GLScene1: TGLScene
    left = 440
    top = 88
    object Camera: TGLCamera
      TagFloat = 0
      DepthOfView = 100
      FocalLength = 50
      NearPlaneBias = 0.00100000004749745
      TargetObject = World
      CameraStyle = csInfinitePerspective
      Position.Coordinates = {
        0000A0400000A0410000C8420000803F
      }
    end
    object LightCube: TGLDummyCube
      TagFloat = 0
      PitchAngle = 0
      Position.Coordinates = {
        000096C30000A040000096430000803F
      }
      RollAngle = 0
      TurnAngle = 0
      OnProgress = LightCubeProgress
      CubeSize = 1
      EdgeColor.Red = 1
      EdgeColor.Green = 1
      EdgeColor.Blue = 1
      EdgeColor.Alpha = 1
      object GLLightSource1: TGLLightSource
        TagFloat = 0
        Ambient.Red = 0
        Ambient.Green = 0
        Ambient.Blue = 0
        Ambient.Alpha = 1
        ConstAttenuation = 1
        Diffuse.Red = 1
        Diffuse.Green = 1
        Diffuse.Blue = 1
        Diffuse.Alpha = 1
        LinearAttenuation = 0
        QuadraticAttenuation = 0
        LightStyle = lsOmni
        Specular.Red = 0
        Specular.Green = 0
        Specular.Blue = 0
        Specular.Alpha = 1
        SpotCutOff = 180
        SpotExponent = 0
      end
    end
    object LightCube2: TGLDummyCube
      TagFloat = 0
      PitchAngle = 0
      Position.Coordinates = {
        000096430000C842000096430000803F
      }
      RollAngle = 0
      TurnAngle = 0
      OnProgress = LightCube2Progress
      CubeSize = 1
      EdgeColor.Red = 1
      EdgeColor.Green = 1
      EdgeColor.Blue = 1
      EdgeColor.Alpha = 1
      object GLLightSource2: TGLLightSource
        TagFloat = 0
        Ambient.Red = 0.100000001490116
        Ambient.Green = 0.100000001490116
        Ambient.Blue = 0.100000001490116
        Ambient.Alpha = 1
        Ambient.Color = {
          CDCCCC3DCDCCCC3DCDCCCC3D0000803F
        }
        ConstAttenuation = 1
        Diffuse.Red = 0.917647004127502
        Diffuse.Green = 0.917647004127502
        Diffuse.Blue = 0.67843097448349
        Diffuse.Alpha = 1
        Diffuse.Color = {
          EAEA6A3FEAEA6A3FA7AD2D3F0000803F
        }
        LinearAttenuation = 0
        QuadraticAttenuation = 0
        LightStyle = lsParallel
        Specular.Red = 1
        Specular.Green = 0.5
        Specular.Blue = 0.5
        Specular.Alpha = 1
        Specular.Color = {
          0000803F0000003F0000003F0000803F
        }
        SpotCutOff = 180
        SpotExponent = 0
      end
    end
    object World: TGLDummyCube
      TagFloat = 0
      PitchAngle = 0
      RollAngle = 0
      TurnAngle = 0
      CubeSize = 1
      EdgeColor.Red = 1
      EdgeColor.Green = 1
      EdgeColor.Blue = 1
      EdgeColor.Alpha = 1
      object ScreenBackGround: TGLHUDSprite
        TagFloat = 0
        Material.MaterialLibrary = MaterialLibrary
        Material.LibMaterialName = 'BackgroundTex'
        PitchAngle = 0
        RollAngle = 0
        TurnAngle = 0
        Visible = False
        Width = 256
        Height = 256
        Rotation = 0
      end
      object Objects: TGLDummyCube
        TagFloat = 0
        PitchAngle = 0
        RollAngle = 0
        TurnAngle = 0
        CubeSize = 1
        EdgeColor.Red = 1
        EdgeColor.Green = 1
        EdgeColor.Blue = 1
        EdgeColor.Alpha = 1
        object GLSphere1: TGLSphere
          TagFloat = 0
          Material.MaterialLibrary = MaterialLibrary
          Material.LibMaterialName = 'ShaderMaterial'
          PitchAngle = 0
          RollAngle = 0
          TurnAngle = 0
          Visible = False
          Radius = 45
          Slices = 64
          Stacks = 64
        end
        object FreeForm: TGLFreeForm
          TagFloat = 0
          Material.BackProperties.Ambient.Red = 0.200000002980232
          Material.BackProperties.Ambient.Green = 0.200000002980232
          Material.BackProperties.Ambient.Blue = 0.200000002980232
          Material.BackProperties.Ambient.Alpha = 1
          Material.BackProperties.Diffuse.Red = 0.800000011920929
          Material.BackProperties.Diffuse.Green = 0.800000011920929
          Material.BackProperties.Diffuse.Blue = 0.800000011920929
          Material.BackProperties.Diffuse.Alpha = 1
          Material.BackProperties.Emission.Red = 0
          Material.BackProperties.Emission.Green = 0
          Material.BackProperties.Emission.Blue = 0
          Material.BackProperties.Emission.Alpha = 1
          Material.BackProperties.Specular.Red = 0
          Material.BackProperties.Specular.Green = 0
          Material.BackProperties.Specular.Blue = 0
          Material.BackProperties.Specular.Alpha = 1
          Material.FrontProperties.Ambient.Red = 0.200000002980232
          Material.FrontProperties.Ambient.Green = 0.200000002980232
          Material.FrontProperties.Ambient.Blue = 0.200000002980232
          Material.FrontProperties.Ambient.Alpha = 1
          Material.FrontProperties.Diffuse.Red = 0.800000011920929
          Material.FrontProperties.Diffuse.Green = 0.800000011920929
          Material.FrontProperties.Diffuse.Blue = 0.800000011920929
          Material.FrontProperties.Diffuse.Alpha = 1
          Material.FrontProperties.Emission.Red = 0
          Material.FrontProperties.Emission.Green = 0
          Material.FrontProperties.Emission.Blue = 0
          Material.FrontProperties.Emission.Alpha = 1
          Material.FrontProperties.Specular.Red = 0
          Material.FrontProperties.Specular.Green = 0
          Material.FrontProperties.Specular.Blue = 0
          Material.FrontProperties.Specular.Alpha = 1
          Material.Texture.EnvColor.Red = 0
          Material.Texture.EnvColor.Green = 0
          Material.Texture.EnvColor.Blue = 0
          Material.Texture.EnvColor.Alpha = 0
          Material.Texture.BorderColor.Red = 0
          Material.Texture.BorderColor.Green = 0
          Material.Texture.BorderColor.Blue = 0
          Material.Texture.BorderColor.Alpha = 0
          PitchAngle = 0
          RollAngle = 0
          TurnAngle = 0
          AutoCentering = [macCenterX, macCenterY, macCenterZ, macUseBarycenter]
          AutoScaling.Coordinates = {
            0000484200004842000048420000803F
          }
        end
        object GLTorus1: TGLTorus
          TagFloat = 0
          Material.BackProperties.Ambient.Red = 0.200000002980232
          Material.BackProperties.Ambient.Green = 0.200000002980232
          Material.BackProperties.Ambient.Blue = 0.200000002980232
          Material.BackProperties.Ambient.Alpha = 1
          Material.BackProperties.Diffuse.Red = 0.800000011920929
          Material.BackProperties.Diffuse.Green = 0.800000011920929
          Material.BackProperties.Diffuse.Blue = 0.800000011920929
          Material.BackProperties.Diffuse.Alpha = 1
          Material.BackProperties.Emission.Red = 0
          Material.BackProperties.Emission.Green = 0
          Material.BackProperties.Emission.Blue = 0
          Material.BackProperties.Emission.Alpha = 1
          Material.BackProperties.Specular.Red = 0
          Material.BackProperties.Specular.Green = 0
          Material.BackProperties.Specular.Blue = 0
          Material.BackProperties.Specular.Alpha = 1
          Material.FrontProperties.Ambient.Red = 0.200000002980232
          Material.FrontProperties.Ambient.Green = 0.200000002980232
          Material.FrontProperties.Ambient.Blue = 0.200000002980232
          Material.FrontProperties.Ambient.Alpha = 1
          Material.FrontProperties.Diffuse.Red = 0.800000011920929
          Material.FrontProperties.Diffuse.Green = 0.800000011920929
          Material.FrontProperties.Diffuse.Blue = 0.800000011920929
          Material.FrontProperties.Diffuse.Alpha = 1
          Material.FrontProperties.Emission.Red = 0
          Material.FrontProperties.Emission.Green = 0
          Material.FrontProperties.Emission.Blue = 0
          Material.FrontProperties.Emission.Alpha = 1
          Material.FrontProperties.Specular.Red = 0
          Material.FrontProperties.Specular.Green = 0
          Material.FrontProperties.Specular.Blue = 0
          Material.FrontProperties.Specular.Alpha = 1
          Material.Texture.EnvColor.Red = 0
          Material.Texture.EnvColor.Green = 0
          Material.Texture.EnvColor.Blue = 0
          Material.Texture.EnvColor.Alpha = 0
          Material.Texture.BorderColor.Red = 0
          Material.Texture.BorderColor.Green = 0
          Material.Texture.BorderColor.Blue = 0
          Material.Texture.BorderColor.Alpha = 0
          PitchAngle = 0
          RollAngle = 0
          TurnAngle = 0
          Visible = False
          MajorRadius = 40
          MinorRadius = 15
          Rings = 64
          Sides = 64
          StartAngle = 0
          StopAngle = 360
          Parts = [toSides, toStartDisk, toStopDisk]
        end
      end
    end
  end
  object Cadencer: TGLCadencer
    Scene = GLScene1
    Enabled = False
    MaxDeltaTime = 0
    MinDeltaTime = 0
    FixedDeltaTime = 0
    OnProgress = CadencerProgress
    left = 384
    top = 88
  end
  object ColorDialog: TColorDialog
    Color = clBlack
    CustomColors.Strings = (
      'ColorA=000000'
      'ColorB=000080'
      'ColorC=008000'
      'ColorD=008080'
      'ColorE=800000'
      'ColorF=800080'
      'ColorG=808000'
      'ColorH=808080'
      'ColorI=C0C0C0'
      'ColorJ=0000FF'
      'ColorK=00FF00'
      'ColorL=00FFFF'
      'ColorM=FF0000'
      'ColorN=FF00FF'
      'ColorO=FFFF00'
      'ColorP=FFFFFF'
      'ColorQ=C0DCC0'
      'ColorR=F0CAA6'
      'ColorS=F0FBFF'
      'ColorT=A4A0A0'
    )
    left = 381
    top = 153
  end
  object OpenPictureDialog: TOpenPictureDialog
    left = 472
    top = 160
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = Viewer
    FormCaption = 'ShaderLab - %FPS'
    KeyCombinations = <    
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end    
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end    
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    left = 361
    top = 207
  end
end
