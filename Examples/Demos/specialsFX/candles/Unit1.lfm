object Form1: TForm1
  Left = 229
  Top = 109
  Width = 382
  Height = 288
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 374
    Height = 230
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object TrackBar1: TTrackBar
    Left = 0
    Top = 230
    Width = 374
    Height = 29
    Hint = 'Wind'
    Align = alBottom
    Max = 20
    Min = -20
    Orientation = trHorizontal
    ParentShowHint = False
    Frequency = 5
    Position = 0
    SelEnd = 0
    SelStart = 0
    ShowHint = True
    TabOrder = 1
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBar1Change
  end
  object GLScene1: TGLScene
    Left = 64
    Top = 8
    object GLLightSource1: TGLLightSource
      Ambient.Color = {0000803F0000803F0000803F0000803F}
      ConstAttenuation = 1
      Position.Coordinates = {0000484200004842000048420000803F}
      SpotCutOff = 180
    end
    object DummyCube1: TGLDummyCube
      Position.Coordinates = {000000000000803F000000000000803F}
      CubeSize = 1
    end
    object RevolutionSolid1: TGLRevolutionSolid
      Direction.Coordinates = {2EBDFBB3000000000000803F00000000}
      Material.FrontProperties.Diffuse.Color = {0000803F8180003FC9C8C83E0000803F}
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      Nodes = <
        item
          Y = 0.5
        end
        item
          Y = 0.5
          Z = 2
        end
        item
          Y = 0.300000011920929
          Z = 2
        end
        item
          Y = -0.5
          Z = 2
        end
        item
          Y = -0.5
        end>
      Division = 9
      SplineMode = lsmCubicSpline
      Slices = 19
      object Candle: TGLCylinder
        Position.Coordinates = {000000000000A03F9A99993F0000803F}
        Material.FrontProperties.Diffuse.Color = {F1F0703FCBCA4A3FCBCA4A3F0000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        BottomRadius = 0.200000002980232
        Height = 1.5
        Slices = 12
        Stacks = 1
        TopRadius = 0.200000002980232
        object Lines1: TGLLines
          Position.Coordinates = {000000006666663F000000000000803F}
          LineColor.Color = {A9A5253FB1A8283EB1A8283E0000803F}
          LineWidth = 2
          Nodes = <
            item
              Y = -0.200000002980232
            end
            item
              X = 0.0500000007450581
              Z = 0.0500000007450581
            end
            item
              X = -0.0500000007450581
              Y = 0.100000001490116
              Z = -0.0500000007450581
            end>
          NodesAspect = lnaInvisible
          Options = []
          EffectsData = {
            0201060A54474C4246697265465802000610474C4669726546584D616E616765
            7231}
        end
        object DummyCube2: TGLDummyCube
          Direction.Coordinates = {F204353F00000000F40435BF00000000}
          CubeSize = 1
          object Plane1: TGLPlane
            Direction.Coordinates = {000000000000803F2CBD3BB300000000}
            Position.Coordinates = {0000C03FA4703DBF000000000000803F}
            Up.Coordinates = {000000B32FBD3BB3000080BF00000000}
            Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
            Material.FrontProperties.Diffuse.Color = {0000000000000000000000003333B33E}
            Material.BlendingMode = bmTransparency
            Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
            Height = 0.400000005960464
            Width = 3
          end
        end
      end
      object GLProxyObject1: TGLProxyObject
        MasterObject = Candle
        ProxyOptions = [pooEffects, pooObjects]
        Position.Coordinates = {0000803F0000A03F9A9919BF0000803F}
      end
      object GLProxyObject2: TGLProxyObject
        MasterObject = Candle
        ProxyOptions = [pooEffects, pooObjects]
        Position.Coordinates = {000080BF0000A03F9A9919BF0000803F}
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 100
      TargetObject = DummyCube1
      Position.Coordinates = {000070410000E040000040400000803F}
      Left = 192
      Top = 120
    end
  end
  object GLFireFXManager1: TGLFireFXManager
    FireDir.Coordinates = {000000009A99993E0000000000000000}
    InitialDir.Coordinates = {00000000CDCC4C3E0000000000000000}
    Cadencer = GLCadencer1
    MaxParticles = 128
    ParticleSize = 0.150000005960464
    FireDensity = 0.600000023841858
    FireEvaporation = 0.860000014305115
    ParticleLife = 2
    FireBurst = 1
    FireRadius = 0.100000001490116
    Disabled = False
    Paused = False
    ParticleInterval = 0.0399999991059303
    UseInterval = True
    Left = 64
    Top = 48
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 64
    Top = 88
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 8
  end
end
