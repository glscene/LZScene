object Form1: TForm1
  Left = 209
  Top = 92
  Width = 483
  Height = 391
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 66
    Width = 467
    Height = 260
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.AntiAliasing = aaNone
    FieldOfView = 137.924972534179700000
    Align = alClient
    TabOrder = 0
  end
  object TrackBar: TTrackBar
    Left = 0
    Top = 326
    Width = 467
    Height = 27
    Align = alBottom
    Max = 180
    Min = -180
    PageSize = 45
    Frequency = 45
    TabOrder = 1
    ThumbLength = 10
    TickMarks = tmBoth
    OnChange = TrackBarChange
  end
  object TrackBar1: TTrackBar
    Left = 0
    Top = 33
    Width = 467
    Height = 33
    Align = alTop
    Max = 50
    Min = -50
    PageSize = 45
    Frequency = 10
    TabOrder = 2
    ThumbLength = 10
    TickMarks = tmBoth
    OnChange = TrackBar1Change
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 467
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      467
      33)
    object Button1: TButton
      Left = 384
      Top = 4
      Width = 83
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Chimes (WAV)'
      TabOrder = 0
      OnClick = Button1Click
    end
    object btnHowl: TButton
      Left = 295
      Top = 2
      Width = 83
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Howl (MP3)'
      Enabled = False
      TabOrder = 1
      OnClick = btnHowlClick
    end
  end
  object GLSMOpenAL: TGLSMOpenAL
    Active = True
    MaxChannels = 32
    MasterVolume = 1.000000000000000000
    Listener = Mickey
    Sources = <>
    Cadencer = GLCadencer1
    Environment = seAuditorium
    Left = 72
    Top = 144
  end
  object GLSoundLibrary: TGLSoundLibrary
    Samples = <>
    Left = 8
    Top = 104
  end
  object GLScene: TGLScene
    Left = 8
    Top = 64
    object DummyCube: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Torus1: TGLTorus
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Emission.Color = {000000008180803E8180003F0000803F}
        Direction.Coordinates = {000000000000803F2EBD3BB300000000}
        Position.Coordinates = {00000000000000BF000000000000803F}
        Scale.Coordinates = {0000803F0000803F0000003F00000000}
        Up.Coordinates = {000000002EBD3BB3000080BF00000000}
        MajorRadius = 5.000000000000000000
        MinorRadius = 0.100000001490116100
        Rings = 16
        Sides = 3
      end
      object Mickey: TGLSphere
        Position.Coordinates = {000000000000003F000000000000803F}
        Radius = 0.500000000000000000
        Slices = 9
        Stacks = 9
        object Sphere2: TGLSphere
          Position.Coordinates = {CDCCCC3ECDCC4C3E000000000000803F}
          Radius = 0.300000011920929000
          Slices = 6
          Stacks = 6
        end
        object Sphere3: TGLSphere
          Position.Coordinates = {CDCCCCBECDCC4C3E000000000000803F}
          Radius = 0.300000011920929000
          Slices = 6
          Stacks = 6
        end
        object Cone1: TGLCone
          Direction.Coordinates = {2EF964BF2EF9E43E0000000000000000}
          Position.Coordinates = {00000000000000000000003F0000803F}
          Up.Coordinates = {00000000000000000000803F00000000}
          BottomRadius = 0.300000011920929000
          Height = 0.500000000000000000
          Slices = 8
          Stacks = 2
          Parts = [coSides]
        end
      end
      object Plane1: TGLPlane
        Direction.Coordinates = {000000000000803F2EBD3BB300000000}
        Position.Coordinates = {00000000000000BF000000000000803F}
        Up.Coordinates = {000000002EBD3BB3000080BF00000000}
        Height = 10.000000000000000000
        Width = 0.500000000000000000
      end
    end
    object Sphere: TGLSphere
      Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
      Material.FrontProperties.Emission.Color = {8180003F00000000000000000000803F}
      Position.Coordinates = {000000400000003F000000000000803F}
      OnProgress = SphereProgress
      Radius = 0.500000000000000000
      Slices = 9
      Stacks = 9
      BehavioursData = {
        0201061054474C42536F756E64456D6974746572020002000200020005000000
        0000000080FF3F050000000000000080FF3F0500000000000000C80540050000
        0000000000B407400500000000000000B407400500000000000000000000060E
        474C536F756E644C696272617279060C6472756D6C6F6F702E77617608080203
        09}
      object Disk1: TGLDisk
        Direction.Coordinates = {000000000000803F2EBD3BB300000000}
        Position.Coordinates = {00000000000080BF000000000000803F}
        Up.Coordinates = {000000002EBD3BB3000080BF00000000}
        OuterRadius = 0.500000000000000000
        Slices = 12
        SweepAngle = 360.000000000000000000
      end
    end
    object GLLightSource: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000204100002041000020410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Plane1
      Position.Coordinates = {000000400000A040000020410000803F}
      Left = 256
      Top = 160
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene
    SleepLength = 1
    Left = 8
    Top = 184
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 8
    Top = 224
  end
end
