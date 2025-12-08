object Form1: TForm1
  Left = 155
  Top = 94
  Width = 527
  Height = 341
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
    Width = 519
    Height = 312
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object Button1: TButton
    Left = 16
    Top = 16
    Width = 35
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = Button1Click
  end
  object GLScene1: TGLScene
    Left = 64
    Top = 8
    object DummyCube1: TGLDummyCube
      Position.Coordinates = {0000000000007041000000000000803F}
      CubeSize = 1
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000484200004842000048420000803F}
      SpotCutOff = 180
    end
    object Sphere1: TGLSphere
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      Radius = 0.5
      Slices = 9
      Stacks = 6
      BehavioursData = {
        0201060B54474C42496E657274696102000200050000000000000080FF3F0200
        0805000000000000000000000500000000000000000000050000000000000000
        000009020008020008}
      EffectsData = {
        0202060A54474C4246697265465802000606466972654658020002000607536D
        6F6B654658}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 150
      FocalLength = 50
      TargetObject = DummyCube1
      Position.Coordinates = {000048420000A041000070410000803F}
      Left = 248
      Top = 144
    end
  end
  object FireFX: TGLFireFXManager
    FireDir.Coordinates = {00000000000000000000000000000000}
    InitialDir.Coordinates = {00000000000000000000000000000000}
    Cadencer = GLCadencer1
    MaxParticles = 512
    ParticleSize = 0.5
    FireDensity = 0.600000023841858
    FireEvaporation = 0.860000014305115
    ParticleLife = 1
    FireRadius = 0.5
    Disabled = False
    Paused = False
    ParticleInterval = 0.00999999977648258
    UseInterval = True
    Reference = Sphere1
    Left = 64
    Top = 56
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Enabled = False
    OnProgress = GLCadencer1Progress
    Left = 64
    Top = 136
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 64
    Top = 216
  end
  object SmokeFX: TGLFireFXManager
    FireDir.Coordinates = {000000000000803F0000000000000000}
    InitialDir.Coordinates = {000000000000003F0000000000000000}
    Cadencer = GLCadencer1
    MaxParticles = 64
    ParticleSize = 2
    InnerColor.Color = {0000803E0000803E0000803E0000803F}
    OuterColor.Color = {0000000000000000000000000000803F}
    FireDensity = 0.600000023841858
    FireEvaporation = 0.860000014305115
    FireRadius = 1
    Disabled = True
    Paused = False
    ParticleInterval = 0.0700000002980232
    UseInterval = False
    Reference = Sphere1
    Left = 64
    Top = 88
  end
end
