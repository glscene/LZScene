object Form1: TForm1
  Left = 228
  Top = 98
  Width = 344
  Height = 303
  BorderWidth = 3
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 330
    Height = 268
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 8
    Top = 8
    object Torus1: TGLTorus
      Material.FrontProperties.Ambient.Color = {9A99193E9A99193E9A99193E0000803F}
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      MajorRadius = 3
      MinorRadius = 0.200000002980232
      Rings = 36
      Sides = 9
      BehavioursData = {
        0201060B54474C42496E657274696102000200050000000000000080FF3F0200
        08050000000000000000000005000000000000000000000500000000000000F0
        034009020008020008}
    end
    object Sphere1: TGLSphere
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      Radius = 0.300000011920929
      Slices = 6
      Stacks = 6
      EffectsData = {
        0201060A54474C4246697265465802000610474C4669726546584D616E616765
        7231}
      object GLLightSource2: TGLLightSource
        Ambient.Color = {0000803F0000803F0000803F0000803F}
        ConstAttenuation = 1
        Diffuse.Color = {0000803F8180003F000000000000803F}
        Position.Coordinates = {000000000000003F000000000000803F}
        SpotCutOff = 180
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = Sphere1
      Position.Coordinates = {00000041000000400000A0400000803F}
      Left = 152
      Top = 104
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 40
    Top = 8
  end
  object GLFireFXManager1: TGLFireFXManager
    FireDir.Coordinates = {000000000000803F0000000000000000}
    InitialDir.Coordinates = {00000000000000000000000000000000}
    Cadencer = GLCadencer1
    MaxParticles = 96
    ParticleSize = 0.699999988079071
    FireDensity = 0.5
    FireEvaporation = 0.860000014305115
    FireBurst = 1
    FireRadius = 0.5
    Disabled = False
    Paused = False
    ParticleInterval = 0.00999999977648258
    UseInterval = True
    Left = 72
    Top = 8
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 8
    Top = 40
  end
end
