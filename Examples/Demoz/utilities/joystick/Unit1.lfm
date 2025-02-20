object Form1: TForm1
  Left = 221
  Top = 107
  Width = 267
  Height = 234
  BorderWidth = 3
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
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 253
    Height = 199
    Camera = GLCamera1
    Buffer.BackgroundColor = clBtnShadow
    Align = alClient
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000A0C000002041000070410000803F}
      SpotCutOff = 180
    end
    object Cube1: TGLCube
      Direction.Coordinates = {D9B35D3FEE83043EE646F7BE00000000}
      Position.Coordinates = {000080BF000000C0000000000000803F}
      Scale.Coordinates = {00000040000000400000004000000000}
      ShowAxes = True
      Up.Coordinates = {B85863B2EA46773FEF83843E00000000}
      Material.FrontProperties.Diffuse.Color = {A9A5253FB1A8283EB1A8283E0000803F}
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      CubeSize = {000000400000003F00000040}
      object DummyCube1: TGLDummyCube
        Position.Coordinates = {000000000000803E000000000000803F}
        CubeSize = 1
        object Cylinder1: TGLCylinder
          Position.Coordinates = {000000000000803F000000000000803F}
          Material.FrontProperties.Ambient.Color = {8D8C0C3F8A89093FD1D0D03D0000803F}
          Material.FrontProperties.Diffuse.Color = {F9F8783FE1E0E03DC1C0403C0000803F}
          Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
          BottomRadius = 0.200000002980232
          Height = 2
          Stacks = 1
          TopRadius = 0.200000002980232
        end
      end
    end
    object DummyCube2: TGLDummyCube
      Position.Coordinates = {0000904000000000000000000000803F}
      CubeSize = 1
      object Sphere1: TGLSphere
        Position.Coordinates = {0000000000004040000000000000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Radius = 0.5
      end
      object Sphere2: TGLSphere
        Position.Coordinates = {000000000000803F000000000000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Radius = 0.5
      end
      object Sphere3: TGLSphere
        Position.Coordinates = {00000000000080BF000000000000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Radius = 0.5
      end
      object Sphere4: TGLSphere
        Position.Coordinates = {00000000000040C0000000000000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Radius = 0.5
      end
    end
    object DummyCube3: TGLDummyCube
      CubeSize = 1
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = DummyCube3
      Position.Coordinates = {00000000000000000000A0410000803F}
    end
  end
  object Joystick1: TJoystick
    Capture = True
    JoystickID = jidJoystick1
    Threshold = 500
    OnJoystickButtonChange = Joystick1JoystickButtonChange
    Left = 8
    Top = 40
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 72
  end
end
