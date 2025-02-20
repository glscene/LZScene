object Form1: TForm1
  Left = 175
  Top = 114
  Width = 400
  Height = 293
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 392
    Height = 264
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Align = alClient
  end
  object ScreenSaver1: TScreenSaver
    OnPropertiesRequested = ScreenSaver1PropertiesRequested
    Left = 48
    Top = 8
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object DummyCube1: TGLDummyCube
      CubeSize = 1
      BehavioursData = {
        0201060B54474C42496E657274696102000200050000000000000080FF3F0200
        0805000000000000008404C005000000000000009002400500000000000000C0
        004008020008020008}
      object DummyCube2: TGLDummyCube
        Position.Coordinates = {0000000000000000000040400000803F}
        CubeSize = 1
        BehavioursData = {
          0201060B54474C42496E657274696102000200050000000000000080FF3F0200
          080500000000000000C000400500000000000000000000050000000000000098
          034008020008020008}
        object DummyCube3: TGLDummyCube
          Position.Coordinates = {000000000000803F000000000000803F}
          CubeSize = 1
          BehavioursData = {
            0201060B54474C42496E657274696102000200050000000000000080FF3F0200
            08050000000000000080FF3F0500000000000000C00040050000000000000080
            034008020008020008}
          object Torus1: TGLTorus
            Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
            MajorRadius = 1
            MinorRadius = 0.300000011920929
            Rings = 32
            Sides = 32
            BehavioursData = {
              0201060B54474C42496E657274696102000200050000000000000080FF3F0200
              0805000000000000000000000500000000000000D80340050000000000000000
              000008020008020008}
          end
        end
      end
    end
    object DummyCube4: TGLDummyCube
      CubeSize = 1
      BehavioursData = {
        0201060B54474C42496E657274696102000200050000000000000080FF3F0200
        080500000000000000A0014005000000000000000000000500000000000000C0
        004008020008020008}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1
        Diffuse.Color = {0000803F00000000000000000000803F}
        Position.Coordinates = {0000704100000000000000000000803F}
        SpotCutOff = 180
      end
      object GLLightSource2: TGLLightSource
        ConstAttenuation = 1
        Diffuse.Color = {9594943E9594943E0000803F0000803F}
        Position.Coordinates = {0000E0C0000000000000E0400000803F}
        SpotCutOff = 180
      end
      object GLLightSource3: TGLLightSource
        ConstAttenuation = 1
        Diffuse.Color = {0000803F0000803F000000000000803F}
        Position.Coordinates = {0000E0C0000000000000E0C00000803F}
        SpotCutOff = 180
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = DummyCube1
      Position.Coordinates = {0000204100000000000000000000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 1
    Left = 88
    Top = 8
  end
end
