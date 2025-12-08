object Form2: TForm2
  Left = 209
  Top = 124
  BorderStyle = bsNone
  BorderWidth = 2
  Caption = 'Form2'
  ClientHeight = 223
  ClientWidth = 368
  Color = clBtnShadow
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
    Width = 368
    Height = 223
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object Button1: TButton
    Left = 280
    Top = 184
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object Button2: TButton
    Left = 144
    Top = 188
    Width = 97
    Height = 19
    Caption = 'Define Password'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 2
    OnClick = Button2Click
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {000048C200004842000048420000803F}
      SpotCutOff = 180
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1
      object SpaceText4: TGLSpaceText
        Position.Coordinates = {0000A0C000008040000000000000803F}
        Scale.Coordinates = {00000040000000400000803F00000000}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803FD1D0D03D0000803F}
        Material.FrontProperties.Emission.Color = {14AE073F8FC2F53DD7A3F03E0000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Extrusion = 1
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        Text = 'Screen Saver'
        AllowedDeviation = 1
        CharacterRange = stcrAlphaNum
      end
      object Torus1: TGLTorus
        Tag = 1
        Direction.Coordinates = {FFFFFF3E71C41C3F71C41C3F00000000}
        Position.Coordinates = {000080C000000000000000000000803F}
        Scale.Coordinates = {00000040000000400000004000000000}
        Up.Coordinates = {EA5EAA32F304353FF30435BF00000000}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        MajorRadius = 1
        MinorRadius = 0.300000011920929
        Rings = 8
        Sides = 6
        BehavioursData = {
          0201060B54474C42496E657274696102000200050000000000000080FF3F0200
          080500000000000000F003400500000000000000000000050000000000000000
          000009020008020008}
      end
      object Torus2: TGLTorus
        Tag = 2
        Direction.Coordinates = {00000000F304353FF304353F00000000}
        Position.Coordinates = {0000804000000000000000000000803F}
        Scale.Coordinates = {00000040000000400000004000000000}
        Up.Coordinates = {00000000F304353FF30435BF00000000}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        MajorRadius = 1
        MinorRadius = 0.300000011920929
        BehavioursData = {
          0201060B54474C42496E657274696102000200050000000000000080FF3F0200
          080500000000000000C803400500000000000000000000050000000000000000
          000009020008020008}
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = DummyCube1
      Position.Coordinates = {000000000000803F0000A0410000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 1
    Left = 40
    Top = 8
  end
end
