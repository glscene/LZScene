object ViewForm: TViewForm
  Left = 69
  Top = 77
  ClientHeight = 790
  ClientWidth = 1098
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDesigned
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object GLSViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 1098
    Height = 771
    Cursor = crHandPoint
    Camera = Camera
    Buffer.BackgroundColor = clActiveCaption
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow]
    FieldOfView = 125.158821105957000000
    Align = alClient
    OnMouseDown = GLSViewerMouseDown
    OnMouseMove = GLSViewerMouseMove
    OnMouseUp = GLSViewerMouseUp
    TabOrder = 0
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 771
    Width = 1098
    Height = 19
    Cursor = crHandPoint
    Color = clCream
    Panels = <
      item
        Alignment = taCenter
        Width = 176
      end
      item
        Alignment = taCenter
        Width = 176
      end
      item
        Alignment = taCenter
        Width = 176
      end
      item
        Alignment = taCenter
        Width = 176
      end
      item
        Alignment = taCenter
        Width = 50
      end>
  end
  object GLScene1: TGLScene
    Left = 64
    Top = 82
    object GLLight: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000484200004842000048420000803F}
      LightStyle = lsOmni
      Specular.Color = {0000803F0000803F0000803F0000803F}
      SpotCutOff = 180.000000000000000000
      SpotDirection.Coordinates = {00000000000000000000000000000000}
    end
    object TargetCube: TGLDummyCube
      Scale.Coordinates = {00000040000000400000004000000000}
      Pickable = False
      CubeSize = 2.000000000000000000
      EdgeColor.Color = {0000803FAE47E13D7B142E3F0000803F}
    end
    object CameraCube: TGLDummyCube
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {CDCC0C3FD7A3F03E295C0F3E0000803F}
      object Camera: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 200.000000000000000000
        NearPlaneBias = 0.009999999776482582
        TargetObject = TargetCube
        Position.Coordinates = {00004842000048420000F0410000803F}
        Direction.Coordinates = {000000000000803F0000008000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
      end
    end
    object GLxyGrid: TGLXYZGrid
      Direction.Coordinates = {00000000000000800000803F00000000}
      LineColor.Color = {00000000000000000000803F0000803F}
      XSamplingScale.Max = 4.000000000000000000
      XSamplingScale.Step = 1.000000000000000000
      YSamplingScale.Max = 4.000000000000000000
      YSamplingScale.Step = 1.000000000000000000
      ZSamplingScale.Step = 1.000000000000000000
    end
    object GLxzGrid: TGLXYZGrid
      Direction.Coordinates = {00000000000000800000803F00000000}
      LineColor.Color = {000000000000003F000000000000803F}
      XSamplingScale.Max = 4.000000000000000000
      XSamplingScale.Step = 1.000000000000000000
      YSamplingScale.Step = 0.100000001490116100
      ZSamplingScale.Max = 4.000000000000000000
      ZSamplingScale.Step = 1.000000000000000000
      Parts = [gpX, gpZ]
    end
    object GLyzGrid: TGLXYZGrid
      LineColor.Color = {0000803F00000000000000000000803F}
      XSamplingScale.Step = 0.100000001490116100
      YSamplingScale.Max = 4.000000000000000000
      YSamplingScale.Step = 1.000000000000000000
      ZSamplingScale.Max = 4.000000000000000000
      ZSamplingScale.Step = 1.000000000000000000
      Parts = [gpY, gpZ]
    end
    object Fields: TGLDummyCube
      Direction.Coordinates = {00000000000000800000803F00000000}
      CubeSize = 1.000000000000000000
    end
    object xCoordLine: TGLLines
      LineColor.Color = {0000803F00000000000000000000803F}
      LineWidth = 3.000000000000000000
      Nodes = <
        item
        end
        item
        end>
      NodesAspect = lnaInvisible
      Options = []
    end
    object yCoordLine: TGLLines
      LineColor.Color = {0000803F00000000000000000000803F}
      LineWidth = 3.000000000000000000
      Nodes = <
        item
        end
        item
        end>
      NodesAspect = lnaInvisible
      Options = []
    end
    object zCoordLine: TGLLines
      LineColor.Color = {0000803F00000000000000000000803F}
      LineWidth = 3.000000000000000000
      Nodes = <
        item
        end
        item
        end>
      NodesAspect = lnaInvisible
      Options = []
    end
    object AddXLine: TGLLines
      LineColor.Color = {0000803F00000000000000000000803F}
      LineWidth = 3.000000000000000000
      Nodes = <
        item
        end
        item
        end>
      NodesAspect = lnaInvisible
      Options = []
    end
    object AddYLine: TGLLines
      LineColor.Color = {0000803F00000000000000000000803F}
      LineWidth = 3.000000000000000000
      Nodes = <
        item
        end
        item
        end>
      NodesAspect = lnaInvisible
      Options = []
    end
    object AddZLine: TGLLines
      LineColor.Color = {0000803F00000000000000000000803F}
      LineWidth = 3.000000000000000000
      Nodes = <
        item
        end
        item
        end>
      NodesAspect = lnaInvisible
      Options = []
    end
    object BoxLine1: TGLLines
      Nodes = <
        item
        end
        item
        end
        item
        end
        item
        end
        item
        end
        item
        end
        item
        end
        item
        end
        item
        end
        item
        end>
      NodesAspect = lnaInvisible
      Options = []
    end
    object BoxLine2: TGLLines
      Nodes = <
        item
        end
        item
        end>
      NodesAspect = lnaInvisible
      Options = []
    end
    object BoxLine3: TGLLines
      Nodes = <
        item
        end
        item
        end>
      NodesAspect = lnaInvisible
      Options = []
    end
    object BoxLine4: TGLLines
      Nodes = <
        item
        end
        item
        end>
      NodesAspect = lnaInvisible
      Options = []
    end
    object XCoordsCube: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object YCoordsCube: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object ZCoordsCube: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object xArrow: TGLArrowLine
      Visible = False
      BottomRadius = 0.100000001490116100
      Height = 1.000000000000000000
      TopRadius = 0.100000001490116100
      TopArrowHeadHeight = 0.500000000000000000
      TopArrowHeadRadius = 0.200000002980232200
      BottomArrowHeadHeight = 0.500000000000000000
      BottomArrowHeadRadius = 0.200000002980232200
    end
    object yArrow: TGLArrowLine
      Visible = False
      BottomRadius = 0.100000001490116100
      Height = 1.000000000000000000
      TopRadius = 0.100000001490116100
      TopArrowHeadHeight = 0.500000000000000000
      TopArrowHeadRadius = 0.200000002980232200
      BottomArrowHeadHeight = 0.500000000000000000
      BottomArrowHeadRadius = 0.200000002980232200
    end
    object AddedField: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object VolumeLines: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
  end
  object GLWinBmpFont: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -27
    Font.Name = 'Tahoma'
    Font.Style = []
    MagFilter = maNearest
    MinFilter = miNearest
    Left = 64
    Top = 32
  end
  object MainMenu: TMainMenu
    AutoHotkeys = maManual
    OwnerDraw = True
    Left = 160
    Top = 33
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Caption = 'New'
        OnClick = New1Click
      end
      object Open1: TMenuItem
        Caption = 'Open...'
        OnClick = Open1Click
      end
      object Recent1: TMenuItem
        Caption = 'Recent'
      end
      object Save1: TMenuItem
        Caption = 'Save'
        OnClick = Save1Click
      end
      object Saveas1: TMenuItem
        Caption = 'Save as...'
        OnClick = Saveas1Click
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object DefaultLayout1: TMenuItem
        Caption = 'Default Layout'
        OnClick = DefaultLayout1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Grid1: TMenuItem
        Caption = 'Grids...'
        OnClick = Grid1Click
      end
      object CoordText1: TMenuItem
        Caption = 'Grids Coordinates...'
        OnClick = CoordText1Click
      end
      object GridColours1: TMenuItem
        Caption = 'Grids Colours...'
        OnClick = GridColours1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Evaluate1: TMenuItem
        Caption = 'Evaluate Function...'
        OnClick = Evaluate1Click
      end
      object PlotColours1: TMenuItem
        Caption = 'Plot Function Colours...'
        OnClick = PlotColours1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object DerivativeOps: TMenuItem
        Caption = 'Partial Derivative or Double Integral...'
        OnClick = DerivativeOpsClick
      end
      object DerivativePlotColours1: TMenuItem
        Caption = 'Derivative or Double Integral Plot Colours...'
        Enabled = False
        OnClick = DerivativePlotColours1Click
      end
    end
  end
end
