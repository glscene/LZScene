unit uGlobal;


interface

uses
  Classes,
  SysUtils,
  Printers,
  OpenGLTokens,
  Graphics,
  GLVectorGeometry,
  GLCoordinates,
  GLVectorLists,
  GLColor;

const
  NewFName: TFileName = 'New Graph';

  PosFloat: Set of Char = ['0'..'9','e','E','.', #8];
  AnyFloat: Set of Char = ['-', '0'..'9','e','E', '.', #8];

type
  TLayout = record
    IsMaximize: Boolean;  { is true if ViewForm.WindowState = wsMaximized }
    MainLeft: integer;
    MainTop: integer;
    MainWidth: integer;
    MainHeight: integer;
    CurrentGraphFName: ShortString;
    CurrentDataPath: ShortString;
    CurrentImagePath: ShortString;
    FuncLeft: integer;
    FuncTop: integer;
    FuncWidth: integer;
    FuncHeight: integer;
    GridsVisible: Boolean;
    GridsLeft: integer;
    GridsTop: integer;
    GridColorsLeft: integer;
    GridColorsTop: integer;
    PlotColorsLeft: integer;
    PlotColorsTop: integer;
    EvaluateVisible: Boolean;
    EvaluateLeft: integer;
    EvaluateTop: integer;
    CoordVisible: Boolean;
    CoordLeft: integer;
    CoordTop: integer;
    DerivLeft: integer;
    DerivTop: integer;
    AddColorsLeft: integer;
    AddColorsTop: integer;
    PointCount: integer;
  end;

  TViewMode = (vmAmbient, vmAmbientandDiffuse, vmDiffuse, vmEmmision, vmNone);
  TViewModes = set of TViewMode;

  TfxyMode = (fxyFill, fxyLines, fxyPoints);
  TfxyModes = set of TfxyMode;

  TAddedType = (AddNone, AddDerivX, AddDerivY, AddVolume);

  TRange = record
    Maximum: TGLFloat;
    Minimum: TGLFloat;
    Step: TGlFloat;
  end;

  TxyGrid = record
    Color: TColorVector;
    xRange, yRange: TRange;
    zPosition: TGLFloat;  { unscaled }
    zScale: TGLFloat;     { actual zPosition = zPosition*zScale }
    IsVisible: Boolean;
    IsChecked: Boolean;
  end;

  TxzGrid = record
    Color: TColorVector;
    xRange, zRange: TRange;
    yPosition: TGLFloat;
    IsVisible: Boolean;
    IsChecked: Boolean;
  end;

  TyzGrid = record
    Color: TColorVector;
    yRange, zRange: TRange;
    xPosition: TGLFloat;
    IsVisible: Boolean;
    IsChecked: Boolean;
  end;

  TViewData = record
    CameraCubeAt: TVector;
    CameraAt: TVector;
    fLength: TGLFloat;
    LightAt: TVector;
    ViewDepth: TGLFloat;
    BackColor: TColor;

    xyGrid: TxyGrid;
    xzGrid: TxzGrid;
    yzGrid: TyzGrid;

    xEvaluate: TGLFloat;
    yEvaluate: TGLFloat;
    CoordChecked: Boolean;
    ToGridsChecked: Boolean;
    dzdx_dyChecked: Boolean;
    BoxChecked: Boolean;
    CoordWidth: integer;
    CoordColor: TColor;
    BoxLnWidth: integer;
    BoxLnColor: TColorVector;

    TextVisible: Boolean;
    TextFontN: ShortString;
    TextFontSz: integer;
    xPosYMax: Boolean;
    xPosZMax: Boolean;
    xTextColor: TColor;

    yPosXMax: Boolean;
    yPosZMax: Boolean;
    yTextColor: TColor;

    zPosXMax: Boolean;
    zPosYMax: Boolean;
    zTextColor: TColor;
    ArrowSize: TGLFloat;
  end;

  TPlotData = record
    fxyStr: string;                       { function string e.g. 'sin(x*y)' }
    txtStr: string;
    NoteStr: string;
    xMin, xMax, xInc: TGLFloat;           { x limits and increment }
    yMin, yMax, yInc: TGLFloat;           { y limits and increment }
    zMin, zMax: TGLFloat;                 { z limits }
    zLim: Boolean;                        { limits upper & lower z }
    zCap: Boolean;                        { caps the above z }

    UpperColor, LowerColor: TColorVector;
    ColorBlend: TGLFloat;
    ColorMove: TGLFloat;
    ViewMode: TViewMode;
    fxyMode: TfxyMode;
  end;

  TPlotDataObject = class(TObject)        { data object stored in listbox }
    constructor Create(D: TPlotData);
    destructor Destroy; override;
  public
    Data: TPlotData;
  end;

  TAddedData = record
    xMin, xMax, xInc: TGLFloat;           { x limits and increment }
    yMin, yMax, yInc: TGLFloat;           { y limits and increment }
    zMin, zMax: TGLFloat;                 { z limits }
    zLim: Boolean;                        { limits upper & lower z }
    zCap: Boolean;                        { caps the above z }

    UpperColor, LowerColor: TColorVector;
    ColorBlend: TGLFloat;
    ColorMove: TGLFloat;
    ViewMode: TViewMode;
    fxyMode: TfxyMode;
    AddLineWidth: integer;
    AddLineColor: integer;
    AddedAs: TAddedType;
  end;

  TVolumeLineObject = class(TObject)
    constructor Create(xT, yT, zT, xE, yE, zE, Vol: TGLFloat);
    destructor Destroy; override;
  public
    x1, y1, z1, x2, y2, z2, v: TGLFloat;
  end;

var
  Altered: Boolean;   { any alteration to the plot }
  GridColorsAltered: Boolean;
  DerivativeAltered: Boolean;

  MainPath: TFileName;
  DataPath: TFileName;
  ImagePath: TFileName;
  GraphFName: TFileName;
  LayoutFName: TFileName;
  RecentFName: TFileName;
  Layout: TLayout;
  ViewData: TViewData;
  PlotData: TPlotData;
  AddedData: TAddedData;

implementation

constructor TPlotDataObject.Create(D: TPlotData);
begin
  inherited Create;
  Data := D;
end;

destructor TPlotDataObject.Destroy;
begin
  inherited Destroy;
end;

constructor TVolumeLineObject.Create(xT, yT, zT, xE, yE, zE, Vol: TGLFloat);
begin
  x1 := xT;
  y1 := yT;
  z1 := zT;
  x2 := xE;
  y2 := yE;
  z2 := zE;
  v := Vol;
end;

destructor TVolumeLineObject.Destroy;
begin
  inherited Destroy;
end;

end.
