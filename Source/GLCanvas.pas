//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Implements a basic Canvas-like interface over for OpenGL.
   This class can be used for generic OpenGL applications and has no dependencies
   to the GLScene core units (only to base units).

  History :  
       10/11/12 - PW - Added CPP compatibility: changed vector arrays to records,
                          Replaced direct access to PenAlpha property with GetPenAlpha method
       05/02/11 - Yar - Now PenColor setter always direct set color
       03/10/10 - Yar - Added RoundRect (thanks eric129)
       21/09/10 - Yar - Added Arc, ArcTo (thanks µAlexx)
       03/09/10 - Yar - Added FillRectGradient, FillEllipseGradient (thanks µAlexx)
       23/08/10 - Yar - Replaced OpenGL1x functions to OpenGLAdapter
       04/04/10 - Yar - Fixes after GLState revision
       07/11/09 - DaStr - Some cosmetic fixes. Overloaded TGLCanvas.EllipseBB(),
                             TGLCanvas.Ellipse(), TGLCanvas.FillEllipse()
       31/07/07 - DaStr - Added missing StopPrimitive call to TGLCanvas.FillRect
                             (Bugtracker ID = 1775528)
       06/06/07 - DaStr - Removed ConvertColorVector and ConvertWinColor (now in GLColor.pas)
                             Added GLColor to uses (BugtrackerID = 1732211)
                             Removed TColor declaration (now in GLCrossPlatform.pas)
       02/08/04 - LR, YHC - BCB corrections: used record instead array
                               Replaced direct access of some properties by a getter and a setter
       08/07/04 - LR - Replace Graphics and TPoint by GLCrossPlatform for Linux
       13/01/04 - EG - Polyline/Polygon fix
       07/05/03 - EG - SetPenWidth now correctly stops the primitive
       08/01/03 - EG - StopPrimitive now public
       09/12/02 - EG - Now properly disables fog
       20/11/02 - EG - Now uses Types/Windows TPoint (D5 & D6 tested only)
       01/10/02 - EG - Added Polygon & Polyline
       04/03/02 - EG - Added FrameRect and FillRect
       31/01/02 - EG - Texture3D/CubeMap only disabled if supported
       24/01/02 - EG - Added PenAlpha
       19/01/02 - EG - Creation
  
}
unit GLCanvas;

interface

{$I GLScene.inc}

uses
  Classes,
  Graphics,
  GLVectorGeometry,
  GLColor,
  GLCrossPlatform,
  GLState;

type

  TArcDirection = (adCounterClockWise, adClockWise);

  // TGLCanvas
  //
    { A simple Canvas-like interface for OpenGL.
       This class implements a small "shell" for 2D operations in OpenGL,
       it operates over the current OpenGL context and provides methods
       for drawing lines, ellipses and points. 
       This class is typically used by creating an instance, using it for drawing,
       and freeing the instance. When drawing (0, 0) is the top left corner. 
       All coordinates are internally maintained with floating point precision.
       Several states are cached and it is of primary importance not to invoke
       OpenGL directly throughout the life of an instance (at the cost of
       unespected behaviour). }
  TGLCanvas = class
  private
     
    FBufferSizeX, FBufferSizeY: Integer;

    FLastPrimitive: Integer;
    FCurrentPos: TAffineVector;
    FPenColor: TColor;
    FPenWidth: Integer;
    FCurrentPenColorVector: TVector;
    FArcDirection: TArcDirection;
  protected
     
    procedure BackupOpenGLStates;

    procedure StartPrimitive(const primitiveType: Integer);

    procedure EllipseVertices(x, y, xRadius, yRadius: Single);

    procedure SetPenColor(const val: TColor);

    function GetPenAlpha: Single;
    procedure SetPenAlpha(const val: Single);
    procedure SetPenWidth(const val: Integer);

    procedure SwapSingle(pX, pY: PSingle);
    procedure NormalizePoint(const x1, y1, x2, y2: Single;
      const x, y: Single; pX, pY: PSingle);

    procedure DrawArc(x1, y1, x2, y2, x3, y3, x4, y4: Single;
      UpdateCurrentPos: Boolean); overload;
    procedure DrawArc(x1, y1, x2, y2: Single;
      AngleBegin, AngleEnd: Single;
      UpdateCurrentPos: Boolean); overload;
  public
     
    constructor Create(bufferSizeX, bufferSizeY: Integer;
      const baseTransform: TMatrix); overload;
    constructor Create(bufferSizeX, bufferSizeY: Integer); overload;
    destructor Destroy; override;

    { Stops the current internal primitive.
       This function is invoked automatically by TGLCanvas when changeing
       primitives, you should directly call if you want to render your
       own stuff intertwined with TGLCanvas drawings. In that case, call
       it before your own OpenGL calls. }
    procedure StopPrimitive;

    { Inverts the orientation of the Y Axis.
       If (0, 0) was in the top left corner, it will move to the bottom
       left corner or vice-versa. }
    procedure InvertYAxis;

    property CanvasSizeX: Integer read FBufferSizeX;
    property CanvasSizeY: Integer read FBufferSizeY;

    { Current Pen Color. }
    property PenColor: TColor read FPenColor write SetPenColor;
    { Current Pen Alpha channel (from 0.0 to 1.0) }
    property PenAlpha : Single read GetPenAlpha write SetPenAlpha;
    { Current Pen Width. }
    property PenWidth: Integer read FPenWidth write SetPenWidth;

    { Updates the current position (absolute coords). }
    procedure MoveTo(const x, y: Integer); overload;
    procedure MoveTo(const x, y: Single); overload;
    { Updates the current position (relative coords). }
    procedure MoveToRel(const x, y: Integer); overload;
    procedure MoveToRel(const x, y: Single); overload;

    { Draws a line from current position to given coordinate.
       Current position is updated. }
    procedure LineTo(const x, y: Integer); overload;
    procedure LineTo(const x, y: Single); overload;
    procedure LineToRel(const x, y: Integer); overload;
    procedure LineToRel(const x, y: Single); overload;
    { Draws a line from (x1, y1) to (x2, y2).
       The current position is NOT updated. }
    procedure Line(const x1, y1, x2, y2: Integer); overload;
    procedure Line(const x1, y1, x2, y2: Single); overload;

    { Draws the set of lines defined by connecting the points.
       Similar to invoking MoveTo on the first point, then LineTo
       on all the following points. }
    procedure Polyline(const points: array of TGLPoint);
    { Similar to Polyline but also connects the last point to the first. }
    procedure Polygon(const points: array of TGLPoint);

    { Plots a pixel at given coordinate.
       PenWidth affects pixel size. 
       The current position is NOT updated. }
    procedure PlotPixel(const x, y: Integer); overload;
    procedure PlotPixel(const x, y: Single); overload;

    { Draw the (x1,y1)-(x2, y2) rectangle's frame (border). }
    procedure FrameRect(const x1, y1, x2, y2: Integer); overload;
    procedure FrameRect(const x1, y1, x2, y2: Single); overload;

    { Draw the (x1,y1)-(x2, y2) rectangle (filled with PenColor). }
    procedure FillRect(const x1, y1, x2, y2: Integer); overload;
    procedure FillRect(const x1, y1, x2, y2: Single); overload;

    { Draw the (x1,y1)-(x2, y2) rectangle (filled with given gradient's color). }
    procedure FillRectGradient(const x1, y1, x2, y2: Single;
      const x1y1Color, x2y1Color, x2y2Color, x1y2Color: TColorVector); overload;
    procedure FillRectGradient(const x1, y1, x2, y2: Integer;
      const x1y1Color, x2y1Color, x2y2Color, x1y2Color: TColorVector); overload;

    { Draws an ellipse with (x1,y1)-(x2, y2) bounding rectangle. }
    procedure EllipseBB(const x1, y1, x2, y2: Integer); overload;
    procedure EllipseBB(const x1, y1, x2, y2: Single); overload;

    { Draws and ellipse centered at (x, y) with given radiuses. }
    procedure Ellipse(const x, y: Integer; const xRadius, yRadius: Single);
      overload;
    procedure Ellipse(const x, y: Single; const xRadius, yRadius: Single);
      overload;
    procedure Ellipse(const x, y: Single; const Radius: Single); overload;

    { Draw a filled ellipse. }
    procedure FillEllipse(const x, y: Integer; const xRadius, yRadius: Single);
      overload;
    procedure FillEllipse(const x, y: Single; const xRadius, yRadius: Single);
      overload;

    procedure FillEllipse(const x, y: Single; const Radius: Single); overload;

    { Draw a filled gradient ellipse.
    OpenGL will use the last PenColor and PenAlpha as the center color and do gradient to edge of ellipse using the edgeColor parameter. }
    procedure FillEllipseGradient(const x, y, xRadius, yRadius: Single;
      const edgeColor: TColorVector); overload;
    procedure FillEllipseGradient(const x, y: Integer;
      const xRadius, yRadius: Integer; const edgeColor: TColorVector); overload;
    procedure FillEllipseGradient(const x, y, Radius: Single;
      const edgeColor: TColorVector); overload;
    { Draw an elliptical arc.
       The points (x1, y1) and (x2, y2) specify the bounding rectangle.
       An ellipse formed by the specified bounding rectangle defines the curve of the arc.
       The arc extends in the current drawing direction from the point where it intersects the radial from the center of the bounding rectangle to the (x3, y3) point.
       The arc ends where it intersects the radial from the center of the bounding rectangle to the (x4, y4) point.
       If the starting point and ending point are the same, a complete ellipse is drawn.
       Use the ArcDirection property to get and set the current drawing direction for a device context.
       The default drawing direction is counterclockwise. }
    procedure Arc(const x1, y1, x2, y2, x3, y3, x4, y4: Integer); overload;
    procedure Arc(const x1, y1, x2, y2, x3, y3, x4, y4: Single); overload;
    procedure Arc(const x1, y1, x2, y2: Single; AngleBegin,
      AngleEnd: Single); overload;

    { Same as Arc but update the current position. }
    procedure ArcTo(const x1, y1, x2, y2, x3, y3, x4, y4: Integer); overload;
    procedure ArcTo(const x1, y1, x2, y2, x3, y3, x4, y4: Single); overload;
    procedure ArcTo(const x1, y1, x2, y2: Single; AngleBegin,
      AngleEnd: Single); overload;

    procedure RoundRect(const x1, y1, x2, y2, xr, yr: Integer); overload;
    procedure RoundRect(const x1, y1, x2, y2, xr, yr: Single); overload;


    property ArcDirection: TArcDirection read FArcDirection
      write FArcDirection;
  end;

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses
  OpenGLTokens,
  GLContext,
  GLVectorTypes;

const
  cNoPrimitive = MaxInt;
  pion2 = pi/2;
  pi3on2 = 3*pion2;

  // ------------------
  // ------------------ TGLCanvas ------------------
  // ------------------

  // Create
  //

constructor TGLCanvas.Create(bufferSizeX, bufferSizeY: Integer;
  const baseTransform: TMatrix);
var
  PM: TMatrix;
begin
  FBufferSizeX := bufferSizeX;
  FBufferSizeY := bufferSizeY;

  GL.MatrixMode(GL_PROJECTION);
  GL.PushMatrix;
  PM := CreateOrthoMatrix(0, bufferSizeX, bufferSizeY, 0, -1, 1);
  GL.LoadMatrixf(@PM);

  GL.MatrixMode(GL_MODELVIEW);
  GL.PushMatrix;
  GL.LoadMatrixf(@baseTransform);

  BackupOpenGLStates;

  FLastPrimitive := cNoPrimitive;
  FArcDirection := adCounterClockWise;
end;

// Create
//

constructor TGLCanvas.Create(bufferSizeX, bufferSizeY: Integer);
begin
  Create(bufferSizeX, bufferSizeY, IdentityHmgMatrix);
end;

// Destroy
//

destructor TGLCanvas.Destroy;
begin
  StopPrimitive;

  GL.MatrixMode(GL_PROJECTION);
  GL.PopMatrix;

  GL.MatrixMode(GL_MODELVIEW);
  GL.PopMatrix;
end;

// BackupOpenGLStates
//

procedure TGLCanvas.BackupOpenGLStates;
begin
  with CurrentGLContext.GLStates do
  begin
    Disable(stLighting);
    Disable(stFog);
    Disable(stCullFace);
    Disable(stColorMaterial);
    Disable(stDepthTest);
    Disable(stLineSmooth);
    Disable(stLineStipple);
    Disable(stPointSmooth);
    Enable(stBlend);
    SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);

    // Setup and backup pen stuff
    FPenColor := clBlack;
    SetVector(FCurrentPenColorVector, NullHmgPoint);
    GL.Color4fv(@FCurrentPenColorVector);
    FPenWidth := 1;
    LineWidth := 1;
    PointSize := 1;
  end;
end;

// StartPrimitive
//

procedure TGLCanvas.StartPrimitive(const primitiveType: Integer);
begin
  if primitiveType <> FLastPrimitive then
  begin
    if FLastPrimitive <> cNoPrimitive then
      GL.End_;
    if primitiveType <> cNoPrimitive then
      GL.Begin_(primitiveType);
    FLastPrimitive := primitiveType;
  end;
end;

// StopPrimitive
//

procedure TGLCanvas.StopPrimitive;
begin
  StartPrimitive(cNoPrimitive);
end;

// InvertYAxis
//

procedure TGLCanvas.InvertYAxis;
var
  mat: TMatrix;
begin
  mat := IdentityHmgMatrix;
  mat.V[1].V[1] := -1;
  mat.V[3].V[1] := FBufferSizeY;
  GL.MultMatrixf(@mat);
end;

// SetPenColor
//

procedure TGLCanvas.SetPenColor(const val: TColor);
begin
  SetVector(FCurrentPenColorVector, ConvertWinColor(val,
    FCurrentPenColorVector.V[3]));
  FPenColor := val;
  GL.Color4fv(@FCurrentPenColorVector);
end;

// SetPenAlpha
//

procedure TGLCanvas.SetPenAlpha(const val: Single);
begin
  FCurrentPenColorVector.V[3] := val;
  GL.Color4fv(@FCurrentPenColorVector);
end;

// SetPenWidth
//

procedure TGLCanvas.SetPenWidth(const val: Integer);
begin
  if val < 1 then
    Exit;
  if val <> FPenWidth then
    with CurrentGLContext.GLStates do
    begin
      FPenWidth := val;
      StopPrimitive;
      LineWidth := val;
      PointSize := val;
    end;
end;

// MoveTo
//

procedure TGLCanvas.MoveTo(const x, y: Integer);
begin
  FCurrentPos.V[0] := x;
  FCurrentPos.V[1] := y;
end;

// MoveTo
//

procedure TGLCanvas.MoveTo(const x, y: Single);
begin
  FCurrentPos.V[0] := x;
  FCurrentPos.V[1] := y;
end;

// MoveToRel
//

procedure TGLCanvas.MoveToRel(const x, y: Integer);
begin
  FCurrentPos.V[0] := FCurrentPos.V[0] + x;
  FCurrentPos.V[1] := FCurrentPos.V[1] + y;
end;

// MoveToRel
//

procedure TGLCanvas.MoveToRel(const x, y: Single);
begin
  FCurrentPos.V[0] := FCurrentPos.V[0] + x;
  FCurrentPos.V[1] := FCurrentPos.V[1] + y;
end;

// LineTo
//

procedure TGLCanvas.LineTo(const x, y: Integer);
begin
  StartPrimitive(GL_LINES);
  GL.Vertex2fv(@FCurrentPos);
  MoveTo(x, y);
  GL.Vertex2fv(@FCurrentPos);
end;

// LineTo
//

procedure TGLCanvas.LineTo(const x, y: Single);
begin
  StartPrimitive(GL_LINES);
  GL.Vertex2fv(@FCurrentPos);
  MoveTo(x, y);
  GL.Vertex2fv(@FCurrentPos);
end;

// LineToRel
//

procedure TGLCanvas.LineToRel(const x, y: Integer);
begin
  LineTo(FCurrentPos.V[0] + x, FCurrentPos.V[1] + y);
end;

// LineToRel
//

procedure TGLCanvas.LineToRel(const x, y: Single);
begin
  LineTo(FCurrentPos.V[0] + x, FCurrentPos.V[1] + y);
end;

// Line
//

procedure TGLCanvas.Line(const x1, y1, x2, y2: Integer);
begin
  StartPrimitive(GL_LINES);
  GL.Vertex2i(x1, y1);
  GL.Vertex2i(x2, y2);
end;

// Line
//

procedure TGLCanvas.Line(const x1, y1, x2, y2: Single);
begin
  StartPrimitive(GL_LINES);
  GL.Vertex2f(x1, y1);
  GL.Vertex2f(x2, y2);
end;

// Polyline
//

procedure TGLCanvas.Polyline(const points: array of TGLPoint);
var
  i, n: Integer;
begin
  n := Length(Points);
  if n > 1 then
  begin
    StartPrimitive(GL_LINE_STRIP);
    GL.Vertex2iv(@points[Low(points)]);
    for i := Low(points) + 1 to High(points) do
      GL.Vertex2iv(@points[i]);
    StopPrimitive;
  end;
end;

// Polygon
//

procedure TGLCanvas.Polygon(const points: array of TGLPoint);
var
  i, n: Integer;
begin
  n := Length(Points);
  if n > 1 then
  begin
    StartPrimitive(GL_LINE_LOOP);
    GL.Vertex2iv(@points[Low(points)]);
    for i := Low(points) + 1 to High(points) do
      GL.Vertex2iv(@points[i]);
    StopPrimitive;
  end;
end;

// PlotPixel
//

procedure TGLCanvas.PlotPixel(const x, y: Integer);
begin
  StartPrimitive(GL_POINTS);
  GL.Vertex2i(x, y);
end;

// PlotPixel
//

procedure TGLCanvas.PlotPixel(const x, y: Single);
begin
  StartPrimitive(GL_POINTS);
  GL.Vertex2f(x, y);
end;

// FrameRect (integer)
//

procedure TGLCanvas.FrameRect(const x1, y1, x2, y2: Integer);
begin
  StartPrimitive(GL_LINE_LOOP);
  GL.Vertex2i(x1, y1);
  GL.Vertex2i(x2, y1);
  GL.Vertex2i(x2, y2);
  GL.Vertex2i(x1, y2);
  StopPrimitive;
end;

// FrameRect (single)
//

procedure TGLCanvas.FrameRect(const x1, y1, x2, y2: Single);
begin
  StartPrimitive(GL_LINE_LOOP);
  GL.Vertex2f(x1, y1);
  GL.Vertex2f(x2, y1);
  GL.Vertex2f(x2, y2);
  GL.Vertex2f(x1, y2);
  StopPrimitive;
end;

function TGLCanvas.GetPenAlpha: Single;
begin
  Result := FCurrentPenColorVector.V[3];
end;

// FillRect (integer)
//

procedure TGLCanvas.FillRect(const x1, y1, x2, y2: Integer);
begin
  StartPrimitive(GL_QUADS);
  GL.Vertex2i(x1, y1);
  GL.Vertex2i(x2, y1);
  GL.Vertex2i(x2, y2);
  GL.Vertex2i(x1, y2);
  StopPrimitive;
end;

// FillRect (single)
//

procedure TGLCanvas.FillRect(const x1, y1, x2, y2: Single);
begin
  StartPrimitive(GL_QUADS);
  GL.Vertex2f(x1, y1);
  GL.Vertex2f(x2, y1);
  GL.Vertex2f(x2, y2);
  GL.Vertex2f(x1, y2);
  StopPrimitive;
end;

// EllipseVertices
//

procedure TGLCanvas.EllipseVertices(x, y, xRadius, yRadius: Single);
var
  i, n: Integer;
  s, c: TSingleArray;
begin
  n := Round(MaxFloat(xRadius, yRadius) * 0.1) + 5;
  SetLength(s, n);
  SetLength(c, n);
  Dec(n);
  PrepareSinCosCache(s, c, 0, 90);
  ScaleFloatArray(s, yRadius);
  ScaleFloatArray(c, xRadius);
  // first quadrant (top right)
  for i := 0 to n do
    GL.Vertex2f(x + c[i], y - s[i]);
  // second quadrant (top left)
  for i := n - 1 downto 0 do
    GL.Vertex2f(x - c[i], y - s[i]);
  // third quadrant (bottom left)
  for i := 1 to n do
    GL.Vertex2f(x - c[i], y + s[i]);
  // fourth quadrant (bottom right)
  for i := n - 1 downto 0 do
    GL.Vertex2f(x + c[i], y + s[i]);
end;

// EllipseBB
//

procedure TGLCanvas.EllipseBB(const x1, y1, x2, y2: Integer);
begin
  Ellipse((x1 + x2) * 0.5, (y1 + y2) * 0.5, Abs(x2 - x1) * 0.5, Abs(y2 - y1) *
    0.5);
end;

// EllipseBB
//

procedure TGLCanvas.EllipseBB(const x1, y1, x2, y2: Single);
begin
  Ellipse((x1 + x2) * 0.5, (y1 + y2) * 0.5, Abs(x2 - x1) * 0.5, Abs(y2 - y1) *
    0.5);
end;

// Ellipse
//

procedure TGLCanvas.Ellipse(const x, y: Single; const Radius: Single);
begin
  Ellipse(x, y, Radius, Radius);
end;

// Ellipse
//

procedure TGLCanvas.Ellipse(const x, y: Integer; const xRadius, yRadius:
  Single);
var
  sx, sy: Single;
begin
  sx := x;
  sy := y;
  Ellipse(sx, sy, xRadius, yRadius);
end;

// Ellipse
//

procedure TGLCanvas.Ellipse(const x, y: Single; const xRadius, yRadius: Single);
begin
  StartPrimitive(GL_LINE_STRIP);
  EllipseVertices(x, y, xRadius, yRadius);
  StopPrimitive;
end;

// FillEllipse
//

procedure TGLCanvas.FillEllipse(const x, y: Integer; const xRadius, yRadius:
  Single);
begin
  StartPrimitive(GL_TRIANGLE_FAN);
  GL.Vertex2f(x, y); // not really necessary, but may help with memory stride
  EllipseVertices(x, y, xRadius, yRadius);
  StopPrimitive;
end;

// FillEllipse
//

procedure TGLCanvas.FillEllipse(const x, y, xRadius, yRadius: Single);
begin
  StartPrimitive(GL_TRIANGLE_FAN);
  GL.Vertex2f(x, y); // not really necessary, but may help with memory stride
  EllipseVertices(x, y, xRadius, yRadius);
  StopPrimitive;
end;

// FillEllipse
//

procedure TGLCanvas.FillEllipse(const x, y, Radius: Single);
begin
  FillEllipse(x, y, Radius, Radius);
end;

// FillRectGradient
//

procedure TGLCanvas.FillRectGradient(const x1, y1, x2, y2: Single;
  const x1y1Color, x2y1Color, x2y2Color, x1y2Color: TColorVector);
begin
  StartPrimitive(GL_QUADS);
  GL.Color4f(x1y1Color.V[0], x1y1Color.V[1], x1y1Color.V[2], x1y1Color.V[3]);
  GL.Vertex2f(x1, y1);
  GL.Color4f(x2y1Color.V[0], x2y1Color.V[1], x2y1Color.V[2], x2y1Color.V[3]);
  GL.Vertex2f(x2, y1);
  GL.Color4f(x2y2Color.V[0], x2y2Color.V[1], x2y2Color.V[2], x2y2Color.V[3]);
  GL.Vertex2f(x2, y2);
  GL.Color4f(x1y2Color.V[0], x1y2Color.V[1], x1y2Color.V[2], x1y2Color.V[3]);
  GL.Vertex2f(x1, y2);
  StopPrimitive;

  // restore pen color
  GL.Color4fv(@FCurrentPenColorVector);
end;

// FillRectGradient
//

procedure TGLCanvas.FillRectGradient(const x1, y1, x2, y2: Integer;
  const x1y1Color, x2y1Color, x2y2Color, x1y2Color: TColorVector);
begin
  StartPrimitive(GL_QUADS);
  GL.Color4f(x1y1Color.V[0], x1y1Color.V[1], x1y1Color.V[2], x1y1Color.V[3]);
  GL.Vertex2i(x1, y1);
  GL.Color4f(x2y1Color.V[0], x2y1Color.V[1], x2y1Color.V[2], x2y1Color.V[3]);
  GL.Vertex2i(x2, y1);
  GL.Color4f(x2y2Color.V[0], x2y2Color.V[1], x2y2Color.V[2], x2y2Color.V[3]);
  GL.Vertex2i(x2, y2);
  GL.Color4f(x1y2Color.V[0], x1y2Color.V[1], x1y2Color.V[2], x1y2Color.V[3]);
  GL.Vertex2i(x1, y2);
  StopPrimitive;

  // restore pen color
  GL.Color4fv(@FCurrentPenColorVector);
end;

// FillEllipseGradient (integer)
//

procedure TGLCanvas.FillEllipseGradient(const x, y: Integer; const xRadius, yRadius: Integer; const edgeColor: TColorVector);
begin
  StartPrimitive(GL_TRIANGLE_FAN);

  // the center will use the last set PenColor and PenAlpha
  GL.Vertex2f(x, y); // really necessary now :)

  // then openGL will do a gradient from the center to the edge using the edgeColor
  GL.Color4f(edgeColor.V[0], edgeColor.V[1], edgeColor.V[2], edgeColor.V[3]);
  EllipseVertices(x, y, xRadius, yRadius);
  StopPrimitive;

  // restore pen color
  GL.Color4fv(@FCurrentPenColorVector);
end;

// FillEllipseGradient (single)
//

procedure TGLCanvas.FillEllipseGradient(const x, y, xRadius, yRadius: Single; const edgeColor: TColorVector);
begin
  StartPrimitive(GL_TRIANGLE_FAN);
  GL.Vertex2f(x, y); // really necessary now :)
  GL.Color4f(edgeColor.V[0], edgeColor.V[1], edgeColor.V[2], edgeColor.V[3]);
  EllipseVertices(x, y, xRadius, yRadius);
  StopPrimitive;

  // restore pen color
  GL.Color4fv(@FCurrentPenColorVector);
end;

// FillEllipseGradient (single)
//

procedure TGLCanvas.FillEllipseGradient(const x, y, Radius: Single; const edgeColor: TColorVector);
begin
  FillEllipseGradient(x, y, Radius, Radius, edgeColor);
end;

// Arc
//

procedure TGLCanvas.Arc(const x1, y1, x2, y2, x3, y3, x4, y4: Integer);
begin
  DrawArc(x1, y1, x2, y2, x3, y3, x4, y4, False);
end;

procedure TGLCanvas.Arc(const x1, y1, x2, y2, x3, y3, x4, y4: Single);
begin
  DrawArc(x1, y1, x2, y2, x3, y3, x4, y4, False);
end;

procedure TGLCanvas.Arc(const x1, y1, x2, y2: Single; AngleBegin, AngleEnd: Single);
begin
  DrawArc(x1, y1, x2, y2, AngleBegin, AngleEnd, False);
end;

// ArcTo
//

procedure TGLCanvas.ArcTo(const x1, y1, x2, y2, x3, y3, x4, y4: Integer);
begin
  DrawArc(x1, y1, x2, y2, x3, y3, x4, y4, True);
end;

procedure TGLCanvas.ArcTo(const x1, y1, x2, y2, x3, y3, x4, y4: Single);
begin
  DrawArc(x1, y1, x2, y2, x3, y3, x4, y4, True);
end;

procedure TGLCanvas.ArcTo(const x1, y1, x2, y2: Single; AngleBegin, AngleEnd: Single);
begin
  DrawArc(x1, y1, x2, y2, AngleBegin, AngleEnd, True);
end;

procedure TGLCanvas.RoundRect(const x1, y1, x2, y2, xr, yr: Integer);
var
  x2r, y2r, x, y: integer;
begin
  x2r := 2*xr;
  y2r := 2*yr;
  x := x1 -1;
  y := y2 +1;
  Arc(x, y1, x + x2r, y1 + y2r, pi3on2, pi);
  Line(x1, y1 + yr, x1, y - yr);
  Arc(x, y, x + x2r,  y - y2r, pi, pion2);
  Line(x + xr, y2, x2 - xr, y2);
  Arc(x2, y, x2 - x2r, y - y2r, pion2, 0);
  Line(x2, y1 + yr, x2, y - yr);
  Arc(x2, y1, x2 - x2r, y1 + y2r, 0, pi3on2);
  Line(x + xr, y1, x2 - xr, y1);
end;

procedure TGLCanvas.RoundRect(const x1, y1, x2, y2, xr, yr: Single);
var
  x2r, y2r, x, y: Single;
begin
  x2r := 2*xr;
  y2r := 2*yr;
  x := x1 -1;
  y := y2 +1;
  Arc(x, y1, x + x2r, y1 + y2r, pi3on2, pi);
  Line(x1, y1 + yr, x1, y - yr);
  Arc(x, y, x + x2r,  y - y2r, pi, pion2);
  Line(x + xr, y2, x2 - xr, y2);
  Arc(x2, y, x2 - x2r, y - y2r, pion2, 0);
  Line(x2, y1 + yr, x2, y - yr);
  Arc(x2, y1, x2 - x2r, y1 + y2r, 0, pi3on2);
  Line(x + xr, y1, x2 - xr, y1);
end;


// Arc Draw
//

// wrapper from "ByPoints" methode

procedure TGLCanvas.DrawArc(x1, y1, x2, y2, x3, y3, x4, y4: Single; UpdateCurrentPos: Boolean);
var
  x, y: Single;
  AngleBegin, AngleEnd: Single;
begin
  if x1 > x2 then
    SwapSingle(@x1, @x2);
  if y1 > y2 then
    SwapSingle(@y1, @y2);

  NormalizePoint(x1, y1, x2, y2, x3, y3, @x, @y);
  AngleBegin := ArcTan2(y, x);

  NormalizePoint(x1, y1, x2, y2, x4, y4, @x, @y);
  AngleEnd := ArcTan2(y, x);

  DrawArc(x1, y1, x2, y2, AngleBegin, AngleEnd, UpdateCurrentPos);
end;

// Real work is here

procedure TGLCanvas.DrawArc(x1, y1, x2, y2: Single; AngleBegin, AngleEnd: Single; UpdateCurrentPos: Boolean);
var
  Xc, Yc, Rx, Ry, x, y, s, c: Single;
  AngleCurrent, AngleDiff, AngleStep: Single;
begin
  // check that our box is well set (as the original Arc function do)
  if x1 > x2 then
    SwapSingle(@x1, @x2);
  if y1 > y2 then
    SwapSingle(@y1, @y2);

  if (x1 = x2) or (y1 = y2) then
    exit;

  Xc := (x1 + x2) * 0.5;
  Yc := (y1 + y2) * 0.5;

  Rx := Abs(x2 - x1) * 0.5;
  Ry := Abs(y2 - y1) * 0.5;

  // if ClockWise then swap AngleBegin and AngleEnd to simulate it.
  if FArcDirection = adClockWise then
  begin
    AngleCurrent := AngleBegin;
    AngleBegin := AngleEnd;
    AngleEnd := AngleCurrent;
  end;

  if (AngleEnd >= AngleBegin) then
  begin // if end sup to begin, remove 2*Pi (360°)
    AngleEnd := AngleEnd - 2 * Pi;
  end;

  AngleDiff := Abs(AngleEnd - AngleBegin); // the amount radian to travel
  AngleStep := AngleDiff / Round(MaxFloat(Rx, Ry) * 0.1 + 5); // granulity of drawing, not too much, not too less

  AngleCurrent := AngleBegin;

  StartPrimitive(GL_LINE_STRIP);
  while AngleCurrent >= AngleBegin - AngleDiff do
  begin
    SinCos(AngleCurrent, s, c);
    x := Xc + (Rx * c);
    y := Yc + (Ry * s);

    GL.Vertex2f(x, y);

    AngleCurrent := AngleCurrent - AngleStep; // always step down, rotate only one way to draw it
  end;

  SinCos(AngleEnd, s, c);
  x := Xc + (Rx * c);
  y := Yc + (Ry * s);

  GL.Vertex2f(x, y);

  StopPrimitive();

  if UpdateCurrentPos then
    MoveTo(x, y); //FCurrentPos := CurrentPos;
end;

// for internal need

procedure TGLCanvas.NormalizePoint(const x1, y1, x2, y2: Single; const x, y: Single; pX, pY: PSingle);
begin
  pX^ := (x - x1) / (x2 - x1) * 2.0 - 1.0;
  pY^ := (y - y1) / (y2 - y1) * 2.0 - 1.0;
end;

procedure TGLCanvas.SwapSingle(pX, pY: PSingle);
var
  tmp: Single;
begin
  tmp := pX^;
  pX^ := pY^;
  pY^ := tmp;
end;

end.

