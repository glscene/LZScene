unit Main;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  SysUtils, Classes, Math,
  Graphics, Controls, Forms, Dialogs, Buttons,
  StdCtrls, ComCtrls, ExtCtrls,
   
  {GLWin32Viewer,} GLCrossPlatform, GLBaseClasses, GLScene, GLGeomObjects,
  GLObjects, GLCoordinates, GLGraph, GLVectorLists, OpenGLTokens,
  GLMesh, GLHUDObjects, GLBitmapFont, GLWindowsFont,
  GLVectorGeometry, GLColor, GLTexture, GLContext, GLCadencer, GLLCLViewer;


type
  TMainForm = class(TForm)
    StatusBar: TStatusBar;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    CameraCube: TGLDummyCube;
    Camera: TGLCamera;
    ObjectsCube: TGLDummyCube;
    ArrowZ: TGLArrowLine;
    ArrowY: TGLArrowLine;
    ArrowX: TGLArrowLine;

    GLLightSource1: TGLLightSource;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label12: TLabel;

    xRadiusTrackBar: TTrackBar;
    yRadiusTrackBar: TTrackBar;
    zRadiusTrackBar: TTrackBar;

    VCurveTrackBar: TTrackBar;
    HCurveTrackBar: TTrackBar;
    GridCheckBox: TCheckBox;
    ArrowsCheckBox: TCheckBox;
    SlicesTrackBar: TTrackBar;
    StacksTrackBar: TTrackBar;
    TopCapRadioGroup: TRadioGroup;
    BottomTrackBar: TTrackBar;
    TopTrackBar: TTrackBar;
    StartTrackBar: TTrackBar;
    StopTrackBar: TTrackBar;

    BottomCapRadioGroup: TRadioGroup;
    Button1: TButton;
    GLXYZGridXZ: TGLXYZGrid;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    GLHUDText: TGLHUDText;
    Button2: TButton;
    GLCadencer1: TGLCadencer;
    GLLightSource: TGLLightSource;
    GLSuperellipsoid: TGLSuperellipsoid;
    GLMesh: TGLMesh;

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure RadiusTrackBarChange(Sender: TObject);
    procedure VCurveTrackBarChange(Sender: TObject);
    procedure HCurveTrackBarChange(Sender: TObject);
    procedure SlicesTrackBarChange(Sender: TObject);
    procedure StacksTrackBarChange(Sender: TObject);
    procedure GridCheckBoxClick(Sender: TObject);
    procedure ArrowsCheckBoxClick(Sender: TObject);
    procedure TopTrackBarChange(Sender: TObject);
    procedure BottomTrackBarChange(Sender: TObject);
    procedure TopCapRadioGroupClick(Sender: TObject);
    procedure BottomCapRadioGroupClick(Sender: TObject);
    procedure StartTrackBarChange(Sender: TObject);
    procedure StopTrackBarChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure checkclick(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
     
    MousePoint: TPoint;
    Superellipsoids: array[0..5, 0..5] of TGLSuperellipsoid;
    procedure ShowCameraLocation;
    procedure ShowFocalLength;
    procedure ShowDisplacement;
    procedure ShowSuperellipsoid;
  public
     
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}
{$R CURSORS.RES}

const
  crLightxz = 1;
  crLightyz = 2;
  crLightxy = 3;
  crSlidexy = 4;
  crSlideyz = 5;
  crSlidexz = 6;
  crRotate = 7;
  crZoom = 8;
  crHandMove = 9;

procedure TMainForm.ArrowsCheckBoxClick(Sender: TObject);
begin
  ArrowX.Visible := not ArrowsCheckBox.Checked;
  ArrowY.Visible := ArrowX.Visible;
  ArrowZ.Visible := ArrowX.Visible;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Screen.Cursors[crSlidexy] := LoadCursor(HInstance, 'SLIDEXY');
  Screen.Cursors[crRotate] := LoadCursor(HInstance, 'ROTATE');
  Screen.Cursors[crZoom] := LoadCursor(HInstance, 'ZOOM');

  Randomize;
  GLSuperellipsoid := TGLSuperellipsoid(GLScene1.Objects.AddNewChild(TGLSuperellipsoid));
  GLSuperellipsoid.Direction.SetVector(0, 0, 1);
  GLSuperellipsoid.Up.SetVector(0, 1, 0);
  GLSuperellipsoid.Position.SetPoint(0, 1, 0);
  GLSuperellipsoid.Material.FrontProperties.Ambient.RandomColor;
  GLSuperellipsoid.Material.FrontProperties.Diffuse.RandomColor;
  GLSuperellipsoid.Material.FrontProperties.Shininess := 100;

end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ShowCameraLocation;
  { focallength: right mouse drag up/down }
  ShowFocalLength;
  { displace origin: x axis: ctrl/left mouse drag left/right
    y axis: ctrl/left mouse drag up/down }
  ShowDisplacement;
  { move light: x axis: ctrl right mouse drag left/right
    y axis: ctrl right mouse drag up/down
    z axis: shift right mouse drag up/down }
  ShowSuperellipsoid;
end;

procedure TMainForm.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MousePoint.X := X;
  MousePoint.Y := Y;
  if ssShift in Shift then
  begin
    if ssLeft in Shift then
      Screen.Cursor := crZoom
    else if ssRight in Shift then
      Screen.Cursor := crLightxz;
  end
  else if ssCtrl in Shift then
  begin
    if ssLeft in Shift then
      Screen.Cursor := crSlidexy
    else if ssRight in Shift then
      Screen.Cursor := crLightxy;
  end
  else { no shift or ctrl key }
  begin
    if Shift = [ssLeft] then
      Screen.Cursor := crRotate
    else if Shift = [ssRight] then
      Screen.Cursor := crZoom;
  end;
end;

procedure TMainForm.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  dx, dy: Integer;
  nx, nz, d: TGLFloat;

begin { refer GLScene\Demos\interface\camera\Camera.dpr }
  dx := MousePoint.X - X;
  dy := MousePoint.Y - Y;
  if ssShift in Shift then { shift key down }
  begin
    if ssLeft in Shift then { left mouse button }
    begin
      { dy = a step which adjusts target distance by 1.25%; zoom in or out }
      with Camera do
        AdjustDistanceToTarget(Power(1.0125, dy));
      ShowCameraLocation;
    end
  end
  else if ssCtrl in Shift then { Ctrl key down }
  begin
    if ssLeft in Shift then { left mouse button }
    begin
      nz := Camera.Position.Z * dy;
      nx := Camera.Position.Z * dx;
      d := 5 * Camera.FocalLength;
      with CameraCube.Position do
      begin
        Z := Z - nz / d;
        X := X - nx / d;
      end;
      ShowDisplacement;
    end
  end
  else { no shift key }
  begin
    if Shift = [ssLeft] then
    { Left mouse button changes camera angle by moving around target }
    begin
      Camera.MoveAroundTarget(dy, dx);
      ShowCameraLocation;
    end;
    if Shift = [ssRight] then
    begin
      { Right mouse button alters the camera's focal length;
        zoom out or in by moving cursor up or down }
      with Camera do
      begin
        FocalLength := FocalLength - dy;
        if FocalLength > 1000 then
          FocalLength := 1000; { max focal length }
        if FocalLength < 20 then
          FocalLength := 20; { min focal length }
      end;
      ShowFocalLength; { display in statusbar palel }
    end;
  end;
  MousePoint.X := X; { update mouse position }
  MousePoint.Y := Y;
end;

procedure TMainForm.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

procedure TMainForm.GridCheckBoxClick(Sender: TObject);
begin
  GLXYZGridXZ.Visible := not GridCheckBox.Checked;
end;

procedure TMainForm.CheckBoxClick(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.VCurveTrackBarChange(Sender: TObject);
var
  n: TGLFloat;

begin
  n := VCurveTrackBar.Position / 10;
  GLSuperellipsoid.XYCurve := n;
  ShowSuperellipsoid;
end;

procedure TMainForm.HCurveTrackBarChange(Sender: TObject);
var
  n: TGLFloat;

begin
  n := HCurveTrackBar.Position / 10;
  GLSuperellipsoid.XYCurve := n;
  ShowSuperellipsoid;
end;

procedure TMainForm.ShowCameraLocation;
begin
  with Camera.Position do
    StatusBar.Panels[0].Text := 'Camera: ' + FloatToStrF(X, ffNumber, 5, 2) +
      ', ' + FloatToStrF(Y, ffNumber, 5, 2) + ', ' +
      FloatToStrF(Z, ffNumber, 5, 2);
end;

procedure TMainForm.ShowFocalLength;
begin
  with Camera do
    StatusBar.Panels[1].Text := 'f = ' + FloatToStrF(FocalLength,
      ffNumber, 5, 2);
end;

procedure TMainForm.ShowDisplacement;
begin
  with CameraCube.Position do
    StatusBar.Panels[2].Text := 'Displaced: ' + FloatToStrF(-X, ffNumber, 5, 2)
      + ', ' + FloatToStrF(-Y, ffNumber, 5, 2);
end;

procedure TMainForm.ShowSuperellipsoid;
begin
  // Superellipsoid.NormalDirection := ndInside;
  // Superellipsoid.Normals :=
  { Determines how and if normals are smoothed.<p>
    - nsFlat : facetted look<br>
    - nsSmooth : smooth look<br>
    - nsNone : unlighted rendering, usefull for decla texturing }
  GLSuperellipsoid.Scale.SetVector(xRadiusTrackBar.Position,
    yRadiusTrackBar.Position, zRadiusTrackBar.Position);
  GLSuperellipsoid.Slices := SlicesTrackBar.Position;
  GLSuperellipsoid.Stacks := StacksTrackBar.Position;
  GLSuperellipsoid.Top := TopTrackBar.Position;

  case TopCapRadioGroup.ItemIndex of
    0: GLSuperellipsoid.TopCap := ctNone;
    1: GLSuperellipsoid.TopCap := ctCenter;
    2: GLSuperellipsoid.TopCap := ctFlat;
  end;

  GLSuperellipsoid.Bottom := -BottomTrackBar.Position;

  case BottomCapRadioGroup.ItemIndex of
    0: GLSuperellipsoid.BottomCap := ctNone;
    1: GLSuperellipsoid.BottomCap := ctCenter;
    2: GLSuperellipsoid.BottomCap := ctFlat;
  end;

  if (StartTrackBar.Position <= StopTrackBar.Position) and
    (StartTrackBar.Position < 360) then
  begin
    GLSuperellipsoid.Start := StartTrackBar.Position;
    GLSuperellipsoid.Stop := StopTrackBar.Position;
  end;
  GLSuperellipsoid.Normals := nsNone;
  GLHUDText.Text := 'Scale:' + FloatToStrF(xRadiusTrackBar.Position / 10,
    ffNumber, 6, 2) + ', ' + FloatToStrF(yRadiusTrackBar.Position / 10,
    ffNumber, 6, 2) + ', ' + FloatToStrF(zRadiusTrackBar.Position / 10,
    ffNumber, 6, 2) + #13#10'VCurve:' +
    FloatToStrF(VCurveTrackBar.Position / 10, ffNumber, 6, 2) + #13#10'HCurve:'
    + FloatToStrF(HCurveTrackBar.Position / 10, ffNumber, 6, 2) +
    #13#10'Slices:' + IntToStr(SlicesTrackBar.Position) + #13#10'Stacks:' +
    IntToStr(StacksTrackBar.Position) + #13#10'Top:' +
    IntToStr(TopTrackBar.Position) + '°' + #13#10'Bottom:' +
    IntToStr(BottomTrackBar.Position) + '°' + #13#10'Start:' +
    IntToStr(StartTrackBar.Position) + '°' + #13#10'Stop:' +
    IntToStr(StopTrackBar.Position) + '°';
end;

procedure TMainForm.SlicesTrackBarChange(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.StacksTrackBarChange(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.StartTrackBarChange(Sender: TObject);
begin
  if (StartTrackBar.Position >= StopTrackBar.Position) then
    StartTrackBar.Position := StopTrackBar.Position;
  ShowSuperellipsoid;
end;

procedure TMainForm.TopCapRadioGroupClick(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.TopTrackBarChange(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.StopTrackBarChange(Sender: TObject);
begin
  if (StopTrackBar.Position <= StartTrackBar.Position) then
    StopTrackBar.Position := StartTrackBar.Position;
  ShowSuperellipsoid;
end;

procedure TMainForm.BottomCapRadioGroupClick(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.BottomTrackBarChange(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  with GLSuperellipsoid.Material.Texture do
  begin
    // We need a CubeMapImage, which unlike the "regular Images" stores
    // multiple images.
    ImageClassName := TGLCubeMapImage.ClassName;
    with Image as TGLCubeMapImage do
    begin
      // Load all 6 texture map components of the cube map
      // The 'PX', 'NX', etc. refer to 'positive X', 'negative X', etc.
      // and follow the RenderMan specs/conventions
      Picture[cmtNX].LoadFromFile('cm_left.png');
      Picture[cmtPX].LoadFromFile('cm_right.png');
      Picture[cmtNY].LoadFromFile('cm_top.png');
      Picture[cmtPY].LoadFromFile('cm_bottom.png');
      Picture[cmtPZ].LoadFromFile('cm_back.png');
      Picture[cmtNZ].LoadFromFile('cm_front.png');
    end;
    // Select reflection cube map environment mapping
    // This is the mode you'll most commonly use with cube maps, normal cube
    // map generation is also supported (used for diffuse environment lighting)
    MappingMode := tmmCubeMapReflection;
    // That's all folks, let us see the thing!
    Disabled := False;
  end;
  Button1.Visible := False;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  i, j: integer;
  x, y, d: single;

begin

  d := 6;
  Randomize;
  for j := 0 to 5 do
  for i := 0 to 5 do
  begin
    x := -d*2.5 + d*i;
    y :=  d*2.5 - d*j;
    Superellipsoids[i, j] :=
    TGLSuperellipsoid(GLScene1.Objects.AddNewChild(TGLSuperellipsoid));

    with Superellipsoids[i, j] do
    begin
      Slices := 32;
      Stacks := 32;
      Scale.SetVector(5, 5, 5);
      Position.SetPoint(x, y, 0);
      Direction.SetVector(0, 1, 0);
      Up.SetVector(0, 0, 1);
      case i of
      0:XYCurve := 0.2;
      1:XYCurve := 0.8;
      2:XYCurve := 1.0;
      3:XYCurve := 1.5;
      4:XYCurve := 2.0;
      5:XYCurve := 3.0;
      end;
      case j of
      0:XYCurve := 0.2;
      1:XYCurve := 0.8;
      2:XYCurve := 1.0;
      3:XYCurve := 1.5;
      4:XYCurve := 2.0;
      5:XYCurve := 3.0;
      end;
      with Material.FrontProperties do
      begin
        Ambient.RandomColor;
        Diffuse.RandomColor;
        Specular.RandomColor;
        Shininess := 125;
      end;
    end;
  end;
  Button2.Visible := False;
end;

procedure TMainForm.checkclick(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.RadiusTrackBarChange(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

end.
