unit Unit1;

{$mode objfpc}{$h+}

interface

uses

  SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
   
  GLCrossPlatform, GLBaseClasses, GLScene, GLLCLViewer,
  GLObjects, GLCoordinates, GLVectorGeometry;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    Scn: TGLSceneViewer;
    cam: TGLCamera;
    light: TGLLightSource;
    GLPlane1: TGLPlane;
    GLCube1: TGLCube;
    dc2: TGLDummyCube;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    dc1: TGLDummyCube;
    shad: TGLPlane;
    procedure ScnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ScnMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
  public

    LastPickPos: TVector;
    CurrentPick: TGLCustomSceneObject;

    function MouseWorldPos(X, Y: single; movingOnZ: boolean): TVector;

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

//
// mouse down
//
procedure TForm1.ScnMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
var
  pick: TGLBaseSceneObject;
begin

  pick := Scn.Buffer.GetPickedObject(X, Y) as TGLCustomSceneObject;

  if (pick <> nil) and (pick.Name <> 'GLCube1') then
    pick := nil;

  if pick <> CurrentPick then
  begin
    if CurrentPick <> nil then
      CurrentPick.Material.FrontProperties.Emission.SetColor(0, 0, 0);
    CurrentPick := TGLCustomSceneObject(pick);
    if CurrentPick <> nil then
      CurrentPick.Material.FrontProperties.Emission.SetColor(1, 0, 0);
  end;

  if CurrentPick <> nil then
    LastPickPos := CurrentPick.AbsolutePosition;

end;

//
// mouse move
//
procedure TForm1.ScnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  newPos, v: TVector;
begin

  if (not(ssLeft in Shift)) or (CurrentPick = nil) then
    exit;

  v := dc2.AbsoluteToLocal(MouseWorldPos(X, Y, ssShift in Shift));
  v.X := ClampValue(v.X, -15, 15);
  v.Y := ClampValue(v.Y, 0, 10);
  v.Z := ClampValue(v.Z, -15, 15);

  CurrentPick.Position.SetPoint(v);

  v.Y := 0.1;
  shad.Position.SetPoint(v);
end;

//
// get mouse world pos
//
function TForm1.MouseWorldPos(X, Y: single; movingOnZ: boolean): TVector;
var
  v1, v2: TVector;
begin

  SetVector(Result, NullVector);
  if CurrentPick = nil then
    exit;

  SetVector(v1, X, Scn.Height - Y, 0);

  if movingOnZ then
  begin
    v2 := dc2.AbsoluteToLocal(cam.AbsoluteVectorToTarget);
    v2.Y := 0;
    v2 := dc2.LocalToAbsolute(VectorNormalize(v2));
    Scn.Buffer.ScreenVectorIntersectWithPlane(v1, LastPickPos, v2, Result)
  end
  else
    Scn.Buffer.ScreenVectorIntersectWithPlane(v1, LastPickPos,
      CurrentPick.LocalToAbsolute(YHmgVector), Result);
end;

//
// turn
//
procedure TForm1.TrackBar1Change(Sender: TObject);
begin

  dc2.TurnAngle := TrackBar1.Position + 45;

end;

//
// pitch
//
procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  dc1.PitchAngle := TrackBar2.Position - 75;
end;

end.
