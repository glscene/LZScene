unit Unit1;

{$mode objfpc}{$h+}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Math, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
   
  GLCadencer, GLScene, GLObjects, GLGeomObjects,
  GLCoordinates, {GLWin32Viewer,} GLCrossPlatform, GLLCLViewer, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCube1: TGLCube;
    GLCone1: TGLCone;
    GLSphere1: TGLSphere;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLPlane1: TGLPlane;
    Target: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    oldTarget: TGLDummyCube;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
  private
    FValue: Single;
    FNewTarget: TGLBaseSceneObject;
    StartTime: Single;
    AnimationTime: Single;
  public
    function GetCurveValue(aPosition: Single): Single;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

function TForm1.GetCurveValue(aPosition: Single): Single;
begin
  Result := (1 + sin(DegToRad(270 + (aPosition * 180))))/2;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  OldTarget.Position.AsAffineVector := Target.Position.AsAffineVector;
  FNewTarget := GLSceneViewer1.Buffer.GetPickedObject(X,Y);
  StartTime := GLCadencer1.CurrentTime;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
var
  lCurveFactor: Single;
begin
  if newTime < StartTime + AnimationTime then
  begin
    lCurveFactor := GetCurveValue((newTime - StartTime)/AnimationTime);
    if assigned(FNewTarget) then
    begin
      Target.Position.X := OldTarget.Position.X + lCurveFactor * (FNewTarget.Position.X - OldTarget.Position.X);
      Target.Position.Y := OldTarget.Position.Y + lCurveFactor * (FNewTarget.Position.Y - OldTarget.Position.Y);
      Target.Position.Z := OldTarget.Position.Z + lCurveFactor * (FNewTarget.Position.Z - OldTarget.Position.Z);
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AnimationTime := 3; //defines animation duration regardless of range
  FNewTarget := GLCube1;
end;

end.
