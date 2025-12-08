{: This sample illustrates basic user-driven camera movements.<p>

  I'm using the GLScene built-in camera movement methods. The camera object is
  a child of its target dummy cube (this means that the camera is translated
  when its target is translate, which is good for flyover/scrolling movements).<p>

  Movements in this sample are done by moving the mouse with a button
  pressed, left button will translate the dummy cube (and the camera),
  right button will rotate the camera around the target, shift+right will
   rotate the object in camera's axis.<br>
  Mouse Wheel allows zooming in/out.<br>
  '7', '9' rotate around the X vector (in red, absolute).<br>
  '4', '6' rotate around the Y vector (in green, absolute).<br>
  '1', '3' rotate around the Z vector (in blue, absolute).<br>
}
unit Unit1;

{$MODE Delphi}

interface

uses
  Forms, GLScene, GLObjects, Classes, Controls, GLTeapot,
  GLLCLViewer, GLCrossPlatform, GLCoordinates, GLBaseClasses, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    Teapot1: TGLTeapot;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure FormKeyPress(Sender: TObject; var Key: char);
  private
    { Déclarations privées }
    mdx, mdy: integer;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses GLVectorGeometry, Math;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  // store mouse coordinates when a button went down
  mdx := x;
  mdy := y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  dx, dy: integer;
  v: TVector;
begin
  // calculate delta since last move or last mousedown
  dx := mdx - x;
  dy := mdy - y;
  mdx := x;
  mdy := y;
  if ssLeft in Shift then
  begin
    if ssShift in Shift then
    begin
      // right button with shift rotates the teapot
      // (rotation happens around camera's axis)
      GLCamera1.RotateObject(Teapot1, dy, dx);
    end
    else
    begin
      // right button without shift changes camera angle
      // (we're moving around the parent and target dummycube)
      GLCamera1.MoveAroundTarget(dy, dx);
    end;
  end
  else if Shift = [ssRight] then
  begin
    // left button moves our target and parent dummycube
    v := GLCamera1.ScreenDeltaToVectorXY(dx, -dy,
      0.12 * GLCamera1.DistanceToTarget / GLCamera1.FocalLength);
    DummyCube1.Position.Translate(v);
    // notify camera that its position/target has been changed
    GLCamera1.TransformationChanged;
  end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  // Note that 1 wheel-step induces a WheelDelta of 120,
  // this code adjusts the distance to target with a 10% per wheel-step ratio
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  with Teapot1 do
    case Key of
      '7': RotateAbsolute(-15, 0, 0);
      '9': RotateAbsolute(+15, 0, 0);
      '4': RotateAbsolute(0, -15, 0);
      '6': RotateAbsolute(0, +15, 0);
      '1': RotateAbsolute(0, 0, -15);
      '3': RotateAbsolute(0, 0, +15);
    end;
end;

end.

