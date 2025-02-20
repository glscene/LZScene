unit Unit1;

{$MODE Delphi}

{ : Newton Game Dynamics Physics Engine demo.<p>

  This exemple show how to move a body by calling the pick function of joint.
  Change kinematicControllerOptions.PickModeLinear to feel the difference.
  Most of the code must be written by developer in mouse events.

  <b>History : </b><font size=-1><ul>
  <li>03/02/11 - FP - Update with design time Behaviors
  <li>01/02/11 - FP - Removed unused variable and use only mouseLeft to pick
  <li>31/01/11 - Yar - Updated after Huge update GLNGDManager (thaks Dev)
  <li>17/09/10 - FP - Created by Franck Papouin
  </ul>
}

interface

uses
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  GLNGDManager,
  GLScene,
  GLObjects,
  GLCoordinates,
  GLCadencer,
  GLViewer,
  GLCrossPlatform,
  GLBaseClasses,
  GLVectorGeometry;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Floor: TGLCube;
    GLCube1: TGLCube;
    GLNGDManager1: TGLNGDManager;
    GLSphere1: TGLSphere;
    GLCube2: TGLCube;
    GLCube3: TGLCube;
    GLCube4: TGLCube;
    GLLines1: TGLLines;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
    point3d, FPaneNormal: TVector;

  public
    { Déclarations publiques }
    pickjoint: TNGDJoint;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  pickjoint := TNGDJoint(GLNGDManager1.NewtonJoint.Items[0]);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  GLNGDManager1.Step(deltaTime);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  PickedSceneObject: TGLBaseSceneObject;
begin
  if Button = TMouseButton(mbLeft) then
  begin

    PickedSceneObject := GLSceneViewer1.Buffer.GetPickedObject(X, Y);
    if Assigned(PickedSceneObject) and
      Assigned(GetNGDDynamic(PickedSceneObject)) then
      pickjoint.ParentObject := PickedSceneObject
    else
      exit;

    point3d := VectorMake(GLSceneViewer1.Buffer.PixelRayToWorld(X, Y));
    // Attach the body
    pickjoint.KinematicControllerPick(point3d, paAttach);

    if Assigned(GLSceneViewer1.Camera.TargetObject) then
      FPaneNormal := GLSceneViewer1.Camera.AbsoluteVectorToTarget
    else
      FPaneNormal := GLSceneViewer1.Camera.AbsoluteDirection;
  end;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  point2d, GotoPoint3d: TVector;
begin

  if ssLeft in Shift then
  begin
    // Get the screenPoint with opengl correction [Height - Y] for the next function
    point2d := VectorMake(X, GLSceneViewer1.Height - Y, 0, 0);

    // Get the intersect point between the plane [parallel to camera] and mouse position
    if GLSceneViewer1.Buffer.ScreenVectorIntersectWithPlane(point2d,
      point3d, FPaneNormal, GotoPoint3d) then
      // Move the body to the new position
      pickjoint.KinematicControllerPick(GotoPoint3d, paMove);

  end
  else
    pickjoint.KinematicControllerPick(GotoPoint3d, paDetach);

end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  // Detach the body
  if Button = TMouseButton(mbLeft) then
    pickjoint.KinematicControllerPick(NullHmgVector, paDetach);
end;

end.

