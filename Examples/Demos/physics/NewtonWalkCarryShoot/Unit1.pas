unit Unit1;

{$MODE Delphi}

{ : Newton Game Dynamics Physics Engine demo.<p>

  This exemple show how to move with keyboard.
  There is already a NewtonWalkAndCarry demo but I wanted to do with another way.
  Press X key to spawn New model.
  Press C key to Respawn Body.
  Press Middle Mouse to grab.
  Press MouseLeft to shoot.
  Hold MouseRight to look

  Body has two Upvectors joint, one in Y one in X.

  <b>History : </b><font size=-1><ul>
  <li>04/02/11 - FP - Created by Franck Papouin. Put directive for lazarus
  </ul>
}
interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, LCLType,
  Dialogs, GLScene, GLObjects, GLCoordinates, GLCadencer, GLNGDManager,
  GLSimpleNavigation, GLViewer, GLCrossPlatform, GLBaseClasses,
  GLVectorFileObjects, GLGeomObjects, GLHUDObjects;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLNGDManager1: TGLNGDManager;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    World: TGLDummyCube;
    GLLines1: TGLLines;
    Beer: TGLFreeForm;
    Mushroom: TGLFreeForm;
    Chair: TGLFreeForm;
    Teapot: TGLFreeForm;
    Map: TGLFreeForm;
    GLHUDCross: TGLHUDSprite;
    Body: TGLTorus;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    { Déclarations privées }
    procedure ScaleMesh(freeform: TGLFreeForm; val: single);
    procedure MoveCam(const deltaTime, newTime: double);
    procedure MoveGrab;
  public
    { Déclarations publiques }
    grabJoint: TNGDJoint;
  end;

var
  Form1: TForm1;

implementation

uses
  GLFile3ds, GLVectorGeometry, GlMaterial, GLKeyboard;

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetCurrentDir('..\..\..\Demos\Media\');

  // Set Mesh and scale their vertice position
  // because matrix scale is not supported in newton
  Map.LoadFromFile('ngdmap1.3ds');
  Map.Direction.SetVector(0, 1, 0);
  Map.Translate(0, -3, 0);

  {$IFNDEF FPC}
  Beer.LoadFromFile('beer.3ds');
  ScaleMesh(Beer, 0.25);           //bug with Lazarus
  Beer.Translate(-5, 10, 10);
  {$ENDIF}

  Mushroom.LoadFromFile('mushroom.3ds');
  ScaleMesh(Mushroom, 0.1);
  Mushroom.Direction.SetVector(0, 1, 0);
  Mushroom.Translate(0, 0, -10);
  Mushroom.Material.FaceCulling := fcNoCull;
  Mushroom.Material.FrontProperties.Emission.SetColor(1, 0.5, 0.5, 0.5);

  Chair.LoadFromFile('ngdchair.3ds');
  Chair.Direction.SetVector(0, 1, 0);
  Chair.Translate(0, 0, -5);

  Teapot.LoadFromFile('teapot.3ds');
  ScaleMesh(Teapot, 0.05);
  Teapot.Direction.SetVector(0, 1, 0);
  Teapot.Translate(0, 0, 10);

  // Create Physic behavior
  GetOrCreateNGDStatic(Map).NGDNewtonCollisions := nc_Tree;
  GetNGDStatic(Map).Manager := GLNGDManager1;

  // nc_Convex use ConvexCollisionTolerance at creation wich is the resolution
  // for collision shape. Bigger the value, lower the collision match,
  // but give higher performance (use mdShowGeometry to see the difference at runtime)
  GetOrCreateNGDDynamic(Chair).NGDNewtonCollisions := nc_Convex;
  GetNGDDynamic(Chair).Manager := GLNGDManager1;

   {$IFNDEF FPC}
  GetOrCreateNGDDynamic(Beer).NGDNewtonCollisions := nc_Convex;
  GetNGDDynamic(Beer).Manager := GLNGDManager1;
   {$ENDIF}

  GetOrCreateNGDDynamic(Teapot).NGDNewtonCollisions := nc_Convex;
  GetNGDDynamic(Teapot).Manager := GLNGDManager1;

  GetOrCreateNGDDynamic(Mushroom).NGDNewtonCollisions := nc_Convex;
  GetNGDDynamic(Mushroom).Manager := GLNGDManager1;

  // Move camera in target
  GLCamera1.Parent := GLCamera1.TargetObject;

  // Create the PickJoint
  grabJoint := TNGDJoint.Create(GLNGDManager1.NewtonJoint);
  grabJoint.JointType := nj_KinematicController;
  grabJoint.KinematicControllerOptions.PickModeLinear := True;
  grabJoint.KinematicControllerOptions.LinearFriction := 100;

  // Set some parameters
  GetNGDDynamic(Body).AutoSleep := False;
  GLHUDCross.Height := 2;
  GLHUDCross.Width := 2;
  GLCamera1.NearPlaneBias := 0.1;
  GLCamera1.DepthOfView := 1E15;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
var
  newFreeform: TGLFreeForm;
  I, rand: integer;
begin
  if Key = 'x' then
  begin

    newFreeform := TGLFreeForm.CreateAsChild(GLDummyCube1);
   {$IFNDEF FPC}
    newFreeform.LoadFromFile('HighPolyObject.3ds');      //bug with Lazarus
    ScaleMesh(newFreeform, 0.05);
   {$ENDIF}


 {$IFDEF FPC}
    newFreeform.LoadFromFile('ngdchair.3ds');
    newFreeform.Direction.SetVector(0, 1, 0);
 {$ENDIF}

    // Keep only one mesh in this newFreeform
    for I := 0 to newFreeform.MeshObjects.Count - 2 do
    begin
      rand := Random(newFreeform.MeshObjects.Count);
      newFreeform.MeshObjects.Delete(rand);
    end;

    GetOrCreateNGDDynamic(newFreeform).NGDNewtonCollisions := nc_Convex;
    GetNGDDynamic(newFreeform).Manager := GLNGDManager1;
  end;
  if Key = 'c' then
    GetNGDDynamic(Body).NewtonBodyMatrix := IdentityHmgMatrix;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  GLNGDManager1.Step(deltaTime);

  MoveCam(deltaTime, newTime);
  MoveGrab;

  // Set a point on screen to see where you shoot or grab
  GLHUDCross.Position.X := GLSceneViewer1.Width div 2;
  GLHUDCross.Position.Y := GLSceneViewer1.Height div 2;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  Ball: TGLSphere;
  delta: Tvector;

  PickedSceneObject: TGLBaseSceneObject;
  point3d: Tvector;
  MyX, MyY: integer;
begin
  // Shoot a Bullet
  if Button = TMouseButton(mbLeft) then
  begin
    Ball := TGLSphere.CreateAsChild(GLDummyCube1);
    Ball.Radius := 0.1;
    Ball.AbsolutePosition := GLCamera1.TargetObject.AbsolutePosition;
    delta := VectorScale(GLCamera1.AbsoluteVectorToTarget, 2);
    Ball.Translate(delta[0], delta[1], delta[2]);
    GetOrCreateNGDDynamic(Ball);
    GetNGDDynamic(Ball).Manager := GLNGDManager1;
    GetNGDDynamic(Ball).Density := 100;
    GetNGDDynamic(Ball).LinearDamping := 0.5;
    // Add impulse in the camera direction
    GetNGDDynamic(Ball).AddImpulse
    (VectorScale(GLCamera1.AbsoluteVectorToTarget, 100),
      Ball.AbsolutePosition);
  end;

  // Start Grab
  if Button = TMouseButton(mbMiddle) then
  begin
    MyX := GLSceneViewer1.Width div 2;
    MyY := GLSceneViewer1.Height div 2;
    PickedSceneObject := GLSceneViewer1.Buffer.GetPickedObject(MyX, MyY);

    if Assigned(PickedSceneObject) and
      Assigned(GetNGDDynamic(PickedSceneObject)) then
      grabJoint.ParentObject := PickedSceneObject
    else
      exit;

    point3d := PickedSceneObject.AbsolutePosition;
    // Attach the body
    grabJoint.KinematicControllerPick(point3d, paAttach);

  end;

end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  // Detach the body
  if Button = TMouseButton(mbMiddle) then
    grabJoint.KinematicControllerPick(NullHmgVector, paDetach);
end;

procedure TForm1.ScaleMesh(freeform: TGLFreeForm; val: single);
var
  I, J: integer;
begin
  for J := 0 to freeform.MeshObjects.Count - 1 do
    for I := 0 to freeform.MeshObjects[J].Vertices.Count - 1 do
    begin
      freeform.MeshObjects[J].Vertices[I] :=
        VectorScale(freeform.MeshObjects[J].Vertices[I], val);
    end;
end;

procedure TForm1.MoveCam(const deltaTime, newTime: double);
var
  f: Tvector;
  fup, fdn, flf, frg: Tvector;
  Bup, Bdn, Blf, Brg: boolean;
  NGDDyn: TGLNGDDynamic;
begin
  Bup := IsKeyDown('w') or IsKeyDown('z') or IsKeyDown(VK_UP);
  Bdn := IsKeyDown('s') or IsKeyDown(VK_DOWN);
  Blf := IsKeyDown('d') or IsKeyDown(VK_LEFT);
  Brg := IsKeyDown('a') or IsKeyDown('q') or IsKeyDown(VK_RIGHT);

  if Bup then
    fup := GLCamera1.AbsoluteVectorToTarget
  else
    fup := VectorMake(0, 0, 0, 0);

  if Bdn then
    fdn := VectorNegate(GLCamera1.AbsoluteVectorToTarget)
  else
    fdn := VectorMake(0, 0, 0, 0);

  if Blf then
    frg := GLCamera1.AbsoluteRightVectorToTarget
  else
    frg := VectorMake(0, 0, 0, 0);

  if Brg then
    flf := VectorNegate(GLCamera1.AbsoluteRightVectorToTarget)
  else
    flf := VectorMake(0, 0, 0, 0);

  NGDDyn := GetNGDDynamic(GLCamera1.TargetObject);

  if Bup or Bdn or Blf or Brg then
  begin

    // Add every vector
    f := VectorAdd(fup, fdn);
    f := VectorAdd(f, frg);
    f := VectorAdd(f, flf);
    f[1] := 0; // Do not allow the body to go up or down
    NormalizeVector(f);

    // Move the body
    if NGDDyn.AppliedVelocity.VectorLength < 5 then
      NGDDyn.AddImpulse(f, GLCamera1.TargetObject.AbsolutePosition);

  end
  else
  begin
    // Slow down the body if the user stop pushing keys
    if NGDDyn.AppliedVelocity.VectorLength > 3 then
      NGDDyn.SetVelocity(VectorScale(NGDDyn.AppliedVelocity.AsVector, 0.5));
  end;

end;

procedure TForm1.MoveGrab;
var
  point3d: Tvector;
  delta: Tvector;
begin
  // Move the object in 3 unit front of GLCamera1.TargetObject
  if IsKeyDown(VK_MBUTTON) then
  begin

    point3d := GLCamera1.TargetObject.AbsolutePosition;
    delta := GLCamera1.AbsoluteVectorToTarget;

    while VectorLength(delta) < 3 do
      delta := VectorScale(delta, 2);

    point3d := VectorAdd(point3d, delta);

    grabJoint.KinematicControllerPick(point3d, paMove);
  end;
end;

end.

