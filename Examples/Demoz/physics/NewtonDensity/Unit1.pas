unit Unit1;

{$MODE Delphi}

{ : Newton Game Dynamics Physics Engine demo.<p>

  This demo explain how to use customForceAndTorque, and show the newton api
  buoyancy effect as exemple.

  Density unit is the number of mass unit per volume unit: D=M/V
  Body volume is calculated by Newton, and Mass is the product result of
  Density*Volume.

  If fluidDensity=bodyDensity, the body will be immerged like a submarine.

  Bugs:
  -Viscosities does not seem to affect buoyancy.
  -Small bodies have huge Viscosities.
  -Sphere and Capsule flows when their density equal fluid density.
  (For sphere we can correct this by multiplying fluidDensity by 1.43)

  Density is also an important parameter when two bodies collide.
  The middle mouse button shoot a small cube with impulse.
  You can see the result when the cube hit the paper ball or the lead ball.

  <b>History : </b><font size=-1><ul>
  <li>03/02/11 - FP - Update with design time Behaviors
  <li>31/01/11 - FP - Update for GLNGDManager
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
  GLObjects,
  GLScene,
  GLCoordinates,
  GLSimpleNavigation,
  GLCadencer,
  GLViewer,
  GLCrossPlatform, GLLCLViewer,
  GLBaseClasses,
  GLVectorGeometry,
  StdCtrls,
  GLHUDObjects,
  GLBitmapFont,
  GLWindowsFont,
  Spin,
  NewtonImport,
  GLGeomObjects;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLPlane1: TGLPlane;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    Mag: TGLDummyCube;
    SubMarine: TGLCube;
    GLPaperSphere: TGLSphere;
    GLLeadSphere: TGLSphere;
    GLCube1: TGLCube;
    SpinEdit1: TSpinEdit;
    GLHUDText1: TGLHUDText;
    GLNGDManager1: TGLNGDManager;
    SpinEdit2: TSpinEdit;
    GLHUDText2: TGLHUDText;
    GLHUDText3: TGLHUDText;
    SpinEdit3: TSpinEdit;
    GLCube2: TGLCube;
    GLCone1: TGLCone;
    GLCylinder1: TGLCylinder;
    obj: TGLDummyCube;
    GLCapsule1: TGLCapsule;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure MyForceAndTorqueDensity(const cbody: PNewtonBody;
      timestep: NGDFloat; threadIndex: Integer);

    procedure Shoot;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

function BuoyancyPlaneCallback(const collisionID: Integer; context: Pointer;
  const globalSpaceMatrix: PNGDFloat; globalSpacePlane: PNGDFloat): Integer;
  cdecl;
var
  BodyMatrix: TMatrix;
  PlaneEquation: TVector;
  pv: PVector;
  MyForm: TForm1;
begin
  // Get the matrix of the actual body
  BodyMatrix := PMatrix(globalSpaceMatrix)^;

  MyForm := TForm1(context);

  // this is the 4-value vector that represents the plane equation for
  // the buoyancy surface
  // This can be used to simulate boats and lighter than air vehicles etc..
  PlaneEquation := MyForm.GLPlane1.Direction.AsVector;
  // the distance along this normal, to the origin.
  PlaneEquation[3] := MyForm.GLPlane1.Position.Y;

  PVector(globalSpacePlane)^ := PlaneEquation;

  Result := 1;
end;

procedure TForm1.Shoot;
var
  Ball: TGLCube;
  NGDDyn: TGLNGDDynamic;
begin
  Ball := TGLCube.CreateAsChild(Mag);
  Ball.CubeWidth := 0.5;
  Ball.CubeHeight := 0.5;
  Ball.CubeDepth := 0.5;
  Ball.AbsolutePosition := GLCamera1.AbsolutePosition;
  NGDDyn := GetOrCreateNGDDynamic(Ball);
  NGDDyn.Manager := GLNGDManager1;
  NGDDyn.Density := 10;
  NGDDyn.UseGravity := false;
  NGDDyn.LinearDamping := 0;

  // Add impulse in the camera direction
  NGDDyn.AddImpulse(VectorScale(GLCamera1.AbsoluteVectorToTarget, 100),
    Ball.AbsolutePosition);

end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  // To use Buoyancy effect, set a custom forceAndTorqueEvent were you can call
  // NewtonBodyAddBuoyancyForce API function
  for I := 0 to obj.Count - 1 do
    GetNGDDynamic(obj[I]).CustomForceAndTorqueEvent := MyForceAndTorqueDensity;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLNGDManager1.Step(deltaTime);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton(mbMiddle) then
    Shoot;
end;

procedure TForm1.MyForceAndTorqueDensity(const cbody: PNewtonBody;
  timestep: NGDFloat; threadIndex: Integer);
var
  worldGravity: TVector;
  NGDDyn: TGLNGDDynamic;
  fluidDensity, fluidLinearViscosity, fluidAngularViscosity: Single;
begin
  worldGravity := GLNGDManager1.Gravity.AsVector;
  NGDDyn := TGLNGDDynamic(NewtonBodyGetUserData(cbody));

  // Add gravity to body: Weight= mass*gravity
  ScaleVector(worldGravity, NGDDyn.mass);
  NewtonBodyAddForce(cbody, @worldGravity);

  fluidDensity := SpinEdit1.Value;
  fluidLinearViscosity := SpinEdit2.Value / 10;
  fluidAngularViscosity := SpinEdit3.Value / 10;

  // We send Self as context for the callback
  NewtonBodyAddBuoyancyForce(cbody, fluidDensity / NGDDyn.mass,
    fluidLinearViscosity, fluidAngularViscosity, @worldGravity,
    @BuoyancyPlaneCallback, self);
end;

end.
