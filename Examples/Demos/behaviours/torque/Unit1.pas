{: This is a basic use for TGLBInertia behaviour.<p>

  There are three objects, which we assign three different dampings, and we
  apply a torque to the object under the mouse pointer, other are left along
  with their inertia (and damping makes them progressively reduce their speed).<br>
  There is also a checkbox to double the objects mass.<p>

  Notice how the constant damping stops abruptly the dodecahedron, while the
  the octahedron, once spinned, is slowing down but never really stops.<br>
  However, don't show this sample to your science teacher, since our "torque"
  is actually an angular acceleration in degrees that gets affected by the
  object's mass... Anyway, it looks like a real torque is applied.<p>

  Note that the inertia behaviour could have been accessed directly with a
  TGLBInertia(Behaviours[0]) for all objects in this sample, but using the
  helper function GetOrCreateInertia is a more convenient (and resilient) way,
  since it will automatically add an inertia behaviour to our object if it
  doesn't have one.
}
unit Unit1;

interface

uses
  Forms, GLObjects, GLScene, StdCtrls, ExtCtrls, Controls, Classes,
  GLCadencer, GLViewer, LResources,
  GLPolyhedron, GLCrossPlatform, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Cube: TGLCube;
    Dodecahedron: TGLDodecahedron;
    Octahedron: TGLSphere;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    CheckBox1: TCheckBox;
    DummyCube1: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
  private
    { Déclarations privées }
    lastTime: double;
    pickedObject: TGLBaseSceneObject;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses SysUtils, GLBehaviours;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialize last time
  lastTime := Now * 3600 * 24;
  // Initialize rotation dampings...
  // ...using properties...
  with GetOrCreateInertia(Cube.Behaviours).RotationDamping do
  begin
    Constant := 1;
    Linear := 1;
    Quadratic := 0;
  end;
  // ...using helper function on the TGLBehaviours...
  GetOrCreateInertia(Dodecahedron.Behaviours).RotationDamping.SetDamping(10, 0, 0.01);
  // ...or using helper function directly on the TGLBaseSceneObject
  GetOrCreateInertia(Octahedron).RotationDamping.SetDamping(0, 0, 0.01);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  // Mouse moved, get what's underneath
  pickedObject := GLSceneViewer1.Buffer.GetPickedObject(x, y);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  // apply some "torque" to the pickedObject if any
  if Assigned(pickedObject) then
    GetOrCreateInertia(pickedObject).ApplyTorque(deltaTime, 200, 0, 0);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
var
  i: integer;
  mass: single;
begin
  if CheckBox1.Checked then
    mass := 2
  else
    mass := 1;
  // all our objects are child of the DummyCube1
  for i := 0 to DummyCube1.Count - 1 do
    GetOrCreateInertia(DummyCube1.Children[i]).Mass := mass;
end;


initialization
{$i Unit1.lrs}

end.
