unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLODEManager, GLScene, GLObjects, GLGeomObjects, GLCadencer,
  GLLCLViewer, GLShadowPlane, StdCtrls, ComCtrls,
  ExtCtrls, GLGraph, GLVectorTypes, GLVectorGeometry, GLODECustomColliders,
  GLCrossPlatform, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    ODEObjects: TGLDummyCube;
    Panel1: TPanel;
    GLODEManager1: TGLODEManager;
    Spawn: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    GLRenderPoint1: TGLRenderPoint;
    GLHeightField1: TGLHeightField;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    TrackBar1: TTrackBar;
    Label2: TLabel;
    GLPlane1: TGLPlane;
    ComboBox2: TComboBox;
    Label3: TLabel;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SpawnClick(Sender: TObject);
    procedure GLHeightField1GetHeight(const x, y: Single; var z: Single;
      var color: TVector4f; var texPoint: TTexPoint);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx,my : integer;

    procedure DoSphere;
    procedure DoBox;
    procedure DoCapsule;
    procedure DoCylinder;
    // CONE IS CURRENTLY UNSUPPOETED FOR ODE 0.9
    //procedure DoCone;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLODEManager1.Step(deltaTime);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx:=x;
  my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my-y,mx-x);
  mx:=x;
  my:=y;
end;

procedure TForm1.SpawnClick(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
    0 : DoSphere;
    1 : DoBox;
    2 : DoCapsule;
    3 : DoCylinder;
    //4 : DoCone; // CONE IS CURRENTLY UNSUPPOETED FOR ODE 0.9
  end;
end;

procedure TForm1.DoSphere;
var
  sphere : TGLSphere;
  dyn : TGLODEDynamic;
begin
  sphere:=TGLSphere(ODEObjects.AddNewChild(TGLSphere));
  sphere.Position.SetPoint(5*random-2.5,2,5*random-2.5);
  sphere.Radius:=0.3*(Random+1);
  dyn:=TGLODEDynamic.Create(sphere.Behaviours);
  with TODEElementSphere(dyn.AddNewElement(TODEElementSphere)) do // ELEMENTS MUST BE ADDED BEFORE SETTING MANAGER
    Radius:=sphere.Radius;
  dyn.Manager:=GLODEManager1;
end;

procedure TForm1.DoBox;
var
  cube : TGLCube;
  dyn : TGLODEDynamic;
begin
  cube:=TGLCube(ODEObjects.AddNewChild(TGLCube));
  cube.Position.SetPoint(5*random-2.5,2,5*random-2.5);
  cube.CubeWidth:=0.5*(Random+1);
  cube.CubeHeight:=0.5*(Random+1);
  cube.CubeDepth:=0.5*(Random+1);
  dyn:=TGLODEDynamic.Create(cube.Behaviours);
  with TODEElementBox(dyn.AddNewElement(TODEElementBox)) do begin // ELEMENTS MUST BE ADDED BEFORE SETTING MANAGER
    BoxWidth:=cube.CubeWidth;
    BoxHeight:=cube.CubeHeight;
    BoxDepth:=cube.CubeDepth;
  end;
  dyn.Manager:=GLODEManager1;
end;

procedure TForm1.DoCapsule;
var
  capsule : TGLCylinder;
  dyn : TGLODEDynamic;
begin
  capsule:=TGLCylinder(ODEObjects.AddNewChild(TGLCylinder));
  with capsule do begin
    Position.SetPoint(5*random-2.5,2,5*random-2.5);
    BottomRadius:=0.25*(Random+1);
    TopRadius:=BottomRadius;
    Height:=random+1;
    Parts:=[cySides];
    with TGLSphere(AddNewChild(TGLSphere)) do begin
      Position.Y:=0.5*Height;
      Radius:=BottomRadius;
      Bottom:=0;
    end;
    with TGLSphere(AddNewChild(TGLSphere)) do begin
      Position.Y:=-0.5*Height;
      Radius:=BottomRadius;
      Top:=0;
    end;
  end;
  dyn:=TGLODEDynamic.Create(capsule.Behaviours);
  with TODEElementCapsule(dyn.AddNewElement(TODEElementCapsule)) do begin // ELEMENTS MUST BE ADDED BEFORE SETTING MANAGER
    Radius:=capsule.BottomRadius;
    Length:=capsule.Height;
    Direction.SetVector(0,1,0);
    Up.SetVector(0,0,1);
  end;
  dyn.Manager:=GLODEManager1;
end;

procedure TForm1.DoCylinder;
var
  cylinder : TGLCylinder;
  dyn : TGLODEDynamic;
begin
  cylinder:=TGLCylinder(ODEObjects.AddNewChild(TGLCylinder));
  with cylinder do begin
    Position.SetPoint(5*random-2.5,2,5*random-2.5);
    BottomRadius:=0.25*(Random+1);
    TopRadius:=BottomRadius;
    Height:=random+1;
  end;
  dyn:=TGLODEDynamic.Create(cylinder.Behaviours);
  with TODEElementCylinder(dyn.AddNewElement(TODEElementCylinder)) do begin // ELEMENTS MUST BE ADDED BEFORE SETTING MANAGER
    Radius:=cylinder.BottomRadius;
    Length:=cylinder.Height;
  end;
  dyn.Manager:=GLODEManager1;
end;

// CONE IS CURRENTLY UNSUPPOETED FOR ODE 0.9
{
procedure TForm1.DoCone;
var
  cone : TGLCone;
  dyn : TGLODEDynamic;
begin
  cone:=TGLCone(ODEObjects.AddNewChild(TGLCone));
  with cone do begin
    Position.SetPoint(5*random-2.5,2,5*random-2.5);
    BottomRadius:=0.25*(Random+1);
    Height:=random+1;
  end;
  dyn:=TGLODEDynamic.Create(cone.Behaviours);
  dyn.Manager:=GLODEManager1;
  with TODEElementCone(dyn.AddNewElement(TODEElementCone)) do begin
    Radius:=cone.BottomRadius;
    Length:=cone.Height;
    Direction.SetVector(0,1,0);
    Up.SetVector(0,0,1);
    Position.SetPoint(0,-cone.Height/2,0);
  end;
end;
}

procedure TForm1.GLHeightField1GetHeight(const x, y: Single; var z: Single;
  var color: TVector4f; var texPoint: TTexPoint);
begin
  z:=0.5*cos(x)*sin(y);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  GLODEManager1.Visible:=CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  TGLODEHeightField(GLHeightField1.Behaviours[0]).RenderContacts:=CheckBox2.Checked;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  with TGLODEHeightField(GLHeightField1.Behaviours[0]) do
    ContactResolution:=0.25+(10-TrackBar1.Position)/20;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  if ComboBox2.ItemIndex = 0 then begin
    GLPlane1.Visible:=True;
    GetODEStatic(GLPlane1).Manager:=GLODEManager1;
  end else begin
    GLPlane1.Visible:=False;
    GetODEStatic(GLPlane1).Manager:=nil;
  end;

  if ComboBox2.ItemIndex = 1 then begin
    GLHeightField1.Visible:=True;
    GetODEHeightField(GLHeightField1).Manager:=GLODEManager1;
  end else begin
    GLHeightField1.Visible:=False;
    GetODEHeightField(GLHeightField1).Manager:=nil;
  end;
end;

end.
