{: This Form demonstrates basic "Pathcontrol" movements.<p>

   You can modified the Looped property of the path to enable the path-looping.
   Set ShowPath property to turn on or turn off the path-displaying 
}
unit Unit1;

interface

uses
  Forms, GLScene, GLObjects, ComCtrls, ExtCtrls, StdCtrls,
  Classes, Controls, GLCadencer, GLBehaviours, Buttons, GLGraph, GLMovement,
  GLViewer, LResources,
  GLCrossPlatform, GLCoordinates, GLBaseClasses, GLUtils,
  GLSimpleNavigation;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    Cube2: TGLCube;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    MoveBtn: TBitBtn;
    Timer1: TTimer;
    Sphere1: TGLSphere;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure FormActivate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure MoveBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure PathTravelStop(Sender: TObject; Path: TGLMovementPath; var Looped: Boolean);
    procedure PathAllTravelledOver(Sender: TObject);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
   SysUtils, GLVectorGeometry;

procedure TForm1.FormActivate(Sender: TObject);
var
  Movement: TGLMovement;
  Path:     TGLMovementPath;
  Node:     TGLPathNode;
begin
  // Create a movement, a path and the first node of the path.
  Movement   := GetOrCreateMovement(Cube2);
  Movement.OnPathTravelStop := PathTravelStop;
  Movement.OnAllPathTravelledOver := PathAllTravelledOver;
  Path       := Movement.AddPath;
  Path.ShowPath := True;

  // Path.StartTime := 2;
  // Path.Looped := True;

  Node       := Path.AddNodeFromObject(Cube2);
  Node.Speed := 4.0;

  // Add a node.
  Node       := Path.AddNode;
  Node.Speed := 4.0;
  Node.PositionAsVector := VectorMake(-10, 0, 0, 1);
  Node.RotationAsVector := VectorMake(0, 0, 0);

  // Add a node.
  Node       := Path.AddNode;
  Node.Speed := 4.0;
  Node.PositionAsVector := VectorMake(0, 5, - 5);
  Node.RotationAsVector := VectorMake(0, 90, 0);

  // Add a node.
  Node       := Path.AddNode;
  Node.Speed := 4.0;
  Node.PositionAsVector := VectorMake(6, - 5, 2);
  Node.RotationAsVector := VectorMake(0, 180, 0);

  // Add a node.
  Node       := Path.AddNode;
  Node.Speed := 4.0;
  Node.PositionAsVector := VectorMake(-6, 0, 0);
  Node.RotationAsVector := VectorMake(0, 259, 0);

  // Activatived the current path.
  Movement.ActivePathIndex := 0;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.MoveBtnClick(Sender: TObject);
var
  Movement: TGLMovement;
begin
  Movement := GetMovement(Cube2);
  if Assigned(Movement) then begin
      Movement.StartPathTravel;
      GLCadencer1.Enabled := True;
  end;
end;

procedure TForm1.PathTravelStop(Sender: TObject; Path: TGLMovementPath; var Looped: Boolean);
begin
   if not Application.Terminated then
      InformationDlg('Path Travel Stopped');
end;

procedure TForm1.PathAllTravelledOver(Sender: TObject);
begin
   if not Application.Terminated then
      InformationDlg('All Path(es) Traveled Over');
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.2f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;


initialization
{$i Unit1.lrs}

end.