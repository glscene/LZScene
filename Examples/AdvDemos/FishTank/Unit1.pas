unit Unit1;


interface

uses
  LCLType,
  SysUtils,
  Math, Classes, DateUtils,
  Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls,
  GLBehaviours, GLScene, GLObjects, GLCadencer, GLMaterial, GLColor,
  GLVectorGeometry, GLGeomObjects,
  GLVectorFileObjects, GLTexture, GLFileMS3D, GLFile3DS,
  GLCoordinates, GLCrossPlatform, GLLCLViewer,
  //FishTank
  uVehicle;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    TimerForFps: TTimer;
    ArrowForDirection: TGLArrowLine;
    Panel1: TPanel;
    Label1: TLabel;
    TimerForSpeed: TTimer;
    GLFreeForm1: TGLFreeForm;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Button1: TButton;
    Button2: TButton;
    GLCamera2: TGLCamera;
    GLCube2: TGLCube;
    Label3: TLabel;
    CollisionSphere: TGLSphere;
    GLArrowLine4: TGLArrowLine;
    GLDummyCube2: TGLDummyCube;
    Target1: TGLDummyCube;
    Target2: TGLDummyCube;
    Target3: TGLDummyCube;
    Target4: TGLDummyCube;
    BtnForTarget1: TButton;
    BtnForTarget2: TButton;
    BtnForTarget3: TButton;
    BtnForTarget4: TButton;
    BSphere: TGLSphere;
    LinesForDirection: TGLLines;
    GLCube1: TGLCube;
    GLArrowLine1: TGLArrowLine;
    GLCamera3: TGLCamera;
    GLSphere1: TGLSphere;
    GLLines1: TGLLines;
    GLCube3: TGLCube;
    GLArrowLine2: TGLArrowLine;
    GLCamera4: TGLCamera;
    GLSphere2: TGLSphere;
    GLLines2: TGLLines;
    GLCube4: TGLCube;
    GLArrowLine3: TGLArrowLine;
    GLCamera5: TGLCamera;
    GLSphere3: TGLSphere;
    GLLines3: TGLLines;
    GLCube5: TGLCube;
    GLArrowLine5: TGLArrowLine;
    GLCamera6: TGLCamera;
    GLSphere4: TGLSphere;
    GLLines4: TGLLines;
    GLCube6: TGLCube;
    GLArrowLine6: TGLArrowLine;
    GLCamera7: TGLCamera;
    GLSphere5: TGLSphere;
    GLLines5: TGLLines;
    GLCube7: TGLCube;
    GLArrowLine7: TGLArrowLine;
    GLCamera8: TGLCamera;
    GLSphere6: TGLSphere;
    GLLines6: TGLLines;
    GLCube8: TGLCube;
    GLArrowLine8: TGLArrowLine;
    GLCamera9: TGLCamera;
    GLSphere7: TGLSphere;
    GLLines7: TGLLines;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
    procedure TimerForFpsTimer(Sender: TObject);
    procedure TimerForSpeedTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BtnForTarget1Click(Sender: TObject);
    procedure BtnForTarget2Click(Sender: TObject);
    procedure BtnForTarget3Click(Sender: TObject);
    procedure BtnForTarget4Click(Sender: TObject);
  private

    FSteeringManager: TGLVehicleManager;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}


procedure TForm1.FormCreate(Sender: TObject);
var
  newSteering: TGLBVehicle;
begin


  BSphere.Radius := GLCube2.BoundingSphereRadius;

  GLFreeForm1.LoadFromFile('BoxedIn.3ds');
  GLFreeForm1.BuildOctree;

  Randomize;

  FSteeringManager := TGLVehicleManager.Create(nil);

  FSteeringManager.WorldCollisionMap := GLFreeForm1;
  newSteering := GetOrCreateVehicle(GLCube2);
  with newSteering do
  begin
    Manager := FSteeringManager;
    Mass := 25;
    MaxSpeed := 15;
    Speed := 0;
    CollisionObject := CollisionSphere;
    Wander.Rate := 5;
    Wander.Strength := 10;
    Wander.Ratio := 1;
    Seek.Target := nil;
    Seek.Ratio := 1;
    Flee.Target := GLCube1;
    Flee.Ratio := 1;
    WorldCollision.Ratio := 1;
    WorldCollision.TurnRate := 0.5;
  end;

  newSteering := GetOrCreateVehicle(GLCube1);
  with newSteering do
  begin
    Manager := FSteeringManager;
    Mass := 25;
    MaxSpeed := 12;
    Speed := 0;
    CollisionObject := CollisionSphere;
    Wander.Rate := 1;
    Wander.Strength := 10;
    Wander.Ratio := 1;
    Seek.Target := GLCube2;
    Seek.Ratio := 1;
    WorldCollision.Ratio := 1;
  end;

  newSteering := GetOrCreateVehicle(GLCube3);
  with newSteering do
  begin
    Manager := FSteeringManager;
    Mass := 15;
    MaxSpeed := 10;
    Speed := 0;
    CollisionObject := CollisionSphere;
    Wander.Rate := 5;
    Wander.Strength := 10;
    Wander.Ratio := 1;
    Seek.Target := nil;
    Seek.Ratio := 1;
    Flee.Target := nil;
    Flee.Ratio := 0;
    WorldCollision.Ratio := 1;
    WorldCollision.TurnRate := 0.3;
  end;

  newSteering := GetOrCreateVehicle(GLCube4);
  with newSteering do
  begin
    Manager := FSteeringManager;
    Mass := 15;
    MaxSpeed := 8;
    Speed := 0;
    CollisionObject := CollisionSphere;
    Wander.Rate := 5;
    Wander.Strength := 10;
    Wander.Ratio := 1;
    Seek.Target := nil;
    Seek.Ratio := 1;
    Flee.Target := nil;
    Flee.Ratio := 0;
    WorldCollision.Ratio := 1;
    WorldCollision.TurnRate := 0.3;
  end;

  newSteering := GetOrCreateVehicle(GLCube5);
  with newSteering do
  begin
    Manager := FSteeringManager;
    Mass := 15;
    MaxSpeed := 12;
    Speed := 0;
    CollisionObject := CollisionSphere;
    Wander.Rate := 5;
    Wander.Strength := 10;
    Wander.Ratio := 1;
    Seek.Target := nil;
    Seek.Ratio := 1;
    Flee.Target := nil;
    Flee.Ratio := 0;
    WorldCollision.Ratio := 1;
    WorldCollision.TurnRate := 0.3;
  end;

  newSteering := GetOrCreateVehicle(GLCube6);
  with newSteering do
  begin
    Manager := FSteeringManager;
    Mass := 15;
    MaxSpeed := 5;
    Speed := 0;
    CollisionObject := CollisionSphere;
    Wander.Rate := 5;
    Wander.Strength := 10;
    Wander.Ratio := 1;
    Seek.Target := nil;
    Seek.Ratio := 1;
    Flee.Target := nil;
    Flee.Ratio := 0;
    WorldCollision.Ratio := 1;
    WorldCollision.TurnRate := 0.3;
  end;

  newSteering := GetOrCreateVehicle(GLCube7);
  with newSteering do
  begin
    Manager := FSteeringManager;
    Mass := 10;
    MaxSpeed := 15;
    Speed := 0;
    CollisionObject := CollisionSphere;
    Wander.Rate := 5;
    Wander.Strength := 10;
    Wander.Ratio := 1;
    Seek.Target := nil;
    Seek.Ratio := 1;
    Flee.Target := nil;
    Flee.Ratio := 0;
    WorldCollision.Ratio := 1;
    WorldCollision.TurnRate := 0.3;
  end;

  newSteering := GetOrCreateVehicle(GLCube8);
  with newSteering do
  begin
    Manager := FSteeringManager;
    Mass := 11;
    MaxSpeed := 9;
    Speed := 0;
    CollisionObject := CollisionSphere;
    Wander.Rate := 5;
    Wander.Strength := 10;
    Wander.Ratio := 1;
    Seek.Target := nil;
    Seek.Ratio := 1;
    Flee.Target := nil;
    Flee.Ratio := 0;
    WorldCollision.Ratio := 1;
    WorldCollision.TurnRate := 0.3;
  end;

  FSteeringManager.SteerInterval := 0.02;

end;

procedure TForm1.FormShow(Sender: TObject);
begin
  TimerForSpeed.Enabled:=True;
  TimerForFps.Enabled:=True;
  GLCadencer1.Enabled:=true;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  GLDummyCube2.Position.X := cos(newTime * pi / 4.2) * 20;
  GLDummyCube2.Position.y := sin(newTime * pi / 7.1) * 5;
  GLDummyCube2.Position.z := sin(newTime * pi / 5) * 20;
  FSteeringManager.DoSteering;
  LinesForDirection.Direction.AsVector := GetOrCreateVehicle(GLCube2).Accumulator.AsVector;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.TimerForFpsTimer(Sender: TObject);
begin
  Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;

  Label1.Caption := FloatToStr(RoundTo(GetOrCreateVehicle(GLCube2).Speed, -2));
end;

procedure TForm1.TimerForSpeedTimer(Sender: TObject);
begin
  //  TimerForSpeed.Interval := trunc(5000 * random) + 1000;
  //  with GetOrCreateVehicle(GLCube2) do
  //    MaxSpeed := RandomRange(1, 20);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  with GetOrCreateVehicle(GLCube2) do
    MaxSpeed := MaxSpeed + 5;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  with GetOrCreateVehicle(GLCube2) do
    MaxSpeed := MaxSpeed - 5;
end;

procedure TForm1.BtnForTarget1Click(Sender: TObject);
begin
  Target1.EdgeColor.Color := clrYellow;
  GetOrCreateVehicle(GLCube2).Seek.Target := Target1;
  Target2.EdgeColor.Color := clrLimeGreen;
  Target3.EdgeColor.Color := clrLimeGreen;
  Target4.EdgeColor.Color := clrLimeGreen;
end;

procedure TForm1.BtnForTarget2Click(Sender: TObject);
begin
  Target1.EdgeColor.Color := clrLimeGreen;
  Target2.EdgeColor.Color := clrYellow;
  GetOrCreateVehicle(GLCube2).Seek.Target := Target2;
  Target3.EdgeColor.Color := clrLimeGreen;
  Target4.EdgeColor.Color := clrLimeGreen;
end;

procedure TForm1.BtnForTarget3Click(Sender: TObject);
begin
  Target1.EdgeColor.Color := clrLimeGreen;
  Target2.EdgeColor.Color := clrLimeGreen;
  Target3.EdgeColor.Color := clrYellow;
  GetOrCreateVehicle(GLCube2).Seek.Target := Target3;
  Target4.EdgeColor.Color := clrLimeGreen;
end;

procedure TForm1.BtnForTarget4Click(Sender: TObject);
begin
  Target1.EdgeColor.Color := clrLimeGreen;
  Target2.EdgeColor.Color := clrLimeGreen;
  Target3.EdgeColor.Color := clrLimeGreen;
  Target4.EdgeColor.Color := clrYellow;
  GetOrCreateVehicle(GLCube2).Seek.Target := Target4;
end;

end.
