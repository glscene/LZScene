// THIS DEMO IS PART OF THE GLSCENE PROJECT
{
  Version History:
    30.01.2008 - mrqzzz - Initial version.
    06.02.2008 - mrqzzz - Added  "RayCastIntersect" for Actorproxy in demo.
    15.03.2008 - DaStr  - Updated RayCastIntersect stuff because of changes in
                          the TGLActorProxy component.

}
unit Unit1;


interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLProxyObjects, GLVectorFileObjects, GLObjects,
  GLVectorGeometry, ExtCtrls, GLCadencer, GLTexture, GLGeomObjects, GLLCLViewer,
  GLFileSMD, StdCtrls, GLCrossPlatform, GLMaterial, GLCoordinates,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    InvisibleDummyCube: TGLDummyCube;
    GLDummyCube2: TGLDummyCube;
    MasterActor: TGLActor;
    GLActorProxy1: TGLActorProxy;
    GLArrowLine1: TGLArrowLine;
    GLLightSource1: TGLLightSource;
    Timer1: TTimer;
    GLSphere1: TGLSphere;
    GLArrowLine3: TGLArrowLine;
    GLActorProxy2: TGLActorProxy;
    GLArrowLine2: TGLArrowLine;
    Panel1: TPanel;
    cbActorsAreTurning: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
  private
    mousex, mousey: integer;
    procedure DoRaycastStuff;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses GLUtils;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  SetGLSceneMediaDir();

  MasterActor.LoadFromFile(MediaPath+'TRINITYrage.smd');
  MasterActor.AddDataFromFile(MediaPath+'run.smd');
  MasterActor.AddDataFromFile(MediaPath+'jump.smd');

  MasterActor.Animations.Items[0].Name := 'still';
  MasterActor.Animations.Items[1].Name := 'walk';
  MasterActor.Animations.Items[2].Name := 'jump';

  for i := 0 to MasterActor.Animations.Count - 1 do
  begin
    MasterActor.Animations[i].MakeSkeletalTranslationStatic;
    MasterActor.SwitchToAnimation(i);
    // forces animations to be initialized for ActorsProxies
  end;
  MasterActor.SwitchToAnimation(0);
  // revert back to empty animation (not necessary)
  MasterActor.AnimationMode := aamLoop; // animationmode is shared between proxies.

  GLActorProxy1.StoreBonesMatrix := True;
  GLActorProxy2.StoreBonesMatrix := True;


  GLActorProxy1.Animation := MasterActor.Animations[1].Name;
  GLActorProxy2.Animation := MasterActor.Animations[2].Name;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  // Align object to hand
  GLArrowLine1.Matrix := GLActorProxy1.BoneMatrix('Bip01 R Finger1');
  GLArrowLine2.Matrix := GLActorProxy2.BoneMatrix('Bip01 R Finger1');

  // turn actors
  if cbActorsAreTurning.Checked then
  begin
    GLActorProxy1.Turn(-deltaTime * 130);
    GLActorProxy2.Turn(deltaTime * 100);
  end;

  DoRaycastStuff;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  mousex := x;
  mouseY := y;
end;

procedure TForm1.DoRaycastStuff;
var
  rayStart, rayVector, iPoint, iNormal: TVector;
begin
  SetVector(rayStart, GLCamera1.AbsolutePosition);
  SetVector(rayVector, GLSceneViewer1.Buffer.ScreenToVector(
    AffineVectorMake(mousex, GLSceneViewer1.Height - mousey, 0)));
  NormalizeVector(rayVector);

  if GLActorProxy1.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal) then
  begin
    GLSphere1.Position.AsVector := iPoint;
    GLSphere1.Direction.AsVector := VectorNormalize(iNormal);
  end
  else
  if GLActorProxy2.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal) then
  begin
    GLSphere1.Position.AsVector := iPoint;
    GLSphere1.Direction.AsVector := VectorNormalize(iNormal);
  end
  else
  begin
    GLSphere1.Position.AsVector := rayStart;
    GLSphere1.Direction.AsVector := rayVector;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := GLSceneViewer1.FramesPerSecondText(0);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.

