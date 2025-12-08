unit Unit1;

{$mode objfpc}{$h+}

interface

uses
  SysUtils,
  Classes,Graphics, Forms, Controls,
   
  GLCadencer, 
  {GLWin32Viewer,} 
  GLScene, 
  GLAsyncTimer, 
  GLObjects,
  GLVectorFileObjects, 
  GLFile3DS, 
  GLVectorGeometry, 
  GLTexture,
  GLCoordinates, 
  GLCrossPlatform, 
  GLBaseClasses, 
  GLRenderContextInfo, GLLCLViewer;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    Cadencer: TGLCadencer;
    cam: TGLCamera;
    AsyncTimer: TGLAsyncTimer;
    ff: TGLFreeForm;
    dc_cam: TGLDummyCube;
    dogl: TGLDirectOpenGL;
    procedure AsyncTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure doglRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure vpMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vpMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    mx,my: Integer;
    m_pos: TPoint;
    m_move: Boolean;
  end;

  TTRS = record
    pos: TVector;
    rot, rotd, scale: single;
  end;

var
  Form1: TForm1;

  lst: array [0 .. 99] of TTRS;

implementation

{$R *.lfm}

//
// setup
//
procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Randomize;
  ff.LoadFromFile('horse.3ds');
  ff.scale.scale(2 / ff.BoundingSphereRadius);
//  ff.Material.BackProperties.Ambient;
  for i := 0 to high(lst) do
  begin
    SetVector(lst[i].pos, round(Random * 40 - 20), 0, round(Random * 40));
    lst[i].rot := Random(100);
    lst[i].rotd := Random - 0.5;
    lst[i].scale := 1 + (Random - 0.5) / 3;
  end;
end;

procedure TForm1.vpMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Shift = [ssRight] then
  begin
    m_pos := mouse.CursorPos;
    m_move := true;
  end;
end;

procedure TForm1.vpMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
 if Shift = [ssLeft] then
   Cam.MoveAroundTarget(my -y, mx -x);
  mx := x;
  my := y;
end;

//
// cadProgress
//
procedure TForm1.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  dc_cam.Turn(deltaTime * 10);
end;

//
// doglRender
//
procedure TForm1.doglRender(Sender: TObject; var rci: TGLRenderContextInfo);
var
  i: integer;
  m: TMatrix;
begin
  m := ff.Matrix;
  for i := 0 to high(lst) do
    if not IsVolumeClipped(lst[i].pos, lst[i].scale, rci.rcci.frustum) then
    begin
        ff.Matrix := m;
        ff.Position.SetPoint(lst[i].pos);
        ff.Roll(lst[i].rot);
        lst[i].rot := lst[i].rot + lst[i].rotd * vp.LastFrameTime * 100;
        ff.scale.scale(lst[i].scale);
        ff.Render(rci);
    end;
  ff.Matrix := m;
end;

//
// timer
//
procedure TForm1.AsyncTimerTimer(Sender: TObject);
begin
  Caption := 'MultiFreeForm: ' + vp.FramesPerSecondText(2);
  vp.ResetPerformanceMonitor;
end;

end.
