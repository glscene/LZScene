{: Demo of the TGLPoints component.<p>

   The component is specialized in rendering large numbers of points,
   with ability to adjust point style (from fast square point to smooth
   round points) and point parameters.<p>
   The point parameters define how point size is adjusted with regard
   to eye-point distance (to make farther points smaller, see ARB_point_parameters
   for more details).<p>
   The component is also suitable for particle systems, but offers less
   flexibility than the TGLParticleFX.
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLObjects, GLLCLViewer, StdCtrls, GLVectorGeometry, GLVectorLists,
  GLCadencer, GLTexture, ExtCtrls, GLColor, GLCrossPlatform, GLCoordinates;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    GLPoints1: TGLPoints;
    GLCadencer1: TGLCadencer;
    GLPoints2: TGLPoints;
    Panel1: TPanel;
    CBPointParams: TCheckBox;
    CBAnimate: TCheckBox;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure CBAnimateClick(Sender: TObject);
    procedure CBPointParamsClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    mx, my: integer
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  cNbPoints = 180;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // allocate points in the 1st point set
  GLPoints1.Positions.Count := cNbPoints;
  // specify white color for the 1st point set
  // (if a single color is defined, all points will use it,
  // otherwise, it's a per-point coloring)
  GLPoints1.Colors.Add(clrWhite);
  // specify blue color for the 2nd point set
  GLPoints2.Colors.Add(clrBlue);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
var
  i: integer;
  f, a, ab, ca, sa: single;
  p: TAffineVectorList;
  v: TAffineVector;
begin
  if CBAnimate.Checked then
  begin
    // update the 1st point set with values from a math func
    f := 1 + Cos(newTime);
    p := GLPoints1.Positions;
    ab := newTime * 0.1;
    for i := 0 to cNbPoints - 1 do
    begin
      a := DegToRad(4.0 * i) + ab;
      SinCos(a, sa, ca);
      v.V[0] := 2 * ca;
      v.V[1] := 2 * Cos(f * a);
      v.V[2] := 2 * sa;
      p[i] := v;
    end;
    // replicate points in second set
    GLPoints2.Positions := GLPoints1.Positions;
  end;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  mx := x;
  my := y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if Shift <> [] then
  begin
    GLCamera1.MoveAroundTarget(my - y, mx - x);
    mx := x;
    my := y;
  end;
end;

procedure TForm1.CBAnimateClick(Sender: TObject);
begin
  GLPoints1.Static := not CBAnimate.Checked;
  GLPoints2.Static := not CBAnimate.Checked;
end;

procedure TForm1.CBPointParamsClick(Sender: TObject);
begin
  GLPoints1.PointParameters.Enabled := CBPointParams.Checked;
  GLPoints2.PointParameters.Enabled := CBPointParams.Checked;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.

