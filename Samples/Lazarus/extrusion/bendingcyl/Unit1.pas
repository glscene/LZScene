{: A very simple demo of the TGLPipe object, to show what it can do.<p>

   The TGLPipe objects extrudes a circle along a trajectory (given by its node).
   You can specify a radius factor for each node and use spline smoothing.<p>

   Here we only use 3 control points, the top ones moves horizontally, and the
   middle one can be made fat/slim.<p>

   The current implementation is very limited when it comes to 3D pipes, as there
   is no "smooth" rotation interpolator, therefore, ou will have best results
   if your trajectory stays in the X/Y (local) plane.
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLObjects, GLExtrusion, GLCadencer, StdCtrls, ExtCtrls,
  GLLCLViewer, GLCrossPlatform, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Pipe1: TGLPipe;
    GLCadencer1: TGLCadencer;
    CBSpline: TCheckBox;
    DummyCube1: TGLDummyCube;
    CBFat: TCheckBox;
    Timer1: TTimer;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure CBSplineClick(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses GLVectorGeometry;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   Pipe1.Nodes[2].X:=1*Sin(newTime*60*cPIdiv180);
   if CBFat.Checked then
      TGLPipeNode(Pipe1.Nodes[1]).RadiusFactor:=1+Cos(newTime*30*cPIdiv180)
   else TGLPipeNode(Pipe1.Nodes[1]).RadiusFactor:=1;
end;

procedure TForm1.CBSplineClick(Sender: TObject);
begin
   if CBSpline.Checked then
      Pipe1.SplineMode:=lsmCubicSpline
   else Pipe1.SplineMode:=lsmLines;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then
      GLCamera1.MoveAroundTarget(my-y, mx-x);
   mx:=x; my:=y;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   with GLSceneViewer1 do begin
      Caption:=Format('%d Triangles, %.1f FPS', [Pipe1.TriangleCount, FramesPerSecond]);
      ResetPerformanceMonitor;
   end;
end;

end.