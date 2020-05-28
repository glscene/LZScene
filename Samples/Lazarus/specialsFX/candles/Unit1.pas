{: FireFX and simulated "wind".<p>

   This samples showcases a bare-bones "birthday cake" with three candles,
   you can adjust wind strength with the horizontal slider, but beware, if the
   wind gets too strong, the candles will be blown off! ;)<p>

   The "cake" is a simple revolution solid, the candles are based on a cylinder,
   line, fire FX on the line, and a transparent plane (for the 2cents "shadow").
   The candles are duplicated with a TGLProxyObject each.<br>
   Particles in a FireFX are submitted to a uniform acceleration, specified with
   the "FireDir" property, and the "wind" slider directly adjusts it.
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLObjects, GLExtrusion, GLScene, GLCadencer, GLFireFX, ComCtrls,
  ExtCtrls, GLLCLViewer, GLGeomObjects, GLCrossPlatform, GLCoordinates,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    Candle: TGLCylinder;
    RevolutionSolid1: TGLRevolutionSolid;
    Lines1: TGLLines;
    GLFireFXManager1: TGLFireFXManager;
    GLCadencer1: TGLCadencer;
    GLProxyObject1: TGLProxyObject;
    GLProxyObject2: TGLProxyObject;
    TrackBar1: TTrackBar;
    Timer1: TTimer;
    Plane1: TGLPlane;
    DummyCube2: TGLDummyCube;
    procedure TrackBar1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormResize(Sender: TObject);
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

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
   GLFireFXManager1.FireDir.Z:=-TrackBar1.Position*0.1;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
   n : Integer;
begin
   Caption:=Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
   if TrackBar1.Position=0 then
      GLFireFXManager1.Disabled:=False
   else begin
      n:=Abs(TrackBar1.Position)-15;
      if n>0 then
         if Random/n<0.15 then GLFireFXManager1.Disabled:=True;
   end;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      GLCadencer1.Progress;
      mx:=x; my:=y;
   end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
   GLCamera1.FocalLength:=Height/3;
end;

end.