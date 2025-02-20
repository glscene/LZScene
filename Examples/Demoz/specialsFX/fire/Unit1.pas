{: The fire special effect basic sample.<p>

   If you look at the code you won't see anything fancy. The FireFX is a dynamic
   special effect (driven by a cadencer). Making use of it means two things :<br>
   - dropping a FirexFXManager, this one controls fire particle systems aspects<br>
   - adding a FireFX effect to the object you want to see burning (here, a sphere)<br>
   You may have multiple objects sharing the same FireFXManager, this means they
   will all look the same, but also that the particle system calculations are
   made only once.<p>

   This effect looks cool but is fill-rate hungry, but un-textured fillrate
   hungry, ie. video card memory bandwith is not an issue. Anyway, you can
   always make it look nice with smaller and/or less particles.
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLFireFX, GLCadencer, GLScene, GLObjects, GLBehaviours, ExtCtrls,
  GLVectorGeometry, GLLCLViewer, GLGeomObjects, GLCrossPlatform, GLCoordinates,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLFireFXManager1: TGLFireFXManager;
    GLCamera1: TGLCamera;
    Sphere1: TGLSphere;
    Torus1: TGLTorus;
    GLLightSource2: TGLLightSource;
    Timer1: TTimer;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
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
   GLCadencer1.Progress;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
	GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;

end.