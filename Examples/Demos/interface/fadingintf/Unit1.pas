{: Fading interface sample.<p>

	This is a smoother (but more CPU and framerate intensive) approach to
	picking objects : when the pointer hovers on an object, it smoothly becomes
	red, and when it moves away it progressively turns back to grey.<p>

	It is implemented here using a shared field, "currentPick" (by shared,
	I mean it's a form field used in more than one event) indicating the
	object the mouse is currently hovering, a classic timer and the "Progress"
	chain of events.<br>
   When a mouse move is detected, it activates a timer, and when this timer
   is fired, the picking is performed. The "direct" approach would perform
   picking in the mousemove event, however, if the picking takes more time
   to be completed than the next mousemove event takes time to arrive,
   the interface will seem to "freeze".<p>

   The other timer is used to provide basic color animation, limited to about
   20 FPS (resolution of the timer isn't high enough to allow much higher
   framerates). Check other samples for better framerate independance techniques
   (and use the TGLCadencer !).<p>

	Note that all objects (sphere, torus...) share the same event.
}
unit Unit1;

interface

uses
  Forms, GLScene, GLObjects, GLTexture, Classes, Controls,
  ExtCtrls, SysUtils, GLVectorGeometry, GLLCLViewer, GLGeomObjects, GLColor,
  GLCrossPlatform, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    Sphere: TGLSphere;
	 Cylinder: TGLCylinder;
    Torus: TGLTorus;
    Cone: TGLCone;
	 Timer1: TTimer;
    TIPickTimer: TTimer;
	 procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
		X, Y: Integer);
	 procedure GLSceneViewer1MouseDown(Sender: TObject;
		Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	 procedure Timer1Timer(Sender: TObject);
	 procedure SphereProgress(Sender: TObject; const deltaTime,
		newTime: Double);
    procedure TIPickTimerTimer(Sender: TObject);
  private
	 { Déclarations privées }
	 currentPick : TGLCustomSceneObject;
  public
	 { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses Dialogs, GLScreen;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   TIPickTimer.Enabled:=True;
end;

procedure TForm1.TIPickTimerTimer(Sender: TObject);
var
   cp : TPoint;
begin
	// get what is under the mouse
   GLGetCursorPos(cp);
   cp:=GLSceneViewer1.ScreenToClient(cp);
	currentPick:=(GLSceneViewer1.Buffer.GetPickedObject(cp.x, cp.y) as TGLCustomSceneObject);
   TIPickTimer.Enabled:=False;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	pick : TGLCustomSceneObject;
begin
	// if an object is picked...
	pick:=(GLSceneViewer1.Buffer.GetPickedObject(x, y) as TGLCustomSceneObject);
	if Assigned(pick) then begin
		// ...turn it to yellow and show its name
		pick.Material.FrontProperties.Emission.Color:=clrYellow;
		ShowMessage('You clicked the '+pick.Name);
	end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
	// trigger progression (we don't use time in this sample)
	GLScene1.Progress(0, 0);
end;

procedure TForm1.SphereProgress(Sender: TObject; const deltaTime,
  newTime: Double);
var
	targetColor : TColorVector;
begin
	with Sender as TGLCustomSceneObject do begin
		// if we are picked, target color is red, else it is black (no emission)
		if Sender=currentPick then
			targetColor:=clrRed
		else targetColor:=clrBlack;
		// Set new color at 66% between current and target color
		with Material.FrontProperties.Emission do
			Color:=VectorLerp(targetColor, Color, 0.66)
	end;
end;

end.
