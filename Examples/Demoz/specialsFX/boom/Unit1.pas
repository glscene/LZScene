{: An FireFX demo showcasing use of explosions.<p>

   In this sample, a small sphere is thrusted upward (with a gravity-like
   deceleration) with a small fire trail, explodes (in one of two styles,
   isotropic or ring, chosen at random), and falls back followed by a smoke
   trail.<br>
   The explosion takes place in 3D, to help you visualize it, hold the mouse
   button down and move around.<p>

   Two FireFXManager components are used, one for the fire, the other for
   the smoke. The explosion is triggered by calling IsotropicExplosion, which
   generates and "isotropic" population of particles that move away from the
   object position.<p>

   Note that to have trail effects, you must adjust the Reference property of
   a FireFX manager. It is unadjusted, the particles will not be "left behind",
   which is convenient for static fireplaces, but not for trials.
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLObjects, GLCadencer, GLFireFX, ExtCtrls, StdCtrls,
  GLBehaviours, GLVectorGeometry, GLLCLViewer, GLCrossPlatform, GLCoordinates,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    FireFX: TGLFireFXManager;
    GLCadencer1: TGLCadencer;
    Sphere1: TGLSphere;
    Timer1: TTimer;
    Button1: TButton;
    SmokeFX: TGLFireFXManager;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
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

procedure TForm1.Button1Click(Sender: TObject);
begin
   // A button click triggers the small animation sequence
   // first, we enabled the cadencer, to get Progression events
   GLCadencer1.Enabled:=True;
   // then we set (or reset) the sphere position and initial speed
   Sphere1.Position.AsVector:=NullHmgPoint;
   GetOrCreateInertia(Sphere1).TranslationSpeed.SetVector(Random, 9, Random);
   // the tagfloat is used as timer (before explosion)
   Sphere1.TagFloat:=3.5;
   // Here we reinitialize the FireFX (starts enabled) and SmokeFX (disabled at first)
   FireFX.ParticleSize:=0.5;
   FireFX.Disabled:=False;
   FireFX.FireInit;
   SmokeFX.Disabled:=True;
   SmokeFX.FireInit;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   // have we exploded yet?
   if Sphere1.TagFloat>0 then begin
      // no, so decrease time before explosion
      Sphere1.TagFloat:=Sphere1.TagFloat-deltaTime;
      // explosion time?
      if Sphere1.TagFloat<0 then begin
         // yep! make particles bigger,
         FireFX.ParticleSize:=2;
         // fire the explosion
         if Random>0.5 then
            FireFX.IsotropicExplosion(8, 10, 5)
         else FireFX.RingExplosion(8, 10, 5, XVector, ZVector);
         // stop the fire trail
         FireFX.Disabled:=True;
         // and start the smoke trail
         SmokeFX.Disabled:=False;
      end;
   end;
   // A gravity-like acceleration is applied to the sphere
   GetOrCreateInertia(Sphere1).ApplyTranslationAcceleration(deltaTime, VectorMake(0, -2, 0));
   // restart effect when sphere fell too low
   if Sphere1.Position.Y<-2 then Button1Click(Self);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   // standard issue framerate & particle count update
   Caption:=Format('%.1f FPS - %d Particles',
                   [GLSceneViewer1.FramesPerSecond, FireFX.ParticleCount+SmokeFX.ParticleCount]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
   // take care of zooming if window is resize
   GLCamera1.FocalLength:=Width*0.1;
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
   if Shift<>[] then begin
      // move around target
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x;
      my:=y;
      GLCadencer1.Progress;
   end;
end;

end.