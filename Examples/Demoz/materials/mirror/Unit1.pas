{: GLMirror demo and sample.<p>

   Depiste its simplistic look, this sample showcases all the dos and don'ts
   of reflections with TGLMirror, this is a powerfull mirroring component,
   but it must be handled with care and knowingly.<p>
   The object that must be mirrored is specified through the "MirrorObject"
   property, you can specify only one, but with proper use of dummy cubes and/or
   proxies, this is not a real limitation, and allows you to select what will be
   reflected (each reflected object must be rendered twice). If no MirrorObject
   is specified, the whole scene will be mirrored.<p>

   If you want your mirror to be transparent, you must respect a rendering order
   and have the non-transparent objects rendered last (this includes the mirror,
   like any other blended object, see materials/transparency for an explanation).<br>
   Also note that some of the options (stenciling, clearZBuffer) <b>require</b>
   a stencil buffer (must be enabled in the viewer's buffer), but stenciling may
   not always be hardware accelerated (modern boards will support it).<p>

   There is also a variety of settings to the right of the screen, those adjust
   internal options that have a direct impact of what the mirror will be able
   to do right, and how much time rendering will take. The scene contains three
   groups of objects of interest:<ul>
   <li>non-reflecting ones: green torus and cylinder, these are behind the mirror,
      there are no particular issues with non-reflecting objects in front of a
      mirror (except objects not reflecting...), but by playing with the settings
      (ClearZBuffer especially) you'll notice they cause some artifacts if improperly
      handled.
   <li>teapot group: those are reflected. See how disabling stencil will cause
      the reflected teapot to be visible outside of the mirror. If your mirror
      is in a wall (opaque on all sides around the mirror), you may not have to
      care about stenciling (the wall will overdraw mirror images).
   <li>lone inclined gray cylinder: this one is a don't, it's an object that is
      reflecting but that goes through the mirror... well, you can do it,
      but you have to activate PlaneClip, otherwise objects on the other side
      of the mirror get reflected on the "wrong" side... (uncheck the option
      and see for yourself). PlaneClip is better avoided, it can make your FPS
      drop significantly on some 3D boards.
   </ul><p>

   In addition to being opaque, transparent or semi-transparent, the mirror
   can also be textured as usual.<p>

   Final note: T&L boards like the GeForce will be the one taking the most
      performance hit from the PlaneClip because it basicly turns-off hardware T&L.
      The glEval-based teapot also performs (relatively) poorly on those boards,
      while on an old-fashioned TNT2 f.i., plane clipping has a negligible
      performance impact.
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  GLScene, GLObjects, GLExtrusion, GLMirror, GLMultiPolygon,
  ExtCtrls, GLCadencer, StdCtrls, GLLCLViewer, GLTeapot, GLGeomObjects,
  GLCoordinates, GLCrossPlatform, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Sphere: TGLSphere;
    ReflectingObjects: TGLDummyCube;
    Cylinder: TGLTorus;
    Teapot1: TGLTeapot;
    Cylinder1: TGLCylinder;
    GLMirror: TGLMirror;
    Cadre: TGLExtrusionSolid;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    Label1: TLabel;
    CBOpaque: TCheckBox;
    CBStencil: TCheckBox;
    Cylinder2: TGLCylinder;
    CBClearZ: TCheckBox;
    CylinderThroughMirror: TGLCylinder;
    CBPlaneClip: TCheckBox;
    DCNonReflectingStuff: TGLDummyCube;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure CBOpaqueClick(Sender: TObject);
    procedure CBStencilClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CBClearZClick(Sender: TObject);
    procedure CBPlaneClipClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    mx, my: integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}


// Those events simply add/remove one of the mirror options
// when the related checkbox is clicked


procedure TForm1.CBOpaqueClick(Sender: TObject);
begin
  if CBOpaque.Checked then
    GLMirror.MirrorOptions := GLMirror.MirrorOptions + [moOpaque]
  else
    GLMirror.MirrorOptions := GLMirror.MirrorOptions - [moOpaque];
end;

procedure TForm1.CBStencilClick(Sender: TObject);
begin
  if CBStencil.Checked then
    GLMirror.MirrorOptions := GLMirror.MirrorOptions + [moUseStencil]
  else
    GLMirror.MirrorOptions := GLMirror.MirrorOptions - [moUseStencil];
end;

procedure TForm1.CBClearZClick(Sender: TObject);
begin
  if CBClearZ.Checked then
    GLMirror.MirrorOptions := GLMirror.MirrorOptions + [moClearZBuffer]
  else
    GLMirror.MirrorOptions := GLMirror.MirrorOptions - [moClearZBuffer];
end;

procedure TForm1.CBPlaneClipClick(Sender: TObject);
begin
  if CBPlaneClip.Checked then
    GLMirror.MirrorOptions := GLMirror.MirrorOptions + [moMirrorPlaneClip]
  else
    GLMirror.MirrorOptions := GLMirror.MirrorOptions - [moMirrorPlaneClip];
end;


// Standard-issue move around target code


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


// Standard issue resize/zoom, timer and viewr invalidation (to force redraws)


procedure TForm1.FormResize(Sender: TObject);
begin
  GLCamera1.SceneScale := GLSceneViewer1.Width / 380;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  GLSceneViewer1.Invalidate;
end;

end.

