{: Dynamic cube map generation demo.<p>

   This a more advanced sample in which the cube map is dynamically generated.
   To generate a cube map, you need three items:<ul>
   <li>a destination texture (where to place the cube map!), the requirement is
      that the TextureImage class must be... a CubeMapImage
   <li>a camera, to specify from where the cubemap will be rendered
      (the orientation and parameters of the camera do not matter, only its
      position is relevant)
   <li>a memory viewer, this is what we'll use to render the 6 images of the
      cube map, it also determines the size of the texture (and must be a square)
   </ul>
   Generating the cube map can then be performed with a single call to the
   memoryviewer's RenderCubeMapTextures method.<p>

   Note: cube map can be used for "cool" looking reflections, but as you'll
   see in this demo it is not perfect, especially if the reflected objects
   are close to the reflecting object (cube map is generated without "knowledge"
   of the object that'll be used for reflecting), so use when appropriate ;)
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLObjects, GLScene, GLLCLViewer, StdCtrls, GLSkydome,
  ExtCtrls, GLCadencer, GLParticleFX, OpenGL1x, GLTeapot, GLGeomObjects,
  GLCrossPlatform, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMemoryViewer1: TGLMemoryViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Torus1: TGLTorus;
    CubeMapCamera: TGLCamera;
    Sphere1: TGLSphere;
    Cylinder1: TGLCylinder;
    Teapot1: TGLTeapot;
    SkyDome1: TGLSkyDome;
    Cube1: TGLCube;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    Panel1: TPanel;
    CBDynamic: TCheckBox;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
  private
    { Private declarations }
    procedure GenerateCubeMap;
  public
    { Public declarations }
    mx, my: integer;
    cubeMapWarnDone: boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.GenerateCubeMap;
begin
  // Don't do anything if cube maps aren't supported
  if not GLSceneViewer1.Buffer.RenderingContext.GL.ARB_texture_cube_map then
  begin
    if not cubeMapWarnDone then
      ShowMessage('Your graphics hardware does not support cube maps...');
    cubeMapWarnDone := True;
    Exit;
  end;
  // Here we generate the new cube map, from CubeMapCamera (a child of the
  // teapot in the scene hierarchy)
  with Teapot1 do
  begin
    // hide the teapot while rendering the cube map
    Visible := False;
    // render cube map to the teapot's texture
    GLMemoryViewer1.RenderCubeMapTextures(Material.Texture);
    // teapot visible again
    Material.Texture.Disabled := False;
    Visible := True;
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  if CBDynamic.Checked then
  begin
    // make things move
    Teapot1.Position.Y := 2 * Sin(newTime);
    Torus1.RollAngle := newTime * 15;
    // generate the cube map
    GenerateCubeMap;
  end;
  GLSceneViewer1.Invalidate;
end;

// Standard issue mouse movement

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

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.

