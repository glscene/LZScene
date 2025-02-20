{: Simple projective shadows.<p>

   The TGLShadowPlane component allows to render simple projective shadows.
   They have the benefit of being quite fast, but as the name says, your shadows
   will be projected only on a plane and  must be (entirely) on the same side
   of the plane as the light (the side pointed by the plane's direction).<p>
   Note that stenciling is required for proper operation (it is an option of
   the Viewer.Buffer.ContextOptions), which should be available on all modern
   graphics hardware. When stenciling is not activated, the ShadowPlane will
   use opaque shadows and you may see shadows appear beyond the plane limits...<p>

   The higher quality lighting on the marble planes is obtained by specifying
   Tiles in the plane and removing 'psSingleQuad' from the style. Lighting
   is computed per-vertex, this changes increase drastically the number of
   vertices that make up the planes, thus allowing for better lighting.
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLShadowPlane, GLScene, GLViewer, GLObjects,
  GLCadencer, StdCtrls, GLVectorGeometry, ExtCtrls, GLTexture, GLGeomObjects,
  GLCrossPlatform, GLMaterial, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    DCShadowing: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    Cube1: TGLCube;
    Sphere1: TGLSphere;
    GLCamera1: TGLCamera;
    GLCadencer1: TGLCadencer;
    DCLight: TGLDummyCube;
    Sphere2: TGLSphere;
    Torus1: TGLTorus;
    DCCameraTarget: TGLDummyCube;
    GLShadowPlane1: TGLShadowPlane;
    Timer1: TTimer;
    Panel1: TPanel;
    CBShadows: TCheckBox;
    CBStencil: TCheckBox;
    GLShadowPlane2: TGLShadowPlane;
    GLMaterialLibrary: TGLMaterialLibrary;
    GLShadowPlane3: TGLShadowPlane;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CBShadowsClick(Sender: TObject);
    procedure CBStencilClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  GLUtils;

procedure TForm1.FormCreate(Sender: TObject);
begin
   SetGLSceneMediaDir();
   GLMaterialLibrary.Materials[0].Material.Texture.Image.LoadFromFile(MediaPath+'\'+'beigemarble.jpg');
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   DCLight.PitchAngle:=Sin(newTime)*60;
   DCShadowing.TurnAngle:=newTime*10;
end;

procedure TForm1.CBShadowsClick(Sender: TObject);
begin
   if CBShadows.Checked then
      GLShadowPlane1.ShadowedLight:=GLLightSource1
   else GLShadowPlane1.ShadowedLight:=nil;
   GLShadowPlane2.ShadowedLight:=GLShadowPlane1.ShadowedLight;
   GLShadowPlane3.ShadowedLight:=GLShadowPlane1.ShadowedLight;
end;

procedure TForm1.CBStencilClick(Sender: TObject);
begin
   if CBStencil.Checked then
      GLShadowPlane1.ShadowOptions:=[spoUseStencil, spoScissor]
   else GLShadowPlane1.ShadowOptions:=[spoScissor];
   GLShadowPlane2.ShadowOptions:=GLShadowPlane1.ShadowOptions;
   GLShadowPlane3.ShadowOptions:=GLShadowPlane1.ShadowOptions;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
