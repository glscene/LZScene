{: Projected Textures example.<p>

   Controls:
     Left Mouse Button: Rotate
     Right Mouse Button: Adjust Distance
     'S': Change styles

   The TGLProjectedTextures object can be used to simulate
   projected lights or other special effects. To use it, add
   it to the scene and set all objects that should receive
   the projected textures as children. Then, add a number
   of TGLTextureEmitter and load the texture that should
   be projected into it. Add this emitter to the "emitters"
   list of the projtex and that's it.

   There are two styles of projection: original and inverse.
   On the original method (developed by Tom Nuyens of
   www.delphi3d.net) the scene is rendered and the textures
   are projected into it. This is useful to simulate all
   kinds of special effects from bullet holes, to shadow
   maps (using tmBlend texture mode).

   On the inverse method, first all emitters are rendered,
   creating an "illumination mask". Then the scene is blended
   to this mask, so that only lit pixels are drawn. This can
   be used to create a projected-texture only illumination system.

}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLCadencer, GLScene, GLLCLViewer, glTexture, tga, GLObjects,
  GLVectorGeometry, ExtCtrls, glProjectedTextures,
  GLHUDObjects, GLCrossPlatform, GLMaterial, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    viewer: TGLSceneViewer;
    scene: TGLScene;
    GLCadencer1: TGLCadencer;
    camera: TGLCamera;
    GLPlane1: TGLPlane;
    GLPlane2: TGLPlane;
    GLPlane3: TGLPlane;
    scenery: TGLDummyCube;
    GLSphere1: TGLSphere;
    matLib: TGLMaterialLibrary;
    Timer1: TTimer;
    Light: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLCube1: TGLCube;
    GLCube2: TGLCube;
    light2: TGLDummyCube;
    GLSphere2: TGLSphere;
    GLSphere3: TGLSphere;
    ProjLight: TGLProjectedTextures;
    emitter1: TGLTextureEmitter;
    emitter2: TGLTextureEmitter;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure viewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure viewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure viewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


var
  Form1: TForm1;
  ang: single;
  mx, my, mk: integer;

implementation

{$R *.lfm}

uses GLUtils, LCLType;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  ang := ang + deltatime * 20;

  Light.Position.Y := sin(degToRad(ang));
  light.position.x := cos(degToRad(ang));

  light2.pitch(deltatime * 20);

  viewer.invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  matLib.Materials[0].Material.Texture.Image.LoadFromFile(MediaPath+'projector.tga');
  matLib.Materials[1].Material.Texture.Image.LoadFromFile(MediaPath+'flare1.bmp');

  emitter1.Material.MaterialLibrary := matLib;
  emitter1.Material.LibMaterialName := 'spot';

  emitter2.Material.MaterialLibrary := matLib;
  emitter2.Material.LibMaterialName := 'spot2';
  emitter2.FOVy := 40;

  GLPlane1.Material.Texture.Image.LoadFromFile(MediaPath+'cm_front.jpg');
  GLPlane2.Material.Texture.Image.LoadFromFile(MediaPath+'cm_left.jpg');
  GLPlane3.Material.Texture.Image.LoadFromFile(MediaPath+'cm_bottom.jpg');

  projLight.Emitters.AddEmitter(emitter1);
  projLight.Emitters.AddEmitter(emitter2);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  form1.Caption := format('%f', [viewer.framespersecond]);
  viewer.resetperformancemonitor;
end;

procedure TForm1.viewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  mk := 1;
  mx := x;
  my := y;
end;

procedure TForm1.viewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  mk := 0;
end;

procedure TForm1.viewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if mk = 1 then
  begin
    if shift = [ssLeft] then
      camera.MoveAroundTarget(y - my, x - mx)
    else if shift = [ssRight] then
      camera.AdjustDistanceToTarget(1.0 + (y - my) / 100);
  end;

  mx := x;
  my := y;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = VK_ADD then
    emitter1.FOVy := emitter1.FOVy + 5
  else if key = VK_SUBTRACT then
    emitter1.FOVy := emitter1.FOVy - 5;

  if chr(Key) = 'S' then
    if ProjLight.style = ptsOriginal then
      ProjLight.style := ptsInverse
    else
      ProjLight.style := ptsOriginal;
end;

end.
