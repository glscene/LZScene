{: Sample showing use of TGLSprite for "caterpillar" effect.<p>

  A bunch of TGLSprite is created in FormCreate, all copied from Sprite2 (used
  as "template"), then we move and resize them as they orbit a pulsating "star".<br>
  Textures are loaded from a "flare1.bmp" file that is expected to be in the
  same directory as the compiled EXE.<p>

  There is nothing really fancy in this code, but in the objects props :<ul>
    <li>blending is set to bmAdditive (for the color saturation effect)
      <li>DepthTest is disabled
    <li>ball color is determined with the Emission color
  </ul><br>
  The number of sprites is low to avoid stalling a software renderer
  (texture alpha-blending is a costly effect), if you're using a 3D hardware,
  you'll get FPS in the hundredths and may want to make the sprite count higher.<p>

  A material library component is used to store the material (and texture) for
  all the sprites, if we don't use it, sprites will all maintain their own copy
  of the texture, which would just be a waste of resources. With the library,
  only one texture exists (well, two, the sun has its own).
}
unit Unit1;

interface

uses
  Forms, GLScene, GLObjects, StdCtrls, GLTexture, Classes, Controls, ExtCtrls,

  GLCadencer, GLLCLViewer, GLCrossPlatform, GLMaterial, GLProxyObjects,
  GLCoordinates;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    DummyCube1: TGLDummyCube;
    GLCamera1: TGLCamera;
    Sprite1: TGLSprite;
    Sprite2: TGLSprite;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses SysUtils, GLVectorGeometry, GLUtils;

procedure TForm1.FormCreate(Sender: TObject);
var

  spr: TGLSprite;
  i:integer;
begin
  SetGLSceneMediaDir();
  // Load texture for sprite2, this is the hand-coded way using a PersistentImage
  // Sprite1 uses a PicFileImage, and so the image is automagically loaded by
  // GLScene when necessary (no code is required).
  // (Had I used two PicFileImage, I would have avoided this code)
  GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile(MediaPath+'\'+'flare1.bmp');
  Sprite1.Material.Texture.Assign(GLMaterialLibrary1.Materials[0].Material.Texture);
  // New sprites are created by duplicating the template "sprite2"
  for i := 1 to 9 do
  begin
    spr := TGLSprite(DummyCube1.AddNewChild(TGLSprite));
    spr.Assign(sprite2);
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
var
  i: integer;
  a, aBase: double;
begin
  // angular reference : 90° per second <=> 4 second per revolution
  aBase := 90 * newTime;
  // "pulse" the star
  a := DegToRad(aBase);
  Sprite1.SetSquareSize(4 + 0.2 * cos(3.5 * a));
  // rotate the sprites around the yellow "star"
  for i := 0 to DummyCube1.Count - 1 do
  begin
    a := DegToRad(aBase + i * 8);
    with (DummyCube1.Children[i] as TGLSprite) do
    begin
      // rotation movement
      Position.X := 4 * cos(a);
      Position.Z := 4 * sin(a);
      // ondulation
      Position.Y := 2 * cos(2.1 * a);
      // sprite size change
      SetSquareSize(2 + cos(3 * a));
    end;
  end;
end;


procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // update FPS count and reset counter
  Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  // This lines take cares of auto-zooming.
  // magic numbers explanation :
  //  333 is a form width where things looks good when focal length is 50,
  //  ie. when form width is 333, uses 50 as focal length,
  //      when form is 666, uses 100, etc...
  GLCamera1.FocalLength := Width * 50 / 333;
end;

end.

