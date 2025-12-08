{: This samples shows how to use a material library for texture animation.<p>

   Texture animation is best handled via material switching (from a material
   library). Directly updating the texture image can get slow because of the
   overhead induces by preparing and uploading the texture to the 3D board.<p>

   In this sample, we prepare a set of textures in a material library,
   then simply switch between them in a cadencer's "Progress" event.
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, GLCadencer, GLScene, GLObjects, GLTexture, StdCtrls,
  GLLCLViewer, GLMaterial, GLCoordinates, GLCrossPlatform, GLBaseClasses;

type
  TForm1 = class(TForm)
    Button1: TButton;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    Cube1: TGLCube;
    GLLightSource1: TGLLightSource;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    CBAnimate: TCheckBox;
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    timeToNextFrame: double;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  i: integer;
  bmp: TBitmap;
begin
  // We generate a handful of bitmaps from scratch
  // you could also load them from a set of files, extract from an AVI etc.
  for i := 0 to 9 do
  begin
    bmp := TBitmap.Create;
    bmp.PixelFormat := pf24bit;
    bmp.Width := 60;
    bmp.Height := 60;
    bmp.Canvas.Font.Name := 'Arial';
    bmp.Canvas.Font.Height := 56;
    bmp.Canvas.TextOut(15, 5, IntToStr(i));
    GLMaterialLibrary1.AddTextureMaterial('IMG' + IntToStr(i), bmp);
    bmp.Free;
  end;
  // Initialize our loop
  Cube1.Material.MaterialLibrary := GLMaterialLibrary1;
  Cube1.Material.LibMaterialName := 'IMG0';
  GLMaterialLibrary1.Tag := 0;
  // GUI update
  CBAnimate.Enabled := True;
  Button1.Enabled := False;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  // cube turns slowly
  Cube1.Turn(deltaTime * 3);
  // cycle textures
  if CBAnimate.Checked then
  begin
    // coutdown to next frame
    timeToNextFrame := timeToNextFrame - deltaTime;
    // if it's time for the next frame
    if timeToNextFrame < 0 then
    begin
      // first, update frame counter (the Tag property in our sample)
      // (such a loop is a little overkill, yeah)
      while timeToNextFrame < 0 do
      begin
        timeToNextFrame := timeToNextFrame + 0.2;
        GLMaterialLibrary1.Tag := (GLMaterialLibrary1.Tag + 1) mod 10;
      end;
      // then, we update the material reference
      Cube1.Material.LibMaterialName := 'IMG' + IntToStr(GLMaterialLibrary1.Tag);
    end;
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  // stop animation
  CBAnimate.Checked := False;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // standard FPS
  Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.

