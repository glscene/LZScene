{: Render To Bitmap sample.<p>

   This demo illustrates the two ways to obtain a 3D scene in a bitmap.<br>
   The first, which is also the fastest, is to use CreateSnapShot. It allows
   to obtain what you see on the screen, as it was rendered by the 3D acceleration
   device if any is available.<br>
   The second is to use RenderToBitmap. This makes use of Software OpenGL
   rendering and is significantly slower, but you can render to any size,
   including to bitmap much larger than the screen which are suited for use
   in printed documents for instance.<p>

   Note that since RenderToBitmap uses software OpenGL, the output may be
   different from what you get with hardware acceleration, not only because
   the rasterizer is different, but also because the software implementation
   may not support the same feature set. 
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  GLCadencer, GLObjects, GLScene, ExtCtrls, GLLCLViewer,
  GLHUDObjects, StdCtrls, GLSpaceText, GLCrossPlatform, GLCoordinates,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    Panel1: TPanel;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Plane1: TGLPlane;
    Sphere1: TGLSphere;
    GLCadencer1: TGLCadencer;
    DummyCube1: TGLDummyCube;
    HUDSprite1: TGLHUDSprite;
    BUSnapShot: TButton;
    BURenderToBitmap: TButton;
    BUBitmapx2: TButton;
    BUBitmap600: TButton;
    BUBitmap300: TButton;
    SpaceText1: TGLSpaceText;
    BUViewerSnapShot: TButton;
    procedure Sphere1Progress(Sender: TObject; const deltaTime, newTime: double);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BUSnapShotClick(Sender: TObject);
    procedure BURenderToBitmapClick(Sender: TObject);
    procedure BUBitmapx2Click(Sender: TObject);
    procedure BUBitmap600Click(Sender: TObject);
    procedure BUBitmap300Click(Sender: TObject);
    procedure BUViewerSnapShotClick(Sender: TObject);
  private
    { Private declarations }
    procedure RenderToBitmap(scale: single);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses Unit2, GLGraphics, GLUtils;

{
   A utility function, this takes the bitmap and uses Form2 to display it with
   a regular TImage component.
}
procedure ViewBitmap(aBitmap: TBitmap; Caption: string);
var
  f: TForm2;
begin
  Application.CreateForm(TForm2, f);
  if (aBitmap.Width < Screen.Width) and (aBitmap.Height < Screen.Height) then
  begin
    f.ClientWidth := aBitmap.Width;
    f.ClientHeight := aBitmap.Height;
  end
  else
  begin
    f.ClientWidth := Round(Screen.Width * 0.75);
    f.ClientHeight := Round(Screen.Height * 0.75);
  end;
  f.Image1.Picture.Bitmap := aBitmap;
  f.Caption := Caption;
  f.Image1.Width := aBitmap.Width;
  f.Image1.Height := aBitmap.Height;
  f.Show;
end;

procedure TForm1.BUViewerSnapShotClick(Sender: TObject);
var
  pt: int64;
  bmp: TBitmap;
  delta: double;
begin
  pt := StartPrecisionTimer;
  // Create a snapshot directly from the viewer content
  bmp := GLSceneViewer1.CreateSnapShotBitmap;
  delta := StopPrecisionTimer(pt);
  // Display the bitmap for the user to see and gaze in everlasting awe...
  ViewBitmap(bmp, Format('SnapShot %dx%d - %.3f ms',
    [bmp.Width, bmp.Height, delta * 1000]));
  // Release the bitmap
  bmp.Free;
end;

procedure TForm1.BUSnapShotClick(Sender: TObject);
var
  bmp32: TGLBitmap32;
  bmp: TBitmap;
  pt: int64;
  delta: double;
begin
  pt := StartPrecisionTimer;
  // CreateSnapShot returns a TGLBitmap32, which is a low-level data buffer.
  // However TGLBitmap32 can spawn a regular TBitmap, which we use here
  bmp32 := GLSceneViewer1.Buffer.CreateSnapShot;
  bmp := bmp32.Create32BitsBitmap;
  delta := StopPrecisionTimer(pt);
  // Display the bitmap for the user to see and gaze in everlasting awe...
  ViewBitmap(bmp, Format('SnapShot %dx%d - %.3f ms',
    [bmp.Width, bmp.Height, delta * 1000]));
  // Don't forget to free your TGLBitmap32 and TBitmap!
  bmp.Free;
  bmp32.Free;
end;

procedure TForm1.RenderToBitmap(scale: single);
var
  bmp: TBitmap;
  pt: int64;
  delta: double;
begin
  pt := StartPrecisionTimer;
  // Rendering to a bitmap requires an existing bitmap,
  // so we create and size a new one
  bmp := TBitmap.Create;
  // Don't forget to specify a PixelFormat, or current screen pixel format
  // will be used, which may not suit your purposes!
  bmp.PixelFormat := pf24bit;
  bmp.Width := Round(GLSceneViewer1.Width * scale);
  bmp.Height := Round(GLSceneViewer1.Height * scale);
  // Here we just request a render
  // The second parameter specifies DPI (Dots Per Inch), which is
  // linked to the bitmap's scaling
  // "96" is the "magic" DPI scale of the screen under windows
  GLSceneViewer1.Buffer.RenderToBitmap(bmp, Round(96 * scale));
  delta := StopPrecisionTimer(pt);
  ViewBitmap(bmp, Format('RenderToBitmap %dx%d - %.1f ms',
    [bmp.Width, bmp.Height, delta * 1000]));
  bmp.Free;
end;

procedure TForm1.BURenderToBitmapClick(Sender: TObject);
begin
  // Render at viewer resolution (scale = 1, DPI = 96)
  RenderToBitmap(1);
end;

procedure TForm1.BUBitmapx2Click(Sender: TObject);
begin
  // Render at twice viewer resolution (scale = 2, DPI = 192 = 96x2)
  RenderToBitmap(2);
end;

procedure TForm1.BUBitmap300Click(Sender: TObject);
begin
  // Screen is "magic" 96 dpi, this gives us our scale
  RenderToBitmap(300 / 96);
end;

procedure TForm1.BUBitmap600Click(Sender: TObject);
begin
  // Screen is "magic" 96 dpi, this gives us our scale
  RenderToBitmap(600 / 96);
end;


// Utility stuff: load textures, animate the sphere and support resizing.


procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  HUDSprite1.Material.Texture.Image.LoadFromFile('ashwood.jpg');
  Plane1.Material.Texture.Image.LoadFromFile('marbletiles.jpg');
  Sphere1.Material.Texture.Image.LoadFromFile('marbletiles.jpg');
end;

procedure TForm1.Sphere1Progress(Sender: TObject; const deltaTime, newTime: double);
var
  h: single;
begin
  h := 2.5 + 2 * Sin(newTime * 4);
  Sphere1.Position.Y := h;
  if h < 1 then
    Sphere1.Scale.Y := h
  else
    Sphere1.Scale.Y := 1;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  with HUDSprite1 do
  begin
    Width := GLSceneViewer1.Width;
    Position.X := Width * 0.5;
    Height := GLSceneViewer1.Height;
    Position.Y := Height * 0.5;
  end;
end;

end.
