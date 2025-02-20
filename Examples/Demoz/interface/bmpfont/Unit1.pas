{: A bare-bones sample for TGLHUDText.<p>

   To use a TGLHUDText, you must first place a TGLBitmapFont component and specify
   a font bitmap and it character ranges (ie. which tile represents which
   character). The component allows for a wide variety of fixed-width font
   bitmaps, and you can reuse many of the old bitmap fonts sets that were
   written for Atari, Amiga etc.<p>

   The TGLHUDText can then be placed in the hierarchy: just link it to the
   TGLBitmapFont, specify a text, alignment, layout, scale and position to
   whatever suits your need and that's all.<p>

   Clicking on the viewer will hide/show the teapot (when teapot is on, the
   framerate is much lower, f.i. on my GF3 / K7 1.2, the rating can reach
   1050FPS with teapot off)
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLHUDObjects, GLObjects, GLCadencer, ExtCtrls,
  GLBitmapFont, GLLCLViewer, GLTeapot, GLCrossPlatform, GLCoordinates,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    BitmapFont1: TGLBitmapFont;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    HUDText1: TGLHUDText;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    HUDText2: TGLHUDText;
    HUDText3: TGLHUDText;
    Teapot1: TGLTeapot;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses GLUtils;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  // Load the font bitmap
  BitmapFont1.Glyphs.LoadFromFile(MediaPath+'darkgold_font.bmp');
  // sorry, couldn't resist...
  HUDText1.Text := 'Hello World !'#13#10#13#10 +
    'This is me, '#13#10 + 'the HUD Text.'#13#10#13#10
    + 'Bitmap Fonts!';
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  // make things move a little
  HUDText2.Rotation := HUDText2.Rotation + 15 * deltaTime;
  HUDText3.Scale.X := 0.5 * sin(newTime) + 1;
  HUDText3.Scale.Y := 0.5 * cos(newTime) + 1;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLSceneViewer1Click(Sender: TObject);
begin
  Teapot1.Visible := not Teapot1.Visible;
end;

end.

