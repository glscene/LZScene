{: A demo for TGLHUDText using the WindowsBitmapFont component.<p>

   The WindowsBitmapFont can automatically generate a font texture based on
   one of the standard windows fonts. The texture dimensions are automatically
   computed to maximize the texture's fill ratio, up to a size of 512x512,
   with the usual ASCII character range being the default.<br>
   Should you happen to require larger fonts (that do not fit the max texture
   size), you can try to reduce the default range, or split it and request
   only the characters you will actually use.<p>

   Clicking on the viewer will hide/show the teapot (when teapot is on, the
   framerate is much lower, f.i. on my GF3 / K7 1.2, the rating can easily
   reach 950FPS with teapot off)
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLHUDObjects, GLObjects, GLCadencer, ExtCtrls,
  GLBitmapFont, GLLCLViewer, GLWindowsFont, Menus, GLTeapot, GLCoordinates,
  GLCrossPlatform, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    HUDText1: TGLHUDText;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    HUDText2: TGLHUDText;
    HUDText3: TGLHUDText;
    Teapot1: TGLTeapot;
    WindowsBitmapFont1: TGLWindowsBitmapFont;
    MainMenu1: TMainMenu;
    MIPickFont: TMenuItem;
    FontDialog1: TFontDialog;
    MIViewTexture: TMenuItem;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1Click(Sender: TObject);
    procedure MIPickFontClick(Sender: TObject);
    procedure MIViewTextureClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
   // sorry, couldn't resist again...
   HUDText1.Text:= 'Lorem ipsum dolor sit amer, consectetaur adipisicing elit,'#13#10
                  +'sed do eiusmod tempor incididunt ut labore et dolore magna'#13#10
                  +'aliqua. Ut enim ad minim veniam, quis nostrud exercitation'#13#10
                  +'ullamco laboris nisi ut aliquip ex ea commodo consequat.'#13#10
                  +'Duis aute irure dolor in reprehenderit in voluptate velit'#13#10
                  +'esse cillum dolore eu fugiat nulla pariatur. Excepteur sint'#13#10
                  +'occaecat cupidatat non proident, sunt in culpa qui officia'#13#10
                  +'deserunt mollit anim id est laborum.'#13#10
                  +'Woblis ten caracuro Zapothek it Setag!'; // I needed an uppercase 'W' too...

  HUDText1.Text := HUDText1.Text + #13#10'Unicode text...'#$0699#$069A#$963f#$54c0;
  WindowsBitmapFont1.EnsureString(HUDText1.Text);
end;

procedure TForm1.MIPickFontClick(Sender: TObject);
begin
  FontDialog1.Font := WindowsBitmapFont1.Font;
  if FontDialog1.Execute then
  begin
    WindowsBitmapFont1.Font := FontDialog1.Font;
    HUDText1.ModulateColor.AsWinColor := FontDialog1.Font.Color;
  end;
end;

procedure TForm1.MIViewTextureClick(Sender: TObject);
begin
  with Form2.Image1 do
  begin
    Picture := WindowsBitmapFont1.Glyphs;
    Form2.Width := Picture.Width;
    Form2.Height := Picture.Height;
  end;
  Form2.Show;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  // make things move a little
  HUDText2.Rotation := HUDText2.Rotation + 15 * deltaTime;
  HUDText3.Scale.X := sin(newTime) + 1.5;
  HUDText3.Scale.Y := cos(newTime) + 1.5;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.1f FPS - %d x %d Font Texture',
    [GLSceneViewer1.FramesPerSecond,
    WindowsBitmapFont1.FontTextureWidth,
    WindowsBitmapFont1.FontTextureHeight]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLSceneViewer1Click(Sender: TObject);
begin
  Teapot1.Visible := not Teapot1.Visible;
end;

end.
