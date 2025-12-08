{: Demo for color modulation and fade-in/out for bitmap fonts.<p>

   The bitmap Font used in this demo is obtained from<p>

   http://www.algonet.se/~guld1/freefont.htm<p>

   and was modified by me to have a red background so that I can have the
   character itself in black.<p>

   Nelson Chu
   cpegnel@ust.hk
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLLCLViewer, GLScene, GLObjects, GLHUDObjects,
  GLBitmapFont, GLCadencer, GLTimeEventsMgr, GLTeapot, GLCrossPlatform,
  GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    BitmapFont: TGLBitmapFont;
    GLCamera1: TGLCamera;
    HUDText1: TGLHUDText;
    GLLightSource1: TGLLightSource;
    Teapot1: TGLTeapot;
    GLTimeEventsMGR1: TGLTimeEventsMGR;
    GLCadencer1: TGLCadencer;
    HUDText2: TGLHUDText;
    HUDText3: TGLHUDText;
    HUDText4: TGLHUDText;
    procedure GLTimeEventsMGR1Events0Event(event: TTimeEvent);
    procedure GLTimeEventsMGR1Events1Event(event: TTimeEvent);
    procedure GLTimeEventsMGR1Events2Event(event: TTimeEvent);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
  FadeOutMax = 100;
  FadeInMax = 100;
  OverallTrans = 0.7;

implementation

{$R *.lfm}

uses GLVectorGeometry, GLVectorTypes, GLUtils;

var
  FadeOutCount: integer;
  FadeInCount: integer;
  OriginalColor: TVector4f;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  BitmapFont.Glyphs.LoadFromFile(MediaPath+'toonfont.bmp');
end;

procedure TForm1.GLTimeEventsMGR1Events0Event(event: TTimeEvent);
begin
  if FadeOutCount < 0 then
    exit;

  HUDText1.ModulateColor.Color :=
    VectorMake(1, 1, 1, (FadeOutCount / FadeOutMax) * OverallTrans);
  Dec(FadeOutCount);
end;

procedure TForm1.GLTimeEventsMGR1Events1Event(event: TTimeEvent);
begin
  FadeOutCount := FadeOutMax;
  FadeInCount := 0;

  OriginalColor := HUDText2.ModulateColor.Color;

  HUDText1.ModulateColor.Color :=
    VectorMake(1, 1, 1, (FadeOutCount / FadeOutMax) * OverallTrans);
  HUDText2.ModulateColor.Color := VectorMake(1, 1, 1, 0);
end;

procedure TForm1.GLTimeEventsMGR1Events2Event(event: TTimeEvent);
var
  NewColor: TVector4f;
begin
  if FadeInCount >= FadeInMax then
    exit;

  NewColor := VectorScale(OriginalColor, FadeInCount / FadeInMax);

  HUDText2.ModulateColor.Color := NewColor;
  Inc(FadeInCount);
end;

end.

