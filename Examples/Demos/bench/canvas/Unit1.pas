{: Benchmark for GLCanvas.<p>

   This project pits TGLCanvas against TCanvas in direct mode (hardware
   acceleration should be available on both sides).<p>
   You may usually bet on TGLCanvas being 3 to 5 times faster, but on fast 3D
   hardware, or when PenWidth is not 1, the performance ratio can reach 1:100.<p>
   However, this is not really an apples-to-apples comparison, because GDI
   (or any other software implementations) are useless when it comes to drawing
   to an OpenGL buffer, so, this is more to show that GLCanvas is far from
   a "decelerator" if you have some 2D stuff to draw on your 3D Scene.<p>

   Figures for PenWidth = 1, GLCanvas / GDI<p>

   CPU         Graphics Board    Lines          Ellipses         Points       TextOut

   Tbird 1.2   GF3 Ti200         5.2 / 227      64 /  756        27 / 408     75 / 208
   ----29/09/02 - Added TextOut bench
   Tbird 1.2   GF2 Pro           7.1 / 162       92 /  557       40 / 223
   Duron 800   TNT2 M64        105.0 / 571      400 / 1148      126 / 676
   ----21/01/02 - Initial
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, ExtCtrls, StdCtrls, GLLCLViewer,
  GLBitmapFont, GLWindowsFont, GLTexture, GLCrossPlatform, GLCoordinates,
  GLBaseClasses, GLRenderContextInfo;

type
  TForm1 = class(TForm)
    BULines: TButton;
    BUEllipses: TButton;
    GLSceneViewer: TGLSceneViewer;
    PaintBox: TPaintBox;
    LAGLCanvas: TLabel;
    LAGDI: TLabel;
    Bevel1: TBevel;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    RBPenWidth1: TRadioButton;
    RBPenWidth2: TRadioButton;
    BUPoints: TButton;
    BURects: TButton;
    BUTextOut: TButton;
    WindowsBitmapFont: TGLWindowsBitmapFont;
    GLDirectOpenGL1: TGLDirectOpenGL;
    procedure BULinesClick(Sender: TObject);
    procedure BUEllipsesClick(Sender: TObject);
    procedure BUPointsClick(Sender: TObject);
    procedure BURectsClick(Sender: TObject);
    procedure BUTextOutClick(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject; var rci: TGLRenderContextInfo);
  private
    { Private declarations }
    procedure PaintTheBox;
    procedure Bench;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses GLCanvas;

type
  TWhat = (wLines, wEllipses, wRects, wPoints, wTextOut);

var
  vWhat: TWhat;
  vPenWidth: integer;

const
  cNbLines = 20000;
  cNbEllipses = 20000;
  cNbRects = 5000;
  cNbPoints = 200000;
  cNbTextOuts = 20000;

procedure TForm1.BULinesClick(Sender: TObject);
begin
  vWhat := wLines;
  Bench;
end;

procedure TForm1.BUEllipsesClick(Sender: TObject);
begin
  vWhat := wEllipses;
  Bench;
end;

procedure TForm1.BURectsClick(Sender: TObject);
begin
  vWhat := wRects;
  Bench;
end;

procedure TForm1.BUPointsClick(Sender: TObject);
begin
  vWhat := wPoints;
  Bench;
end;

procedure TForm1.BUTextOutClick(Sender: TObject);
begin
  vWhat := wTextOut;
  Bench;
end;

procedure TForm1.Bench;
var
  t: int64;
begin
  if RBPenWidth1.Checked then
    vPenWidth := 1
  else
    vPenWidth := 2;

  Application.ProcessMessages;
  RandSeed := 0;

  t := StartPrecisionTimer;
  GLSceneViewer.Refresh;
  LAGLCanvas.Caption := Format('GLCanvas: %.2f msec', [StopPrecisionTimer(t) * 1000]);

  Application.ProcessMessages;
  RandSeed := 0;

  t := StartPrecisionTimer;
  PaintTheBox;
  LAGDI.Caption := Format('GDI: %.1f msec', [StopPrecisionTimer(t) * 1000]);
end;

procedure TForm1.GLDirectOpenGL1Render(Sender: TObject; var rci: TGLRenderContextInfo);
var
  i, x, y: integer;
  glc: TGLCanvas;
  r: TRect;
  color: TColor;
begin
  glc := TGLCanvas.Create(256, 256);
  with glc do
  begin
    PenWidth := vPenWidth;
    case vWhat of
      wLines:
      begin
        for i := 1 to cNbLines do
        begin
          PenColor := Random(256 * 256 * 256);
          MoveTo(Random(256), Random(256));
          LineTo(Random(256), Random(256));
        end;
      end;
      wEllipses:
      begin
        for i := 1 to cNbEllipses do
        begin
          PenColor := Random(256 * 256 * 256);
          Ellipse(Random(256), Random(256),
            Random(256), Random(256));
        end;
      end;
      wRects:
      begin
        for i := 1 to cNbRects do
        begin
          PenColor := Random(256 * 256 * 256);
          r := Rect(Random(256), Random(256), Random(256),
            Random(256));
          FillRect(r.Left, r.Top, r.Right, r.Bottom);
        end;
      end;
      wPoints:
      begin
        for i := 1 to cNbPoints do
        begin
          PenColor := Random(256 * 256 * 256);
          PlotPixel(Random(256), Random(256));
        end;
      end;
      wTextOut:
      begin
        for i := 1 to cNbTextOuts do
        begin
          color := Random(256 * 256 * 256);
          x := Random(256);
          y := Random(256);
          WindowsBitmapFont.TextOut(rci, x, y, 'Hello', color);
        end;
      end;
    end;
  end;
  glc.Free;
end;

procedure TForm1.PaintTheBox;
var
  i, x, y: integer;
  r: TRect;
begin
  with PaintBox.Canvas do
  begin
    Brush.Style := bsClear;
    Pen.Width := vPenWidth;
    case vWhat of
      wLines:
      begin
        for i := 1 to cNbLines do
        begin
          Pen.Color := Random(256 * 256 * 256);
          MoveTo(Random(256), Random(256));
          LineTo(Random(256), Random(256));
        end;
      end;
      wEllipses:
      begin
        for i := 1 to cNbEllipses do
        begin
          Pen.Color := Random(256 * 256 * 256);
          Ellipse(Random(256), Random(256),
            Random(256), Random(256));
        end;
      end;
      wRects:
      begin
        Brush.Style := bsSolid;
        for i := 1 to cNbRects do
        begin
          Brush.Color := Random(256 * 256 * 256);
          r := Rect(Random(256), Random(256), Random(256),
            Random(256));
          FillRect(r);
        end;
      end;
      wPoints:
      begin
        for i := 1 to cNbPoints do
        begin
          Pixels[Random(256), Random(256)] := Random(256 * 256 * 256);
        end;
      end;
      wTextOut:
      begin
        Font := WindowsBitmapFont.Font;
        for i := 1 to cNbTextOuts do
        begin
          Font.Color := Random(256 * 256 * 256);
          x := Random(256);
          y := Random(256);
          TextOut(x, y, 'Hello');
        end;
      end;
    end;
  end;
end;

end.
