{: Advenced for the TGLHeightField object.<p>

   Check the fxy sample first.<p>

   This sample shows a few more tricks : how to switch formulas at run-time,
   effects of base grid extents and resolution change as well as color and
   lighting options of the TGLHeightField.<p>

   Note that maxed out grid size and minimum step (high resolution) will bring
   most of todays cards to their knees (if they do not just crash, that is).<p>

   Used formulas :<p>

   The Formula1 is of type Sin(d)/(1+d), with d=sqr(x)+sqr(y), you may note
   the interesting sampling-interference effect with big step values (low res)
   and remember your math teacher's warnings on graph-plotting :)<p>

   Formula2 is a more classic sin*cos mix<p>

   Dynamic is the third formula, if you pick it, a small ball will appear and
   move around, the plotted formula being the square distance to the ball.
}
unit Unit1;

{.$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ComCtrls, GLScene, GLGraph, GLObjects, ExtCtrls, GLTexture,
  GLCadencer, GLVectorGeometry, GLLCLViewer, GLColor, GLCrossPlatform,
  GLCoordinates;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    HeightField1: TGLHeightField;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    TrackBar3: TTrackBar;
    RadioGroup1: TRadioGroup;
    Timer1: TTimer;
    Label4: TLabel;
    ComboBox1: TComboBox;
    Sphere1: TGLSphere;
    GLCadencer1: TGLCadencer;
    Lines1: TGLLines;
    CheckBox2: TCheckBox;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CheckBox1Click(Sender: TObject);
    procedure HeightField1GetHeight(const x, y: Single; var z: Single;
      var Color: TColorVector; var TexPoint: TTexPoint);
    procedure HeightField1GetHeight2(Sender: TObject; const x, y: Single;
      var z: Single; var Color: TColorVector; var TexPoint: TTexPoint);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Sphere1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure CheckBox2Click(Sender: TObject);
  private
    { Déclarations privées }
    procedure Formula1(const x, y: Single; var z: Single;
      var color: TColorVector; var texPoint: TTexPoint);
    procedure Formula2(const x, y: Single; var z: Single;
      var color: TColorVector; var texPoint: TTexPoint);
    procedure Formula3(const x, y: Single; var z: Single;
      var color: TColorVector; var texPoint: TTexPoint);
  public
    { Déclarations publiques }
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
   // start with first formula
   HeightField1.OnGetHeight:=Formula1;
   // no per-vertex coloring
   ComboBox1.ItemIndex:=0;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
   Sphere1.Visible:=False;
   // switch between formulas
   case RadioGroup1.ItemIndex of
      0 : HeightField1.OnGetHeight:=Formula1;
      1 : HeightField1.OnGetHeight:=Formula2;
      2 : begin
         HeightField1.OnGetHeight:=Formula3;
         Sphere1.Visible:=True;
      end;
   end;
end;

procedure TForm1.Formula1(const x, y: Single; var z: Single;
  var color: TColorVector; var texPoint: TTexPoint);
begin
   // first formula
   z:=VectorNorm(x, y);
   z:=cos(z*12)/(2*(z*6.28+1));
   VectorLerp(clrBlue, clrRed, (z+1)/2, color);
end;

procedure TForm1.Formula2(const x, y: Single; var z: Single;
  var color: TColorVector; var texPoint: TTexPoint);
begin
   // 2nd formula
   z:=0.5*cos(x*6.28)*sin(Sqrt(abs(y))*6.28);
   VectorLerp(clrBlue, clrRed, (z+1)/2, color);
end;

procedure TForm1.Formula3(const x, y: Single; var z: Single;
  var color: TColorVector; var texPoint: TTexPoint);
begin
   // 3rd formula, dynamic
   z:=1/(1+VectorNorm(Sphere1.position.X-x, Sphere1.position.Y-y));
   if ((Round(x*4)+Round(y*4)) and 1)=1 then
      color:=clrBlue
   else color:=clrYellow;
end;

procedure TForm1.Sphere1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   // move our little sphere around
   if Sphere1.Visible then begin
      Sphere1.Position.SetPoint(cos(newTime*2.3), sin(newTime), 1.5);
      HeightField1.StructureChanged;
   end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
   // enable two sided surface
   if CheckBox1.Checked then
      HeightField1.Options:=HeightField1.Options+[hfoTwoSided]
   else HeightField1.Options:=HeightField1.Options-[hfoTwoSided];
end;

procedure TForm1.HeightField1GetHeight(const x, y: Single; var z: Single;
  var Color: TColorVector; var TexPoint: TTexPoint);
begin

end;

procedure TForm1.HeightField1GetHeight2(Sender: TObject; const x, y: Single;
  var z: Single; var Color: TColorVector; var TexPoint: TTexPoint);
begin

end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
   // change per vertex color mode
   case ComboBox1.ItemIndex of
      0 : HeightField1.ColorMode:=hfcmNone;
      1 : HeightField1.ColorMode:=hfcmEmission;
      2 : HeightField1.ColorMode:=hfcmDiffuse;
   end;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
   GLLightSource1.Shining:=CheckBox2.Checked;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
   // adjust X extents
   with HeightField1.XSamplingScale do begin
      Min:=-TrackBar1.Position/10;
      Max:=TrackBar1.Position/10;
   end;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
   // adjust Y extents
   with HeightField1.YSamplingScale do begin
      Min:=-TrackBar2.Position/10;
      Max:=TrackBar2.Position/10;
   end;
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
begin
   // adjust grid steps (resolution)
   with HeightField1 do begin
      XSamplingScale.Step:=TrackBar3.Position/1000;
      YSamplingScale.Step:=TrackBar3.Position/1000;
   end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   // Display number of triangles used in the mesh
   // You will note that this number quickly gets out of hand if you are
   // using large high-resolution grids
   Caption:=Format('%d Triangles - %.2f FPS',
                   [HeightField1.TriangleCount, GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

// following code takes care of camera movement, see camera & movement demos
// for explanations and more samples

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x; my:=y;
   end;
end;

end.
