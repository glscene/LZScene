unit Unit1;



interface

uses
  Windows, Messages,
  SysUtils,  Classes,
  Math,

  Controls, Graphics, Forms, Dialogs,
  StdCtrls, ExtCtrls,

  //GLScene
  GLScene, GLObjects, GLCadencer, GLVectorGeometry,
  GLCoordinates, GLCrossPlatform, GLBaseClasses, GLLCLViewer, Types;

type

  { TForm1 }

  TForm1 = class (TForm)
    GLSceneViewer1 : TGLSceneViewer;
    GLScene1 : TGLScene;
    GLCamera1 : TGLCamera;
    GLDummyCube1 : TGLDummyCube;
    GLDummyCube2 : TGLDummyCube;
    Panel1 : TPanel;
    Button1 : TButton;
    Timer1 : TTimer;
    GLCadencer1 : TGLCadencer;
    Edit2 :  TEdit;
    Edit3 :  TEdit;
    Edit4 :  TEdit;
    Edit1 :  TEdit;
    GLDummyCube4 : TGLDummyCube;
    Button2 : TButton;
    Edit5 :  TEdit;
    Label1 : TLabel;
    Edit6 :  TEdit;
    Label2 : TLabel;
    Edit7 :  TEdit;
    Label3 : TLabel;
    CheckBox1: TCheckBox;
    procedure GLSceneViewer1MouseMove(Sender : TObject; Shift : TShiftState; X,
      Y : Integer);
    procedure GLSceneViewer1MouseDown(Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer);
    procedure FormMouseWheel(Sender : TObject; Shift : TShiftState;
      WheelDelta : Integer; MousePos : TPoint; var Handled : Boolean);
    procedure Button1Click(Sender : TObject);
    procedure Timer1Timer(Sender : TObject);
    procedure GLCadencer1Progress(Sender : TObject; const deltaTime,
      newTime : Double);
    procedure FormCreate(Sender : TObject);
    procedure Button2Click(Sender : TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1 : TForm1;

implementation

{$R *.lfm}


var
  mx, my, pnum, niter : integer;
  point, colorpoint : TGLPoints;
  oldpos, newpos : TAffineVector;
  posarray : array of TAffineVector;
  started : boolean;
  dpas : extended;

// SinCos (Double)
//



function ConvertDecimalSeparator(s : string) : string;
var
  n : integer;
begin
  for n := 1 to length(s) do
    begin
      if s[n] in ['.', ','] then
         s[n] := FormatSettings.DecimalSeparator
    end;
  Result := s;
end;

function strtofloatF(str : string) : extended;
begin
  Result := strtofloat(ConvertDecimalSeparator(str));
end;

function Dist(p1, p2 : TAffineVector) : extended;
begin
  Result := sqrt(sqr(p1.X - p2.X) + sqr(p1.Y - p2.Y) + sqr(p1.Z - p2.Z));
end;


procedure TForm1.Button1Click(Sender : TObject);
var
  iter, i, i2, adp : integer;
  sinc, cosc : double;
  sinc2, cosc2 : double;
  A, B, C, D, pc : extended;
  color : TAffineVector;
  dst : extended;
  minx, miny, minz, maxx, maxy, maxz, sx, sy, sz : extended;
begin
  point.Free;
  colorpoint.Free;

  pnum := strtoint(Edit5.Text);
  niter := strtoint(Edit7.Text);
  adp := strtoint(Edit6.Text);
  
  setlength(posarray, pnum);


  a := strtofloatF(edit1.Text);
  b := strtofloatF(edit2.Text);
  c := strtofloatF(edit3.Text);
  d := strtofloatF(edit4.Text);

  point := TGLPoints(GlDummyCube2.AddNewChild(TGLPoints));
  colorpoint := TGLPoints(GlDummyCube4.AddNewChild(TGLPoints));
  colorpoint.Size := 10.0;
  point.Size := 1.0;
  point.NoZWrite := CheckBox1.Checked;


  for i := 0 to pnum - 1 do
    begin
    posarray[i].X := (random(1000000) - 500000) / 100000;
    posarray[i].Y := (random(1000000) - 500000) / 100000;
    posarray[i].Z := (random(1000000) - 500000) / 100000;
    end;

  for iter := 0 to niter - 1 do
    begin
    if iter mod 10 = 0 then
      begin
      Button1.Caption := inttostr(iter) + '/' + inttostr(niter);
      Application.ProcessMessages;
      end;

    for i := 0 to pnum - 1 do
      begin
      oldpos := posarray[i];

      //  posarray[i][0] := sin(A * oldPos[1]) - oldPos[2] * cos(B * oldPos[0]);
      //  posarray[i][1] := oldPos[2] * sin(B * oldPos[0]) - cos(C * oldPos[2]);
      //  posarray[i][2] := sin(oldPos[0]);

      sincos(A * oldPos.Y, sinc, cosc);
      sincos(B * oldPos.X, sinc2, cosc2);

      posarray[i].X := sinc - oldPos.Z * cosc2;

      sincos(oldPos.X, sinc, cosc);

      posarray[i].Z := sinc;

      sincos(C * oldPos.X, sinc, cosc);
      sincos(D * oldPos.Y, sinc2, cosc2);

      posarray[i].Y := oldPos.Z * sinc - cosc2;
      end;
    end;

  minx := 1000;
  miny := 1000;
  minz := 1000;

  maxx := -1000;
  maxy := -1000;
  maxz := -1000;

  for i := 0 to pnum - 1 do
    begin
    minx := min(posarray[i].X, minx);
    miny := min(posarray[i].Y, miny);
    minz := min(posarray[i].Z, minz);
    maxx := max(posarray[i].X, maxx);
    maxy := max(posarray[i].Y, maxy);
    maxz := max(posarray[i].Z, maxz);
    end;

  sx := -(maxx + minx) / 2;
  sy := -(maxy + miny) / 2;
  sz := -(maxz + minz) / 2;


  for i := 0 to pnum - 1 do
    begin
    point.Positions.Add((posarray[i].X + sx) * 10, (posarray[i].Y + sy) * 10, (posarray[i].Z + sz) * 10);
    end;

  for i := 0 to adp - 1 do
    begin
    colorpoint.Colors.AddPoint(random, random, random);
    colorpoint.Positions.Add((random(10000) - 5000) / 500, (random(10000) - 5000) / 500, (random(10000) - 5000) / 500);
    end;


  for i := 0 to pnum - 1 do
    begin
    color.X := 0;
    color.Y := 0;
    color.Z := 0;

    for i2 := 0 to adp - 1 do
      begin
      dst := Dist(colorpoint.Positions.Items[i2], point.Positions.Items[i]);

      if dst > 0 then
        begin pc := 1 / sqr(dst * 1.3) * 12 end
      else
        begin pc := 1 end;

   {   color[0] := min(colorpoint.Colors.Items[i2][0],color[0] + pc * colorpoint.Colors.Items[i2][0]);
      color[1] := min(colorpoint.Colors.Items[i2][1],color[1] + pc * colorpoint.Colors.Items[i2][1]);
      color[2] := min(colorpoint.Colors.Items[i2][2],color[2] + pc * colorpoint.Colors.Items[i2][2]);
  }

      color.X := min(1, color.X + pc * colorpoint.Colors.Items[i2].X);
      color.Y := min(1, color.Y + pc * colorpoint.Colors.Items[i2].Y);
      color.Z := min(1, color.Z + pc * colorpoint.Colors.Items[i2].Z);

      end;

    point.Colors.AddPoint(color.X, color.Y, color.Z);
    end;

  Button1.Caption := 'Generate';

end;

procedure TForm1.Button2Click(Sender : TObject);
begin
  randomize;
  Edit1.Text := floattostrF((random(6000000) - 3000000) / 1000000, fffixed, 6, 6);
  Edit2.Text := floattostrF((random(6000000) - 3000000) / 1000000, fffixed, 6, 6);
  Edit3.Text := floattostrF((random(6000000) - 3000000) / 1000000, fffixed, 6, 6);
  Edit4.Text := floattostrF((random(6000000) - 3000000) / 1000000, fffixed, 6, 6);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  point.NoZWrite := CheckBox1.Checked;
end;

procedure TForm1.FormCreate(Sender : TObject);
begin
  Randomize;
  started := false;
end;

procedure TForm1.FormMouseWheel(Sender : TObject; Shift : TShiftState;
  WheelDelta : Integer; MousePos : TPoint; var Handled : Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TForm1.GLCadencer1Progress(Sender : TObject; const deltaTime,
  newTime : Double);
var
  iter, i : integer;
  sinc, cosc : double;
  sinc2, cosc2 : double;
begin
  GlSceneViewer1.Invalidate;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
begin
  my := y;
  mx := x;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender : TObject; Shift : TShiftState; X,
  Y : Integer);
begin
  if ssright in shift then
    begin GlCamera1.MoveAroundTarget(my - y, mx - x) end;
  my := y;
  mx := x;
end;

procedure TForm1.Timer1Timer(Sender : TObject);
begin
  Caption := Format('[%.2f FPS]', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.

