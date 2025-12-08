unit Unit1;

{$MODE Delphi}

interface

uses
Windows,

  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,

  GLScene,
  GLObjects,
  GLLCLViewer,
  GLTexture,
  GLWaterPlane,
  GLCadencer,
  GLFile3DS,
  GLVectorTypes,
  GLVectorGeometry,
  GLVectorFileObjects,
  GLVectorLists,
  GLGraph,
  GLMesh,
  GLGeomObjects,
  GLAsyncTimer,
  GLCelShader,
  GLOutlineShader,
  GLHiddenLineShader,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLMesh1: TGLMesh;
    AsyncTimer1: TGLAsyncTimer;
    GLLightSource1: TGLLightSource;
    Panel1: TPanel;
    Button5: TButton;
    Button8: TButton;
    Button7: TButton;
    Button4: TButton;
    Button2: TButton;
    Edit3: TEdit;
    Edit2: TEdit;
    Edit1: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Button6: TButton;
    Edit5: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    procedure create_cylinder(master: TGLMesh; num: integer; r, l: single);
    procedure create_T(master: TGLMesh; num: integer; r, l: single);
    procedure create_plus(master: TGLMesh; num: integer; r, l: single);
    procedure create_G(master: TGLMesh; num: integer; r, l: single);
  end;

var
  vcount, vcur, iter, iterstep, ci: integer;
  vert1, vert2, vert3, ad_vert4, ad_vert5: TVertexData;
  TriList: TAffineVectorList;
  mx, my:  integer;
  Form1:   TForm1;

implementation

{$R *.lfm}

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  mx := x;
  my := y;
end;


procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget((my - y) * 0.8, (mx - x) * 0.8);
  end;
  mx := x;
  my := y;
end;


procedure TForm1.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  glcamera1.AdjustDistanceToTarget(0.9);
end;


procedure TForm1.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  glcamera1.AdjustDistanceToTarget(1.1);
end;


//   z:=0.1-(GLBitmapHDS1.Picture.Bitmap.Canvas.Pixels[Round(x+64), Round(y+64)] and $FF)/255;


//z:=2-(GLBitmapHDS1.Picture.Bitmap.Canvas.Pixels[Round(x+64), Round(y+64)] and $FF)/255;
//   z:=0.5-(GLWaterPlane1.Mask.Bitmap.Canvas.Pixels[Round(x+64), Round(y+64)] and $FF)/255;

procedure TForm1.Button5Click(Sender: TObject);
begin
  glmesh1.Vertices.Clear;
end;


procedure TForm1.Button6Click(Sender: TObject);
//var i:integer;
begin
  vcur := 1;
  vcount := glmesh1.Vertices.Count;
  asynctimer1.Interval := StrToInt(edit4.Text);
  iter := StrToInt(edit5.Text);
  iterstep := 1;
  if vcount > 0 then
    asynctimer1.Enabled := True;
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
  create_cylinder(glmesh1, StrToInt(edit1.Text),
    strtofloat(edit2.Text), strtofloat(edit3.Text));
  form1.Caption := IntToStr(GLMesh1.Vertices.Count);
end;


procedure TForm1.create_cylinder(master: TGLMesh; num: integer; r, l: single);
var
  i: integer;
  g1, g2, g3: single;

begin
  master.Vertices.Clear;
  //-------------
  g1 := ((360 / num) * 0) * (3.14 / 180);
  vert1.coord.X := sin(g1) * r;
  vert1.coord.Y := -l * 0.5;
  vert1.coord.Z := cos(g1) * r;

  vert1.color.X := 0;//random(254)*0.01;
  vert1.color.Y := (random(100) + 154) * 0.01;
  vert1.color.Z := 0;//random(254)*0.01;

  g2 := ((360 / num) * 1) * (3.14 / 180);
  vert2.coord.X := sin(g2) * r;
  vert2.coord.Y := -l * 0.5;
  vert2.coord.Z := cos(g2) * r;

  vert2.color := vert1.color;


  g2 := ((360 / num) * 1) * (3.14 / 180);
  ad_vert4.coord.X := sin(g2) * r;
  ad_vert4.coord.Y := l * 0.5;
  ad_vert4.coord.Z := cos(g2) * r;

  ad_vert4.color := vert1.color;

  //
  g3 := ((360 / num) * 0) * (3.14 / 180);
  ad_vert5.coord.X := sin(g3) * r;
  ad_vert5.coord.Y := l * 0.5;
  ad_vert5.coord.Z := cos(g3) * r;

  ad_vert5.color := vert1.color;

  master.Vertices.AddVertex3(vert1, vert2, ad_vert4);
  master.Vertices.AddVertex3(vert1, ad_vert4, ad_vert5);

  for i := 1 to num - 2 do
  begin
    g1 := ((360 / num) * 0) * (3.14 / 180);
    vert1.coord.X := sin(g1) * r;
    vert1.coord.Y := -l * 0.5;
    vert1.coord.Z := cos(g1) * r;

    vert1.color.X := 0;//random(254)*0.01;
    vert1.color.Y := (random(100) + 154) * 0.01;
    vert1.color.Z := 0;//random(254)*0.01;
    //
    g2 := ((360 / num) * i) * (3.14 / 180);
    vert2.coord.X := sin(g2) * r;
    vert2.coord.Y := -l * 0.5;
    vert2.coord.Z := cos(g2) * r;

    vert2.color.X := 0;//random(254)*0.01;
    vert2.color.Y := (random(100) + 154) * 0.01;
    vert2.color.Z := 0;//random(254)*0.01;
    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    vert3.coord.X := sin(g3) * r;
    vert3.coord.Y := -l * 0.5;
    vert3.coord.Z := cos(g3) * r;

    vert3.color.X := 0;//random(254)*0.01;
    vert3.color.Y := (random(100) + 154) * 0.01;
    vert3.color.Z := 0;//random(254)*0.01;
    //
    master.Vertices.AddVertex3(vert1, vert3, vert2);

    //---------------------
    g2 := ((360 / num) * i) * (3.14 / 180);
    ad_vert4.coord.X := sin(g2) * r;
    ad_vert4.coord.Y := l * 0.5;
    ad_vert4.coord.Z := cos(g2) * r;

    ad_vert4.color := vert2.color;

    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    ad_vert5.coord.X := sin(g3) * r;
    ad_vert5.coord.Y := l * 0.5;
    ad_vert5.coord.Z := cos(g3) * r;

    ad_vert5.color := vert3.color;

    master.Vertices.AddVertex3(vert2, vert3, ad_vert4);
    master.Vertices.AddVertex3(vert3, ad_vert5, ad_vert4);

    //-----------------

    g1 := ((360 / num) * 0) * (3.14 / 180);
    vert1.coord.X := sin(g1) * r;
    vert1.coord.Y := l * 0.5;
    vert1.coord.Z := cos(g1) * r;

    vert1.color.X := 0;//random(254)*0.01;
    vert1.color.Y := (random(100) + 154) * 0.01;
    vert1.color.Z := 0;//random(254)*0.01;
    //
    g2 := ((360 / num) * i) * (3.14 / 180);
    vert2.coord.X := sin(g2) * r;
    vert2.coord.Y := l * 0.5;
    vert2.coord.Z := cos(g2) * r;

    vert2.color.X := 0;//random(254)*0.01;
    vert2.color.Y := (random(100) + 154) * 0.01;
    vert2.color.Z := 0;//random(254)*0.01;
    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    vert3.coord.X := sin(g3) * r;
    vert3.coord.Y := l * 0.5;
    vert3.coord.Z := cos(g3) * r;

    vert3.color.X := 0;//random(254)*0.01;
    vert3.color.Y := (random(100) + 154) * 0.01;
    vert3.color.Z := 0;//random(254)*0.01;
    //
    master.Vertices.AddVertex3(vert1, vert2, vert3);
  end;
  //--------------

  g1 := ((360 / num) * 0) * (3.14 / 180);
  vert1.coord.X := sin(g1) * r;
  vert1.coord.Y := -l * 0.5;
  vert1.coord.Z := cos(g1) * r;

  vert1.color.X := 0;//random(254)*0.01;
  vert1.color.Y := 250 * 0.01;//(random(100)+154)*0.01;
  vert1.color.Z := 0;//random(254)*0.01;

  g2 := ((360 / num) * (num - 1)) * (3.14 / 180);
  vert2.coord.X := sin(g2) * r;
  vert2.coord.Y := -l * 0.5;
  vert2.coord.Z := cos(g2) * r;

  vert2.color := vert1.color;


  g2 := ((360 / num) * (num - 1)) * (3.14 / 180);
  ad_vert4.coord.X := sin(g2) * r;
  ad_vert4.coord.Y := l * 0.5;
  ad_vert4.coord.Z := cos(g2) * r;

  ad_vert4.color := vert1.color;

  //
  g3 := ((360 / num) * 0) * (3.14 / 180);
  ad_vert5.coord.X := sin(g3) * r;
  ad_vert5.coord.Y := l * 0.5;
  ad_vert5.coord.Z := cos(g3) * r;

  ad_vert5.color := vert1.color;

  master.Vertices.AddVertex3(vert1, ad_vert5, ad_vert4);
  master.Vertices.AddVertex3(vert2, vert1, ad_vert4);

end;


procedure TForm1.create_T(master: TGLMesh; num: integer; r, l: single);
var
  i: integer;
  g1, g2, g3: single;

begin
  master.Vertices.Clear;
  //-------------
  g1 := ((360 / num) * 0) * (3.14 / 180);
  vert1.coord.X := sin(g1) * r;
  vert1.coord.Y := -l * 0.5;
  vert1.coord.Z := cos(g1) * r;
  vert1.color.X := 0;
  vert1.color.Y := (random(100) + 154) * 0.01;
  vert1.color.Z := 0;

  g2 := ((360 / num) * 1) * (3.14 / 180);
  vert2.coord.X := sin(g2) * r;
  vert2.coord.Y := -l * 0.5;
  vert2.coord.Z := cos(g2) * r;
  vert2.color := vert1.color;

  g2 := ((360 / num) * 1) * (3.14 / 180);
  ad_vert4.coord.X := sin(g2) * r;
  ad_vert4.coord.Y := l * 0.5;
  ad_vert4.coord.Z := cos(g2) * r;
  ad_vert4.color := vert1.color;

  //
  g3 := ((360 / num) * 0) * (3.14 / 180);
  ad_vert5.coord.X := sin(g3) * r;
  ad_vert5.coord.Y := l * 0.5;
  ad_vert5.coord.Z := cos(g3) * r;
  ad_vert5.color := vert1.color;

  master.Vertices.AddVertex3(vert1, vert2, ad_vert4);
  master.Vertices.AddVertex3(vert1, ad_vert4, ad_vert5);
  //-----------------
  {the same for side}

  g1 := ((360 / num) * 0) * (3.14 / 180);
  vert1.coord.X := sin(g1) * r;
  vert1.coord.Y := cos(g1) * r;
  vert1.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
  vert1.color.X := 0;//random(254)*0.01;
  vert1.color.Y := (random(100) + 154) * 0.01;
  vert1.color.Z := 0;//random(254)*0.01;

  g2 := ((360 / num) * 1) * (3.14 / 180);
  vert2.coord.X := sin(g2) * r;
  vert2.coord.Y := cos(g2) * r;
  vert2.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
  vert2.color := vert1.color;

  g2 := ((360 / num) * 1) * (3.14 / 180);
  ad_vert4.coord.X := sin(g2) * r;
  ad_vert4.coord.Y := cos(g2) * r;
  ad_vert4.coord.Z := 0 - cos(g2) * r;
  ad_vert4.color := vert1.color;

  //
  g3 := ((360 / num) * 0) * (3.14 / 180);
  ad_vert5.coord.X := sin(g3) * r;
  ad_vert5.coord.Y := cos(g3) * r;
  ad_vert5.coord.Z := 0 - cos(g3) * r;
  ad_vert5.color := vert1.color;

  master.Vertices.AddVertex3(vert2, vert1, ad_vert4);
  master.Vertices.AddVertex3(vert1, ad_vert5, ad_vert4);
  //
  for i := 1 to num - 2 do
  begin
    g1 := ((360 / num) * 0) * (3.14 / 180);
    vert1.coord.X := sin(g1) * r;
    vert1.coord.Y := -l * 0.5;
    vert1.coord.Z := cos(g1) * r;
    ;
    vert1.color.X := 0;//random(254)*0.01;
    vert1.color.Y := (random(100) + 154) * 0.01;
    vert1.color.Z := 0;//random(254)*0.01;
    //
    g2 := ((360 / num) * i) * (3.14 / 180);
    vert2.coord.X := sin(g2) * r;
    vert2.coord.Y := -l * 0.5;
    vert2.coord.Z := cos(g2) * r;
    ;
    vert2.color.X := 0;//random(254)*0.01;
    vert2.color.Y := (random(100) + 154) * 0.01;
    vert2.color.Z := 0;//random(254)*0.01;
    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    vert3.coord.X := sin(g3) * r;
    vert3.coord.Y := -l * 0.5;
    vert3.coord.Z := cos(g3) * r;
    vert3.color.X := 0;//random(254)*0.01;
    vert3.color.Y := (random(100) + 154) * 0.01;
    vert3.color.Z := 0;//random(254)*0.01;
    //
    master.Vertices.AddVertex3(vert1, vert3, vert2);

    //---------------------
    g2 := ((360 / num) * i) * (3.14 / 180);
    ad_vert4.coord.X := sin(g2) * r;
    ad_vert4.coord.Y := l * 0.5;
    ad_vert4.coord.Z := cos(g2) * r;
    ad_vert4.color := vert2.color;

    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    ad_vert5.coord.X := sin(g3) * r;
    ad_vert5.coord.Y := l * 0.5;
    ad_vert5.coord.Z := cos(g3) * r;
    ad_vert5.color := vert3.color;

    master.Vertices.AddVertex3(vert2, vert3, ad_vert4);
    master.Vertices.AddVertex3(vert3, ad_vert5, ad_vert4);

    //-----------------

    g1 := ((360 / num) * 0) * (3.14 / 180);
    vert1.coord.X := sin(g1) * r;
    vert1.coord.Y := l * 0.5;
    vert1.coord.Z := cos(g1) * r;
    vert1.color.X := 0;//random(254)*0.01;
    vert1.color.Y := (random(100) + 154) * 0.01;
    vert1.color.Z := 0;//random(254)*0.01;
    //
    g2 := ((360 / num) * i) * (3.14 / 180);
    vert2.coord.X := sin(g2) * r;
    vert2.coord.Y := l * 0.5;
    vert2.coord.Z := cos(g2) * r;
    vert2.color.X := 0;//random(254)*0.01;
    vert2.color.Y := (random(100) + 154) * 0.01;
    vert2.color.Z := 0;//random(254)*0.01;
    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    vert3.coord.X := sin(g3) * r;
    vert3.coord.Y := l * 0.5;
    vert3.coord.Z := cos(g3) * r;
    vert3.color.X := 0;//random(254)*0.01;
    vert3.color.Y := (random(100) + 154) * 0.01;
    vert3.color.Z := 0;//random(254)*0.01;
    //
    master.Vertices.AddVertex3(vert1, vert2, vert3);

    //--------------
    {side}

    g1 := ((360 / num) * 0) * (3.14 / 180);
    vert1.coord.X := sin(g1) * r;
    vert1.coord.Y := cos(g1) * r;
    vert1.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
    vert1.color.X := 0;//random(254)*0.01;
    vert1.color.Y := (random(100) + 154) * 0.01;
    vert1.color.Z := 0;//random(254)*0.01;
    //
    g2 := ((360 / num) * i) * (3.14 / 180);
    vert2.coord.X := sin(g2) * r;
    vert2.coord.Y := cos(g2) * r;
    vert2.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
    vert2.color.X := 0;//random(254)*0.01;
    vert2.color.Y := (random(100) + 154) * 0.01;
    vert2.color.Z := 0;//random(254)*0.01;
    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    vert3.coord.X := sin(g3) * r;
    vert3.coord.Y := cos(g3) * r;
    vert3.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
    vert3.color.X := 0;//random(254)*0.01;
    vert3.color.Y := (random(100) + 154) * 0.01;
    vert3.color.Z := 0;//random(254)*0.01;
    //
    master.Vertices.AddVertex3(vert1, vert2, vert3);
    //------------
    {the cap for side}

    g2 := ((360 / num) * i) * (3.14 / 180);
    ad_vert4.coord.X := sin(g2) * r;
    ad_vert4.coord.Y := cos(g2) * r;
    ad_vert4.coord.Z := 0 + cos(g2) * r;
    ad_vert4.color := vert2.color;

    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    ad_vert5.coord.X := sin(g3) * r;
    ad_vert5.coord.Y := cos(g3) * r;
    ad_vert5.coord.Z := 0 + cos(g3) * r;
    ad_vert5.color := vert3.color;

    master.Vertices.AddVertex3(vert3, vert2, ad_vert4);
    master.Vertices.AddVertex3(vert3, ad_vert4, ad_vert5);

  end;
  //--------------

  g1 := ((360 / num) * 0) * (3.14 / 180);
  vert1.coord.X := sin(g1) * r;
  vert1.coord.Y := -l * 0.5;
  vert1.coord.Z := cos(g1) * r;
  vert1.color.X := 0;//random(254)*0.01;
  vert1.color.Y := (random(100) + 154) * 0.01;
  vert1.color.Z := 0;//random(254)*0.01;

  g2 := ((360 / num) * (num - 1)) * (3.14 / 180);
  vert2.coord.X := sin(g2) * r;
  vert2.coord.Y := -l * 0.5;
  vert2.coord.Z := cos(g2) * r;
  vert2.color := vert1.color;

  g2 := ((360 / num) * (num - 1)) * (3.14 / 180);
  ad_vert4.coord.X := sin(g2) * r;
  ad_vert4.coord.Y := l * 0.5;
  ad_vert4.coord.Z := cos(g2) * r;
  ad_vert4.color := vert1.color;

  //
  g3 := ((360 / num) * 0) * (3.14 / 180);
  ad_vert5.coord.X := sin(g3) * r;
  ad_vert5.coord.Y := l * 0.5;
  ad_vert5.coord.Z := cos(g3) * r;
  ad_vert5.color := vert1.color;

  master.Vertices.AddVertex3(vert1, ad_vert5, ad_vert4);
  master.Vertices.AddVertex3(vert2, vert1, ad_vert4);

  //-----------
  {the same for side}

  g1 := ((360 / num) * 0) * (3.14 / 180);
  vert1.coord.X := sin(g1) * r;
  vert1.coord.Y := cos(g1) * r;
  vert1.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
  vert1.color.X := 0;//random(254)*0.01;
  vert1.color.Y := (random(100) + 154) * 0.01;
  vert1.color.Z := 0;//random(254)*0.01;

  g2 := ((360 / num) * (num - 1)) * (3.14 / 180);
  vert2.coord.X := sin(g2) * r;
  vert2.coord.Y := cos(g2) * r;
  vert2.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
  vert2.color := vert1.color;

  g2 := ((360 / num) * (num - 1)) * (3.14 / 180);
  ad_vert4.coord.X := sin(g2) * r;
  ad_vert4.coord.Y := cos(g2) * r;
  ad_vert4.coord.Z := 0 - cos(g2) * r;
  ad_vert4.color := vert1.color;

  //
  g3 := ((360 / num) * 0) * (3.14 / 180);
  ad_vert5.coord.X := sin(g3) * r;
  ad_vert5.coord.Y := cos(g3) * r;
  ad_vert5.coord.Z := 0 - cos(g3) * r;
  ad_vert5.color := vert1.color;

  master.Vertices.AddVertex3(vert1, ad_vert4, ad_vert5);
  master.Vertices.AddVertex3(vert1, vert2, ad_vert4);

end;


procedure TForm1.create_plus(master: TGLMesh; num: integer; r, l: single);
var
  i: integer;
  g1, g2, g3: single;

begin
  master.Vertices.Clear;
  //-------------
  g1 := ((360 / num) * 0) * (3.14 / 180);
  vert1.coord.X := sin(g1) * r;
  vert1.coord.Y := -l * 0.5;
  vert1.coord.Z := cos(g1) * r;
  vert1.color.X := 0;//random(254)*0.01;
  vert1.color.Y := (random(100) + 154) * 0.01;
  vert1.color.Z := 0;//random(254)*0.01;

  g2 := ((360 / num) * 1) * (3.14 / 180);
  vert2.coord.X := sin(g2) * r;
  vert2.coord.Y := -l * 0.5;
  vert2.coord.Z := cos(g2) * r;
  vert2.color := vert1.color;

  g2 := ((360 / num) * 1) * (3.14 / 180);
  ad_vert4.coord.X := sin(g2) * r;
  ad_vert4.coord.Y := l * 0.5;
  ad_vert4.coord.Z := cos(g2) * r;
  ad_vert4.color := vert1.color;

  //
  g3 := ((360 / num) * 0) * (3.14 / 180);
  ad_vert5.coord.X := sin(g3) * r;
  ad_vert5.coord.Y := l * 0.5;
  ad_vert5.coord.Z := cos(g3) * r;
  ad_vert5.color := vert1.color;

  master.Vertices.AddVertex3(vert1, vert2, ad_vert4);
  master.Vertices.AddVertex3(vert1, ad_vert4, ad_vert5);
  //-----------------
  {the same for side}

  g1 := ((360 / num) * 0) * (3.14 / 180);
  vert1.coord.X := sin(g1) * r;
  vert1.coord.Y := cos(g1) * r;
  vert1.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
  vert1.color.X := 0;//random(254)*0.01;
  vert1.color.Y := (random(100) + 154) * 0.01;
  vert1.color.Z := 0;//random(254)*0.01;

  g2 := ((360 / num) * 1) * (3.14 / 180);
  vert2.coord.X := sin(g2) * r;
  vert2.coord.Y := cos(g2) * r;
  vert2.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
  vert2.color := vert1.color;

  g2 := ((360 / num) * 1) * (3.14 / 180);
  ad_vert4.coord.X := sin(g2) * r;
  ad_vert4.coord.Y := cos(g2) * r;
  ad_vert4.coord.Z := 0 - cos(g2) * r;
  ad_vert4.color := vert1.color;

  //
  g3 := ((360 / num) * 0) * (3.14 / 180);
  ad_vert5.coord.X := sin(g3) * r;
  ad_vert5.coord.Y := cos(g3) * r;
  ad_vert5.coord.Z := 0 - cos(g3) * r;
  ad_vert5.color := vert1.color;

  master.Vertices.AddVertex3(vert2, vert1, ad_vert4);
  master.Vertices.AddVertex3(vert1, ad_vert5, ad_vert4);

  //-----------------
  {the same for side-2}

  g1 := ((360 / num) * 0) * (3.14 / 180);
  vert1.coord.X := sin(g1) * r;
  vert1.coord.Y := cos(g1) * r;
  vert1.coord.Z := l * 0.5;//+(r*0.5);
  vert1.color.X := 0;//random(254)*0.01;
  vert1.color.Y := (random(100) + 154) * 0.01;
  vert1.color.Z := 0;//random(254)*0.01;

  g2 := ((360 / num) * 1) * (3.14 / 180);
  vert2.coord.X := sin(g2) * r;
  vert2.coord.Y := cos(g2) * r;
  vert2.coord.Z := l * 0.5;//+(r*0.5);
  vert2.color := vert1.color;

  g2 := ((360 / num) * 1) * (3.14 / 180);
  ad_vert4.coord.X := sin(g2) * r;
  ad_vert4.coord.Y := cos(g2) * r;
  ad_vert4.coord.Z := 0 + cos(g2) * r;
  ad_vert4.color := vert1.color;

  //
  g3 := ((360 / num) * 0) * (3.14 / 180);
  ad_vert5.coord.X := sin(g3) * r;
  ad_vert5.coord.Y := cos(g3) * r;
  ad_vert5.coord.Z := 0 + cos(g3) * r;
  ad_vert5.color := vert1.color;

  master.Vertices.AddVertex3(vert1, vert2, ad_vert4);
  master.Vertices.AddVertex3(vert1, ad_vert4, ad_vert5);
  //
  for i := 1 to num - 2 do
  begin
    g1 := ((360 / num) * 0) * (3.14 / 180);
    vert1.coord.X := sin(g1) * r;
    vert1.coord.Y := -l * 0.5;
    vert1.coord.Z := cos(g1) * r;
    ;
    vert1.color.X := 0;//random(254)*0.01;
    vert1.color.Y := (random(100) + 154) * 0.01;
    vert1.color.Z := 0;//random(254)*0.01;
    //
    g2 := ((360 / num) * i) * (3.14 / 180);
    vert2.coord.X := sin(g2) * r;
    vert2.coord.Y := -l * 0.5;
    vert2.coord.Z := cos(g2) * r;
    ;
    vert2.color.X := 0;//random(254)*0.01;
    vert2.color.Y := (random(100) + 154) * 0.01;
    vert2.color.Z := 0;//random(254)*0.01;
    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    vert3.coord.X := sin(g3) * r;
    vert3.coord.Y := -l * 0.5;
    vert3.coord.Z := cos(g3) * r;
    vert3.color.X := 0;//random(254)*0.01;
    vert3.color.Y := (random(100) + 154) * 0.01;
    vert3.color.Z := 0;//random(254)*0.01;
    //
    master.Vertices.AddVertex3(vert1, vert3, vert2);

    //---------------------
    g2 := ((360 / num) * i) * (3.14 / 180);
    ad_vert4.coord.X := sin(g2) * r;
    ad_vert4.coord.Y := l * 0.5;
    ad_vert4.coord.Z := cos(g2) * r;
    ad_vert4.color := vert2.color;

    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    ad_vert5.coord.X := sin(g3) * r;
    ad_vert5.coord.Y := l * 0.5;
    ad_vert5.coord.Z := cos(g3) * r;
    ad_vert5.color := vert3.color;

    master.Vertices.AddVertex3(vert2, vert3, ad_vert4);
    master.Vertices.AddVertex3(vert3, ad_vert5, ad_vert4);

    //-----------------

    g1 := ((360 / num) * 0) * (3.14 / 180);
    vert1.coord.X := sin(g1) * r;
    vert1.coord.Y := l * 0.5;
    vert1.coord.Z := cos(g1) * r;
    vert1.color.X := 0;//random(254)*0.01;
    vert1.color.Y := (random(100) + 154) * 0.01;
    vert1.color.Z := 0;//random(254)*0.01;
    //
    g2 := ((360 / num) * i) * (3.14 / 180);
    vert2.coord.X := sin(g2) * r;
    vert2.coord.Y := l * 0.5;
    vert2.coord.Z := cos(g2) * r;
    vert2.color.X := 0;//random(254)*0.01;
    vert2.color.Y := (random(100) + 154) * 0.01;
    vert2.color.Z := 0;//random(254)*0.01;
    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    vert3.coord.X := sin(g3) * r;
    vert3.coord.Y := l * 0.5;
    vert3.coord.Z := cos(g3) * r;
    vert3.color.X := 0;//random(254)*0.01;
    vert3.color.Y := (random(100) + 154) * 0.01;
    vert3.color.Z := 0;//random(254)*0.01;
    //
    master.Vertices.AddVertex3(vert1, vert2, vert3);

    //--------------
    { the side}

    g1 := ((360 / num) * 0) * (3.14 / 180);
    vert1.coord.X := sin(g1) * r;
    vert1.coord.Y := cos(g1) * r;
    vert1.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
    vert1.color.X := 0;//random(254)*0.01;
    vert1.color.Y := (random(100) + 154) * 0.01;
    vert1.color.Z := 0;//random(254)*0.01;
    //
    g2 := ((360 / num) * i) * (3.14 / 180);
    vert2.coord.X := sin(g2) * r;
    vert2.coord.Y := cos(g2) * r;
    vert2.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
    vert2.color.X := 0;//random(254)*0.01;
    vert2.color.Y := (random(100) + 154) * 0.01;
    vert2.color.Z := 0;//random(254)*0.01;
    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    vert3.coord.X := sin(g3) * r;
    vert3.coord.Y := cos(g3) * r;
    vert3.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
    vert3.color.X := 0;//random(254)*0.01;
    vert3.color.Y := (random(100) + 154) * 0.01;
    vert3.color.Z := 0;//random(254)*0.01;
    //
    master.Vertices.AddVertex3(vert1, vert2, vert3);
    //------------
    {cap for the side surface}

    g2 := ((360 / num) * i) * (3.14 / 180);
    ad_vert4.coord.X := sin(g2) * r;
    ad_vert4.coord.Y := cos(g2) * r;
    ad_vert4.coord.Z := 0 + cos(g2) * r;
    ad_vert4.color := vert2.color;

    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    ad_vert5.coord.X := sin(g3) * r;
    ad_vert5.coord.Y := cos(g3) * r;
    ad_vert5.coord.Z := 0 + cos(g3) * r;
    ad_vert5.color := vert3.color;

    master.Vertices.AddVertex3(vert3, vert2, ad_vert4);
    master.Vertices.AddVertex3(vert3, ad_vert4, ad_vert5);

    //--------------
    {the side-2}

    g1 := ((360 / num) * 0) * (3.14 / 180);
    vert1.coord.X := sin(g1) * r;
    vert1.coord.Y := cos(g1) * r;
    vert1.coord.Z := l * 0.5;//+(r*0.5);
    vert1.color.X := 0;//random(254)*0.01;
    vert1.color.Y := (random(100) + 154) * 0.01;
    vert1.color.Z := 0;//random(254)*0.01;
    //
    g2 := ((360 / num) * i) * (3.14 / 180);
    vert2.coord.X := sin(g2) * r;
    vert2.coord.Y := cos(g2) * r;
    vert2.coord.Z := l * 0.5;//+(r*0.5);
    vert2.color.X := 0;//random(254)*0.01;
    vert2.color.Y := (random(100) + 154) * 0.01;
    vert2.color.Z := 0;//random(254)*0.01;
    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    vert3.coord.X := sin(g3) * r;
    vert3.coord.Y := cos(g3) * r;
    vert3.coord.Z := l * 0.5;//+(r*0.5);
    vert3.color.X := 0;//random(254)*0.01;
    vert3.color.Y := (random(100) + 154) * 0.01;
    vert3.color.Z := 0;//random(254)*0.01;
    //
    master.Vertices.AddVertex3(vert1, vert3, vert2);

    //------------
    {cap for the side-2}

    g2 := ((360 / num) * i) * (3.14 / 180);
    ad_vert4.coord.X := sin(g2) * r;
    ad_vert4.coord.Y := cos(g2) * r;
    ad_vert4.coord.Z := 0 - cos(g2) * r;
    ad_vert4.color := vert2.color;

    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    ad_vert5.coord.X := sin(g3) * r;
    ad_vert5.coord.Y := cos(g3) * r;
    ad_vert5.coord.Z := 0 - cos(g3) * r;
    ad_vert5.color := vert3.color;

    master.Vertices.AddVertex3(vert2, vert3, ad_vert4);
    master.Vertices.AddVertex3(vert3, ad_vert5, ad_vert4);
    //
  end;
  //--------------

  g1 := ((360 / num) * 0) * (3.14 / 180);
  vert1.coord.X := sin(g1) * r;
  vert1.coord.Y := -l * 0.5;
  vert1.coord.Z := cos(g1) * r;
  vert1.color.X := 0;//random(254)*0.01;
  vert1.color.Y := (random(100) + 154) * 0.01;
  vert1.color.Z := 0;//random(254)*0.01;

  g2 := ((360 / num) * (num - 1)) * (3.14 / 180);
  vert2.coord.X := sin(g2) * r;
  vert2.coord.Y := -l * 0.5;
  vert2.coord.Z := cos(g2) * r;
  vert2.color := vert1.color;

  g2 := ((360 / num) * (num - 1)) * (3.14 / 180);
  ad_vert4.coord.X := sin(g2) * r;
  ad_vert4.coord.Y := l * 0.5;
  ad_vert4.coord.Z := cos(g2) * r;
  ad_vert4.color := vert1.color;

  //
  g3 := ((360 / num) * 0) * (3.14 / 180);
  ad_vert5.coord.X := sin(g3) * r;
  ad_vert5.coord.Y := l * 0.5;
  ad_vert5.coord.Z := cos(g3) * r;
  ad_vert5.color := vert1.color;

  master.Vertices.AddVertex3(vert1, ad_vert5, ad_vert4);
  master.Vertices.AddVertex3(vert2, vert1, ad_vert4);

  //-----------
  {òî æå äëÿ áîêà}

  g1 := ((360 / num) * 0) * (3.14 / 180);
  vert1.coord.X := sin(g1) * r;
  vert1.coord.Y := cos(g1) * r;
  vert1.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
  vert1.color.X := 0;//random(254)*0.01;
  vert1.color.Y := (random(100) + 154) * 0.01;
  vert1.color.Z := 0;//random(254)*0.01;

  g2 := ((360 / num) * (num - 1)) * (3.14 / 180);
  vert2.coord.X := sin(g2) * r;
  vert2.coord.Y := cos(g2) * r;
  vert2.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
  vert2.color := vert1.color;

  g2 := ((360 / num) * (num - 1)) * (3.14 / 180);
  ad_vert4.coord.X := sin(g2) * r;
  ad_vert4.coord.Y := cos(g2) * r;
  ad_vert4.coord.Z := 0 - cos(g2) * r;
  ad_vert4.color := vert1.color;

  //
  g3 := ((360 / num) * 0) * (3.14 / 180);
  ad_vert5.coord.X := sin(g3) * r;
  ad_vert5.coord.Y := cos(g3) * r;
  ad_vert5.coord.Z := 0 - cos(g3) * r;
  ad_vert5.color := vert1.color;

  master.Vertices.AddVertex3(vert1, ad_vert4, ad_vert5);
  master.Vertices.AddVertex3(vert1, vert2, ad_vert4);


  //-----------
  {òî æå äëÿ áîêà-2}

  g1 := ((360 / num) * 0) * (3.14 / 180);
  vert1.coord.X := sin(g1) * r;
  vert1.coord.Y := cos(g1) * r;
  vert1.coord.Z := l * 0.5;//+(r*0.5);
  vert1.color.X := 0;//random(254)*0.01;
  vert1.color.Y := (random(100) + 154) * 0.01;
  vert1.color.Z := 0;//random(254)*0.01;

  g2 := ((360 / num) * (num - 1)) * (3.14 / 180);
  vert2.coord.X := sin(g2) * r;
  vert2.coord.Y := cos(g2) * r;
  vert2.coord.Z := l * 0.5;//+(r*0.5);
  vert2.color := vert1.color;

  g2 := ((360 / num) * (num - 1)) * (3.14 / 180);
  ad_vert4.coord.X := sin(g2) * r;
  ad_vert4.coord.Y := cos(g2) * r;
  ad_vert4.coord.Z := 0 + cos(g2) * r;
  ad_vert4.color := vert1.color;

  //
  g3 := ((360 / num) * 0) * (3.14 / 180);
  ad_vert5.coord.X := sin(g3) * r;
  ad_vert5.coord.Y := cos(g3) * r;
  ad_vert5.coord.Z := 0 + cos(g3) * r;
  ad_vert5.color := vert1.color;

  master.Vertices.AddVertex3(vert1, ad_vert5, ad_vert4);
  master.Vertices.AddVertex3(vert2, vert1, ad_vert4);
end;




procedure TForm1.AsyncTimer1Timer(Sender: TObject);
var
  i: integer;
begin
  //if vcur<=vcount then
  //begin

  if iterstep < iter then
    ci := vcur + trunc(vcount / iter)
  else
  begin
    ci := vcount;
    asynctimer1.Enabled := False;
  end;

  for i := vcur to ci do
  begin
    vert1.color.X := 0;
    vert1.color.Y := 0;
    vert1.color.Z := (random(100) + 154) * 0.01;
    vert1.coord := glmesh1.Vertices.Vertices[i - 1].coord;

    glmesh1.Vertices.Vertices[i - 1] := vert1;

  end;
  Inc(iterstep);
  vcur := ci;

  //end
  //else asynctimer1.Enabled:=false;

end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  //glmesh1.Vertices.Clear;
end;


procedure TForm1.Button4Click(Sender: TObject);
begin
  create_T(glmesh1, StrToInt(edit1.Text),
    strtofloat(edit2.Text), strtofloat(edit3.Text));
  form1.Caption := IntToStr(GLMesh1.Vertices.Count);
end;


procedure TForm1.Button7Click(Sender: TObject);
begin
  create_plus(glmesh1, StrToInt(edit1.Text),
    strtofloat(edit2.Text), strtofloat(edit3.Text));
  form1.Caption := IntToStr(GLMesh1.Vertices.Count);
end;


procedure TForm1.Button8Click(Sender: TObject);
begin
  create_G(glmesh1, StrToInt(edit1.Text),
    strtofloat(edit2.Text), strtofloat(edit3.Text));
  form1.Caption := IntToStr(GLMesh1.Vertices.Count);
end;


procedure TForm1.create_G(master: TGLMesh; num: integer; r, l: single);
var
  i: integer;
  g1, g2, g3: single;

begin
  master.Vertices.Clear;
  //-------------
  g1 := ((360 / num) * 0) * (3.14 / 180);
  vert1.coord.X := sin(g1) * r;
  vert1.coord.Y := 0 - sin(g1 + 1.57) * r;
  vert1.coord.Z := cos(g1) * r;
  vert1.color.X := 0;//random(254)*0.01;
  vert1.color.Y := (random(100) + 154) * 0.01;
  vert1.color.Z := 0;//random(254)*0.01;

  g2 := ((360 / num) * 1) * (3.14 / 180);
  vert2.coord.X := sin(g2) * r;
  vert2.coord.Y := 0 - sin(g2 + 1.57) * r;
  vert2.coord.Z := cos(g2) * r;
  vert2.color := vert1.color;

  g2 := ((360 / num) * 1) * (3.14 / 180);
  ad_vert4.coord.X := sin(g2) * r;
  ad_vert4.coord.Y := l * 0.5;
  ad_vert4.coord.Z := cos(g2) * r;
  ad_vert4.color := vert1.color;

  //
  g3 := ((360 / num) * 0) * (3.14 / 180);
  ad_vert5.coord.X := sin(g3) * r;
  ad_vert5.coord.Y := l * 0.5;
  ad_vert5.coord.Z := cos(g3) * r;
  ad_vert5.color := vert1.color;

  master.Vertices.AddVertex3(vert1, vert2, ad_vert4);
  master.Vertices.AddVertex3(vert1, ad_vert4, ad_vert5);
  //-----------------
  {the same for side surface}

  g1 := ((360 / num) * 0) * (3.14 / 180);
  vert1.coord.X := sin(g1) * r;
  vert1.coord.Y := cos(g1) * r;
  vert1.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
  vert1.color.X := 0;//random(254)*0.01;
  vert1.color.Y := (random(100) + 154) * 0.01;
  vert1.color.Z := 0;//random(254)*0.01;

  g2 := ((360 / num) * 1) * (3.14 / 180);
  vert2.coord.X := sin(g2) * r;
  vert2.coord.Y := cos(g2) * r;
  vert2.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
  vert2.color := vert1.color;

  g2 := ((360 / num) * 1) * (3.14 / 180);
  ad_vert4.coord.X := sin(g2) * r;
  ad_vert4.coord.Y := cos(g2) * r;
  ad_vert4.coord.Z := 0 - cos(g2) * r;
  ad_vert4.color := vert1.color;

  //
  g3 := ((360 / num) * 0) * (3.14 / 180);
  ad_vert5.coord.X := sin(g3) * r;
  ad_vert5.coord.Y := cos(g3) * r;
  ad_vert5.coord.Z := 0 - cos(g3) * r;
  ad_vert5.color := vert1.color;

  master.Vertices.AddVertex3(vert2, vert1, ad_vert4);
  master.Vertices.AddVertex3(vert1, ad_vert5, ad_vert4);
  //
  for i := 1 to num - 2 do
  begin

    //-----------------

    g1 := ((360 / num) * 0) * (3.14 / 180);
    vert1.coord.X := sin(g1) * r;
    vert1.coord.Y := l * 0.5;
    vert1.coord.Z := cos(g1) * r;
    vert1.color.X := 0;//random(254)*0.01;
    vert1.color.Y := (random(100) + 154) * 0.01;
    vert1.color.Z := 0;//random(254)*0.01;
    //
    g2 := ((360 / num) * i) * (3.14 / 180);
    vert2.coord.X := sin(g2) * r;
    vert2.coord.Y := l * 0.5;
    vert2.coord.Z := cos(g2) * r;
    vert2.color.X := 0;//random(254)*0.01;
    vert2.color.Y := (random(100) + 154) * 0.01;
    vert2.color.Z := 0;//random(254)*0.01;
    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    vert3.coord.X := sin(g3) * r;
    vert3.coord.Y := l * 0.5;
    vert3.coord.Z := cos(g3) * r;
    vert3.color.X := 0;//random(254)*0.01;
    vert3.color.Y := (random(100) + 154) * 0.01;
    vert3.color.Z := 0;//random(254)*0.01;
    //
    master.Vertices.AddVertex3(vert1, vert2, vert3);
    //---------------------
    g2 := ((360 / num) * i) * (3.14 / 180);
    ad_vert4.coord.X := sin(g2) * r;
    ad_vert4.coord.Y := 0 - sin(g2 + 1.57) * r;
    ad_vert4.coord.Z := cos(g2) * r;
    ad_vert4.color := vert2.color;

    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    ad_vert5.coord.X := sin(g3) * r;
    ad_vert5.coord.Y := 0 - sin(g3 + 1.57) * r;
    ad_vert5.coord.Z := cos(g3) * r;
    ad_vert5.color := vert3.color;

    master.Vertices.AddVertex3(vert3, vert2, ad_vert4);
    master.Vertices.AddVertex3(vert3, ad_vert4, ad_vert5);

    //--------------
    {side}

    g1 := ((360 / num) * 0) * (3.14 / 180);
    vert1.coord.X := sin(g1) * r;
    vert1.coord.Y := cos(g1) * r;
    vert1.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
    vert1.color.X := 0;//random(254)*0.01;
    vert1.color.Y := (random(100) + 154) * 0.01;
    vert1.color.Z := 0;//random(254)*0.01;
    //
    g2 := ((360 / num) * i) * (3.14 / 180);
    vert2.coord.X := sin(g2) * r;
    vert2.coord.Y := cos(g2) * r;
    vert2.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
    vert2.color.X := 0;//random(254)*0.01;
    vert2.color.Y := (random(100) + 154) * 0.01;
    vert2.color.Z := 0;//random(254)*0.01;
    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    vert3.coord.X := sin(g3) * r;
    vert3.coord.Y := cos(g3) * r;
    vert3.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
    vert3.color.X := 0;//random(254)*0.01;
    vert3.color.Y := (random(100) + 154) * 0.01;
    vert3.color.Z := 0;//random(254)*0.01;
    //
    master.Vertices.AddVertex3(vert1, vert2, vert3);
    //------------
    {cap for the side}

    g2 := ((360 / num) * i) * (3.14 / 180);
    ad_vert4.coord.X := sin(g2) * r;
    ad_vert4.coord.Y := cos(g2) * r;
    ad_vert4.coord.Z := 0 - cos(g2) * r;
    ad_vert4.color := vert2.color;

    //
    g3 := ((360 / num) * (i + 1)) * (3.14 / 180);
    ad_vert5.coord.X := sin(g3) * r;
    ad_vert5.coord.Y := cos(g3) * r;
    ad_vert5.coord.Z := 0 - cos(g3) * r;
    ad_vert5.color := vert3.color;

    master.Vertices.AddVertex3(vert3, vert2, ad_vert4);
    master.Vertices.AddVertex3(vert3, ad_vert4, ad_vert5);

  end;
  //--------------

  g1 := ((360 / num) * 0) * (3.14 / 180);
  vert1.coord.X := sin(g1) * r;
  vert1.coord.Y := 0 - sin(g1 + 1.57) * r;
  vert1.coord.Z := cos(g1) * r;
  vert1.color.X := 0;//random(254)*0.01;
  vert1.color.Y := (random(100) + 154) * 0.01;
  vert1.color.Z := 0;//random(254)*0.01;

  g2 := ((360 / num) * (num - 1)) * (3.14 / 180);
  vert2.coord.X := sin(g2) * r;
  vert2.coord.Y := 0 - sin(g2 + 1.57) * r;
  vert2.coord.Z := cos(g2) * r;
  vert2.color := vert1.color;

  g2 := ((360 / num) * (num - 1)) * (3.14 / 180);
  ad_vert4.coord.X := sin(g2) * r;
  ad_vert4.coord.Y := l * 0.5;
  ad_vert4.coord.Z := cos(g2) * r;
  ad_vert4.color := vert1.color;

  //
  g3 := ((360 / num) * 0) * (3.14 / 180);
  ad_vert5.coord.X := sin(g3) * r;
  ad_vert5.coord.Y := l * 0.5;
  ad_vert5.coord.Z := cos(g3) * r;
  ad_vert5.color := vert1.color;

  master.Vertices.AddVertex3(vert1, ad_vert5, ad_vert4);
  master.Vertices.AddVertex3(vert2, vert1, ad_vert4);

  //-----------
  {the same for side}

  g1 := ((360 / num) * 0) * (3.14 / 180);
  vert1.coord.X := sin(g1) * r;
  vert1.coord.Y := cos(g1) * r;
  vert1.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
  vert1.color.X := 0;//random(254)*0.01;
  vert1.color.Y := (random(100) + 154) * 0.01;
  vert1.color.Z := 0;//random(254)*0.01;

  g2 := ((360 / num) * (num - 1)) * (3.14 / 180);
  vert2.coord.X := sin(g2) * r;
  vert2.coord.Y := cos(g2) * r;
  vert2.coord.Z := -(l * 0.5);//-l*0.5-(r*0.5);
  vert2.color := vert1.color;

  g2 := ((360 / num) * (num - 1)) * (3.14 / 180);
  ad_vert4.coord.X := sin(g2) * r;
  ad_vert4.coord.Y := cos(g2) * r;
  ad_vert4.coord.Z := 0 - cos(g2) * r;
  ad_vert4.color := vert1.color;

  //
  g3 := ((360 / num) * 0) * (3.14 / 180);
  ad_vert5.coord.X := sin(g3) * r;
  ad_vert5.coord.Y := cos(g3) * r;
  ad_vert5.coord.Z := 0 - cos(g3) * r;
  ad_vert5.color := vert1.color;

  master.Vertices.AddVertex3(vert1, ad_vert4, ad_vert5);
  master.Vertices.AddVertex3(vert1, vert2, ad_vert4);

end;


end.

