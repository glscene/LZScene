unit Unit1;


interface

uses
  LCLType,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  ExtCtrls,
  StdCtrls,
  Dialogs,
  GLScene,
  GLVectorFileObjects,
  GLObjects,
  GLCadencer,
  GLTexture,
  GLVectorLists,
  GLVectorGeometry,
  GLTerrainRenderer,
  GLHeightData,
  GLFireFX,
  GLCoordinates,
  GLCrossPlatform, GLLCLViewer,
  GLFile3ds;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLFreeForm1: TGLFreeForm;
    GLLightSource1: TGLLightSource;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    GLTerrainRenderer1: TGLTerrainRenderer;
    GLBitmapHDS1: TGLBitmapHDS;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
    procedure GLSceneViewer1AfterRender(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLDummyCube1Progress(Sender: TObject; const deltaTime, newTime: double);
  private

  public

    mx, my, mx2, my2: integer;
  end;

var
  Form1: TForm1;
  angulo: single;
  Fig: TAffineVectorList;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
Begin
end;

procedure TForm1.FormShow(Sender: TObject);
  var
  i: integer;
begin
  GLFreeForm1.LoadFromFile('media\dolphin.3ds');
  // Guardamos el estado inicial del delfin
  Fig := TAffineVectorList.Create;
  Fig.Assign(GLFreeForm1.MeshObjects.Items[0].Vertices);
  // cargamos el fondo marino...
  GLBitmapHDS1.MaxPoolSize := 8 * 1024 * 1024;
  GLBitmapHDS1.Picture.LoadFromFile('media\terrain.bmp');
  GLTerrainRenderer1.TilesPerTexture := 256 / GLTerrainRenderer1.TileSize;
  GLTerrainRenderer1.Material.Texture.Image.LoadFromFile('media\tex.jpg');
  GLTerrainRenderer1.Material.Texture.Disabled := False;

  GLSceneViewer1.Buffer.BackgroundColor := rgb(0, 0, 160);
  with GLSceneViewer1.Buffer.FogEnvironment do
  begin
    FogColor.AsWinColor := rgb(0, 0, 110 { 160 });
    FogStart := -FogStart;
  end;
  Timer1.Enabled:=true;

end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  mx := X;
  my := Y;
  mx2 := X;
  my2 := Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if ssLeft in Shift then
  begin
    mx2 := X;
    my2 := Y;
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
const
  AMPLITUD_ALETAZO = 20;
  FRECUENCIA_DE_PATADA = 15;
var
  j: integer;
  v: TAffineVector;
  f: single;
begin
  // Permission to move around target...
  if ((mx <> mx2) or (my <> my2)) then
  begin
    GLCamera1.MoveAroundTarget(my - my2, mx - mx2);
    mx := mx2;
    my := my2;
  end;

  if CheckBox1.Checked then
  begin
    angulo := newTime;
    // Application of animation for Dolfin...
    for j := 0 to GLFreeForm1.MeshObjects.Items[0].Vertices.Count - 1 do
      with GLFreeForm1.MeshObjects.Items[0].Vertices do
      begin
        f := sin(angulo * FRECUENCIA_DE_PATADA) * (sqr(Items[j].X) / AMPLITUD_ALETAZO);
        v.X := 0;
        v.Y := 0;
        v.Z := f;
        TranslateItem(j, v);
      end;
    GLFreeForm1.StructureChanged;
  end;
end;

procedure TForm1.GLSceneViewer1AfterRender(Sender: TObject);
var
  j: integer;
  v: TAffineVector;
  f: single;
begin
  // Despues de Presentar un cuadro regresamos al delfín
  // a su estado inicial...
  GLFreeForm1.MeshObjects.Items[0].Vertices.Assign(Fig);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Fig.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.2f FPS  Dolphin animation', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLDummyCube1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  GLDummyCube1.Move(3);
end;

end.
