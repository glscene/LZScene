{ Basic terrain rendering demo.

  The base terrain renderer uses a hybrid ROAM/brute-force approach to
  rendering terrain, by requesting height data tiles, then rendering them
  using either triangle strips (for those below "QualityDistance") or ROAM
  tessellation.
  Controls:
  Direction keys move the came nora (shift to speedup)
  PageUp/PageDown move the camera up and down
  Orient the camera freely by holding down the left button
  Toggle wireframe mode with 'w'
  Increase/decrease the viewing distance with '+'/'-'.
  Increase/decrease CLOD precision with '*' and '/'.
  Increase/decrease QualityDistance with '9' and '8'.

  When increasing the range, or moving after having increased the range you
  may notice a one-time slowdown, this originates in the base height data
  being duplicated to create the illusion of an "infinite" terrain (at max
  range the visible area covers 1024x1024 height samples, and with tiles of
  size 16 or less, this is a lot of tiles to prepare).
}
unit Unit1;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages,// weird, but for Winapi.Windows there is no wireframes mode

 SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,


  GLScene,
  GLState,
  GLTerrainRenderer,
  GLBaseClasses,
  GLObjects,
  GLHeightData,
  GLMaterial,
  GLCadencer,
  GLTexture,
  GLHUDObjects,
  GLBitmapFont,
  GLSkydome,
  GLVectorGeometry,
  GLMesh,
  GLVectorFileObjects,
  GLFireFX,
  GLCoordinates,
  GLCrossPlatform, GLLCLViewer,
  GLFile3DS,
  GLKeyboard;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLBitmapHDS1: TGLBitmapHDS;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    Label1: TLabel;
    TerrainRenderer1: TGLTerrainRenderer;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    SkyDome1: TGLSkyDome;
    FreeForm1: TGLFreeForm;
    GLFireFXManager1: TGLFireFXManager;
    DummyCube2: TGLDummyCube;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
  public
    mx, my: Integer;
    fullScreen: Boolean;
    FCamHeight: Single;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  fullScreen := false;
  SetCurrentDir(ExtractFilePath(Application.ExeName));
  // 8 MB height data cache
  // Note this is the data size in terms of elevation samples, it does not
  // take into account all the data required/allocated by the renderer
  GLBitmapHDS1.MaxPoolSize := 8 * 1024 * 1024;
  // specify height map data
  GLBitmapHDS1.Picture.LoadFromFile('terrain.bmp');
  // load the texture maps
  GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile
    ('snow512.jpg');
  GLMaterialLibrary1.Materials[1].Material.Texture.Image.LoadFromFile
    ('detailmap.jpg');
  // apply texture map scale (our heightmap size is 256)
  TerrainRenderer1.TilesPerTexture := 256 / TerrainRenderer1.TileSize;
  // Could've been done at design time, but it the, it hurts the eyes ;)
  GLSceneViewer1.Buffer.BackgroundColor := clBlack;
  // Move camera starting point to an interesting hand-picked location
  DummyCube1.Position.X := 570;
  DummyCube1.Position.Z := -385;
  DummyCube1.Turn(90);
  // Initial camera height offset (controled with pageUp/pageDown)
  FCamHeight := 10;
  with SkyDome1 do
  begin
    Bands[1].StopColor.AsWinColor := RGB(0, 0, 16);
    Bands[1].StartColor.AsWinColor := RGB(0, 0, 8);
    Bands[0].StopColor.AsWinColor := RGB(0, 0, 8);
    Bands[0].StartColor.AsWinColor := RGB(0, 0, 0);
    with Stars do
    begin
      AddRandomStars(700, clWhite, True); // many white stars
      AddRandomStars(100, RGB(255, 200, 200), True); // some redish ones
      AddRandomStars(100, RGB(200, 200, 255), True); // some blueish ones
      AddRandomStars(100, RGB(255, 255, 200), True); // some yellowish ones
    end;
    GLSceneViewer1.Buffer.BackgroundColor := clBlack;
    with GLSceneViewer1.Buffer.FogEnvironment do
    begin
      FogColor.AsWinColor := clBlack;
      FogStart := -FogStart; // Fog is used to make things darker
    end;
  end;
  FreeForm1.LoadFromFile('ship.3ds');
  FreeForm1.Material.Texture.Image.LoadFromFile('avion512.jpg');
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  speed: Single;
begin
  // handle keypresses
  { if IsKeyDown(VK_SHIFT) then
    speed:=300*deltaTime
    else } speed := 150 * deltaTime;
  // with GLCamera1.Position do begin
  DummyCube1.Translate(FreeForm1.direction.Z * speed, -FreeForm1.direction.Y *
    speed, -FreeForm1.direction.X * speed);
  if IsKeyDown(VK_UP) then
  begin
    FreeForm1.Pitch(1);
    GLCamera1.Pitch(1);
    // GLCamera1.MoveAroundTarget(-1, 0);
  end;
  if IsKeyDown(VK_DOWN) then
  begin
    FreeForm1.Pitch(-1);
    GLCamera1.Pitch(-1);
    // GLCamera1.MoveAroundTarget(1, 0);
  end;
  if IsKeyDown(VK_LEFT) then
  begin
    // DummyCube1.Translate(-X*speed, 0, -Z*speed);
    // freeform1.Turn(-1);
    FreeForm1.Roll(-1);
    GLCamera1.Roll(1);
    // GLCamera1.MoveAroundTarget(0, 1);
  end;
  if IsKeyDown(VK_RIGHT) then
  begin
    // DummyCube1.Translate(X*speed, 0, Z*speed);
    // freeform1.Turn(1);
    FreeForm1.Roll(1);
    GLCamera1.Roll(-1);
    // GLCamera1.MoveAroundTarget(0, -1);
  end;
  { if IsKeyDown(VK_PRIOR) then
    FCamHeight:=FCamHeight+10*speed;
    if IsKeyDown(VK_NEXT) then
    FCamHeight:=FCamHeight-10*speed; }
  if IsKeyDown(VK_ESCAPE) then
    Close;
  // end;
  // don't drop through terrain!

  with DummyCube1.Position do
    if Y < TerrainRenderer1.InterpolatedHeight(AsVector) then
      Y := TerrainRenderer1.InterpolatedHeight(AsVector) + FCamHeight;
end;

// Standard mouse rotation & FPS code below

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget(my-y, mx-x);
    mx := X;
    my := Y;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    'w', 'W':
      with GLMaterialLibrary1.Materials[0].Material do
      begin
        if PolygonMode = pmLines then
          PolygonMode := pmFill
        else
          PolygonMode := pmLines;
      end;
    '+':
      if GLCamera1.DepthOfView < 2000 then
      begin
        GLCamera1.DepthOfView := GLCamera1.DepthOfView * 1.2;
        with GLSceneViewer1.Buffer.FogEnvironment do
        begin
          FogEnd := FogEnd * 1.2;
          FogStart := FogStart * 1.2;
        end;
      end;
    '-':
      if GLCamera1.DepthOfView > 300 then
      begin
        GLCamera1.DepthOfView := GLCamera1.DepthOfView / 1.2;
        with GLSceneViewer1.Buffer.FogEnvironment do
        begin
          FogEnd := FogEnd / 1.2;
          FogStart := FogStart / 1.2;
        end;
      end;
    '*':
      with TerrainRenderer1 do
        if CLODPrecision > 20 then
          CLODPrecision := Round(CLODPrecision * 0.8);
    '/':
      with TerrainRenderer1 do
        if CLODPrecision < 1000 then
          CLODPrecision := Round(CLODPrecision * 1.2);
    '8':
      with TerrainRenderer1 do
        if QualityDistance > 40 then
          QualityDistance := Round(QualityDistance * 0.8);
    '9':
      with TerrainRenderer1 do
        if QualityDistance < 1000 then
          QualityDistance := Round(QualityDistance * 1.2);
  end;

  Key := #0;
end;

end.
