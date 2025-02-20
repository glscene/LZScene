unit fDuneFighterU;

{$MODE Delphi}

interface

uses
  LCLType,
  SysUtils,
  StdCtrls,
  Buttons,
  Controls,
  ExtCtrls,
  Forms,
  Graphics,


  GLCadencer,
  GLVectorFileObjects,
  GLScene,
  GLObjects,
  GLMaterial,
  GLNavigator,
  GLTerrainRenderer,
  GLCoordinates,
  GLCrossPlatform,

  GLSkydome,
  GLFileMD2,
  GLFile3DS,
  GLTexture,
  GLColor,
  GLVectorGeometry,
  GLKeyboard, GLLCLViewer,
  GLRandomHDS;

type
  TFDuneFighter = class(TForm)
    GLScene1: TGLScene;
    camThirdPerson: TGLCamera;
    dcMushroom: TGLDummyCube;
    GLSceneViewer1: TGLSceneViewer;
    Actor1: TGLActor;
    Actor2: TGLActor;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    Timer1: TTimer;
    camFirstPerson: TGLCamera;
    Label3: TLabel;
    Label4: TLabel;
    DummyCube2: TGLDummyCube;
    ffMushroom: TGLFreeForm;
    GLLightSource2: TGLLightSource;
    dcCamera3rd: TGLDummyCube;
    Label1: TLabel;
    GLNavigator1: TGLNavigator;
    GLUserInterface1: TGLUserInterface;
    GLTerrainRenderer1: TGLTerrainRenderer;
    dcTerrain: TGLDummyCube;
    GLMaterialLibrary1: TGLMaterialLibrary;

    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure HandleKeys(const deltaTime: Double);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormActivate(Sender: TObject);
  private
    hdsDunes: TGLFractalHDS;
    procedure AddMushrooms;
  public
  end;

var
  FDuneFighter: TFDuneFighter;

implementation

{$R *.lfm}

const
  cWalkStep = 6; // this is our walking speed, in 3D units / second
  cStrafeStep = 6; // this is our strafing speed, in 3D units / second
  cRotAngle = 60; // this is our turning speed, in degrees / second
  cRunBoost = 2; // speed boost when running
  cSpread = 200;
  cDepthOfView = 200;
  cNbMushrooms = 25;

var
  FirstActivate: boolean = True;

procedure TfDuneFighter.FormCreate(Sender: TObject);
begin
  { Create terrain }
  GLTerrainRenderer1.Up.SetVector(0, 0, 1);
  GLTerrainRenderer1.Direction.SetVector(0, 1, 0);
//  with GLMaterialLibrary1.AddTextureMaterial('Dune','057terresable-Clair.jpg') do begin
  with GLMaterialLibrary1.AddTextureMaterial('Dune', 'nature073-Terre+Herbe.jpg') do begin
    Material.FrontProperties.Emission.Color := clrWhite;
    // TextureScale.SetVector(3,3,3);
    // GLTerrainRenderer1.Material.MaterialLibrary.LibMaterialByName('Dune');
  end;
  hdsDunes := TGLFractalHDS.Create(Self);
  with hdsDunes do
  begin
    TerrainRenderer := GLTerrainRenderer1;
    MaterialName := 'Dune';
    TextureScale := 4;
    LandCover := False;
    Sea := True;
    Amplitude := 30;
    SeaLevel := -Amplitude * 0.5;
    Depth := 7;
    Cyclic := True;
    Shadows := True;
    Lighting := True;
    LightDirection := VectorMake(1, 0.5, 1);
    KeepNormals := True;
    BuildLandscape;
  end; // with

  // Load mushroom mesh
  ffMushroom.LoadFromFile('mushroom.3ds');

  // Load Actor into GLScene
  Actor1.LoadFromFile('waste.md2');
  Actor1.Material.Texture.Image.LoadFromFile('waste.jpg');
  Actor1.Animations.LoadFromFile('Quake2Animations.aaf');
  Actor1.Scale.SetVector(0.04, 0.04, 0.04, 0);
  // Load weapon model and texture
  Actor2.LoadFromFile('WeaponWaste.md2');
  Actor2.Material.Texture.Image.LoadFromFile('WeaponWaste.jpg');
  Actor2.Animations.Assign(Actor1.Animations);

  // Define animation properties
  Actor1.AnimationMode := aamLoop;
  Actor1.SwitchToAnimation('stand');
  Actor1.FrameInterpolation := afpLinear;
  Actor2.Synchronize(Actor1);

  { View parameters }
  camThirdPerson.DepthOfView := cDepthOfView;
  camFirstPerson.DepthOfView := cDepthOfView;
  GLSceneViewer1.Buffer.FogEnvironment.FogEnd := cDepthOfView;
  GLSceneViewer1.Buffer.FogEnvironment.FogStart := cDepthOfView / 2;
  GLTerrainRenderer1.QualityDistance := cDepthOfView;

  { Set 3rd person camera }
  GLSceneViewer1.Camera := camThirdPerson;
  Actor1.Visible := True;
  Label4.Font.Style := Label4.Font.Style - [fsBold];
  Label3.Font.Style := Label3.Font.Style + [fsBold];

  GLCadencer1.Enabled := True;
end;

procedure TfDuneFighter.HandleKeys(const deltaTime: Double);
var
  moving: String;
  boost: Single;
  ay, cy: Single;
  v: tVector;
begin
  // This function uses asynchronous keyboard check (see Keyboard.pas)
  if IsKeyDown(VK_ESCAPE) then
    Close;

  // Change Cameras
  if IsKeyDown(VK_F7) then
  begin
    GLSceneViewer1.Camera := camThirdPerson;
    Actor1.Visible := True;
    Label4.Font.Style := Label4.Font.Style - [fsBold];
    Label3.Font.Style := Label3.Font.Style + [fsBold];
  end;
  if IsKeyDown(VK_F8) then
  begin
    GLSceneViewer1.Camera := camFirstPerson;
    Actor1.Visible := False;
    Label4.Font.Style := Label4.Font.Style + [fsBold];
    Label3.Font.Style := Label3.Font.Style - [fsBold];
  end;

  // Move Actor in the scene

  // if nothing specified, we are standing
  moving := 'stand';

  // first, are we running ? if yes give animation & speed a boost
  if IsKeyDown(VK_SHIFT) then
  begin
    Actor1.Interval := 100;
    boost := cRunBoost * deltaTime
  end
  else
  begin
    Actor1.Interval := 150;
    boost := deltaTime;
  end;
  Actor2.Interval := Actor1.Interval;

  // are we advaning/backpedaling ?
  if IsKeyDown(VK_UP) then
  begin
    GLNavigator1.MoveForward(cWalkStep * boost);
    moving := 'run';
  end;
  if IsKeyDown(VK_DOWN) then
  begin
    GLNavigator1.MoveForward(-cWalkStep * boost);
    moving := 'run';
  end;

  // slightly more complex, depending on CTRL key, we either turn or strafe
  if IsKeyDown(VK_LEFT) then
  begin
    if IsKeyDown(VK_CONTROL) then
      GLNavigator1.StrafeHorizontal(-cStrafeStep * boost)
    else
      GLNavigator1.TurnHorizontal(-cRotAngle * boost);
    moving := 'run';
  end;
  if IsKeyDown(VK_RIGHT) then
  begin
    if IsKeyDown(VK_CONTROL) then
      GLNavigator1.StrafeHorizontal(cStrafeStep * boost)
    else
      GLNavigator1.TurnHorizontal(cRotAngle * boost);
    moving := 'run';
  end;

  { Walk on the terrain, not beneath! }
  with DummyCube2.Position do
  begin
    ay := GLTerrainRenderer1.InterpolatedHeight(VectorMake(X, Y, Z)) *
      dcTerrain.Scale.Y + 1;
    Y := ay;
  end; // with
  { Prevent the 3rd person camera from going under the ground }
  with camThirdPerson do
  begin
    cy := GLTerrainRenderer1.InterpolatedHeight(AbsolutePosition);
    cy := MaxFloat(ay + 1, cy + 2);
    v := AbsolutePosition;
    v.Y := cy;
    AbsolutePosition := v;
  end; // with
  // camFirstPerson.Up.AsVector:=hdsDunes.Normal(camFirstPerson.AbsolutePosition);

  // update animation (if required)
  // you can use faster methods (such as storing the last value of "moving")
  // but this ones shows off the brand new "CurrentAnimation" function :)
  if Actor1.CurrentAnimation <> moving then
  begin
    Actor1.SwitchToAnimation(moving);
    Actor2.Synchronize(Actor1);
  end;
end;

procedure TfDuneFighter.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  HandleKeys(deltaTime);
  GLUserInterface1.Mouselook;

  GLSceneViewer1.Invalidate;
  GLUserInterface1.MouseUpdate;
end;

// add a few mushrooms to make the "landscape"

procedure TfDuneFighter.AddMushrooms;
var
  i: Integer;
  proxy: TGLProxyObject;
  s: tVector;
  f: Single;
  X, Y, Z: Single;
begin
  // spawn some more mushrooms using proxy objects
  for i := 0 to cNbMushrooms - 1 do
  begin
    // create a new proxy and set its MasterObject property
    proxy := TGLProxyObject(dcMushroom.AddNewChild(TGLProxyObject));
    with proxy do
    begin
      ProxyOptions := [pooObjects];
      MasterObject := ffMushroom;
      // retrieve reference attitude
      Direction := ffMushroom.Direction;
      Up := ffMushroom.Up;
      // randomize scale
      s := ffMushroom.Scale.AsVector;
      f := (1 * Random + 1);
      ScaleVector(s, f);
      Scale.AsVector := s;
      // randomize position
      X := Random(cSpread) - (cSpread / 2);
      Z := Random(cSpread) - (cSpread / 2);
      Y := GLTerrainRenderer1.InterpolatedHeight(VectorMake(X, Y, Z)) + 1.1;
      AbsolutePosition := VectorMake(X, Y, Z);
      // randomize orientation
      RollAngle := Random(360);
      TransformationChanged;
    end;
  end;
end;

procedure TfDuneFighter.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.2f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TfDuneFighter.FormActivate(Sender: TObject);
begin
  { Duplicate our reference mushroom (but not its mesh data !)
    We cannot do this in the OnCreate event as the tTerrainRenderer must have been
    built (we need the height to place the mushrooms) }
  if FirstActivate then
  begin
    GLSceneViewer1.Update;
    AddMushrooms;
    FirstActivate := False;
  end; // if
end;

end.
