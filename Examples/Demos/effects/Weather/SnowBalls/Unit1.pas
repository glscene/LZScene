{:  Based on 'Particles' demo from glscene <p>
    This demo will show snow effect;
     Michail Sychev AKA Riz (riz@eternalmind.ru)
}
unit Unit1;

{$MODE Delphi}

interface

uses
  Forms, GLScene, GLObjects, GLParticles, GLCadencer, ExtCtrls,
  GLBehaviours, Classes, Controls, GLVectorGeometry, SysUtils, GLLCLViewer, tga, Dialogs,
  GLSkydome, gltexture, GLCoordinates, GLCrossPlatform,
  GLMaterial;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLParticles1: TGLParticles;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    GLDummyCube1: TGLDummyCube;
    Timer2: TTimer;
    procedure GLParticles1ActivateParticle(Sender: TObject; particle: TGLBaseSceneObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure Timer2Timer(Sender: TObject);

  private
    procedure SSpriteProgress(Sender: TObject; const deltaTime, newTime: double);
  public

  end;

type // Let's create simple class to hold data for particles movement
  TSpriteHolder = class(TObject)
  public
    amp, kof: real;
    initalPosx, initalPosz: real;
    speed: real;
  end;

var
  Form1: TForm1;
  SSprite: TGLSprite;

implementation

{$R *.lfm}


procedure TForm1.SSpriteProgress(Sender: TObject; const deltaTime, newTime: double);
var
  life: double;
  tempholder: TSpriteHolder;
begin
  with TGlSprite(Sender) do
  begin
    // calculate for how long we've been living
    life := (newTime - TagFloat);
    tempholder := TSpriteholder(TGlSprite(Sender).TagObject);

    Position.y := Position.y - (deltatime / 2) * tempholder.speed;

    position.X := tempholder.initalPosx - (tempholder.amp / 2) + (tempholder.amp / 2 * sin(life));
    position.Z := tempholder.initalPosz - (tempholder.amp / 2) + (tempholder.amp / 2 * cos(life));

    if life > 25 then
      // old particle to kill
    begin
      Form1.GLParticles1.KillParticle(TGlSprite(Sender));
    end
    // We are happy because actually we don't destroy anything(fragment memory and all bad stuff)
    // rather than reusing free particles from pool
    else
    begin
      Material.FrontProperties.Diffuse.Alpha := (24 - life) / 24;
    end;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // if we don't do this, our random won't look like random
  Randomize;
  SSprite := TGLSprite(GLParticles1.AddNewChild(TGLSprite));
  SSprite.Material.Texture.Image.LoadFromFile('Flare1.bmp');
  SSprite.Material.BlendingMode := bmAdditive;
  SSprite.Material.Texture.Disabled := False;
  SSprite.OnProgress := SSpriteProgress;
end;

procedure TForm1.GLParticles1ActivateParticle(Sender: TObject; particle: TGLBaseSceneObject);
begin
  // this event is called when a particle is activated,
  // ie. just before it will be rendered
  with Tglsprite(particle) do
  begin
    with Material.FrontProperties do
    begin
      // we pick a random color
      Emission.Color := PointMake(1, 1, 1);

      // our halo starts transparent
      Diffuse.Alpha := 1;
    end;
    // this is our "birth time"
    TagFloat := GLCadencer1.CurrentTime;
  end;
end;

var
  mx, my: integer;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  mx := x;
  my := y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget(my - y, mx - x);
  end;
  mx := x;
  my := y;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to 6 do
  begin
    with TGlSprite(GLParticles1.CreateParticle) do
    begin
      Position.X := GlDummyCube1.Position.X + (GlDummyCube1.CubeSize / 2) * random - (GlDummyCube1.CubeSize / 4);
      Position.Z := GlDummyCube1.Position.Z + (GlDummyCube1.CubeSize / 2) * random - (GlDummyCube1.CubeSize / 4);

      // Snow should fall from the top of the cube
      Position.Y := GlDummyCube1.Position.Y + (GlDummyCube1.CubeSize / 4);
      ;

      Width := random * 0.2;
      Height := Width;

      //   We need to store some additional info
      TagObject := TSpriteholder.Create;
      (TagObject as TSpriteholder).amp := random;
      (TagObject as TSpriteholder).kof := random;
      (TagObject as TSpriteholder).initalposx := Position.X;
      (TagObject as TSpriteholder).initalposz := Position.z;
      (TagObject as TSpriteholder).speed := random;
    end;

  end;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin

  // infos for the user
  Caption := Format('%d particles, %.1f FPS', [GLParticles1.Count - 1, GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  // change focal so the view will shrink and not just get clipped
  GLCamera1.FocalLength := 50 * Width / 280;
end;


procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  GLCamera1.SceneScale := GLCamera1.SceneScale * (1000 - WheelDelta) / 1000;
  Handled := True;
end;

end.

