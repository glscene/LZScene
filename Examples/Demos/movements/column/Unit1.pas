{: This form showcases runtime object creation and framerate independant motion.<p>

  We start with an almost empty scene. The dummy cube is used as a convenient
  way to orient the camera (using its TargetObject property). Planes are
  programmatically added to the scene in FormCreate and spinned in the
  GLCadencer1Progress event.<p>

  Framerate independance motion is obtained by using a clock reference (in
  this sample, it is given by the TGLCadencer, which uses the high performance
   precision counter as reference). You can check it by resizing the window :
   whatever the framerate, the spin speed is the same.<br>
  In this sample, it is extremely simply done, but with more complex scenes
  and movements the same rule applies : for framerate independant motion, you
  need a clock measurement.<p>

   Using the TGLCadencer is the standard way for animating under GLScene, it
   offers various option and can drive animation automatically or just act as
   a manual trigger. Basic use just involves dropping a cadencer and adjusting
   its "Scene" property.<p>

  Note that measured framerates are 1 sec averages, a TTimer is used to refresh
   and reset FPS counter.
}
unit Unit1;

interface

{$MODE Delphi}

uses
  Forms, GLScene, GLObjects, GLTexture, Classes, Controls,
  ExtCtrls, StdCtrls, GLCadencer, GLLCLViewer, GLColor, GLCrossPlatform,
  GLCoordinates;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    StaticText1: TStaticText;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses GLVectorGeometry, SysUtils;

const
  cNbPlanes = 30;
  cStackHeight = 8;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  plane: TGLPlane;
begin
  // our column is just a stack of planes
  for i := 0 to cNbPlanes - 1 do
  begin
    // create planes as child of the dummycube
    plane := TGLPlane(DummyCube1.AddNewChild(TGLPlane));
    // default plane size is 1x1, we want bigger planes !
    plane.Width := 2;
    plane.Height := 2;
    // orient and position then planes in the stack
    plane.Position.Y := cStackHeight * (0.5 - i / cNbPlanes);
    plane.Direction.AsVector := YHmgVector;
    // we use the emission color, since there is no light in the scene
    // (allows 50+ FPS with software opengl on <400 Mhz CPUs ;)
    plane.Material.FrontProperties.Emission.Color :=
      VectorLerp(clrBlue, clrYellow, i / cNbPlanes);
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
var
  i: integer;
begin
  // for all planes (all childs of the dummycube)
  for i := 0 to DummyCube1.Count - 1 do
    // roll them accordingly to our time reference and position in the stack
    (DummyCube1.Children[i] as TGLPlane).RollAngle := 90 * cos(newTime + i * PI / cNbPlanes);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // update FPS and reset counter for the next second
  StaticText1.Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.

