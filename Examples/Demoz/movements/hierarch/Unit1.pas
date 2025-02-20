{: This Form demonstrates basic "hierarchical" movements.<p>

  Our visible (cube) objects are children of TGLDummyCube object and we move
  them through rotations of the DummyCubes.<br>
  Movements in this demo differs from the "manual" demo : "earth" now spins,
  "moon" has an inclined orbit and spins too.<p>

  You may use any GLScene object when building hierarchical scenes, but it
  is recommended to use TGLDummyCube for structural (regroupment only) objects,
  since dummycube are just "virtual" at run-time and cost no OpenGL setup or
  rendering time.
}
unit Unit1;

interface

uses
  Forms, GLScene, GLObjects, ComCtrls, ExtCtrls, StdCtrls,
  GLAsyncTimer, Classes, Controls, GLCadencer, GLLCLViewer, GLCrossPlatform,
  GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    TrackBar: TTrackBar;
    Cube1: TGLCube;
    Cube3: TGLCube;
    Cube2: TGLCube;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    CBPlay: TCheckBox;
    DummyCube1: TGLDummyCube;
    DummyCube2: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    procedure TrackBarChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
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

uses SysUtils;

procedure TForm1.TrackBarChange(Sender: TObject);
var
  t: integer;
begin
  t := TrackBar.Position;
  // the "sun" spins slowly
  Cube1.TurnAngle := t / 4;
  // "earth" rotates around the sun and spins
  DummyCube1.TurnAngle := -t;
  Cube2.TurnAngle := t * 2;
  // "moon" rotates around earth and spins
  DummyCube2.RollAngle := 3 * t;
  Cube3.TurnAngle := 4 * t;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  if CBPlay.Checked and Visible then
  begin
    // simulate a user action on the trackbar...
    TrackBar.Position := ((TrackBar.Position + 1) mod 360);
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  // We need to stop playing here :
  //   since the timer is asynchronous, if we don't stop play,
  //   it may get triggered during the form's destruction
  CBPlay.Checked := False;
end;

end.
