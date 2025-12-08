{: A variation of the magacube bench for testing sorting.<p>

  The cubes in this sample are transparent, and as such need to be depth-sorted
   to render correctly, this uses an alternate sorting code which this benchmark
   helps testing. The sorting mode is osRenderBlended last, which isn't the most
   efficient for that particular case (as a matter of fact, this bench is a worst
   case situation for osRenderBlendedLast), osRenderFarthestFirst would be more
   suited since all the objets must be sorted (osRenderBlendedLast attempts
   ta take advantange of the fact that only a fraction of the objects must be
   depth-sorted, which is a disadvantage if most of them must be sorted).<p>

  Results :

  Size   Triangles     FPS      CPU      OpenGL     ColorDepth

    5      15972      90.0     K7-1145    GF2 Pro       32Bits (vs 139.3 for megacube)
    5      15972      27.6     Du-800     TNT2 M64      32Bits
   --- 26/01/02 --- Sorting optims, XOpenGL change
    5      15972      68.2     K7-1145    GF2 Pro       32Bits (vs 110.4 for megacube)
   --- 26/01/02 --- Introduced bench
}
unit Unit1;

{.$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, GLScene, GLObjects,
  GLVectorGeometry, ExtCtrls, GLTexture, GLCadencer, GLColor,
  GLCrossPlatform, GLCoordinates, GLMaterial, GLLCLViewer, GLAsyncTimer;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLCadencer1: TGLCadencer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GLAsyncTimer1Timer(Sender: TObject);

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

const
  cSize = 5;

procedure TForm1.FormCreate(Sender: TObject);
var
  x, y, z: integer;
  cube: TGLCube;
  factor, cubeSize: single;
begin
  // bench only creation and 1st render (with lists builds, etc...)
  factor := 70 / (cSize * 2 + 1);
  cubeSize := 0.4 * factor;
  for x := -cSize to cSize do
    for y := -cSize to cSize do
      for z := -cSize to cSize do
      begin
        cube := TGLCube(DummyCube1.AddNewChild(TGLCube));
        cube.Position.AsVector := PointMake(factor * x, factor * y, factor * z);
        cube.CubeWidth := cubeSize;
        cube.CubeHeight := cubeSize;
        cube.CubeDepth := cubeSize;
        cube.Material.BlendingMode := bmTransparency;
        with cube.Material.FrontProperties do
        begin
          Diffuse.Color := VectorLerp(clrBlue, clrWhite, (x * x + y * y + z * z) / (cSize * cSize * 3));
          Diffuse.Alpha := 0.5;
        end;
      end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  GLCadencer1.Enabled:=true;
  //Timer1.Enabled:=true;

end;

procedure TForm1.GLAsyncTimer1Timer(Sender: TObject);
begin
   Self.Caption := Format('%.2f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  DummyCube1.TurnAngle := 90 * newTime; // 90° per second
  //GLSceneViewer1.Invalidate;
  //  Caption := Format('%.2f FPS', [GLSceneViewer1.FramesPerSecond]);
 // GLSceneViewer1.ResetPerformanceMonitor;
end;



end.

