{: This sample is basicly a bench for GLScene.<p>

  A fair number of TGLCube are created and rendered (for cSize=5, that's about
  1331 cubes, 7986 polygons or 15972 triangles). Only one light and basic
   shading is used, no texture here, the aim is to test the performance of
   GLScene and T&L, not  the fillrate.<p>

  Results :

  Size   Triangles     FPS      CPU      OpenGL     ColorDepth


    5      15972     174.9     K7-2200+   GF3 Ti200     32Bits   Amalgamate = False
   --- 07/01/04 --- 2004 bench
    5      15972     200.5     K7-1800+   GF2 Pro       32Bits   Amalgamate = True
    5      15972     158.6     K7-1800+   GF2 Pro       32Bits
   --- 29/11/02 --- Long time no bench, added 'Amalgamate' score
    5      15972     139.3     K7-1145    GF2 Pro       32Bits
    5      15972      33.6     Du-800     TNT2 M64      32Bits
   --- 26/01/02 --- XOpenGL flexes its muscles, more memory optimizations
    5      15972     110.4     K7-1145    GF2 Pro       32Bits
    5      15972      30.1     Du-800     TNT2 M64      32Bits
   --- 23/01/02 --- Several structural optimizations (memory use & access)
    5      15972      92.2     K7-1145    GF2 Pro       32Bits
   --- 20/01/02 --- Long time no bench, fair improvement though :)
    5      15972      80.1     K7-1066    GF2 Pro       32Bits
   --- 04/09/01 --- Internal changes (buffers, contexts...)
    5      15972      64.1     K7-1066    GF2 Pro       32Bits
   --- 11/08/01 --- GLScene v0.8.3wip
    5      15972      20.1     Du-800     V3 2000       16Bits
   --- 12/03/01 --- GLScene v0.8.2
    5      15972      36.4     K7-500   GeForce-6.50    32Bits
   --- 05/02/01 --- Cube build list and SetGLMaterialColors boost
    5      15972      25.8     K7-500   GeForce-5.33    32Bits
   --- 13/01/01 --- Long time no bench
    5      15972       7.7     K6-400     V3 2000       16Bits
   --- 20/06/00 --- Geometry.pas optimizations
    5      15972      17.2     K7-500     V3 2000       16Bits
   --- 24/03/00 --- PolygonMode & Color cacheing optimization
    5      15972      15.5     K7-500     V3 2000       16Bits
    5      15972       5.9     K6-400     V3 3000       16Bits
    5      15972       4.0     K6-400      MS NT4       24Bits
   --- 22/03/00 --- Set/UnSet states optimization
    5      15972       5.5     K6-400     V3 3000       16Bits
    5      15972       3.6     K6-400      MS NT4       24Bits
   --- 22/03/00 --- Created the bench
}
unit Unit1;

{.$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,Dialogs,
  GLScene, GLObjects, GLVectorGeometry,  GLTexture,
  GLCadencer, GLCrossPlatform, GLColor, GLLCLViewer, GLCoordinates;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
  private
    { Déclarations privées }
    UpdateFPS:Boolean;
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
        with cube.Material.FrontProperties do
        begin
          Diffuse.Color := VectorLerp(clrYellow, clrRed, (x * x + y * y + z * z) / (cSize * cSize * 3));

          // uncomment following lines to stress OpenGL with more color changes calls

         //          Ambient.Color:=VectorLerp(clrYellow, clrRed, (x*x+y*y+z*z)/(cSize*cSize*3));
         //          Emission.Color:=VectorLerp(clrYellow, clrRed, (x*x+y*y+z*z)/(cSize*cSize*3));
         //          Specular.Color:=VectorLerp(clrYellow, clrRed, (x*x+y*y+z*z)/(cSize*cSize*3));
        end;
      end;
  UpdateFPS:=false;
 // GLCadencer1.Enabled:=true;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  DummyCube1.TurnAngle := 90 * newTime; // 90° per second
  //Work here
  if UpdateFPS then
  begin
    Caption := Format('%.2f FPS', [GLSceneViewer1.FramesPerSecond]);
    GLSceneViewer1.ResetPerformanceMonitor;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  UpdateFPS := not(UpdateFPS);
end;

end.

