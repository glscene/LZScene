{: Demo/test case for the PointTo method of objects.<p>

   The PointTo method allows to easily orient an object to point toward another
   object, whatever their relative positions in the scene hierarchy.<br>
   In this sample, we have a green sphere turning in circle and riding a sin,
   while a blue arrow, turning in a smaller circle, is maintained pointed
   toward the sphere. The other items (lines...) are just here to help visualize
   the 3D nature of the thing.
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLObjects, GLScene, StdCtrls, GLVectorGeometry, GLCadencer, GLLCLViewer,
  GLGeomObjects, GLCrossPlatform, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DCSphere: TGLDummyCube;
    ArrowLine: TGLArrowLine;
    GLLightSource1: TGLLightSource;
    Sphere: TGLSphere;
    DCArrow: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    Disk1: TGLDisk;
    Disk2: TGLDisk;
    Lines1: TGLLines;
    Plane1: TGLPlane;
    Lines2: TGLLines;
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

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  // Make the blue sphere turn and ride a sin
  DCSphere.Turn(deltaTime * 30);
  Sphere.Position.Y := Sin(DegToRad(newTime * 50)) * 3;

  // Make the arrow turn
  DCArrow.Turn(-deltaTime * 15);

  // Make the arrow point toward the sphere, using Y as up reference
  ArrowLine.PointTo(Sphere, YHmgVector);
end;

end.
