{: A Demo of the TGLMotionBlur component

  Version history
    02/03/07 - DaStr - Updated GLSimpleNavigation component
    25/02/07 - DaStr - Initial version (based on demo by Dave Gravel)

}
unit uMainForm;

interface

uses
  Forms, SysUtils,classes, GLVectorGeometry, StdCtrls,
  GLObjects, GLScene, GLBlur, GLSimpleNavigation, GLTexture, GLCadencer,
  GLTeapot, GLPolyhedron, GLGeomObjects, Controls, GLLCLViewer,
  GLCrossPlatform, GLMaterial, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    Cam: TGLCamera;
    Light: TGLLightSource;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCube1: TGLCube;
    GLSphere1: TGLSphere;
    GLTorus1: TGLTorus;
    GLIcosahedron1: TGLIcosahedron;
    GLTeapot1: TGLTeapot;
    GLCube2: TGLCube;
    GLCube3: TGLCube;
    GLMotionBlur1: TGLMotionBlur;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Private declarations }
  public
    { Public declarations }
    MotionBlur: TGLMotionBlur;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLCube1.RollAngle:= GLCube1.RollAngle - 175*deltaTime;
  GLCube1.TurnAngle:= GLCube1.TurnAngle + 175*deltaTime;
  GLTorus1.RollAngle:= GLTorus1.RollAngle - 5*deltaTime;
  GLTorus1.TurnAngle:= GLTorus1.TurnAngle + 5*deltaTime;
  GLIcosahedron1.RollAngle:= GLIcosahedron1.RollAngle - 50*deltaTime;
  GLIcosahedron1.TurnAngle:= GLIcosahedron1.TurnAngle + 50*deltaTime;
  GLCube1.Position.X:=Sin(newTime+2)*8-1;
  GLCube1.Position.Y:=Cos(newTime+2)*2;
  GLCube1.Position.Z:=Cos(newTime+2)*3;
  GLCube2.Position.X:=Sin(newTime+2)*8-1;
  GLSceneViewer1.Invalidate;
end;

end.
