unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLLCLViewer, GLTexture,
  GLGeomObjects, GLCrossPlatform, GLMaterial, GLCoordinates, GLBaseClasses;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera: TGLCamera;
    DCCamera: TGLDummyCube;
    PLGround: TGLPlane;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary: TGLMaterialLibrary;
    GLCube1: TGLCube;
    DCPositionInvariant: TGLDummyCube;
    GLCylinder1: TGLCylinder;
    GLArrowLine1: TGLArrowLine;
    DCOrientationInvariant: TGLDummyCube;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses GLUtils;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  GLMaterialLibrary.TextureByName('walkway').Image.LoadFromFile('walkway.jpg');
  GLMaterialLibrary.TextureByName('rawwall').Image.LoadFromFile('rawwall.jpg');
  GLMaterialLibrary.TextureByName('marbletiles').Image.LoadFromFile('marbletiles.jpg');
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then
      GLCamera.MoveAroundTarget(my-y, mx-x);
   if ssRight in Shift then
      GLCamera.MoveTargetInEyeSpace((y-my)*0.05, (mx-x)*0.05, 0);
   mx:=x; my:=y;
end;

end.
