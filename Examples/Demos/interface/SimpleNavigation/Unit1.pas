// a Demo with absolutely no code :)
unit Unit1;

interface

uses
  // VCL
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  // GLScene
  GLScene, GLObjects, GLLCLViewer, GLCadencer, GLSimpleNavigation,
  GLCrossPlatform, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLCadencer1: TGLCadencer;
    GLCube1: TGLCube;
    GLLightSource1: TGLLightSource;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

end.