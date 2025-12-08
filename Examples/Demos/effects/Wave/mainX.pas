unit mainX;



interface

uses

  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLGraph, GLObjects, GLAsyncTimer, GLLCLViewer,
  GLVectorGeometry,
  GLTexture, GLVectorTypes, GLCadencer, StdCtrls, ExtCtrls,
  GLCoordinates, GLCrossPlatform, GLBaseClasses;

type
  TForm4 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLAsyncTimer1: TGLAsyncTimer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLHeightField1: TGLHeightField;
    GLCube1: TGLCube;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure GLHeightField1GetHeight(const x, y: Single; var z: Single;
      var color: TVector4f; var texPoint: TTexPoint);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    t, w, v, pi, frq, gz, aX: Single;
  end;

var
  Form4: TForm4;

implementation


  {$R *.lfm}

uses math;

procedure TForm4.FormCreate(Sender: TObject);
begin
  GLHeightField1.Material.Texture.Image.LoadFromFile
    (ExtractFilePath(Application.ExeName) + 'OPOCEAN2.JPG');
  GLHeightField1.StructureChanged;
  t := 0.0;
  v := 1.9;
  pi := 3.14;
  frq := 15;
  w := 2 * pi * v;
end;

procedure TForm4.GLHeightField1GetHeight(const x, y: Single; var z: Single;
  var color: TVector4f; var texPoint: TTexPoint);
begin
  z := VectorNorm(x, y);
  z := sin((w * t + y) - (z / frq)) + x / 2;
  gz := z;
  aX := x / 2;
end;

procedure TForm4.AsyncTimer1Timer(Sender: TObject);
begin
  t := t + 0.005;
  GLHeightField1.StructureChanged;
  GLCube1.Position.y := sin(w * t);
  GLCube1.PitchAngle := RadToDeg(-gz);
  GLCube1.TurnAngle := -aX * w;
  GLCube1.RollAngle := -aX * w;
end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  GLAsyncTimer1.Enabled := False;
  Application.ProcessMessages;
  Close;
end;

end.
