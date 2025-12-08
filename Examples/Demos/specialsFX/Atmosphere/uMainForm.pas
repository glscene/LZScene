{
  GLAtmosphere Demo.

  Note: object "Not_a_planet" is used to test atmosphere transparency issues.

  Version history:
    26/01/10 - Yar - Added GLColor to uses
    20/08/07 - DaStr - Now demo correctly displays FPS
    15/08/07 - LC - Added GLBehaviours to "uses" section to
                    prevent run-time error.
    03/04/07 - DaStr - Cleaned up "uses" section
    02/03/07 - DaStr - Fixed LensFlare object sorting issue
                       Updated GLSimpleNavigation component
    07/02/07 - DaStr - Initial version

}
unit uMainForm;

interface


uses
  // VCL
  Forms, StdCtrls, Classes, Controls, ExtCtrls,

  // GLScene
  GLScene, GLObjects, GLCadencer, GLLensFlare, GLLCLViewer,
  GLTexture, GLSkydome, GLVectorGeometry, GLCrossPlatform,
  GLAtmosphere, GLSimpleNavigation, GLBehaviours,

  // File formats
  GLFileJPEG, GLCoordinates, GLBaseClasses;

type
  TMainForm = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1:   TGLScene;
    GLCamera1:  TGLCamera;
    GLLensFlare1: TGLLensFlare;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    GLSphere1:  TGLSphere;
    GLDummyCube1: TGLDummyCube;
    Button1:    TButton;
    Button2:    TButton;
    Button3:    TButton;
    Button4:    TButton;
    Button5:    TButton;
    Timer1:     TTimer;
    Button6:    TButton;
    Not_a_planet: TGLSphere;
    Button8:    TButton;
    CameraTarget: TGLDummyCube;
    Button9:    TButton;
    Button10:   TButton;
    GLSkyDome1: TGLSkyDome;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    GLSimpleNavigation1: TGLSimpleNavigation;
    World: TGLDummyCube;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm:  TMainForm;
  Atmosphere:  TGLAtmosphere;

implementation

{$R *.lfm}

uses
  GLColor;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Atmosphere := TGLAtmosphere.CreateAsChild(GLDummyCube1);
  Atmosphere.Sun := glLensFlare1;
  Atmosphere.SetOptimalAtmosphere2(GLSphere1.Radius);

  GLSkyDome1.Bands.Clear;
  GLSkyDome1.Stars.AddRandomStars(5000, ConvertColorVector(clrWhite));
end;


procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Atmosphere.Free;
end;


procedure TMainForm.Button1Click(Sender: TObject);
begin
  GLSphere1.Roll(10);
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  glLensFlare1.Slide(0.8);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  glLensFlare1.Slide(-0.8);
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
  GLDummyCube1.Slide(-0.5);
end;

procedure TMainForm.Button5Click(Sender: TObject);
begin
  GLDummyCube1.Slide(0.5);
end;

procedure TMainForm.Button6Click(Sender: TObject);
begin
  Atmosphere.Visible := not Atmosphere.Visible;
end;

procedure TMainForm.Button8Click(Sender: TObject);
begin
  Atmosphere.TogleBlendingMode;
end;

procedure TMainForm.Button10Click(Sender: TObject);
begin
  GLCamera1.AdjustDistanceToTarget(1.1);
end;

procedure TMainForm.Button9Click(Sender: TObject);
begin
  GLCamera1.AdjustDistanceToTarget(1 / 1.1);
end;

procedure TMainForm.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

end.