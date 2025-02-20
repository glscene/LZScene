{:
  GLSL Bump Shader Demo<p>

  A demo that shows how to use the TGLSLBumpShader component.

  Note: All unsupported scene objects have only one thing in common -
        they cannot call the BuildTangentSpace() function.

  Version history:
    24/07/09 - DaStr - Added the "Show unsupported scene objects" checkbox
    12/07/07 - DaStr - Bugfixed MultiLight stuff. Other small bigfixes...
    03/04/07 - DaStr - Added more objects
    30/03/07 - DaStr - Initial version
}
unit umainform;

interface

uses
  // VCL
  SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls,

  // GLScene
  GLTexture, GLCadencer, GLViewer, GLScene, GLObjects, GLPolyhedron,
  GLVectorFileObjects, GLGraph, GLGeomObjects, GLVectorGeometry, GLSLBumpShader,
  GLCustomShader, GLSLShader, GLCrossPlatform, GLMaterial, GLCoordinates,

  // FileFormats
  TGA, GLFileMS3D, GLFile3DS, DDSImage, GLFileMD2, GLFileSMD;

type

  { TGLSLTestForm }

  TGLSLTestForm = class(TForm)
    Scene: TGLScene;
    Viewer: TGLSceneViewer;
    Cadencer: TGLCadencer;
    Camera: TGLCamera;
    Timer1: TTimer;
    Light: TGLLightSource;
    LightCube: TGLDummyCube;
    GLSphere1: TGLSphere;
    GLXYZGrid1: TGLXYZGrid;
    GLArrowLine1: TGLArrowLine;
    Panel1: TPanel;
    LightMovingCheckBox: TCheckBox;
    GUICube: TGLDummyCube;
    WorldCube: TGLDummyCube;
    Fighter: TGLActor;
    Teapot: TGLActor;
    Sphere_big: TGLActor;
    Sphere_little: TGLActor;
    MaterialLibrary: TGLMaterialLibrary;
    RollPitchTurnCheckBox: TCheckBox;
    ShaderEnabledCheckBox: TCheckBox;
    GLSphere2: TGLSphere;
    Light2: TGLLightSource;
    LightCube2: TGLDummyCube;
    MultiLightShaderCheckBox: TCheckBox;
    UseSpecularTextureCheckBox: TCheckBox;
    UseNormalTextureCheckBox: TCheckBox;
    MyBumpShader: TGLSLBumpShader;
    TrinityMatlib: TGLMaterialLibrary;
    GLCube: TGLCube;
    GLDodecahedron: TGLDodecahedron;
    GLSphere: TGLSphere;
    ShowUnsupportedSceneObjectsCheckBox: TCheckBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure CadencerProgress(Sender: TObject; const DeltaTime, newTime: Double);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure LightCubeProgress(Sender: TObject; const DeltaTime, newTime: Double);
    procedure ShaderEnabledCheckBoxClick(Sender: TObject);
    procedure MultiLightShaderCheckBoxClick(Sender: TObject);
    procedure UseSpecularTextureCheckBoxClick(Sender: TObject);
    procedure UseNormalTextureCheckBoxClick(Sender: TObject);
    procedure ShowUnsupportedSceneObjectsCheckBoxClick(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
  end;

var
  GLSLTestForm: TGLSLTestForm;
  mx, my:    Integer;
  MultiLightShader: TGLSLMLBumpShader;

implementation

{$R *.lfm}

uses
  GLUtils, GLCompositeImage, GLFileDDS;

procedure TGLSLTestForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
   SetGLSceneMediaDir();
  //First load models
  Fighter.LoadFromFile(MediaPath+'\'+'TRINITYrage.smd'); //Fighter
  Fighter.AddDataFromFile(MediaPath+'\'+'walk.smd');
  Fighter.Animations[1].MakeSkeletalTranslationStatic;
  Fighter.AddDataFromFile(MediaPath+'\'+'run.smd');
  Fighter.Animations[2].MakeSkeletalTranslationStatic;
  Fighter.AddDataFromFile(MediaPath+'\'+'long_jump.smd');
  Fighter.AddDataFromFile(MediaPath+'\'+'jump.smd');
  Fighter.AddDataFromFile(MediaPath+'\'+'look_left_right.smd');
  Fighter.Animations[5].MakeSkeletalRotationDelta;
  Fighter.SwitchToAnimation(1);
{
  Fighter.LoadFromFile(FILE_PATH + 'waste.md2'); //Fighter
  Fighter.SwitchToAnimation(0, True);
  Fighter.AnimationMode := aamLoop;
  Fighter.Scale.Scale(3);
}
  Fighter.AnimationMode := aamLoop;
  Fighter.Scale.Scale(3);
//  Fighter.MeshObjects.BuildTangentSpace;

  Teapot.LoadFromFile(MediaPath+'\'+'teapot.3ds'); //Teapot
  Teapot.Scale.Scale(0.8);
  //  Teapot.MeshObjects.BuildTangentSpace; does not have texture coordinates...

  Sphere_big.LoadFromFile(MediaPath+'\'+'Sphere_big.3DS'); //Sphere_big
  Sphere_big.Scale.Scale(70);
  Sphere_big.MeshObjects.BuildTangentSpace;

  Sphere_little.LoadFromFile(MediaPath+'\'+'Sphere_little.3DS'); //Sphere_little
  Sphere_little.Scale.Scale(4);
  Sphere_little.MeshObjects.BuildTangentSpace;

  // Then load textures
  MaterialLibrary.LibMaterialByName('Earth').Material.Texture.Image.LoadFromFile(MediaPath+'\'+'Earth.jpg');
  with MaterialLibrary.LibMaterialByName('EarthGross').Material.Texture do
  begin
    ImageClassName:= TGLCompositeImage.ClassName;
    Image.LoadFromFile(MediaPath+'\'+'EarthSpec.dds');
  end;
  MaterialLibrary.LibMaterialByName('EarthNormals').Material.Texture.Image.LoadFromFile(MediaPath+'\'+'EarthNormals.jpg');

  // Create Shader
  MultiLightShader := TGLSLMLBumpShader.Create(Self);
  MultiLightShader.LightSources := [1, 2];
  MultiLightShader.LightCompensation := 0.7;
  MultiLightShader.NormalTexture := MaterialLibrary.LibMaterialByName('EarthNormals').Material.Texture;
  MultiLightShader.SpecularTexture := MaterialLibrary.LibMaterialByName('EarthGross').Material.Texture;

  // Attach shader to the material
  MaterialLibrary.LibMaterialByName('Earth').Shader := MyBumpShader;
  for I := 0 to TrinityMatlib.Materials.Count - 1 do
    TrinityMatlib.Materials[I].Shader := MyBumpShader;
end;

procedure TGLSLTestForm.CadencerProgress(Sender: TObject; const DeltaTime, newTime: Double);
begin
  Viewer.Invalidate;

  if RollPitchTurnCheckBox.Checked then
  begin
    Sphere_big.Turn(DeltaTime * 40);
    Sphere_big.Roll(DeltaTime * 40);
    Sphere_little.Pitch(DeltaTime * 20);
    Fighter.Roll(DeltaTime * 20);
    Teapot.Roll(-DeltaTime * 10);
    GLCube.Pitch(-DeltaTime * 10);
    GLDodecahedron.Pitch(DeltaTime * 10);
    GLSphere.Roll(-DeltaTime * 10);
  end;
end;


procedure TGLSLTestForm.ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;


procedure TGLSLTestForm.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (ssRight in Shift) and (ssLeft in Shift) then
    Camera.AdjustDistanceToTarget(Power(1.01, Y - my))
  else
  if (ssRight in Shift) or (ssLeft in Shift) then
    Camera.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;


procedure TGLSLTestForm.Timer1Timer(Sender: TObject);
begin
  Caption := 'GLSL Bump Shader Demo  -  ' + Viewer.FramesPerSecondText;
  Viewer.ResetPerformanceMonitor;
end;


procedure TGLSLTestForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Cadencer.Enabled := False;
end;

procedure TGLSLTestForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Camera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TGLSLTestForm.LightCubeProgress(Sender: TObject; const DeltaTime, newTime: Double);
begin
  if LightMovingCheckBox.Checked then
    LightCube.MoveObjectAround(Camera.TargetObject, sin(NewTime) * DeltaTime * 10, DeltaTime * 20);
end;


procedure TGLSLTestForm.ShaderEnabledCheckBoxClick(Sender: TObject);
begin
  MyBumpShader.Enabled := ShaderEnabledCheckBox.Checked;
  MultiLightShader.Enabled := ShaderEnabledCheckBox.Checked;
end;

procedure TGLSLTestForm.MultiLightShaderCheckBoxClick(Sender: TObject);
var
  I: Integer;
begin
  if MultiLightShaderCheckBox.Checked then
  begin
    MaterialLibrary.LibMaterialByName('Earth').Shader := MultiLightShader;
    for I := 0 to TrinityMatlib.Materials.Count - 1 do
      TrinityMatlib.Materials[I].Shader := MultiLightShader;
  end
  else
  begin
    MaterialLibrary.LibMaterialByName('Earth').Shader := MyBumpShader;
    for I := 0 to TrinityMatlib.Materials.Count - 1 do
      TrinityMatlib.Materials[I].Shader := MyBumpShader;
  end;

  Light2.Shining := MultiLightShaderCheckBox.Checked;
  LightCube2.Visible := MultiLightShaderCheckBox.Checked;
end;

procedure TGLSLTestForm.UseSpecularTextureCheckBoxClick(Sender: TObject);
begin
  if UseSpecularTextureCheckBox.Checked then
  begin
    MyBumpShader.SpecularTexture := MaterialLibrary.LibMaterialByName('EarthGross').Material.Texture;
    MultiLightShader.SpecularTexture := MaterialLibrary.LibMaterialByName('EarthGross').Material.Texture;
  end
  else
  begin
    MyBumpShader.SpecularTexture := nil;
    MultiLightShader.SpecularTexture := nil;
  end;
end;

procedure TGLSLTestForm.UseNormalTextureCheckBoxClick(Sender: TObject);
begin
  if UseNormalTextureCheckBox.Checked then
  begin
    MyBumpShader.NormalTexture := MaterialLibrary.LibMaterialByName('EarthNormals').Material.Texture;
    MultiLightShader.NormalTexture := MaterialLibrary.LibMaterialByName('EarthNormals').Material.Texture;
  end
  else
  begin
    MyBumpShader.NormalTexture := nil;
    MultiLightShader.NormalTexture := nil;
  end;
end;

procedure TGLSLTestForm.ShowUnsupportedSceneObjectsCheckBoxClick(
  Sender: TObject);
begin
  Teapot.Visible := ShowUnsupportedSceneObjectsCheckBox.Checked;
  Fighter.Visible := ShowUnsupportedSceneObjectsCheckBox.Checked;
  GLCube.Visible := ShowUnsupportedSceneObjectsCheckBox.Checked;
  GLDodecahedron.Visible := ShowUnsupportedSceneObjectsCheckBox.Checked;
  GLSphere.Visible := ShowUnsupportedSceneObjectsCheckBox.Checked;
end;

end.

