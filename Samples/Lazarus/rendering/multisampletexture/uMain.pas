unit uMain;

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  // GLScene Units
  GLScene, GLObjects, GLLCLViewer, GLCrossPlatform, GLCoordinates,
  GLBaseClasses, GLBitmapFont, GLWindowsFont, GLMaterial,
  GLCustomShader, GLSLShader, GLCadencer, GLHUDObjects, GLGeomObjects,
  GLFBORenderer, GLVectorGeometry, GLVectorTypes, GLTexture, GLSimpleNavigation;

type
  TGLDemoForm = class(TForm)
    MainScene: TGLScene;
    MainCamera: TGLCamera;
    MainCadencer: TGLCadencer;
    MainMaterialLibrary: TGLMaterialLibrary;
    SceneObjects: TGLDummyCube;
    MainViewer: TGLSceneViewer;
    TestLight: TGLLightSource;
    MultisampleFBO: TGLFBORenderer;
    GLSLShader1: TGLSLShader;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLScreenQuad: TGLHUDSprite;
    GLSphere1: TGLSphere;
    GLTorus1: TGLTorus;
    GLTorus2: TGLTorus;
    GLCone1: TGLCone;
    GLLines1: TGLLines;
    FBOContainer: TGLDummyCube;
    GLHUDText1: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    procedure FormCreate(Sender: TObject);
    procedure MainCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormResize(Sender: TObject);
    procedure GLSLShader1Initialize(Shader: TGLCustomGLSLShader);
    procedure GLSLShader1Apply(Shader: TGLCustomGLSLShader);
  end;

var
  GLDemoForm: TGLDemoForm;

implementation

{$R *.lfm}

uses
  GLState,
  GLKeyboard, GLMultisampleImage, GLContext, LCLType;

procedure TGLDemoForm.FormCreate(Sender: TObject);
begin
  width := Screen.width;
  height := Screen.height;
  WindowState := wsMaximized;

  with  MainMaterialLibrary.TextureByName('MultisampledColor') do
  begin
    ImageClassName := 'TGLMultisampleImage';
    TGLMultiSampleImage(Image).SamplesCount := 4;
  end;

  with  MainMaterialLibrary.TextureByName('Depth') do
  begin
    ImageClassName := 'TGLMultisampleImage';
    TGLMultiSampleImage(Image).SamplesCount := 4;
  end;

  GLSLShader1.Enabled := True;

  GLHUDText1.Text := 'Press F1/F2 to switch multisampling' + #10#13 +
                     'F3/F4 - Warframe mode';
end;

procedure TGLDemoForm.MainCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if isKeyDown(VK_F2) then
  begin
    FBOContainer.Visible := false;
    GLScreenQuad.Visible := false;
    SceneObjects.Visible := true;
  end;

  if isKeyDown(VK_F1) then
  begin
    FBOContainer.Visible := true;
    GLScreenQuad.Visible := true;
    SceneObjects.Visible := false;
  end;

  if isKeyDown(VK_F3) then
  begin
    GLTorus1.Material.PolygonMode := pmLines;
    GLTorus2.Material.PolygonMode := pmLines;
    GLCone1.Material.PolygonMode := pmLines;
  end;

  if isKeyDown(VK_F4) then
  begin
    GLTorus1.Material.PolygonMode := pmFill;
    GLTorus2.Material.PolygonMode := pmFill;
    GLCone1.Material.PolygonMode := pmFill;
  end;

  MainViewer.Invalidate;
end;

procedure TGLDemoForm.FormResize(Sender: TObject);
begin
  MultisampleFBO.Width := MainViewer.Width;
  MultisampleFBO.Height := MainViewer.Height;
  GLScreenQuad.Width  := MainViewer.Width;
  GLScreenQuad.Height := MainViewer.Height;
  GLScreenQuad.Position.SetPoint(
    MainViewer.Width div 2,
    MainViewer.Height div 2,
    0);
end;

procedure TGLDemoForm.GLSLShader1Initialize(Shader: TGLCustomGLSLShader);
begin
  if not GL.EXT_framebuffer_multisample then
  begin
    ShowMessage
      ('Sorry, your hardware do not support Multisampling');
    Close;
  end;
end;

procedure TGLDemoForm.GLSLShader1Apply(Shader: TGLCustomGLSLShader);
begin
  with Shader, MainViewer do
  begin
    Param['TexUnit0'].AsTexture[0] :=
      MainMaterialLibrary.TextureByName('MultisampledColor');
    Param['ViewerSize'].AsVector2f := Vector2fMake(Width, Height);
  end;
end;

end.
