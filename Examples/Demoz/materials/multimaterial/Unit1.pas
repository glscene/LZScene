{: Basic GLMultiMaterialShader example.<p>

   The GLMultiMaterialShader applies a pass for each material in
   the assigned MaterialLibrary. This example shows how to apply
   three blended textures to an object.

   A fourth texture is used in the specular pass to map the area
   affected by the chrome-like shine.
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLLCLViewer, GLTexture,
  GLCadencer, GLMultiMaterialShader, GLTexCombineShader, GLMaterial,
  GLCoordinates, GLCrossPlatform, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLCube1: TGLCube;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary2: TGLMaterialLibrary;
    GLMultiMaterialShader1: TGLMultiMaterialShader;
    GLCadencer1: TGLCadencer;
    GLTexCombineShader1: TGLTexCombineShader;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my: integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

 uses
   GLUtils;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();

  with GLMaterialLibrary1 do
  begin
    // Add the specular pass
    with AddTextureMaterial('specular', MediaPath+'GLScene_alpha.bmp') do
    begin
      // tmBlend for shiny background
      //Material.Texture.TextureMode:=tmBlend;
      // tmModulate for shiny text
      Material.Texture.TextureMode := tmModulate;
      Material.BlendingMode := bmAdditive;
      Texture2Name := 'specular_tex2';
    end;
    with AddTextureMaterial('specular_tex2',MediaPath+ 'chrome_buckle.bmp') do
    begin
      Material.Texture.MappingMode := tmmCubeMapReflection;
      Material.Texture.ImageBrightness := 0.3;
    end;

  end;

  // GLMaterialLibrary2 is the source of the GLMultiMaterialShader
  // passes.
  with GLMaterialLibrary2 do
  begin
    // Pass 1 : Base texture
    AddTextureMaterial('Pass1',MediaPath+ 'GLScene.bmp');//}

    // Pass 2 : Add a bit of detail
    with AddTextureMaterial('Pass2', MediaPath+'detailmap.jpg') do
    begin
      Material.Texture.TextureMode := tmBlend;
      Material.BlendingMode := bmAdditive;
    end;//}

    // Pass 3 : And a little specular reflection
    with TGLLibMaterial.Create(GLMaterialLibrary2.Materials) do
    begin
      Material.MaterialLibrary := GLMaterialLibrary1;
      Material.LibMaterialName := 'specular';
    end;//}

    // This isn't limited to 3, try adding some more passes!
  end;

end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  mx := x;
  my := y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if ssLeft in shift then
    GLCamera1.MoveAroundTarget(my - y, mx - x);
  mx := x;
  my := y;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  GLCube1.Turn(deltaTime * 10);
end;

end.

