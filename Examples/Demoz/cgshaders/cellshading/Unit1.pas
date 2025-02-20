{: Simple cell shading with CgShaders.<p>

   This demo uses a vertex program to combine the normal and
   light vector to produce a light intensity which is passed
   to the fragment program as the 3rd element in the texture
   coordinate stream. This intensity is then clamped to specific
   values based on the range the intensity falls into. This is
   how the cells are created. You can add or remove cells by
   adding and removing ranges from the intensity clamping in
   the fragment program. This intensity is multiplied to the
   color value for each fragment retrieved from the texture
   map. Using solid colors on the texture gives nice results
   once cell shaded.<p>
   
   While this demo only shows parallel lighting, you could use
   point lights quite easily by modifying the uniform
   parameters passed to the vertex program and the processing
   of the intensity. Multiple lights wouldn't be difficult
   to implement either.<p>
}
unit Unit1;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLCadencer, GLTexture, GLCgShader,
  GLLCLViewer, CgGL, GLVectorFileObjects, GLAsyncTimer,
  GLCrossPlatform, GLMaterial, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    CgCellShader: TCgShader;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLActor1: TGLActor;
    AsyncTimer1: TGLAsyncTimer;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CgCellShaderApplyVP(CgProgram: TCgProgram; Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CgCellShaderInitialize(CgShader: TCustomCgShader);
    procedure CgCellShaderApplyFP(CgProgram: TCgProgram; Sender: TObject);
    procedure CgCellShaderUnApplyFP(CgProgram: TCgProgram);
    procedure AsyncTimer1Timer(Sender: TObject);
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

uses
  GLFileMD2, FileUtil;

procedure TForm1.FormCreate(Sender: TObject);
var
  r : Single;
  path: UTF8String;
  p: Integer;
begin
   path := ExtractFilePath(ParamStrUTF8(0));
   SetCurrentDirUTF8(path);
   // Load the vertex and fragment Cg programs
   CgCellShader.VertexProgram.LoadFromFile('cellshading_vp.cg');
   CgCellShader.FragmentProgram.LoadFromFile('cellshading_fp.cg');
   p := Pos('DemosLCL', path);
   Delete(path, p+5, Length(path));
   path := IncludeTrailingPathDelimiter(path) + 'media';
   SetCurrentDirUTF8(path);


  // Load and scale the actor
  GLActor1.LoadFromFile('waste.md2');
  r:=GLActor1.BoundingSphereRadius;
  GLActor1.Scale.SetVector(2.5/r,2.5/r,2.5/r);
  GLActor1.AnimationMode:=aamLoop;
  // Load the texture
  GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile('wastecell.jpg');
end;

procedure TForm1.CgCellShaderApplyVP(CgProgram: TCgProgram; Sender: TObject);
begin
  // Apply the per frame uniform parameters
  with CgProgram do begin
    ParamByName('LightDir').SetAsVector(GLLightSource1.AbsoluteDirection);
    ParamByName('ModelViewProj').SetAsStateMatrix( CG_GL_MODELVIEW_PROJECTION_MATRIX, CG_GL_MATRIX_IDENTITY);
    ParamByName('ModelViewIT').SetAsStateMatrix( CG_GL_MODELVIEW_MATRIX, CG_GL_MATRIX_INVERSE_TRANSPOSE);
  end;
end;

procedure TForm1.CgCellShaderInitialize(CgShader: TCustomCgShader);
begin
  // Set up the texture sampler parameter
  CgCellShader.FragmentProgram.ParamByName('Map0').SetAsTexture2D(GLMaterialLibrary1.Materials[0].Material.Texture.Handle);
end;

procedure TForm1.CgCellShaderApplyFP(CgProgram: TCgProgram; Sender: TObject);
begin
  // Enable the texture map sampler for use in the fragment
  // program
  CgProgram.ParamByName('Map0').EnableTexture;
end;

procedure TForm1.CgCellShaderUnApplyFP(CgProgram: TCgProgram);
begin
  // Disable the texture map sampler
  CgProgram.ParamByName('Map0').DisableTexture;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx:=x;
  my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my-y,mx-x);
  mx:=x;
  my:=y;
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  Form1.Caption:=Format('Cg Cell Shading Demo - %.2f FPS',[GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
