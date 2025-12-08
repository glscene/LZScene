unit Unit1;


// cf https://www.shadertoy.com/view/ldS3zW

interface

uses

  SysUtils, Classes, Controls, Dialogs, Forms, LCLIntf,
   
  GLScene,GLObjects, GLCadencer, GLLCLViewer,
  GLRenderContextInfo, OpenGLAdapter, GLVectorGeometry,
  GLCrossPlatform, GLAsyncTimer, GLContext,
  GLCoordinates, GLHUDObjects, GLUtils;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    cad: TGLCadencer;
    cam: TGLCamera;
    dc_cam: TGLDummyCube;
    hud: TGLHUDSprite;
    at: TGLAsyncTimer;
    dogl: TGLDirectOpenGL;
    procedure FormCreate(Sender: TObject);
    procedure doglRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure FormResize(Sender: TObject);
    procedure atTimer(Sender: TObject);
    procedure cadProgress(Sender: TObject; const deltaTime, newTime: Double);
  end;

var
  Form1: TForm1;

  glsl: TGLProgramHandle;
  initDGL: boolean;

implementation

{$R *.lfm}

//
// cadProgress
//
procedure TForm1.cadProgress;
begin
  vp.Invalidate;
end;

//
// doglRender
//
procedure TForm1.doglRender;
begin
  if not initDGL then
  begin
    if not(gl.ARB_shader_objects and gl.ARB_fragment_shader) then
    begin
      ShowMessage('Shader not supported by your hardware');
      Halt;
    end;
    glsl := TGLProgramHandle.CreateAndAllocate;
    glsl.AddShader(TGLFragmentShaderHandle,
      LoadAnsiStringFromFile('eiffie_too-early.fp'));
    if not glsl.LinkProgram then
      raise Exception.Create(glsl.InfoLog);
    if not glsl.ValidateProgram then
      raise Exception.Create(glsl.InfoLog);
    initDGL := True;
  end;

  if initDGL then
  begin
    glsl.UseProgramObject;
    glsl.Uniform3f['iResolution'] := AffineVectorMake(vp.Width, vp.Height, 0);
    glsl.Uniform1f['iGlobalTime'] := cad.CurrentTime;
    hud.Render(rci);
    glsl.EndUseProgramObject;
  end;
end;

//
// formCreate
//
procedure TForm1.FormCreate;
begin
  vp.Buffer.RenderingContext.Activate;
end;

//
// formResize
//
procedure TForm1.FormResize;
begin
  hud.Width := vp.Width;
  hud.Height := vp.Height;
  hud.Position.SetPoint(vp.Width div 2, vp.Height div 2, 0);
end;

//
// atTimer
//
procedure TForm1.atTimer;
begin
  caption :=
    format('Shadertoy.com [eiffie, "Too early?"] /  fps: %.3f  time: %.3f',
    [vp.FramesPerSecond, cad.CurrentTime]);
  vp.ResetPerformanceMonitor;
end;



end.
