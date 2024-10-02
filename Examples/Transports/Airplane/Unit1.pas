unit Unit1;

{$MODE objfpc}{$H+}

interface

uses
  GL, GLExt,
  LCLIntf, LCLType, LMessages,
  SysUtils,

  Classes,
  Math,
  Graphics,
  Controls,
  Forms,
  Dialogs,

  ExtCtrls,
  StdCtrls,

  GLCadencer,
  GLScene,
  GLContext,
  GLVectorTypes,
  GLVectorGeometry,
  GLTexture,
  GLMaterial,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  GLVectorFileObjects,
  GLFile3DS,
  GLBitmapFont,
  GLRenderContextInfo,
  GLWindowsFont, GLLCLViewer,
  GLAsyncTimer, GLObjects, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLDummyCube1: TGLDummyCube;
    GLDummyCube2: TGLDummyCube;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLLightSource1: TGLLightSource;
    GLActor1: TGLActor;
    GLCamera1: TGLCamera;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    Panel1: TPanel;
    flipH_CB: TCheckBox;
    flipV_CB: TCheckBox;
    RadioGroup1: TRadioGroup;
    Render1: TGLDirectOpenGL;
    GLFreeForm2: TGLFreeForm;
    AsyncTimer1: TGLAsyncTimer;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var aAction: TCloseAction);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Render1Render(Sender: TObject; var rci: TGLRenderContextInfo);
  private
    mx, my: Integer;
  public
    ScreenTexture: TGLTextureHandle;
  end;

var
  Form1: TForm1;
  Initialized: Boolean = false;

implementation

{$R *.lfm}

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  Caption := Format('AirCraft [%.2f] FPS',
    [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;

end;

procedure TForm1.FormClose(Sender: TObject; var aAction: TCloseAction);
begin
  GLCadencer1.Enabled := false;
  AsyncTimer1.Enabled := false;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  if RadioGroup1.ItemIndex = 0 then
  begin
    GLFreeForm2.Visible:=False;
    GLActor1.Visible:= true;
  end
  else if RadioGroup1.ItemIndex = 1 then
  begin
    GLActor1.Visible:= False;
    GLFreeForm2.Visible:=True;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLActor1.LoadFromFile('AMX.3DS');
  GLMaterialLibrary1.TexturePaths := '.\Models';
  GLFreeForm2.LoadFromFile('.\Models\model.3ds');

end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
   GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  if Assigned(GLSceneViewer1) then GLSceneViewer1.Invalidate;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

procedure TForm1.Render1Render(Sender: TObject; var rci: TGLRenderContextInfo);
var
  VertexArray: array [0 .. 3] of TVector2f;

  procedure initialize;
  begin
    ScreenTexture := TGLTextureHandle.Create;

    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, screenTexture.Handle);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    glCopyTexImage2d(GL_TEXTURE_2D, 0, GL_RGBA8, 0, 0,
      GLSceneViewer1.ClientWidth, GLSceneViewer1.ClientHeight, 0);
    glDisable(GL_TEXTURE_2D);

    Initialized := True;
  end;

  procedure FlipVertical(var vertices: array of TVector2f);
  var
    tmp1, tmp2: TVector2f;
  begin
    tmp1 := vertices[0];
    tmp2 := vertices[1];

    vertices[0] := vertices[3];
    vertices[1] := vertices[2];
    vertices[3] := tmp1;
    vertices[2] := tmp2;
  end;

  procedure FlipHorizontal(var vertices: array of TVector2f);
  var
    tmp1, tmp2: TVector2f;
  begin
    tmp1 := vertices[0];
    tmp2 := vertices[2];

    vertices[0] := vertices[1];
    vertices[2] := vertices[3];
    vertices[1] := tmp1;
    vertices[3] := tmp2;
  end;

begin
  // allocating texture handle
  // occurs on form creation and on every form resize
  // needed to create texture of exact size of form
  if not initialized then initialize;

  // Create snapshot
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, screenTexture.Handle);
  glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 0, 0, GLSceneViewer1.ClientWidth,
    GLSceneViewer1.ClientHeight);

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadIdentity;
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;

  // drawing rectangle over screen
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);

  VertexArray[0] := Vector2fMake(-1, -1);
  VertexArray[1] := Vector2fMake(1, -1);
  VertexArray[2] := Vector2fMake(1, 1);
  VertexArray[3] := Vector2fMake(-1, 1);

  if flipV_CB.Checked then FlipVertical(vertexArray);
  if flipH_CB.Checked then FlipHorizontal(vertexArray);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2fv(@vertexArray[0]);
  glTexCoord2f(1.0, 0.0);
  glVertex2fv(@vertexArray[1]);
  glTexCoord2f(1.0, 1.0);
  glVertex2fv(@vertexArray[2]);
  glTexCoord2f(0.0, 1.0);
  glVertex2fv(@vertexArray[3]);
  glEnd;

  glVertex2f(-1, -1);
  glVertex2f(1, -1);
  glVertex2f(1, 1);
  glVertex2f(-1, 1);

  glEnable(GL_DEPTH_TEST);
  glDisable(GL_TEXTURE_2D);

  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;
end;

end.
