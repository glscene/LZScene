{: Rendering to a TMemoryViewer and using the render as texture.<p>

   This sample illustrates use of the TGLMemoryViewer to render to a texture.
   The simple scene features a lone cube, when rendered to the memory viewer,
   a red background is used (the TGLSceneViewer uses a gray background).<p>

   After each main viewer render, the scene is rendered to the memory viewer
   and the result is copied to the texture of the cube (a "BlankImage" was
   defined at runtime, because we only need to specify the texture size).
   Most of the time, you won't need to render textures at each frame, and a set
   options illustrates that. The 1:2 mode is significantly faster and visually
   equivalent (even with VSync on, to limit the framerate).<p>

   Never forget a memory viewer will use 3D board memory, thus reducing
   available space for GLVectorGeometry and textures... try using only one memory
   viewer and maximize its use.

   This sample will only work on 3D boards that support WGL_ARB_pbuffer, which
   should be the case for all of the modern boards, and even some of the older
   ones.
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, StdCtrls, GLObjects, ExtCtrls, GLCadencer,
  GLTexture, GLLCLViewer, OpenGLAdapter, GLCrossPlatform, GLCoordinates,
  GLBaseClasses, GLContext;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    CheckBox1: TCheckBox;
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    Cube1: TGLCube;
    GLLightSource1: TGLLightSource;
    GLMemoryViewer1: TGLMemoryViewer;
    GLCadencer1: TGLCadencer;
    Label1: TLabel;
    RB1to1: TRadioButton;
    RB1to2: TRadioButton;
    RB1to10: TRadioButton;
    Label2: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure CheckBox1Click(Sender: TObject);
    procedure GLSceneViewer1AfterRender(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RB1to1Click(Sender: TObject);
  private
    { Private declarations }
    textureFramerateRatio, n : Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
   textureFramerateRatio:=1;
   n:=0;
end;

procedure TForm1.RB1to1Click(Sender: TObject);
begin
   textureFramerateRatio:=(Sender as TRadioButton).Tag;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
   if CheckBox1.Checked then
      GLSceneViewer1.VSync:=vsmSync
   else GLSceneViewer1.VSync:=vsmNoSync;
end;

procedure TForm1.GLSceneViewer1AfterRender(Sender: TObject);
begin
   if
{$IFDEF MSWINDOWS}
   not GLSceneViewer1.Buffer.RenderingContext.GL.W_ARB_pbuffer
{$ENDIF}
{$IFDEF LINUX}
   not (GLSceneViewer1.Buffer.RenderingContext.GL.X_VERSION_1_3 or GLSceneViewer1.Buffer.RenderingContext.GL.X_VERSION_1_4)
{$ENDIF}
{$IFDEF DARWIN}
   not (GLSceneViewer1.Buffer.RenderingContext.GL.A_pixel_buffer)
{$ENDIF}
   then
   begin
      ShowMessage( 'ARB_pbuffer not supported...'#13#10#13#10
                  +'Get newer graphics hardware or try updating your drivers!');
      GLSceneViewer1.AfterRender:=nil;
      Exit;
   end;
   Inc(n);
   try
      if n>=textureFramerateRatio then begin
         // render to the viewer
         GLMemoryViewer1.Render;
         // copy result to the textures
         GLMemoryViewer1.CopyToTexture(Cube1.Material.Texture);
         n:=0;
      end;
   except
      // pbuffer not supported... catchall for exotic ICDs...
      GLSceneViewer1.AfterRender:=nil;
      raise;
   end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   DummyCube1.TurnAngle:=newTime*60;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
