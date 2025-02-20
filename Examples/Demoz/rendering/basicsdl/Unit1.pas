{: Basic demo for using the SDLViewer in GLScene.<p>

   The SDL Viewer allows to use SDL for setting up OpenGL, but still render
   with GLScene. The main differences are that SDL has no design-time preview
   and that  only one OpenGL window may exists throughout the application's
   lifetime (you may have standard forms around it, but as soon as the SDL
   window is closed, the application terminates.<p>

   The SDL viewer is more suited for games or simple apps that aim for
   cross-platform support, for SDL is available on multiple platforms.
   SDL also provides several game-related support APIs for sound, controlers,
   video etc. (see http://www.libsdl.org).<p>

   The rendered scene is similar to the one in the materials/cubemap demo.
}
unit Unit1;

interface

uses
  Forms, SysUtils, Classes, GLScene, GLObjects, GLSDLContext, SDL,
  GLTeapot, GLCrossPlatform, GLCoordinates, GLBaseClasses;

type
  TDataModule1 = class(TDataModule)
    GLScene1: TGLScene;
    GLSDLViewer1: TGLSDLViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Teapot1: TGLTeapot;
    procedure DataModuleCreate(Sender: TObject);
    procedure GLSDLViewer1EventPollDone(Sender: TObject);
    procedure GLSDLViewer1Resize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    firstPassDone: boolean;
  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.lfm}

uses Dialogs, GLTexture, FileUtil;

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
  // When using SDL, the standard VCL message queue is no longer operational,
  // so you must have/make your own loop to prevent the application from
  // terminating immediately
  GLSDLViewer1.Render;
  while GLSDLViewer1.Active do
  begin
    // Message queue is not operational, but there may still be some messages
    Forms.Application.ProcessMessages;
    // Relinquish some of that CPU time
    SDL_Delay(1);
    // Slowly rotate the teapot
    Teapot1.RollAngle := 4 * Frac(Now * 24) * 3600;
  end;
end;

procedure TDataModule1.GLSDLViewer1EventPollDone(Sender: TObject);
var
  path: UTF8String;
  p: integer;
begin
  path := ExtractFilePath(ParamStrUTF8(0));
  p := Pos('DemosLCL', path);
  Delete(path, p + 5, Length(path));
  path := IncludeTrailingPathDelimiter(path) + 'media';
  SetCurrentDirUTF8(path);
  if not firstPassDone then
  begin
    // Loads a texture map for the teapot
    // (see materials/cubemap for details on that)

    // The odd bit is that it must be done upon first render, otherwise
    // SDL OpenGL support has not been initialized and things like checking
    // an extension support (cube maps here) would fail...
    // Something less clunky will be introduced, someday...
    firstPassDone := True;
    if not GLSDLViewer1.Buffer.RenderingContext.GL.ARB_texture_cube_map then
      ShowMessage('Your graphics board does not support cube maps...'#13#10 +
        'So, no cube maps for ya...')
    else
    begin
      with Teapot1.Material.Texture do
      begin
        ImageClassName := TGLCubeMapImage.ClassName;
        with Image as TGLCubeMapImage do
        begin
          Picture[cmtPX].LoadFromFile('cm_left.jpg');
          Picture[cmtNX].LoadFromFile('cm_right.jpg');
          Picture[cmtPY].LoadFromFile('cm_top.jpg');
          Picture[cmtNY].LoadFromFile('cm_bottom.jpg');
          Picture[cmtPZ].LoadFromFile('cm_back.jpg');
          Picture[cmtNZ].LoadFromFile('cm_front.jpg');
        end;
        MappingMode := tmmCubeMapReflection;
        Disabled := False;
      end;
    end;
  end;

  GLSDLViewer1.Render;
end;

procedure TDataModule1.GLSDLViewer1Resize(Sender: TObject);
begin
  // Zoom if SDL window gets smaller/bigger
  GLCamera1.SceneScale := GLSDLViewer1.Width / 160;
end;

end.

