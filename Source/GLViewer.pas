//
// The graphics engine GLXEngine. The unit of LZScene for Lazarus
//
{
    Platform independant viewer.
}

unit GLViewer;

interface

{$I GLScene.inc}

uses
  GLContext,

  GLLCLViewer;
type

  TGLSceneViewer = GLLCLViewer.TGLSceneViewer;


procedure SetupVSync(const AVSyncMode : TVSyncMode);

implementation

uses
  OpenGLTokens, 
  OpenGLAdapter;

procedure SetupVSync(const AVSyncMode : TVSyncMode);
{$IFDEF MSWINDOWS}
var
  I: Integer;
begin
  if GL.W_EXT_swap_control then
  begin
    I := GL.WGetSwapIntervalEXT;
    case AVSyncMode of
      vsmSync  : if I <> 1 then GL.WSwapIntervalEXT(1);
      vsmNoSync: if I <> 0 then GL.WSwapIntervalEXT(0);
    else
       Assert(False);
    end;
  end;
end;
{$ENDIF}
{$IFDEF Linux}
begin
  if GL.X_SGI_swap_control then
  begin
    case AVSyncMode of
      vsmSync  : GL.XSwapIntervalSGI(GL_True);
      vsmNoSync: GL.XSwapIntervalSGI(GL_False);
    else
       Assert(False);
    end;
  end;
end;
{$ENDIF}
{$IFDEF DARWIN}
var ctx: TAGLContext;
const ISync: Integer = 0;
      INoSync: Integer = 1;
begin
  if Assigned(GL) then
  begin
    ctx := GL.aGetCurrentContext();
    if Assigned(ctx) then
      case AVSyncMode of
        vsmSync  : GL.aSetInteger(ctx, AGL_SWAP_INTERVAL, @ISync); 
        vsmNoSync: GL.aSetInteger(ctx, AGL_SWAP_INTERVAL, @INoSync);
      else
         Assert(False);
      end;
  end;
end;
{$ENDIF}

end.
