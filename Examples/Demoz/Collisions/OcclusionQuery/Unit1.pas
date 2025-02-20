{: Demo for Occlusion Querying (also includes timer query).<p>

   Occlusion querying is useful for counting how many pixels (or samples) of an
   object are visible on screen.<p>

   This demo renders a few objects normally, then queries how many pixels are
   visible of the objects rendered between the start of the query and the
   end of the query (the objects contained inside dcTestObjects dummycube).<p>

   Any objects rendered after the query has finished won't be included in the
   results.<p>

   A timer query is also included to see how long it takes to render the same
   objects.
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLGeomObjects, GLObjects, GLCadencer, GLLCLViewer,
  StdCtrls, ExtCtrls, GLCrossPlatform, GLCoordinates, GLBaseClasses,
  GLRenderContextInfo, GLContext;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLCube1: TGLCube;
    GLCylinder1: TGLCylinder;
    GLDummyCube1: TGLDummyCube;
    OGLBeginQueries: TGLDirectOpenGL;
    dcTestObjects: TGLDummyCube;
    OGLEndQueries: TGLDirectOpenGL;
    GLTorus1: TGLTorus;
    GLLightSource1: TGLLightSource;
    GLDummyCube2: TGLDummyCube;
    GLCube2: TGLCube;
    Timer1: TTimer;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    GLCone1: TGLCone;
    procedure GLSceneViewer1BeforeRender(Sender: TObject);
    procedure OGLBeginQueriesRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure OGLEndQueriesRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure FormDestroy(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  TimerQuery: TGLTimerQueryHandle;
  OcclusionQuery: TGLOcclusionQueryHandle;
  bOcclusionQuery: TGLBooleanOcclusionQueryHandle;

  queriesCreated: boolean;
  timerQuerySupported: boolean;

  timeTaken: integer;  // in nanoseconds
  samplesPassed: integer;

implementation

{$R *.lfm}

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Delete the queries
  TimerQuery.Free;
  OcclusionQuery.Free;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  // Move some of the scene objects around
  GLDummyCube1.Position.X := Sin(newTime);
  dcTestObjects.Turn(DeltaTime * 50);
  dcTestObjects.Position.z := 2 * Sin(newTime);
  GLDummyCube2.Position.X := -sin(newTime);
end;

procedure TForm1.GLSceneViewer1BeforeRender(Sender: TObject);
begin
  // Occlusion queries are supported by extensions with lower version of OpenGL.
  // To use them, you'd need to check if GL_NV_occlusion_query or GL_ARB_occlusion_query
  // extensions are present, and makes the appropriate calls to the functions/procedures
  // they provide.
  if (not TGLOcclusionQueryHandle.IsSupported) then
  begin
    Messagedlg('Requires hardware that supports occlusion queries to run',
      mtError, [mbOK], 0);
    Close;
  end;
end;

procedure TForm1.OGLBeginQueriesRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  // Generate the queries, if not already created
  if not queriesCreated then
  begin
    OcclusionQuery := TGLOcclusionQueryHandle.CreateAndAllocate();
    CheckBox1.Enabled := TGLBooleanOcclusionQueryHandle.IsSupported;
    if CheckBox1.Enabled then
      bOcclusionQuery := TGLBooleanOcclusionQueryHandle.CreateAndAllocate();

    timerQuerySupported := TGLTimerQueryHandle.IsSupported;
    if timerQuerySupported then
      TimerQuery := TGLTimerQueryHandle.CreateAndAllocate();
    queriesCreated := true;
  end;
  // Begin the timer + occlusion queries

  if timerQuerySupported then
    TimerQuery.BeginQuery;
  if CheckBox1.Checked then
    bOcclusionQuery.BeginQuery
  else
    OcclusionQuery.BeginQuery;
end;

procedure TForm1.OGLEndQueriesRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  lQuery: TGLQueryHandle;
begin
  // End the timer + occlusion queries
  if CheckBox1.Checked then
    lQuery := bOcclusionQuery
  else
    lQuery := OcclusionQuery;
  lQuery.EndQuery;
  if timerQuerySupported then
    TimerQuery.EndQuery;

  // Most of the frame rate is lost waiting for results to become available
  //  + updating the captions every frame, but as this is a demo, we want to
  // see what is going on.

  while not lQuery.IsResultAvailable do
    { wait }; // would normally do something in this period before checking if
  // result is available

  samplesPassed := OcclusionQuery.PixelCount;

  if timerQuerySupported then
  begin
    while not TimerQuery.IsResultAvailable do
      { wait }; // would normally do something in this period before checking if
    // result is available
    timeTaken := TimerQuery.Time;
    // Use this line instead of the one above to use 64 bit timer, to allow
    // recording time periods more than a couple of seconds (requires Delphi 7+)
    // timeTaken := TimerQuery.QueryResultUInt64;
  end;

  case CheckBox1.Checked of
    True:
    begin
      label3.Visible := not lQuery.QueryResultBool;
    end;
    False:
    begin
      label3.Visible := (samplesPassed = 0);
      label2.caption := 'Number of test pixels visible: ' + IntToStr(samplesPassed);
    end;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Convert time taken from ns => ms & display
  if timerQuerySupported then
    label1.Caption := 'Time taken: ' + FloatToSTr(timeTaken / 1000000) + ' ms'
  else
    label1.Caption := 'Time query unavailable, requires hardware support';

  Caption := GLSceneViewer1.FramesPerSecondText(0);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
