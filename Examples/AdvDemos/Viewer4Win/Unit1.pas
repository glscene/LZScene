unit Unit1;

{$MODE Delphi}

interface

uses

  SysUtils,

  Classes,
  DateUtils,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,

  GLLCLViewer,
  GLAsyncTimer,
  GLCrossPlatform,
  GLBaseClasses,
  GLScene,
  GLObjects,
  GLCoordinates,
  GLGeomObjects,
  GLState,
  GLGraph,
  GLCadencer;

type

  { TForm1 }

  TForm1 = class(TForm)
    Panel1: TPanel;
    AsyncTimer1: TGLAsyncTimer;
    GLScene1: TGLScene;
    cam: TGLCamera;
    dc_cam: TGLDummyCube;
    GLCube1: TGLCube;
    light1: TGLLightSource;
    dc_light: TGLDummyCube;
    dc_world: TGLDummyCube;
    GLCylinder1: TGLCylinder;
    dc_views: TGLDummyCube;
    cam1: TGLCamera;
    cam2: TGLCamera;
    cam3: TGLCamera;
    GLSphere1: TGLSphere;
    Panel2: TPanel;
    pos_r: TImage;
    pos_t: TImage;
    pos_c: TImage;
    dc_helpers: TGLDummyCube;
    xyz_grid: TGLXYZGrid;
    vp: TGLSceneViewer;
    Shape1: TShape;
    xy_grid: TGLXYZGrid;
    vp2: TGLSceneViewer;
    vp1: TGLSceneViewer;
    vp3: TGLSceneViewer;
    yz_grid: TGLXYZGrid;
    xz_grid: TGLXYZGrid;
    procedure FormResize(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure vp1BeforeRender(Sender: TObject);
    procedure vp1AfterRender(Sender: TObject);
    procedure vp2BeforeRender(Sender: TObject);
    procedure vp2AfterRender(Sender: TObject);
    procedure vp3BeforeRender(Sender: TObject);
    procedure vp3AfterRender(Sender: TObject);
    procedure vpMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vpMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  public
    procedure showScn(mv, v1, v2, v3: boolean);
  private
    mx, my : Integer;
  end;

type
  TScnMod = (scnTurn, scnMove);

var
  Form1: TForm1;
  xAlign: single = 0.5;
  yAlign: single = 0.5;
  scnMod: set of TScnMod;

//==================================================================
implementation
//==================================================================

{$R *.lfm}

procedure TForm1.FormResize(Sender: TObject);
var
  w, w2, h2: integer;
begin
  w := clientWidth - Panel1.width;
  w2 := round((clientWidth - Panel1.width) * xAlign);
  h2 := round(clientHeight * yAlign);
  vp1.BoundsRect := rect(3, 3, w2 - 4, h2 - 4);
  vp2.BoundsRect := rect(w2 + 3, 3, w - 4, h2 - 4);
  vp3.BoundsRect := rect(3, h2 + 3, w2 - 4, clientHeight - 4);
  vp.BoundsRect := rect(w2 + 3, h2 + 3, w - 4, clientHeight - 4);
  Shape1.BoundsRect := rect(w2, h2, w - 1, clientHeight - 1);

  pos_r.BoundsRect := rect(w2 - 4, 3, w2 + 3, clientHeight - 4);
  pos_t.BoundsRect := rect(3, h2 - 4, w - 4, h2 + 3);
  pos_c.BoundsRect := rect(w2 - 4, h2 - 4, w2 + 3, h2 + 3);
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  if scnMod <> [] then
  begin
    if scnTurn in scnMod then
    begin
    end;
  end;
  GLCube1.Position.Y := sin(GetTickCount / 1000);
  GLSphere1.Position.Z := cos(GetTickCount / 600);
  vp.Refresh;
  vp1.Refresh;
  vp2.Refresh;
  vp3.Refresh;
end;

procedure TForm1.showScn(mv, v1, v2, v3: boolean);
begin
  if not mv then
  begin
    GLCube1.Material.PolygonMode := pmLines;
    GLCylinder1.Material.PolygonMode := pmLines;
    GLSphere1.Material.PolygonMode := pmLines;
    light1.Shining := false;
    xyz_grid.visible := false;
  end
  else
  begin
    GLCube1.Material.PolygonMode := pmFill;
    GLCylinder1.Material.PolygonMode := pmFill;
    GLSphere1.Material.PolygonMode := pmFill;
    light1.Shining := true;
    xyz_grid.visible := true;
  end;
  xz_grid.visible := v1;
  xy_grid.visible := v2;
  yz_grid.visible := v3;
end;

procedure TForm1.vp1BeforeRender(Sender: TObject);
begin
  showScn(false, true, false, false);
end;

procedure TForm1.vp1AfterRender(Sender: TObject);
begin
  showScn(true, false, false, false);
end;

procedure TForm1.vp2BeforeRender(Sender: TObject);
begin
  showScn(false, false, true, false);
end;

procedure TForm1.vp2AfterRender(Sender: TObject);
begin
  showScn(true, false, false, false);
end;

procedure TForm1.vp3BeforeRender(Sender: TObject);
begin
  showScn(false, false, false, true);
end;

procedure TForm1.vpMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.vpMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
   if Shift<>[] then
      Cam.MoveAroundTarget(my-y, mx-x);
   mx:=x; my:=y;
end;

procedure TForm1.vp3AfterRender(Sender: TObject);
begin
  showScn(true, false, false, false);
end;

end.
