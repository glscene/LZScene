unit Unit1;

{$mode objfpc}{$h+}

interface

uses
  SysUtils,Classes,
  Graphics, Controls, Forms,
  Dialogs,
  GLCadencer, GLTexture, {GLWin32Viewer,} GLScene,
  GLkeyboard,
  GLVectorFileObjects,
  GLObjects, GLFile3ds,
  GLVectorTypes, GLVectorGeometry,
  GLGeomObjects, GLCoordinates,
  GLCrossPlatform, GLLCLViewer, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    cad: TGLCadencer;
    cam: TGLCamera;
    dc: TGLDummyCube;
    ff: TGLFreeForm;
    points1: TGLPoints;
    points2: TGLPoints;
    poly: TGLPolygon;
    procedure FormClose(Sender: TObject; var aAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure cadProgress(Sender: TObject; const dt, nt: Double);
    procedure vpMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
     
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetCurrentDir(Application.ExeName);
  ff.LoadFromFile('horse.3ds');
  ff.Scale.Scale(8 / ff.BoundingSphereRadius);
end;


procedure TForm1.FormClose(Sender: TObject; var aAction: TCloseAction);
begin
  cad.Enabled := false;

end;

procedure TForm1.cadProgress(Sender: TObject; const dt, nt: Double);
begin
  ff.roll(dt * 10);
end;

procedure TForm1.vpMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  sel: Tvector;
  rs, rv, v: Tvector4f;
  v1, v2, v3: Tvector3f;
  a1, a2, a3, tr, mo, fg: Integer;
begin
  points1.Positions.Clear;
  points2.Positions.Clear;
  poly.Nodes.Clear;

  setvector(rs, cam.AbsolutePosition);
  setvector(rv, vp.Buffer.ScreenToVector(X, vp.Height - Y));

  if ff.RayCastIntersect(rs, rv, @sel) then
  begin
    for a1 := 0 to ff.MeshObjects.Count - 1 do
      for a2 := 0 to ff.MeshObjects[a1].FaceGroups.Count - 1 do
        with (ff.MeshObjects[a1].FaceGroups[a2] as TFGVertexIndexList) do
          for a3 := 0 to TriangleCount - 1 do
          begin
            v1 := ff.LocalToAbsolute(ff.MeshObjects[a1].Vertices
              [VertexIndices[a3 * 3 + 0]]);
            v2 := ff.LocalToAbsolute(ff.MeshObjects[a1].Vertices
              [VertexIndices[a3 * 3 + 1]]);
            v3 := ff.LocalToAbsolute(ff.MeshObjects[a1].Vertices
              [VertexIndices[a3 * 3 + 2]]);
            if RayCastTriangleIntersect(rs, rv, v1, v2, v3, @v) then
              if vectorlength(VectorSubtract(v,sel)) < 0.0001 then
              begin
                mo := a1;
                fg := a2;
                tr := a3;
                break;
              end;
          end;

    with points1 do
    begin
      Colors.Add(1, 0, 0, 1);
      Positions.Add(v1);
      Positions.Add(v2);
      Positions.Add(v3);
    end;

    with poly.Nodes do
    begin
      AddNode(v1);
      AddNode(v2);
      AddNode(v3);
    end;

    points2.Colors.Add(0, 0, 1, 1);
    with (ff.MeshObjects[mo].FaceGroups[fg] as TFGVertexIndexList),
      ff.MeshObjects[mo], points2.Positions do
    begin
      Add(Vertices[VertexIndices[tr * 3 + 0]]);
      Add(Vertices[VertexIndices[tr * 3 + 1]]);
      Add(Vertices[VertexIndices[tr * 3 + 2]]);
    end;

    caption :=
      format('object: %d  facegroup: %d  triangle: %d (%.3f %.3f %.3f) (%.3f %.3f %.3f) (%.3f %.3f %.3f)',
      [mo, fg, tr, v1.X, v1.Y, v1.Z, v2.X, v2.Y, v2.Z,
       v3.X, v3.Y, v3.Z]);

  end
  else
    caption := 'none';

end;

end.
