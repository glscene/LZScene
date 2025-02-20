unit u_Main;

{$MODE Delphi}

interface

uses
  SysUtils,Classes,
 Controls, Forms, StdCtrls, ExtCtrls, Graphics,
  //GLS
GLCrossPlatform, GLBaseClasses, GLScene, GLObjects, GLMaterial,
  GLHUDObjects, GLCoordinates, GLMesh, GLVectorFileObjects, GLCadencer,
  GLRenderContextInfo, GLAsyncTimer, GLVectorTypes, GLVectorGeometry, //openGL1x,
  GLTexture, GLContext, GLLCLViewer,
  u_simpleVBO;


type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    cam: TGLCamera;
    dc: TGLDummyCube;
    Bevel1: TBevel;
    dogl: TGLDirectOpenGL;
    GLFreeForm: TGLFreeForm;
    GLMesh: TGLMesh;
    GLCube: TGLCube;
    cad: TGLCadencer;
    light: TGLLightSource;
    at: TGLAsyncTimer;
    matlib: TGLMaterialLibrary;
    back: TGLHUDSprite;
    Panel1: TPanel;
    Image1: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    cnt_1: TLabel;
    cnt_2: TLabel;
    cnt_3: TLabel;
    Image2: TImage;
    pb_1: TImage;
    pb_2: TImage;
    Image5: TImage;
    pb_3: TImage;
    Image7: TImage;
    cnt_4: TLabel;
    pb_4: TImage;
    Image9: TImage;
    vp: TGLSceneViewer;
    procedure doglRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure cadProgress(Sender: TObject; const deltaTime,newTime: Double);
    procedure atTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public

    vbo: c_simpleVBO;

    cap: string;

    frames: integer;
    test_num: integer;
    test_dt: integer;
    test_res: array[0..3] of integer;
    test_pb: array[0..3] of TImage;
    test_cap: array[0..3] of TLabel;

    procedure createGeometry;
    procedure fill_GLMesh;
    procedure fill_GLFreeForm;
    procedure fill_VBO;
  end;

const
    q0 = 10;
    q1 = q0 * 2 + 1;
    qcnt = q1 * q1 * q1 * 6 * 4;

var
  Form1: TForm1;
  Quads: array[0..qcnt-1] of TVertexData;


implementation


{$R *.lfm}

procedure TForm1.doglRender(Sender: TObject; var rci: TGLRenderContextInfo);
  var
      i,j,k: integer;

  begin
    case test_num of
      0: for i := -q0 to q0 do
          for j := -q0 to q0 do
            for k := -q0 to q0 do
              with GLCube do begin
                position.SetPoint(i, j, k);
                Render(rci);
                end;
      1: GLMesh.Render(rci);
      2: GLFreeForm.Render(rci);
      3: VBO.Render(rci);
      end;
end;

procedure TForm1.cadProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  inc(frames);
  dogl.TurnAngle := sin(newtime * 2) * 4 - 14;
end;

procedure TForm1.atTimer(Sender: TObject);
begin
caption := cap + ' / fps: ' + format('%.2f', [vp.FramesPerSecond]);
vp.ResetPerformanceMonitor;

dec(test_dt);

inc(test_res[test_num], frames);
test_pb[test_num].Width := 8 * (21 - test_dt);
test_cap[test_num].Caption := inttostr(test_res[test_num]);
frames := 0;

if test_dt = 0 then begin
  inc(test_num);
  test_dt := 21;
  end;

if ((test_num = 3) and (VBO = nil)) or (test_num = 4) then begin
  caption := cap + ' / tested';
  at.Enabled := false;
  cad.Enabled := false;
  end;
end;

//
// FormCreate
//
procedure TForm1.FormCreate(Sender: TObject);

  procedure att(i:integer; p:TImage; c:TLabel);
  begin
    test_res[i] := 0;
    test_pb[i] := p;
    test_cap[i] := c;
  end;

begin
  vp.Buffer.RenderingContext.Activate;

  clientWidth := 1024;
  clientHeight := 512 + 48;
  cap := 'MegaCube / cubes: ' + IntToStr(qcnt div 24) +', triangles: ' +
    IntToStr(qcnt div 2);

  frames := 0;
  test_num := 0;
  test_dt := 21;

  att(0, pb_1 ,cnt_1);
  att(1, pb_2 ,cnt_2);
  att(2, pb_3 ,cnt_3);
  att(3, pb_4 ,cnt_4);

  panel1.DoubleBuffered := true;


  createGeometry;
  fill_GLMesh;
  fill_GLFreeForm;
  fill_VBO;
end;


//
// genGeometry
//
procedure TForm1.createGeometry;
const
    d = 0.25;
var
    i,j,k,q: integer;

  procedure _addVertices;
  var vc: array[0..7] of TVector3f;
  begin
    SetVector(vc[0], i - d, j - d, k - d);
    SetVector(vc[1], i - d, j - d, k + d);
    SetVector(vc[2], i - d, j + d, k - d);
    SetVector(vc[3], i - d, j + d, k + d);
    SetVector(vc[4], i + d, j - d, k - d);
    SetVector(vc[5], i + d, j - d, k + d);
    SetVector(vc[6], i + d, j + d, k - d);
    SetVector(vc[7], i + d, j + d, k + d);
    quads[q + 00].coord := vc[0];
    quads[q + 01].coord := vc[2];
    quads[q + 02].coord := vc[6];
    quads[q + 03].coord := vc[4];
    quads[q + 04].coord := vc[0];
    quads[q + 05].coord := vc[1];
    quads[q + 06].coord := vc[3];
    quads[q + 07].coord := vc[2];
    quads[q + 08].coord := vc[1];
    quads[q + 09].coord := vc[5];
    quads[q + 10].coord := vc[7];
    quads[q + 11].coord := vc[3];
    quads[q + 12].coord := vc[4];
    quads[q + 13].coord := vc[6];
    quads[q + 14].coord := vc[7];
    quads[q + 15].coord := vc[5];
    quads[q + 16].coord := vc[2];
    quads[q + 17].coord := vc[3];
    quads[q + 18].coord := vc[7];
    quads[q + 19].coord := vc[6];
    quads[q + 20].coord := vc[1];
    quads[q + 21].coord := vc[4];
    quads[q + 22].coord := vc[5];
    quads[q + 23].coord := vc[1];
  end;

  procedure _addNormals;
  var i:integer;
  begin
    for i := 0 to 3 do begin
      quads[q + i + 00].normal := MinusZVector;
      quads[q + i + 04].normal := MinusXVector;
      quads[q + i + 08].normal := ZVector;
      quads[q + i + 12].normal := XVector;
      quads[q + i + 16].normal := YVector;
      quads[q + i + 20].normal := MinusYVector;
      end;
    end;

  procedure _addTexCoords;
  var i:integer;
  begin
    for i := 0 to 5 do begin
      quads[q + i * 4 + 0].textCoord := NullTexPoint;
      quads[q + i * 4 + 1].textCoord := YTexPoint;
      quads[q + i * 4 + 2].textCoord := XYTexPoint;
      quads[q + i * 4 + 3].textCoord := XTexPoint;
      end;
    end;

begin

  for i := -q0 to q0 do
    for j := -q0 to q0 do
      for k := -q0 to q0 do begin

        q := (q1 * ((i + q0) * q1 + (j + q0)) + k + q0) * 24;

        _addVertices;
        _addNormals;
        _addTexCoords;

        end;

end;


//
// fill_GLMesh
//
procedure TForm1.fill_GLMesh;
var
    i: integer;

begin

  with GLMesh.Vertices do begin
    Clear;
    Capacity := length(quads);
    for i := 0 to high(quads) do
      AddVertex(quads[i]);
    end;

end;


//
// fill_GLFreeForm
//
procedure TForm1.fill_GLFreeForm;
var
    i: integer;
    MObj: TGLMeshObject;
    FG: TFGVertexIndexList;

begin

  MObj := TGLMeshObject.CreateOwned(GLFreeForm.MeshObjects);
  MObj.Mode := momFaceGroups;
  FG := TFGVertexIndexList.CreateOwned(MObj.FaceGroups);
  FG.Mode := fgmmQuads;

  with Mobj do begin

    Vertices.Capacity := length(quads);
    Normals.Capacity := length(quads);
    TexCoords.Capacity := length(quads);

    for i := 0 to high(quads) do begin
      with quads[i] do begin
        Vertices.Add(coord);
        Normals.Add(normal);
        TexCoords.Add(textCoord);
        end;
      FG.Add(i);
      end;

    end;

  //with GLFreeForm do ObjectStyle := ObjectStyle + [osDirectDraw];

end;


//
// fill_VBO
//
procedure TForm1.fill_VBO;
begin

  if gl.ARB_vertex_buffer_object then begin

    VBO := c_simpleVBO.CreateAsChild(dogl, @quads, qcnt);
    with VBO.Material do begin
      FrontProperties.Diffuse.SetColor(1, 1, 1, 1);
      MaterialLibrary := matlib;
      LibMaterialName := 'logo';
      end;
    VBO.Visible := false;

    end
  else cnt_4.Caption := ' ---';

end;


procedure TForm1.FormShow(Sender: TObject);
begin

  cad.Enabled := true;
  at.Enabled := true;

end;

end.
