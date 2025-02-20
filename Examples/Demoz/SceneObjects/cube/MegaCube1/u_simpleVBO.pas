unit u_simpleVBO;

{$MODE Delphi}

interface

uses
  GLScene, GLMesh, GLVectorTypes, GLVectorGeometry, GLRenderContextInfo,
  OpenGL1x, OpenGLTokens, GLContext;


type
  c_simpleVBO = class(TGLSceneObject)
  protected

    f_varr: array of TVector3f;
    f_narr: array of TVector3f;
    f_tarr: array of TTexPoint;
    f_bufs: array[0..2] of Cardinal;

    procedure _initBufs;

  public

    constructor CreateAsChild(a_Owner:TGLBaseSceneObject;
      a_Array:PVertexDataArray; a_VertexCount:integer);

    procedure DoRender(var a_Rci:TGLRenderContextInfo;
        a_RenderSelf,a_RenderChildren:Boolean); override;

    procedure loadData(a_Array:PVertexDataArray; a_VertexCount:integer);

    end;


implementation


//
// _initBufs
//
procedure c_simpleVBO._initBufs;
var
    c: integer;

begin
  c := length(f_varr);
  gl.GenBuffers(1, @f_bufs[0]);
  gl.BindBuffer(GL_ARRAY_BUFFER, f_bufs[0]);
  gl.BufferData(GL_ARRAY_BUFFER, 12 * c, @f_varr[0], GL_STATIC_DRAW);
  gl.BindBuffer(GL_ARRAY_BUFFER, 0);

  gl.GenBuffers(1, @f_bufs[1]);
  gl.BindBuffer(GL_ARRAY_BUFFER_ARB, f_bufs[1]);
  gl.BufferData(GL_ARRAY_BUFFER_ARB, 12 * c, @f_narr[0], GL_STATIC_DRAW);
  gl.BindBuffer(GL_ARRAY_BUFFER_ARB, 0);

  gl.GenBuffers(1, @f_bufs[2]);
  gl.BindBuffer(GL_ARRAY_BUFFER, f_bufs[2]);
  gl.BufferData(GL_ARRAY_BUFFER, 8 * c, @f_tarr[0], GL_STATIC_DRAW);
  gl.BindBuffer(GL_ARRAY_BUFFER, 0);

end;


//
// constructor
//
constructor c_simpleVBO.CreateAsChild;
begin
  inherited CreateAsChild(a_Owner);
  loadData(a_Array, a_VertexCount);
end;


//
// loadData
//
procedure c_simpleVBO.loadData;
var
    i: integer;
begin
  if a_VertexCount < 3 then exit;
  setlength(f_varr, a_VertexCount);
  setlength(f_narr, a_VertexCount);
  setlength(f_tarr, a_VertexCount);

  for i := 0 to a_VertexCount - 1 do begin
    f_varr[i] := a_Array[i].coord;
    f_narr[i] := a_Array[i].normal;
    f_tarr[i] := a_Array[i].textCoord;
    end;

  _initBufs;

end;


//
// DoRender
//
procedure c_simpleVBO.DoRender;
begin
  if high(f_varr) < 2 then exit;
  Material.Apply(a_Rci);

  gl.EnableClientState(GL_VERTEX_ARRAY);
  gl.BindBuffer(GL_ARRAY_BUFFER, f_bufs[0]);
  gl.VertexPointer(3, GL_FLOAT, 0, nil);

  gl.EnableClientState(GL_NORMAL_ARRAY);
  gl.BindBuffer(GL_ARRAY_BUFFER, f_bufs[1]);
  gl.NormalPointer(GL_FLOAT, 0, nil);

  gl.EnableClientState(GL_TEXTURE_COORD_ARRAY);
  gl.BindBuffer(GL_ARRAY_BUFFER, f_bufs[2]);
  gl.TexCoordPointer(2, GL_FLOAT, 0, nil);

  gl.DrawArrays(GL_QUADS, 0, high(f_varr) + 1);

  gl.DisableClientState(GL_VERTEX_ARRAY);
  gl.DisableClientState(GL_NORMAL_ARRAY);
  gl.DisableClientState(GL_TEXTURE_COORD_ARRAY);

  Material.UnApply(a_Rci);

end;


end.
