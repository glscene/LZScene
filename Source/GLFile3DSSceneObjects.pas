//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  3ds-specific scene objects.

   History : 
       23/08/10 - Yar - Replaced OpenGL1x to OpenGLTokens
       22/04/10 - Yar - Fixes after GLState revision
       05/03/10 - DanB - More state added to TGLStateCache
       17/05/08 - DaStr - Added vGLFile3DSSceneObjects_RenderCameraAndLights
       06/04/08 - DaStr - Initial version (by Lexer)
   
}
unit GLFile3DSSceneObjects;

interface

{$I GLScene.inc}

uses
  // VCL
  Classes,
  SysUtils,
  Math,

  GLVectorGeometry, OpenGLTokens, OpenGLAdapter, GLContext, GLScene,
  GLVectorFileObjects, GLVectorTypes, GLPersistentClasses,
  GLCrossPlatform, GLCoordinates, GLRenderContextInfo, GLState;

type
  TGLFile3DSLight = class(TGLLightSource)
  private
    FTargetPos: TGLCoordinates;
    FHotSpot: Single;
    FMultipler: Single;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoRender(var rci: TGLRenderContextInfo; renderSelf, renderChildren: Boolean); override;
    procedure CoordinateChanged(Sender: TGLCustomCoordinates); override;
    destructor Destroy; override;
  published
    property SpotTargetPos: TGLCoordinates read FTargetPos;
    property HotSpot: Single read FHotSpot write FHotSpot;
    property Multipler: Single read FMultipler write FMultipler;
  end;

  TGLFile3DSCamera = class(TGLCamera)
  private
    FTargetPos: TGLCoordinates;
    FQuadCyl: array[0..1] of PGLUquadric;
    FQuadDisk: array[0..1] of PGLUquadric;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoRender(var rci: TGLRenderContextInfo; renderSelf, renderChildren: Boolean); override;
    procedure CoordinateChanged(Sender: TGLCustomCoordinates); override;
    destructor Destroy; override;
  published
    property CameraTargetPos: TGLCoordinates read FTargetPos;
    property RollAngle;
  end;

  TGLFile3DSActor = class(TGLActor)
  private
    procedure ReadMesh(Stream: TStream);
    procedure WriteMesh(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  end;

  TGLFile3DSFreeForm = class(TGLFreeForm)
  private
    FTransfMat, FScaleMat, ParentMatrix: TMatrix;

    FS_Rot3DS: TGLCoordinates4;
    FRot3DS: TGLCoordinates4;
    FScale3DS: TGLCoordinates4;
    procedure ReadMesh(Stream: TStream);
    procedure WriteMesh(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    FRefMat: TMatrix;
    constructor Create(AOWner: TComponent); override;
    destructor Destroy; override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    procedure CoordinateChanged(Sender: TGLCustomCoordinates); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function BarycenterAbsolutePosition: TVector; override;
  published
    property S_Rot3DS: TGLCoordinates4 read FS_Rot3DS;
    property Rot3DS: TGLCoordinates4 read FRot3DS;
    property Scale3DS: TGLCoordinates4 read FScale3DS;
  end;

var
  vGLFile3DSSceneObjects_RenderCameraAndLights: Boolean = False;

implementation

function MakeRotationQuaternion(const axis: TAffineVector; angle: Single): TQuaternion;
var
  v: Tvector;
  halfAngle, invAxisLengthMult: Single;
begin
  halfAngle := (angle) / 2;
  invAxisLengthMult := 1 / VectorLength(axis) * sin(halfAngle);

  v.V[0] := axis.V[0] * invAxisLengthMult;
  v.V[1] := axis.V[1] * invAxisLengthMult;
  v.V[2] := axis.V[2] * invAxisLengthMult;
  v.V[3] := cos(halfAngle);

  Result.ImagPart := AffineVectorMake(v);
  Result.RealPart := v.V[3];
end;

function QuaternionToRotateMatrix(const Quaternion: TQuaternion): TMatrix;
var
  wx, wy, wz, xx, yy, yz, xy, xz, zz, x2, y2, z2: Single;
  quat: TVector;
  m: TMatrix;
begin
  quat := VectorMake(Quaternion.ImagPart);
  quat.V[3] := Quaternion.RealPart;

  x2 := quat.V[0] + quat.V[0];
  y2 := quat.V[1] + quat.V[1];
  z2 := quat.V[2] + quat.V[2];
  xx := quat.V[0] * x2;
  xy := quat.V[0] * y2;
  xz := quat.V[0] * z2;
  yy := quat.V[1] * y2;
  yz := quat.V[1] * z2;
  zz := quat.V[2] * z2;
  wx := quat.V[3] * x2;
  wy := quat.V[3] * y2;
  wz := quat.V[3] * z2;

  m.V[0].V[0] := 1.0 - (yy + zz);
  m.V[0].V[1] := xy - wz;
  m.V[0].V[2] := xz + wy;
  m.V[1].V[0] := xy + wz;
  m.V[1].V[1] := 1.0 - (xx + zz);
  m.V[1].V[2] := yz - wx;
  m.V[2].V[0] := xz - wy;
  m.V[2].V[1] := yz + wx;
  m.V[2].V[2] := 1.0 - (xx + yy);

  m.V[0].V[3] := 0;
  m.V[1].V[3] := 0;
  m.V[2].V[3] := 0;
  m.V[3].V[0] := 0;
  m.V[3].V[1] := 0;
  m.V[3].V[2] := 0;
  m.V[3].V[3] := 1;

  Result := m;
end;

constructor TGLFile3DSLight.Create(AOwner: TComponent);
begin
  inherited;

  FTargetPos := TGLCoordinates.CreateInitialized(self, VectorMake(NullVector), csPoint);
  FHotSpot := 1;
  FMultipler := 1;
end;

procedure TGLFile3DSLight.DoRender(var rci: TGLRenderContextInfo; renderSelf, renderChildren: Boolean);

  procedure BuildFace;
  begin
    GL.Begin_(GL_TRIANGLES);
    GL.Vertex3f(0.03, 0, 0);
    GL.Vertex3f(0, 0.03, 0);
    GL.Vertex3f(0, 0, 0.07);
    GL.End_;
  end;

var
  dv: Single;

begin
  inherited;
  if not vGLFile3DSSceneObjects_RenderCameraAndLights then
    Exit;

  rci.GLStates.PolygonMode := pmLines;
  GL.PushMatrix;

  dv := VectorDistance(Position.AsVector, rci.cameraPosition);
  GL.Scalef(dv, dv, dv);

  // Up.
  BuildFace;
  GL.Rotatef(90, 0, 0, 1);
  BuildFace;
  GL.Rotatef(180, 0, 0, 1);
  BuildFace;
  GL.Rotatef(270, 0, 0, 1);
  BuildFace;

  // Down.
  GL.Rotatef(180, 0, 1, 0);
  BuildFace;
  GL.Rotatef(90, 0, 0, 1);
  BuildFace;
  GL.Rotatef(180, 0, 0, 1);
  BuildFace;
  GL.Rotatef(270, 0, 0, 1);
  BuildFace;

  GL.PopMatrix;
end;

procedure TGLFile3DSLight.CoordinateChanged(Sender: TGLCustomCoordinates);
begin
  inherited;

  if (Sender = FTargetPos) or (Sender = Position) then
    SpotDirection.SetVector(VectorNormalize(VectorSubtract(FTargetPos.AsAffineVector, Position.AsAffineVector)));
end;

destructor TGLFile3DSLight.Destroy;
begin
  FTargetPos.Free;
  inherited;
end;

constructor TGLFile3DSCamera.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;

  FTargetPos := TGLCoordinates.CreateInitialized(self, VectorMake(NullVector), csPoint);

  for I := 0 to 1 do
  begin
    //    FQuadCyl[I] := gluNewQuadric;
    //    FQuadDisk[I] := gluNewQuadric;
    //    gluQuadricNormals(FQuadCyl[I], GLU_SMOOTH);
    //    gluQuadricNormals(FQuadDisk[I], GLU_SMOOTH);
  end;
end;

procedure TGLFile3DSCamera.DoRender(var rci: TGLRenderContextInfo; renderSelf, renderChildren: Boolean);

  procedure BuildCyl;
  begin
    //    gluCylinder(FQuadCyl[0], 1, 1, 0.5, 6, 1);
    //    glTranslatef(0, 0, 0.5);
    //    gluDisk(FQuadDisk[0], 0, 1, 6, 1);
    GL.Translatef(0, 0, -0.5);
    rci.GLStates.InvertGLFrontFace;
    //    gluDisk(FQuadDisk[0], 0, 1, 6, 1);
    rci.GLStates.InvertGLFrontFace;
  end;

  procedure BuildFace;
  begin
    GL.Rotatef(-90, 0, 1, 0);
    GL.Rotatef(45, 0, 0, 1);
    GL.Translatef(0, -0.5, 1);
    //    gluCylinder(FQuadCyl[0], 0.5, 1.3, 2.4, 4, 1);
    GL.Translatef(0, 0, 2.4);
    //    gluDisk(FQuadDisk[0], 0, 1.3, 4, 1);
  end;

var
  dv, ang: Single;
  v, v1: TAffineVector;

begin
  inherited;
  if not vGLFile3DSSceneObjects_RenderCameraAndLights then
    Exit;

  v := VectorNormalize(VectorSubtract(FTargetPos.AsAffineVector, Position.AsAffineVector));

  v1 := AffineVectorMake(v.V[0], v.V[1], 0);
  NormalizeVector(v1);
  ang := GLVectorGeometry.arccos(VectorDotProduct(v, v1));

  rci.GLStates.PolygonMode := pmLines;

  GL.PushMatrix;
  GL.Rotatef(ang * 180 / pi, 0, 0, 1);
  dv := VectorDistance(Position.AsVector, rci.cameraPosition);
  GL.Scalef(dv / 25, dv / 25, dv / 25);

  GL.RotateF(90, 0, 1, 0);
  GL.Translatef(0, 1, 0);
  BuildCyl;
  GL.Translatef(1, -1, 0);
  BuildCyl;
  BuildFace;
  GL.PopMatrix;

  rci.GLStates.PolygonMode := pmFill;
end;

procedure TGLFile3DSCamera.CoordinateChanged(Sender: TGLCustomCoordinates);
begin
  inherited;

  if (Sender = FTargetPos) or (Sender = Position) then
  begin
    //    Up.AsAffineVector := ZVector;
    //    Direction.SetVector(VectorNormalize(VectorSubtract(FTargetPos.AsAffineVector, Position.AsAffineVector)));
  end;
end;

destructor TGLFile3DSCamera.Destroy;
var
  I: Integer;
begin
  inherited;
  FTargetPos.Free;
  for I := 0 to 1 do
  begin
    gluDeleteQuadric(FQuadCyl[I]);
    gluDeleteQuadric(FQuadDisk[I]);
  end;
end;

procedure TGLFile3DSActor.ReadMesh(Stream: TStream);
var
  virt: TBinaryReader;
begin
  virt := TBinaryReader.Create(Stream);
  MeshOBjects.ReadFromFiler(virt);
  virt.Free;
end;

procedure TGLFile3DSActor.WriteMesh(Stream: TStream);
var
  virt: TBinaryWriter;
begin
  virt := TBinaryWriter.Create(Stream);
  MeshOBjects.WriteToFiler(virt);
  virt.Free;
end;

procedure TGLFile3DSActor.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('MeshObjectsData', ReadMesh, WriteMesh, True);
end;

constructor TGLFile3DSFreeForm.Create(AOWner: TComponent);
begin
  inherited;

  FRefMat := IdentityHmgMatrix;
  FTransfMat := IdentityHmgMatrix;
  FScaleMat := IdentityHmgMatrix;
  FS_Rot3DS := TGLCoordinates4.CreateInitialized(self, VectorMake(1, 0, 0), csVector);
  FRot3DS := TGLCoordinates4.CreateInitialized(self, VectorMake(1, 0, 0), csVector);
  FScale3DS := TGLCoordinates4.CreateInitialized(self, VectorMake(1, 1, 1), csVector);

  ObjectStyle := [osDirectDraw];
end;

destructor TGLFile3DSFreeForm.Destroy;
begin
  FS_Rot3DS.Free;
  FRot3DS.Free;
  FScale3DS.Free;

  inherited;
end;

procedure TGLFile3DSFreeForm.ReadMesh(Stream: TStream);
var
  v: TVector;
  virt: TBinaryReader;
begin
  virt := TBinaryReader.Create(Stream);

  virt.read(FRefMat, sizeof(FRefMat));
  virt.read(v, sizeof(v));
  S_Rot3DS.SetVector(v);
  virt.read(v, sizeof(v));
  Rot3DS.SetVector(v);
  virt.read(v, sizeof(v));
  Scale3DS.SetVector(v);

  MeshOBjects.ReadFromFiler(virt);
  virt.Free;
end;

procedure TGLFile3DSFreeForm.WriteMesh(Stream: TStream);
var
  virt: TBinaryWriter;
  v: TVector;
begin
  virt := TBinaryWriter.Create(Stream);

  virt.write(FRefMat, sizeof(FRefMat));
  v := S_Rot3DS.AsVector;
  virt.write(v, sizeof(v));
  v := Rot3DS.AsVector;
  virt.write(v, sizeof(v));
  v := Scale3DS.AsVector;
  virt.write(v, sizeof(v));

  MeshOBjects.WriteToFiler(virt);
  virt.Free;
end;

procedure TGLFile3DSFreeForm.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('MeshObjectsData', ReadMesh, WriteMesh, True);
end;

procedure TGLFile3DSFreeForm.BuildList(var rci: TGLRenderContextInfo);
begin
  GL.MultMatrixf(@FTransfMat);
  GL.MultMatrixf(@FScaleMat);

  GL.PushMatrix;
  GL.MultMatrixf(@FRefMat);
  inherited;
  GL.PopMatrix;

  if parent is TGLFile3DSFreeForm then
    ParentMatrix := (parent as TGLFile3DSFreeForm).ParentMatrix
  else
    ParentMatrix := IdentityHmgMatrix;

  ParentMatrix := MatrixMultiply(FScaleMat, ParentMatrix);
  ParentMatrix := MatrixMultiply(FTransfMat, ParentMatrix);
end;

procedure TGLFile3DSFreeForm.CoordinateChanged(Sender: TGLCustomCoordinates);
var
  quat, quat1, quat2: TQuaternion;
begin
  inherited;

  if Sender.ClassType = FRot3DS.ClassType then
  begin
    quat1 := MakeRotationQuaternion(FS_Rot3DS.AsAffineVector, FS_Rot3DS.W);
    quat2 := MakeRotationQuaternion(FRot3DS.AsAffineVector, FRot3DS.W);

    quat := QuaternionMultiply(quat1, quat2);
    NormalizeQuaternion(quat);
    FTransfMat := QuaternionToRotateMatrix(quat);
    NormalizeMatrix(FTransfMat);
  end;
  if Sender.ClassType = FScale3DS.ClassType then
  begin
    FScaleMat := CreateScaleMatrix(FScale3DS.AsAffineVector);
  end;
end;

function TGLFile3DSFreeForm.AxisAlignedDimensionsUnscaled: TVector;
var
  dMin, dMax: TAffineVector;
  mat: TMatrix;
begin
  MeshObjects.GetExtents(dMin, dMax);
  mat := ParentMatrix;
  mat := MatrixMultiply(FRefMat, mat);
  if not IsInfinite(dMin.V[0]) then
    dMin := VectorTransform(dMin, mat);
  if not IsInfinite(dMax.V[0]) then
    dMax := VectorTransform(dMax, mat);

  Result.V[0] := (dMax.V[0] - dMin.V[0]) / 2;
  Result.V[1] := (dMax.V[1] - dMin.V[1]) / 2;
  Result.V[2] := (dMax.V[2] - dMin.V[2]) / 2;
  Result.V[3] := 0;
end;

// BarycenterAbsolutePosition
//

function TGLFile3DSFreeForm.BarycenterAbsolutePosition: TVector;
var
  dMin, dMax: TAffineVector;
  mat: TMatrix;
begin
  MeshObjects.GetExtents(dMin, dMax);
  mat := ParentMatrix;
  mat := MatrixMultiply(FRefMat, mat);
  if not IsInfinite(dMin.V[0]) then
    dMin := VectorTransform(dMin, mat);
  if not IsInfinite(dMax.V[0]) then
    dMax := VectorTransform(dMax, mat);

  Result.V[0] := (dMax.V[0] + dMin.V[0]) / 2;
  Result.V[1] := (dMax.V[1] + dMin.V[1]) / 2;
  Result.V[2] := (dMax.V[2] + dMin.V[2]) / 2;
  Result.V[3] := 1;

  Result := LocalToAbsolute(Result);
end;

initialization
  RegisterClasses([TGLFile3DSLight, TGLFile3DSCamera, TGLFile3DSActor, TGLFile3DSFreeForm]);

end.

