//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  Class for managing a ROAM (square) patch.

   History :  
   29/12/14 - PW - Fixed SafeTesselation function that caused gaps between tiles
   22/08/10 - DaStr - Fixed compiler warning
   27/07/10 - YP - Safe tesselation operation to avoid AV after a memory shift
   26/07/10 - YP - Invalid range test when splitting, we need to check space for n and n+1
   20/05/10 - Yar - Fixes for Linux x64
   16/10/08 - UweR - Compatibility fix for Delphi 2009
   30/03/07 - DaStr - Added $I GLScene.inc
   19/10/06 - LC - Added code to gracefully handle the case when MaxCLODTriangles is reached.
  It will now increase the buffer instead of not splitting. Bugtracker ID=1574111
   09/10/06 - Lin - Added OnMaxCLODTrianglesReached event.
   09/06/06 - Lin - Bugfix: Stop splitting Triangles when MaxCLODTriangles is reached (prevents Access Violations)
   10/06/05 - Mathx - Protection against cards that have GL_EXT_compiled_vertex_array
  but not GL_EXT_draw_range_elements
   25/04/04 - EG - Occlusion testing support
   06/02/03 - EG - Adaptative variance computation
   03/12/02 - EG - Minor ROAM tessel/render optimizations
   15/06/02 - EG - Fixed patch rendering bug "introduced" by TBaseList fix
   24/02/02 - EG - Hybrid ROAM-stripifier engine
   10/09/01 - EG - Creation
   
}
unit GLROAMPatch;

interface

{$I GLScene.inc}

uses
  SysUtils,
  GLVectorGeometry, GLHeightData, GLVectorLists, GLCrossPlatform, GLContext,
  OpenGLTokens, XOpenGL;

type

  // Exception use by Split for SafeTesselate
  EGLROAMException = class(Exception);

  // TROAMTriangleNode
  //
  PROAMTriangleNode = ^TROAMTriangleNode;

  TROAMTriangleNode = packed record
    base, left, right: PROAMTriangleNode;
    leftChild, rightChild: PROAMTriangleNode;
  end;

  // TROAMRenderPoint
  //
  TROAMRenderPoint = packed record
    X, Y: Integer;
    idx: Integer;
  end;

  // TGLROAMPatch
  //
  TGLROAMPatch = class(TObject)
  private
     
    FID: Integer;
    FHeightData: TGLHeightData; // Referred, not owned
    FHeightRaster: PSmallIntRaster;
    FTLNode, FBRNode: Integer;
    FTLVariance, FBRVariance: array of cardinal;
    FPatchSize, FTriangleCount: Integer;
    FListHandle: TGLListHandle;
    FTag: Integer;
    FObserverPosition: TAffineVector;
    FNorth, FSouth, FWest, FEast: TGLROAMPatch; // neighbours
    FHighRes: Boolean;
    FMaxDepth: Integer;
    FVertexScale, FVertexOffset: TAffineVector;
    FTextureScale, FTextureOffset: TAffineVector;
    FMaxTLVarianceDepth, FMaxBRVarianceDepth: Integer;

    FOcclusionQuery: TGLOcclusionQueryHandle;
    FOcclusionSkip, FOcclusionCounter: Integer;
    FLastOcclusionTestPassed: Boolean;

  protected
     
    procedure SeTGLHeightData(val: TGLHeightData);
    procedure SetOcclusionSkip(val: Integer);

    procedure RenderROAM(vertices: TAffineVectorList;
      vertexIndices: TIntegerList; texCoords: TTexPointList);
    procedure RenderAsStrips(vertices: TAffineVectorList;
      vertexIndices: TIntegerList; texCoords: TTexPointList);

    function Tesselate: boolean;
    // Returns false if MaxCLODTriangles limit is reached(Lin)
  public
     
    constructor Create;
    destructor Destroy; override;

    procedure ComputeVariance(variance: Integer);

    procedure ResetTessellation;
    procedure ConnectToTheWest(westPatch: TGLROAMPatch);
    procedure ConnectToTheNorth(northPatch: TGLROAMPatch);

    { : AV free version of Tesselate.
      When IncreaseTrianglesCapacity is called, all PROAMTriangleNode
      values in higher function became invalid due to the memory shifting.
      Recursivity is the main problem, that's why SafeTesselate is calling
      Tesselate in a try..except . }
    function SafeTesselate: boolean;

    { : Render the patch in high-resolution.
      The lists are assumed to have enough capacity to allow AddNC calls
      (additions without capacity check). High-resolution renders use
      display lists, and are assumed to be made together. }
    procedure RenderHighRes(vertices: TAffineVectorList;
      vertexIndices: TIntegerList; texCoords: TTexPointList;
      forceROAM: Boolean);
    { : Render the patch by accumulating triangles.
      The lists are assumed to have enough capacity to allow AddNC calls
      (additions without capacity check). 
      Once at least autoFlushVertexCount vertices have been accumulated,
      perform a FlushAccum }
    procedure RenderAccum(vertices: TAffineVectorList;
      vertexIndices: TIntegerList; texCoords: TTexPointList;
      autoFlushVertexCount: Integer);
    { : Render all vertices accumulated in the arrays and set their count
      back to zero. }
    class procedure FlushAccum(vertices: TAffineVectorList;
      vertexIndices: TIntegerList; texCoords: TTexPointList);

    property HeightData: TGLHeightData read FHeightData write SeTGLHeightData;
    property VertexScale: TAffineVector read FVertexScale write FVertexScale;
    property VertexOffset: TAffineVector read FVertexOffset write FVertexOffset;

    property ObserverPosition: TAffineVector read FObserverPosition
      write FObserverPosition;

    property TextureScale: TAffineVector read FTextureScale write FTextureScale;
    property TextureOffset: TAffineVector read FTextureOffset
      write FTextureOffset;

    property HighRes: Boolean read FHighRes write FHighRes;

    { : Number of frames to skip after an occlusion test returned zero pixels. }
    property OcclusionSkip: Integer read FOcclusionSkip write SetOcclusionSkip;
    { : Number of frames remaining to next occlusion test. }
    property OcclusionCounter: Integer read FOcclusionCounter
      write FOcclusionCounter;
    { : Result for the last occlusion test.
      Note that this value is updated upon rendering the tile in
      non-high-res mode only. }
    property LastOcclusionTestPassed: Boolean read FLastOcclusionTestPassed;

    property ID: Integer read FID;
    property TriangleCount: Integer read FTriangleCount;
    property Tag: Integer read FTag write FTag;
  end;

  { : Specifies the maximum number of ROAM triangles that may be allocated. }
procedure SetROAMTrianglesCapacity(nb: Integer);
function GetROAMTrianglesCapacity: Integer;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
  FVBOVertHandle, FVBOTexHandle: TGLVBOArrayBufferHandle;
  FVBOIndicesHandle: TGLVBOElementArrayHandle;

type

  // TROAMVariancePoint
  //
  TROAMVariancePoint = packed record
    X, Y: Integer;
    Z: Integer;
  end;

var
  vNextPatchID: Integer;
  vNbTris, vTriangleNodesCapacity: Integer;
  vTriangleNodes: array of TROAMTriangleNode;

  // SetROAMTrianglesCapacity

procedure SetROAMTrianglesCapacity(nb: Integer);
begin
  vNbTris := 0;
  if vTriangleNodesCapacity <> nb then
  begin
    SetLength(vTriangleNodes, nb);
    vTriangleNodesCapacity := nb;
  end;
end;

function GetROAMTrianglesCapacity: Integer;
begin
  Result := vTriangleNodesCapacity;
end;

// The result is the delta between the old address of the array and the new one
function IncreaseTrianglesCapacity(NewCapacity: Integer): int64;

  procedure FixNodePtr(var p: PROAMTriangleNode; const delta: int64);
  begin
    if p = nil then
      exit;

    Inc(PByte(p), delta);
  end;

var
  oldbase, newbase: pointer;
  node: PROAMTriangleNode;
  i, oldsize: Integer;
begin
  Result := 0;
  if NewCapacity <= vTriangleNodesCapacity then
    exit;

  oldsize := vTriangleNodesCapacity;

  oldbase := pointer(vTriangleNodes);
  SetLength(vTriangleNodes, NewCapacity);

  vTriangleNodesCapacity := NewCapacity;

  newbase := pointer(vTriangleNodes);

  // Array has not been relocated, no need to fix
  if oldbase = newbase then
    exit;

  // go through all the old nodes and fix the pointers
  // YP: Delphi needs int64 dual casting to avoid overflow exception
  Result := int64(PtrUInt(newbase)) - int64(PtrUInt(oldbase));
  for i := 0 to oldsize - 1 do
  begin
    node := @vTriangleNodes[i];

    FixNodePtr(node^.base, Result);
    FixNodePtr(node^.left, Result);
    FixNodePtr(node^.right, Result);
    FixNodePtr(node^.leftChild, Result);
    FixNodePtr(node^.rightChild, Result);
  end;
end;

// AllocTriangleNode
//
function AllocTriangleNode: Integer;
var
  nilNode: PROAMTriangleNode;
begin
  if vNbTris >= vTriangleNodesCapacity then
  begin
    // grow by 50%
    IncreaseTrianglesCapacity(vTriangleNodesCapacity +
      (vTriangleNodesCapacity shr 1));
  end;
  Result := vNbTris;
  with vTriangleNodes[vNbTris] do
  begin
    nilNode := nil;
    left := nilNode;
    right := nilNode;
    leftChild := nilNode;
    rightChild := nilNode;
  end;
  Inc(vNbTris);
end;

// Split
//
function Split(tri: PROAMTriangleNode): Boolean;
var
  n: Integer;
  lc, rc: PROAMTriangleNode;
  Shift: int64;
begin
  Result := Assigned(tri.leftChild);
  if Result then
    exit; // dont split if tri already has a left child
  with tri^ do 
  begin 
    if Assigned(base) and (base.base <> tri) then
	  Split(base);
   // If this triangle is not in a proper diamond, force split our base neighbor
    n := vNbTris;
  end;

  if n >= vTriangleNodesCapacity - 1 then
  begin
    // grow by 50%
    Shift := IncreaseTrianglesCapacity(vTriangleNodesCapacity +
      (vTriangleNodesCapacity shr 1));
    if Shift <> 0 then
    begin
      raise EGLROAMException.Create
        ('PROAMTriangleNode addresses are invalid now');
    end;
  end;

  with tri^ do
  begin

    // Create children and cross-link them
    lc := @vTriangleNodes[n]; // left child
    rc := @vTriangleNodes[n + 1]; // right child

    leftChild := lc;
    rightChild := rc;

    rc.base := right; // right child
    rc.leftChild := nil;
    rc.rightChild := leftChild;
    rc.right := leftChild;

    lc.base := left; // left child
    lc.leftChild := nil;
    lc.rightChild := leftChild;
    lc.left := rightChild;

    Inc(vNbTris, 2);

    if Assigned(left) then // Link our Left Neighbour to the new children
      if left.base = tri then
        left.base := lc
      else if left.left = tri then
        left.left := lc
      else
        left.right := lc;
    if Assigned(right) then // Link our Right Neighbour to the new children
      if right.base = tri then
        right.base := rc
      else if right.left = tri then
        right.left := rc
      else
        right.right := rc;
    // Link our Base Neighbor to the new children
    if Assigned(base) then
    begin
      if Assigned(base.leftChild) then
      begin
        base.leftChild.right := rightChild;
        rightChild.left := base.leftChild;
        base.rightChild.left := leftChild;
        leftChild.right := base.rightChild;
      end
      else
        Split(base);
    end
    else
    begin // An edge triangle, trivial case.
      leftChild.right := nil;
      rightChild.left := nil;
    end;
  end;
  Result := True;
end;


// ------------------
// ------------------ TGLROAMPatch ------------------
// ------------------

// Create
//
constructor TGLROAMPatch.Create;
begin
  inherited Create;
  FID := vNextPatchID;
  Inc(vNextPatchID);
  FListHandle := TGLListHandle.Create;
  FOcclusionQuery := TGLOcclusionQueryHandle.Create;
end;

// Destroy
//
destructor TGLROAMPatch.Destroy;
begin
  FListHandle.Free;
  FOcclusionQuery.Free;
  inherited Destroy;
end;

// SeTGLHeightData
//
procedure TGLROAMPatch.SeTGLHeightData(val: TGLHeightData);
begin
  FHeightData := val;
  FPatchSize := FHeightData.Size - 1;
  FHeightRaster := val.SmallIntRaster;
end;

// SetOcclusionSkip
//
procedure TGLROAMPatch.SetOcclusionSkip(val: Integer);
begin
  if val < 0 then
    val := 0;
  if FOcclusionSkip <> val then
  begin
    FOcclusionSkip := val;
    FOcclusionQuery.DestroyHandle;
  end;
end;

// ConnectToTheWest
//
procedure TGLROAMPatch.ConnectToTheWest(westPatch: TGLROAMPatch);
begin
  if Assigned(westPatch) then
  begin
    if not(westPatch.HighRes or HighRes) then
    begin
      vTriangleNodes[FTLNode].left := @vTriangleNodes[westPatch.FBRNode];
      vTriangleNodes[westPatch.FBRNode].left := @vTriangleNodes[FTLNode];
    end;
    FWest := westPatch;
    westPatch.FEast := Self;
  end;
end;

// ConnectToTheNorth
//
procedure TGLROAMPatch.ConnectToTheNorth(northPatch: TGLROAMPatch);
begin
  if Assigned(northPatch) then
  begin
    if not(northPatch.HighRes or HighRes) then
    begin
      vTriangleNodes[FTLNode].right := @vTriangleNodes[northPatch.FBRNode];
      vTriangleNodes[northPatch.FBRNode].right := @vTriangleNodes[FTLNode];
    end;
    FNorth := northPatch;
    northPatch.FSouth := Self;
  end;
end;

// ComputeVariance
//
procedure TGLROAMPatch.ComputeVariance(variance: Integer);
var
  raster: PSmallIntRaster;
  currentVariance: PIntegerArray;
  maxVarianceDepth: Integer;
  maxNonNullIndex: Integer;
  invVariance: Single;

  function ROAMVariancePoint(anX, anY: Integer): TROAMVariancePoint;
  begin
    Result.X := anX;
    Result.Y := anY;
    Result.Z := (Integer(FHeightRaster[anY][anX]) shl 8);
  end;

  function RecursComputeVariance(const left, right, apex: TROAMVariancePoint;
    node: Integer): Cardinal;
  var
    half: TROAMVariancePoint;
    v: Cardinal;
    n2: Integer;
  begin
    with half do
    begin
      X := (left.X + right.X) shr 1;
      Y := (left.Y + right.Y) shr 1;
      Z := Integer(raster[Y][X]) shl 8;
      Result := ScaleAndRound(Abs(((left.Z + right.Z) div 2) - Z), invVariance);
    end;

    n2 := node shl 1;
    if n2 < maxVarianceDepth then
    begin
      v := RecursComputeVariance(apex, left, half, n2);
      if v > Result then
        Result := v;
      v := RecursComputeVariance(right, apex, half, 1 + n2);
      if v > Result then
        Result := v;
    end;
    currentVariance[node] := Result;
  end;

  procedure ScaleVariance(n, d: Integer);
  var
    newVal: Integer;
  begin
    if d >= 0 then
      newVal := (currentVariance[n] shl (d shr 1))
    else
      newVal := (currentVariance[n] shr (-d shr 1));
    currentVariance[n] := newVal;
    if newVal > 0 then
      if n > maxNonNullIndex then
        maxNonNullIndex := n;
    n := n shl 1;
    if n < maxVarianceDepth then
    begin
      Dec(d);
      ScaleVariance(n, d);
      ScaleVariance(n + 1, d);
    end;
  end;

var
  s, p: Integer;
begin
  invVariance := 1 / variance;
  s := Sqr(FPatchSize);
  raster := FHeightRaster;
  FMaxDepth := 1;
  p := -1 - 8;
  repeat
    FMaxDepth := FMaxDepth shl 2;
    Inc(p);
  until FMaxDepth >= s;
  maxVarianceDepth := FMaxDepth;
  SetLength(FTLVariance, maxVarianceDepth);
  SetLength(FBRVariance, maxVarianceDepth);

  s := FPatchSize;
  currentVariance := @FTLVariance[0];
  maxNonNullIndex := 1;
  RecursComputeVariance(ROAMVariancePoint(0, s), ROAMVariancePoint(s, 0),
    ROAMVariancePoint(0, 0), 1);
  ScaleVariance(1, p);
  FMaxTLVarianceDepth := maxNonNullIndex + 1;
  SetLength(FTLVariance, FMaxTLVarianceDepth);
  currentVariance := @FBRVariance[0];
  maxNonNullIndex := 1;
  RecursComputeVariance(ROAMVariancePoint(s, 0), ROAMVariancePoint(0, s),
    ROAMVariancePoint(s, s), 1);
  ScaleVariance(1, p);
  FMaxBRVarianceDepth := maxNonNullIndex + 1;
  SetLength(FBRVariance, FMaxBRVarianceDepth);
end;

// ResetTessellation
//
procedure TGLROAMPatch.ResetTessellation;
begin
  FTLNode := AllocTriangleNode;
  FBRNode := AllocTriangleNode;
  vTriangleNodes[FTLNode].base := @vTriangleNodes[FBRNode];
  vTriangleNodes[FBRNode].base := @vTriangleNodes[FTLNode];
  FNorth := nil;
  FSouth := nil;
  FWest := nil;
  FEast := nil;
end;

// Tessellate
//
var
  tessMaxVariance: Cardinal;
  tessMaxDepth: Cardinal;
  tessCurrentVariance: PIntegerArray;
  tessObserverPosX, tessObserverPosY: Integer;

function RecursTessellate(tri: PROAMTriangleNode; n: cardinal;
  const left, right, apex: cardinal): boolean;
// returns false if tessellation failed due to MaxCLODTriangles limit
var
  d: Integer;
begin
  Result := True;
  d := ((left + right) shr 1);
  if tessCurrentVariance[n] > d then
  begin
    Result := False;
    if Split(tri) then
    begin
      n := n shl 1;
      if n < tessMaxVariance then
      begin
        RecursTessellate(tri.leftChild, n, apex, left, d);
        Result := RecursTessellate(tri.rightChild, n + 1, right, apex, d);
      end;
    end;
  end;
end;

function TGLROAMPatch.Tesselate: boolean;
// Returns false if MaxCLODTriangles limit is reached.
var
  tessFrameVarianceDelta: Integer;

  function VertexDist(X, Y: Integer): cardinal;
  var
    f: Single;
  const
    c1Div100: Single = 0.01;
  begin
    if HighRes then
      f := 0.2 * Sqr(FPatchSize)
    else
      f := Sqr(X - tessObserverPosX) + Sqr(Y - tessObserverPosY) +
        tessFrameVarianceDelta;
    Result := Round(Sqrt(f) + f * c1Div100);
  end;

procedure FullBaseTess(tri: PROAMTriangleNode; n: Cardinal); forward;

  procedure FullLeftTess(tri: PROAMTriangleNode; n: Cardinal);
  begin
    if Split(tri) then
    begin
      n := n shl 1;
      if n < tessMaxDepth then
        FullBaseTess(tri.leftChild, n);
    end;
  end;

  procedure FullRightTess(tri: PROAMTriangleNode; n: Cardinal);
  begin
    if Split(tri) then
    begin
      n := n shl 1;
      if n < tessMaxDepth then
        FullBaseTess(tri.rightChild, n);
    end;
  end;

  procedure FullBaseTess(tri: PROAMTriangleNode; n: Cardinal);
  begin
    if Split(tri) then
    begin
      n := n shl 1;
      if n < tessMaxDepth then
      begin
        FullRightTess(tri.leftChild, n);
        FullLeftTess(tri.rightChild, n);
      end;
    end;
  end;

var
  s: Integer;
begin
  tessMaxDepth := FMaxDepth;
  tessObserverPosX := Round(FObserverPosition.X);
  tessObserverPosY := Round(FObserverPosition.Y);

  if HighRes then
  begin
    FullRightTess(@vTriangleNodes[FTLNode], 1);
    FullRightTess(@vTriangleNodes[FBRNode], 1);
    FullLeftTess(@vTriangleNodes[FBRNode], 1);
    FullLeftTess(@vTriangleNodes[FTLNode], 1);
    tessFrameVarianceDelta := 0;
  end
  else
  begin
    if Assigned(FNorth) and FNorth.HighRes then
      FullRightTess(@vTriangleNodes[FTLNode], 1);
    if Assigned(FSouth) and FSouth.HighRes then
      FullRightTess(@vTriangleNodes[FBRNode], 1);
    if Assigned(FEast) and FEast.HighRes then
      FullLeftTess(@vTriangleNodes[FBRNode], 1);
    if Assigned(FWest) and FWest.HighRes then
      FullLeftTess(@vTriangleNodes[FTLNode], 1);
    if FObserverPosition.v[2] > 0 then
      tessFrameVarianceDelta := Round(Sqr(FObserverPosition.Z * (1 / 16)))
    else
      tessFrameVarianceDelta := 0;
  end;
  s := FPatchSize;
  tessCurrentVariance := @FTLVariance[0];
  tessMaxVariance := FMaxTLVarianceDepth;
  Result := RecursTessellate(@vTriangleNodes[FTLNode], 1, VertexDist(0, s),
    VertexDist(s, 0), VertexDist(0, 0));
  tessCurrentVariance := @FBRVariance[0];
  tessMaxVariance := FMaxBRVarianceDepth;
  if Result then
    Result := RecursTessellate(@vTriangleNodes[FBRNode], 1, VertexDist(s, 0),
      VertexDist(0, s), VertexDist(s, s));
end;


// SafeTesselate

function TGLROAMPatch.SafeTesselate: boolean;
var
  Fail: boolean;
begin
  Result := False;
  Fail := True;
  repeat
    try
      //ResetTessellation; <- creates gaps between tiles
      Result := Tesselate;
      Fail := False;
    except
      on e: EGLROAMException do
      begin
        // Nothing to do, just wait the next iteration
        Fail := True;
      end;
    end;
  until not Fail;
end;

// RenderHighRes
//
procedure TGLROAMPatch.RenderHighRes(vertices: TAffineVectorList;
  vertexIndices: TIntegerList; texCoords: TTexPointList; forceROAM: Boolean);
var
  primitive: TGLEnum;
begin
  // Prepare display list if needed
  if FListHandle.Handle = 0 then
  begin
    // either use brute-force strips or a high-res static tesselation
    if forceROAM then
    begin
      SafeTesselate;
      RenderROAM(vertices, vertexIndices, texCoords);
      primitive := GL_TRIANGLES;
      FTriangleCount := vertexIndices.Count div 3;
    end
    else
    begin
      RenderAsStrips(vertices, vertexIndices, texCoords);
      primitive := GL_TRIANGLE_STRIP;
      FTriangleCount := vertexIndices.Count - 2 * FPatchSize;
    end;

    vertices.Translate(VertexOffset);
    texCoords.ScaleAndTranslate(PTexPoint(@TextureScale)^,
      PTexPoint(@TextureOffset)^);

    GL.VertexPointer(3, GL_FLOAT, 0, vertices.List);
    xgl.TexCoordPointer(2, GL_FLOAT, 0, texCoords.List);

    FListHandle.AllocateHandle;
    GL.NewList(FListHandle.Handle, GL_COMPILE);
    GL.DrawElements(primitive, vertexIndices.Count, GL_UNSIGNED_INT,
      vertexIndices.List);
    GL.EndList;

    vertices.Count := 0;
    texCoords.Count := 0;
    vertexIndices.Count := 0;
  end;
  // perform the render
  GL.CallList(FListHandle.Handle);
end;

// RenderAccum
//
procedure TGLROAMPatch.RenderAccum(vertices: TAffineVectorList;
  vertexIndices: TIntegerList; texCoords: TTexPointList;
  autoFlushVertexCount: Integer);
var
  occlusionPassed: Boolean;
  n, nb, nvi: Integer;
begin
  // CLOD tiles are rendered via ROAM
  if (FOcclusionSkip > 0) and FOcclusionQuery.IsSupported then
  begin
    if FOcclusionQuery.Handle = 0 then
    begin
      FOcclusionQuery.AllocateHandle;
      FOcclusionCounter := -(ID mod (FOcclusionSkip));
    end;
    occlusionPassed := (FOcclusionCounter <= 0) or
      (FOcclusionQuery.PixelCount > 0);
    Dec(FOcclusionCounter);
    if occlusionPassed then
    begin
      if FOcclusionCounter <= 0 then
        Inc(FOcclusionCounter, FOcclusionSkip);
      FOcclusionQuery.BeginQuery;
    end;
  end
  else
    occlusionPassed := True;
  FLastOcclusionTestPassed := occlusionPassed;
  if occlusionPassed then
  begin
    nvi := vertexIndices.Count;
    n := vertices.Count;
    RenderROAM(vertices, vertexIndices, texCoords);
    nb := vertices.Count - n;
    FTriangleCount := (vertexIndices.Count - nvi) div 3;

    vertices.Translate(VertexOffset, n, nb);
    texCoords.ScaleAndTranslate(PTexPoint(@TextureScale)^,
      PTexPoint(@TextureOffset)^, n, nb);

    if FOcclusionQuery.Active then
    begin
      FlushAccum(vertices, vertexIndices, texCoords);
      FOcclusionQuery.EndQuery;
    end
    else if vertexIndices.Count > autoFlushVertexCount then
      FlushAccum(vertices, vertexIndices, texCoords);
  end
  else
    FTriangleCount := 0;
end;

// FlushAccum
//
class procedure TGLROAMPatch.FlushAccum(vertices: TAffineVectorList;
  vertexIndices: TIntegerList; texCoords: TTexPointList);
begin
  if vertexIndices.Count = 0 then
    Exit;

  if GL.ARB_vertex_buffer_object then
  begin
    FVBOVertHandle.AllocateHandle;
    FVBOVertHandle.BindBufferData(vertices.List, vertices.DataSize,
      GL_STREAM_DRAW_ARB);
    GL.VertexPointer(3, GL_FLOAT, 0, nil);

    FVBOTexHandle.AllocateHandle;
    FVBOTexHandle.BindBufferData(texCoords.List, texCoords.DataSize,
      GL_STREAM_DRAW_ARB);
    xgl.TexCoordPointer(2, GL_FLOAT, 0, nil);

    GL.DrawRangeElements(GL_TRIANGLES, 0, vertices.Count - 1,
      vertexIndices.Count, GL_UNSIGNED_INT, vertexIndices.List);
    GL.BindBuffer(GL_ARRAY_BUFFER_ARB, 0);
    GL.BindBuffer(GL_ELEMENT_ARRAY_BUFFER_ARB, 0);
  end
  else if GL.EXT_compiled_vertex_array and GL.EXT_draw_range_elements then
  begin
    GL.LockArrays(0, vertices.Count);
    GL.DrawRangeElements(GL_TRIANGLES, 0, vertices.Count - 1,
      vertexIndices.Count, GL_UNSIGNED_INT, vertexIndices.List);
    GL.UnLockArrays;
  end
  else
  begin
    GL.DrawElements(GL_TRIANGLES, vertexIndices.Count, GL_UNSIGNED_INT,
      vertexIndices.List);
  end;
  vertices.Count := 0;
  texCoords.Count := 0;
  vertexIndices.Count := 0;
end;

// RenderROAM
//
var
  renderRaster: PSmallIntRaster;
  renderIndices: PIntegerArray;
  renderVertices: TAffineVectorList;
  renderTexCoords: TTexPointList;

procedure RecursRender(const tri: PROAMTriangleNode;
  const left, right, apex: TROAMRenderPoint);
var
  half: TROAMRenderPoint;
  localIndices: PIntegerArray;
begin
  if Assigned(tri.leftChild) then
  begin // = if node is split
    half.Y := (left.Y + right.Y) shr 1;
    half.X := (left.X + right.X) shr 1;
    renderTexCoords.AddNC(@half.X);
    half.idx := renderVertices.AddNC(@half.X, renderRaster[half.Y][half.X]);
    RecursRender(tri.leftChild, apex, left, half);
    RecursRender(tri.rightChild, right, apex, half);
  end
  else
  begin
    localIndices := renderIndices;
    localIndices[0] := left.idx;
    localIndices[1] := apex.idx;
    localIndices[2] := right.idx;
    renderIndices := PIntegerArray(@localIndices[3]);
  end;
end;

procedure TGLROAMPatch.RenderROAM(vertices: TAffineVectorList;
  vertexIndices: TIntegerList; texCoords: TTexPointList);

  procedure ROAMRenderPoint(var p: TROAMRenderPoint; anX, anY: Integer);
  begin
    p.X := anX;
    p.Y := anY;
    p.idx := vertices.Add(anX, anY, renderRaster[anY][anX]);
    texCoords.Add(anX, anY);
  end;

var
  rtl, rtr, rbl, rbr: TROAMRenderPoint;
begin
  renderVertices := vertices;
  renderTexCoords := texCoords;
  vertexIndices.AdjustCapacityToAtLeast(Sqr(FPatchSize) * 6 + 15000);
  // this is required, the actual item count is maintained out of the list scope
  vertexIndices.SetCountResetsMemory := False;
  renderIndices := @vertexIndices.List[vertexIndices.Count];

  renderRaster := FHeightData.SmallIntRaster;

  ROAMRenderPoint(rtl, 0, 0);
  ROAMRenderPoint(rtr, FPatchSize, 0);
  ROAMRenderPoint(rbl, 0, FPatchSize);
  ROAMRenderPoint(rbr, FPatchSize, FPatchSize);

  RecursRender(@vTriangleNodes[FTLNode], rbl, rtr, rtl);
  RecursRender(@vTriangleNodes[FBRNode], rtr, rbl, rbr);

  vertexIndices.Count := (PtrUInt(renderIndices) - PtrUInt(vertexIndices.List))
    div SizeOf(Integer);
end;

// RenderAsStrips
//
procedure TGLROAMPatch.RenderAsStrips(vertices: TAffineVectorList;
  vertexIndices: TIntegerList; texCoords: TTexPointList);

var
  X, Y, baseTop, rowLength: Integer;
  p: TAffineVector;
  row: PSmallIntArray;
  raster: PSmallIntRaster;
  tex: TTexPoint;
  verticesList: PAffineVector;
  texCoordsList: PTexPoint;
  indicesList: PInteger;
begin
  raster := FHeightData.SmallIntRaster;
  rowLength := FPatchSize + 1;
  // prepare vertex data
  vertices.Count := Sqr(rowLength);
  verticesList := PAffineVector(vertices.List);
  texCoords.Count := Sqr(rowLength);
  texCoordsList := PTexPoint(texCoords.List);
  for Y := 0 to FPatchSize do
  begin
    p.Y := Y;
    tex.T := p.Y;
    row := raster[Y];
    for X := 0 to FPatchSize do
    begin
      p.X := X;
      tex.s := p.X;
      p.Z := row[X];
      verticesList^ := p;
      Inc(verticesList);
      texCoordsList^ := tex;
      Inc(texCoordsList);
    end;
  end;
  // build indices list
  baseTop := 0;
  vertexIndices.Count := (rowLength * 2 + 2) * FPatchSize - 1;
  indicesList := PInteger(vertexIndices.List);
  Y := 0;
  while Y < FPatchSize do
  begin
    if Y > 0 then
    begin
      indicesList^ := baseTop + FPatchSize;
      Inc(indicesList);
    end;
    for X := baseTop + FPatchSize downto baseTop do
    begin
      indicesList^ := X;
      PIntegerArray(indicesList)[1] := rowLength + X;
      Inc(indicesList, 2);
    end;
    indicesList^ := baseTop + rowLength;
    Inc(baseTop, rowLength);
    PIntegerArray(indicesList)[1] := baseTop + rowLength;
    Inc(indicesList, 2);
    for X := baseTop to baseTop + FPatchSize do
    begin
      indicesList^ := rowLength + X;
      PIntegerArray(indicesList)[1] := X;
      Inc(indicesList, 2);
    end;
    indicesList^ := baseTop + FPatchSize;
    Inc(indicesList);
    Inc(baseTop, rowLength);
    Inc(Y, 2);
  end;
  vertexIndices.Count := vertexIndices.Count - 1;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

FVBOVertHandle := TGLVBOArrayBufferHandle.Create;
FVBOTexHandle := TGLVBOArrayBufferHandle.Create;
FVBOIndicesHandle := TGLVBOElementArrayHandle.Create;

finalization

FVBOVertHandle.Free;
FVBOTexHandle.Free;
FVBOIndicesHandle.Free;

SetROAMTrianglesCapacity(0);

end.
