unit GLRevolutionMeshObject;
{
   TGLRevolutionMeshObject

    A class used to build revolution onjects with features, such as shafts /
    columms, or with some work freeform.

    Each section leaves an index value to the last ring of Vertices made.
    Callable routines should have the option to use previous ring, mainly
    for use with rounded features where the normals are the same for both
    edges of the feature.

    If previous ring is not used at a given Z then dup points will be
    generated, generally with a differing normal, which creates a hard edge.
    Hopefully this will create a topologically correct mesh, with minimum
    number of vertices.

    Setting  FaceGroups[0]).Mode to fgmmQuads will generate quad faces.
    The default is triangular faces.

    Mesh density can be radially increased/decreased through
    MakeTransitionFiner and MakeTransitionCoarser. This is a stateful change
    which will keep the new density until another call is made.
    NOTE: mesh density changing can only be used with triangular meshes.

    Revolutions should be built from the 'top' down. Top for solid rotations
    means highest point that meets the Z axis. See Drilled hole example.
    For shelled ojects such as a tube, top is the lowest Z point and the inside
    of the shelled rotation is generated first.

    Edges are stored with an index to the first ring and the ring size.
    This ring and the next ring should have congruent vertices. But this will
    allow only one ring to be used if we need to collapse the edges for export,
    reducing the risk of holes at the edge vertices. Plus we can dispay them
    for debugging :) where we forget to set a UsePrevious to True.

    TODO
      Add in Tex coords obviously only one is needed as the conic will
      use either u or v for the wrap round, and the other specified in the call
      to build the section? More thought required but basically not of interest
      for my immediate needs.



    Optimisation Techniques Used.

    Sine and cosine calls removed from any inner loop by using predefined
      x,y 2D unit vectors.

    Z components and normals are optimised by taking into account the fact that
      any affine vector rotated around the Z axis will have the same z value.
      So if we calculate that once in 2D space on a major axis, the x.z plane
      for instance, the y component of the 2D unit vector becomes the z part
      of the unit 3D vector and the x component becomes a scaling factor to
      use with the predefined xy unit vector array.

    Added bonus to the above approach is we are calculating unit vector
    normals. No need to call normalise on the Normals saving a sqrt call
    per vertex.

 (c) Copyright 2017 Peter Dyson. dicepd on Lazarus forum.
}
{$DEFINE STANDALONE}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  GLScene, OpenGLTokens, GLVectorTypes, GLVectorGeometry, GLTexture,
  GLMaterial, GLMesh, GLVectorLists, GLPersistentClasses,  GLContext, GLColor,
  GLCoordinates, GLObjects, GLVectorFileObjects;

const
  vAxisX: TAffineVector = (X: 1; Y: 0; Z: 0);
  vAxisY: TAffineVector = (X: 0; Y: 1; Z: 0);
  vAxisZ: TAffineVector = (X: 0; Y: 0; Z: 1);

  vNegAxisX: TAffineVector = (X: -1; Y: 0; Z: 0);
  vNegAxisY: TAffineVector = (X: 0; Y: -1; Z: 0);
  vNegAxisZ: TAffineVector = (X: 0; Y: 0; Z: -1);


type

   TFilletQuadrant = (tfqNE, tfqSE, tfqSW, tfqNW);
   TSectorResolution = (tsrStandard, tsrDouble, tsrQuad);

   TRevEdge = record
     VertexIndex: integer;
     VertexCount: integer;
   end;

   // could use generic here but not sure how portable.
   TRevEdgeArray = array of TRevEdge;

   { TGLRevolutionMeshObject }

   TGLRevolutionMeshObject = class(TGLMeshObject)
   private
     fUsesDoubleMesh: boolean;
     fUsesQuadMesh: boolean;
     fEdges: TRevEdgeArray;
     fEdgeCount: Integer;
     fEdgeCapacity: Integer;
     StandardSinArray: array of single;
     StandardCosArray: array of single;
     DoubleSinArray: array of single;
     DoubleCosArray: array of single;
     QuadSinArray: array of single;
     QuadCosArray: array of single;

     procedure SetSectors(AValue: integer);
     procedure SetUsesDoubleMesh(AValue: boolean);
     procedure SetUsesQuadMesh(AValue: boolean);
     procedure InitSectors;
     procedure AddEdge(RingIndex, Sectors: integer);  // we only need to add edges.
   protected
     FFaceList: TFGVertexIndexList;
     fSectors: integer;
     fReqSectors: integer;
     fTopoFaces: integer;
     fCurrentResolution: TSectorResolution;
     fPreviousRing: integer;
     SinArray: array of single;
     CosArray: array of single;
   public
     constructor Create; override;
     destructor Destroy; override;

     procedure MakeTip(Radius, Z, TipZ: single; IsUp, UsePrevious: boolean);
     procedure MakeConicSection(RadiusT, RadiusB, TZ, BZ: single;
                                slices: integer; UsePrevious: boolean);
     procedure MakeDomedCap(Radius, CenZ: single; slices: integer;
                                IsUp: boolean);
     procedure MakeTorus(MajorRadius, MinorRadius, Z: single;
                         slices: integer);
     procedure MakeFillet (MajorRadius, MinorRadius, CenZ: single;
                           slices: integer; Quadrant: TFilletQuadrant;
                           UsePrevious: boolean);
     procedure MakeTransitionFiner(RadiusT, RadiusB, TZ, BZ: single;
                                   UsePrevious: boolean);
     procedure MakeTransitionCoarser(RadiusT, RadiusB, TZ, BZ: single;
                                     UsePrevious: boolean);

     function AddCircVertices(Radius, BZ, normZ, xyFact: single): integer;

     procedure GenGLFaces(TopIndex, BottomIndex: integer); inline;
     procedure GenTriangles(TopIndex, BottomIndex: integer); inline;
     procedure GenQuads(TopIndex, BottomIndex: integer); inline;

     procedure AddFace(v1, v2, v3: Integer);
     procedure AddQuadFace(v1, v2, v3, v4: Integer);
     function AddVertex(X, Y, Z: Single; const NoDuplicate: Boolean = false)
       : Integer;

     function AddNormal(X, Y, Z: Single)
       : Integer; overload;
     function AddNormal(v: TAffineVector)
       : Integer; overload;
     function AddTexCoords(u, v: Single)
       : Integer;
     procedure RenderNormals;
     procedure RenderEdges;
   published
     property Sectors: integer read fReqSectors write SetSectors;
     property UsesDoublMesh: boolean read fUsesDoubleMesh write SetUsesDoubleMesh;
     property UsesQuadMesh: boolean read fUsesQuadMesh write SetUsesQuadMesh;
     property TopoFaces: integer read fTopoFaces write fTopoFaces;
     property EdgeCount: integer read fEdgeCount;
   end;


implementation

{$IFDEF STANDALONE}
// For test app extract some routines from 2D lib.
// TODO ? decouple this from my 2D Library
// replace with GLScene routines at some point.
//
type
   TVector2D = record
     case integer of
       0: (V: array[0..1] of single);
       1: (X,Y: single);
   end;

   function LeftPerp(v: TVector2D): TVector2D;
   begin
     Result.X := -v.Y;
     Result.Y := v.X;
   end;

   function RightPerp(v: TVector2D): TVector2D;
   begin
     Result.X := v.Y;
     Result.Y := -v.X;
   end;

   function DistSquared(const v: TVector2D): single;
   begin
     Result := v.X * v.X + v.Y * v.Y;
   end;

   function Magnitude(v: TVector2D): single;
   begin
     Result := sqrt(DistSquared(v));
   end;

   function Scale(const v: TVector2D; sFact: single): TVector2D;
   begin
     Result.X := v.X * sFact;
     Result.Y := v.y * sFact;
   end;


   function UnitVector(v: TVector2D): TVector2D;
   begin
     Result := Scale(v,(1/Magnitude(v)));
   end;

   function OriginVector(v1, v2: TVector2D): TVector2D;
   begin
     Result.X := v2.X - v1.X;
     Result.Y := v2.Y - v1.Y;
   end;

{$ENDIF}

{ TGLRevolutionMeshObject }

procedure TGLRevolutionMeshObject.SetSectors(AValue: integer);
begin
  fReqSectors := AValue;
  InitSectors;
end;

procedure TGLRevolutionMeshObject.SetUsesDoubleMesh(AValue: boolean);
begin
  fUsesDoubleMesh := AValue;
  InitSectors;
end;

procedure TGLRevolutionMeshObject.SetUsesQuadMesh(AValue: boolean);
begin
  fUsesQuadMesh := AValue;
  InitSectors;
end;

procedure TGLRevolutionMeshObject.InitSectors;
var
  i: integer;
  delta, theta: single;
begin

  fSectors := fReqSectors;

  SetLength(StandardSinArray, fSectors);
  SetLength(StandardCosArray, fSectors);
  delta := pi * 2 / fSectors;
  for i := 0 to fSectors - 1 do
  begin
    theta := delta * i;
    SinCos(theta, StandardSinArray[i], StandardCosArray[i]);
  end;

  // fine mesh
  if fUsesDoubleMesh or fUsesQuadMesh then
  begin
    SetLength(DoubleSinArray, fSectors * 2);
    SetLength(DoubleCosArray, fSectors * 2);
    delta := pi / fSectors;
    for i := 0 to fSectors * 2 - 1 do
    begin
      theta := delta * i;
      SinCos(theta,DoubleSinArray[i],DoubleCosArray[i]);
    end;

  end;
  // even finer mesh
  if fUsesQuadMesh then
  begin
    SetLength(QuadSinArray, fSectors * 4);
    SetLength(QuadCosArray, fSectors * 4);
    delta := pi / fSectors / 2;
    for i := 0 to fSectors * 4 - 1 do
    begin
      theta := delta * i;
      SinCos(theta,QuadSinArray[i],QuadCosArray[i]);
    end;

  end;

  SinArray := StandardSinArray;
  CosArray := StandardCosArray;

end;

procedure TGLRevolutionMeshObject.AddEdge(RingIndex, Sectors: integer);
begin
  if not assigned(fEdges) then
  begin
    SetLength(fEdges, 10);
    fEdgeCapacity:=10;
    fEdgeCount:= 0;
  end;

  if fEdgeCount = fEdgeCapacity - 1 then
  begin
     SetLength(fEdges, fEdgeCapacity + 10);
     inc(fEdgeCapacity, 10);
  end;

  fEdges[fEdgeCount].VertexIndex := RingIndex;
  fEdges[fEdgeCount].VertexCount := Sectors;

  inc(fEdgeCount);

end;

constructor TGLRevolutionMeshObject.Create;
begin
  inherited Create;
  FFaceList      := TFGVertexIndexList.CreateOwned(self.FaceGroups);
  FFaceList.Mode := fgmmTriangles;
  Mode           := momFaceGroups;
  fReqSectors    := 8;
  fCurrentResolution := tsrStandard;
  self.UseVBO    := True;
  fUsesDoubleMesh:= False;
  fUsesQuadMesh  := False;
  fTopoFaces     := 0;
  InitSectors;
end;

destructor TGLRevolutionMeshObject.Destroy;
begin
  FFaceList.Free;
  inherited Destroy;
end;

procedure TGLRevolutionMeshObject.MakeTip(Radius, Z, TipZ: single;
  IsUp, UsePrevious: boolean);
var
  spoint, i: integer;
  v1,v2: TVector2D;
begin

  if not UsePrevious then
  begin
    v1.X := Radius;
    v1.Y := Z-TipZ;

    if IsUp then
      v2 := LeftPerp(UnitVector(v1))
    else
      begin
        v2 := RightPerp(UnitVector(v1));
        addEdge(fPreviousRing,fSectors);
      end;

    fPreviousRing :=  AddCircVertices(Radius, Z, v2.y, v2.x);
    inc(fTopoFaces);



  end;

  spoint := fPreviousRing;

  if IsUp then
    AddNormal(vAxisZ)
  else
    AddNormal(vNegAxisZ);

  AddVertex(0, 0, TipZ);

  if FFaceList.Mode = fgmmTriangles then
    if IsUp then
      for i := 0 to fSectors - 1 do
        if i = 0 then
          AddFace(spoint + fSectors, spoint + i, spoint + fSectors - 1)
        else
          AddFace(spoint + fSectors, spoint + i, spoint + i - 1)
    else
      for i := 0 to fSectors - 1 do
        if i = 0 then
          AddFace(spoint + fSectors, spoint + fSectors - 1, spoint + i)
        else
          AddFace(spoint + fSectors, spoint + i - 1, spoint + i)
  else
    if IsUp then
      for i := 0 to fSectors - 1 do
        if i = 0 then
          AddQuadFace(spoint + fSectors, spoint + fSectors, spoint + i, spoint + fSectors - 1)
        else
          AddQuadFace(spoint + fSectors, spoint + fSectors, spoint + i, spoint + i - 1)
    else
      for i := 0 to fSectors - 1 do
        if i = 0 then
          AddQuadFace(spoint + fSectors, spoint + fSectors, spoint + fSectors - 1, spoint + i)
        else
          AddQuadFace(spoint + fSectors, spoint + fSectors, spoint + i - 1, spoint + i);

end;

procedure TGLRevolutionMeshObject.MakeConicSection(RadiusT, RadiusB, TZ, BZ: single;
                       slices: integer; UsePrevious: boolean );
var
  dRad, dZ: single;
  posTop, posBot: integer;
  i: integer;
  v1,v2: TVector2D;
begin
  v1.X := RadiusT - RadiusB;
  v1.Y := TZ - BZ;
  v2 := RightPerp(UnitVector(v1));

  dRad := (RadiusT - RadiusB) / slices;
  dZ := (TZ - BZ) / slices;
  if UsePrevious then
    posBot := fPreviousRing
  else
    begin
      posBot := AddCircVertices(RadiusT, TZ, v2.y, v2.x);
      inc(fTopoFaces);
      AddEdge(fPreviousRing, fSectors);
    end;

  for i := 1 to slices do
  begin
    posTop := posBot;
    posBot := AddCircVertices(RadiusT - dRad * i,
                              TZ - dZ * i,
                              v2.y, v2.x);
    GenGLFaces(posTop, posBot);

  end;
  fPreviousRing := posBot;
end;

procedure TGLRevolutionMeshObject.MakeDomedCap(Radius, CenZ: single; slices: integer;
  IsUp: boolean);
var
  dT, CosT, SinT: single;
  i, posBot, posTop: integer;
begin

  dT := pi / 2 / slices;

  if IsUp then
    begin
      inc(fTopoFaces);

      SinCos(dt, 1, SinT, CosT);

      // ensure make tip uses a ring of vertices with the normal we want
      // not the normal of the segment.
      posBot :=  AddCircVertices(Radius * SinT,
                           CenZ + CosT * Radius,
                           CosT, SinT);

      makeTip(SinT * Radius, CenZ + CosT * Radius, CenZ + Radius, True, True);


      for i := 2 to slices do
      begin
        SinCos(dt * i , 1, SinT, CosT);
        posTop := posBot;
        posBot :=  AddCircVertices(Radius * SinT,
                                   CenZ + CosT * Radius,
                                   CosT, SinT);

        GenGLFaces(posTop, posBot);

      end;
      fPreviousRing := posBot;
    end
  else
    begin
       posBot := fPreviousRing;

       for i := 1 to slices  do
       begin
         SinCos(dt * i , 1, SinT, CosT);
         posTop := posBot;
         posBot :=  AddCircVertices(Radius * CosT,
                                    CenZ - SinT * Radius,
                                    -SinT, CosT);

         GenGLFaces(posTop, posBot);
       end;
       fPreviousRing := posbot;
       // using previous so first two params are dicarded
       makeTip(1, 1, CenZ - Radius, False, True);

    end;



end;

procedure TGLRevolutionMeshObject.MakeTorus(MajorRadius, MinorRadius, Z: single;
  slices: integer);
var
  i, posTop, posStart, posBot: integer;
  dT, SinT, CosT: single;
begin

  dT := pi * 2 / slices;
  inc(fTopoFaces);
  // start with the extreme
  SinCos(0, 1, SinT, CosT);
  posBot :=  AddCircVertices(MajorRadius + MinorRadius * SinT,
                             Z + MinorRadius * CosT,
                             CosT, SinT);
  posStart := posBot;

  for i := 1 to slices - 1 do
  begin
     SinCos(dt * i, 1, SinT, CosT);
     posTop := posBot;
     posBot :=  AddCircVertices(MajorRadius + MinorRadius * SinT,
                                Z + MinorRadius * CosT,
                                CosT, SinT);
     GenGLFaces(posTop, posBot);
  end;

  GenGLFaces(posBot, posStart);

end;

procedure TGLRevolutionMeshObject.MakeFillet(MajorRadius, MinorRadius, CenZ: single;
  slices: integer; Quadrant: TFilletQuadrant; UsePrevious: boolean);
var
  i, posTop, posBot: integer;
  sA, dT, CosT, SinT, dirFact, normFact: single;
begin

  dT := pi * 0.5 / slices;
  sA := 0;
  dirFact := 1;
  normFact := 1;

  case Quadrant of
  tfqNE:;
  tfqNW:
    begin
      sA := pi * 2;
      dirFact := -1;
    end;
  tfqSE:
    sA := pi * 0.5;
  tfqSW:
    begin
      sA := pi * 1.5 ;
      dirFact := -1;
    end;
  end;

  if UsePrevious then
    posBot := fPreviousRing
  else
    begin
      SinCos( sA, 1, SinT, CosT);
      posBot :=  AddCircVertices(MajorRadius + MinorRadius * SinT,
                                 CenZ + MinorRadius * CosT ,
                                 CosT * dirFact , SinT * dirFact );
      inc(fTopoFaces);
      AddEdge(fPreviousRing, fSectors);
    end;

  for i := 1 to slices  do
  begin
     SinCos(sA + dt * i * dirFact, 1, SinT, CosT);
     posTop := posBot;

     posBot :=  AddCircVertices(MajorRadius + MinorRadius * SinT,
                                CenZ + MinorRadius * CosT,
                                CosT * dirFact , SinT * dirFact);

     GenGLFaces(posTop, posBot);
  end;

  fPreviousRing := posBot;
end;

procedure TGLRevolutionMeshObject.MakeTransitionFiner(RadiusT, RadiusB, TZ,
  BZ: single; UsePrevious: boolean);
var
  i, prevSect, posTop, posBot: integer;
  leftTind, leftBind, curBind, midBind, curTind: integer;
  v1,v2: TVector2D;
begin
  if fCurrentResolution = tsrQuad then  // can't go higher than quad atm
    MakeConicSection(RadiusT, RadiusB, TZ,  BZ, 1,UsePrevious)
  else
    begin
      v1.X := RadiusT - RadiusB;
      v1.Y := TZ - BZ;
      v2 := RightPerp(UnitVector(v1));

      // use or make the top
      if UsePrevious then
        posTop := fPreviousRing
      else
        begin
          posTop := AddCircVertices(RadiusT, TZ, v2.y, v2.x);
          inc(fTopoFaces);
          AddEdge(fPreviousRing, fSectors);
       end;
      prevSect := fSectors;

      // now set state values for next sections
      case fCurrentResolution of
        tsrStandard:
          begin
            fCurrentResolution:=tsrDouble;
            SinArray := DoubleSinArray;
            CosArray := DoubleCosArray;
          end;
        tsrDouble:
          begin
            fCurrentResolution:=tsrQuad;
            SinArray := QuadSinArray;
            CosArray := QuadCosArray;
          end;
      end;
      fSectors := fSectors * 2;


      posBot := AddCircVertices(RadiusB, BZ, v2.y, v2.x);

      for i := 0 to prevSect - 1 do
      begin
        if i = 0 then
          begin
            leftTInd := posTop + prevSect - 1;
            leftBind := posBot + fSectors - 2 ;
            midBind := posBot + fSectors - 1;
            curBind := posbot;
            curTind := posTop;
          end
        else
          begin
           leftTInd := posTop + i - 1 ;
           curTind := posTop + i;
           leftBInd := posBot + i * 2 - 2;
           midBind := posBot + i * 2 - 1;
           curBind := posBot + i * 2;
          end;

          AddFace(leftTind, midBind , leftBind);
          AddFace(leftTind, curTind, midBind );
          AddFace(curTind, curBind, midBind );



      end;



      fPreviousRing:=posBot;
    end;
end;

procedure TGLRevolutionMeshObject.MakeTransitionCoarser(RadiusT, RadiusB, TZ,
  BZ: single; UsePrevious: boolean);
var
  i, prevSect, posTop, posBot: integer;
  leftTind, leftBind, curBind, midTind, curTind: integer;
  v1,v2: TVector2D;
begin
  if fCurrentResolution = tsrStandard then  // can't go lower than standard PERIOD.
    MakeConicSection(RadiusT, RadiusB, TZ,  BZ, 1,UsePrevious)
  else
    begin
      v1.X := RadiusT - RadiusB;
      v1.Y := TZ - BZ;
      v2 := RightPerp(UnitVector(v1));

      // use or make the top
      if UsePrevious then
        posTop := fPreviousRing
      else
        begin
          posTop := AddCircVertices(RadiusT, TZ, v2.y, v2.x);
          inc(fTopoFaces);
          AddEdge(fPreviousRing, fSectors);
        end;
      prevSect := fSectors;

      // now set state values for next sections
      case fCurrentResolution of
        tsrQuad:
          begin
            fCurrentResolution:=tsrDouble;
            SinArray := DoubleSinArray;
            CosArray := DoubleCosArray;
          end;
        tsrDouble:
          begin
            fCurrentResolution:=tsrQuad;
            SinArray := StandardSinArray;
            CosArray := StandardCosArray;
          end;
      end;
      fSectors := fSectors div 2;

      posBot := AddCircVertices(RadiusB, BZ, v2.y, v2.x);

      for i := 0 to fSectors - 1 do
      begin
        if i = 0 then
          begin
            leftTInd := posTop + prevSect - 2;
            midTind := posTop + prevSect - 1;
            leftBind := posBot + fSectors - 1 ;
            curBind := posbot;
            curTind := posTop;
          end
        else
          begin
           leftTInd := posTop + i * 2 - 2 ;
           midTind := posTop + i * 2 - 1;
           curTind := posTop + i * 2;
           leftBInd := posBot + i - 1;
           curBind := posBot + i;
          end;

          AddFace(leftTind, midTind , leftBind);
          AddFace(leftBind, midTind , curBind);
          AddFace(curTind, curBind, midTind );



      end;

      fPreviousRing := posBot;
    end;
end;

procedure TGLRevolutionMeshObject.AddFace(v1, v2, v3: Integer);
begin
  FFaceList.VertexIndices.Add(v1, v2, v3);
end;

procedure TGLRevolutionMeshObject.AddQuadFace(v1, v2, v3, v4: Integer);
begin
  FFaceList.VertexIndices.Add(v1);
  FFaceList.VertexIndices.Add(v2);
  FFaceList.VertexIndices.Add(v3);
  FFaceList.VertexIndices.Add(v4);
end;

function TGLRevolutionMeshObject.AddCircVertices(Radius, BZ, normZ, xyFact: single): integer;
var
  res, i: integer;
  x, y: single;
begin

  for i := 0 to fSectors - 1 do
  begin
    x := SinArray[i] * Radius;
    y := CosArray[i] * Radius;
    AddNormal(SinArray[i] * xyFact, CosArray[i] * xyFact, normZ);
    if i = 0 then
      res := AddVertex(x,y,BZ)
    else
      AddVertex(x,y,BZ);
  end;
  Result := res;
end;

procedure TGLRevolutionMeshObject.GenGLFaces(TopIndex, BottomIndex: integer);
begin
  if FFaceList.Mode = fgmmQuads then
    GenQuads(TopIndex,BottomIndex)
  else
    GenTriangles(TopIndex,BottomIndex);
end;

procedure TGLRevolutionMeshObject.GenTriangles(TopIndex, BottomIndex: integer);
var
  i, leftInd: integer;
begin
  for i := 0 to fSectors-1 do
  begin
    if i = 0 then
      leftInd := fSectors - 1
    else
      leftInd := i - 1;

    AddFace(TopIndex + leftInd, BottomIndex + i, BottomIndex + leftInd);
    AddFace(TopIndex + leftInd, TopIndex + i, BottomIndex + i);
  end;
end;

procedure TGLRevolutionMeshObject.GenQuads(TopIndex, BottomIndex: integer);
var
  i, leftInd: integer;
begin
  for i := 0 to fSectors-1 do
  begin
    if i = 0 then
      leftInd := fSectors - 1
    else
      leftInd := i - 1;

    AddQuadFace(TopIndex + leftInd,
                TopIndex + i,
                BottomIndex + i,
                BottomIndex + leftInd
                 );
  end;

end;

function TGLRevolutionMeshObject.AddVertex(X, Y, Z: Single; const NoDuplicate: Boolean = false)
       : Integer;
var
  i: Integer;
begin
  if (NoDuplicate) and (vertices.Count > 0) then
  begin
    for i := 0 to vertices.Count - 1 do
    begin
      if ((vertices.Items[i].X = X) and (vertices.Items[i].Y = Y) and (vertices.Items[i].Z = Z)) then
        result := i
      else
        result := vertices.Add(X, Y, Z);
    end;
  end
  else
    result := vertices.Add(X, Y, Z);
end;

function TGLRevolutionMeshObject.AddNormal(X, Y, Z: Single): Integer;
var
  v: TAffineVector;
begin
  setAffineVector(v, X, Y, Z);
//  result := Normals.Add(VectorNormalize(v));
  result := Normals.Add(v);
end;

function TGLRevolutionMeshObject.AddNormal(v: TAffineVector): Integer;
begin
//  result := Normals.Add(VectorNormalize(v));
  result := Normals.Add(v);
end;

function TGLRevolutionMeshObject.AddTexCoords(u, v: Single): Integer;
begin
  // Ensure ranges
  if u < 0 then
    u := 0;
  if u > 1 then
    u := 1;
  if v < 0 then
    v := 0;
  if v > 0 then
    v := 1;

  result := TexCoords.Add(u, v);
end;

procedure TGLRevolutionMeshObject.RenderNormals;
var
  j:      Integer;
  iVert:     TAffineVector;
  iNormVert: TAffineVector;
begin
  gl.PushAttrib(GL_LIGHTING_BIT or GL_TEXTURE_BIT);
  gl.Disable(GL_LIGHTING);
  gl.ActiveTexture(GL_TEXTURE0_ARB);
  gl.Disable(GL_TEXTURE_2D);
  gl.ActiveTexture(GL_TEXTURE1_ARB);
  gl.Disable(GL_TEXTURE_2D);
  gl.ActiveTexture(GL_TEXTURE3_ARB);
  gl.Disable(GL_TEXTURE_2D);
  gl.ActiveTexture(GL_TEXTURE4_ARB);
  gl.Disable(GL_TEXTURE_2D);
  gl.Color3f(0,0,1);
  for j := 0 to FFaceList.VertexIndices.Count - 1 do
  begin
    iVert     := vertices[FFaceList.VertexIndices[j]];
    iNormVert := Normals[FFaceList.VertexIndices[j]];
    iNormVert := GLVectorGeometry.VectorAdd(iNormVert, iVert);
    gl.Begin_(GL_LINES);
    gl.Vertex3fv(@iVert);
    gl.Vertex3fv(@iNormVert);
    gl.End_;
  end;
  gl.ActiveTexture(GL_TEXTURE0_ARB);
  gl.PopAttrib;
end;

procedure TGLRevolutionMeshObject.RenderEdges;
var
  i, j:      Integer;
  iVert:     TAffineVector;
begin
  gl.PushAttrib(GL_LIGHTING_BIT or GL_TEXTURE_BIT);
  gl.Disable(GL_LIGHTING);
  gl.ActiveTexture(GL_TEXTURE0_ARB);
  gl.Disable(GL_TEXTURE_2D);
  gl.ActiveTexture(GL_TEXTURE1_ARB);
  gl.Disable(GL_TEXTURE_2D);
  gl.ActiveTexture(GL_TEXTURE3_ARB);
  gl.Disable(GL_TEXTURE_2D);
  gl.ActiveTexture(GL_TEXTURE4_ARB);
  gl.Disable(GL_TEXTURE_2D);
  gl.Color3f(0,0,1);
  gl.PushAttrib(GL_LINE_BIT);
  gl.Disable(GL_LINE_STIPPLE);
  gl.LineWidth(3);
  for i := 0 to fEdgeCount - 1 do
    begin
    gl.Begin_(GL_LINE_LOOP);
    for j := 0 to fEdges[i].VertexCount - 1 do
      begin
        iVert     := vertices[fEdges[i].VertexIndex + j];
        gl.Vertex3fv(@iVert);
      end;
    gl.End_;
  end;
  gl.PopAttrib;
  gl.ActiveTexture(GL_TEXTURE0_ARB);
  gl.PopAttrib;
end;


end.

