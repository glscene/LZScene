//
// The graphics engine GLXEngine. The unit of LZScene for Lazarus
//
{
  Miscelaneous functions used by DCE (Dynamic Collision Engine).
}
unit GLDCEMisc;

interface

{$I GLScene.inc}

uses
  GLVectorFileObjects, 
  GLEllipseCollision, 
  GLVectorGeometry, 
  GLVectorLists,
  GLScene, 
  GLTerrainRenderer,
  GLProxyObjects, 
  GLMultiProxy
  , GLVectorTypes;

//Calculate and set the collision range
procedure ECSetCollisionRange(var MovePack: TECMovePack);

//Set the collider lists to null
procedure ECResetColliders(var MovePack: TECMovePack);

//Add freeform's octree data
procedure ECAddFreeForm(var MovePack: TECMovePack; FreeForm: TGLBaseSceneObject;
  Solid: Boolean; ObjectID: Integer);

//Add a TriMesh box
procedure ECAddBox(var MovePack: TECMovePack;
  BoxObj: TGLBaseSceneObject; BoxSize: TAffineVector;
  Solid: Boolean; ObjectID: Integer);

//Add the terrain as a TriMesh
procedure ECAddTerrain(var MovePack: TECMovePack;
  TerrainRenderer: TGLTerrainRenderer; Resolution: Single;
  Solid: Boolean; ObjectID: Integer);

//Add a static ellipsoid
procedure ECAddEllipsoid(var MovePack: TECMovePack;
  ePos, eRadius: TAffineVector;
  Solid: Boolean; ObjectID: Integer);


const
    DCEBox: array [0..35] of TAffineVector = (
      (X: 1; Y:-1; Z:-1),  (X: 1; Y: 1; Z:-1),  (X: 1; Y:-1; Z: 1),
      (X: 1; Y: 1; Z:-1),  (X: 1; Y: 1; Z: 1),  (X: 1; Y:-1; Z: 1),

      (X: 1; Y: 1; Z:-1),  (X:-1; Y: 1; Z:-1),  (X:-1; Y: 1; Z: 1),
      (X: 1; Y: 1; Z: 1),  (X: 1; Y: 1; Z:-1),  (X:-1; Y: 1; Z: 1),

      (X:-1; Y: 1; Z: 1),  (X:-1; Y:-1; Z: 1),  (X: 1; Y:-1; Z: 1),
      (X: 1; Y: 1; Z: 1),  (X:-1; Y: 1; Z: 1),  (X: 1; Y:-1; Z: 1),

      (X:-1; Y:-1; Z: 1),  (X:-1; Y: 1; Z: 1),  (X:-1; Y: 1; Z:-1),
      (X:-1; Y:-1; Z:-1),  (X:-1; Y:-1; Z: 1),  (X:-1; Y: 1; Z:-1),

      (X: 1; Y:-1; Z: 1),  (X:-1; Y:-1; Z: 1),  (X: 1; Y:-1; Z:-1),
      (X:-1; Y:-1; Z: 1),  (X:-1; Y:-1; Z:-1),  (X: 1; Y:-1; Z:-1),

      (X: 1; Y: 1; Z:-1),  (X: 1; Y:-1; Z:-1),  (X:-1; Y: 1; Z:-1),
      (X: 1; Y:-1; Z:-1),  (X:-1; Y:-1; Z:-1),  (X:-1; Y: 1; Z:-1)

    );

implementation

procedure ECSetCollisionRange(var MovePack: TECMovePack);
var  N: TAffineVector;
begin
  N.V[0] := Abs(MovePack.Velocity.V[0]) + Abs(MovePack.Gravity.V[0]) + (MovePack.Radius.V[0]);
  N.V[1] := Abs(MovePack.Velocity.V[1]) + Abs(MovePack.Gravity.V[1]) + (MovePack.Radius.V[1]);
  N.V[2] := Abs(MovePack.Velocity.V[2]) + Abs(MovePack.Gravity.V[2]) + (MovePack.Radius.V[2]);
  MovePack.CollisionRange := MaxXYZComponent(N);
end;

procedure ECResetColliders(var MovePack: TECMovePack);
begin
  SetLength(MovePack.TriMeshes,0);
  SetLength(MovePack.Freeforms,0);
  SetLength(MovePack.Colliders,0);
end;

procedure ECAddFreeForm(var MovePack: TECMovePack; FreeForm: TGLBaseSceneObject;
  Solid: Boolean; ObjectID: Integer);
var
  i, count : Integer;
  Pos: TVector;
  Master: TGLBaseSceneObject;
  d1,d2: Single;
begin
  Master := FreeForm;
  if Master is TGLFreeFormProxy then
    Master := TGLFreeFormProxy(Master).MasterObject;
  if Master is TGLMultiProxy then
    if TGLMultiProxy(Master).MasterObjects.Count > 0 then
      Master := TGLMultiProxy(Master).MasterObjects[0].MasterObject;
  Assert((Master is TGLFreeForm), 'Object must be freeform, freeformproxy or freeformbased Multiproxy.');
  Assert(Assigned(TGLFreeForm(Master).Octree), 'Octree must have been prepared and setup before use.');
  SetVector(Pos,  FreeForm.AbsoluteToLocal(MovePack.Position));

  //Is in boundingsphere?
  d1 := VectorDistance2(MovePack.Position,AffineVectorMake(FreeForm.AbsolutePosition));
  d2 := sqr(MovePack.CollisionRange + Freeform.BoundingSphereRadius);
  if d1 > d2 then exit;

  count := Length(MovePack.Freeforms);
  with TGLFreeForm(Master).Octree do
  begin
    WalkSphereToLeaf(RootNode, Pos, MovePack.CollisionRange);

    if not Assigned(resultarray) then exit;
    //Copy the result array
    SetLength(MovePack.Freeforms,count+1);
    SetLength(MovePack.Freeforms[count].OctreeNodes, Length(resultarray));
    for i := 0 to High(resultarray) do
      MovePack.Freeforms[count].OctreeNodes[i] := resultarray[i];
    //Reference to this octree
    MovePack.Freeforms[count].triangleFiler := @triangleFiler;
    MovePack.Freeforms[count].ObjectInfo.AbsoluteMatrix := Freeform.AbsoluteMatrix;
    MovePack.Freeforms[count].ObjectInfo.Solid := Solid;
    MovePack.Freeforms[count].ObjectInfo.ObjectID := ObjectID;
    MovePack.Freeforms[count].InvertedNormals := TGLFreeForm(Master).NormalsOrientation = mnoInvert;
  end;
end;

procedure ECAddBox(var MovePack: TECMovePack;
  BoxObj: TGLBaseSceneObject; BoxSize: TAffineVector;
  Solid: Boolean; ObjectID: Integer);
var t,count,i : Integer;
    p1, p2, p3: TAffineVector;
    BoxRadius,d1,d2: Single;
begin

  BoxRadius := MaxXYZComponent(BoxSize)*MaxXYZComponent(BoxObj.Scale.AsAffineVector);
  d1 := VectorDistance2(MovePack.Position,AffineVectorMake(BoxObj.AbsolutePosition));
  d2 := sqr(MovePack.CollisionRange + BoxRadius);
  if d1 > d2 then exit;

  //Add the box to the triangle list
  t := Length(MovePack.TriMeshes);
  SetLength(MovePack.TriMeshes,t+1);
  ScaleVector(BoxSize,0.5);
  count := 0;
  i := 0;
  while i < 36 do
  begin

    count := count + 1;
    SetLength(MovePack.TriMeshes[t].Triangles,count);

    with MovePack.TriMeshes[t] do
    begin
      p1 := DCEBox[i]; ScaleVector(p1,BoxSize); p1 := BoxObj.LocalToAbsolute(p1);
      p2 := DCEBox[i+1]; ScaleVector(p2,BoxSize); p2 := BoxObj.LocalToAbsolute(p2);
      p3 := DCEBox[i+2]; ScaleVector(p3,BoxSize); p3 := BoxObj.LocalToAbsolute(p3);
      i := i + 3;
      SetVector(Triangles[count-1].p1, p1);
      SetVector(Triangles[count-1].p2, p2);
      SetVector(Triangles[count-1].p3, p3);
      ObjectInfo.Solid := Solid;
      ObjectInfo.ObjectID := ObjectID;
    end;

  end;
end;

procedure ECAddTerrain(var MovePack: TECMovePack;
  TerrainRenderer: TGLTerrainRenderer; Resolution: Single;
  Solid: Boolean; ObjectID: Integer);


  function intvec(x,z: Single): TAffineVector;
  begin
    result.V[0] := x + MovePack.Position.V[0];
    result.V[1] := 0 + MovePack.Position.V[1];
    result.V[2] := z + MovePack.Position.V[2];
  end;

  function locabs(x,y,z: Single): TAffineVector;
  begin
    //result := TerrainRenderer.LocalToAbsolute(AffineVectorMake(x,y,z));
    //result := AffineVectorMake(x,y,z);
    result.V[0] := x + MovePack.Position.V[0];
    result.V[1] := y + TerrainRenderer.AbsolutePosition.V[1];
    result.V[2] := z + MovePack.Position.V[2];
  end;


var count,t : Integer;
    x,y,z: Single;
begin
  //Add the terrain to the list
  count := Length(MovePack.TriMeshes);
  SetLength(MovePack.TriMeshes,count+1);
  with MovePack.TriMeshes[count] do
  begin
    ObjectInfo.Solid := Solid;
    ObjectInfo.ObjectID := ObjectID;
    t := 0;
    x := - MovePack.CollisionRange;
    while x < MovePack.CollisionRange do
    begin

      z := - MovePack.CollisionRange;
      while z < MovePack.CollisionRange do
      begin
        //Add 2 triangles
        t := t + 2;
        SetLength(Triangles, t);

        //Tri 1
        y := TerrainRenderer.InterpolatedHeight(intvec(x,z));
        Triangles[t-2].p1 := locabs(x,y,z);
        y := TerrainRenderer.InterpolatedHeight(intvec(x,z+Resolution));
        Triangles[t-2].p2 := locabs(x,y,z+Resolution);
        y := TerrainRenderer.InterpolatedHeight(intvec(x+Resolution,z));
        Triangles[t-2].p3 := locabs(x+Resolution,y,z);

        //Tri 2
        y := TerrainRenderer.InterpolatedHeight(intvec(x+Resolution,z+Resolution));
        Triangles[t-1].p1 := locabs(x+Resolution,y,z+Resolution);
        y := TerrainRenderer.InterpolatedHeight(intvec(x+Resolution,z));
        Triangles[t-1].p2 := locabs(x+Resolution,y,z);
        y := TerrainRenderer.InterpolatedHeight(intvec(x,z+Resolution));
        Triangles[t-1].p3 := locabs(x,y,z+Resolution);

        z := z + Resolution;
      end;

      x := x + Resolution;
    end;

  end;
end;


procedure ECAddEllipsoid(var MovePack: TECMovePack; ePos, eRadius: TAffineVector;
  Solid: Boolean; ObjectID: Integer);
var count : Integer;
    d1, d2, r: single;
begin
  r := MaxXYZComponent(eRadius);
  d1 := VectorDistance2(MovePack.Position,ePos);
  d2 := sqr(MovePack.CollisionRange + r);
  if d1 > d2 then exit;

  //Add possible collider
  count := Length(MovePack.Colliders);
  SetLength(MovePack.Colliders,count+1);
  with MovePack.Colliders[count] do
  begin
    Position := ePos;
    Radius := eRadius;
    ObjectInfo.Solid := Solid;
    ObjectInfo.ObjectID := ObjectID;
  end;

end;


end.
