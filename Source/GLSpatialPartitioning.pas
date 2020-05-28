//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  Spatial partitioning related code that also uses GLScene objects

  History :  
       23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
       06/06/10 - Yar - Added GLVectorTypes to uses
       22/04/10 - Yar - Fixes after GLState revision
       05/03/10 - DanB - More state added to TGLStateCache
       24/03/07 - DaStr - Replaced GLWin32Viewer with GLViewer
                             (thanks Burkhard Carstens) (Bugtracker ID = 1684432)
       06/03/07 - DaStr - Removed obsolete FPC IFDEF's
       19/12/06 - DaStr - Old version of ExtendedFrustumMakeFromSceneViewer function
                              restored as an overloaded version of the new one
       04/11/05 - Mathx - Corrections related to bug 1335349
                             (ExtendedFrustumMakeFromSceneViewer supporting more
                             than just regular TGLSceneViewer).
       03/12/04 - MF - Created
   
}

unit GLSpatialPartitioning;

interface

uses
  GLViewer, GLSpacePartition, GLScene, GLVectorGeometry,
  OpenGLTokens, GLGeometryBB, GLRenderContextInfo, GLState;

type
  { Object for holding glscene objects in a spatial partitioning }
  TSceneObj = class(TSpacePartitionLeaf)
  public
    Obj: TGLBaseSceneObject;
    procedure UpdateCachedAABBAndBSphere; override;
    constructor CreateObj(Owner: TSectoredSpacePartition; aObj: TGLBaseSceneObject);
    destructor Destroy; override;
  end;

  { Render a spacial partitioning descending from TSectoredSpacePartition
  (octree and quadtree) as a grid - great for debugging and visualisation }
procedure RenderSpatialPartitioning(var rci: TGLRenderContextInfo;
  const Space: TSectoredSpacePartition);

{ Create an extended frustum from a GLSceneViewer - this makes the unit
specific to the windows platform!}
function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum;
  const vWidth, vHeight: integer; AGLCamera: TGLCamera): TExtendedFrustum; overload;

function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum;
  const AGLSceneViewer: TGLSceneViewer): TExtendedFrustum; overload;

{ Renders an AABB as a line }
procedure RenderAABB(var rci: TGLRenderContextInfo; AABB: TAABB; w, r, g, b: single); overload;
procedure RenderAABB(var rci: TGLRenderContextInfo; AABB: TAABB); overload;

implementation

uses
  GLVectorTypes,
  GLContext;

procedure RenderAABB(var rci: TGLRenderContextInfo; AABB: TAABB);
begin
  RenderAABB(rci, AABB, 1, 0.8, 0.8, 0.8);
end;

procedure RenderAABB(var rci: TGLRenderContextInfo; AABB: TAABB; w, r, g, b: single);
begin
  GL.Color3f(r, g, b);
  rci.GLStates.LineWidth := w;

  GL.Begin_(GL_LINE_STRIP);
  GL.Vertex3f(AABB.min.V[0], AABB.min.V[1], AABB.min.V[2]);
  GL.Vertex3f(AABB.min.V[0], AABB.max.V[1], AABB.min.V[2]);
  GL.Vertex3f(AABB.max.V[0], AABB.max.V[1], AABB.min.V[2]);
  GL.Vertex3f(AABB.max.V[0], AABB.min.V[1], AABB.min.V[2]);
  GL.Vertex3f(AABB.min.V[0], AABB.min.V[1], AABB.min.V[2]);

  GL.Vertex3f(AABB.min.V[0], AABB.min.V[1], AABB.max.V[2]);
  GL.Vertex3f(AABB.min.V[0], AABB.max.V[1], AABB.max.V[2]);
  GL.Vertex3f(AABB.max.V[0], AABB.max.V[1], AABB.max.V[2]);
  GL.Vertex3f(AABB.max.V[0], AABB.min.V[1], AABB.max.V[2]);
  GL.Vertex3f(AABB.min.V[0], AABB.min.V[1], AABB.max.V[2]);
  GL.End_;

  GL.Begin_(GL_LINES);
  GL.Vertex3f(AABB.min.V[0], AABB.max.V[1], AABB.min.V[2]);
  GL.Vertex3f(AABB.min.V[0], AABB.max.V[1], AABB.max.V[2]);

  GL.Vertex3f(AABB.max.V[0], AABB.max.V[1], AABB.min.V[2]);
  GL.Vertex3f(AABB.max.V[0], AABB.max.V[1], AABB.max.V[2]);

  GL.Vertex3f(AABB.max.V[0], AABB.min.V[1], AABB.min.V[2]);
  GL.Vertex3f(AABB.max.V[0], AABB.min.V[1], AABB.max.V[2]);
  GL.End_;
end;

// RenderSpatialPartitioning
//

procedure RenderSpatialPartitioning(var rci: TGLRenderContextInfo;
  const Space: TSectoredSpacePartition);

  procedure RenderSectorNode(Node: TSectorNode);
  var
    i: integer;
    AABB: TAABB;
  begin
    if Node.NoChildren then
    begin
      AABB := Node.AABB;

      if Node.RecursiveLeafCount > 0 then
        RenderAABB(rci, AABB, 1, 0, 0, 0)
      else
        RenderAABB(rci, AABB, 1, 0.8, 0.8, 0.8) //}

    end
    else
    begin
      for i := 0 to Node.ChildCount - 1 do
        RenderSectorNode(Node.Children[i]);
    end;
  end;
begin
  rci.GLStates.Disable(stLighting);
  RenderSectorNode(Space.RootNode);
end;

function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum;
  const AGLSceneViewer: TGLSceneViewer): TExtendedFrustum; //old version
begin
  Assert(Assigned(AGLSceneViewer.Camera), 'GLSceneViewer must have camera specified!');
  result := ExtendedFrustumMake(AFrustum,
    AGLSceneViewer.Camera.NearPlane,
    AGLSceneViewer.Camera.DepthOfView,
    AGLSceneViewer.FieldOfView,
    AGLSceneViewer.Camera.Position.AsAffineVector,
    AGLSceneViewer.Camera.Direction.AsAffineVector);
end;

function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum;
  const vWidth, vHeight: integer; AGLCamera: TGLCamera): TExtendedFrustum; //changed version
var
  buffov: single;
begin
  if vWidth < vHeight then
    buffov := AGLCamera.GetFieldOfView(vWidth)
  else
    buffov := AGLCamera.GetFieldOfView(vHeight);
  result := ExtendedFrustumMake(AFrustum,
    AGLCamera.NearPlane,
    AGLCamera.DepthOfView,
    buffov,
    AGLCamera.Position.AsAffineVector,
    AGLCamera.Direction.AsAffineVector);
end;

{ TSceneObj }

constructor TSceneObj.CreateObj(Owner: TSectoredSpacePartition; aObj: TGLBaseSceneObject);
begin
  Obj := aObj;
  inherited CreateOwned(Owner);
end;

destructor TSceneObj.Destroy;
begin
  inherited;
end;

procedure TSceneObj.UpdateCachedAABBAndBSphere;
begin
  FCachedAABB := Obj.AxisAlignedBoundingBox;
  FCachedAABB.min := Obj.LocalToAbsolute(FCachedAABB.min);
  FCachedAABB.max := Obj.LocalToAbsolute(FCachedAABB.max);
  FCachedBSphere.Radius := Obj.BoundingSphereRadius;
  FCachedBSphere.Center := AffineVectorMake(Obj.AbsolutePosition);
end;
end.

