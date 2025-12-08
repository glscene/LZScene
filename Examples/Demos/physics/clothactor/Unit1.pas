{: Verlet cloth simulation and verlet constraints controlled by an
   actor's skeleton.<p>

   Verlet physics is used to simulate a cloth-like effect on a mesh.
   In this demo, the cape mesh is linked to the verlet world and the
   verlet nodes control the surface of the mesh. Verlet constraints
   define boundaries that the verlet nodes cannot enter.<p>

   The skeleton colliders define the verlet constraints for the actor
   using simple spheres and capsules (sphere-capped cylinders). The
   constraints get updated each frame to match the actor's current
   skeleton frame. Using simple primitives to approximate a mesh is
   much quicker than determining mesh volume -> verlet interactions.<p>

   A dynamic octree is used here to give a little performace boost to
   the verlet testing.
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  GLScene, GLVectorFileObjects, GLObjects, GLCadencer, GLTexture, GLLCLViewer,
  GLFileSMD, GLFile3DS, GLVerletClothify, GLVerletSkeletonColliders,
  GLShadowVolume,
  OpenGLTokens, GLVectorGeometry, GLGeometryBB, GLVerletTypes,
  GLSpacePartition, GLCrossPlatform, GLMaterial, GLCoordinates, GLBaseClasses,
  GLRenderContextInfo, GLState;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLActor1: TGLActor;
    ActorDummy: TGLDummyCube;
    Timer1: TTimer;
    OctreeRenderer: TGLDirectOpenGL;
    CheckBox_ShowOctree: TCheckBox;
    GLLightSource1: TGLLightSource;
    GLPlane1: TGLPlane;
    GLShadowVolume1: TGLShadowVolume;
    Cape: TGLActor;
    GLLightSource2: TGLLightSource;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure Timer1Timer(Sender: TObject);
    procedure OctreeRendererRender(Sender: TObject; var rci: TGLRenderContextInfo);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my: integer;
    VerletWorld: TglVerletWorld;
    EdgeDetector: TEdgeDetector;
    AirResistance: TVFAirResistance;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  GLUtils, GLContext;

// Mesh normal recalculation routines

procedure PrepareMeshForNormalsRecalc(BaseMesh: TGLBaseMesh);
var
  i, j, k: integer;
  mo: TGLMeshObject;
  fg: TFGVertexNormalTexIndexList;
begin
  // update normals
  // (not very efficient, could use some work...)
  for i := 0 to BaseMesh.MeshObjects.Count - 1 do
  begin
    mo := BaseMesh.MeshObjects[i];

    for j := 0 to mo.FaceGroups.Count - 1 do
    begin
      if mo.FaceGroups[j] is TFGVertexNormalTexIndexList then
      begin
        fg := TFGVertexNormalTexIndexList(mo.FaceGroups[j]);
        for k := 0 to fg.VertexIndices.Count - 1 do
        begin
          fg.NormalIndices.List[k] := fg.VertexIndices.List[k];
        end;
      end;
    end;
  end;

  BaseMesh.StructureChanged;
end;

procedure RecalcMeshNormals(BaseMesh: TGLBaseMesh);
var
  i, j, k: integer;
  mo: TGLMeshObject;
  fg: TFGVertexIndexList;
  n: TAffineVector;
begin
  // update normals
  // (not very efficient, could use some work...)
  for i := 0 to BaseMesh.MeshObjects.Count - 1 do
  begin
    mo := BaseMesh.MeshObjects[i];

    FillChar(mo.Normals.List[0], SizeOf(TAffineVector) * mo.Normals.Count, 0);

    for j := 0 to mo.FaceGroups.Count - 1 do
    begin
      if mo.FaceGroups[j] is TFGVertexIndexList then
      begin
        fg := TFGVertexIndexList(mo.FaceGroups[j]);
        k := 0;
        while k <= fg.VertexIndices.Count - 3 do
        begin
          n := CalcPlaneNormal(mo.Vertices.List[fg.VertexIndices.List[k]],
            mo.Vertices.List[fg.VertexIndices.List[k + 1]],
            mo.Vertices.List[fg.VertexIndices.List[k + 2]]);
          mo.Normals.TranslateItem(fg.VertexIndices.List[k], n);
          mo.Normals.TranslateItem(fg.VertexIndices.List[k + 1], n);
          mo.Normals.TranslateItem(fg.VertexIndices.List[k + 2], n);//}

          Inc(k, 3);
        end;
      end;
    end;
    mo.Normals.Normalize;
  end;

  BaseMesh.StructureChanged;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  SetGLSceneMediaDir();
  Randomize;
  // Load the actor and animations
  GLActor1.LoadFromFile(MediaPath+'trinityRAGE.smd');
  GLActor1.AddDataFromFile(MediaPath+'walk.smd');
  GLActor1.Animations[1].MakeSkeletalTranslationStatic;
  GLActor1.SwitchToAnimation('walk');
  GLActor1.BuildSilhouetteConnectivityData;

  // Load the cape
  Cape.LoadFromFile(MediaPath+'cape.3ds');
  Cape.Position.Y := GLActor1.BoundingSphereRadius - 10;
  PrepareMeshForNormalsRecalc(Cape);
  Cape.BuildSilhouetteConnectivityData;

  // Set up the floor texture and reposition to below the actors feet
  with GLPlane1.Material.Texture do
  begin
    Image.LoadFromFile(MediaPath+'beigemarble.jpg');
    Disabled := False;
  end;
  GLPlane1.Position.Y := -GLActor1.BoundingSphereRadius;

  // Setting up the verlet world using the optional dynamic octree can
  // give good perfamnce increases.
  VerletWorld := TGLVerletWorld.Create;
  VerletWorld.CreateOctree(
    AffineVectorMake(0, 0, 0),
    AffineVectorMake(0, 0, 0), 10, 6);

  VerletWorld.UpdateSpacePartion := uspEveryFrame;
  VerletWorld.Iterations := 3;

  // 'Clothify' the cape and add it to the verlet world
  EdgeDetector := TEdgeDetector.Create(Cape);
  EdgeDetector.ProcessMesh;
  EdgeDetector.AddEdgesAsSticks(VerletWorld, 0.15);
  EdgeDetector.AddEdgesAsSolidEdges(VerletWorld);
  //EdgeDetector.AddOuterEdgesAsSolidEdges(VerletWorld);

  // Set up verlet gravity and add the floor as a constraint
  with TVFGravity.Create(VerletWorld) do
    Gravity := AffineVectorMake(0, -98.1, 0);
  with TVCFloor.Create(VerletWorld) do
  begin
    Normal := GLPlane1.Direction.AsAffineVector;
    Location := VectorAdd(GLPlane1.Position.AsAffineVector,
      VectorScale(GLPlane1.Direction.AsAffineVector, 0.1));
  end;

  // Load the skeleton colliders. Skeleton colliders define an
  // approximate collision boundary for actors and are controlled
  // by the actor's skeleton.
  with GLActor1.Skeleton.Colliders do
  begin
    LoadFromFile(MediaPath+'trinityRAGE.glsc');
    AlignColliders;
  end;

  // Add the collider's verlet constraints to the verlet world
  AddSCVerletConstriantsToVerletWorld(GLActor1.Skeleton.Colliders, VerletWorld);

  {AirResistance := TVFAirResistance.Create(VerletWorld);
  AirResistance.DragCoeff := 0.001;
  AirResistance.WindDirection := AffineVectorMake(0,0,1);
  AirResistance.WindMagnitude := 15;
  AirResistance.WindChaos := 2;//}
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  VerletWorld.Free;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  // Step the verlet world (this is where the magic happens)
  VerletWorld.Progress(deltaTime, newTime);

  // Recalculate the cape's normals
  RecalcMeshNormals(Cape);

  // Cycle the floor texture to make it look like it's moving
  GLPlane1.YOffset := GLPlane1.YOffset - 0.25 * deltaTime;
  if GLPlane1.YOffset < 0 then
    GLPlane1.YOffset := GLPlane1.YOffset + 1;

  // Orbit the light (to show off the pretty shadow volumes)
  GLLightSource1.MoveObjectAround(GLActor1, 0, -deltaTime * 20);
  GLLightSource1.PointTo(GLActor1, YHMGVector);
end;

procedure TForm1.OctreeRendererRender(Sender: TObject; var rci: TGLRenderContextInfo);

  procedure RenderAABB(AABB: TAABB; w, r, g, b: single);
  begin
    GL.Color3f(r, g, b);
    rci.GLStates.LineWidth := w;

    GL.Begin_(GL_LINE_STRIP);
    GL.Vertex3f(AABB.min.v[0], AABB.min.v[1], AABB.min.v[2]);
    GL.Vertex3f(AABB.min.v[0], AABB.max.v[1], AABB.min.v[2]);
    GL.Vertex3f(AABB.max.v[0], AABB.max.v[1], AABB.min.v[2]);
    GL.Vertex3f(AABB.max.v[0], AABB.min.v[1], AABB.min.v[2]);
    GL.Vertex3f(AABB.min.v[0], AABB.min.v[1], AABB.min.v[2]);

    GL.Vertex3f(AABB.min.v[0], AABB.min.v[1], AABB.max.v[2]);
    GL.Vertex3f(AABB.min.v[0], AABB.max.v[1], AABB.max.v[2]);
    GL.Vertex3f(AABB.max.v[0], AABB.max.v[1], AABB.max.v[2]);
    GL.Vertex3f(AABB.max.v[0], AABB.min.v[1], AABB.max.v[2]);
    GL.Vertex3f(AABB.min.v[0], AABB.min.v[1], AABB.max.v[2]);
    GL.End_;

    GL.Begin_(GL_LINES);
    GL.Vertex3f(AABB.min.v[0], AABB.max.v[1], AABB.min.v[2]);
    GL.Vertex3f(AABB.min.v[0], AABB.max.v[1], AABB.max.v[2]);

    GL.Vertex3f(AABB.max.v[0], AABB.max.v[1], AABB.min.v[2]);
    GL.Vertex3f(AABB.max.v[0], AABB.max.v[1], AABB.max.v[2]);

    GL.Vertex3f(AABB.max.v[0], AABB.min.v[1], AABB.min.v[2]);
    GL.Vertex3f(AABB.max.v[0], AABB.min.v[1], AABB.max.v[2]);
    GL.End_;
  end;

  procedure RenderOctreeNode(Node: TSectorNode);
  var
    i: integer;
    AABB: TAABB;
  begin
    if Node.NoChildren then
    begin
      AABB := Node.AABB;

      if Node.RecursiveLeafCount > 0 then
        RenderAABB(AABB, 1, 0, 0, 0)
      else
        RenderAABB(AABB, 1, 0.8, 0.8, 0.8);//}

    end
    else
    begin
      for i := 0 to Node.ChildCount - 1 do
        RenderOctreeNode(Node.Children[i]);
    end;
  end;

begin
  if CheckBox_ShowOctree.Checked then
  begin
    if VerletWorld.SpacePartition is TOctreeSpacePartition then
    begin
      rci.GLStates.Disable(stLighting);
      rci.GLStates.Disable(stBlend);
      rci.GLStates.Disable(stLineStipple);
      rci.GLStates.Disable(stColorMaterial);
      RenderOctreeNode(TOctreeSpacePartition(VerletWorld.SpacePartition).RootNode);
    end;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%2.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  mx := x;
  my := y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my - y, mx - x);
  mx := x;
  my := y;
end;

end.

