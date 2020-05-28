//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  Silhouette classes for GLBaseMesh and FaceGroups.

   History :  
   16/11/10 - Yar - Added mesh visibility checking in TGLBaseMeshConnectivity.SetGLBaseMesh (thanks to dalex65)
   30/03/07 - DaStr - Added $I GLScene.inc
   25/03/07 - DaStr - Renamed parameters in some methods
  (thanks Burkhard Carstens) (Bugtracker ID = 1678658)
   23/03/07 - DaStr - Added explicit pointer dereferencing
  (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
   09/02/04 - MF - Fixed bug where vertices weren't freed when owned
   24/06/03 - MF - Created file from parts of GLShilouette
   
}

unit GLBaseMeshSilhouette;

interface

{$I GLScene.inc}

uses
  Classes, 
  GLVectorGeometry, 
  GLVectorLists, 
  GLVectorFileObjects, 
  GLSilhouette;

type
  // TGLFaceGroupConnectivity
  //
  TGLFaceGroupConnectivity = class(TConnectivity)
  private
    FMeshObject: TGLMeshObject;
    FOwnsVertices: boolean;
    procedure SetMeshObject(const Value: TGLMeshObject);

  public
    procedure Clear; override;

    { : Builds the connectivity information. }
    procedure RebuildEdgeList;

    property MeshObject: TGLMeshObject read FMeshObject write SetMeshObject;

    constructor Create(APrecomputeFaceNormal: boolean); override;
    constructor CreateFromMesh(aMeshObject: TGLMeshObject; APrecomputeFaceNormal: boolean);
    destructor Destroy; override;
  end;

  // TGLBaseMeshConnectivity
  //
  TGLBaseMeshConnectivity = class(TBaseConnectivity)
  private
    FGLBaseMesh: TGLBaseMesh;
    FFaceGroupConnectivityList: TList;
    function GetFaceGroupConnectivity(i: integer): TGLFaceGroupConnectivity;
    function GetConnectivityCount: integer;
    procedure SetGLBaseMesh(const Value: TGLBaseMesh);

  protected
    function GetEdgeCount: integer; override;
    function GetFaceCount: integer; override;

  public
    property ConnectivityCount: integer read GetConnectivityCount;
    property FaceGroupConnectivity[i: integer]: TGLFaceGroupConnectivity read GetFaceGroupConnectivity;
    property GLBaseMesh: TGLBaseMesh read FGLBaseMesh write SetGLBaseMesh;

    procedure Clear(SaveFaceGroupConnectivity: boolean);

    { : Builds the connectivity information. }
    procedure RebuildEdgeList;

    procedure CreateSilhouette(const silhouetteParameters: TGLSilhouetteParameters; var aSilhouette: TGLSilhouette; AddToSilhouette: boolean); override;

    constructor Create(APrecomputeFaceNormal: boolean); override;
    constructor CreateFromMesh(aGLBaseMesh: TGLBaseMesh);
    destructor Destroy; override;
  end;

implementation

{ TGLFaceGroupConnectivity }

// ------------------
// ------------------ TGLFaceGroupConnectivity ------------------
// ------------------

procedure TGLFaceGroupConnectivity.Clear;
  begin
    if Assigned(FVertices) then
    begin
      if FOwnsVertices then
        FVertices.Clear
      else
        FVertices := nil;

      inherited;

      if not FOwnsVertices and Assigned(FMeshObject) then
        FVertices := FMeshObject.Vertices;
    end
    else
      inherited;
  end;

constructor TGLFaceGroupConnectivity.Create(APrecomputeFaceNormal: boolean);
  begin
    inherited;

    FOwnsVertices := true;
  end;

procedure TGLFaceGroupConnectivity.SetMeshObject(const Value: TGLMeshObject);
  begin
    Clear;

    FMeshObject := Value;

    if FOwnsVertices then
      FVertices.Free;

    FVertices := FMeshObject.Vertices;

    FOwnsVertices := false;

    RebuildEdgeList;
  end;

constructor TGLFaceGroupConnectivity.CreateFromMesh(aMeshObject: TGLMeshObject; APrecomputeFaceNormal: boolean);
  begin
    Create(APrecomputeFaceNormal);

    MeshObject := aMeshObject;
  end;

destructor TGLFaceGroupConnectivity.Destroy;
  begin
    if FOwnsVertices then
      FVertices.Free;

    FVertices := nil;
    inherited;
  end;

procedure TGLFaceGroupConnectivity.RebuildEdgeList;
  var
    iFaceGroup, iFace, iVertex: integer;
    FaceGroup: TFGVertexIndexList;
    List: PIntegerArray;
  begin
    // Make sure that the connectivity information is empty
    Clear;

    // Create a list of edges for the meshobject
    for iFaceGroup := 0 to FMeshObject.FaceGroups.Count - 1 do
    begin
      Assert(FMeshObject.FaceGroups[iFaceGroup] is TFGVertexIndexList, 'Method only works for descendants of TFGVertexIndexList.');
      FaceGroup := TFGVertexIndexList(FMeshObject.FaceGroups[iFaceGroup]);

      case FaceGroup.Mode of
        fgmmTriangles, fgmmFlatTriangles:
          begin
            for iFace := 0 to FaceGroup.TriangleCount - 1 do
            begin
              List := @FaceGroup.VertexIndices.List[iFace * 3 + 0];
              AddIndexedFace(List^[0], List^[1], List^[2]);
            end;
          end;
        fgmmTriangleStrip:
          begin
            for iFace := 0 to FaceGroup.VertexIndices.Count - 3 do
            begin
              List := @FaceGroup.VertexIndices.List[iFace];
              if (iFace and 1) = 0 then
                AddIndexedFace(List^[0], List^[1], List^[2])
              else
                AddIndexedFace(List^[2], List^[1], List^[0]);
            end;
          end;
        fgmmTriangleFan:
          begin
            List := FaceGroup.VertexIndices.List;

            for iVertex := 2 to FaceGroup.VertexIndices.Count - 1 do
              AddIndexedFace(List^[0], List^[iVertex - 1], List^[iVertex])
          end;
      else
        Assert(false, 'Not supported');
      end;
    end;
  end;

// ------------------
// ------------------ TGLBaseMeshConnectivity ------------------
// ------------------

procedure TGLBaseMeshConnectivity.RebuildEdgeList;
  var
    i: integer;
  begin
    for i := 0 to ConnectivityCount - 1 do
      FaceGroupConnectivity[i].RebuildEdgeList;
  end;

procedure TGLBaseMeshConnectivity.Clear(SaveFaceGroupConnectivity: boolean);
  var
    i: integer;
  begin
    if SaveFaceGroupConnectivity then
    begin
      for i := 0 to ConnectivityCount - 1 do
        FaceGroupConnectivity[i].Clear;
    end
    else
    begin
      for i := 0 to ConnectivityCount - 1 do
        FaceGroupConnectivity[i].Free;

      FFaceGroupConnectivityList.Clear;
    end;
  end;

constructor TGLBaseMeshConnectivity.Create(APrecomputeFaceNormal: boolean);
  begin
    FFaceGroupConnectivityList := TList.Create;

    inherited;
  end;

constructor TGLBaseMeshConnectivity.CreateFromMesh(aGLBaseMesh: TGLBaseMesh);
  begin
    Create(not(aGLBaseMesh is TGLActor));
    GLBaseMesh := aGLBaseMesh;
  end;

procedure TGLBaseMeshConnectivity.SetGLBaseMesh(const Value: TGLBaseMesh);
  var
    i: integer;
    MO: TGLMeshObject;
    Connectivity: TGLFaceGroupConnectivity;
  begin
    Clear(false);

    FGLBaseMesh := Value;

    // Only precompute normals if the basemesh isn't an actor (because they change)
    FPrecomputeFaceNormal := not(Value is TGLActor);
    FGLBaseMesh := Value;

    for i := 0 to Value.MeshObjects.Count - 1 do
    begin
      MO := Value.MeshObjects[i];

      if MO.Visible then
      begin
        Connectivity := TGLFaceGroupConnectivity.CreateFromMesh(MO, FPrecomputeFaceNormal);

        FFaceGroupConnectivityList.Add(Connectivity);
      end;
    end;
  end;

procedure TGLBaseMeshConnectivity.CreateSilhouette(const silhouetteParameters: TGLSilhouetteParameters; var aSilhouette: TGLSilhouette; AddToSilhouette: boolean);

  var
    i: integer;
  begin
    if aSilhouette = nil then
      aSilhouette := TGLSilhouette.Create
    else
      aSilhouette.Flush;

    for i := 0 to ConnectivityCount - 1 do
      FaceGroupConnectivity[i].CreateSilhouette(silhouetteParameters, aSilhouette, true);
  end;

destructor TGLBaseMeshConnectivity.Destroy;
  begin
    Clear(false);
    FFaceGroupConnectivityList.Free;

    inherited;
  end;

function TGLBaseMeshConnectivity.GetConnectivityCount: integer;
  begin
    result := FFaceGroupConnectivityList.Count;
  end;

function TGLBaseMeshConnectivity.GetEdgeCount: integer;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to ConnectivityCount - 1 do
      result := result + FaceGroupConnectivity[i].EdgeCount;
  end;

function TGLBaseMeshConnectivity.GetFaceCount: integer;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to ConnectivityCount - 1 do
      result := result + FaceGroupConnectivity[i].FaceCount;
  end;

function TGLBaseMeshConnectivity.GetFaceGroupConnectivity(i: integer): TGLFaceGroupConnectivity;
  begin
    result := TGLFaceGroupConnectivity(FFaceGroupConnectivityList[i]);
  end;

end.
