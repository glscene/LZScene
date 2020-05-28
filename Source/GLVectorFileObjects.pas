
// This unit is part of the GLScene Project, http://glscene.org

{
   Vector File related objects for GLScene

  History : 

       28/06/13 - YP - Added support for vector color
       10/11/12 - PW - Added CPP compatibility: changed vector arrays to records
       11/07/12 - YP - Added BarycenterPosition and BarycenterOffset
                          New centering option macRestorePosition
       02/07/11 - DaStr - Replaced TAABB.Revision with TGLMeshObject.FExtentCacheRevision
       30/06/11 - DaStr - TGLBaseMesh.BarycenterAbsolutePosition() now uses caching
       23/02/11 - Yar - Added extent caching to TGLMeshObject
       03/12/10 - Yar - Added mesh visibility checking in
                            TGLMeshObjectList.ExtractTriangles (thnaks to Sandor Domokos)
       23/08/10 - Yar - Added OpenGLTokens to uses
       23/07/10 - Yar - Bugfixed TGLSkeleton.WriteToFiler (thanks E-Cone)
       11/06/10 - Yar - Bugfixed binary reading TGLMeshObject for FPC
                           Replace OpenGL1x functions to OpenGLAdapter.
                           Fixes for Linux x64
       22/04/10 - Yar - Fixes after GLState revision
       11/04/10 - Yar - Replaced function InsideList to GLState.InsideList
       05/03/10 - DanB - More state added to TGLStateCache
       25/12/09 - DaStr - Separated TGLActor.DoAnimate() from TGLActor.BuildList()
       16/01/09 - DanB - re-disable VBOs in display list to prevent AV on ATI cards
       27/11/08 - DanB - fix to TFGVertexIndexList.BuildList
       05/10/08 - DaStr - Added GLSM format backward compatibility after
                              MeshObject.LightMapTexCoords update
                              (thanks Uwe Raabe) (Bugtracker ID = 2140994)
       03/10/08 - DanB -  Added Delphi 2009 (Unicode) support
       22/06/08 - DaStr - TGLMeshObject.LightMapTexCoords converted to TAffineVectorList
                              (thanks Ast) (Bugtracker ID = 2000089)
       07/06/08 - DaStr - Implemented TBaseMeshObject.Assign(), TGLMeshObject.Assign()
       20/05/08 - Mrqzzz - Fixed memory leak in TGLSkeletonMeshObject.Destroy (thanks Dave Gravel)
       17/05/08 - DaStr - Added TGLSkeleton.MorphInvisibleParts
                             (thanks andron13 and Veon (BugtrackerID = 1966020)
                             Added vGLVectorFileObjectsEnableVBOByDefault
       01/05/08 - DaStr - Implemented TGLBaseMesh.BarycenterAbsolutePosition()
                             Bugfixed TGLBaseMesh.AxisAlignedDimensionsUnscaled()
       06/04/08 - DaStr - TGLMeshObjectList.MorphTo() and Lerp() are now virtual
       06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
       16/05/07 - PvD - Applied fixes to skeletonmesh to fix problems with
                            physics engines. (Bugtracker ID = 1719652)
       15/05/07 - LC - Added workaround for ATI bug in TFGVertexIndexList. (Bugtracker ID = 1719611)
       13/05/07 - LC - Fixed AV bug in TGLMeshObject.BufferArrays (Bugtracker ID = 1718033)
       03/04/07 - LC - Added VBO support for TextureEx (Bugtracker ID = 1693378)
       30/03/07 - DaStr - Added $I GLScene.inc
       28/03/07 - DaStr - Added explicit pointer dereferencing
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
       25/03/07 - LC - Added VBO support to TFGVertexIndexList, depends
                          on MeshObject owner's UseVBO status
       25/03/07 - LC - Fixed VBO bug. Bugtracker ID=1687665
       16/03/07 - DaStr - Added explicit pointer dereferencing
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
       21/02/07 - DaStr - Added TGLMeshObjectList.BuildTangentSpace, UseVBO
                             Added TGLActor.SetCurrentFrameDirect
       19/02/07 - LC - Added some VBO support
       19/10/06 - LC - Fixed bug in TGLActor.SetCurrentFrame. Bugtracker ID=1580511
       04/10/06 - PhP - fixed TGLActor.SetCurrentFrame (thanks dikoe)
       05/12/05 - PhP - fixed TFGIndexTexCoordList.BuildList (thanks fig)
       10/11/05 - Mathx - Added LastLoadedFilename to TGLBaseMesh (RFE 955083).
       09/11/05 - Mathx - Added isSwitchingAnimation to TGLActor.
       05/09/05 - Mathx - Fixed TGLSkeletonMeshObject read/write filer (thanks to Zapology)
       04/07/05 - Mathx - Protection against picking mode texture mapping errors
       27/01/05 - Mathx - BuildOctree can now specify an (optional) TreeDepth.
       11/01/05 - SG - Another fix for TGLBaseMesh.Assign (dikoe Kenguru)
       11/01/05 - SG - Fix for TGLBaseMesh.Assign when assigning actors
       26/11/04 - MRQZZZ - by Uwe Raabe : fixed TBaseMeshObject.BuildNormals
       26/11/04 - MRQZZZ - Added "Rendered" property to TGLBaseMesh in order
                              to prevent rendering of the GLBaseMesh but allowing
                              the rendering of it's children
       25/11/04 - SG - Fixed memory leak in TGLMeshObject (dikoe Kenguru)
       24/11/04 - MF - Added OctreePointInMesh
       03/10/04 - MRQZZZ - Fixed memory leak (FAutoScaling.Free) in TGLBaseMesh.Destroy; (thanks Jan Zizka)
       24/09/04 - SG - Added GetTriangleData/SetTriangleData functions,
                          Added TexCoordsEx, Binormals, Tangents,
                          Added BuildTangentSpace function (experimental).
       23/07/04 - SG - Added fgmmQuad case for TFGVertexIndexList.TraingleCount
                          (Thanks fig).
       02/08/04 - LR, YHC - BCB corrections: use record instead array
                               moved TBlendedLerpInfo to top of declaration
       18/07/04 - LR - Suppress Consts in uses
       20/06/04 - MRQZZZ - Added AutoScaling property to GLBaseMesh to scale
                              a mesh after loading (like Autocentering)
       30/03/04 - EG - Added TGLSkeletonBoneList.BoneCount
       23/03/04 - SG - External positions added to skeleton blended lerps.
                          AutoUpdate flag added to skeleton collider list.
       09/03/04 - SG - TFGIndexTexCoordList.BuildList can now use per vertex color
       29/01/04 - SG - Fix for ApplyCurrentSkeletonFrame with multiple bones per vertex.
                          Mesh reassembles correctly now (tested up to 4 bones per vertex).
       03/12/03 - SG - Added TGLSkeletonCollider and TGLSkeletonColliderList
                          Added Colliders (TGLSkeletonColliderList) to TGLSkeleton
       24/10/03 - SG - Various fixes for multiple bones per vertex
       21/09/03 - MRQZZZ - Added "aamLoopBackward" to AnimationMode property
       19/09/03 - EG - "Lighmap" -&gt; "LightMap"
       01/09/03 - SG - Added skeleton frame conversion methods to convert between
                          Rotations and Quaternions.
       27/08/03 - SG - Fixed AddWeightedBone for multiple bones per vertex
       13/08/03 - SG - Added quaternion transforms for skeletal animation
       12/08/03 - SG - Fixed a tiny bug in TGLSkeleton.MorphMesh
       08/07/03 - EG - Fixed puny bug in skeletal normals transformation
       05/06/03 - SG - Split SMD, MD2, 3DS, PLY, TIN and GTS code into separate units,
                          FileFormats\GLFile???.pas
       16/05/03 - SG - Fixed OpenGL error caused by glColorMaterial in TGLMeshObject.BuildList
       08/05/03 - DanB - added OctreeAABBIntersect (Matheus Degiovani)
       07/05/03 - SG - Added TGLSMDVectorFile.SaveToFile method and [read,write] capabilities
       17/04/03 - SG - Added TGLMeshObjectList.FindMeshByName method
       01/04/03 - SG - Fixed TGLBaseMesh.Assign
       13/02/03 - DanB - added AxisAlignedDimensionsUnscaled
       03/02/03 - EG - Faster PrepareBuildList logic
       31/01/03 - EG - Added MaterialCache logic
       30/01/03 - EG - Fixed color array enable/disable (Nelson Chu),
                          Normals extraction and extraction standardization
       27/01/03 - EG - Assign support, fixed MorphableMeshObjects persistence
       16/01/03 - EG - Updated multiples Bones per vertex transformation code,
                          now makes use of CVAs
       14/01/03 - EG - Added DisableOpenGLArrays
       09/01/03 - EG - Added Clear methods for MeshObjects
       25/11/02 - EG - Colors and TexCoords lists now disabled if ignoreMaterials is true
       23/10/02 - EG - Faster .GTS and .PLY imports (parsing)
       22/10/02 - EG - Added actor options, fixed skeleton normals transform (thx Marcus)
       21/10/02 - EG - Read support for .GTS (GNU Triangulated Surface library)
       18/10/02 - EG - FindExtByIndex (Adem)
       17/10/02 - EG - TGLSTLVectorFile moved to new GLFileSTL unit
       04/09/02 - EG - Fixed TGLBaseMesh.AxisAlignedDimensions
       23/08/02 - EG - Added TGLBaseMesh.Visible
       23/07/02 - EG - TGLBaseMesh.LoadFromStream fix (D. Angilella)
       13/07/02 - EG - AutoCenter on barycenter
       22/03/02 - EG - TGLAnimationControler basics now functional
       13/03/02 - EG - Octree support (experimental)
       18/02/02 - EG - Fixed persistence of skeletal meshes
       04/01/02 - EG - Added basic RayCastIntersect implementation
       17/12/01 - EG - Upgraded TGLActor.Synchronize (smooth transitions support)
       30/11/01 - EG - Added smooth transitions (based on Mrqzzz code)
       14/09/01 - EG - Use of vFileStreamClass
       18/08/01 - EG - Added TriangleCount methods, STL export, PLY import
       15/08/01 - EG - FaceGroups can now be rendered by material group
                          (activate with RenderingOption "moroGroupByMaterial")
       14/08/01 - EG - Added TGLSkeletonBoneList and support for skeleton with
                          multiple root bones, updated SMD loader
       13/08/01 - EG - Improved/fixed SMD loader
       12/08/01 - EG - Completely rewritten handles management,
                          Fixed TActorAnimation.Assign,
                          Fixed persistence
       08/08/01 - EG - Added TGLBaseMesh.AxisAlignedDimensions
       19/07/01 - EG - AutoCentering is now a property of TGLBaseMesh,
                          3DS loader no longer auto-centers,
                          Added ExtractTriangles and related methods
       18/07/01 - EG - VisibilityCulling compatibility changes
       19/06/01 - EG - StrToFloat outlawed and replaced by StrToFloatDef
       25/03/01 - EG - Added TGLAnimationControler
       18/03/01 - EG - Added basic Skeleton structures & SMD importer
       16/03/01 - EG - Introduced new PersistentClasses
       15/03/01 - EG - Fix in TActorAnimation.SetEndFrame (thx David Costa)
       08/03/01 - EG - TGL3DSVectorFile now loads materials for TGLBaseMesh
       26/02/01 - EG - Added TBaseMeshObject & BuildNormals, MD2 normals auto-builded
       21/02/01 - EG - Now XOpenGL based (multitexture)
       15/01/01 - EG - Added Translate methods
       10/01/01 - EG - Fixed in TGLBaseMesh.DoRender for RenderChildren states
       08/01/01 - EG - Fixed TGLBaseMesh.BuildList messup of attrib states
       22/12/00 - EG - Fixed non-interpolated TGLActor animation (was freezing),
                          Fixed TGLBaseMesh.DoRender messup of attrib states
       18/12/00 - EG - TFGIndexTexCoordList now supports normals (automatically),
                          NormalsOrientation code moved to TGLBaseMesh
       11/12/00 - EG - Fix for NormalOrientation (3DS importer)
       06/12/00 - EG - Added PrepareBuildList mechanism
       08/10/00 - EG - Removed TGLOBJVectorFile, use GLFileOBJ instead
       13/08/00 - EG - Enhancements for Portal Rendering support,
                          Added utility methods & triangle fans
       10/08/00 - EG - Added CurrentAnimation, fixed TGLMeshObject.GetExtents
       21/07/00 - EG - Vastly improved memory use and mechanisms for MD2/TGLActor
       19/07/00 - EG - Introduced enhanced mesh structure
       16/07/00 - EG - Made use of new TGLDataFile class
       15/07/00 - EG - FreeForm can now handle 3DS files with multiple textures,
                          Added TGLBaseMesh.GetExtents
       28/06/00 - EG - Support for "ObjectStyle"
       23/06/00 - EG - Reversed "t" texture coord for MD2,
                          TActorAnimations can now load/save
       21/06/00 - EG - Added frame change events to TGLActor,
                          Added TActorAnimations collection
       19/06/00 - EG - Completed smooth movement interpolation for TGLActor
       07/06/00 - EG - TGLVectorFile now longers assumes a TGLFreeForm as Owner,
                          Added generic TGLVectorFile.LoadFromFile
       26/05/00 - EG - Removed dependency to GLObjects,
                          TGLFreeForm now may use InterleavedArrays instead of
                          IndexedArrays (better BuildList compatibility)
       22/04/00 - EG - Fixed Material handlings in TGLFreeForm, inverted CCW/CW
                          convention for 3DS Release3
       11/04/00 - EG - Removed unnecessary code in finalization (thanks Uwe)
       09/02/00 - EG - Creation from split of GLObjects,
                          fixed class registrations and formats unregistration
  
}
unit GLVectorFileObjects;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils, Types,
  GLScene, OpenGLTokens, GLVectorGeometry, GLTexture,
  GLMaterial, GLMesh, GLVectorLists, GLPersistentClasses, GLOctree, GLGeometryBB,
  GLApplicationFileIO, GLSilhouette, GLContext, GLColor, GLRenderContextInfo,
  GLCoordinates, GLBaseClasses, GLTextureFormat;

type

  TGLMeshObjectList = class;
  TGLFaceGroups = class;

  // TMeshAutoCentering

  TMeshAutoCentering = (macCenterX, macCenterY, macCenterZ, macUseBarycenter,
    macRestorePosition);
  TMeshAutoCenterings = set of TMeshAutoCentering;

  // TGLMeshObjectMode

  TGLMeshObjectMode = (momTriangles, momTriangleStrip, momFaceGroups);

  // TBaseMeshObject

  { A base class for mesh objects.
     The class introduces a set of vertices and normals for the object but
     does no rendering of its own. }
  TBaseMeshObject = class(TPersistentObject)
  private

    FName: string;
    FVertices: TAffineVectorList;
    FNormals: TAffineVectorList;
    FVisible: boolean;

  protected

    procedure SetVertices(const val: TAffineVectorList);
    procedure SetNormals(const val: TAffineVectorList);

    procedure ContributeToBarycenter(var currentSum: TAffineVector;
      var nb: integer); dynamic;

  public

    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    { Clears all mesh object data, submeshes, facegroups, etc. }
    procedure Clear; dynamic;

    { Translates all the vertices by the given delta. }
    procedure Translate(const delta: TAffineVector); dynamic;
    { Builds (smoothed) normals for the vertex list.
       If normalIndices is nil, the method assumes a bijection between
       vertices and normals sets, and when performed, Normals and Vertices
       list will have the same number of items (whatever previously was in
       the Normals list is ignored/removed).
       If normalIndices is defined, normals will be added to the list and
       their indices will be added to normalIndices. Already defined
       normals and indices are preserved.
       The only valid modes are currently momTriangles and momTriangleStrip
       (ie. momFaceGroups not supported). }
    procedure BuildNormals(vertexIndices: TIntegerList; mode: TGLMeshObjectMode;
      normalIndices: TIntegerList = nil);
    { Extracts all mesh triangles as a triangles list.
       The resulting list size is a multiple of 3, each group of 3 vertices
       making up and independant triangle.
       The returned list can be used independantly from the mesh object
       (all data is duplicated) and should be freed by caller.
       If texCoords is specified, per vertex texture coordinates will be
       placed there, when available. }
    function ExtractTriangles(texCoords: TAffineVectorList = nil;
      normals: TAffineVectorList = nil): TAffineVectorList; dynamic;

    property Name: string read FName write FName;
    property Visible: boolean read FVisible write FVisible;
    property Vertices: TAffineVectorList read FVertices write SetVertices;
    property Normals: TAffineVectorList read FNormals write SetNormals;
  end;

  TGLSkeletonFrameList = class;

  TGLSkeletonFrameTransform = (sftRotation, sftQuaternion);

  // TGLSkeletonFrame

    { Stores position and rotation for skeleton joints.
       If you directly alter some values, make sure to call FlushLocalMatrixList
       so that the local matrices will be recalculated (the call to Flush does
       not recalculate the matrices, but marks the current ones as dirty). }
  TGLSkeletonFrame = class(TPersistentObject)
  private

    FOwner: TGLSkeletonFrameList;
    FName: string;
    FPosition: TAffineVectorList;
    FRotation: TAffineVectorList;
    FQuaternion: TQuaternionList;
    FLocalMatrixList: PMatrixArray;
    FTransformMode: TGLSkeletonFrameTransform;

  protected

    procedure SetPosition(const val: TAffineVectorList);
    procedure SetRotation(const val: TAffineVectorList);
    procedure SetQuaternion(const val: TQuaternionList);

  public

    constructor CreateOwned(aOwner: TGLSkeletonFrameList);
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    property Owner: TGLSkeletonFrameList read FOwner;
    property Name: string read FName write FName;
    { Position values for the joints. }
    property Position: TAffineVectorList read FPosition write SetPosition;
    { Rotation values for the joints. }
    property Rotation: TAffineVectorList read FRotation write SetRotation;
    { Quaternions are an alternative to Euler rotations to build the
       global matrices for the skeleton bones. }
    property Quaternion: TQuaternionList read FQuaternion write SetQuaternion;
    { TransformMode indicates whether to use Rotation or Quaternion to build
       the local transform matrices. }
    property TransformMode: TGLSkeletonFrameTransform
      read FTransformMode write FTransformMode;

    { Calculate or retrieves an array of local bone matrices.
       This array is calculated on the first call after creation, and the
       first call following a FlushLocalMatrixList. Subsequent calls return
       the same arrays. }
    function LocalMatrixList: PMatrixArray;
    { Flushes (frees) then LocalMatrixList data.
       Call this function to allow a recalculation of local matrices. }
    procedure FlushLocalMatrixList;
    // As the name states; Convert Quaternions to Rotations or vice-versa.
    procedure ConvertQuaternionsToRotations(KeepQuaternions: boolean = True);
    procedure ConvertRotationsToQuaternions(KeepRotations: boolean = True);
  end;

  // TGLSkeletonFrameList

  { A list of TGLSkeletonFrame objects. }
  TGLSkeletonFrameList = class(TPersistentObjectList)
  private

    FOwner: TPersistent;

  protected

    function GetSkeletonFrame(Index: integer): TGLSkeletonFrame;

  public

    constructor CreateOwned(AOwner: TPersistent);
    destructor Destroy; override;

    procedure ReadFromFiler(reader: TVirtualReader); override;

    // As the name states; Convert Quaternions to Rotations or vice-versa.
    procedure ConvertQuaternionsToRotations(KeepQuaternions: boolean = True;
      SetTransformMode: boolean = True);
    procedure ConvertRotationsToQuaternions(KeepRotations: boolean = True;
      SetTransformMode: boolean = True);

    property Owner: TPersistent read FOwner;
    procedure Clear; override;
    property Items[Index: integer]: TGLSkeletonFrame read GetSkeletonFrame; default;
  end;

  TGLSkeleton = class;
  TGLSkeletonBone = class;

  // TGLSkeletonBoneList

  { A list of skeleton bones. }
  TGLSkeletonBoneList = class(TPersistentObjectList)
  private

    FSkeleton: TGLSkeleton; // not persistent

  protected

    FGlobalMatrix: TMatrix;

    function GetSkeletonBone(Index: integer): TGLSkeletonBone;
    procedure AfterObjectCreatedByReader(Sender: TObject); override;

  public

    constructor CreateOwned(aOwner: TGLSkeleton);
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    property Skeleton: TGLSkeleton read FSkeleton;
    property Items[Index: integer]: TGLSkeletonBone read GetSkeletonBone; default;

    { Returns a bone by its BoneID, nil if not found. }
    function BoneByID(anID: integer): TGLSkeletonBone; virtual;
    { Returns a bone by its Name, nil if not found. }
    function BoneByName(const aName: string): TGLSkeletonBone; virtual;
    { Number of bones (including all children and self). }

    function BoneCount: integer;

    // Render skeleton wireframe
    procedure BuildList(var mrci: TGLRenderContextInfo); virtual; abstract;
    procedure PrepareGlobalMatrices; virtual;
  end;

  // TGLSkeletonRootBoneList

  { This list store skeleton root bones exclusively. }
  TGLSkeletonRootBoneList = class(TGLSkeletonBoneList)
  private


  protected


  public

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    // Render skeleton wireframe
    procedure BuildList(var mrci: TGLRenderContextInfo); override;

    property GlobalMatrix: TMatrix read FGlobalMatrix write FGlobalMatrix;
  end;

  // TGLSkeletonBone

    { A skeleton bone or node and its children.
       This class is the base item of the bones hierarchy in a skeletal model.
       The joint values are stored in a TGLSkeletonFrame, but the calculated bone
       matrices are stored here. }
  TGLSkeletonBone = class(TGLSkeletonBoneList)
  private

    FOwner: TGLSkeletonBoneList; // indirectly persistent
    FBoneID: integer;
    FName: string;
    FColor: cardinal;

  protected

    function GetSkeletonBone(Index: integer): TGLSkeletonBone;
    procedure SetColor(const val: cardinal);

  public

    constructor CreateOwned(aOwner: TGLSkeletonBoneList);
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    // Render skeleton wireframe
    procedure BuildList(var mrci: TGLRenderContextInfo); override;

    property Owner: TGLSkeletonBoneList read FOwner;
    property Name: string read FName write FName;
    property BoneID: integer read FBoneID write FBoneID;
    property Color: cardinal read FColor write SetColor;
    property Items[Index: integer]: TGLSkeletonBone read GetSkeletonBone; default;

    { Returns a bone by its BoneID, nil if not found. }
    function BoneByID(anID: integer): TGLSkeletonBone; override;
    function BoneByName(const aName: string): TGLSkeletonBone; override;

    { Set the bone's matrix. Becareful using this. }
    procedure SetGlobalMatrix(Matrix: TMatrix); // Ragdoll
    { Set the bone's GlobalMatrix. Used for Ragdoll. }
    procedure SetGlobalMatrixForRagDoll(RagDollMatrix: TMatrix); // Ragdoll

    { Calculates the global matrix for the bone and its sub-bone.
       Call this function directly only the RootBone. }
    procedure PrepareGlobalMatrices; override;
    { Global Matrix for the bone in the current frame.
       Global matrices must be prepared by invoking PrepareGlobalMatrices
       on the root bone. }
    property GlobalMatrix: TMatrix read FGlobalMatrix;

    { Free all sub bones and reset BoneID and Name. }
    procedure Clean; override;
  end;

  TGLSkeletonColliderList = class;

  // TGLSkeletonCollider

  { A general class storing the base level info required for skeleton
     based collision methods. This class is meant to be inherited from
     to create skeleton driven Verlet Constraints, ODE Geoms, etc.
     Overriden classes should be named as TSCxxxxx. }
  TGLSkeletonCollider = class(TPersistentObject)
  private

    FOwner: TGLSkeletonColliderList;
    FBone: TGLSkeletonBone;
    FBoneID: integer;
    FLocalMatrix, FGlobalMatrix: TMatrix;
    FAutoUpdate: boolean;

  protected

    procedure SetBone(const val: TGLSkeletonBone);
    procedure SetLocalMatrix(const val: TMatrix);

  public

    constructor Create; override;
    constructor CreateOwned(AOwner: TGLSkeletonColliderList);
    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;
    { This method is used to align the colliders and their
       derived objects to their associated skeleton bone.
       Override to set up descendant class alignment properties. }
    procedure AlignCollider; virtual;

    property Owner: TGLSkeletonColliderList read FOwner;
    // The bone that this collider associates with.
    property Bone: TGLSkeletonBone read FBone write SetBone;
    { Offset and orientation of the collider in the associated
       bone's space. }
    property LocalMatrix: TMatrix read FLocalMatrix write SetLocalMatrix;
    { Global offset and orientation of the collider. This
       gets set in the AlignCollider method. }
    property GlobalMatrix: TMatrix read FGlobalMatrix;
    property AutoUpdate: boolean read FAutoUpdate write FAutoUpdate;
  end;

  // TGLSkeletonColliderList

  { List class for storing TGLSkeletonCollider objects. }
  TGLSkeletonColliderList = class(TPersistentObjectList)
  private

    FOwner: TPersistent;

  protected

    function GetSkeletonCollider(index: integer): TGLSkeletonCollider;

  public

    constructor CreateOwned(AOwner: TPersistent);
    destructor Destroy; override;

    procedure ReadFromFiler(reader: TVirtualReader); override;
    procedure Clear; override;
    { Calls AlignCollider for each collider in the list. }
    procedure AlignColliders;

    property Owner: TPersistent read FOwner;
    property Items[Index: integer]: TGLSkeletonCollider read GetSkeletonCollider;
      default;
  end;

  TGLBaseMesh = class;

  // TBlendedLerpInfo

  { Small structure to store a weighted lerp for use in blending. }
  TBlendedLerpInfo = record
    frameIndex1, frameIndex2: integer;
    lerpFactor: single;
    weight: single;
    externalPositions: TAffineVectorList;
    externalRotations: TAffineVectorList;
    externalQuaternions: TQuaternionList;
  end;

  // TGLSkeleton

    { Main skeleton object.
       This class stores the bones hierarchy and animation frames. 
       It is also responsible for maintaining the "CurrentFrame" and allowing
       various frame blending operations. }
  TGLSkeleton = class(TPersistentObject)
  private

    FOwner: TGLBaseMesh;
    FRootBones: TGLSkeletonRootBoneList;
    FFrames: TGLSkeletonFrameList;
    FCurrentFrame: TGLSkeletonFrame; // not persistent
    FBonesByIDCache: TList;
    FColliders: TGLSkeletonColliderList;
    FRagDollEnabled: boolean; // ragdoll
    FMorphInvisibleParts: boolean;

  protected

    procedure SetRootBones(const val: TGLSkeletonRootBoneList);
    procedure SetFrames(const val: TGLSkeletonFrameList);
    function GetCurrentFrame: TGLSkeletonFrame;
    procedure SetCurrentFrame(val: TGLSkeletonFrame);
    procedure SetColliders(const val: TGLSkeletonColliderList);

  public

    constructor CreateOwned(AOwner: TGLBaseMesh);
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    property Owner: TGLBaseMesh read FOwner;
    property RootBones: TGLSkeletonRootBoneList read FRootBones write SetRootBones;
    property Frames: TGLSkeletonFrameList read FFrames write SetFrames;
    property CurrentFrame: TGLSkeletonFrame read GetCurrentFrame write SetCurrentFrame;
    property Colliders: TGLSkeletonColliderList read FColliders write SetColliders;

    procedure FlushBoneByIDCache;
    function BoneByID(anID: integer): TGLSkeletonBone;
    function BoneByName(const aName: string): TGLSkeletonBone;
    function BoneCount: integer;

    procedure MorphTo(frameIndex: integer); overload;
    procedure MorphTo(frame: TGLSkeletonFrame); overload;
    procedure Lerp(frameIndex1, frameIndex2: integer; lerpFactor: single);
    procedure BlendedLerps(const lerpInfos: array of TBlendedLerpInfo);

    { Linearly removes the translation component between skeletal frames.
       This function will compute the translation of the first bone (index 0)
       and linearly subtract this translation in all frames between startFrame
       and endFrame. Its purpose is essentially to remove the 'slide' that
       exists in some animation formats (f.i. SMD). }
    procedure MakeSkeletalTranslationStatic(startFrame, endFrame: integer);
    { Removes the absolute rotation component of the skeletal frames.
       Some formats will store frames with absolute rotation information,
       if this correct if the animation is the "main" animation.
       This function removes that absolute information, making the animation
       frames suitable for blending purposes. }
    procedure MakeSkeletalRotationDelta(startFrame, endFrame: integer);

    { Applies current frame to morph all mesh objects. }
    procedure MorphMesh(normalize: boolean);

    { Copy bone rotations from reference skeleton. }
    procedure Synchronize(reference: TGLSkeleton);
    { Release bones and frames info. }
    procedure Clear;
    { Backup and prepare the BoneMatrixInvertedMeshes to use with ragdolls }
    procedure StartRagdoll; // ragdoll
    { Restore the BoneMatrixInvertedMeshes to stop the ragdoll }
    procedure StopRagdoll; // ragdoll

    { Turning this option off (by default) alows to increase FPS,
       but may break backwards-compatibility, because some may choose to
       attach other objects to invisible parts. }
    property MorphInvisibleParts: boolean read FMorphInvisibleParts
      write FMorphInvisibleParts;
  end;

  // TGLMeshObjectRenderingOption

  { Rendering options per TGLMeshObject.

   moroGroupByMaterial : if set, the facegroups will be rendered by material
     in batchs, this will optimize rendering by reducing material switches, but
     also implies that facegroups will not be rendered in the order they are in
     the list.
    }
  TGLMeshObjectRenderingOption = (moroGroupByMaterial);
  TGLMeshObjectRenderingOptions = set of TGLMeshObjectRenderingOption;

  TVBOBuffer = (vbVertices, vbNormals, vbColors, vbTexCoords,
    vbLightMapTexCoords,
    vbTexCoordsEx);
  TVBOBuffers = set of TVBOBuffer;

  // TGLMeshObject

  { Base mesh class.
     Introduces base methods and properties for mesh objects.
     Subclasses are named "TMOxxx". }
  TGLMeshObject = class(TBaseMeshObject)
  private

    FOwner: TGLMeshObjectList;
    FExtentCacheRevision: cardinal;
    FTexCoords: TAffineVectorList; // provision for 3D textures
    FLightMapTexCoords: TAffineVectorList; // reserved for 2D surface needs
    FColors: TVectorList;
    FFaceGroups: TGLFaceGroups;
    FMode: TGLMeshObjectMode;
    FRenderingOptions: TGLMeshObjectRenderingOptions;
    FArraysDeclared: boolean; // not persistent
    FLightMapArrayEnabled: boolean; // not persistent
    FLastLightMapIndex: integer; // not persistent
    FTexCoordsEx: TList;
    FBinormalsTexCoordIndex: integer;
    FTangentsTexCoordIndex: integer;
    FLastXOpenGLTexMapping: cardinal;
    FUseVBO: boolean;
    FVerticesVBO: TGLVBOHandle;
    FNormalsVBO: TGLVBOHandle;
    FColorsVBO: TGLVBOHandle;
    FTexCoordsVBO: array of TGLVBOHandle;
    FLightmapTexCoordsVBO: TGLVBOHandle;
    FValidBuffers: TVBOBuffers;
    FExtentCache: TAABB;

    procedure SetUseVBO(const Value: boolean);
    procedure SetValidBuffers(Value: TVBOBuffers);
  protected

    procedure SetTexCoords(const val: TAffineVectorList);
    procedure SetLightmapTexCoords(const val: TAffineVectorList);
    procedure SetColors(const val: TVectorList);

    procedure BufferArrays;

    procedure DeclareArraysToOpenGL(var mrci: TGLRenderContextInfo;
      evenIfAlreadyDeclared: boolean = False);
    procedure DisableOpenGLArrays(var mrci: TGLRenderContextInfo);

    procedure EnableLightMapArray(var mrci: TGLRenderContextInfo);
    procedure DisableLightMapArray(var mrci: TGLRenderContextInfo);

    procedure SetTexCoordsEx(index: integer; const val: TVectorList);
    function GetTexCoordsEx(index: integer): TVectorList;

    procedure SetBinormals(const val: TVectorList);
    function GetBinormals: TVectorList;
    procedure SetBinormalsTexCoordIndex(const val: integer);
    procedure SetTangents(const val: TVectorList);
    function GetTangents: TVectorList;
    procedure SetTangentsTexCoordIndex(const val: integer);

    property ValidBuffers: TVBOBuffers read FValidBuffers write SetValidBuffers;
  public

    { Creates, assigns Owner and adds to list. }
    constructor CreateOwned(AOwner: TGLMeshObjectList);
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure Clear; override;

    function ExtractTriangles(texCoords: TAffineVectorList = nil;
      normals: TAffineVectorList = nil): TAffineVectorList; override;
    { Returns number of triangles in the mesh object. }
    function TriangleCount: integer; dynamic;

    procedure PrepareMaterialLibraryCache(matLib: TGLMaterialLibrary);
    procedure DropMaterialLibraryCache;

    { Prepare the texture and materials before rendering.
       Invoked once, before building the list and NOT while building the list. }
    procedure PrepareBuildList(var mrci: TGLRenderContextInfo); virtual;
    // Similar to regular scene object's BuildList method
    procedure BuildList(var mrci: TGLRenderContextInfo); virtual;

    // The extents of the object (min and max coordinates)
    procedure GetExtents(out min, max: TAffineVector); overload; virtual;
    procedure GetExtents(out aabb: TAABB); overload; virtual;

    // Barycenter from vertices data
    function GetBarycenter: TVector;

    // Precalculate whatever is needed for rendering, called once
    procedure Prepare; dynamic;

    function PointInObject(const aPoint: TAffineVector): boolean; virtual;

    // Returns the triangle data for a given triangle
    procedure GetTriangleData(tri: integer; list: TAffineVectorList;
      var v0, v1, v2: TAffineVector); overload;
    procedure GetTriangleData(tri: integer; list: TVectorList;
      var v0, v1, v2: TVector); overload;

    // Sets the triangle data of a given triangle
    procedure SetTriangleData(tri: integer; list: TAffineVectorList;
      const v0, v1, v2: TAffineVector); overload;
    procedure SetTriangleData(tri: integer; list: TVectorList;
      const v0, v1, v2: TVector); overload;

    { Build the tangent space from the mesh object's vertex, normal
       and texcoord data, filling the binormals and tangents where
       specified. }
    procedure BuildTangentSpace(buildBinormals: boolean = True;
      buildTangents: boolean = True);

    property Owner: TGLMeshObjectList read FOwner;
    property Mode: TGLMeshObjectMode read FMode write FMode;
    property TexCoords: TAffineVectorList read FTexCoords write SetTexCoords;
    property LightMapTexCoords: TAffineVectorList
      read FLightMapTexCoords write SetLightMapTexCoords;
    property Colors: TVectorList read FColors write SetColors;
    property FaceGroups: TGLFaceGroups read FFaceGroups;
    property RenderingOptions: TGLMeshObjectRenderingOptions
      read FRenderingOptions write FRenderingOptions;

    { If set, rendering will use VBO's instead of vertex arrays. }
    property UseVBO: boolean read FUseVBO write SetUseVBO;

    { The TexCoords Extension is a list of vector lists that are used
       to extend the vertex data applied during rendering.

       The lists are applied to the GL_TEXTURE0_ARB + index texture
       environment. This means that if TexCoordsEx 0 or 1 have data it
       will override the TexCoords or LightMapTexCoords repectively.
       Lists are created on demand, meaning that if you request
       TexCoordsEx[4] it will create the list up to and including 4.
       The extensions are only applied to the texture environment if
       they contain data. }
    property TexCoordsEx[index: integer]: TVectorList
      read GetTexCoordsEx write SetTexCoordsEx;

    { A TexCoordsEx list wrapper for binormals usage,
       returns TexCoordsEx[BinormalsTexCoordIndex]. }
    property Binormals: TVectorList read GetBinormals write SetBinormals;
    { A TexCoordsEx list wrapper for tangents usage,
       returns TexCoordsEx[BinormalsTexCoordIndex]. }
    property Tangents: TVectorList read GetTangents write SetTangents;
    // Specify the texcoord extension index for binormals (default = 2)
    property BinormalsTexCoordIndex: integer
      read FBinormalsTexCoordIndex write SetBinormalsTexCoordIndex;
    // Specify the texcoord extension index for tangents (default = 3)
    property TangentsTexCoordIndex: integer
      read FTangentsTexCoordIndex write SetTangentsTexCoordIndex;

  end;

  // TGLMeshObjectList

  { A list of TGLMeshObject objects. }
  TGLMeshObjectList = class(TPersistentObjectList)
  private

    FOwner: TGLBaseMesh;

    { Resturns True if all its MeshObjects use VBOs. }
    function GetUseVBO: boolean;
    procedure SetUseVBO(const Value: boolean);
  protected

    function GetMeshObject(Index: integer): TGLMeshObject;

  public

    constructor CreateOwned(aOwner: TGLBaseMesh);
    destructor Destroy; override;

    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure PrepareMaterialLibraryCache(matLib: TGLMaterialLibrary);
    procedure DropMaterialLibraryCache;

    { Prepare the texture and materials before rendering.
       Invoked once, before building the list and NOT while building the list. }
    procedure PrepareBuildList(var mrci: TGLRenderContextInfo); virtual;
    // Similar to regular scene object's BuildList method
    procedure BuildList(var mrci: TGLRenderContextInfo); virtual;

    procedure MorphTo(morphTargetIndex: integer);
    procedure Lerp(morphTargetIndex1, morphTargetIndex2: integer;
      lerpFactor: single);
    function MorphTargetCount: integer;

    procedure GetExtents(out min, max: TAffineVector);
    procedure Translate(const delta: TAffineVector);
    function ExtractTriangles(texCoords: TAffineVectorList = nil;
      normals: TAffineVectorList = nil): TAffineVectorList;
    { Returns number of triangles in the meshes of the list. }
    function TriangleCount: integer;

    { Build the tangent space from the mesh object's vertex, normal
       and texcoord data, filling the binormals and tangents where
       specified. }
    procedure BuildTangentSpace(buildBinormals: boolean = True;
      buildTangents: boolean = True);

    { If set, rendering will use VBO's instead of vertex arrays.
       Resturns True if all its MeshObjects use VBOs. }
    property UseVBO: boolean read GetUseVBO write SetUseVBO;

    // Precalculate whatever is needed for rendering, called once
    procedure Prepare; dynamic;

    function FindMeshByName(MeshName: string): TGLMeshObject;

    property Owner: TGLBaseMesh read FOwner;
    procedure Clear; override;
    property Items[Index: integer]: TGLMeshObject read GetMeshObject; default;
  end;

  TGLMeshObjectListClass = class of TGLMeshObjectList;

  TMeshMorphTargetList = class;

  // TMeshMorphTarget

  { A morph target, stores alternate lists of vertices and normals. }
  TMeshMorphTarget = class(TBaseMeshObject)
  private

    FOwner: TMeshMorphTargetList;

  protected


  public

    constructor CreateOwned(AOwner: TMeshMorphTargetList);
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    property Owner: TMeshMorphTargetList read FOwner;
  end;

  // TMeshMorphTargetList

  { A list of TMeshMorphTarget objects. }
  TMeshMorphTargetList = class(TPersistentObjectList)
  private

    FOwner: TPersistent;

  protected

    function GetMeshMorphTarget(Index: integer): TMeshMorphTarget;

  public

    constructor CreateOwned(AOwner: TPersistent);
    destructor Destroy; override;

    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure Translate(const delta: TAffineVector);

    property Owner: TPersistent read FOwner;
    procedure Clear; override;
    property Items[Index: integer]: TMeshMorphTarget read GetMeshMorphTarget; default;
  end;

  // TMorphableMeshObject

  { Mesh object with support for morph targets.
     The morph targets allow to change vertices and normals according to pre-
     existing "morph targets". }
  TMorphableMeshObject = class(TGLMeshObject)
  private

    FMorphTargets: TMeshMorphTargetList;

  protected


  public

    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure Clear; override;

    procedure Translate(const delta: TAffineVector); override;

    procedure MorphTo(morphTargetIndex: integer); virtual;
    procedure Lerp(morphTargetIndex1, morphTargetIndex2: integer;
      lerpFactor: single); virtual;

    property MorphTargets: TMeshMorphTargetList read FMorphTargets;
  end;

  // TVertexBoneWeight

  TVertexBoneWeight = packed record
    BoneID: integer;
    Weight: single;
  end;

  TVertexBoneWeightArray = array[0..MaxInt div (2 * SizeOf(TVertexBoneWeight))] of TVertexBoneWeight;
  PVertexBoneWeightArray = ^TVertexBoneWeightArray;
  TVerticesBoneWeights = array[0..MaxInt div (2 * SizeOf(PVertexBoneWeightArray))] of PVertexBoneWeightArray;
  PVerticesBoneWeights = ^TVerticesBoneWeights;
  TVertexBoneWeightDynArray = array of TVertexBoneWeight;

  // TGLSkeletonMeshObject

    { A mesh object with vertice bone attachments.
       The class adds per vertex bone weights to the standard morphable mesh.
       The TVertexBoneWeight structures are accessed via VerticesBonesWeights,
       they must be initialized by adjusting the BonesPerVertex and
       VerticeBoneWeightCount properties, you can also add vertex by vertex
       by using the AddWeightedBone method.
       When BonesPerVertex is 1, the weight is ignored (set to 1.0). }
  TGLSkeletonMeshObject = class(TMorphableMeshObject)
  private

    FVerticesBonesWeights: PVerticesBoneWeights;
    FVerticeBoneWeightCount, FVerticeBoneWeightCapacity: integer;
    FBonesPerVertex: integer;
    FLastVerticeBoneWeightCount, FLastBonesPerVertex: integer; // not persistent
    FBoneMatrixInvertedMeshes: TList; // not persistent
    FBackupInvertedMeshes: TList; // ragdoll
    procedure BackupBoneMatrixInvertedMeshes; // ragdoll
    procedure RestoreBoneMatrixInvertedMeshes; // ragdoll
  protected

    procedure SetVerticeBoneWeightCount(const val: integer);
    procedure SetVerticeBoneWeightCapacity(const val: integer);
    procedure SetBonesPerVertex(const val: integer);
    procedure ResizeVerticesBonesWeights;

  public

    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure Clear; override;

    property VerticesBonesWeights: PVerticesBoneWeights read FVerticesBonesWeights;
    property VerticeBoneWeightCount: integer
      read FVerticeBoneWeightCount write SetVerticeBoneWeightCount;
    property VerticeBoneWeightCapacity: integer
      read FVerticeBoneWeightCapacity write SetVerticeBoneWeightCapacity;
    property BonesPerVertex: integer read FBonesPerVertex write SetBonesPerVertex;

    function FindOrAdd(boneID: integer;
      const vertex, normal: TAffineVector): integer; overload;
    function FindOrAdd(const boneIDs: TVertexBoneWeightDynArray;
      const vertex, normal: TAffineVector): integer; overload;

    procedure AddWeightedBone(aBoneID: integer; aWeight: single);
    procedure AddWeightedBones(const boneIDs: TVertexBoneWeightDynArray);
    procedure PrepareBoneMatrixInvertedMeshes;
    procedure ApplyCurrentSkeletonFrame(normalize: boolean);

  end;

  // TGLFaceGroup

  { Describes a face group of a TGLMeshObject.
     Face groups should be understood as "a way to use mesh data to render
     a part or the whole mesh object".
     Subclasses implement the actual behaviours, and should have at least
     one "Add" method, taking in parameters all that is required to describe
     a single base facegroup element. }
  TGLFaceGroup = class(TPersistentObject)
  private

    FOwner: TGLFaceGroups;
    FMaterialName: string;
    FMaterialCache: TGLLibMaterial;
    FLightMapIndex: integer;
    FRenderGroupID: integer;
    // NOT Persistent, internal use only (rendering options)

  protected

    procedure AttachLightmap(lightMap: TGLTexture; var mrci: TGLRenderContextInfo);
    procedure AttachOrDetachLightmap(var mrci: TGLRenderContextInfo);

  public

    constructor CreateOwned(AOwner: TGLFaceGroups); virtual;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure PrepareMaterialLibraryCache(matLib: TGLMaterialLibrary);
    procedure DropMaterialLibraryCache;

    procedure BuildList(var mrci: TGLRenderContextInfo); virtual; abstract;

    { Add to the list the triangles corresponding to the facegroup.
       This function is used by TGLMeshObjects ExtractTriangles to retrieve
       all the triangles in a mesh. }
    procedure AddToTriangles(aList: TAffineVectorList;
      aTexCoords: TAffineVectorList = nil; aNormals: TAffineVectorList = nil);
      dynamic;
    { Returns number of triangles in the facegroup. }
    function TriangleCount: integer; dynamic; abstract;
    { Reverses the rendering order of faces.
       Default implementation does nothing }
    procedure Reverse; dynamic;

    // Precalculate whatever is needed for rendering, called once
    procedure Prepare; dynamic;

    property Owner: TGLFaceGroups read FOwner write FOwner;
    property MaterialName: string read FMaterialName write FMaterialName;
    property MaterialCache: TGLLibMaterial read FMaterialCache;
    { Index of lightmap in the lightmap library. }
    property LightMapIndex: integer read FLightMapIndex write FLightMapIndex;
  end;

  // TGLFaceGroupMeshMode

  { Known descriptions for face group mesh modes.
     - fgmmTriangles : issue all vertices with GL_TRIANGLES.
     - fgmmTriangleStrip : issue all vertices with GL_TRIANGLE_STRIP.
     - fgmmFlatTriangles : same as fgmmTriangles, but take advantage of having
        the same normal for all vertices of a triangle.
     - fgmmTriangleFan : issue all vertices with GL_TRIANGLE_FAN.
     - fgmmQuads : issue all vertices with GL_QUADS. }
  TGLFaceGroupMeshMode = (fgmmTriangles, fgmmTriangleStrip, fgmmFlatTriangles,
    fgmmTriangleFan, fgmmQuads);

  // TFGVertexIndexList

  { A face group based on an indexlist.
     The index list refers to items in the mesh object (vertices, normals, etc.),
     that are all considered in sync, the render is obtained issueing the items
     in the order given by the vertices. }
  TFGVertexIndexList = class(TGLFaceGroup)
  private

    FVertexIndices: TIntegerList;
    FIndexVBO: TGLVBOElementArrayHandle;
    FMode: TGLFaceGroupMeshMode;

    procedure SetupVBO;
    procedure InvalidateVBO;
  protected

    procedure SetVertexIndices(const val: TIntegerList);

    procedure AddToList(Source, destination: TAffineVectorList;
      indices: TIntegerList);

  public

    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure BuildList(var mrci: TGLRenderContextInfo); override;
    procedure AddToTriangles(aList: TAffineVectorList;
      aTexCoords: TAffineVectorList = nil; aNormals: TAffineVectorList = nil);
      override;
    function TriangleCount: integer; override;
    procedure Reverse; override;

    procedure Add(idx: integer);
    procedure GetExtents(var min, max: TAffineVector);
    { If mode is strip or fan, convert the indices to triangle list indices. }
    procedure ConvertToList;

    // Return the normal from the 1st three points in the facegroup
    function GetNormal: TAffineVector;

    property Mode: TGLFaceGroupMeshMode read FMode write FMode;
    property VertexIndices: TIntegerList read FVertexIndices write SetVertexIndices;
  end;

  // TFGVertexNormalTexIndexList

  { Adds normals and texcoords indices.
     Allows very compact description of a mesh. The Normals ad TexCoords
     indices are optionnal, if missing (empty), VertexIndices will be used. }
  TFGVertexNormalTexIndexList = class(TFGVertexIndexList)
  private

    FNormalIndices: TIntegerList;
    FTexCoordIndices: TIntegerList;

  protected

    procedure SetNormalIndices(const val: TIntegerList);
    procedure SetTexCoordIndices(const val: TIntegerList);

  public

    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure BuildList(var mrci: TGLRenderContextInfo); override;
    procedure AddToTriangles(aList: TAffineVectorList;
      aTexCoords: TAffineVectorList = nil; aNormals: TAffineVectorList = nil);
      override;

    procedure Add(vertexIdx, normalIdx, texCoordIdx: integer);

    property NormalIndices: TIntegerList read FNormalIndices write SetNormalIndices;
    property TexCoordIndices: TIntegerList read FTexCoordIndices
      write SetTexCoordIndices;
  end;

  // TFGIndexTexCoordList

  { Adds per index texture coordinates to its ancestor.
     Per index texture coordinates allows having different texture coordinates
     per triangle, depending on the face it is used in. }
  TFGIndexTexCoordList = class(TFGVertexIndexList)
  private

    FTexCoords: TAffineVectorList;

  protected

    procedure SetTexCoords(const val: TAffineVectorList);

  public

    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure BuildList(var mrci: TGLRenderContextInfo); override;
    procedure AddToTriangles(aList: TAffineVectorList;
      aTexCoords: TAffineVectorList = nil; aNormals: TAffineVectorList = nil);
      override;

    procedure Add(idx: integer; const texCoord: TAffineVector); overload;
    procedure Add(idx: integer; const s, t: single); overload;

    property TexCoords: TAffineVectorList read FTexCoords write SetTexCoords;
  end;

  // TGLFaceGroups

  { A list of TGLFaceGroup objects. }
  TGLFaceGroups = class(TPersistentObjectList)
  private

    FOwner: TGLMeshObject;

  protected

    function GetFaceGroup(Index: integer): TGLFaceGroup;

  public

    constructor CreateOwned(AOwner: TGLMeshObject);
    destructor Destroy; override;

    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure PrepareMaterialLibraryCache(matLib: TGLMaterialLibrary);
    procedure DropMaterialLibraryCache;

    property Owner: TGLMeshObject read FOwner;
    procedure Clear; override;
    property Items[Index: integer]: TGLFaceGroup read GetFaceGroup; default;

    procedure AddToTriangles(aList: TAffineVectorList;
      aTexCoords: TAffineVectorList = nil; aNormals: TAffineVectorList = nil);

    { Material Library of the owner TGLBaseMesh. }
    function MaterialLibrary: TGLMaterialLibrary;
    { Sort faces by material.
       Those without material first in list, followed by opaque materials,
       then transparent materials. }
    procedure SortByMaterial;
  end;

  // TMeshNormalsOrientation

  { Determines how normals orientation is defined in a mesh.
     - mnoDefault : uses default orientation
     - mnoInvert : inverse of default orientation
     - mnoAutoSolid : autocalculate to make the mesh globally solid
     - mnoAutoHollow : autocalculate to make the mesh globally hollow  }
  TMeshNormalsOrientation = (mnoDefault, mnoInvert);
  //, mnoAutoSolid, mnoAutoHollow);

  // TGLVectorFile

  { Abstract base class for different vector file formats.
     The actual implementation for these files (3DS, DXF..) must be done
     seperately. The concept for TGLVectorFile is very similar to TGraphic
     (see Delphi Help). }
  TGLVectorFile = class(TGLDataFile)
  private

    FNormalsOrientation: TMeshNormalsOrientation;

  protected

    procedure SetNormalsOrientation(const val: TMeshNormalsOrientation);
      virtual;

  public

    constructor Create(AOwner: TPersistent); override;

    function Owner: TGLBaseMesh;

    property NormalsOrientation: TMeshNormalsOrientation
      read FNormalsOrientation write SetNormalsOrientation;
  end;

  TGLVectorFileClass = class of TGLVectorFile;

  // TGLGLSMVectorFile

  { GLSM (GLScene Mesh) vector file.
     This corresponds to the 'native' GLScene format, and object persistence
     stream, which should be the 'fastest' of all formats to load, and supports
     all of GLScene features. }
  TGLGLSMVectorFile = class(TGLVectorFile)
  public

    class function Capabilities: TGLDataFileCapabilities; override;

    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
  end;

  // TGLBaseMesh

  { Base class for mesh objects. }
  TGLBaseMesh = class(TGLSceneObject)
  private

    FNormalsOrientation: TMeshNormalsOrientation;
    FMaterialLibrary: TGLMaterialLibrary;
    FLightmapLibrary: TGLMaterialLibrary;
    FAxisAlignedDimensionsCache: TVector;
    FBaryCenterOffsetChanged: boolean;
    FBaryCenterOffset: TVector;
    FUseMeshMaterials: boolean;
    FOverlaySkeleton: boolean;
    FIgnoreMissingTextures: boolean;
    FAutoCentering: TMeshAutoCenterings;
    FAutoScaling: TGLCoordinates;
    FMaterialLibraryCachesPrepared: boolean;
    FConnectivity: TObject;
    FLastLoadedFilename: string;

  protected

    FMeshObjects: TGLMeshObjectList; // a list of mesh objects
    FSkeleton: TGLSkeleton; // skeleton data & frames
    procedure SetUseMeshMaterials(const val: boolean);
    procedure SetMaterialLibrary(const val: TGLMaterialLibrary);
    procedure SetLightmapLibrary(const val: TGLMaterialLibrary);
    procedure SetNormalsOrientation(const val: TMeshNormalsOrientation);
    procedure SetOverlaySkeleton(const val: boolean);
    procedure SetAutoScaling(const Value: TGLCoordinates);
    procedure DestroyHandle; override;

    { Invoked after creating a TGLVectorFile and before loading.
       Triggered by LoadFromFile/Stream and AddDataFromFile/Stream.
       Allows to adjust/transfer subclass-specific features. }
    procedure PrepareVectorFile(aFile: TGLVectorFile); dynamic;

    { Invoked after a mesh has been loaded/added.
       Triggered by LoadFromFile/Stream and AddDataFromFile/Stream.
       Allows to adjust/transfer subclass-specific features. }
    procedure PrepareMesh; dynamic;

    { Recursively propagated to mesh object and facegroups.
       Notifies that they all can establish their material library caches. }
    procedure PrepareMaterialLibraryCache;
    { Recursively propagated to mesh object and facegroups.
       Notifies that they all should forget their material library caches. }
    procedure DropMaterialLibraryCache;

    { Prepare the texture and materials before rendering.
       Invoked once, before building the list and NOT while building the list,
       MaterialLibraryCache can be assumed to having been prepared if materials
       are active. Default behaviour is to prepare build lists for the
       meshobjects. }
    procedure PrepareBuildList(var mrci: TGLRenderContextInfo); dynamic;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function BarycenterOffset: TVector;
    function BarycenterPosition: TVector;
    function BarycenterAbsolutePosition: TVector; override;

    procedure BuildList(var rci: TGLRenderContextInfo); override;
    procedure DoRender(var rci: TGLRenderContextInfo;
      renderSelf, renderChildren: boolean); override;
    procedure StructureChanged; override;
    { Notifies that geometry data changed, but no re-preparation is needed.
       Using this method will usually be faster, but may result in incorrect
       rendering, reduced performance and/or invalid bounding box data
       (ie. invalid collision detection). Use with caution. }
    procedure StructureChangedNoPrepare;

    { BEWARE! Utterly inefficient implementation! }
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil; intersectNormal: PVector = nil): boolean;
      override;
    function GenerateSilhouette(
      const silhouetteParameters: TGLSilhouetteParameters): TGLSilhouette; override;

    { This method allows fast shadow volumes for GLActors.
       If your actor/mesh doesn't change, you don't need to call this.
       It basically caches the connectivity data.}
    procedure BuildSilhouetteConnectivityData;

    property MeshObjects: TGLMeshObjectList read FMeshObjects;
    property Skeleton: TGLSkeleton read FSkeleton;

    { Computes the extents of the mesh. }
    procedure GetExtents(out min, max: TAffineVector);
    { Computes the barycenter of the mesh. }
    function GetBarycenter: TAffineVector;
    { Invoked after a mesh has been loaded.
       Should auto-center according to the AutoCentering property. }
    procedure PerformAutoCentering; dynamic;
    { Invoked after a mesh has been loaded.
       Should auto-scale the vertices of the meshobjects to AutoScaling the property. }
    procedure PerformAutoScaling; dynamic;
    { Loads a vector file.
       A vector files (for instance a ".3DS") stores the definition of
       a mesh as well as materials property.
       Loading a file replaces the current one (if any). }
    procedure LoadFromFile(const filename: string); dynamic;
    { Loads a vector file from a stream.
       See LoadFromFile.
       The filename attribute is required to identify the type data you're
       streaming (3DS, OBJ, etc.) }
    procedure LoadFromStream(const filename: string; aStream: TStream); dynamic;
    { Saves to a vector file.
       Note that only some of the vector files formats can be written to
       by GLScene. }
    procedure SaveToFile(const fileName: string); dynamic;
    { Saves to a vector file in a stream.
       Note that only some of the vector files formats can be written to
       by GLScene. }
    procedure SaveToStream(const fileName: string; aStream: TStream); dynamic;

    { Loads additionnal data from a file.
       Additionnal data could be more animation frames or morph target.
       The VectorFile importer must be able to handle addition of data
       flawlessly. }
    procedure AddDataFromFile(const filename: string); dynamic;
    { Loads additionnal data from stream.
       See AddDataFromFile. }
    procedure AddDataFromStream(const filename: string; aStream: TStream);
      dynamic;

    { Returns the filename of the last loaded file, or a blank string if not
       file was loaded (or if the mesh was dinamically built). This does not
       take into account the data added to the mesh (through AddDataFromFile)
       or saved files.}
    function LastLoadedFilename: string;

    { Determines if a mesh should be centered and how.
       AutoCentering is performed  only  after loading a mesh, it has
       no effect on already loaded mesh data or when adding from a file/stream.
       If you want to alter mesh data, use direct manipulation methods
       (on the TGLMeshObjects). }
    property AutoCentering: TMeshAutoCenterings
      read FAutoCentering write FAutoCentering default [];

    { Scales vertices to a AutoScaling.
       AutoScaling is performed  only  after loading a mesh, it has
       no effect on already loaded mesh data or when adding from a file/stream.
       If you want to alter mesh data, use direct manipulation methods
       (on the TGLMeshObjects). }
    property AutoScaling: TGLCoordinates read FAutoScaling write FAutoScaling;

    { Material library where mesh materials will be stored/retrieved.
       If this property is not defined or if UseMeshMaterials is false,
       only the FreeForm's material will be used (and the mesh's materials
       will be ignored. }
    property MaterialLibrary: TGLMaterialLibrary
      read FMaterialLibrary write SetMaterialLibrary;
    { Defines wether materials declared in the vector file mesh are used.
       You must also define the MaterialLibrary property. }
    property UseMeshMaterials: boolean read FUseMeshMaterials
      write SetUseMeshMaterials default True;
    { LightMap library where lightmaps will be stored/retrieved.
       If this property is not defined, lightmaps won't be used.
       Lightmaps currently *always* use the second texture unit (unit 1),
       and may interfere with multi-texture materials. }
    property LightmapLibrary: TGLMaterialLibrary
      read FLightmapLibrary write SetLightmapLibrary;
    { If True, exceptions about missing textures will be ignored.
       Implementation is up to the file loader class (ie. this property
       may be ignored by some loaders) }
    property IgnoreMissingTextures: boolean
      read FIgnoreMissingTextures write FIgnoreMissingTextures default False;

    { Normals orientation for owned mesh. }
    property NormalsOrientation: TMeshNormalsOrientation
      read FNormalsOrientation write SetNormalsOrientation default mnoDefault;

    { Request rendering of skeleton bones over the mesh. }
    property OverlaySkeleton: boolean read FOverlaySkeleton
      write SetOverlaySkeleton default False;

  end;

  // TGLFreeForm

  { Container objects for a vector file mesh.
     FreeForms allows loading and rendering vector files (like 3DStudio
     ".3DS" file) in GLScene. Meshes can be loaded with the LoadFromFile
     method.
     A FreeForm may contain more than one mesh, but they will all be handled
     as a single object in a scene. }
  TGLFreeForm = class(TGLBaseMesh)
  private

    FOctree: TOctree;

  protected

    function GetOctree: TOctree;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function OctreeRayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil; intersectNormal: PVector = nil): boolean;
    function OctreeSphereSweepIntersect(const rayStart, rayVector: TVector;
      const velocity, radius: single; intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): boolean;
    function OctreeTriangleIntersect(const v1, v2, v3: TAffineVector): boolean;
    { Returns true if Point is inside the free form - this will only work
    properly on closed meshes. Requires that Octree has been prepared.}
    function OctreePointInMesh(const Point: TVector): boolean;
    function OctreeAABBIntersect(const AABB: TAABB;
      objMatrix, invObjMatrix: TMatrix; triangles: TAffineVectorList = nil): boolean;
    //         TODO:  function OctreeSphereIntersect

             { Octree support *experimental*.
                Use only if you understand what you're doing! }
    property Octree: TOctree read GetOctree;
    procedure BuildOctree(TreeDepth: integer = 3);

  published

    property AutoCentering;
    property AutoScaling;
    property MaterialLibrary;
    property LightmapLibrary;
    property UseMeshMaterials;
    property NormalsOrientation;
  end;

  // TGLActorOption

  { Miscellanious actor options.

      aoSkeletonNormalizeNormals : if set the normals of a skeleton-animated
         mesh will be normalized, this is not required if no normals-based texture
         coordinates generation occurs, and thus may be unset to improve performance.
       }
  TGLActorOption = (aoSkeletonNormalizeNormals);
  TGLActorOptions = set of TGLActorOption;

const
  cDefaultGLActorOptions = [aoSkeletonNormalizeNormals];

type

  TGLActor = class;

  // TActorAnimationReference

  TActorAnimationReference = (aarMorph, aarSkeleton, aarNone);

  // TActorAnimation

    { An actor animation sequence.
       An animation sequence is a named set of contiguous frames that can be used
       for animating an actor. The referred frames can be either morph or skeletal
       frames (choose which via the Reference property).
       An animation can be directly "played" by the actor by selecting it with
       SwitchAnimation, and can also be "blended" via a TGLAnimationControler. }
  TActorAnimation = class(TCollectionItem)
  private

    FName: string;
    FStartFrame: integer;
    FEndFrame: integer;
    FReference: TActorAnimationReference;

  protected

    function GetDisplayName: string; override;
    function FrameCount: integer;
    procedure SetStartFrame(const val: integer);
    procedure SetEndFrame(const val: integer);
    procedure SetReference(val: TActorAnimationReference);
    procedure SetAsString(const val: string);
    function GetAsString: string;

  public

    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property AsString: string read GetAsString write SetAsString;

    function OwnerActor: TGLActor;

    { Linearly removes the translation component between skeletal frames.
       This function will compute the translation of the first bone (index 0)
       and linearly subtract this translation in all frames between startFrame
       and endFrame. Its purpose is essentially to remove the 'slide' that
       exists in some animation formats (f.i. SMD). }
    procedure MakeSkeletalTranslationStatic;
    { Removes the absolute rotation component of the skeletal frames.
       Some formats will store frames with absolute rotation information,
       if this correct if the animation is the "main" animation.
       This function removes that absolute information, making the animation
       frames suitable for blending purposes. }
    procedure MakeSkeletalRotationDelta;

  published

    property Name: string read FName write FName;
    { Index of the initial frame of the animation. }
    property StartFrame: integer read FStartFrame write SetStartFrame;
    { Index of the final frame of the animation. }
    property EndFrame: integer read FEndFrame write SetEndFrame;
    { Indicates if this is a skeletal or a morph-based animation. }
    property Reference: TActorAnimationReference
      read FReference write SetReference default aarMorph;
  end;

  TActorAnimationName = string;

  // TActorAnimations

  { Collection of actor animations sequences. }
  TActorAnimations = class(TCollection)
  private

    FOwner: TGLActor;

  protected

    function GetOwner: TPersistent; override;
    procedure SetItems(index: integer; const val: TActorAnimation);
    function GetItems(index: integer): TActorAnimation;

  public

    constructor Create(AOwner: TGLActor);
    function Add: TActorAnimation;
    function FindItemID(ID: integer): TActorAnimation;
    function FindName(const aName: string): TActorAnimation;
    function FindFrame(aFrame: integer;
      aReference: TActorAnimationReference): TActorAnimation;

    procedure SetToStrings(aStrings: TStrings);
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToFile(const fileName: string);
    procedure LoadFromFile(const fileName: string);

    property Items[index: integer]: TActorAnimation read GetItems write SetItems;
      default;
    function Last: TActorAnimation;
  end;

  // TGLBaseAnimationControler

  { Base class for skeletal animation control. }
  TGLBaseAnimationControler = class(TComponent)
  private

    FEnabled: boolean;
    FActor: TGLActor;

  protected

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetEnabled(const val: boolean);
    procedure SetActor(const val: TGLActor);

    procedure DoChange; virtual;
    function Apply(var lerpInfo: TBlendedLerpInfo): boolean; virtual;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published

    property Enabled: boolean read FEnabled write SetEnabled default True;
    property Actor: TGLActor read FActor write SetActor;
  end;

  // TGLAnimationControler

    { Controls the blending of an additionnal skeletal animation into an actor.
       The animation controler allows animating an actor with several animations
       at a time, for instance, you could use a "run" animation as base animation
       (in TGLActor), blend an animation that makes the arms move differently
       depending on what the actor is carrying, along with an animation that will
       make the head turn toward a target. }
  TGLAnimationControler = class(TGLBaseAnimationControler)
  private

    FAnimationName: TActorAnimationName;
    FRatio: single;

  protected

    procedure SetAnimationName(const val: TActorAnimationName);
    procedure SetRatio(const val: single);

    procedure DoChange; override;
    function Apply(var lerpInfo: TBlendedLerpInfo): boolean; override;

  published

    property AnimationName: string read FAnimationName write SetAnimationName;
    property Ratio: single read FRatio write SetRatio;
  end;

  // TActorFrameInterpolation

  { Actor frame-interpolation mode.
     - afpNone : no interpolation, display CurrentFrame only
     - afpLinear : perform linear interpolation between current and next frame }
  TActorFrameInterpolation = (afpNone, afpLinear);

  // TActorActionMode

  { Defines how an actor plays between its StartFrame and EndFrame.

      aamNone : no animation is performed
      aamPlayOnce : play from current frame to EndFrame, once end frame has
        been reached, switches to aamNone
      aamLoop : play from current frame to EndFrame, once end frame has
        been reached, sets CurrentFrame to StartFrame
      aamBounceForward : play from current frame to EndFrame, once end frame
        has been reached, switches to aamBounceBackward
      aamBounceBackward : play from current frame to StartFrame, once start
        frame has been reached, switches to aamBounceForward
      aamExternal : Allows for external animation control
       }
  TActorAnimationMode = (aamNone, aamPlayOnce, aamLoop, aamBounceForward,
    aamBounceBackward, aamLoopBackward, aamExternal);

  // TGLActor

  { Mesh class specialized in animated meshes.
     The TGLActor provides a quick interface to animated meshes based on morph
     or skeleton frames, it is capable of performing frame interpolation and
     animation blending (via TGLAnimationControler components). }
  TGLActor = class(TGLBaseMesh)
  private

    FStartFrame, FEndFrame: integer;
    FReference: TActorAnimationReference;
    FCurrentFrame: integer;
    FCurrentFrameDelta: single;
    FFrameInterpolation: TActorFrameInterpolation;
    FInterval: integer;
    FAnimationMode: TActorAnimationMode;
    FOnFrameChanged: TNotifyEvent;
    FOnEndFrameReached, FOnStartFrameReached: TNotifyEvent;
    FAnimations: TActorAnimations;
    FTargetSmoothAnimation: TActorAnimation;
    FControlers: TList;
    FOptions: TGLActorOptions;

  protected

    procedure SetCurrentFrame(val: integer);
    procedure SetStartFrame(val: integer);
    procedure SetEndFrame(val: integer);
    procedure SetReference(val: TActorAnimationReference);
    procedure SetAnimations(const val: TActorAnimations);
    function StoreAnimations: boolean;
    procedure SetOptions(const val: TGLActorOptions);

    procedure PrepareMesh; override;
    procedure PrepareBuildList(var mrci: TGLRenderContextInfo); override;
    procedure DoAnimate; virtual;

    procedure RegisterControler(aControler: TGLBaseAnimationControler);
    procedure UnRegisterControler(aControler: TGLBaseAnimationControler);

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TGLRenderContextInfo); override;

    procedure DoProgress(const progressTime: TProgressTimes); override;

    procedure LoadFromStream(const filename: string; aStream: TStream);
      override;

    procedure SwitchToAnimation(anAnimation: TActorAnimation;
      smooth: boolean = False); overload;
    procedure SwitchToAnimation(const animationName: string;
      smooth: boolean = False); overload;
    procedure SwitchToAnimation(animationIndex: integer;
      smooth: boolean = False); overload;
    function CurrentAnimation: string;

    { Synchronize self animation with an other actor.
       Copies Start/Current/End Frame values, CurrentFrameDelta,
       AnimationMode and FrameInterpolation. }
    procedure Synchronize(referenceActor: TGLActor);

    { Provides a direct access to FCurrentFrame without any checks.
       Used in TGLActorProxy. }
    procedure SetCurrentFrameDirect(const Value: integer);

    function NextFrameIndex: integer;

    procedure NextFrame(nbSteps: integer = 1);
    procedure PrevFrame(nbSteps: integer = 1);

    function FrameCount: integer;

    { Indicates whether the actor is currently swithing animations (with
       smooth interpolation).}
    function isSwitchingAnimation: boolean;

  published

    property StartFrame: integer read FStartFrame write SetStartFrame default 0;
    property EndFrame: integer read FEndFrame write SetEndFrame default 0;

    { Reference Frame Animation mode.
       Allows specifying if the model is primarily morph or skeleton based. }
    property Reference: TActorAnimationReference
      read FReference write FReference default aarMorph;

    { Current animation frame. }
    property CurrentFrame: integer read FCurrentFrame write SetCurrentFrame default 0;
    { Value in the [0; 1] range expressing the delta to the next frame. }
    property CurrentFrameDelta: single read FCurrentFrameDelta write FCurrentFrameDelta;
    { Frame interpolation mode (afpNone/afpLinear). }
    property FrameInterpolation: TActorFrameInterpolation
      read FFrameInterpolation write FFrameInterpolation default afpLinear;

    { See TActorAnimationMode. }
    property AnimationMode: TActorAnimationMode
      read FAnimationMode write FAnimationMode default aamNone;
    { Interval between frames, in milliseconds. }
    property Interval: integer read FInterval write FInterval;
    { Actor and animation miscellanious options. }
    property Options: TGLActorOptions read FOptions write SetOptions default
      cDefaultGLActorOptions;

    { Triggered after each CurrentFrame change. }
    property OnFrameChanged: TNotifyEvent read FOnFrameChanged write FOnFrameChanged;
    { Triggered after EndFrame has been reached by progression or "nextframe" }
    property OnEndFrameReached: TNotifyEvent
      read FOnEndFrameReached write FOnEndFrameReached;
    { Triggered after StartFrame has been reached by progression or "nextframe" }
    property OnStartFrameReached: TNotifyEvent
      read FOnStartFrameReached write FOnStartFrameReached;

    { Collection of animations sequences. }
    property Animations: TActorAnimations
      read FAnimations write SetAnimations stored StoreAnimations;

    property AutoCentering;
    property MaterialLibrary;
    property LightmapLibrary;
    property UseMeshMaterials;
    property NormalsOrientation;
    property OverlaySkeleton;
  end;

  // TGLVectorFileFormat

  TGLVectorFileFormat = class
  public
    VectorFileClass: TGLVectorFileClass;
    Extension: string;
    Description: string;
    DescResID: integer;
  end;

  // TGLVectorFileFormatsList

  { Stores registered vector file formats. }
  TGLVectorFileFormatsList = class(TPersistentObjectList)
  public

    destructor Destroy; override;

    procedure Add(const Ext, Desc: string; DescID: integer;
      AClass: TGLVectorFileClass);
    function FindExt(ext: string): TGLVectorFileClass;
    function FindFromFileName(const fileName: string): TGLVectorFileClass;
    procedure Remove(AClass: TGLVectorFileClass);
    procedure BuildFilterStrings(vectorFileClass: TGLVectorFileClass;
      out descriptions, filters: string; formatsThatCanBeOpened: boolean = True;
      formatsThatCanBeSaved: boolean = False);
    function FindExtByIndex(index: integer; formatsThatCanBeOpened: boolean = True;
      formatsThatCanBeSaved: boolean = False): string;
  end;

  EInvalidVectorFile = class(Exception);

// Read access to the list of registered vector file formats
function GetVectorFileFormats: TGLVectorFileFormatsList;
// A file extension filter suitable for dialog's 'Filter' property
function VectorFileFormatsFilter: string;
// A file extension filter suitable for a savedialog's 'Filter' property
function VectorFileFormatsSaveFilter: string;
{ Returns an extension by its index in the vector files dialogs filter.
   Use VectorFileFormatsFilter to obtain the filter. }
function VectorFileFormatExtensionByIndex(index: integer): string;

procedure RegisterVectorFileFormat(const aExtension, aDescription: string;
  aClass: TGLVectorFileClass);
procedure UnregisterVectorFileClass(aClass: TGLVectorFileClass);

var
  vGLVectorFileObjectsAllocateMaterials: boolean = True;
  // Mrqzzz : Flag to avoid loading materials (useful for IDE Extentions or scene editors)
  vGLVectorFileObjectsEnableVBOByDefault: boolean = True;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  GLStrings, XOpenGL, GLCrossPlatform, GLMeshUtils, GLState, GLUtils,
  GLBaseMeshSilhouette, GLVectorTypes;

var
  vVectorFileFormats: TGLVectorFileFormatsList;
  vNextRenderGroupID: integer = 1;

const
  cAAFHeader: ansistring = 'AAF';

// GetVectorFileFormats


function GetVectorFileFormats: TGLVectorFileFormatsList;
begin
  if not Assigned(vVectorFileFormats) then
    vVectorFileFormats := TGLVectorFileFormatsList.Create;
  Result := vVectorFileFormats;
end;

// VectorFileFormatsFilter


function VectorFileFormatsFilter: string;
var
  f: string;
begin
  GetVectorFileFormats.BuildFilterStrings(TGLVectorFile, Result, f);
end;

// VectorFileFormatsSaveFilter


function VectorFileFormatsSaveFilter: string;
var
  f: string;
begin
  GetVectorFileFormats.BuildFilterStrings(TGLVectorFile, Result, f, False, True);
end;

// RegisterVectorFileFormat


procedure RegisterVectorFileFormat(const AExtension, ADescription: string;
  AClass: TGLVectorFileClass);
begin
  RegisterClass(AClass);
  GetVectorFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

// UnregisterVectorFileClass


procedure UnregisterVectorFileClass(AClass: TGLVectorFileClass);
begin
  if Assigned(vVectorFileFormats) then
    vVectorFileFormats.Remove(AClass);
end;

// VectorFileFormatExtensionByIndex


function VectorFileFormatExtensionByIndex(index: integer): string;
begin
  Result := GetVectorFileFormats.FindExtByIndex(index);
end;

// TGLVectorFileFormatsList.Destroy


destructor TGLVectorFileFormatsList.Destroy;
begin
  Clean;
  inherited;
end;

// Add


procedure TGLVectorFileFormatsList.Add(const Ext, Desc: string;
  DescID: integer; AClass: TGLVectorFileClass);
var
  newRec: TGLVectorFileFormat;
begin
  newRec := TGLVectorFileFormat.Create;
  with newRec do
  begin
    Extension := AnsiLowerCase(Ext);
    VectorFileClass := AClass;
    Description := Desc;
    DescResID := DescID;
  end;
  inherited Add(newRec);
end;

// FindExt


function TGLVectorFileFormatsList.FindExt(ext: string): TGLVectorFileClass;
var
  i: integer;
begin
  ext := AnsiLowerCase(ext);
  for i := Count - 1 downto 0 do
    with TGLVectorFileFormat(Items[I]) do
    begin
      if Extension = ext then
      begin
        Result := VectorFileClass;
        Exit;
      end;
    end;
  Result := nil;
end;

// FindFromFileName


function TGLVectorFileFormatsList.FindFromFileName(
  const fileName: string): TGLVectorFileClass;
var
  ext: string;
begin
  ext := ExtractFileExt(Filename);
  System.Delete(ext, 1, 1);
  Result := FindExt(ext);
  if not Assigned(Result) then
    raise EInvalidVectorFile.CreateFmt(glsUnknownExtension,
      [ext, 'GLFile' + UpperCase(ext)]);
end;

// Remove


procedure TGLVectorFileFormatsList.Remove(AClass: TGLVectorFileClass);
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if TGLVectorFileFormat(Items[i]).VectorFileClass.InheritsFrom(AClass) then
      DeleteAndFree(i);
  end;
end;

// BuildFilterStrings


procedure TGLVectorFileFormatsList.BuildFilterStrings(
  vectorFileClass: TGLVectorFileClass; out descriptions, filters: string;
  formatsThatCanBeOpened: boolean = True; formatsThatCanBeSaved: boolean = False);
var
  k, i: integer;
  p: TGLVectorFileFormat;
begin
  descriptions := '';
  filters := '';
  k := 0;
  for i := 0 to Count - 1 do
  begin
    p := TGLVectorFileFormat(Items[i]);
    if p.VectorFileClass.InheritsFrom(vectorFileClass) and
      (p.Extension <> '') and ((formatsThatCanBeOpened and
      (dfcRead in p.VectorFileClass.Capabilities)) or
      (formatsThatCanBeSaved and (dfcWrite in p.VectorFileClass.Capabilities))) then
    begin
      with p do
      begin
        if k <> 0 then
        begin
          descriptions := descriptions + '|';
          filters := filters + ';';
        end;
        if (Description = '') and (DescResID <> 0) then
          Description := LoadStr(DescResID);
        FmtStr(descriptions, '%s%s (*.%s)|*.%2:s',
          [descriptions, Description, Extension]);
        filters := filters + '*.' + Extension;
        Inc(k);
      end;
    end;
  end;
  if (k > 1) and (not formatsThatCanBeSaved) then
    FmtStr(descriptions, '%s (%s)|%1:s|%s',
      [glsAllFilter, filters, descriptions]);
end;

// FindExtByIndex


function TGLVectorFileFormatsList.FindExtByIndex(index: integer;
  formatsThatCanBeOpened: boolean = True;
  formatsThatCanBeSaved: boolean = False): string;
var
  i: integer;
  p: TGLVectorFileFormat;
begin
  Result := '';
  if index > 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      p := TGLVectorFileFormat(Items[i]);
      if (formatsThatCanBeOpened and
        (dfcRead in p.VectorFileClass.Capabilities)) or
        (formatsThatCanBeSaved and (dfcWrite in p.VectorFileClass.Capabilities)) then
      begin
        if index = 1 then
        begin
          Result := p.Extension;
          Break;
        end
        else
          Dec(index);
      end;
    end;
  end;
end;

// ------------------
// ------------------ TBaseMeshObject ------------------
// ------------------

// Create


constructor TBaseMeshObject.Create;
begin
  FVertices := TAffineVectorList.Create;
  FNormals := TAffineVectorList.Create;
  FVisible := True;
  inherited Create;
end;

// Destroy


destructor TBaseMeshObject.Destroy;
begin
  FNormals.Free;
  FVertices.Free;
  inherited;
end;




procedure TBaseMeshObject.Assign(Source: TPersistent);
begin
  if Source is TBaseMeshObject then
  begin
    FName := TBaseMeshObject(Source).Name;
    FVertices.Assign(TBaseMeshObject(Source).FVertices);
    FNormals.Assign(TBaseMeshObject(Source).FNormals);
  end
  else
    inherited; // Die!
end;

// WriteToFiler


procedure TBaseMeshObject.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(1); // Archive Version 1, added FVisible
    WriteString(FName);
    FVertices.WriteToFiler(writer);
    FNormals.WriteToFiler(writer);
    WriteBoolean(FVisible);
  end;
end;

// ReadFromFiler


procedure TBaseMeshObject.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion in [0..1] then
    with reader do
    begin
      FName := ReadString;
      FVertices.ReadFromFiler(reader);
      FNormals.ReadFromFiler(reader);
      if archiveVersion >= 1 then
        FVisible := ReadBoolean
      else
        FVisible := True;
    end
  else
    RaiseFilerException(archiveVersion);
end;

// Clear


procedure TBaseMeshObject.Clear;
begin
  FNormals.Clear;
  FVertices.Clear;
end;

// ContributeToBarycenter


procedure TBaseMeshObject.ContributeToBarycenter(var currentSum: TAffineVector;
  var nb: integer);
begin
  AddVector(currentSum, FVertices.Sum);
  nb := nb + FVertices.Count;
end;

// Translate


procedure TBaseMeshObject.Translate(const delta: TAffineVector);
begin
  FVertices.Translate(delta);
end;

// BuildNormals


procedure TBaseMeshObject.BuildNormals(vertexIndices: TIntegerList;
  mode: TGLMeshObjectMode; normalIndices: TIntegerList = nil);
var
  i, base: integer;
  n: TAffineVector;
  newNormals: TIntegerList;

  function TranslateNewNormal(vertexIndex: integer;
  const delta: TAffineVector): integer;
  var
    pv: PAffineVector;
  begin
    Result := newNormals[vertexIndex];
    if Result < base then
    begin
      Result := Normals.Add(NullVector);
      newNormals[vertexIndex] := Result;
    end;
    pv := @Normals.List[Result];
    AddVector(pv^, delta);
  end;

begin
  if not Assigned(normalIndices) then
  begin
    // build bijection
    Normals.Clear;
    Normals.Count := Vertices.Count;
    case mode of
      momTriangles:
      begin
        i := 0;
        while i <= vertexIndices.Count - 3 do
          with Normals do
          begin
            with Vertices do
            begin
              CalcPlaneNormal(Items[vertexIndices[i + 0]],
                Items[vertexIndices[i + 1]],
                Items[vertexIndices[i + 2]], n);
            end;
            with Normals do
            begin
              TranslateItem(vertexIndices[i + 0], n);
              TranslateItem(vertexIndices[i + 1], n);
              TranslateItem(vertexIndices[i + 2], n);
            end;
            Inc(i, 3);
          end;
      end;
      momTriangleStrip:
      begin
        i := 0;
        while i <= vertexIndices.Count - 3 do
          with Normals do
          begin
            with Vertices do
            begin
              if (i and 1) = 0 then
                CalcPlaneNormal(Items[vertexIndices[i + 0]],
                  Items[vertexIndices[i + 1]],
                  Items[vertexIndices[i + 2]], n)
              else
                CalcPlaneNormal(Items[vertexIndices[i + 0]],
                  Items[vertexIndices[i + 2]],
                  Items[vertexIndices[i + 1]], n);
            end;
            with Normals do
            begin
              TranslateItem(vertexIndices[i + 0], n);
              TranslateItem(vertexIndices[i + 1], n);
              TranslateItem(vertexIndices[i + 2], n);
            end;
            Inc(i, 1);
          end;
      end;
      else
        Assert(False);
    end;
    Normals.Normalize;
  end
  else
  begin
    // add new normals
    base := Normals.Count;
    newNormals := TIntegerList.Create;
    newNormals.AddSerie(-1, 0, Vertices.Count);
    case mode of
      momTriangles:
      begin
        i := 0;
        while i <= vertexIndices.Count - 3 do
        begin
          with Vertices do
          begin
            CalcPlaneNormal(Items[vertexIndices[i + 0]],
              Items[vertexIndices[i + 1]],
              Items[vertexIndices[i + 2]], n);
          end;
          normalIndices.Add(TranslateNewNormal(vertexIndices[i + 0], n));
          normalIndices.Add(TranslateNewNormal(vertexIndices[i + 1], n));
          normalIndices.Add(TranslateNewNormal(vertexIndices[i + 2], n));
          Inc(i, 3);
        end;
      end;
      momTriangleStrip:
      begin
        i := 0;
        while i <= vertexIndices.Count - 3 do
        begin
          with Vertices do
          begin
            if (i and 1) = 0 then
              CalcPlaneNormal(Items[vertexIndices[i + 0]],
                Items[vertexIndices[i + 1]],
                Items[vertexIndices[i + 2]], n)
            else
              CalcPlaneNormal(Items[vertexIndices[i + 0]],
                Items[vertexIndices[i + 2]],
                Items[vertexIndices[i + 1]], n);
          end;
          normalIndices.Add(TranslateNewNormal(vertexIndices[i + 0], n));
          normalIndices.Add(TranslateNewNormal(vertexIndices[i + 1], n));
          normalIndices.Add(TranslateNewNormal(vertexIndices[i + 2], n));
          Inc(i, 1);
        end;
      end;
      else
        Assert(False);
    end;
    for i := base to Normals.Count - 1 do
      NormalizeVector(Normals.List^[i]);
    newNormals.Free;
  end;
end;

// ExtractTriangles


function TBaseMeshObject.ExtractTriangles(texCoords: TAffineVectorList = nil;
  normals: TAffineVectorList = nil): TAffineVectorList;
begin
  Result := TAffineVectorList.Create;
  if (Vertices.Count mod 3) = 0 then
  begin
    Result.Assign(Vertices);
    if Assigned(normals) then
      normals.Assign(Self.Normals);
  end;
end;

// SetVertices


procedure TBaseMeshObject.SetVertices(const val: TAffineVectorList);
begin
  FVertices.Assign(val);
end;

// SetNormals


procedure TBaseMeshObject.SetNormals(const val: TAffineVectorList);
begin
  FNormals.Assign(val);
end;

// ------------------
// ------------------ TGLSkeletonFrame ------------------
// ------------------

// CreateOwned


constructor TGLSkeletonFrame.CreateOwned(aOwner: TGLSkeletonFrameList);
begin
  FOwner := aOwner;
  aOwner.Add(Self);
  Create;
end;

// Create


constructor TGLSkeletonFrame.Create;
begin
  inherited Create;
  FPosition := TAffineVectorList.Create;
  FRotation := TAffineVectorList.Create;
  FQuaternion := TQuaternionList.Create;
  FTransformMode := sftRotation;
end;

// Destroy


destructor TGLSkeletonFrame.Destroy;
begin
  FlushLocalMatrixList;
  FRotation.Free;
  FPosition.Free;
  FQuaternion.Free;
  inherited Destroy;
end;

// WriteToFiler


procedure TGLSkeletonFrame.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(1); // Archive Version 1
    WriteString(FName);
    FPosition.WriteToFiler(writer);
    FRotation.WriteToFiler(writer);
    FQuaternion.WriteToFiler(writer);
    WriteInteger(integer(FTransformMode));
  end;
end;

// ReadFromFiler


procedure TGLSkeletonFrame.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if (archiveVersion = 0) or (archiveVersion = 1) then
    with reader do
    begin
      FName := ReadString;
      FPosition.ReadFromFiler(reader);
      FRotation.ReadFromFiler(reader);
      if (archiveVersion = 1) then
      begin
        FQuaternion.ReadFromFiler(reader);
        FTransformMode := TGLSkeletonFrameTransform(ReadInteger);
      end;
    end
  else
    RaiseFilerException(archiveVersion);
  FlushLocalMatrixList;
end;

// SetPosition


procedure TGLSkeletonFrame.SetPosition(const val: TAffineVectorList);
begin
  FPosition.Assign(val);
end;

// SetRotation


procedure TGLSkeletonFrame.SetRotation(const val: TAffineVectorList);
begin
  FRotation.Assign(val);
end;

// SetQuaternion


procedure TGLSkeletonFrame.SetQuaternion(const val: TQuaternionList);
begin
  FQuaternion.Assign(val);
end;

// LocalMatrixList


function TGLSkeletonFrame.LocalMatrixList: PMatrixArray;
var
  i: integer;
  s, c: single;
  mat, rmat: TMatrix;
  quat: TQuaternion;
begin
  if not Assigned(FLocalMatrixList) then
  begin
    case FTransformMode of
      sftRotation:
      begin
        FLocalMatrixList := AllocMem(SizeOf(TMatrix) * Rotation.Count);
        for i := 0 to Rotation.Count - 1 do
        begin
          if Rotation[i].V[0] <> 0 then
          begin
            SinCos(Rotation[i].V[0], s, c);
            mat := CreateRotationMatrixX(s, c);
          end
          else
            mat := IdentityHmgMatrix;
          if Rotation[i].V[1] <> 0 then
          begin
            SinCos(Rotation[i].V[1], s, c);
            rmat := CreateRotationMatrixY(s, c);
            mat := MatrixMultiply(mat, rmat);
          end;
          if Rotation[i].V[2] <> 0 then
          begin
            SinCos(Rotation[i].V[2], s, c);
            rmat := CreateRotationMatrixZ(s, c);
            mat := MatrixMultiply(mat, rmat);
          end;
          mat.V[3].V[0] := Position[i].V[0];
          mat.V[3].V[1] := Position[i].V[1];
          mat.V[3].V[2] := Position[i].V[2];
          FLocalMatrixList^[i] := mat;
        end;
      end;
      sftQuaternion:
      begin
        FLocalMatrixList := AllocMem(SizeOf(TMatrix) * Quaternion.Count);
        for i := 0 to Quaternion.Count - 1 do
        begin
          quat := Quaternion[i];
          mat := QuaternionToMatrix(quat);
          mat.V[3].V[0] := Position[i].V[0];
          mat.V[3].V[1] := Position[i].V[1];
          mat.V[3].V[2] := Position[i].V[2];
          mat.V[3].V[3] := 1;
          FLocalMatrixList^[i] := mat;
        end;
      end;
    end;
  end;
  Result := FLocalMatrixList;
end;

// FlushLocalMatrixList


procedure TGLSkeletonFrame.FlushLocalMatrixList;
begin
  if Assigned(FLocalMatrixList) then
  begin
    FreeMem(FLocalMatrixList);
    FLocalMatrixList := nil;
  end;
end;

// ConvertQuaternionsToRotations


procedure TGLSkeletonFrame.ConvertQuaternionsToRotations(KeepQuaternions:
  boolean = True);
var
  i: integer;
  t: TTransformations;
  m: TMatrix;
begin
  Rotation.Clear;
  for i := 0 to Quaternion.Count - 1 do
  begin
    m := QuaternionToMatrix(Quaternion[i]);
    if MatrixDecompose(m, t) then
      Rotation.Add(t[ttRotateX], t[ttRotateY], t[ttRotateZ])
    else
      Rotation.Add(NullVector);
  end;
  if not KeepQuaternions then
    Quaternion.Clear;
end;

// ConvertRotationsToQuaternions


procedure TGLSkeletonFrame.ConvertRotationsToQuaternions(KeepRotations: boolean = True);
var
  i: integer;
  mat, rmat: TMatrix;
  s, c: single;
begin
  Quaternion.Clear;
  for i := 0 to Rotation.Count - 1 do
  begin
    mat := IdentityHmgMatrix;
    SinCos(Rotation[i].V[0], s, c);
    rmat := CreateRotationMatrixX(s, c);
    mat := MatrixMultiply(mat, rmat);
    SinCos(Rotation[i].V[1], s, c);
    rmat := CreateRotationMatrixY(s, c);
    mat := MatrixMultiply(mat, rmat);
    SinCos(Rotation[i].V[2], s, c);
    rmat := CreateRotationMatrixZ(s, c);
    mat := MatrixMultiply(mat, rmat);
    Quaternion.Add(QuaternionFromMatrix(mat));
  end;
  if not KeepRotations then
    Rotation.Clear;
end;

// ------------------
// ------------------ TGLSkeletonFrameList ------------------
// ------------------

// CreateOwned


constructor TGLSkeletonFrameList.CreateOwned(AOwner: TPersistent);
begin
  FOwner := AOwner;
  Create;
end;

// Destroy


destructor TGLSkeletonFrameList.Destroy;
begin
  Clear;
  inherited;
end;

// ReadFromFiler


procedure TGLSkeletonFrameList.ReadFromFiler(reader: TVirtualReader);
var
  i: integer;
begin
  inherited;
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

// Clear


procedure TGLSkeletonFrameList.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      FOwner := nil;
      Free;
    end;
  inherited;
end;

// GetSkeletonFrame


function TGLSkeletonFrameList.GetSkeletonFrame(Index: integer): TGLSkeletonFrame;
begin
  Result := TGLSkeletonFrame(List^[Index]);
end;

// ConvertQuaternionsToRotations


procedure TGLSkeletonFrameList.ConvertQuaternionsToRotations(
  KeepQuaternions: boolean = True; SetTransformMode: boolean = True);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].ConvertQuaternionsToRotations(KeepQuaternions);
    if SetTransformMode then
      Items[i].TransformMode := sftRotation;
  end;
end;

// ConvertRotationsToQuaternions


procedure TGLSkeletonFrameList.ConvertRotationsToQuaternions(
  KeepRotations: boolean = True; SetTransformMode: boolean = True);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].ConvertRotationsToQuaternions(KeepRotations);
    if SetTransformMode then
      Items[i].TransformMode := sftQuaternion;
  end;
end;

// ------------------
// ------------------ TGLSkeletonBoneList ------------------
// ------------------

// CreateOwned


constructor TGLSkeletonBoneList.CreateOwned(aOwner: TGLSkeleton);
begin
  FSkeleton := aOwner;
  Create;
end;

// Create


constructor TGLSkeletonBoneList.Create;
begin
  inherited;
  FGlobalMatrix := IdentityHmgMatrix;
end;

// Destroy


destructor TGLSkeletonBoneList.Destroy;
begin
  Clean;
  inherited;
end;

// WriteToFiler


procedure TGLSkeletonBoneList.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing, yet
  end;
end;

// ReadFromFiler


procedure TGLSkeletonBoneList.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion, i: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      // nothing, yet
    end
  else
    RaiseFilerException(archiveVersion);
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;


procedure TGLSkeletonBoneList.AfterObjectCreatedByReader(Sender: TObject);
begin
  with (Sender as TGLSkeletonBone) do
  begin
    FOwner := Self;
    FSkeleton := Self.Skeleton;
  end;
end;

function TGLSkeletonBoneList.GetSkeletonBone(Index: integer): TGLSkeletonBone;
begin
  Result := TGLSkeletonBone(List^[Index]);
end;

function TGLSkeletonBoneList.BoneByID(anID: integer): TGLSkeletonBone;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].BoneByID(anID);
    if Assigned(Result) then
      Break;
  end;
end;


function TGLSkeletonBoneList.BoneByName(const aName: string): TGLSkeletonBone;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].BoneByName(aName);
    if Assigned(Result) then
      Break;
  end;
end;


function TGLSkeletonBoneList.BoneCount: integer;
var
  i: integer;
begin
  Result := 1;
  for i := 0 to Count - 1 do
    Inc(Result, Items[i].BoneCount);
end;


procedure TGLSkeletonBoneList.PrepareGlobalMatrices;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].PrepareGlobalMatrices;
end;


{%region=====[ TGLSkeletonRootBoneList ]========================================}


procedure TGLSkeletonRootBoneList.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing, yet
  end;
end;

procedure TGLSkeletonRootBoneList.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion, i: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      // nothing, yet
    end
  else
    RaiseFilerException(archiveVersion);
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;


procedure TGLSkeletonRootBoneList.BuildList(var mrci: TGLRenderContextInfo);
var
  i: integer;
begin
  // root node setups and restore OpenGL stuff
  mrci.GLStates.Disable(stColorMaterial);
  mrci.GLStates.Disable(stLighting);
  GL.Color3f(1, 1, 1);
  // render root-bones
  for i := 0 to Count - 1 do
    Items[i].BuildList(mrci);
end;

{%endregion%}

// ------------------
{%region=====[ TGLSkeletonBone ]================================================}

constructor TGLSkeletonBone.CreateOwned(aOwner: TGLSkeletonBoneList);
begin
  FOwner := aOwner;
  aOwner.Add(Self);
  FSkeleton := aOwner.Skeleton;
  Create;
end;

constructor TGLSkeletonBone.Create;
begin
  FColor := $FFFFFFFF; // opaque white
  inherited;
end;

destructor TGLSkeletonBone.Destroy;
begin
  if Assigned(Owner) then
    Owner.Remove(Self);
  inherited Destroy;
end;

procedure TGLSkeletonBone.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteString(FName);
    WriteInteger(FBoneID);
    WriteInteger(integer(FColor));
  end;
end;

procedure TGLSkeletonBone.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion, i: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FName := ReadString;
      FBoneID := ReadInteger;
      FColor := cardinal(ReadInteger);
    end
  else
    RaiseFilerException(archiveVersion);
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

procedure TGLSkeletonBone.BuildList(var mrci: TGLRenderContextInfo);

  procedure IssueColor(color: cardinal);
  begin
    GL.Color4f(GetRValue(color) / 255, GetGValue(color) / 255,
      GetBValue(color) / 255,
      ((color shr 24) and 255) / 255);
  end;

var
  i: integer;
begin
  // point for self
  mrci.GLStates.PointSize := 5;
  GL.Begin_(GL_POINTS);
  IssueColor(Color);
  GL.Vertex3fv(@GlobalMatrix.V[3].V[0]);
  GL.End_;
  // parent-self bone line
  if Owner is TGLSkeletonBone then
  begin
    GL.Begin_(GL_LINES);
    GL.Vertex3fv(@TGLSkeletonBone(Owner).GlobalMatrix.V[3].V[0]);
    GL.Vertex3fv(@GlobalMatrix.V[3].V[0]);
    GL.End_;
  end;
  // render sub-bones
  for i := 0 to Count - 1 do
    Items[i].BuildList(mrci);
end;

function TGLSkeletonBone.GetSkeletonBone(Index: integer): TGLSkeletonBone;
begin
  Result := TGLSkeletonBone(List^[Index]);
end;

procedure TGLSkeletonBone.SetColor(const val: cardinal);
begin
  FColor := val;
end;

function TGLSkeletonBone.BoneByID(anID: integer): TGLSkeletonBone;
begin
  if BoneID = anID then
    Result := Self
  else
    Result := inherited BoneByID(anID);
end;

function TGLSkeletonBone.BoneByName(const aName: string): TGLSkeletonBone;
begin
  if Name = aName then
    Result := Self
  else
    Result := inherited BoneByName(aName);
end;

procedure TGLSkeletonBone.Clean;
begin
  BoneID := 0;
  Name := '';
  inherited;
end;

procedure TGLSkeletonBone.PrepareGlobalMatrices;
begin
  if (Skeleton.FRagDollEnabled) then
    Exit; // ragdoll
  FGlobalMatrix :=
    MatrixMultiply(Skeleton.CurrentFrame.LocalMatrixList^[BoneID],
    TGLSkeletonBoneList(Owner).FGlobalMatrix);
  inherited;
end;

procedure TGLSkeletonBone.SetGlobalMatrix(Matrix: TMatrix); // ragdoll
begin
  FGlobalMatrix := Matrix;
end;

procedure TGLSkeletonBone.SetGlobalMatrixForRagDoll(RagDollMatrix: TMatrix);
// ragdoll
begin
  FGlobalMatrix := MatrixMultiply(RagDollMatrix, Skeleton.Owner.InvAbsoluteMatrix);
  inherited;
end;

{%endregion%}

{%region=====[  TGLSkeletonCollider ]===========================================}

constructor TGLSkeletonCollider.Create;
begin
  inherited;
  FLocalMatrix := IdentityHMGMatrix;
  FGlobalMatrix := IdentityHMGMatrix;
  FAutoUpdate := True;
end;

constructor TGLSkeletonCollider.CreateOwned(AOwner: TGLSkeletonColliderList);
begin
  Create;
  FOwner := AOwner;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

procedure TGLSkeletonCollider.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    if Assigned(FBone) then
      WriteInteger(FBone.BoneID)
    else
      WriteInteger(-1);
    Write(FLocalMatrix, SizeOf(TMatrix));
  end;
end;

procedure TGLSkeletonCollider.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FBoneID := ReadInteger;
      Read(FLocalMatrix, SizeOf(TMatrix));
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TGLSkeletonCollider.AlignCollider;
var
  mat: TMatrix;
begin
  if Assigned(FBone) then
  begin
    if Owner.Owner is TGLSkeleton then
      if TGLSkeleton(Owner.Owner).Owner is TGLBaseSceneObject then
        mat := MatrixMultiply(FBone.GlobalMatrix,
          TGLBaseSceneObject(TGLSkeleton(Owner.Owner).Owner).AbsoluteMatrix)
      else
        mat := FBone.GlobalMatrix;
    MatrixMultiply(FLocalMatrix, mat, FGlobalMatrix);
  end
  else
    FGlobalMatrix := FLocalMatrix;
end;

procedure TGLSkeletonCollider.SetBone(const val: TGLSkeletonBone);
begin
  if val <> FBone then
    FBone := val;
end;

procedure TGLSkeletonCollider.SetLocalMatrix(const val: TMatrix);
begin
  FLocalMatrix := val;
end;

{%endregion%}


{%region=====[ TGLSkeletonColliderList ]========================================}

constructor TGLSkeletonColliderList.CreateOwned(AOwner: TPersistent);
begin
  Create;
  FOwner := AOwner;
end;

destructor TGLSkeletonColliderList.Destroy;
begin
  Clear;
  inherited;
end;


function TGLSkeletonColliderList.GetSkeletonCollider(index: integer):TGLSkeletonCollider;
begin
  Result := TGLSkeletonCollider(inherited Get(index));
end;

procedure TGLSkeletonColliderList.ReadFromFiler(reader: TVirtualReader);
var
  i: integer;
begin
  inherited;
  for i := 0 to Count - 1 do
  begin
    Items[i].FOwner := Self;
    if (Owner is TGLSkeleton) and (Items[i].FBoneID <> -1) then
      Items[i].Bone := TGLSkeleton(Owner).BoneByID(Items[i].FBoneID);
  end;
end;

procedure TGLSkeletonColliderList.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].FOwner := nil;
    Items[i].Free;
  end;
  inherited;
end;

procedure TGLSkeletonColliderList.AlignColliders;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].AutoUpdate then
      Items[i].AlignCollider;
end;

{%endregion%}

{%region=====[ TGLSkeleton ]====================================================}

constructor TGLSkeleton.CreateOwned(AOwner: TGLBaseMesh);
begin
  FOwner := aOwner;
  Create;
end;

constructor TGLSkeleton.Create;
begin
  inherited Create;
  FRootBones := TGLSkeletonRootBoneList.CreateOwned(Self);
  FFrames := TGLSkeletonFrameList.CreateOwned(Self);
  FColliders := TGLSkeletonColliderList.CreateOwned(Self);
end;

destructor TGLSkeleton.Destroy;
begin
  FlushBoneByIDCache;
  FCurrentFrame.Free;
  FFrames.Free;
  FRootBones.Free;
  FColliders.Free;
  inherited Destroy;
end;

procedure TGLSkeleton.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    if FColliders.Count > 0 then
      WriteInteger(1) // Archive Version 1 : with colliders
    else
      WriteInteger(0); // Archive Version 0
    FRootBones.WriteToFiler(writer);
    FFrames.WriteToFiler(writer);
    if FColliders.Count > 0 then
      FColliders.WriteToFiler(writer);
  end;
end;

procedure TGLSkeleton.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if (archiveVersion = 0) or (archiveVersion = 1) then
    with reader do
    begin
      FRootBones.ReadFromFiler(reader);
      FFrames.ReadFromFiler(reader);
      if (archiveVersion = 1) then
        FColliders.ReadFromFiler(reader);
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TGLSkeleton.SetRootBones(const val: TGLSkeletonRootBoneList);
begin
  FRootBones.Assign(val);
end;

procedure TGLSkeleton.SetFrames(const val: TGLSkeletonFrameList);
begin
  FFrames.Assign(val);
end;

function TGLSkeleton.GetCurrentFrame: TGLSkeletonFrame;
begin
  if not Assigned(FCurrentFrame) then
    FCurrentFrame := TGLSkeletonFrame(FFrames.Items[0].CreateClone);
  Result := FCurrentFrame;
end;

procedure TGLSkeleton.SetCurrentFrame(val: TGLSkeletonFrame);
begin
  if Assigned(FCurrentFrame) then
    FCurrentFrame.Free;
  FCurrentFrame := TGLSkeletonFrame(val.CreateClone);
end;

procedure TGLSkeleton.SetColliders(const val: TGLSkeletonColliderList);
begin
  FColliders.Assign(val);
end;

procedure TGLSkeleton.FlushBoneByIDCache;
begin
  FBonesByIDCache.Free;
  FBonesByIDCache := nil;
end;

function TGLSkeleton.BoneByID(anID: integer): TGLSkeletonBone;

  procedure CollectBones(bone: TGLSkeletonBone);
  var
    i: integer;
  begin
    if bone.BoneID >= FBonesByIDCache.Count then
      FBonesByIDCache.Count := bone.BoneID + 1;
    FBonesByIDCache[bone.BoneID] := bone;
    for i := 0 to bone.Count - 1 do
      CollectBones(bone[i]);
  end;

var
  i: integer;
begin
  if not Assigned(FBonesByIDCache) then
  begin
    FBonesByIDCache := TList.Create;
    for i := 0 to RootBones.Count - 1 do
      CollectBones(RootBones[i]);
  end;
  Result := TGLSkeletonBone(FBonesByIDCache[anID]);
end;

function TGLSkeleton.BoneByName(const aName: string): TGLSkeletonBone;
begin
  Result := RootBones.BoneByName(aName);
end;


function TGLSkeleton.BoneCount: integer;
begin
  Result := RootBones.BoneCount;
end;

procedure TGLSkeleton.MorphTo(frameIndex: integer);
begin
  CurrentFrame := Frames[frameIndex];
end;

procedure TGLSkeleton.MorphTo(frame: TGLSkeletonFrame);
begin
  CurrentFrame := frame;
end;

procedure TGLSkeleton.Lerp(frameIndex1, frameIndex2: integer; lerpFactor: single);
begin
  if Assigned(FCurrentFrame) then
    FCurrentFrame.Free;
  FCurrentFrame := TGLSkeletonFrame.Create;
  FCurrentFrame.TransformMode := Frames[frameIndex1].TransformMode;
  with FCurrentFrame do
  begin
    Position.Lerp(Frames[frameIndex1].Position,
      Frames[frameIndex2].Position, lerpFactor);
    case TransformMode of
      sftRotation: Rotation.AngleLerp(Frames[frameIndex1].Rotation,
          Frames[frameIndex2].Rotation, lerpFactor);
      sftQuaternion: Quaternion.Lerp(Frames[frameIndex1].Quaternion,
          Frames[frameIndex2].Quaternion, lerpFactor);
    end;
  end;
end;

procedure TGLSkeleton.BlendedLerps(const lerpInfos: array of TBlendedLerpInfo);
var
  i, n: integer;
  blendPositions: TAffineVectorList;
  blendRotations: TAffineVectorList;
  blendQuaternions: TQuaternionList;
begin
  n := High(lerpInfos) - Low(lerpInfos) + 1;
  Assert(n >= 1);
  i := Low(lerpInfos);
  if n = 1 then
  begin
    // use fast lerp (no blend)
    with lerpInfos[i] do
      Lerp(frameIndex1, frameIndex2, lerpFactor);
  end
  else
  begin
    if Assigned(FCurrentFrame) then
      FCurrentFrame.Free;
    FCurrentFrame := TGLSkeletonFrame.Create;
    FCurrentFrame.TransformMode :=
      Frames[lerpInfos[i].frameIndex1].TransformMode;
    with FCurrentFrame do
    begin
      blendPositions := TAffineVectorList.Create;
      // lerp first item separately
      Position.Lerp(Frames[lerpInfos[i].frameIndex1].Position,
        Frames[lerpInfos[i].frameIndex2].Position,
        lerpInfos[i].lerpFactor);
      if lerpInfos[i].weight <> 1 then
        Position.Scale(lerpInfos[i].weight);

      Inc(i);
      // combine the other items
      while i <= High(lerpInfos) do
      begin
        if not Assigned(lerpInfos[i].externalPositions) then
        begin
          blendPositions.Lerp(Frames[lerpInfos[i].frameIndex1].Position,
            Frames[lerpInfos[i].frameIndex2].Position,
            lerpInfos[i].lerpFactor);
          Position.AngleCombine(blendPositions, 1);
        end
        else
          Position.Combine(lerpInfos[i].externalPositions, 1);
        Inc(i);
      end;
      blendPositions.Free;

      i := Low(lerpInfos);
      case TransformMode of
        sftRotation:
        begin
          blendRotations := TAffineVectorList.Create;
          // lerp first item separately
          Rotation.AngleLerp(Frames[lerpInfos[i].frameIndex1].Rotation,
            Frames[lerpInfos[i].frameIndex2].Rotation,
            lerpInfos[i].lerpFactor);
          Inc(i);
          // combine the other items
          while i <= High(lerpInfos) do
          begin
            if not Assigned(lerpInfos[i].externalRotations) then
            begin
              blendRotations.AngleLerp(Frames[lerpInfos[i].frameIndex1].Rotation,
                Frames[lerpInfos[i].frameIndex2].Rotation,
                lerpInfos[i].lerpFactor);
              Rotation.AngleCombine(blendRotations, 1);
            end
            else
              Rotation.AngleCombine(lerpInfos[i].externalRotations, 1);
            Inc(i);
          end;
          blendRotations.Free;
        end;

        sftQuaternion:
        begin
          blendQuaternions := TQuaternionList.Create;
          // Initial frame lerp
          Quaternion.Lerp(Frames[lerpInfos[i].frameIndex1].Quaternion,
            Frames[lerpInfos[i].frameIndex2].Quaternion,
            lerpInfos[i].lerpFactor);
          Inc(i);
          // Combine the lerped frames together
          while i <= High(lerpInfos) do
          begin
            if not Assigned(lerpInfos[i].externalQuaternions) then
            begin
              blendQuaternions.Lerp(Frames[lerpInfos[i].frameIndex1].Quaternion,
                Frames[lerpInfos[i].frameIndex2].Quaternion,
                lerpInfos[i].lerpFactor);
              Quaternion.Combine(blendQuaternions, 1);
            end
            else
              Quaternion.Combine(lerpInfos[i].externalQuaternions, 1);
            Inc(i);
          end;
          blendQuaternions.Free;
        end;
      end;
    end;
  end;
end;

procedure TGLSkeleton.MakeSkeletalTranslationStatic(startFrame, endFrame: integer);
var
  delta: TAffineVector;
  i: integer;
  f: single;
begin
  if endFrame <= startFrame then
    Exit;
  delta := VectorSubtract(Frames[endFrame].Position[0],
    Frames[startFrame].Position[0]);
  f := -1 / (endFrame - startFrame);
  for i := startFrame to endFrame do
    Frames[i].Position[0] := VectorCombine(Frames[i].Position[0],
      delta, 1, (i - startFrame) * f);
end;

procedure TGLSkeleton.MakeSkeletalRotationDelta(startFrame, endFrame: integer);
var
  i, j: integer;
  v: TAffineVector;
begin
  if endFrame <= startFrame then
    Exit;
  for i := startFrame to endFrame do
  begin
    for j := 0 to Frames[i].Position.Count - 1 do
    begin
      Frames[i].Position[j] := NullVector;
      v := VectorSubtract(Frames[i].Rotation[j], Frames[0].Rotation[j]);
      if VectorNorm(v) < 1e-6 then
        Frames[i].Rotation[j] := NullVector
      else
        Frames[i].Rotation[j] := v;
    end;
  end;
end;

procedure TGLSkeleton.MorphMesh(normalize: boolean);
var
  i: integer;
  mesh: TBaseMeshObject;
begin
  if Owner.MeshObjects.Count > 0 then
  begin
    RootBones.PrepareGlobalMatrices;
    if Colliders.Count > 0 then
      Colliders.AlignColliders;

    if FMorphInvisibleParts then
      for i := 0 to Owner.MeshObjects.Count - 1 do
      begin
        mesh := Owner.MeshObjects.Items[i];
        if (mesh is TGLSkeletonMeshObject) then
          TGLSkeletonMeshObject(mesh).ApplyCurrentSkeletonFrame(normalize);
      end
    else
      for i := 0 to Owner.MeshObjects.Count - 1 do
      begin
        mesh := Owner.MeshObjects.Items[i];
        if (mesh is TGLSkeletonMeshObject) and mesh.Visible then
          TGLSkeletonMeshObject(mesh).ApplyCurrentSkeletonFrame(normalize);
      end;
  end;
end;

procedure TGLSkeleton.Synchronize(reference: TGLSkeleton);
begin
  CurrentFrame.Assign(reference.CurrentFrame);
  MorphMesh(True);
end;

procedure TGLSkeleton.Clear;
begin
  FlushBoneByIDCache;
  RootBones.Clean;
  Frames.Clear;
  FCurrentFrame.Free;
  FCurrentFrame := nil;
  FColliders.Clear;
end;

procedure TGLSkeleton.StartRagDoll; // ragdoll
var
  i: integer;
  mesh: TBaseMeshObject;
begin
  if FRagDollEnabled then
    Exit
  else
    FRagDollEnabled := True;

  if Owner.MeshObjects.Count > 0 then
  begin
    for i := 0 to Owner.MeshObjects.Count - 1 do
    begin
      mesh := Owner.MeshObjects.Items[i];
      if mesh is TGLSkeletonMeshObject then
      begin
        TGLSkeletonMeshObject(mesh).BackupBoneMatrixInvertedMeshes;
        TGLSkeletonMeshObject(mesh).PrepareBoneMatrixInvertedMeshes;
      end;
    end;
  end;
end;

procedure TGLSkeleton.StopRagDoll; // ragdoll
var
  i: integer;
  mesh: TBaseMeshObject;
begin
  FRagDollEnabled := False;
  if Owner.MeshObjects.Count > 0 then
  begin
    for i := 0 to Owner.MeshObjects.Count - 1 do
    begin
      mesh := Owner.MeshObjects.Items[i];
      if mesh is TGLSkeletonMeshObject then
        TGLSkeletonMeshObject(mesh).RestoreBoneMatrixInvertedMeshes;
    end;
  end;
end;

{%endregion%}

{%region=====[ TGLMeshObject ]==================================================}

constructor TGLMeshObject.CreateOwned(AOwner: TGLMeshObjectList);
begin
  FOwner := AOwner;
  Create;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

constructor TGLMeshObject.Create;
begin
  FMode := momTriangles;
  FTexCoords := TAffineVectorList.Create;
  FLightMapTexCoords := TAffineVectorList.Create;
  FColors := TVectorList.Create;
  FFaceGroups := TGLFaceGroups.CreateOwned(Self);
  FTexCoordsEx := TList.Create;
  FTangentsTexCoordIndex := 1;
  FBinormalsTexCoordIndex := 2;

  FUseVBO := vGLVectorFileObjectsEnableVBOByDefault;
  inherited;
end;

destructor TGLMeshObject.Destroy;
var
  i: integer;
begin
  FVerticesVBO.Free;
  FNormalsVBO.Free;
  FColorsVBO.Free;
  for i := 0 to high(FTexCoordsVBO) do
    FTexCoordsVBO[i].Free;
  FLightmapTexCoordsVBO.Free;

  FFaceGroups.Free;
  FColors.Free;
  FTexCoords.Free;
  FLightMapTexCoords.Free;
  for i := 0 to FTexCoordsEx.Count - 1 do
    TVectorList(FTexCoordsEx[i]).Free;
  FTexCoordsEx.Free;
  if Assigned(FOwner) then
    FOwner.Remove(Self);
  inherited;
end;

procedure TGLMeshObject.Assign(Source: TPersistent);
var
  I: integer;
begin
  inherited Assign(Source);

  if Source is TGLMeshObject then
  begin
    FTexCoords.Assign(TGLMeshObject(Source).FTexCoords);
    FLightMapTexCoords.Assign(TGLMeshObject(Source).FLightMapTexCoords);
    FColors.Assign(TGLMeshObject(Source).FColors);
    FFaceGroups.Assign(TGLMeshObject(Source).FFaceGroups);
    FMode := TGLMeshObject(Source).FMode;
    FRenderingOptions := TGLMeshObject(Source).FRenderingOptions;
    FBinormalsTexCoordIndex := TGLMeshObject(Source).FBinormalsTexCoordIndex;
    FTangentsTexCoordIndex := TGLMeshObject(Source).FTangentsTexCoordIndex;

    // Clear FTexCoordsEx.
    for I := 0 to FTexCoordsEx.Count - 1 do
      TVectorList(FTexCoordsEx[I]).Free;

    FTexCoordsEx.Count := TGLMeshObject(Source).FTexCoordsEx.Count;

    // Fill FTexCoordsEx.
    for I := 0 to FTexCoordsEx.Count - 1 do
    begin
      FTexCoordsEx[I] := TVectorList.Create;
      TVectorList(FTexCoordsEx[I]).Assign(TGLMeshObject(Source).FTexCoordsEx[I]);
    end;
  end;
end;

procedure TGLMeshObject.WriteToFiler(writer: TVirtualWriter);
var
  i: integer;
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(3); // Archive Version 3
    FTexCoords.WriteToFiler(writer);
    FLightMapTexCoords.WriteToFiler(writer);
    FColors.WriteToFiler(writer);
    FFaceGroups.WriteToFiler(writer);
    WriteInteger(integer(FMode));
    WriteInteger(SizeOf(FRenderingOptions));
    Write(FRenderingOptions, SizeOf(FRenderingOptions));
    WriteInteger(FTexCoordsEx.Count);
    for i := 0 to FTexCoordsEx.Count - 1 do
      TexCoordsEx[i].WriteToFiler(writer);
    WriteInteger(BinormalsTexCoordIndex);
    WriteInteger(TangentsTexCoordIndex);
  end;
end;

procedure TGLMeshObject.ReadFromFiler(reader: TVirtualReader);
var
  i, Count, archiveVersion: integer;
  lOldLightMapTexCoords: TTexPointList;
  tc: TTexPoint;
  size, ro: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion in [0..3] then
    with reader do
    begin
      FTexCoords.ReadFromFiler(reader);

      if archiveVersion = 0 then
      begin
        // FLightMapTexCoords did not exist back than.
        FLightMapTexCoords.Clear;
      end
      else if (archiveVersion = 1) or (archiveVersion = 2) then
      begin
        lOldLightMapTexCoords := TTexPointList.CreateFromFiler(reader);
        for i := 0 to lOldLightMapTexCoords.Count - 1 do
        begin
          tc := lOldLightMapTexCoords[i];
          FLightMapTexCoords.Add(tc.S, tc.T);
        end;
        lOldLightMapTexCoords.Free;
      end
      else
      begin
        // Load FLightMapTexCoords the normal way.
        FLightMapTexCoords.ReadFromFiler(reader);
      end;

      FColors.ReadFromFiler(reader);
      FFaceGroups.ReadFromFiler(reader);
      FMode := TGLMeshObjectMode(ReadInteger);
      size := ReadInteger;
      ro := 0;
      Read(ro, size);
     {$IF (FPC_VERSION > 2)}
      FRenderingOptions := TGLMeshObjectRenderingOptions(byte(ro));
     {$ELSE}
      FRenderingOptions := TGLMeshObjectRenderingOptions(ro);
     {$ENDIF}
      if archiveVersion >= 2 then
      begin
        Count := ReadInteger;
        for i := 0 to Count - 1 do
          TexCoordsEx[i].ReadFromFiler(reader);
        BinormalsTexCoordIndex := ReadInteger;
        TangentsTexCoordIndex := ReadInteger;
      end;
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TGLMeshObject.Clear;
var
  i: integer;
begin
  inherited;
  FFaceGroups.Clear;
  FColors.Clear;
  FTexCoords.Clear;
  FLightMapTexCoords.Clear;
  for i := 0 to FTexCoordsEx.Count - 1 do
    TexCoordsEx[i].Clear;
end;

function TGLMeshObject.ExtractTriangles(texCoords: TAffineVectorList = nil;
  normals: TAffineVectorList = nil): TAffineVectorList;
begin
  case mode of
    momTriangles:
    begin
      Result := inherited ExtractTriangles;
      if Assigned(texCoords) then
        texCoords.Assign(Self.TexCoords);
      if Assigned(normals) then
        normals.Assign(Self.Normals);
    end;
    momTriangleStrip:
    begin
      Result := TAffineVectorList.Create;
      ConvertStripToList(Vertices, Result);
      if Assigned(texCoords) then
        ConvertStripToList(Self.TexCoords, texCoords);
      if Assigned(normals) then
        ConvertStripToList(Self.Normals, normals);
    end;
    momFaceGroups:
    begin
      Result := TAffineVectorList.Create;
      FaceGroups.AddToTriangles(Result, texCoords, normals);
    end;
    else
      Result := nil;
      Assert(False);
  end;
end;

function TGLMeshObject.TriangleCount: integer;
var
  i: integer;
begin
  case Mode of
    momTriangles:
      Result := (Vertices.Count div 3);
    momTriangleStrip:
    begin
      Result := Vertices.Count - 2;
      if Result < 0 then
        Result := 0;
    end;
    momFaceGroups:
    begin
      Result := 0;
      for i := 0 to FaceGroups.Count - 1 do
        Result := Result + FaceGroups[i].TriangleCount;
    end;
    else
      Result := 0;
      Assert(False);
  end;
end;

procedure TGLMeshObject.PrepareMaterialLibraryCache(matLib: TGLMaterialLibrary);
begin
  FaceGroups.PrepareMaterialLibraryCache(matLib);
end;

procedure TGLMeshObject.DropMaterialLibraryCache;
begin
  FaceGroups.DropMaterialLibraryCache;
end;

procedure TGLMeshObject.GetExtents(out min, max: TAffineVector);
begin
  if FVertices.Revision <> FExtentCacheRevision then
  begin
    FVertices.GetExtents(FExtentCache.min, FExtentCache.max);
    FExtentCacheRevision := FVertices.Revision;
  end;
  min := FExtentCache.min;
  max := FExtentCache.max;
end;

procedure TGLMeshObject.GetExtents(out aabb: TAABB);
begin
  if FVertices.Revision <> FExtentCacheRevision then
  begin
    FVertices.GetExtents(FExtentCache.min, FExtentCache.max);
    FExtentCacheRevision := FVertices.Revision;
  end;
  aabb := FExtentCache;
end;

function TGLMeshObject.GetBarycenter: TVector;
var
  dMin, dMax: TAffineVector;
begin
  GetExtents(dMin, dMax);

  Result.V[0] := (dMin.V[0] + dMax.V[0]) / 2;
  Result.V[1] := (dMin.V[1] + dMax.V[1]) / 2;
  Result.V[2] := (dMin.V[2] + dMax.V[2]) / 2;
  Result.V[3] := 0;
end;

procedure TGLMeshObject.Prepare;
var
  i: integer;
begin
  ValidBuffers := [];
  for i := 0 to FaceGroups.Count - 1 do
    FaceGroups[i].Prepare;
end;

function TGLMeshObject.PointInObject(const aPoint: TAffineVector): boolean;
var
  min, max: TAffineVector;
begin
  GetExtents(min, max);
  Result := (aPoint.V[0] >= min.V[0]) and (aPoint.V[1] >= min.V[1]) and
    (aPoint.V[2] >= min.V[2]) and (aPoint.V[0] <= max.V[0]) and
    (aPoint.V[1] <= max.V[1]) and (aPoint.V[2] <= max.V[2]);
end;

procedure TGLMeshObject.SetTexCoords(const val: TAffineVectorList);
begin
  FTexCoords.Assign(val);
end;

procedure TGLMeshObject.SetLightmapTexCoords(const val: TAffineVectorList);
begin
  FLightMapTexCoords.Assign(val);
end;

procedure TGLMeshObject.SetColors(const val: TVectorList);
begin
  FColors.Assign(val);
end;

procedure TGLMeshObject.SetTexCoordsEx(index: integer; const val: TVectorList);
begin
  TexCoordsEx[index].Assign(val);
end;

function TGLMeshObject.GetTexCoordsEx(index: integer): TVectorList;
var
  i: integer;
begin
  if index > FTexCoordsEx.Count - 1 then
    for i := FTexCoordsEx.Count - 1 to index do
      FTexCoordsEx.Add(TVectorList.Create);
  Result := TVectorList(FTexCoordsEx[index]);
end;

procedure TGLMeshObject.SetBinormals(const val: TVectorList);
begin
  Binormals.Assign(val);
end;

function TGLMeshObject.GetBinormals: TVectorList;
begin
  Result := TexCoordsEx[BinormalsTexCoordIndex];
end;

procedure TGLMeshObject.SetBinormalsTexCoordIndex(const val: integer);
begin
  Assert(val >= 0);
  if val <> FBinormalsTexCoordIndex then
  begin
    FBinormalsTexCoordIndex := val;
  end;
end;

procedure TGLMeshObject.SetTangents(const val: TVectorList);
begin
  Tangents.Assign(val);
end;

function TGLMeshObject.GetTangents: TVectorList;
begin
  Result := TexCoordsEx[TangentsTexCoordIndex];
end;

procedure TGLMeshObject.SetTangentsTexCoordIndex(const val: integer);
begin
  Assert(val >= 0);
  if val <> FTangentsTexCoordIndex then
  begin
    FTangentsTexCoordIndex := val;
  end;
end;

procedure TGLMeshObject.GetTriangleData(tri: integer; list: TAffineVectorList;
  var v0, v1, v2: TAffineVector);
var
  i, LastCount, Count: integer;
  fg: TFGVertexIndexList;
begin
  case Mode of
    momTriangles:
    begin
      v0 := list[3 * tri];
      v1 := list[3 * tri + 1];
      v2 := list[3 * tri + 2];
    end;
    momTriangleStrip:
    begin
      v0 := list[tri];
      v1 := list[tri + 1];
      v2 := list[tri + 2];
    end;
    momFaceGroups:
    begin
      Count := 0;
      for i := 0 to FaceGroups.Count - 1 do
      begin
        LastCount := Count;
        fg := TFGVertexIndexList(FaceGroups[i]);
        Count := Count + fg.TriangleCount;
        if Count > tri then
        begin
          Count := tri - LastCount;
          case fg.Mode of
            fgmmTriangles, fgmmFlatTriangles:
            begin
              v0 := list[fg.VertexIndices[3 * Count]];
              v1 := list[fg.VertexIndices[3 * Count + 1]];
              v2 := list[fg.VertexIndices[3 * Count + 2]];
            end;
            fgmmTriangleStrip:
            begin
              v0 := list[fg.VertexIndices[Count]];
              v1 := list[fg.VertexIndices[Count + 1]];
              v2 := list[fg.VertexIndices[Count + 2]];
            end;
            fgmmTriangleFan:
            begin
              v0 := list[fg.VertexIndices[0]];
              v1 := list[fg.VertexIndices[Count + 1]];
              v2 := list[fg.VertexIndices[Count + 2]];
            end;
            fgmmQuads:
            begin
              if Count mod 2 = 0 then
              begin
                v0 := list[fg.VertexIndices[4 * (Count div 2)]];
                v1 := list[fg.VertexIndices[4 * (Count div 2) + 1]];
                v2 := list[fg.VertexIndices[4 * (Count div 2) + 2]];
              end
              else
              begin
                v0 := list[fg.VertexIndices[4 * (Count div 2)]];
                v1 := list[fg.VertexIndices[4 * (Count div 2) + 2]];
                v2 := list[fg.VertexIndices[4 * (Count div 2) + 3]];
              end;
            end;
            else
              Assert(False);
          end;
          break;
        end;
      end;

    end;
    else
      Assert(False);
  end;
end;

procedure TGLMeshObject.GetTriangleData(tri: integer; list: TVectorList;
  var v0, v1, v2: TVector);
var
  i, LastCount, Count: integer;
  fg: TFGVertexIndexList;
begin
  case Mode of
    momTriangles:
    begin
      v0 := list[3 * tri];
      v1 := list[3 * tri + 1];
      v2 := list[3 * tri + 2];
    end;
    momTriangleStrip:
    begin
      v0 := list[tri];
      v1 := list[tri + 1];
      v2 := list[tri + 2];
    end;
    momFaceGroups:
    begin
      Count := 0;
      for i := 0 to FaceGroups.Count - 1 do
      begin
        LastCount := Count;
        fg := TFGVertexIndexList(FaceGroups[i]);
        Count := Count + fg.TriangleCount;
        if Count > tri then
        begin
          Count := tri - LastCount;
          case fg.Mode of
            fgmmTriangles, fgmmFlatTriangles:
            begin
              v0 := list[fg.VertexIndices[3 * Count]];
              v1 := list[fg.VertexIndices[3 * Count + 1]];
              v2 := list[fg.VertexIndices[3 * Count + 2]];
            end;
            fgmmTriangleStrip:
            begin
              v0 := list[fg.VertexIndices[Count]];
              v1 := list[fg.VertexIndices[Count + 1]];
              v2 := list[fg.VertexIndices[Count + 2]];
            end;
            fgmmTriangleFan:
            begin
              v0 := list[fg.VertexIndices[0]];
              v1 := list[fg.VertexIndices[Count + 1]];
              v2 := list[fg.VertexIndices[Count + 2]];
            end;
            fgmmQuads:
            begin
              if Count mod 2 = 0 then
              begin
                v0 := list[fg.VertexIndices[4 * (Count div 2)]];
                v1 := list[fg.VertexIndices[4 * (Count div 2) + 1]];
                v2 := list[fg.VertexIndices[4 * (Count div 2) + 2]];
              end
              else
              begin
                v0 := list[fg.VertexIndices[4 * (Count div 2)]];
                v1 := list[fg.VertexIndices[4 * (Count div 2) + 2]];
                v2 := list[fg.VertexIndices[4 * (Count div 2) + 3]];
              end;
            end;
            else
              Assert(False);
          end;
          break;
        end;
      end;

    end;
    else
      Assert(False);
  end;
end;

procedure TGLMeshObject.SetTriangleData(tri: integer; list: TAffineVectorList;
  const v0, v1, v2: TAffineVector);
var
  i, LastCount, Count: integer;
  fg: TFGVertexIndexList;
begin
  case Mode of
    momTriangles:
    begin
      list[3 * tri] := v0;
      list[3 * tri + 1] := v1;
      list[3 * tri + 2] := v2;
    end;
    momTriangleStrip:
    begin
      list[tri] := v0;
      list[tri + 1] := v1;
      list[tri + 2] := v2;
    end;
    momFaceGroups:
    begin
      Count := 0;
      for i := 0 to FaceGroups.Count - 1 do
      begin
        LastCount := Count;
        fg := TFGVertexIndexList(FaceGroups[i]);
        Count := Count + fg.TriangleCount;
        if Count > tri then
        begin
          Count := tri - LastCount;
          case fg.Mode of
            fgmmTriangles, fgmmFlatTriangles:
            begin
              list[fg.VertexIndices[3 * Count]] := v0;
              list[fg.VertexIndices[3 * Count + 1]] := v1;
              list[fg.VertexIndices[3 * Count + 2]] := v2;
            end;
            fgmmTriangleStrip:
            begin
              list[fg.VertexIndices[Count]] := v0;
              list[fg.VertexIndices[Count + 1]] := v1;
              list[fg.VertexIndices[Count + 2]] := v2;
            end;
            fgmmTriangleFan:
            begin
              list[fg.VertexIndices[0]] := v0;
              list[fg.VertexIndices[Count + 1]] := v1;
              list[fg.VertexIndices[Count + 2]] := v2;
            end;
            fgmmQuads:
            begin
              if Count mod 2 = 0 then
              begin
                list[fg.VertexIndices[4 * (Count div 2)]] := v0;
                list[fg.VertexIndices[4 * (Count div 2) + 1]] := v1;
                list[fg.VertexIndices[4 * (Count div 2) + 2]] := v2;
              end
              else
              begin
                list[fg.VertexIndices[4 * (Count div 2)]] := v0;
                list[fg.VertexIndices[4 * (Count div 2) + 2]] := v1;
                list[fg.VertexIndices[4 * (Count div 2) + 3]] := v2;
              end;
            end;
            else
              Assert(False);
          end;
          break;
        end;
      end;

    end;
    else
      Assert(False);
  end;
end;

procedure TGLMeshObject.SetTriangleData(tri: integer; list: TVectorList;
  const v0, v1, v2: TVector);
var
  i, LastCount, Count: integer;
  fg: TFGVertexIndexList;
begin
  case Mode of
    momTriangles:
    begin
      list[3 * tri] := v0;
      list[3 * tri + 1] := v1;
      list[3 * tri + 2] := v2;
    end;
    momTriangleStrip:
    begin
      list[tri] := v0;
      list[tri + 1] := v1;
      list[tri + 2] := v2;
    end;
    momFaceGroups:
    begin
      Count := 0;
      for i := 0 to FaceGroups.Count - 1 do
      begin
        LastCount := Count;
        fg := TFGVertexIndexList(FaceGroups[i]);
        Count := Count + fg.TriangleCount;
        if Count > tri then
        begin
          Count := tri - LastCount;
          case fg.Mode of
            fgmmTriangles, fgmmFlatTriangles:
            begin
              list[fg.VertexIndices[3 * Count]] := v0;
              list[fg.VertexIndices[3 * Count + 1]] := v1;
              list[fg.VertexIndices[3 * Count + 2]] := v2;
            end;
            fgmmTriangleStrip:
            begin
              list[fg.VertexIndices[Count]] := v0;
              list[fg.VertexIndices[Count + 1]] := v1;
              list[fg.VertexIndices[Count + 2]] := v2;
            end;
            fgmmTriangleFan:
            begin
              list[fg.VertexIndices[0]] := v0;
              list[fg.VertexIndices[Count + 1]] := v1;
              list[fg.VertexIndices[Count + 2]] := v2;
            end;
            fgmmQuads:
            begin
              if Count mod 2 = 0 then
              begin
                list[fg.VertexIndices[4 * (Count div 2)]] := v0;
                list[fg.VertexIndices[4 * (Count div 2) + 1]] := v1;
                list[fg.VertexIndices[4 * (Count div 2) + 2]] := v2;
              end
              else
              begin
                list[fg.VertexIndices[4 * (Count div 2)]] := v0;
                list[fg.VertexIndices[4 * (Count div 2) + 2]] := v1;
                list[fg.VertexIndices[4 * (Count div 2) + 3]] := v2;
              end;
            end;
            else
              Assert(False);
          end;
          break;
        end;
      end;

    end;
    else
      Assert(False);
  end;
end;

procedure TGLMeshObject.SetUseVBO(const Value: boolean);
var
  i: integer;
begin
  if Value = FUseVBO then
    exit;

  if FUseVBO then
  begin
    FreeAndNil(FVerticesVBO);
    FreeAndNil(FNormalsVBO);
    FreeAndNil(FColorsVBO);
    for i := 0 to high(FTexCoordsVBO) do
      FreeAndNil(FTexCoordsVBO[i]);
    FreeAndNil(FLightmapTexCoordsVBO);
  end;

  FValidBuffers := [];

  FUseVBO := Value;
end;

procedure TGLMeshObject.SetValidBuffers(Value: TVBOBuffers);
var
  I: integer;
begin
  if FValidBuffers <> Value then
  begin
    FValidBuffers := Value;
    if Assigned(FVerticesVBO) then
      FVerticesVBO.NotifyChangesOfData;
    if Assigned(FNormalsVBO) then
      FNormalsVBO.NotifyChangesOfData;
    if Assigned(FColorsVBO) then
      FColorsVBO.NotifyChangesOfData;
    for I := 0 to high(FTexCoordsVBO) do
      if Assigned(FTexCoordsVBO[I]) then
        FTexCoordsVBO[I].NotifyChangesOfData;
    if Assigned(FLightmapTexCoordsVBO) then
      FLightmapTexCoordsVBO.NotifyChangesOfData;
  end;
end;

procedure TGLMeshObject.BuildTangentSpace(buildBinormals: boolean = True;
  buildTangents: boolean = True);
var
  i, j: integer;
  v, n, t: array[0..2] of TAffineVector;
  tangent, binormal: array[0..2] of TVector;
  vt, tt: TAffineVector;
  interp, dot: single;

  procedure SortVertexData(sortidx: integer);
  begin
    if t[0].V[sortidx] < t[1].V[sortidx] then
    begin
      vt := v[0];
      tt := t[0];
      v[0] := v[1];
      t[0] := t[1];
      v[1] := vt;
      t[1] := tt;
    end;
    if t[0].V[sortidx] < t[2].V[sortidx] then
    begin
      vt := v[0];
      tt := t[0];
      v[0] := v[2];
      t[0] := t[2];
      v[2] := vt;
      t[2] := tt;
    end;
    if t[1].V[sortidx] < t[2].V[sortidx] then
    begin
      vt := v[1];
      tt := t[1];
      v[1] := v[2];
      t[1] := t[2];
      v[2] := vt;
      t[2] := tt;
    end;
  end;

begin
  Tangents.Clear;
  Binormals.Clear;
  if buildTangents then
    Tangents.Count := Vertices.Count;
  if buildBinormals then
    Binormals.Count := Vertices.Count;
  for i := 0 to TriangleCount - 1 do
  begin
    // Get triangle data
    GetTriangleData(i, Vertices, v[0], v[1], v[2]);
    GetTriangleData(i, Normals, n[0], n[1], n[2]);
    GetTriangleData(i, TexCoords, t[0], t[1], t[2]);

    for j := 0 to 2 do
    begin
      // Compute tangent
      if buildTangents then
      begin
        SortVertexData(1);

        if (t[2].V[1] - t[0].V[1]) = 0 then
          interp := 1
        else
          interp := (t[1].V[1] - t[0].V[1]) / (t[2].V[1] - t[0].V[1]);

        vt := VectorLerp(v[0], v[2], interp);
        interp := t[0].V[0] + (t[2].V[0] - t[0].V[0]) * interp;
        vt := VectorSubtract(vt, v[1]);
        if t[1].V[0] < interp then
          vt := VectorNegate(vt);
        dot := VectorDotProduct(vt, n[j]);
        vt.V[0] := vt.V[0] - n[j].V[0] * dot;
        vt.V[1] := vt.V[1] - n[j].V[1] * dot;
        vt.V[2] := vt.V[2] - n[j].V[2] * dot;
        tangent[j] := VectorMake(VectorNormalize(vt), 0);
      end;

      // Compute Bi-Normal
      if buildBinormals then
      begin
        SortVertexData(0);

        if (t[2].V[0] - t[0].V[0]) = 0 then
          interp := 1
        else
          interp := (t[1].V[0] - t[0].V[0]) / (t[2].V[0] - t[0].V[0]);

        vt := VectorLerp(v[0], v[2], interp);
        interp := t[0].V[1] + (t[2].V[1] - t[0].V[1]) * interp;
        vt := VectorSubtract(vt, v[1]);
        if t[1].V[1] < interp then
          vt := VectorNegate(vt);
        dot := VectorDotProduct(vt, n[j]);
        vt.V[0] := vt.V[0] - n[j].V[0] * dot;
        vt.V[1] := vt.V[1] - n[j].V[1] * dot;
        vt.V[2] := vt.V[2] - n[j].V[2] * dot;
        binormal[j] := VectorMake(VectorNormalize(vt), 0);
      end;
    end;

    if buildTangents then
      SetTriangleData(i, Tangents, tangent[0], tangent[1], tangent[2]);
    if buildBinormals then
      SetTriangleData(i, Binormals, binormal[0], binormal[1], binormal[2]);
  end;
end;

procedure TGLMeshObject.DeclareArraysToOpenGL(var mrci: TGLRenderContextInfo;
  evenIfAlreadyDeclared: boolean = False);
var
  i: integer;
  currentMapping: cardinal;
  lists: array[0..4] of pointer;
  tlists: array of pointer;
begin
  if evenIfAlreadyDeclared or (not FArraysDeclared) then
  begin
    FillChar(lists, sizeof(lists), 0);
    SetLength(tlists, FTexCoordsEx.Count);

    // workaround for ATI bug, disable element VBO if
    // inside a display list
    FUseVBO := FUseVBO and GL.ARB_vertex_buffer_object and
      not mrci.GLStates.InsideList;

    if not FUseVBO then
    begin
      lists[0] := Vertices.List;
      lists[1] := Normals.List;
      lists[2] := Colors.List;
      lists[3] := TexCoords.List;
      lists[4] := LightMapTexCoords.List;

      for i := 0 to FTexCoordsEx.Count - 1 do
        tlists[i] := TexCoordsEx[i].List;
    end
    else
    begin
      BufferArrays;
    end;

    if not mrci.ignoreMaterials then
    begin
      if Normals.Count > 0 then
      begin
        if FUseVBO then
          FNormalsVBO.Bind;
        GL.EnableClientState(GL_NORMAL_ARRAY);
        GL.NormalPointer(GL_FLOAT, 0, lists[1]);
      end
      else
        GL.DisableClientState(GL_NORMAL_ARRAY);
      if (Colors.Count > 0) and (not mrci.ignoreMaterials) then
      begin
        if FUseVBO then
          FColorsVBO.Bind;
        GL.EnableClientState(GL_COLOR_ARRAY);
        GL.ColorPointer(4, GL_FLOAT, 0, lists[2]);
      end
      else
        GL.DisableClientState(GL_COLOR_ARRAY);
      if TexCoords.Count > 0 then
      begin
        if FUseVBO then
          FTexCoordsVBO[0].Bind;
        xgl.EnableClientState(GL_TEXTURE_COORD_ARRAY);
        xgl.TexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), lists[3]);
      end
      else
        xgl.DisableClientState(GL_TEXTURE_COORD_ARRAY);
      if GL.ARB_multitexture then
      begin
        if LightMapTexCoords.Count > 0 then
        begin
          if FUseVBO then
            FLightmapTexCoordsVBO.Bind;
          GL.ClientActiveTexture(GL_TEXTURE1);
          GL.TexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), lists[4]);
          GL.EnableClientState(GL_TEXTURE_COORD_ARRAY);
        end;
        for i := 0 to FTexCoordsEx.Count - 1 do
        begin
          if TexCoordsEx[i].Count > 0 then
          begin
            if FUseVBO then
              FTexCoordsVBO[i].Bind;
            GL.ClientActiveTexture(GL_TEXTURE0 + i);
            GL.TexCoordPointer(4, GL_FLOAT, SizeOf(TVector), tlists[i]);
            GL.EnableClientState(GL_TEXTURE_COORD_ARRAY);
          end;
        end;
        GL.ClientActiveTexture(GL_TEXTURE0);
      end;
    end
    else
    begin
      GL.DisableClientState(GL_NORMAL_ARRAY);
      GL.DisableClientState(GL_COLOR_ARRAY);
      xgl.DisableClientState(GL_TEXTURE_COORD_ARRAY);
    end;

    if Vertices.Count > 0 then
    begin
      if FUseVBO then
        FVerticesVBO.Bind;
      GL.EnableClientState(GL_VERTEX_ARRAY);
      GL.VertexPointer(3, GL_FLOAT, 0, lists[0]);
    end
    else
      GL.DisableClientState(GL_VERTEX_ARRAY);

    if GL.EXT_compiled_vertex_array and (LightMapTexCoords.Count = 0) and
      not FUseVBO then
      GL.LockArrays(0, vertices.Count);

    FLastLightMapIndex := -1;
    FArraysDeclared := True;
    FLightMapArrayEnabled := False;
    if mrci.drawState <> dsPicking then
      FLastXOpenGLTexMapping := xgl.GetBitWiseMapping;
  end
  else
  begin
    if not mrci.ignoreMaterials and not (mrci.drawState = dsPicking) then
      if TexCoords.Count > 0 then
      begin
        currentMapping := xgl.GetBitWiseMapping;
        if FLastXOpenGLTexMapping <> currentMapping then
        begin
          xgl.EnableClientState(GL_TEXTURE_COORD_ARRAY);
          xgl.TexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector),
            TexCoords.List);
          FLastXOpenGLTexMapping := currentMapping;
        end;
      end;
  end;
end;

procedure TGLMeshObject.DisableOpenGLArrays(var mrci: TGLRenderContextInfo);
var
  i: integer;
begin
  if FArraysDeclared then
  begin
    DisableLightMapArray(mrci);
    if GL.EXT_compiled_vertex_array and (LightMapTexCoords.Count = 0) and
      not FUseVBO then
      GL.UnLockArrays;
    if Vertices.Count > 0 then
      GL.DisableClientState(GL_VERTEX_ARRAY);
    if not mrci.ignoreMaterials then
    begin
      if Normals.Count > 0 then
        GL.DisableClientState(GL_NORMAL_ARRAY);
      if (Colors.Count > 0) and (not mrci.ignoreMaterials) then
        GL.DisableClientState(GL_COLOR_ARRAY);
      if TexCoords.Count > 0 then
        xgl.DisableClientState(GL_TEXTURE_COORD_ARRAY);
      if GL.ARB_multitexture then
      begin
        if LightMapTexCoords.Count > 0 then
        begin
          GL.ClientActiveTexture(GL_TEXTURE1);
          GL.DisableClientState(GL_TEXTURE_COORD_ARRAY);
        end;
        for i := 0 to FTexCoordsEx.Count - 1 do
        begin
          if TexCoordsEx[i].Count > 0 then
          begin
            GL.ClientActiveTexture(GL_TEXTURE0 + i);
            GL.DisableClientState(GL_TEXTURE_COORD_ARRAY);
          end;
        end;
        GL.ClientActiveTexture(GL_TEXTURE0);
      end;
    end;

    if FUseVBO then
    begin
      if Vertices.Count > 0 then
        FVerticesVBO.UnBind;
      if Normals.Count > 0 then
        FNormalsVBO.UnBind;
      if Colors.Count > 0 then
        FColorsVBO.UnBind;
      if TexCoords.Count > 0 then
        FTexCoordsVBO[0].UnBind;
      if LightMapTexCoords.Count > 0 then
        FLightmapTexCoordsVBO.UnBind;
      if FTexCoordsEx.Count > 0 then
      begin
        for i := 0 to FTexCoordsEx.Count - 1 do
        begin
          if TexCoordsEx[i].Count > 0 then
            FTexCoordsVBO[i].UnBind;
        end;
      end;

    end;
    FArraysDeclared := False;
  end;
end;

procedure TGLMeshObject.EnableLightMapArray(var mrci: TGLRenderContextInfo);
begin
  if GL.ARB_multitexture and (not mrci.ignoreMaterials) then
  begin
    Assert(FArraysDeclared);
    if not FLightMapArrayEnabled then
    begin
      mrci.GLStates.ActiveTexture := 1;
      mrci.GLStates.ActiveTextureEnabled[ttTexture2D] := True;
      mrci.GLStates.ActiveTexture := 0;
      FLightMapArrayEnabled := True;
    end;
  end;
end;

procedure TGLMeshObject.DisableLightMapArray(var mrci: TGLRenderContextInfo);
begin
  if GL.ARB_multitexture and FLightMapArrayEnabled then
  begin
    mrci.GLStates.ActiveTexture := 1;
    mrci.GLStates.ActiveTextureEnabled[ttTexture2D] := False;
    mrci.GLStates.ActiveTexture := 0;
    FLightMapArrayEnabled := False;
  end;
end;

procedure TGLMeshObject.PrepareBuildList(var mrci: TGLRenderContextInfo);
var
  i: integer;
begin
  if (Mode = momFaceGroups) and Assigned(mrci.materialLibrary) then
  begin
    for i := 0 to FaceGroups.Count - 1 do
      with TGLFaceGroup(FaceGroups.List^[i]) do
      begin
        if MaterialCache <> nil then
          MaterialCache.PrepareBuildList;
      end;
  end;
end;

procedure TGLMeshObject.BufferArrays;
const
  BufferUsage = GL_DYNAMIC_DRAW;
var
  I: integer;
begin
  if Vertices.Count > 0 then
  begin
    if not Assigned(FVerticesVBO) then
      FVerticesVBO := TGLVBOArrayBufferHandle.Create;
    FVerticesVBO.AllocateHandle;

    if FVerticesVBO.IsDataNeedUpdate then
    begin
      FVerticesVBO.BindBufferData(
        Vertices.List,
        sizeof(TAffineVector) * Vertices.Count,
        BufferUsage);
      FVerticesVBO.NotifyDataUpdated;
      FVerticesVBO.UnBind;
    end;

    Include(FValidBuffers, vbVertices);
  end;

  if Normals.Count > 0 then
  begin
    if not Assigned(FNormalsVBO) then
      FNormalsVBO := TGLVBOArrayBufferHandle.Create;
    FNormalsVBO.AllocateHandle;

    if FNormalsVBO.IsDataNeedUpdate then
    begin
      FNormalsVBO.BindBufferData(
        Normals.List,
        sizeof(TAffineVector) * Normals.Count,
        BufferUsage);
      FNormalsVBO.NotifyDataUpdated;
      FNormalsVBO.UnBind;
    end;

    Include(FValidBuffers, vbNormals);
  end;

  if Colors.Count > 0 then
  begin
    if not Assigned(FColorsVBO) then
      FColorsVBO := TGLVBOArrayBufferHandle.Create;
    FColorsVBO.AllocateHandle;

    if FColorsVBO.IsDataNeedUpdate then
    begin
      FColorsVBO.BindBufferData(
        Colors.List,
        sizeof(TVector) * Colors.Count,
        BufferUsage);
      FColorsVBO.NotifyDataUpdated;
      FColorsVBO.UnBind;
    end;

    Include(FValidBuffers, vbColors);
  end;

  if TexCoords.Count > 0 then
  begin
    if Length(FTexCoordsVBO) < 1 then
      SetLength(FTexCoordsVBO, 1);

    if not Assigned(FTexCoordsVBO[0]) then
      FTexCoordsVBO[0] := TGLVBOArrayBufferHandle.Create;
    FTexCoordsVBO[0].AllocateHandle;

    if FTexCoordsVBO[0].IsDataNeedUpdate then
    begin
      FTexCoordsVBO[0].BindBufferData(
        TexCoords.List,
        sizeof(TAffineVector) * TexCoords.Count,
        BufferUsage);
      FTexCoordsVBO[0].NotifyDataUpdated;
      FTexCoordsVBO[0].UnBind;
    end;

    Include(FValidBuffers, vbTexCoords);
  end;

  if LightMapTexCoords.Count > 0 then
  begin
    if not Assigned(FLightmapTexCoordsVBO) then
      FLightmapTexCoordsVBO := TGLVBOArrayBufferHandle.Create;
    FLightmapTexCoordsVBO.AllocateHandle;

    FLightmapTexCoordsVBO.BindBufferData(
      LightMapTexCoords.List,
      sizeof(TAffineVector) * LightMapTexCoords.Count,
      BufferUsage);
    FLightmapTexCoordsVBO.NotifyDataUpdated;
    FLightmapTexCoordsVBO.UnBind;

    Include(FValidBuffers, vbLightMapTexCoords);
  end;

  if FTexCoordsEx.Count > 0 then
  begin
    if Length(FTexCoordsVBO) < FTexCoordsEx.Count then
      SetLength(FTexCoordsVBO, FTexCoordsEx.Count);

    for I := 0 to FTexCoordsEx.Count - 1 do
    begin
      if TexCoordsEx[i].Count <= 0 then
        continue;

      if not Assigned(FTexCoordsVBO[i]) then
        FTexCoordsVBO[i] := TGLVBOArrayBufferHandle.Create;
      FTexCoordsVBO[i].AllocateHandle;

      if FTexCoordsVBO[i].IsDataNeedUpdate then
      begin
        FTexCoordsVBO[i].BindBufferData(
          TexCoordsEx[i].List, sizeof(TVector) * TexCoordsEx[i].Count,
          BufferUsage);
        FTexCoordsVBO[i].NotifyDataUpdated;
        FTexCoordsVBO[i].UnBind;
      end;
    end;

    Include(FValidBuffers, vbTexCoordsEx);
  end;

  GL.CheckError;
end;

procedure TGLMeshObject.BuildList(var mrci: TGLRenderContextInfo);
var
  i, j, groupID, nbGroups: integer;
  gotNormals, gotTexCoords, gotColor: boolean;
  gotTexCoordsEx: array of boolean;
  libMat: TGLLibMaterial;
  fg: TGLFaceGroup;
begin
  // Make sure no VBO is bound and states enabled
  FArraysDeclared := False;
  FLastXOpenGLTexMapping := 0;
  gotColor := (Vertices.Count = Colors.Count);
  if gotColor then
  begin
    mrci.GLStates.Enable(stColorMaterial);
    GL.ColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
    mrci.GLStates.SetGLMaterialColors(cmFront, clrBlack, clrGray20,
      clrGray80, clrBlack, 0);
    mrci.GLStates.SetGLMaterialColors(cmBack, clrBlack, clrGray20,
      clrGray80, clrBlack, 0);
  end;
  case Mode of
    momTriangles, momTriangleStrip: if Vertices.Count > 0 then
      begin
        DeclareArraysToOpenGL(mrci);
        gotNormals := (Vertices.Count = Normals.Count);
        gotTexCoords := (Vertices.Count = TexCoords.Count);
        SetLength(gotTexCoordsEx, FTexCoordsEx.Count);
        for i := 0 to FTexCoordsEx.Count - 1 do
          gotTexCoordsEx[i] :=
            (TexCoordsEx[i].Count > 0) and GL.ARB_multitexture;
        if Mode = momTriangles then
          GL.Begin_(GL_TRIANGLES)
        else
          GL.Begin_(GL_TRIANGLE_STRIP);
        for i := 0 to Vertices.Count - 1 do
        begin
          if gotNormals then
            GL.Normal3fv(@Normals.List[i]);
          if gotColor then
            GL.Color4fv(@Colors.List[i]);
          if FTexCoordsEx.Count > 0 then
          begin
            if gotTexCoordsEx[0] then
              GL.MultiTexCoord4fv(GL_TEXTURE0, @TexCoordsEx[0].List[i])
            else if gotTexCoords then
              xgl.TexCoord2fv(@TexCoords.List[i]);
            for j := 1 to FTexCoordsEx.Count - 1 do
              if gotTexCoordsEx[j] then
                GL.MultiTexCoord4fv(GL_TEXTURE0 + j,
                  @TexCoordsEx[j].List[i]);
          end
          else
          begin
            if gotTexCoords then
              xgl.TexCoord2fv(@TexCoords.List[i]);
          end;
          GL.Vertex3fv(@Vertices.List[i]);
        end;
        GL.End_;
      end;
    momFaceGroups:
    begin
      if Assigned(mrci.materialLibrary) then
      begin
        if moroGroupByMaterial in RenderingOptions then
        begin
          // group-by-material rendering, reduces material switches,
          // but alters rendering order
          groupID := vNextRenderGroupID;
          Inc(vNextRenderGroupID);
          for i := 0 to FaceGroups.Count - 1 do
          begin
            if FaceGroups[i].FRenderGroupID <> groupID then
            begin
              libMat := FaceGroups[i].FMaterialCache;
              if Assigned(libMat) then
                libMat.Apply(mrci);
              repeat
                for j := i to FaceGroups.Count - 1 do
                  with FaceGroups[j] do
                  begin
                    if (FRenderGroupID <> groupID) and
                      (FMaterialCache = libMat) then
                    begin
                      FRenderGroupID := groupID;
                      BuildList(mrci);
                    end;
                  end;
              until (not Assigned(libMat)) or (not libMat.UnApply(mrci));
            end;
          end;
        end
        else
        begin
          // canonical rendering (regroups only contiguous facegroups)
          i := 0;
          nbGroups := FaceGroups.Count;
          while i < nbGroups do
          begin
            libMat := FaceGroups[i].FMaterialCache;
            if Assigned(libMat) then
            begin
              libMat.Apply(mrci);
              repeat
                j := i;
                while j < nbGroups do
                begin
                  fg := FaceGroups[j];
                  if fg.MaterialCache <> libMat then
                    Break;
                  fg.BuildList(mrci);
                  Inc(j);
                end;
              until not libMat.UnApply(mrci);
              i := j;
            end
            else
            begin
              FaceGroups[i].BuildList(mrci);
              Inc(i);
            end;
          end;
        end;
        // restore faceculling
        if (stCullFace in mrci.GLStates.States) then
        begin
          if not mrci.bufferFaceCull then
            mrci.GLStates.Disable(stCullFace);
        end
        else
        begin
          if mrci.bufferFaceCull then
            mrci.GLStates.Enable(stCullFace);
        end;
      end
      else
        for i := 0 to FaceGroups.Count - 1 do
          FaceGroups[i].BuildList(mrci);
    end;
    else
      Assert(False);
  end;
  DisableOpenGLArrays(mrci);
end;

{%endregion%}


{%region=====[ TGLMeshObjectList ]==============================================}

constructor TGLMeshObjectList.CreateOwned(aOwner: TGLBaseMesh);
begin
  FOwner := AOwner;
  Create;
end;

destructor TGLMeshObjectList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TGLMeshObjectList.ReadFromFiler(reader: TVirtualReader);
var
  i: integer;
  mesh: TGLMeshObject;
begin
  inherited;
  for i := 0 to Count - 1 do
  begin
    mesh := Items[i];
    mesh.FOwner := Self;
    if mesh is TGLSkeletonMeshObject then
      TGLSkeletonMeshObject(mesh).PrepareBoneMatrixInvertedMeshes;
  end;
end;

procedure TGLMeshObjectList.PrepareMaterialLibraryCache(matLib: TGLMaterialLibrary);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    TGLMeshObject(List^[i]).PrepareMaterialLibraryCache(matLib);
end;

procedure TGLMeshObjectList.DropMaterialLibraryCache;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    TGLMeshObject(List^[i]).DropMaterialLibraryCache;
end;

procedure TGLMeshObjectList.PrepareBuildList(var mrci: TGLRenderContextInfo);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
      if Visible then
        PrepareBuildList(mrci);
end;

procedure TGLMeshObjectList.BuildList(var mrci: TGLRenderContextInfo);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
      if Visible then
        BuildList(mrci);
end;

procedure TGLMeshObjectList.MorphTo(morphTargetIndex: integer);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i] is TMorphableMeshObject then
      TMorphableMeshObject(Items[i]).MorphTo(morphTargetIndex);
end;

procedure TGLMeshObjectList.Lerp(morphTargetIndex1, morphTargetIndex2: integer;
  lerpFactor: single);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i] is TMorphableMeshObject then
      TMorphableMeshObject(Items[i]).Lerp(morphTargetIndex1, morphTargetIndex2,
        lerpFactor);
end;

function TGLMeshObjectList.MorphTargetCount: integer;
var
  i: integer;
begin
  Result := MaxInt;
  for i := 0 to Count - 1 do
    if Items[i] is TMorphableMeshObject then
      with TMorphableMeshObject(Items[i]) do
        if Result > MorphTargets.Count then
          Result := MorphTargets.Count;
  if Result = MaxInt then
    Result := 0;
end;

procedure TGLMeshObjectList.Clear;
var
  i: integer;
begin
  DropMaterialLibraryCache;
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      FOwner := nil;
      Free;
    end;
  inherited;
end;

function TGLMeshObjectList.GetMeshObject(Index: integer): TGLMeshObject;
begin
  Result := TGLMeshObject(List^[Index]);
end;

procedure TGLMeshObjectList.GetExtents(out min, max: TAffineVector);
var
  i, k: integer;
  lMin, lMax: TAffineVector;
const
  cBigValue: single = 1e30;
  cSmallValue: single = -1e30;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for i := 0 to Count - 1 do
  begin
    GetMeshObject(i).GetExtents(lMin, lMax);
    for k := 0 to 2 do
    begin
      if lMin.V[k] < min.V[k] then
        min.V[k] := lMin.V[k];
      if lMax.V[k] > max.V[k] then
        max.V[k] := lMax.V[k];
    end;
  end;
end;

procedure TGLMeshObjectList.Translate(const delta: TAffineVector);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    GetMeshObject(i).Translate(delta);
end;

function TGLMeshObjectList.ExtractTriangles(texCoords: TAffineVectorList = nil;
  normals: TAffineVectorList = nil): TAffineVectorList;
var
  i: integer;
  obj: TGLMeshObject;
  objTris: TAffineVectorList;
  objTexCoords: TAffineVectorList;
  objNormals: TAffineVectorList;
begin
  Result := TAffineVectorList.Create;
  if Assigned(texCoords) then
    objTexCoords := TAffineVectorList.Create
  else
    objTexCoords := nil;
  if Assigned(normals) then
    objNormals := TAffineVectorList.Create
  else
    objNormals := nil;
  try
    for i := 0 to Count - 1 do
    begin
      obj := GetMeshObject(i);
      if not obj.Visible then
        continue;
      objTris := obj.ExtractTriangles(objTexCoords, objNormals);
      try
        Result.Add(objTris);
        if Assigned(texCoords) then
        begin
          texCoords.Add(objTexCoords);
          objTexCoords.Count := 0;
        end;
        if Assigned(normals) then
        begin
          normals.Add(objNormals);
          objNormals.Count := 0;
        end;
      finally
        objTris.Free;
      end;
    end;
  finally
    objTexCoords.Free;
    objNormals.Free;
  end;
end;

function TGLMeshObjectList.TriangleCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    Result := Result + Items[i].TriangleCount;
end;

procedure TGLMeshObjectList.Prepare;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Prepare;
end;

function TGLMeshObjectList.FindMeshByName(MeshName: string): TGLMeshObject;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Name = MeshName then
    begin
      Result := Items[i];
      break;
    end;
end;

procedure TGLMeshObjectList.BuildTangentSpace(buildBinormals, buildTangents: boolean);
var
  I: integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      GetMeshObject(I).BuildTangentSpace(buildBinormals, buildTangents);
end;

function TGLMeshObjectList.GetUseVBO: boolean;
var
  I: integer;
begin
  Result := True;
  if Count <> 0 then
    for I := 0 to Count - 1 do
      Result := Result and GetMeshObject(I).FUseVBO;
end;

procedure TGLMeshObjectList.SetUseVBO(const Value: boolean);
var
  I: integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      GetMeshObject(I).SetUseVBO(Value);
end;

{%endregion%}
{%region=====[ TMeshMorphTarget ]===============================================}

constructor TMeshMorphTarget.CreateOwned(AOwner: TMeshMorphTargetList);
begin
  FOwner := AOwner;
  Create;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

destructor TMeshMorphTarget.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.Remove(Self);
  inherited;
end;

procedure TMeshMorphTarget.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    //nothing
  end;
end;

procedure TMeshMorphTarget.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      //nothing
    end
  else
    RaiseFilerException(archiveVersion);
end;

{%endregion%}

{%region=====[ TMeshMorphTargetList ]===========================================}

constructor TMeshMorphTargetList.CreateOwned(AOwner: TPersistent);
begin
  FOwner := AOwner;
  Create;
end;

destructor TMeshMorphTargetList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TMeshMorphTargetList.ReadFromFiler(reader: TVirtualReader);
var
  i: integer;
begin
  inherited;
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

procedure TMeshMorphTargetList.Translate(const delta: TAffineVector);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Translate(delta);
end;

procedure TMeshMorphTargetList.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      FOwner := nil;
      Free;
    end;
  inherited;
end;

function TMeshMorphTargetList.GetMeshMorphTarget(Index: integer): TMeshMorphTarget;
begin
  Result := TMeshMorphTarget(List^[Index]);
end;

{%endregion%}

{%region=====[ TMorphableMeshObject ]===========================================}

constructor TMorphableMeshObject.Create;
begin
  inherited;
  FMorphTargets := TMeshMorphTargetList.CreateOwned(Self);
end;

destructor TMorphableMeshObject.Destroy;
begin
  FMorphTargets.Free;
  inherited;
end;

procedure TMorphableMeshObject.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FMorphTargets.WriteToFiler(writer);
  end;
end;

procedure TMorphableMeshObject.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FMorphTargets.ReadFromFiler(reader);
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TMorphableMeshObject.Clear;
begin
  inherited;
  FMorphTargets.Clear;
end;

procedure TMorphableMeshObject.Translate(const delta: TAffineVector);
begin
  inherited;
  MorphTargets.Translate(delta);
  ValidBuffers := ValidBuffers - [vbVertices];
end;

procedure TMorphableMeshObject.MorphTo(morphTargetIndex: integer);
begin
  if (morphTargetIndex = 0) and (MorphTargets.Count = 0) then
    Exit;
  Assert(cardinal(morphTargetIndex) < cardinal(MorphTargets.Count));
  with MorphTargets[morphTargetIndex] do
  begin
    if Vertices.Count > 0 then
    begin
      Self.Vertices.Assign(Vertices);
      ValidBuffers := ValidBuffers - [vbVertices];
    end;
    if Normals.Count > 0 then
    begin
      Self.Normals.Assign(Normals);
      ValidBuffers := ValidBuffers - [vbNormals];
    end;
  end;
end;

procedure TMorphableMeshObject.Lerp(morphTargetIndex1, morphTargetIndex2: integer;
  lerpFactor: single);
var
  mt1, mt2: TMeshMorphTarget;
begin
  Assert((cardinal(morphTargetIndex1) < cardinal(MorphTargets.Count)) and
    (cardinal(morphTargetIndex2) < cardinal(MorphTargets.Count)));
  if lerpFactor = 0 then
    MorphTo(morphTargetIndex1)
  else if lerpFactor = 1 then
    MorphTo(morphTargetIndex2)
  else
  begin
    mt1 := MorphTargets[morphTargetIndex1];
    mt2 := MorphTargets[morphTargetIndex2];
    if mt1.Vertices.Count > 0 then
    begin
      Vertices.Lerp(mt1.Vertices, mt2.Vertices, lerpFactor);
      ValidBuffers := ValidBuffers - [vbVertices];
    end;
    if mt1.Normals.Count > 0 then
    begin
      Normals.Lerp(mt1.Normals, mt2.Normals, lerpFactor);
      Normals.Normalize;
      ValidBuffers := ValidBuffers - [vbNormals];
    end;
  end;
end;

{%endregion%}

{%region=====[ TGLSkeletonMeshObject ]==========================================}

constructor TGLSkeletonMeshObject.Create;
begin
  FBoneMatrixInvertedMeshes := TList.Create;
  FBackupInvertedMeshes := TList.Create; // ragdoll
  inherited Create;
end;

destructor TGLSkeletonMeshObject.Destroy;
begin
  Clear;
  FBoneMatrixInvertedMeshes.Free;
  FBackupInvertedMeshes.Free;
  inherited Destroy;
end;

procedure TGLSkeletonMeshObject.WriteToFiler(writer: TVirtualWriter);
var
  i: integer;
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteInteger(FVerticeBoneWeightCount);
    WriteInteger(FBonesPerVertex);
    WriteInteger(FVerticeBoneWeightCapacity);
    for i := 0 to FVerticeBoneWeightCount - 1 do
      Write(FVerticesBonesWeights[i][0], FBonesPerVertex *
        SizeOf(TVertexBoneWeight));
  end;
end;

procedure TGLSkeletonMeshObject.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion, i: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FVerticeBoneWeightCount := ReadInteger;
      FBonesPerVertex := ReadInteger;
      FVerticeBoneWeightCapacity := ReadInteger;
      ResizeVerticesBonesWeights;
      for i := 0 to FVerticeBoneWeightCount - 1 do
        Read(FVerticesBonesWeights[i][0], FBonesPerVertex *
          SizeOf(TVertexBoneWeight));
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TGLSkeletonMeshObject.Clear;
var
  i: integer;
begin
  inherited;
  FVerticeBoneWeightCount := 0;
  FBonesPerVertex := 0;
  ResizeVerticesBonesWeights;
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
    TBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  FBoneMatrixInvertedMeshes.Clear;
end;

procedure TGLSkeletonMeshObject.SetVerticeBoneWeightCount(const val: integer);
begin
  if val <> FVerticeBoneWeightCount then
  begin
    FVerticeBoneWeightCount := val;
    if FVerticeBoneWeightCount > FVerticeBoneWeightCapacity then
      VerticeBoneWeightCapacity := FVerticeBoneWeightCount + 16;
    FLastVerticeBoneWeightCount := FVerticeBoneWeightCount;
  end;
end;

procedure TGLSkeletonMeshObject.SetVerticeBoneWeightCapacity(const val: integer);
begin
  if val <> FVerticeBoneWeightCapacity then
  begin
    FVerticeBoneWeightCapacity := val;
    ResizeVerticesBonesWeights;
  end;
end;

procedure TGLSkeletonMeshObject.SetBonesPerVertex(const val: integer);
begin
  if val <> FBonesPerVertex then
  begin
    FBonesPerVertex := val;
    ResizeVerticesBonesWeights;
  end;
end;

procedure TGLSkeletonMeshObject.ResizeVerticesBonesWeights;
var
  n, m, i, j: integer;
  newArea: PVerticesBoneWeights;
begin
  n := BonesPerVertex * VerticeBoneWeightCapacity;
  if n = 0 then
  begin
    // release everything
    if Assigned(FVerticesBonesWeights) then
    begin
      FreeMem(FVerticesBonesWeights[0]);
      FreeMem(FVerticesBonesWeights);
      FVerticesBonesWeights := nil;
    end;
  end
  else
  begin
    // allocate new area
    GetMem(newArea, VerticeBoneWeightCapacity * SizeOf(PVertexBoneWeightArray));
    newArea[0] := AllocMem(n * SizeOf(TVertexBoneWeight));
    for i := 1 to VerticeBoneWeightCapacity - 1 do
      newArea[i] := PVertexBoneWeightArray(PtrUInt(newArea[0]) +
        PtrUInt(i * SizeOf(TVertexBoneWeight) * BonesPerVertex));
    // transfer old data
    if FLastVerticeBoneWeightCount < VerticeBoneWeightCount then
      n := FLastVerticeBoneWeightCount
    else
      n := VerticeBoneWeightCount;
    if FLastBonesPerVertex < BonesPerVertex then
      m := FLastBonesPerVertex
    else
      m := BonesPerVertex;
    for i := 0 to n - 1 do
      for j := 0 to m - 1 do
        newArea[i][j] := VerticesBonesWeights[i][j];
    // release old area and switch to new
    if Assigned(FVerticesBonesWeights) then
    begin
      FreeMem(FVerticesBonesWeights[0]);
      FreeMem(FVerticesBonesWeights);
    end;
    FVerticesBonesWeights := newArea;
  end;
  FLastBonesPerVertex := FBonesPerVertex;
end;

procedure TGLSkeletonMeshObject.AddWeightedBone(aBoneID: integer; aWeight: single);
begin
  if BonesPerVertex < 1 then
    BonesPerVertex := 1;
  VerticeBoneWeightCount := VerticeBoneWeightCount + 1;
  with VerticesBonesWeights^[VerticeBoneWeightCount - 1]^[0] do
  begin
    BoneID := aBoneID;
    Weight := aWeight;
  end;
end;

procedure TGLSkeletonMeshObject.AddWeightedBones(
  const boneIDs: TVertexBoneWeightDynArray);
var
  i: integer;
  n: integer;
begin
  n := Length(boneIDs);
  if BonesPerVertex < n then
    BonesPerVertex := n;
  VerticeBoneWeightCount := VerticeBoneWeightCount + 1;
  for i := 0 to n - 1 do
  begin
    with VerticesBonesWeights^[VerticeBoneWeightCount - 1]^[i] do
    begin
      BoneID := boneIDs[i].BoneID;
      Weight := boneIDs[i].Weight;
    end;
  end;
end;

function TGLSkeletonMeshObject.FindOrAdd(boneID: integer;
  const vertex, normal: TAffineVector): integer;
var
  i: integer;
  dynArray: TVertexBoneWeightDynArray;
begin
  if BonesPerVertex > 1 then
  begin
    SetLength(dynArray, 1);
    dynArray[0].BoneID := boneID;
    dynArray[0].Weight := 1;
    Result := FindOrAdd(dynArray, vertex, normal);
    Exit;
  end;
  Result := -1;
  for i := 0 to Vertices.Count - 1 do
    if (VerticesBonesWeights^[i]^[0].BoneID = boneID) and
      VectorEquals(Vertices.List^[i], vertex) and
      VectorEquals(Normals.List^[i], normal) then
    begin
      Result := i;
      Break;
    end;
  if Result < 0 then
  begin
    AddWeightedBone(boneID, 1);
    Vertices.Add(vertex);
    Result := Normals.Add(normal);
  end;
end;

function TGLSkeletonMeshObject.FindOrAdd(const boneIDs: TVertexBoneWeightDynArray;
  const vertex, normal: TAffineVector): integer;
var
  i, j: integer;
  bonesMatch: boolean;
begin
  Result := -1;
  for i := 0 to Vertices.Count - 1 do
  begin
    bonesMatch := True;
    for j := 0 to High(boneIDs) do
    begin
      if (boneIDs[j].BoneID <> VerticesBonesWeights^[i]^[j].BoneID) or
        (boneIDs[j].Weight <> VerticesBonesWeights^[i]^[j].Weight) then
      begin
        bonesMatch := False;
        Break;
      end;
    end;
    if bonesMatch and VectorEquals(Vertices[i], vertex) and
      VectorEquals(Normals[i], normal) then
    begin
      Result := i;
      Break;
    end;
  end;
  if Result < 0 then
  begin
    AddWeightedBones(boneIDs);
    Vertices.Add(vertex);
    Result := Normals.Add(normal);
  end;
end;

procedure TGLSkeletonMeshObject.PrepareBoneMatrixInvertedMeshes;
var
  i, k, boneIndex: integer;
  invMesh: TBaseMeshObject;
  invMat: TMatrix;
  bone: TGLSkeletonBone;
  p: TVector;
begin
  // cleanup existing stuff
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
    TBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  FBoneMatrixInvertedMeshes.Clear;
  // calculate
  for k := 0 to BonesPerVertex - 1 do
  begin
    invMesh := TBaseMeshObject.Create;
    FBoneMatrixInvertedMeshes.Add(invMesh);
    invMesh.Vertices := Vertices;
    invMesh.Normals := Normals;
    for i := 0 to Vertices.Count - 1 do
    begin
      boneIndex := VerticesBonesWeights^[i]^[k].BoneID;
      bone := Owner.Owner.Skeleton.RootBones.BoneByID(boneIndex);
      // transform point
      MakePoint(p, Vertices[i]);
      invMat := bone.GlobalMatrix;
      InvertMatrix(invMat);
      p := VectorTransform(p, invMat);
      invMesh.Vertices[i] := PAffineVector(@p)^;
      // transform normal
      SetVector(p, Normals[i]);
      invMat := bone.GlobalMatrix;
      invMat.V[3] := NullHmgPoint;
      InvertMatrix(invMat);
      p := VectorTransform(p, invMat);
      invMesh.Normals[i] := PAffineVector(@p)^;
    end;
  end;
end;

procedure TGLSkeletonMeshObject.BackupBoneMatrixInvertedMeshes; // ragdoll
var
  i: integer;
  bm: TBaseMeshObject;
begin
  // cleanup existing stuff
  for i := 0 to FBackupInvertedMeshes.Count - 1 do
    TBaseMeshObject(FBackupInvertedMeshes[i]).Free;
  FBackupInvertedMeshes.Clear;
  // copy current stuff
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
  begin
    bm := TBaseMeshObject.Create;
    bm.Assign(TBaseMeshObject(FBoneMatrixInvertedMeshes[i]));
    FBackupInvertedMeshes.Add(bm);
    TBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  end;
  FBoneMatrixInvertedMeshes.Clear;
end;

procedure TGLSkeletonMeshObject.RestoreBoneMatrixInvertedMeshes; // ragdoll
var
  i: integer;
  bm: TBaseMeshObject;
begin
  // cleanup existing stuff
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
    TBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  FBoneMatrixInvertedMeshes.Clear;
  // restore the backup
  for i := 0 to FBackupInvertedMeshes.Count - 1 do
  begin
    bm := TBaseMeshObject.Create;
    bm.Assign(TBaseMeshObject(FBackupInvertedMeshes[i]));
    FBoneMatrixInvertedMeshes.Add(bm);
    TBaseMeshObject(FBackupInvertedMeshes[i]).Free;
  end;
  FBackupInvertedMeshes.Clear;
end;

procedure TGLSkeletonMeshObject.ApplyCurrentSkeletonFrame(normalize: boolean);
var
  i, j, boneID: integer;
  refVertices, refNormals: TAffineVectorList;
  n, nt: TVector;
  bone: TGLSkeletonBone;
  skeleton: TGLSkeleton;
  tempvert, tempnorm: TAffineVector;
begin
  with TBaseMeshObject(FBoneMatrixInvertedMeshes[0]) do
  begin
    refVertices := Vertices;
    refNormals := Normals;
  end;
  skeleton := Owner.Owner.Skeleton;
  n.V[3] := 0;
  if BonesPerVertex = 1 then
  begin
    // simple case, one bone per vertex
    for i := 0 to refVertices.Count - 1 do
    begin
      boneID := VerticesBonesWeights^[i]^[0].BoneID;
      bone := skeleton.BoneByID(boneID);
      Vertices.List^[i] := VectorTransform(refVertices.List^[i],
        bone.GlobalMatrix);
      PAffineVector(@n)^ := refNormals.List^[i];
      nt := VectorTransform(n, bone.GlobalMatrix);
      Normals.List^[i] := PAffineVector(@nt)^;
    end;
  end
  else
  begin
    // multiple bones per vertex
    for i := 0 to refVertices.Count - 1 do
    begin
      Vertices.List^[i] := NullVector;
      Normals.List^[i] := NullVector;
      for j := 0 to BonesPerVertex - 1 do
      begin
        with TBaseMeshObject(FBoneMatrixInvertedMeshes[j]) do
        begin
          refVertices := Vertices;
          refNormals := Normals;
        end;
        tempvert := NullVector;
        tempnorm := NullVector;
        if VerticesBonesWeights^[i]^[j].Weight <> 0 then
        begin
          boneID := VerticesBonesWeights^[i]^[j].BoneID;
          bone := skeleton.BoneByID(boneID);
          CombineVector(tempvert,
            VectorTransform(refVertices.List^[i], bone.GlobalMatrix),
            VerticesBonesWeights^[i]^[j].Weight);
          PAffineVector(@n)^ := refNormals.List^[i];
          n := VectorTransform(n, bone.GlobalMatrix);
          CombineVector(tempnorm,
            PAffineVector(@n)^,
            VerticesBonesWeights^[i]^[j].Weight);
        end;
        AddVector(Vertices.List^[i], tempvert);
        AddVector(Normals.List^[i], tempnorm);
      end;
    end;
  end;
  if normalize then
    Normals.Normalize;
end;

{%endregion%}

{%region=====[ TGLFaceGroup ]===================================================}

constructor TGLFaceGroup.CreateOwned(AOwner: TGLFaceGroups);
begin
  FOwner := AOwner;
  FLightMapIndex := -1;
  Create;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

destructor TGLFaceGroup.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.Remove(Self);
  inherited;
end;

procedure TGLFaceGroup.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    if FLightMapIndex < 0 then
    begin
      WriteInteger(0); // Archive Version 0
      WriteString(FMaterialName);
    end
    else
    begin
      WriteInteger(1); // Archive Version 1, added FLightMapIndex
      WriteString(FMaterialName);
      WriteInteger(FLightMapIndex);
    end;
  end;
end;

procedure TGLFaceGroup.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion in [0..1] then
    with reader do
    begin
      FMaterialName := ReadString;
      if archiveVersion >= 1 then
        FLightMapIndex := ReadInteger
      else
        FLightMapIndex := -1;
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TGLFaceGroup.AttachLightmap(lightMap: TGLTexture;
  var mrci: TGLRenderContextInfo);
begin
  if GL.ARB_multitexture then
    with lightMap do
    begin
      Assert(Image.NativeTextureTarget = ttTexture2D);
      mrci.GLStates.TextureBinding[1, ttTexture2D] := Handle;
      GL.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

      mrci.GLStates.ActiveTexture := 0;
    end;
end;

procedure TGLFaceGroup.AttachOrDetachLightmap(var mrci: TGLRenderContextInfo);
var
  libMat: TGLLibMaterial;
begin
  if GL.ARB_multitexture then
  begin
    if (not mrci.ignoreMaterials) and Assigned(mrci.lightmapLibrary) then
    begin
      if Owner.Owner.FLastLightMapIndex <> lightMapIndex then
      begin
        Owner.Owner.FLastLightMapIndex := lightMapIndex;
        if lightMapIndex >= 0 then
        begin
          // attach and activate lightmap
          Assert(lightMapIndex < TGLMaterialLibrary(
            mrci.lightmapLibrary).Materials.Count);
          libMat :=
            TGLMaterialLibrary(mrci.lightmapLibrary).Materials[lightMapIndex];
          AttachLightmap(libMat.Material.Texture, mrci);
          Owner.Owner.EnableLightMapArray(mrci);
        end
        else
        begin
          // desactivate lightmap
          Owner.Owner.DisableLightMapArray(mrci);
        end;
      end;
    end;
  end;
end;

procedure TGLFaceGroup.PrepareMaterialLibraryCache(matLib: TGLMaterialLibrary);
begin
  if (FMaterialName <> '') and (matLib <> nil) then
    FMaterialCache := matLib.Materials.GetLibMaterialByName(FMaterialName)
  else
    FMaterialCache := nil;
end;


procedure TGLFaceGroup.DropMaterialLibraryCache;
begin
  FMaterialCache := nil;
end;

procedure TGLFaceGroup.AddToTriangles(aList: TAffineVectorList;
  aTexCoords: TAffineVectorList = nil; aNormals: TAffineVectorList = nil);
begin
  // nothing
end;

procedure TGLFaceGroup.Reverse;
begin
  // nothing
end;

procedure TGLFaceGroup.Prepare;
begin
  // nothing
end;

{%endregion%}

{%region=====[ TFGVertexIndexList ]=============================================}

constructor TFGVertexIndexList.Create;
begin
  inherited;
  FVertexIndices := TIntegerList.Create;
  FMode := fgmmTriangles;
end;

destructor TFGVertexIndexList.Destroy;
begin
  FVertexIndices.Free;
  FIndexVBO.Free;
  inherited;
end;

procedure TFGVertexIndexList.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FVertexIndices.WriteToFiler(writer);
    WriteInteger(integer(FMode));
  end;
end;

procedure TFGVertexIndexList.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FVertexIndices.ReadFromFiler(reader);
      FMode := TGLFaceGroupMeshMode(ReadInteger);
      InvalidateVBO;
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TFGVertexIndexList.SetupVBO;
const
  BufferUsage = GL_STATIC_DRAW;
begin
  if not Assigned(FIndexVBO) then
    FIndexVBO := TGLVBOElementArrayHandle.Create;

  FIndexVBO.AllocateHandle;

  if FIndexVBO.IsDataNeedUpdate then
  begin
    FIndexVBO.BindBufferData(
      VertexIndices.List,
      SizeOf(integer) * VertexIndices.Count,
      BufferUsage);
    FIndexVBO.NotifyDataUpdated;
  end;
end;

procedure TFGVertexIndexList.SetVertexIndices(const val: TIntegerList);
begin
  FVertexIndices.Assign(val);
  InvalidateVBO;
end;

procedure TFGVertexIndexList.BuildList(var mrci: TGLRenderContextInfo);
const
  cFaceGroupMeshModeToOpenGL: array[TGLFaceGroupMeshMode] of integer =
    (GL_TRIANGLES, GL_TRIANGLE_STRIP, GL_TRIANGLES, GL_TRIANGLE_FAN, GL_QUADS);
begin
  if VertexIndices.Count = 0 then
    Exit;
  Owner.Owner.DeclareArraysToOpenGL(mrci, False);
  AttachOrDetachLightmap(mrci);

  if Owner.Owner.UseVBO then
  begin
    SetupVBO;

    FIndexVBO.Bind;
    GL.DrawElements(cFaceGroupMeshModeToOpenGL[Mode], VertexIndices.Count,
      GL_UNSIGNED_INT, nil);
    FIndexVBO.UnBind;
  end
  else
  begin
    GL.DrawElements(cFaceGroupMeshModeToOpenGL[Mode], VertexIndices.Count,
      GL_UNSIGNED_INT, VertexIndices.List);
  end;
end;

procedure TFGVertexIndexList.AddToList(Source, destination: TAffineVectorList;
  indices: TIntegerList);
var
  i, n: integer;
begin
  if not Assigned(destination) then
    Exit;
  if indices.Count < 3 then
    Exit;
  case Mode of
    fgmmTriangles, fgmmFlatTriangles:
    begin
      n := (indices.Count div 3) * 3;
      if Source.Count > 0 then
      begin
        destination.AdjustCapacityToAtLeast(destination.Count + n);
        for i := 0 to n - 1 do
          destination.Add(Source[indices.List^[i]]);
      end
      else
        destination.AddNulls(destination.Count + n);
    end;
    fgmmTriangleStrip:
    begin
      if Source.Count > 0 then
        ConvertStripToList(Source, indices, destination)
      else
        destination.AddNulls(destination.Count + (indices.Count - 2) * 3);
    end;
    fgmmTriangleFan:
    begin
      n := (indices.Count - 2) * 3;
      if Source.Count > 0 then
      begin
        destination.AdjustCapacityToAtLeast(destination.Count + n);
        for i := 2 to VertexIndices.Count - 1 do
        begin
          destination.Add(Source[indices.List^[0]],
            Source[indices.List^[i - 1]],
            Source[indices.List^[i]]);
        end;
      end
      else
        destination.AddNulls(destination.Count + n);
    end;
    fgmmQuads:
    begin
      n := indices.Count div 4;
      if Source.Count > 0 then
      begin
        destination.AdjustCapacityToAtLeast(destination.Count + n * 6);
        i := 0;
        while n > 0 do
        begin
          destination.Add(Source[indices.List^[i]],
            Source[indices.List^[i + 1]],
            Source[indices.List^[i + 2]]);
          destination.Add(Source[indices.List^[i]],
            Source[indices.List^[i + 2]],
            Source[indices.List^[i + 3]]);
          Inc(i, 4);
          Dec(n);
        end;
      end
      else
        destination.AddNulls(destination.Count + n * 6);
    end;
    else
      Assert(False);
  end;
end;

procedure TFGVertexIndexList.AddToTriangles(aList: TAffineVectorList;
  aTexCoords: TAffineVectorList = nil; aNormals: TAffineVectorList = nil);
var
  mo: TGLMeshObject;
begin
  mo := Owner.Owner;
  AddToList(mo.Vertices, aList, VertexIndices);
  AddToList(mo.TexCoords, aTexCoords, VertexIndices);
  AddToList(mo.Normals, aNormals, VertexIndices);
  InvalidateVBO;
end;

function TFGVertexIndexList.TriangleCount: integer;
begin
  case Mode of
    fgmmTriangles, fgmmFlatTriangles:
      Result := VertexIndices.Count div 3;
    fgmmTriangleFan, fgmmTriangleStrip:
    begin
      Result := VertexIndices.Count - 2;
      if Result < 0 then
        Result := 0;
    end;
    fgmmQuads:
      Result := VertexIndices.Count div 2;
    else
      Result := 0;
      Assert(False);
  end;
end;

procedure TFGVertexIndexList.Reverse;
begin
  VertexIndices.Reverse;
  InvalidateVBO;
end;

procedure TFGVertexIndexList.Add(idx: integer);
begin
  FVertexIndices.Add(idx);
  InvalidateVBO;
end;

procedure TFGVertexIndexList.GetExtents(var min, max: TAffineVector);
var
  i, k: integer;
  f: single;
  ref: PFloatArray;
const
  cBigValue: single = 1e50;
  cSmallValue: single = -1e50;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for i := 0 to VertexIndices.Count - 1 do
  begin
    ref := Owner.Owner.Vertices.ItemAddress[VertexIndices[i]];
    for k := 0 to 2 do
    begin
      f := ref^[k];
      if f < min.V[k] then
        min.V[k] := f;
      if f > max.V[k] then
        max.V[k] := f;
    end;
  end;
end;

procedure TFGVertexIndexList.ConvertToList;
var
  i: integer;
  bufList: TIntegerList;
begin
  if VertexIndices.Count >= 3 then
  begin
    case Mode of
      fgmmTriangleStrip:
      begin
        bufList := TIntegerList.Create;
        try
          ConvertStripToList(VertexIndices, bufList);
          VertexIndices := bufList;
        finally
          bufList.Free;
        end;
        FMode := fgmmTriangles;
      end;
      fgmmTriangleFan:
      begin
        bufList := TIntegerList.Create;
        try
          for i := 0 to VertexIndices.Count - 3 do
            bufList.Add(VertexIndices[0], VertexIndices[i],
              VertexIndices[i + 1]);
          VertexIndices := bufList;
        finally
          bufList.Free;
        end;
        FMode := fgmmTriangles;
      end;
    end;
    InvalidateVBO;
  end;
end;

function TFGVertexIndexList.GetNormal: TAffineVector;
begin
  if VertexIndices.Count < 3 then
    Result := NullVector
  else
    with Owner.Owner.Vertices do
      CalcPlaneNormal(Items[VertexIndices[0]], Items[VertexIndices[1]],
        Items[VertexIndices[2]], Result);
end;

procedure TFGVertexIndexList.InvalidateVBO;
begin
  if Assigned(FIndexVBO) then
    FIndexVBO.NotifyChangesOfData;
end;

{%endregion%}


{%region=====[ TFGVertexNormalTexIndexList ]====================================}

constructor TFGVertexNormalTexIndexList.Create;
begin
  inherited;
  FNormalIndices := TIntegerList.Create;
  FTexCoordIndices := TIntegerList.Create;
end;

destructor TFGVertexNormalTexIndexList.Destroy;
begin
  FTexCoordIndices.Free;
  FNormalIndices.Free;
  inherited;
end;

procedure TFGVertexNormalTexIndexList.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FNormalIndices.WriteToFiler(writer);
    FTexCoordIndices.WriteToFiler(writer);
  end;
end;

procedure TFGVertexNormalTexIndexList.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FNormalIndices.ReadFromFiler(reader);
      FTexCoordIndices.ReadFromFiler(reader);
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TFGVertexNormalTexIndexList.SetNormalIndices(const val: TIntegerList);
begin
  FNormalIndices.Assign(val);
end;

procedure TFGVertexNormalTexIndexList.SetTexCoordIndices(const val: TIntegerList);
begin
  FTexCoordIndices.Assign(val);
end;

procedure TFGVertexNormalTexIndexList.BuildList(var mrci: TGLRenderContextInfo);
var
  i: integer;
  vertexPool: PAffineVectorArray;
  normalPool: PAffineVectorArray;
  texCoordPool: PAffineVectorArray;
  colorPool: PVectorArray;
  normalIdxList, texCoordIdxList, vertexIdxList: PIntegerVector;
begin
  Assert(((TexCoordIndices.Count = 0) or (VertexIndices.Count <=
    TexCoordIndices.Count)) and ((NormalIndices.Count = 0) or
    (VertexIndices.Count <= NormalIndices.Count)));
  vertexPool := Owner.Owner.Vertices.List;
  normalPool := Owner.Owner.Normals.List;
  colorPool := Owner.Owner.Colors.List;
  texCoordPool := Owner.Owner.TexCoords.List;
  case Mode of
    fgmmTriangles, fgmmFlatTriangles: GL.Begin_(GL_TRIANGLES);
    fgmmTriangleStrip: GL.Begin_(GL_TRIANGLE_STRIP);
    fgmmTriangleFan: GL.Begin_(GL_TRIANGLE_FAN);
    else
      Assert(False);
  end;
  vertexIdxList := VertexIndices.List;
  if NormalIndices.Count > 0 then
    normalIdxList := NormalIndices.List
  else
    normalIdxList := vertexIdxList;
  if TexCoordIndices.Count > 0 then
    texCoordIdxList := TexCoordIndices.List
  else
    texCoordIdxList := vertexIdxList;

  for i := 0 to VertexIndices.Count - 1 do
  begin
    GL.Normal3fv(@normalPool[normalIdxList^[i]]);
    if Assigned(colorPool) then
      GL.Color4fv(@colorPool[vertexIdxList^[i]]);
    if Assigned(texCoordPool) then
      xgl.TexCoord2fv(@texCoordPool[texCoordIdxList^[i]]);
    GL.Vertex3fv(@vertexPool[vertexIdxList^[i]]);
  end;

  GL.End_;
end;

procedure TFGVertexNormalTexIndexList.AddToTriangles(aList: TAffineVectorList;
  aTexCoords: TAffineVectorList = nil; aNormals: TAffineVectorList = nil);
begin
  AddToList(Owner.Owner.Vertices, aList, VertexIndices);
  AddToList(Owner.Owner.TexCoords, aTexCoords, TexCoordIndices);
  AddToList(Owner.Owner.Normals, aNormals, NormalIndices);
end;

procedure TFGVertexNormalTexIndexList.Add(vertexIdx, normalIdx, texCoordIdx: integer);
begin
  inherited Add(vertexIdx);
  FNormalIndices.Add(normalIdx);
  FTexCoordIndices.Add(texCoordIdx);
end;

{%endregion%}

{%region=====[ TFGIndexTexCoordList ]===========================================}

constructor TFGIndexTexCoordList.Create;
begin
  inherited;
  FTexCoords := TAffineVectorList.Create;
end;

destructor TFGIndexTexCoordList.Destroy;
begin
  FTexCoords.Free;
  inherited;
end;

procedure TFGIndexTexCoordList.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FTexCoords.WriteToFiler(writer);
  end;
end;

procedure TFGIndexTexCoordList.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FTexCoords.ReadFromFiler(reader);
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TFGIndexTexCoordList.SetTexCoords(const val: TAffineVectorList);
begin
  FTexCoords.Assign(val);
end;

procedure TFGIndexTexCoordList.BuildList(var mrci: TGLRenderContextInfo);
var
  i, k: integer;
  texCoordPool: PAffineVectorArray;
  vertexPool: PAffineVectorArray;
  normalPool: PAffineVectorArray;
  indicesPool: PIntegerArray;
  colorPool: PVectorArray;
  gotColor: boolean;

begin
  Assert(VertexIndices.Count = TexCoords.Count);
  texCoordPool := TexCoords.List;
  vertexPool := Owner.Owner.Vertices.List;
  indicesPool := @VertexIndices.List[0];
  colorPool := @Owner.Owner.Colors.List[0];
  gotColor := (Owner.Owner.Vertices.Count = Owner.Owner.Colors.Count);

  case Mode of
    fgmmTriangles: GL.Begin_(GL_TRIANGLES);
    fgmmFlatTriangles: GL.Begin_(GL_TRIANGLES);
    fgmmTriangleStrip: GL.Begin_(GL_TRIANGLE_STRIP);
    fgmmTriangleFan: GL.Begin_(GL_TRIANGLE_FAN);
    fgmmQuads: GL.Begin_(GL_QUADS);
    else
      Assert(False);
  end;
  if Owner.Owner.Normals.Count = Owner.Owner.Vertices.Count then
  begin
    normalPool := Owner.Owner.Normals.List;
    for i := 0 to VertexIndices.Count - 1 do
    begin
      xgl.TexCoord2fv(@texCoordPool[i]);
      k := indicesPool[i];
      if gotColor then
        GL.Color4fv(@colorPool[k]);
      GL.Normal3fv(@normalPool[k]);
      GL.Vertex3fv(@vertexPool[k]);
    end;
  end
  else
  begin
    for i := 0 to VertexIndices.Count - 1 do
    begin
      xgl.TexCoord2fv(@texCoordPool[i]);
      if gotColor then
        GL.Color4fv(@colorPool[indicesPool[i]]);
      GL.Vertex3fv(@vertexPool[indicesPool[i]]);
    end;
  end;
  GL.End_;
  GL.CheckError;
end;

procedure TFGIndexTexCoordList.AddToTriangles(aList: TAffineVectorList;
  aTexCoords: TAffineVectorList = nil; aNormals: TAffineVectorList = nil);
var
  i, n: integer;
  texCoordList: TAffineVectorList;
begin
  AddToList(Owner.Owner.Vertices, aList, VertexIndices);
  AddToList(Owner.Owner.Normals, aNormals, VertexIndices);
  texCoordList := Self.TexCoords;
  case Mode of
    fgmmTriangles, fgmmFlatTriangles:
    begin
      if Assigned(aTexCoords) then
      begin
        n := (VertexIndices.Count div 3) * 3;
        aTexCoords.AdjustCapacityToAtLeast(aTexCoords.Count + n);
        for i := 0 to n - 1 do
          aTexCoords.Add(texCoordList[i]);
      end;
    end;
    fgmmTriangleStrip:
    begin
      if Assigned(aTexCoords) then
        ConvertStripToList(aTexCoords, texCoordList);
    end;
    fgmmTriangleFan:
    begin
      if Assigned(aTexCoords) then
      begin
        aTexCoords.AdjustCapacityToAtLeast(aTexCoords.Count +
          (VertexIndices.Count - 2) * 3);
        for i := 2 to VertexIndices.Count - 1 do
        begin
          aTexCoords.Add(texCoordList[0],
            texCoordList[i - 1],
            texCoordList[i]);
        end;
      end;
    end;
    else
      Assert(False);
  end;
end;

procedure TFGIndexTexCoordList.Add(idx: integer; const texCoord: TAffineVector);
begin
  TexCoords.Add(texCoord);
  inherited Add(idx);
end;

procedure TFGIndexTexCoordList.Add(idx: integer; const s, t: single);
begin
  TexCoords.Add(s, t, 0);
  inherited Add(idx);
end;

{%endregion%}

{%region=====[ TGLFaceGroups ]==================================================}

constructor TGLFaceGroups.CreateOwned(AOwner: TGLMeshObject);
begin
  FOwner := AOwner;
  Create;
end;

destructor TGLFaceGroups.Destroy;
begin
  Clear;
  inherited;
end;

procedure TGLFaceGroups.ReadFromFiler(reader: TVirtualReader);
var
  i: integer;
begin
  inherited;
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

procedure TGLFaceGroups.Clear;
var
  i: integer;
  fg: TGLFaceGroup;
begin
  for i := 0 to Count - 1 do
  begin
    fg := GetFaceGroup(i);
    if Assigned(fg) then
    begin
      fg.FOwner := nil;
      fg.Free;
    end;
  end;
  inherited;
end;

function TGLFaceGroups.GetFaceGroup(Index: integer): TGLFaceGroup;
begin
  Result := TGLFaceGroup(List^[Index]);
end;

procedure TGLFaceGroups.PrepareMaterialLibraryCache(matLib: TGLMaterialLibrary);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    TGLFaceGroup(List^[i]).PrepareMaterialLibraryCache(matLib);
end;

procedure TGLFaceGroups.DropMaterialLibraryCache;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    TGLFaceGroup(List^[i]).DropMaterialLibraryCache;
end;

procedure TGLFaceGroups.AddToTriangles(aList: TAffineVectorList;
  aTexCoords: TAffineVectorList = nil; aNormals: TAffineVectorList = nil);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].AddToTriangles(aList, aTexCoords, aNormals);
end;

function TGLFaceGroups.MaterialLibrary: TGLMaterialLibrary;
var
  mol: TGLMeshObjectList;
  bm: TGLBaseMesh;
begin
  if Assigned(Owner) then
  begin
    mol := Owner.Owner;
    if Assigned(mol) then
    begin
      bm := mol.Owner;
      if Assigned(bm) then
      begin
        Result := bm.MaterialLibrary;
        ;
        Exit;
      end;
    end;
  end;
  Result := nil;
end;

function CompareMaterials(item1, item2: TObject): integer;

  function MaterialIsOpaque(fg: TGLFaceGroup): boolean;
  var
    libMat: TGLLibMaterial;
  begin
    libMat := fg.MaterialCache;
    Result := (not Assigned(libMat)) or (not libMat.Material.Blended);
  end;

var
  fg1, fg2: TGLFaceGroup;
  opaque1, opaque2: boolean;
begin
  fg1 := TGLFaceGroup(item1);
  opaque1 := MaterialIsOpaque(fg1);
  fg2 := TGLFaceGroup(item2);
  opaque2 := MaterialIsOpaque(fg2);
  if opaque1 = opaque2 then
  begin
    Result := CompareStr(fg1.MaterialName, fg2.MaterialName);
    if Result = 0 then
      Result := fg1.LightMapIndex - fg2.LightMapIndex;
  end
  else if opaque1 then
    Result := -1
  else
    Result := 1;
end;

procedure TGLFaceGroups.SortByMaterial;
begin
  PrepareMaterialLibraryCache(Owner.Owner.Owner.MaterialLibrary);
  Sort(@CompareMaterials);
end;

{%endregion%}

{%region=====[ TGLVectorFile ]==================================================}

constructor TGLVectorFile.Create(AOwner: TPersistent);
begin
  Assert(AOwner is TGLBaseMesh);
  inherited;
end;

function TGLVectorFile.Owner: TGLBaseMesh;
begin
  Result := TGLBaseMesh(GetOwner);
end;

procedure TGLVectorFile.SetNormalsOrientation(const val: TMeshNormalsOrientation);
begin
  FNormalsOrientation := val;
end;

{%endregion%}


{%region=====[ TGLGLSMVectorFile ]==============================================}

class function TGLGLSMVectorFile.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

procedure TGLGLSMVectorFile.LoadFromStream(aStream: TStream);
begin
  Owner.MeshObjects.LoadFromStream(aStream);
end;

procedure TGLGLSMVectorFile.SaveToStream(aStream: TStream);
begin
  Owner.MeshObjects.SaveToStream(aStream);
end;

{%endregion%}

{%region=====[ TGLBaseMesh ]====================================================}

constructor TGLBaseMesh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if FMeshObjects = nil then
    FMeshObjects := TGLMeshObjectList.CreateOwned(Self);
  if FSkeleton = nil then
    FSkeleton := TGLSkeleton.CreateOwned(Self);
  FUseMeshMaterials := True;
  FAutoCentering := [];
  FAxisAlignedDimensionsCache.V[0] := -1;
  FBaryCenterOffsetChanged := True;
  FAutoScaling := TGLCoordinates.CreateInitialized(Self, XYZWHmgVector, csPoint);
end;

destructor TGLBaseMesh.Destroy;
begin
  FConnectivity.Free;
  DropMaterialLibraryCache;
  FSkeleton.Free;
  FMeshObjects.Free;
  FAutoScaling.Free;
  inherited Destroy;
end;

procedure TGLBaseMesh.Assign(Source: TPersistent);
begin
  if Source is TGLBaseMesh then
  begin
    FSkeleton.Clear;
    FNormalsOrientation := TGLBaseMesh(Source).FNormalsOrientation;
    FMaterialLibrary := TGLBaseMesh(Source).FMaterialLibrary;
    FLightmapLibrary := TGLBaseMesh(Source).FLightmapLibrary;
    FAxisAlignedDimensionsCache := TGLBaseMesh(Source).FAxisAlignedDimensionsCache;
    FBaryCenterOffset := TGLBaseMesh(Source).FBaryCenterOffset;
    FUseMeshMaterials := TGLBaseMesh(Source).FUseMeshMaterials;
    FOverlaySkeleton := TGLBaseMesh(Source).FOverlaySkeleton;
    FIgnoreMissingTextures := TGLBaseMesh(Source).FIgnoreMissingTextures;
    FAutoCentering := TGLBaseMesh(Source).FAutoCentering;
    FAutoScaling.Assign(TGLBaseMesh(Source).FAutoScaling);
    FSkeleton.Assign(TGLBaseMesh(Source).FSkeleton);
    FSkeleton.RootBones.PrepareGlobalMatrices;
    FMeshObjects.Assign(TGLBaseMesh(Source).FMeshObjects);
  end;
  inherited Assign(Source);
end;

procedure TGLBaseMesh.LoadFromFile(const filename: string);
var
  fs: TStream;
begin
  FLastLoadedFilename := '';
  if fileName <> '' then
  begin
    fs := CreateFileStream(fileName, fmOpenRead + fmShareDenyWrite);
    try
      LoadFromStream(fileName, fs);
      FLastLoadedFilename := filename;
    finally
      fs.Free;
    end;
  end;
end;

procedure TGLBaseMesh.LoadFromStream(const fileName: string; aStream: TStream);
var
  newVectorFile: TGLVectorFile;
  vectorFileClass: TGLVectorFileClass;
begin
  FLastLoadedFilename := '';
  if fileName <> '' then
  begin
    MeshObjects.Clear;
    Skeleton.Clear;
    vectorFileClass := GetVectorFileFormats.FindFromFileName(filename);
    newVectorFile := VectorFileClass.Create(Self);
    try
      newVectorFile.ResourceName := filename;
      PrepareVectorFile(newVectorFile);
      if Assigned(Scene) then
        Scene.BeginUpdate;
      try
        newVectorFile.LoadFromStream(aStream);
        FLastLoadedFilename := filename;
      finally
        if Assigned(Scene) then
          Scene.EndUpdate;
      end;
    finally
      newVectorFile.Free;
    end;
    PerformAutoScaling;
    PerformAutoCentering;
    PrepareMesh;
  end;
end;

procedure TGLBaseMesh.SaveToFile(const filename: string);
var
  fs: TStream;
begin
  if fileName <> '' then
  begin
    fs := CreateFileStream(fileName, fmCreate);
    try
      SaveToStream(fileName, fs);
    finally
      fs.Free;
    end;
  end;
end;

procedure TGLBaseMesh.SaveToStream(const fileName: string; aStream: TStream);
var
  newVectorFile: TGLVectorFile;
  vectorFileClass: TGLVectorFileClass;
begin
  if fileName <> '' then
  begin
    vectorFileClass := GetVectorFileFormats.FindFromFileName(filename);
    newVectorFile := VectorFileClass.Create(Self);
    try
      newVectorFile.ResourceName := filename;
      PrepareVectorFile(newVectorFile);
      newVectorFile.SaveToStream(aStream);
    finally
      newVectorFile.Free;
    end;
  end;
end;

procedure TGLBaseMesh.AddDataFromFile(const filename: string);
var
  fs: TStream;
begin
  if fileName <> '' then
  begin
    fs := CreateFileStream(fileName, fmOpenRead + fmShareDenyWrite);
    try
      AddDataFromStream(fileName, fs);
    finally
      fs.Free;
    end;
  end;
end;

procedure TGLBaseMesh.AddDataFromStream(const filename: string; aStream: TStream);
var
  newVectorFile: TGLVectorFile;
  vectorFileClass: TGLVectorFileClass;
begin
  if fileName <> '' then
  begin
    vectorFileClass := GetVectorFileFormats.FindFromFileName(filename);
    newVectorFile := VectorFileClass.Create(Self);
    newVectorFile.ResourceName := filename;
    PrepareVectorFile(newVectorFile);
    try
      if Assigned(Scene) then
        Scene.BeginUpdate;
      newVectorFile.LoadFromStream(aStream);
      if Assigned(Scene) then
        Scene.EndUpdate;
    finally
      NewVectorFile.Free;
    end;
    PrepareMesh;
  end;
end;

procedure TGLBaseMesh.GetExtents(out min, max: TAffineVector);
var
  i, k: integer;
  lMin, lMax: TAffineVector;
const
  cBigValue: single = 1e50;
  cSmallValue: single = -1e50;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for i := 0 to MeshObjects.Count - 1 do
  begin
    TGLMeshObject(MeshObjects[i]).GetExtents(lMin, lMax);
    for k := 0 to 2 do
    begin
      if lMin.V[k] < min.V[k] then
        min.V[k] := lMin.V[k];
      if lMax.V[k] > max.V[k] then
        max.V[k] := lMax.V[k];
    end;
  end;
end;

function TGLBaseMesh.GetBarycenter: TAffineVector;
var
  i, nb: integer;
begin
  Result := NullVector;
  nb := 0;
  for i := 0 to MeshObjects.Count - 1 do
    TGLMeshObject(MeshObjects[i]).ContributeToBarycenter(Result, nb);
  if nb > 0 then
    ScaleVector(Result, 1 / nb);
end;

function TGLBaseMesh.LastLoadedFilename: string;
begin
  Result := FLastLoadedFilename;
end;

procedure TGLBaseMesh.SetMaterialLibrary(const val: TGLMaterialLibrary);
begin
  if FMaterialLibrary <> val then
  begin
    if FMaterialLibraryCachesPrepared then
      DropMaterialLibraryCache;
    if Assigned(FMaterialLibrary) then
    begin
      DestroyHandle;
      FMaterialLibrary.RemoveFreeNotification(Self);
    end;
    FMaterialLibrary := val;
    if Assigned(FMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
    StructureChanged;
  end;
end;

procedure TGLBaseMesh.SetLightmapLibrary(const val: TGLMaterialLibrary);
begin
  if FLightmapLibrary <> val then
  begin
    if Assigned(FLightmapLibrary) then
    begin
      DestroyHandle;
      FLightmapLibrary.RemoveFreeNotification(Self);
    end;
    FLightmapLibrary := val;
    if Assigned(FLightmapLibrary) then
      FLightmapLibrary.FreeNotification(Self);
    StructureChanged;
  end;
end;

procedure TGLBaseMesh.SetNormalsOrientation(const val: TMeshNormalsOrientation);
begin
  if val <> FNormalsOrientation then
  begin
    FNormalsOrientation := val;
    StructureChanged;
  end;
end;

procedure TGLBaseMesh.SetOverlaySkeleton(const val: boolean);
begin
  if FOverlaySkeleton <> val then
  begin
    FOverlaySkeleton := val;
    NotifyChange(Self);
  end;
end;

procedure TGLBaseMesh.SetAutoScaling(const Value: TGLCoordinates);
begin
  FAutoScaling.SetPoint(Value.DirectX, Value.DirectY, Value.DirectZ);
end;

procedure TGLBaseMesh.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FMaterialLibrary then
      MaterialLibrary := nil
    else if AComponent = FLightmapLibrary then
      LightmapLibrary := nil;
  end;
  inherited;
end;

function TGLBaseMesh.AxisAlignedDimensionsUnscaled: TVector;
var
  dMin, dMax: TAffineVector;
begin
  if FAxisAlignedDimensionsCache.V[0] < 0 then
  begin
    MeshObjects.GetExtents(dMin, dMax);
    FAxisAlignedDimensionsCache.V[0] := (dMax.V[0] - dMin.V[0]) / 2;
    FAxisAlignedDimensionsCache.V[1] := (dMax.V[1] - dMin.V[1]) / 2;
    FAxisAlignedDimensionsCache.V[2] := (dMax.V[2] - dMin.V[2]) / 2;
    FAxisAlignedDimensionsCache.V[3] := 0;
  end;
  SetVector(Result, FAxisAlignedDimensionsCache);
end;

function TGLBaseMesh.BarycenterOffset: TVector;
var
  dMin, dMax: TAffineVector;
begin
  if FBaryCenterOffsetChanged then
  begin
    MeshObjects.GetExtents(dMin, dMax);

    FBaryCenterOffset.V[0] := (dMin.V[0] + dMax.V[0]) / 2;
    FBaryCenterOffset.V[1] := (dMin.V[1] + dMax.V[1]) / 2;
    FBaryCenterOffset.V[2] := (dMin.V[2] + dMax.V[2]) / 2;
    FBaryCenterOffset.V[3] := 0;
    FBaryCenterOffsetChanged := False;
  end;
  Result := FBaryCenterOffset;
end;

function TGLBaseMesh.BarycenterPosition: TVector;
begin
  Result := VectorAdd(Position.DirectVector, BarycenterOffset);
end;

function TGLBaseMesh.BarycenterAbsolutePosition: TVector;
begin
  Result := LocalToAbsolute(BarycenterPosition);
end;

procedure TGLBaseMesh.DestroyHandle;
begin
  if Assigned(FMaterialLibrary) then
    MaterialLibrary.DestroyHandles;
  if Assigned(FLightmapLibrary) then
    LightmapLibrary.DestroyHandles;
  inherited;
end;

procedure TGLBaseMesh.PrepareVectorFile(aFile: TGLVectorFile);
begin
  aFile.NormalsOrientation := NormalsOrientation;
end;

procedure TGLBaseMesh.PerformAutoCentering;
var
  delta, min, max: TAffineVector;
begin
  if macUseBarycenter in AutoCentering then
  begin
    delta := VectorNegate(GetBarycenter);
  end
  else
  begin
    GetExtents(min, max);
    if macCenterX in AutoCentering then
      delta.V[0] := -0.5 * (min.V[0] + max.V[0])
    else
      delta.V[0] := 0;
    if macCenterY in AutoCentering then
      delta.V[1] := -0.5 * (min.V[1] + max.V[1])
    else
      delta.V[1] := 0;
    if macCenterZ in AutoCentering then
      delta.V[2] := -0.5 * (min.V[2] + max.V[2])
    else
      delta.V[2] := 0;
  end;
  MeshObjects.Translate(delta);

  if macRestorePosition in AutoCentering then
    Position.Translate(VectorNegate(delta));
end;

procedure TGLBaseMesh.PerformAutoScaling;
var
  i: integer;
  vScal: TAffineFltVector;
begin
  if (FAutoScaling.DirectX <> 1) or (FAutoScaling.DirectY <> 1) or
    (FAutoScaling.DirectZ <> 1) then
  begin
    MakeVector(vScal, FAutoScaling.DirectX, FAutoScaling.DirectY,
      FAutoScaling.DirectZ);
    for i := 0 to MeshObjects.Count - 1 do
    begin
      MeshObjects[i].Vertices.Scale(vScal);
    end;
  end;
end;

procedure TGLBaseMesh.PrepareMesh;
begin
  StructureChanged;
end;

procedure TGLBaseMesh.PrepareMaterialLibraryCache;
begin
  if FMaterialLibraryCachesPrepared then
    DropMaterialLibraryCache;
  MeshObjects.PrepareMaterialLibraryCache(FMaterialLibrary);
  FMaterialLibraryCachesPrepared := True;
end;

procedure TGLBaseMesh.DropMaterialLibraryCache;
begin
  if FMaterialLibraryCachesPrepared then
  begin
    MeshObjects.DropMaterialLibraryCache;
    FMaterialLibraryCachesPrepared := False;
  end;
end;

procedure TGLBaseMesh.PrepareBuildList(var mrci: TGLRenderContextInfo);
begin
  MeshObjects.PrepareBuildList(mrci);
  if LightmapLibrary <> nil then
    LightmapLibrary.Materials.PrepareBuildList;
end;

procedure TGLBaseMesh.SetUseMeshMaterials(const val: boolean);
begin
  if val <> FUseMeshMaterials then
  begin
    FUseMeshMaterials := val;
    if FMaterialLibraryCachesPrepared and (not val) then
      DropMaterialLibraryCache;
    StructureChanged;
  end;
end;

procedure TGLBaseMesh.BuildList(var rci: TGLRenderContextInfo);
begin
  MeshObjects.BuildList(rci);
end;

procedure TGLBaseMesh.DoRender(var rci: TGLRenderContextInfo;
  renderSelf, renderChildren: boolean);
begin
  if Assigned(LightmapLibrary) then
    xgl.ForbidSecondTextureUnit;
  if renderSelf then
  begin
    // set winding
    case FNormalsOrientation of
      mnoDefault: ; // nothing
      mnoInvert: rci.GLStates.InvertGLFrontFace;
      else
        Assert(False);
    end;
    if not rci.ignoreMaterials then
    begin
      if UseMeshMaterials and Assigned(MaterialLibrary) then
      begin
        rci.materialLibrary := MaterialLibrary;
        if not FMaterialLibraryCachesPrepared then
          PrepareMaterialLibraryCache;
      end
      else
        rci.materialLibrary := nil;
      if Assigned(LightmapLibrary) then
        rci.lightmapLibrary := LightmapLibrary
      else
        rci.lightmapLibrary := nil;
      if rci.amalgamating or not (ListHandleAllocated or
        (osDirectDraw in ObjectStyle)) then
        PrepareBuildList(rci);
      Material.Apply(rci);
      repeat
        if (osDirectDraw in ObjectStyle) or rci.amalgamating or
          UseMeshMaterials then
          BuildList(rci)
        else
          rci.GLStates.CallList(GetHandle(rci));
      until not Material.UnApply(rci);
      rci.materialLibrary := nil;
    end
    else
    begin
      if (osDirectDraw in ObjectStyle) or rci.amalgamating then
        BuildList(rci)
      else
        rci.GLStates.CallList(GetHandle(rci));
    end;
    if FNormalsOrientation <> mnoDefault then
      rci.GLStates.InvertGLFrontFace;
  end;
  if Assigned(LightmapLibrary) then
    xgl.AllowSecondTextureUnit;
  if renderChildren and (Count > 0) then
    Self.RenderChildren(0, Count - 1, rci);
end;

procedure TGLBaseMesh.StructureChanged;
begin
  FAxisAlignedDimensionsCache.V[0] := -1;
  FBaryCenterOffsetChanged := True;
  DropMaterialLibraryCache;
  MeshObjects.Prepare;
  inherited;
end;

procedure TGLBaseMesh.StructureChangedNoPrepare;
begin
  inherited StructureChanged;
end;

function TGLBaseMesh.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): boolean;
var
  i: integer;
  tris: TAffineVectorList;
  locRayStart, locRayVector, iPoint, iNormal: TVector;
  d, minD: single;
begin
  // BEWARE! Utterly inefficient implementation!
  tris := MeshObjects.ExtractTriangles;
  try
    SetVector(locRayStart, AbsoluteToLocal(rayStart));
    SetVector(locRayVector, AbsoluteToLocal(rayVector));
    minD := -1;
    i := 0;
    while i < tris.Count do
    begin
      if RayCastTriangleIntersect(locRayStart, locRayVector,
        tris.List^[i], tris.List^[i + 1], tris.List^[i + 2], @iPoint,
        @iNormal) then
      begin
        d := VectorDistance2(locRayStart, iPoint);
        if (d < minD) or (minD < 0) then
        begin
          minD := d;
          if intersectPoint <> nil then
            intersectPoint^ := iPoint;
          if intersectNormal <> nil then
            intersectNormal^ := iNormal;
        end;
      end;
      Inc(i, 3);
    end;
  finally
    tris.Free;
  end;
  Result := (minD >= 0);
  if Result then
  begin
    if intersectPoint <> nil then
      SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
    if intersectNormal <> nil then
    begin
      SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      if NormalsOrientation = mnoInvert then
        NegateVector(intersectNormal^);
    end;
  end;
end;

function TGLBaseMesh.GenerateSilhouette(
  const silhouetteParameters: TGLSilhouetteParameters): TGLSilhouette;
var
  mc: TGLBaseMeshConnectivity;
  sil: TGLSilhouette;
begin
  sil := nil;
  if Assigned(FConnectivity) then
  begin
    mc := TGLBaseMeshConnectivity(FConnectivity);
    mc.CreateSilhouette(silhouetteParameters,
      sil,
      True);
  end
  else
  begin
    mc := TGLBaseMeshConnectivity.CreateFromMesh(Self);
    try
      mc.CreateSilhouette(silhouetteParameters,
        sil,
        True);
    finally
      mc.Free;
    end;
  end;
  Result := sil;
end;

procedure TGLBaseMesh.BuildSilhouetteConnectivityData;
var
  i, j: integer;
  mo: TGLMeshObject;
begin
  FreeAndNil(FConnectivity);
  // connectivity data works only on facegroups of TFGVertexIndexList class
  for i := 0 to MeshObjects.Count - 1 do
  begin
    mo := (MeshObjects[i] as TGLMeshObject);
    if mo.Mode <> momFacegroups then
      Exit;
    for j := 0 to mo.FaceGroups.Count - 1 do
      if not mo.FaceGroups[j].InheritsFrom(TFGVertexIndexList) then
        Exit;
  end;
  FConnectivity := TGLBaseMeshConnectivity.CreateFromMesh(Self);
end;

{%endregion%}

{%region=====[ TGLFreeForm ]====================================================}

constructor TGLFreeForm.Create(AOwner: TComponent);
begin
  inherited;
  //  ObjectStyle := [osDirectDraw];
  FUseMeshMaterials := True;
end;

destructor TGLFreeForm.Destroy;
begin
  FOctree.Free;
  inherited Destroy;
end;

function TGLFreeForm.GetOctree: TOctree;
begin
  //   if not Assigned(FOctree) then     //If auto-created, can never use "if Assigned(GLFreeform1.Octree)"
  //     FOctree:=TOctree.Create;        //moved this code to BuildOctree
  Result := FOctree;
end;

procedure TGLFreeForm.BuildOctree(TreeDepth: integer = 3);
var
  emin, emax: TAffineVector;
  tl: TAffineVectorList;
begin
  if not Assigned(FOctree) then //moved here from GetOctree
    FOctree := TOctree.Create;

  GetExtents(emin, emax);
  tl := MeshObjects.ExtractTriangles;
  try
    with Octree do
    begin
      DisposeTree;
      InitializeTree(emin, emax, tl, TreeDepth);
    end;
  finally
    tl.Free;
  end;
end;

function TGLFreeForm.OctreeRayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): boolean;
var
  locRayStart, locRayVector: TVector;
begin
  Assert(Assigned(FOctree),
    'Octree must have been prepared and setup before use.');
  SetVector(locRayStart, AbsoluteToLocal(rayStart));
  SetVector(locRayVector, AbsoluteToLocal(rayVector));
  Result := Octree.RayCastIntersect(locRayStart, locRayVector,
    intersectPoint, intersectNormal);
  if Result then
  begin
    if intersectPoint <> nil then
      SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
    if intersectNormal <> nil then
    begin
      SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      if NormalsOrientation = mnoInvert then
        NegateVector(intersectNormal^);
    end;
  end;
end;

function TGLFreeForm.OctreePointInMesh(const Point: TVector): boolean;
const
  cPointRadiusStep = 10000;
var
  rayStart, rayVector, hitPoint, hitNormal: TVector;
  BRad: double;
  HitCount: integer;
  hitDot: double;
begin
  Assert(Assigned(FOctree),
    'Octree must have been prepared and setup before use.');

  Result := False;

  // Makes calculations sligthly faster by ignoring cases that are guaranteed
  // to be outside the object
  if not PointInObject(Point) then
    exit;

  BRad := BoundingSphereRadius;

  // This could be a fixed vector, but a fixed vector could have a systemic
  // bug on an non-closed mesh, making it fail constantly for one or several
  // faces.
  rayVector := VectorMake(2 * random - 1, 2 * random - 1, 2 * random - 1);
  rayStart := VectorAdd(VectorScale(rayVector, -BRad), Point);

  HitCount := 0;

  while OctreeRayCastIntersect(rayStart, rayVector, @hitPoint, @hitNormal) do
  begin
    // Are we past our taget?
    if VectorDotProduct(rayVector, VectorSubtract(Point, hitPoint)) < 0 then
    begin
      Result := HitCount > 0;
      exit;
    end;

    hitDot := VectorDotProduct(hitNormal, rayVector);
    if hitDot < 0 then
      Inc(HitCount)
    else if hitDot > 0 then
      Dec(HitCount);

    // ditDot = 0 is a tricky special case where the ray is just grazing the
    // side of a face - this case means that it doesn't necessarily actually
    // enter the mesh - but it _could_ enter the mesh. If this situation occurs,
    // we should restart the run using a new rayVector - but this implementation
    // currently doesn't.

    // Restart the ray slightly beyond the point it hit the previous face. Note
    // that this step introduces a possible issue with faces that are very close
    rayStart := VectorAdd(hitPoint, VectorScale(rayVector, BRad /
      cPointRadiusStep));
  end;
end;

function TGLFreeForm.OctreeSphereSweepIntersect(const rayStart, rayVector: TVector;
  const velocity, radius: single; intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): boolean;
var
  locRayStart, locRayVector: TVector;
begin
  Assert(Assigned(FOctree),
    'Octree must have been prepared and setup before use.');
  SetVector(locRayStart, AbsoluteToLocal(rayStart));
  SetVector(locRayVector, AbsoluteToLocal(rayVector));
  Result := Octree.SphereSweepIntersect(locRayStart, locRayVector,
    velocity, radius, intersectPoint, intersectNormal);
  if Result then
  begin
    if intersectPoint <> nil then
      SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
    if intersectNormal <> nil then
    begin
      SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      if NormalsOrientation = mnoInvert then
        NegateVector(intersectNormal^);
    end;
  end;
end;

function TGLFreeForm.OctreeTriangleIntersect(const v1, v2, v3: TAffineVector): boolean;
var
  t1, t2, t3: TAffineVector;
begin
  Assert(Assigned(FOctree),
    'Octree must have been prepared and setup before use.');
  SetVector(t1, AbsoluteToLocal(v1));
  SetVector(t2, AbsoluteToLocal(v2));
  SetVector(t3, AbsoluteToLocal(v3));

  Result := Octree.TriangleIntersect(t1, t2, t3);
end;

function TGLFreeForm.OctreeAABBIntersect(const AABB: TAABB;
  objMatrix, invObjMatrix: TMatrix; triangles: TAffineVectorList = nil): boolean;
var
  m1to2, m2to1: TMatrix;
begin
  Assert(Assigned(FOctree),
    'Octree must have been prepared and setup before use.');

  //get matrixes needed
  //object to self
  MatrixMultiply(objMatrix, InvAbsoluteMatrix, m1to2);
  //self to object
  MatrixMultiply(AbsoluteMatrix, invObjMatrix, m2to1);

  Result := octree.AABBIntersect(aabb, m1to2, m2to1, triangles);
end;

{%endregion%}

{%region=====[ TActorAnimation ]================================================}

constructor TActorAnimation.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TActorAnimation.Destroy;
begin
  with (Collection as TActorAnimations).FOwner do
    if FTargetSmoothAnimation = Self then
      FTargetSmoothAnimation := nil;
  inherited Destroy;
end;

procedure TActorAnimation.Assign(Source: TPersistent);
begin
  if Source is TActorAnimation then
  begin
    FName := TActorAnimation(Source).FName;
    FStartFrame := TActorAnimation(Source).FStartFrame;
    FEndFrame := TActorAnimation(Source).FEndFrame;
    FReference := TActorAnimation(Source).FReference;
  end
  else
    inherited;
end;

function TActorAnimation.GetDisplayName: string;
begin
  Result := Format('%d - %s [%d - %d]', [Index, Name, StartFrame, EndFrame]);
end;

function TActorAnimation.FrameCount: integer;
begin
  case Reference of
    aarMorph:
      Result :=
        TActorAnimations(Collection).FOwner.MeshObjects.MorphTargetCount;
    aarSkeleton:
      Result := TActorAnimations(Collection).FOwner.Skeleton.Frames.Count;
    else
      Result := 0;
      Assert(False);
  end;
end;

procedure TActorAnimation.SetStartFrame(const val: integer);
var
  m: integer;
begin
  if val < 0 then
    FStartFrame := 0
  else
  begin
    m := FrameCount;
    if val >= m then
      FStartFrame := m - 1
    else
      FStartFrame := val;
  end;
  if FStartFrame > FEndFrame then
    FEndFrame := FStartFrame;
end;

procedure TActorAnimation.SetEndFrame(const val: integer);
var
  m: integer;
begin
  if val < 0 then
    FEndFrame := 0
  else
  begin
    m := FrameCount;
    if val >= m then
      FEndFrame := m - 1
    else
      FEndFrame := val;
  end;
  if FStartFrame > FEndFrame then
    FStartFrame := FEndFrame;
end;

procedure TActorAnimation.SetReference(val: TActorAnimationReference);
begin
  if val <> FReference then
  begin
    FReference := val;
    StartFrame := StartFrame;
    EndFrame := EndFrame;
  end;
end;

procedure TActorAnimation.SetAsString(const val: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.CommaText := val;
    Assert(sl.Count >= 3);
    FName := sl[0];
    FStartFrame := StrToInt(sl[1]);
    FEndFrame := StrToInt(sl[2]);
    if sl.Count = 4 then
    begin
      if LowerCase(sl[3]) = 'morph' then
        Reference := aarMorph
      else if LowerCase(sl[3]) = 'skeleton' then
        Reference := aarSkeleton
      else
        Assert(False);
    end
    else
      Reference := aarMorph;
  finally
    sl.Free;
  end;
end;

function TActorAnimation.GetAsString: string;
const
  cAARToString: array[aarMorph..aarSkeleton] of string = ('morph', 'skeleton');
begin
  Result := Format('"%s",%d,%d,%s', [FName, FStartFrame, FEndFrame,
    cAARToString[Reference]]);
end;

function TActorAnimation.OwnerActor: TGLActor;
begin
  Result := ((Collection as TActorAnimations).GetOwner as TGLActor);
end;

procedure TActorAnimation.MakeSkeletalTranslationStatic;
begin
  OwnerActor.Skeleton.MakeSkeletalTranslationStatic(StartFrame, EndFrame);
end;

procedure TActorAnimation.MakeSkeletalRotationDelta;
begin
  OwnerActor.Skeleton.MakeSkeletalRotationDelta(StartFrame, EndFrame);
end;

{%endregion%}

{%region=====[ TActorAnimations ]===============================================}

constructor TActorAnimations.Create(AOwner: TGLActor);
begin
  FOwner := AOwner;
  inherited Create(TActorAnimation);
end;

function TActorAnimations.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TActorAnimations.SetItems(index: integer; const val: TActorAnimation);
begin
  inherited Items[index] := val;
end;

function TActorAnimations.GetItems(index: integer): TActorAnimation;
begin
  Result := TActorAnimation(inherited Items[index]);
end;

function TActorAnimations.Last: TActorAnimation;
begin
  if Count > 0 then
    Result := TActorAnimation(inherited Items[Count - 1])
  else
    Result := nil;
end;

function TActorAnimations.Add: TActorAnimation;
begin
  Result := (inherited Add) as TActorAnimation;
end;

function TActorAnimations.FindItemID(ID: integer): TActorAnimation;
begin
  Result := (inherited FindItemID(ID)) as TActorAnimation;
end;

function TActorAnimations.FindName(const aName: string): TActorAnimation;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(Items[i].Name, aName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TActorAnimations.FindFrame(aFrame: integer;
  aReference: TActorAnimationReference): TActorAnimation;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    with Items[i] do
      if (StartFrame <= aFrame) and (EndFrame >= aFrame) and
        (Reference = aReference) then
      begin
        Result := Items[i];
        Break;
      end;
end;

procedure TActorAnimations.SetToStrings(aStrings: TStrings);

var
  i: integer;
begin
  with aStrings do
  begin
    BeginUpdate;
    Clear;
    for i := 0 to Self.Count - 1 do
      Add(Self.Items[i].Name);
    EndUpdate;
  end;
end;

procedure TActorAnimations.SaveToStream(aStream: TStream);
var
  i: integer;
begin
  WriteCRLFString(aStream, cAAFHeader);
  WriteCRLFString(aStream, ansistring(IntToStr(Count)));
  for i := 0 to Count - 1 do
    WriteCRLFString(aStream, ansistring(Items[i].AsString));
end;

procedure TActorAnimations.LoadFromStream(aStream: TStream);
var
  i, n: integer;
begin
  Clear;
  if ReadCRLFString(aStream) <> cAAFHeader then
    Assert(False);
  n := StrToInt(string(ReadCRLFString(aStream)));
  for i := 0 to n - 1 do
    Add.AsString := string(ReadCRLFString(aStream));
end;

procedure TActorAnimations.SaveToFile(const fileName: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TActorAnimations.LoadFromFile(const fileName: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmOpenRead + fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

{%endregion%}

{%region=====[ TGLBaseAnimationControler =======================================}

constructor TGLBaseAnimationControler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
end;

destructor TGLBaseAnimationControler.Destroy;
begin
  SetActor(nil);
  inherited Destroy;
end;

procedure TGLBaseAnimationControler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FActor) and (Operation = opRemove) then
    SetActor(nil);
  inherited;
end;

procedure TGLBaseAnimationControler.DoChange;
begin
  if Assigned(FActor) then
    FActor.NotifyChange(Self);
end;

procedure TGLBaseAnimationControler.SetEnabled(const val: boolean);
begin
  if val <> FEnabled then
  begin
    FEnabled := val;
    if Assigned(FActor) then
      DoChange;
  end;
end;

procedure TGLBaseAnimationControler.SetActor(const val: TGLActor);
begin
  if FActor <> val then
  begin
    if Assigned(FActor) then
      FActor.UnRegisterControler(Self);
    FActor := val;
    if Assigned(FActor) then
    begin
      FActor.RegisterControler(Self);
      DoChange;
    end;
  end;
end;

function TGLBaseAnimationControler.Apply(var lerpInfo: TBlendedLerpInfo): boolean;
begin
  // virtual
  Result := False;
end;

{%endregion%}


{%region=====[ TGLAnimationControler ]==========================================}

procedure TGLAnimationControler.DoChange;
begin
  if AnimationName <> '' then
    inherited;
end;

procedure TGLAnimationControler.SetAnimationName(const val: TActorAnimationName);
begin
  if FAnimationName <> val then
  begin
    FAnimationName := val;
    DoChange;
  end;
end;

procedure TGLAnimationControler.SetRatio(const val: single);
begin
  if FRatio <> val then
  begin
    FRatio := ClampValue(val, 0, 1);
    DoChange;
  end;
end;

function TGLAnimationControler.Apply(var lerpInfo: TBlendedLerpInfo): boolean;
var
  anim: TActorAnimation;
  baseDelta: integer;
begin
  if not Enabled then
  begin
    Result := False;
    Exit;
  end;

  anim := Actor.Animations.FindName(AnimationName);
  Result := (anim <> nil);
  if not Result then
    Exit;

  with lerpInfo do
  begin
    if Ratio = 0 then
    begin
      frameIndex1 := anim.StartFrame;
      frameIndex2 := frameIndex1;
      lerpFactor := 0;
    end
    else if Ratio = 1 then
    begin
      frameIndex1 := anim.EndFrame;
      frameIndex2 := frameIndex1;
      lerpFactor := 0;
    end
    else
    begin
      baseDelta := anim.EndFrame - anim.StartFrame;
      lerpFactor := anim.StartFrame + baseDelta * Ratio;
      frameIndex1 := GLVectorGeometry.Trunc(lerpFactor);
      frameIndex2 := frameIndex1 + 1;
      lerpFactor := GLVectorGeometry.Frac(lerpFactor);
    end;
    weight := 1;
    externalRotations := nil;
    externalQuaternions := nil;
  end;
end;

{%endregion%}


{%region=====[ TGLActor ]=======================================================}

constructor TGLActor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FFrameInterpolation := afpLinear;
  FAnimationMode := aamNone;
  FInterval := 100; // 10 animation frames per second
  FAnimations := TActorAnimations.Create(Self);
  FControlers := nil; // created on request
  FOptions := cDefaultGLActorOptions;
end;

destructor TGLActor.Destroy;
begin
  inherited Destroy;
  FControlers.Free;
  FAnimations.Free;
end;

procedure TGLActor.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TGLActor then
  begin
    FAnimations.Assign(TGLActor(Source).FAnimations);
    FAnimationMode := TGLActor(Source).FAnimationMode;
    Synchronize(TGLActor(Source));
  end;
end;

procedure TGLActor.RegisterControler(aControler: TGLBaseAnimationControler);
begin
  if not Assigned(FControlers) then
    FControlers := TList.Create;
  FControlers.Add(aControler);
  FreeNotification(aControler);
end;

procedure TGLActor.UnRegisterControler(aControler: TGLBaseAnimationControler);
begin
  Assert(Assigned(FControlers));
  FControlers.Remove(aControler);
  RemoveFreeNotification(aControler);
  if FControlers.Count = 0 then
    FreeAndNil(FControlers);
end;

procedure TGLActor.SetCurrentFrame(val: integer);
begin
  if val <> CurrentFrame then
  begin
    if val > FrameCount - 1 then
      FCurrentFrame := FrameCount - 1
    else if val < 0 then
      FCurrentFrame := 0
    else
      FCurrentFrame := val;
    FCurrentFrameDelta := 0;
    case AnimationMode of
      aamPlayOnce: if (CurrentFrame = EndFrame) and
          (FTargetSmoothAnimation = nil) then
          FAnimationMode := aamNone;
      aamBounceForward: if CurrentFrame = EndFrame then
          FAnimationMode := aamBounceBackward;
      aamBounceBackward: if CurrentFrame = StartFrame then
          FAnimationMode := aamBounceForward;
    end;
    StructureChanged;
    if Assigned(FOnFrameChanged) then
      FOnFrameChanged(Self);
  end;
end;

procedure TGLActor.SetCurrentFrameDirect(const Value: integer);
begin
  FCurrentFrame := Value;
end;

procedure TGLActor.SetStartFrame(val: integer);
begin
  if (val >= 0) and (val < FrameCount) and (val <> StartFrame) then
    FStartFrame := val;
  if EndFrame < StartFrame then
    FEndFrame := FStartFrame;
  if CurrentFrame < StartFrame then
    CurrentFrame := FStartFrame;
end;

procedure TGLActor.SetEndFrame(val: integer);
begin
  if (val >= 0) and (val < FrameCount) and (val <> EndFrame) then
    FEndFrame := val;
  if CurrentFrame > EndFrame then
    CurrentFrame := FEndFrame;
end;

procedure TGLActor.SetReference(val: TActorAnimationReference);
begin
  if val <> Reference then
  begin
    FReference := val;
    StartFrame := StartFrame;
    EndFrame := EndFrame;
    CurrentFrame := CurrentFrame;
    StructureChanged;
  end;
end;

procedure TGLActor.SetAnimations(const val: TActorAnimations);
begin
  FAnimations.Assign(val);
end;

function TGLActor.StoreAnimations: boolean;
begin
  Result := (FAnimations.Count > 0);
end;

procedure TGLActor.SetOptions(const val: TGLActorOptions);
begin
  if val <> FOptions then
  begin
    FOptions := val;
    StructureChanged;
  end;
end;

function TGLActor.NextFrameIndex: integer;
begin
  case AnimationMode of
    aamLoop, aamBounceForward:
    begin
      if FTargetSmoothAnimation <> nil then
        Result := FTargetSmoothAnimation.StartFrame
      else
      begin
        Result := CurrentFrame + 1;
        if Result > EndFrame then
        begin
          Result := StartFrame + (Result - EndFrame - 1);
          if Result > EndFrame then
            Result := EndFrame;
        end;
      end;
    end;
    aamNone, aamPlayOnce:
    begin
      if FTargetSmoothAnimation <> nil then
        Result := FTargetSmoothAnimation.StartFrame
      else
      begin
        Result := CurrentFrame + 1;
        if Result > EndFrame then
          Result := EndFrame;
      end;
    end;
    aamBounceBackward, aamLoopBackward:
    begin
      if FTargetSmoothAnimation <> nil then
        Result := FTargetSmoothAnimation.StartFrame
      else
      begin
        Result := CurrentFrame - 1;
        if Result < StartFrame then
        begin
          Result := EndFrame - (StartFrame - Result - 1);
          if Result < StartFrame then
            Result := StartFrame;
        end;
      end;
    end;
    aamExternal: Result := CurrentFrame; // Do nothing
    else
      Result := CurrentFrame;
      Assert(False);
  end;
end;

procedure TGLActor.NextFrame(nbSteps: integer = 1);
var
  n: integer;
begin
  n := nbSteps;
  while n > 0 do
  begin
    CurrentFrame := NextFrameIndex;
    Dec(n);
    if Assigned(FOnEndFrameReached) and (CurrentFrame = EndFrame) then
      FOnEndFrameReached(Self);
    if Assigned(FOnStartFrameReached) and (CurrentFrame = StartFrame) then
      FOnStartFrameReached(Self);
  end;
end;

procedure TGLActor.PrevFrame(nbSteps: integer = 1);
var
  Value: integer;
begin
  Value := FCurrentFrame - nbSteps;
  if Value < FStartFrame then
  begin
    Value := FEndFrame - (FStartFrame - Value);
    if Value < FStartFrame then
      Value := FStartFrame;
  end;
  CurrentFrame := Value;
end;

procedure TGLActor.DoAnimate();
var
  i, k: integer;
  nextFrameIdx: integer;
  lerpInfos: array of TBlendedLerpInfo;
begin
  nextFrameIdx := NextFrameIndex;
  case Reference of
    aarMorph: if nextFrameIdx >= 0 then
      begin
        case FrameInterpolation of
          afpLinear:
            MeshObjects.Lerp(CurrentFrame, nextFrameIdx, CurrentFrameDelta)
          else
            MeshObjects.MorphTo(CurrentFrame);
        end;
      end;
    aarSkeleton: if Skeleton.Frames.Count > 0 then
      begin
        if Assigned(FControlers) and (AnimationMode <> aamExternal) then
        begin
          // Blended Skeletal Lerping
          SetLength(lerpInfos, FControlers.Count + 1);
          if nextFrameIdx >= 0 then
          begin
            case FrameInterpolation of
              afpLinear: with lerpInfos[0] do
                begin
                  frameIndex1 := CurrentFrame;
                  frameIndex2 := nextFrameIdx;
                  lerpFactor := CurrentFrameDelta;
                  weight := 1;
                end;
              else
                with lerpInfos[0] do
                begin
                  frameIndex1 := CurrentFrame;
                  frameIndex2 := CurrentFrame;
                  lerpFactor := 0;
                  weight := 1;
                end;
            end;
          end
          else
          begin
            with lerpInfos[0] do
            begin
              frameIndex1 := CurrentFrame;
              frameIndex2 := CurrentFrame;
              lerpFactor := 0;
              weight := 1;
            end;
          end;
          k := 1;
          for i := 0 to FControlers.Count - 1 do
            if TGLBaseAnimationControler(FControlers[i]).Apply(lerpInfos[k])
            then
              Inc(k);
          SetLength(lerpInfos, k);
          Skeleton.BlendedLerps(lerpInfos);
        end
        else if (nextFrameIdx >= 0) and (AnimationMode <> aamExternal) then
        begin
          // Single Skeletal Lerp
          case FrameInterpolation of
            afpLinear:
              Skeleton.Lerp(CurrentFrame, nextFrameIdx, CurrentFrameDelta);
            else
              Skeleton.SetCurrentFrame(Skeleton.Frames[CurrentFrame]);
          end;
        end;
        Skeleton.MorphMesh(aoSkeletonNormalizeNormals in Options);
      end;
    aarNone: ; // do nothing
  end;
end;

procedure TGLActor.BuildList(var rci: TGLRenderContextInfo);
begin
  DoAnimate;
  inherited;
  if OverlaySkeleton then
  begin
    rci.GLStates.Disable(stDepthTest);
    Skeleton.RootBones.BuildList(rci);
  end;
end;

procedure TGLActor.PrepareMesh;
begin
  FStartFrame := 0;
  FEndFrame := FrameCount - 1;
  FCurrentFrame := 0;
  if Assigned(FOnFrameChanged) then
    FOnFrameChanged(Self);
  inherited;
end;

procedure TGLActor.PrepareBuildList(var mrci: TGLRenderContextInfo);
begin
  // no preparation needed for actors, they don't use buildlists
end;

function TGLActor.FrameCount: integer;
begin
  case Reference of
    aarMorph:
      Result := MeshObjects.MorphTargetCount;
    aarSkeleton:
      Result := Skeleton.Frames.Count;
    aarNone:
      Result := 0;
    else
      Result := 0;
      Assert(False);
  end;
end;

procedure TGLActor.DoProgress(const progressTime: TProgressTimes);
var
  fDelta: single;
begin
  inherited;
  if (AnimationMode <> aamNone) and (Interval > 0) then
  begin
    if (StartFrame <> EndFrame) and (FrameCount > 1) then
    begin
      FCurrentFrameDelta := FCurrentFrameDelta +
        (progressTime.deltaTime * 1000) / FInterval;
      if FCurrentFrameDelta > 1 then
      begin
        if Assigned(FTargetSmoothAnimation) then
        begin
          SwitchToAnimation(FTargetSmoothAnimation);
          FTargetSmoothAnimation := nil;
        end;
        // we need to step on
        fDelta := Frac(FCurrentFrameDelta);
        NextFrame(Trunc(FCurrentFrameDelta));
        FCurrentFrameDelta := fDelta;
        StructureChanged;
      end
      else if FrameInterpolation <> afpNone then
        StructureChanged;
    end;
  end;
end;

procedure TGLActor.LoadFromStream(const fileName: string; aStream: TStream);
begin
  if fileName <> '' then
  begin
    Animations.Clear;
    inherited LoadFromStream(fileName, aStream);
  end;
end;

procedure TGLActor.SwitchToAnimation(const animationName: string;
  smooth: boolean = False);
begin
  SwitchToAnimation(Animations.FindName(animationName), smooth);
end;

procedure TGLActor.SwitchToAnimation(animationIndex: integer; smooth: boolean = False);
begin
  if (animationIndex >= 0) and (animationIndex < Animations.Count) then
    SwitchToAnimation(Animations[animationIndex], smooth);
end;

procedure TGLActor.SwitchToAnimation(anAnimation: TActorAnimation;
  smooth: boolean = False);
begin
  if Assigned(anAnimation) then
  begin
    if smooth then
    begin
      FTargetSmoothAnimation := anAnimation;
      FCurrentFrameDelta := 0;
    end
    else
    begin
      Reference := anAnimation.Reference;
      StartFrame := anAnimation.StartFrame;
      EndFrame := anAnimation.EndFrame;
      CurrentFrame := StartFrame;
    end;
  end;
end;

function TGLActor.CurrentAnimation: string;
var
  aa: TActorAnimation;
begin
  aa := Animations.FindFrame(CurrentFrame, Reference);
  if Assigned(aa) then
    Result := aa.Name
  else
    Result := '';
end;

procedure TGLActor.Synchronize(referenceActor: TGLActor);
begin
  if Assigned(referenceActor) then
  begin
    if referenceActor.StartFrame < FrameCount then
      FStartFrame := referenceActor.StartFrame;
    if referenceActor.EndFrame < FrameCount then
      FEndFrame := referenceActor.EndFrame;
    FReference := referenceActor.Reference;
    if referenceActor.CurrentFrame < FrameCount then
      FCurrentFrame := referenceActor.CurrentFrame;
    FCurrentFrameDelta := referenceActor.CurrentFrameDelta;
    FAnimationMode := referenceActor.AnimationMode;
    FFrameInterpolation := referenceActor.FrameInterpolation;
    if referenceActor.FTargetSmoothAnimation <> nil then
      FTargetSmoothAnimation :=
        Animations.FindName(referenceActor.FTargetSmoothAnimation.Name)
    else
      FTargetSmoothAnimation := nil;
    if (Skeleton.Frames.Count > 0) and (referenceActor.Skeleton.Frames.Count >
      0) then
      Skeleton.Synchronize(referenceActor.Skeleton);
  end;
end;

function TGLActor.isSwitchingAnimation: boolean;
begin
  Result := FTargetSmoothAnimation <> nil;
end;

{%endregion%}

initialization

  RegisterVectorFileFormat('glsm', 'GLScene Mesh', TGLGLSMVectorFile);

  RegisterClasses([TGLFreeForm, TGLActor, TGLSkeleton, TGLSkeletonFrame,
    TGLSkeletonBone, TGLSkeletonMeshObject, TGLMeshObject,
    TGLSkeletonFrameList, TMeshMorphTarget, TMorphableMeshObject,
    TGLFaceGroup, TFGVertexIndexList, TFGVertexNormalTexIndexList,
    TGLAnimationControler, TFGIndexTexCoordList, TGLSkeletonCollider,
    TGLSkeletonColliderList]);

finalization

  FreeAndNil(vVectorFileFormats);

end.
