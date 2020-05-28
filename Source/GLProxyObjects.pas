//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Implements specific proxying classes.

  History :  
       16/03/11 - Yar - Fixes after emergence of GLMaterialEx
       23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
       25/12/09 - DaStr - Bugfixed TGLActorProxy.RayCastIntersect()
                               in aarMorph mode (thanks Vovik)
       22/12/09 - DaStr - Added TGLActorProxy.AnimationMode (thanks Vovik)
                             Removed TGLActorProxy.Interval (was not used)
       18/06/08 - mrqzzz - Don't raise error when setting animation to an
                               ActorProxy and no MasterObject is defined
       15/03/08 - DaStr - Fixup after previous update: removed all hints and
                              warnings, TGLActorProxy now has two versions of
                              RayCastIntersect()
       06/02/08 - mrqzzz - Added a "RayCastIntersect" overload for Actorproxy
       07/11/07 - mrqzzz - Added "OnBeforeRender" event to Actorproxy
                              allowing to apply extra transformations (f.ex: bone rotations)
                              to the referenced Actor in order to have the proxy render these changes.
       07/11/07 - mrqzzz - Added "StoredBoneNames" property
       07/11/07 - mrqzzz - Added "BoneMatrix(Boneidndex|boneName)" function and
                               StoreBonesMatrix property for TGLActorProxy
                              (To read each ActorProxy's individual Bone matrices,
                              f.ex to align a weapon in it's hand)
       06/11/07 - mrqzzz - Added MaterialLibrary and LibMaterialName for TGLActorProxy
                              (allows different materials on proxy actors sharing same master)
       06/11/07 - mrqzzz - Added public readonly properties for TGLActorProxy
                              (CurrentFrame,StartFrame,Endframe,etc..)
       05/10/07 - DaStr - Bugfixed TGLMaterialProxy.DoRender
                              (Bugtracker ID = 1808666)
       04/09/07 - DaStr - Added TGLMaterialProxy
                             Cleaned up this unit a bit
       10/05/07 - DaStr - Bugfixed TGLColorProxy.DoRender
                              (thanks Paul Robello) (Bugtracker ID = 1716692)
       28/03/07 - DaStr - Renamed parameters in some methods
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678658)
       25/02/07 - Made TGLActorProxy.SetAnimation a bit safer
       20/02/07 - DaStr - Redeclared MasterObject of TGLColorProxy and TGLFreeFormProxy
                             Added TGLActorProxy (based on a demo published
                             on the newsgroup by don't know who...)
       18/12/03 - Dave - Dropped "Object" from "ProxyObject" class names
       17/12/03 - Dave - Changed class check in Octree code to Assert
       17/12/03 - Dave+Dan - Added OctreeSphereSweep
       06/12/03 - EG - Creation from GLScene.pas split
    
}
unit GLProxyObjects;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,
  GLScene,  GLVectorGeometry,  GLTexture,  GLVectorFileObjects,
  GLStrings,  GLRenderContextInfo,  GLBaseClasses, GLMaterial,
  OpenGLTokens,  GLContext,  GLVectorTypes;

type
  EGLProxyException = class(Exception);

  // TGLColorProxy
  //
  { A proxy object with its own color.
     This proxy object can have a unique color. Note that multi-material
     objects (Freeforms linked to a material library f.i.) won't honour
     the color. }
  TGLColorProxy = class(TGLProxyObject)
  private
     
    FFrontColor: TGLFaceProperties;
    function GetMasterMaterialObject: TGLCustomSceneObject;
    procedure SetMasterMaterialObject(const Value: TGLCustomSceneObject);
    procedure SetFrontColor(AValue: TGLFaceProperties);
  public
     
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
  published
     
    property FrontColor: TGLFaceProperties read FFrontColor write
      SetFrontColor;
    // Redeclare as TGLCustomSceneObject.
    property MasterObject: TGLCustomSceneObject read GetMasterMaterialObject
      write SetMasterMaterialObject;
  end;

  // TGLMaterialProxy
  //
  { A proxy object with its own material.
     This proxy object can take a mesh from one master and a materia from
     a material library. }
  TGLMaterialProxy = class(TGLProxyObject, IGLMaterialLibrarySupported)
  private
     
    FTempLibMaterialName: string;
    FMasterLibMaterial: TGLLibMaterial;
    FMaterialLibrary: TGLMaterialLibrary;
    procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
    function GetMasterLibMaterialName: TGLLibMaterialName;
    procedure SetMasterLibMaterialName(const Value: TGLLibMaterialName);
    function GetMasterMaterialObject: TGLCustomSceneObject;
    procedure SetMasterMaterialObject(const Value: TGLCustomSceneObject);
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
  public
     
    constructor Create(AOwner: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    destructor Destroy; override;

    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    { Specifies the Material, that current master object will use.
       Provides a faster way to access FMasterLibMaterial, compared to
       MasterLibMaterialName }
    property MasterLibMaterial: TGLLibMaterial read FMasterLibMaterial write
      FMasterLibMaterial stored False;
  published
     
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary write
      SetMaterialLibrary;
    { Specifies the Material, that current master object will use. }
    property MasterLibMaterialName: TGLLibMaterialName read
      GetMasterLibMaterialName write SetMasterLibMaterialName;
    { Redeclare as TGLCustomSceneObject. }
    property MasterObject: TGLCustomSceneObject read GetMasterMaterialObject
      write SetMasterMaterialObject;
  end;

  // TGLFreeFormProxy
  //
  { A proxy object specialized for FreeForms. }
  TGLFreeFormProxy = class(TGLProxyObject)
  private
    function GetMasterFreeFormObject: TGLFreeForm;
    procedure SetMasterFreeFormObject(const Value: TGLFreeForm);
  protected
     

  public
     

    { If the MasterObject is a FreeForm, you can raycast against the Octree,
       which is alot faster.  You must build the octree before using. }
    function OctreeRayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean;
    { WARNING: This function is not yet 100% reliable with scale+rotation. }
    function OctreeSphereSweepIntersect(const rayStart, rayVector: TVector;
      const velocity, radius, modelscale: Single;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean;
  published
     
   // Redeclare as TGLFreeForm.
    property MasterObject: TGLFreeForm read GetMasterFreeFormObject write
      SetMasterFreeFormObject;
  end;

  // TBoneMatrixObj
  //
  { An object containing the bone matrix for TGLActorProxy. }
  TBoneMatrixObj = class
  public
    Matrix: TMatrix;
    BoneName: string;
    BoneIndex: integer;
  end;

  // pamLoop mode was too difficalt to implement, so it was discarded ...for now.
  // pamPlayOnce only works if Actor.AnimationMode <> aamNone.
  TGLActorProxyAnimationMode = (pamInherited, pamNone, pamPlayOnce);

  // TGLActorProxy
  //
  { A proxy object specialized for Actors. }
  TGLActorProxy = class(TGLProxyObject, IGLMaterialLibrarySupported)
  private
     
    FCurrentFrame: Integer;
    FStartFrame: Integer;
    FEndFrame: Integer;
    FLastFrame: Integer;
    FCurrentFrameDelta: Single;
    FCurrentTime: TProgressTimes;
    FAnimation: TActorAnimationName;

    FTempLibMaterialName: string;
    FMasterLibMaterial: TGLLibMaterial;
    FMaterialLibrary: TGLMaterialLibrary;

    FBonesMatrices: TStringList;
    FStoreBonesMatrix: boolean;
    FStoredBoneNames: TStrings;
    FOnBeforeRender: TGLProgressEvent;
    FAnimationMode: TGLActorProxyAnimationMode;

    procedure SetAnimation(const Value: TActorAnimationName);
    procedure SetMasterActorObject(const Value: TGLActor);
    function GetMasterActorObject: TGLActor;
    function GetLibMaterialName: TGLLibMaterialName;
    procedure SetLibMaterialName(const Value: TGLLibMaterialName);
    procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
    procedure SetStoreBonesMatrix(const Value: boolean);
    procedure SetStoredBoneNames(const Value: TStrings);
    procedure SetOnBeforeRender(const Value: TGLProgressEvent);
  protected
     
    procedure DoStoreBonesMatrices;
      // stores matrices of bones of the current frame rendered
  public
     
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure DoProgress(const progressTime: TProgressTimes); override;
    property CurrentFrame: Integer read FCurrentFrame;
    property StartFrame: Integer read FStartFrame;
    property EndFrame: Integer read FEndFrame;
    property CurrentFrameDelta: Single read FCurrentFrameDelta;
    property CurrentTime: TProgressTimes read FCurrentTime;
    { Gets the Bones Matrix in the current animation frame.
     (since the masterobject is shared between all proxies, each proxy will have it's bones matrices) }
    function BoneMatrix(BoneIndex: integer): TMatrix; overload;
    function BoneMatrix(BoneName: string): TMatrix; overload;
    procedure BoneMatricesClear;

    { A standard version of the RayCastIntersect function. }
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean; override;

    { Raycasts on self, but actually on the "RefActor" Actor.
       Note that the "RefActor" parameter does not necessarily have to be
       the same Actor refernced by the MasterObject property:
       This allows to pass a low-low-low-poly Actor to raycast in the "RefActor" parameter,
       while using a high-poly Actor in the "MasterObject" property,
       of course we assume that the two Masterobject Actors have same animations.
      }
    function RayCastIntersectEx(RefActor: TGLActor; const rayStart, rayVector:
      TVector;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean; overload;

  published
     
    property AnimationMode: TGLActorProxyAnimationMode read FAnimationMode write
      FAnimationMode default pamInherited;
    property Animation: TActorAnimationName read FAnimation write SetAnimation;
    // Redeclare as TGLActor.
    property MasterObject: TGLActor read GetMasterActorObject write
      SetMasterActorObject;
    // Redeclare without pooTransformation
    // (Don't know why it causes the object to be oriented incorrecly.)
    property ProxyOptions default [pooEffects, pooObjects];
    { Specifies the MaterialLibrary, that current proxy will use. }
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary write
      SetMaterialLibrary;
    { Specifies the Material, that current proxy will use. }
    property LibMaterialName: TGLLibMaterialName read GetLibMaterialName write
      SetLibMaterialName;
    { Specifies if it will store the Bones Matrices, accessible via the BoneMatrix function
     (since the masterobject is shared between all proxies, each proxy will have it's bones matrices) }
    property StoreBonesMatrix: boolean read FStoreBonesMatrix write
      SetStoreBonesMatrix;
    { Specifies the names of the bones we want the matrices to be stored. If empty, all bones will be stored
     (since the masterobject is shared between all proxies, each proxy will have it's bones matrices) }
    property StoredBoneNames: TStrings read FStoredBoneNames write
      SetStoredBoneNames;
    { Event allowing to apply extra transformations (f.ex: bone rotations) to the referenced
       Actor on order to have the proxy render these changes.  }
    property OnBeforeRender: TGLProgressEvent read FOnBeforeRender write
      SetOnBeforeRender;
  end;

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

// ------------------
// ------------------ TGLColorProxy ------------------
// ------------------

// Create
//

constructor TGLColorProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFrontColor := TGLFaceProperties.Create(Self);
end;

// Destroy
//

destructor TGLColorProxy.Destroy;
begin
  FFrontColor.Free;

  inherited Destroy;
end;

// Render
//

procedure TGLColorProxy.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  gotMaster, masterGotEffects, oldProxySubObject: Boolean;
begin
  if FRendering then
    Exit;
  FRendering := True;
  try
    gotMaster := Assigned(MasterObject);
    masterGotEffects := gotMaster and (pooEffects in ProxyOptions)
      and (MasterObject.Effects.Count > 0);
    if gotMaster then
    begin
      if pooObjects in ProxyOptions then
      begin
        oldProxySubObject := ARci.proxySubObject;
        ARci.proxySubObject := True;
        if pooTransformation in ProxyOptions then
          GL.MultMatrixf(PGLFloat(MasterObject.MatrixAsAddress));
        GetMasterMaterialObject.Material.FrontProperties.Assign(FFrontColor);
        MasterObject.DoRender(ARci, ARenderSelf, MasterObject.Count > 0);
        ARci.proxySubObject := oldProxySubObject;
      end;
    end;
    // now render self stuff (our children, our effects, etc.)
    if ARenderChildren and (Count > 0) then
      Self.RenderChildren(0, Count - 1, ARci);
    if masterGotEffects then
      MasterObject.Effects.RenderPostEffects(ARci);
  finally
    FRendering := False;
  end;
  ClearStructureChanged;
end;

// GetMasterMaterialObject
//

function TGLColorProxy.GetMasterMaterialObject: TGLCustomSceneObject;
begin
  Result := TGLCustomSceneObject(inherited MasterObject);
end;

// SetMasterMaterialObject
//

procedure TGLColorProxy.SetFrontColor(AValue: TGLFaceProperties);
begin
  FFrontColor.Assign(AValue);
end;

procedure TGLColorProxy.SetMasterMaterialObject(
  const Value: TGLCustomSceneObject);
begin
  inherited SetMasterObject(Value);
end;

// ------------------
// ------------------ TGLFreeFormProxy ------------------
// ------------------

// OctreeRayCastIntersect
//

function TGLFreeFormProxy.OctreeRayCastIntersect(const rayStart, rayVector:
  TVector;
  intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
var
  localRayStart, localRayVector: TVector;
begin
  if Assigned(MasterObject) then
  begin
    SetVector(localRayStart, AbsoluteToLocal(rayStart));
    SetVector(localRayStart, MasterObject.LocalToAbsolute(localRayStart));
    SetVector(localRayVector, AbsoluteToLocal(rayVector));
    SetVector(localRayVector, MasterObject.LocalToAbsolute(localRayVector));
    NormalizeVector(localRayVector);

    Result := GetMasterFreeFormObject.OctreeRayCastIntersect(localRayStart,
      localRayVector,
      intersectPoint, intersectNormal);
    if Result then
    begin
      if Assigned(intersectPoint) then
      begin
        SetVector(intersectPoint^,
          MasterObject.AbsoluteToLocal(intersectPoint^));
        SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
      end;
      if Assigned(intersectNormal) then
      begin
        SetVector(intersectNormal^,
          MasterObject.AbsoluteToLocal(intersectNormal^));
        SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      end;
    end;
  end
  else
    Result := False;
end;

// OctreeSphereSweepIntersect
//

function TGLFreeFormProxy.OctreeSphereSweepIntersect(const rayStart, rayVector:
  TVector;
  const velocity, radius, modelscale: Single;
  intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
var
  localRayStart, localRayVector: TVector;
  localVelocity, localRadius: single;
begin
  Result := False;
  if Assigned(MasterObject) then
  begin
    localVelocity := velocity * modelscale;
    localRadius := radius * modelscale;

    SetVector(localRayStart, AbsoluteToLocal(rayStart));
    SetVector(localRayStart, MasterObject.LocalToAbsolute(localRayStart));
    SetVector(localRayVector, AbsoluteToLocal(rayVector));
    SetVector(localRayVector, MasterObject.LocalToAbsolute(localRayVector));
    NormalizeVector(localRayVector);

    Result := GetMasterFreeFormObject.OctreeSphereSweepIntersect(localRayStart,
      localRayVector,
      localVelocity, localRadius,
      intersectPoint, intersectNormal);
    if Result then
    begin
      if Assigned(intersectPoint) then
      begin
        SetVector(intersectPoint^,
          MasterObject.AbsoluteToLocal(intersectPoint^));
        SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
      end;
      if Assigned(intersectNormal) then
      begin
        SetVector(intersectNormal^,
          MasterObject.AbsoluteToLocal(intersectNormal^));
        SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      end;
    end;

  end;
end;

// GetMasterFreeFormObject
//

function TGLFreeFormProxy.GetMasterFreeFormObject: TGLFreeForm;
begin
  Result := TGLFreeForm(inherited MasterObject);
end;

// SetMasterFreeFormObject
//

procedure TGLFreeFormProxy.SetMasterFreeFormObject(
  const Value: TGLFreeForm);
begin
  inherited SetMasterObject(Value);
end;

// ------------------
// ------------------ TGLActorProxy ------------------
// ------------------

// Create
//

function TGLActorProxy.BoneMatrix(BoneIndex: integer): TMatrix;
begin
  if BoneIndex < FBonesMatrices.count then
    result := TBoneMatrixObj(FBonesMatrices.Objects[BoneIndex]).Matrix;
end;

function TGLActorProxy.BoneMatrix(BoneName: string): TMatrix;
var
  i: Integer;
begin
  i := FBonesMatrices.IndexOf(BoneName);
  if i > -1 then
    result := TBoneMatrixObj(FBonesMatrices.Objects[i]).Matrix;
end;

procedure TGLActorProxy.BoneMatricesClear;
var
  i: Integer;
begin
  for i := 0 to FBonesMatrices.Count - 1 do
  begin
    TBoneMatrixObj(FBonesMatrices.Objects[i]).free;
  end;
  FBonesMatrices.Clear;
end;

constructor TGLActorProxy.Create(AOwner: TComponent);
begin
  inherited;
  FAnimationMode := pamInherited;
  ProxyOptions := ProxyOptions - [pooTransformation];
  FBonesMatrices := TStringList.create;
  FStoredBoneNames := TStringList.create;
  FStoreBonesMatrix := false;
    // default is false to speed up a little if we don't need bones info
end;

// DoProgress
//

destructor TGLActorProxy.Destroy;
begin
  BoneMatricesClear;
  FBonesMatrices.free;
  FStoredBoneNames.free;
  inherited;
end;

procedure TGLActorProxy.DoProgress(const progressTime: TProgressTimes);
begin
  inherited;
  FCurrentTime := progressTime;
end;

// DoRender
//

procedure TGLActorProxy.DoRender(var ARci: TGLRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);
var
  // TGLActorProxy specific
  cf, sf, ef: Integer;
  cfd: Single;
  // General proxy stuff.
  gotMaster, masterGotEffects, oldProxySubObject: Boolean;
  MasterActor: TGLActor;
begin
  try
    MasterActor := GetMasterActorObject;
    gotMaster := MasterActor <> nil;
    masterGotEffects := gotMaster and (pooEffects in ProxyOptions) and
      (MasterObject.Effects.Count > 0);
    if gotMaster then
    begin
      if pooObjects in ProxyOptions then
      begin
        oldProxySubObject := ARci.proxySubObject;
        ARci.proxySubObject := True;
        if pooTransformation in ProxyOptions then
          with ARci.PipelineTransformation do
            ModelMatrix := MatrixMultiply(MasterActor.Matrix, ModelMatrix);

        // At last TGLActorProxy specific stuff!
        with MasterActor do
        begin
          cfd := CurrentFrameDelta;
          cf := CurrentFrame;
          sf := startframe;
          ef := endframe;

          case FAnimationMode of
            pamInherited: CurrentFrameDelta := FCurrentFrameDelta;
            pamPlayOnce:
              begin
                if (FLastFrame <> FEndFrame - 1) then
                  CurrentFrameDelta := FCurrentFrameDelta
                else
                begin
                  FCurrentFrameDelta := 0;
                  FAnimationMode := pamNone;
                end;
              end;
            pamNone: CurrentFrameDelta := 0;
          else
            Assert(False, glsUnknownType);
          end;

          SetCurrentFrameDirect(FCurrentFrame);
          FLastFrame := FCurrentFrame;
          StartFrame := FStartFrame;
          EndFrame := FEndFrame;

          if (FMasterLibMaterial <> nil) and (FMaterialLibrary <> nil) then
            MasterActor.Material.QuickAssignMaterial(
              FMaterialLibrary, FMasterLibMaterial);

          DoProgress(FCurrentTime);

          if Assigned(FOnBeforeRender) then
            FOnBeforeRender(self, FCurrentTime.deltaTime, FCurrentTime.newTime);

          DoRender(ARci, ARenderSelf, Count > 0);

          // Stores Bones matrices of the current frame
          if (FStoreBonesMatrix) and (MasterActor.Skeleton <> nil) then
            DoStoreBonesMatrices;

          FCurrentFrameDelta := CurrentFrameDelta;
          FCurrentFrame := CurrentFrame;
          CurrentFrameDelta := cfd;
          SetCurrentFrameDirect(cf);
          startframe := sf;
          endframe := ef;
        end;

        ARci.proxySubObject := oldProxySubObject;
      end;
    end;
    // now render self stuff (our children, our effects, etc.)
    oldProxySubObject := ARci.proxySubObject;
    ARci.proxySubObject := True;
    if ARenderChildren and (Count > 0) then
      Self.RenderChildren(0, Count - 1, ARci);
    if masterGotEffects then
      MasterActor.Effects.RenderPostEffects(ARci);
    ARci.proxySubObject := oldProxySubObject;
  finally
    ClearStructureChanged;
  end;
end;

procedure TGLActorProxy.DoStoreBonesMatrices;
var
  i, n: integer;
  Bmo: TBoneMatrixObj;
  Bone: TGLSkeletonBone;
begin
  if FStoredBoneNames.count > 0 then
  begin
    // If we specified some bone names, only those bones matrices will be stored (save some cpu)
    if FBonesMatrices.Count < FStoredBoneNames.Count then
    begin
      n := FBonesMatrices.Count;
      for i := n to FStoredBoneNames.Count - 1 do
      begin
        Bone := MasterObject.Skeleton.BoneByName(FStoredBoneNames[i]);
        if Bone <> nil then
        begin
          Bmo := TBoneMatrixObj.Create;
          Bmo.BoneName := Bone.Name;
          Bmo.BoneIndex := Bone.BoneID;
          FBonesMatrices.AddObject(Bone.Name, Bmo);
        end;

      end;
    end;
  end
  else
  begin
    // Add (missing) TBoneMatrixObjects (actually ony 1st time) from all bones in skeleton
    if FBonesMatrices.Count < MasterObject.Skeleton.BoneCount - 1 then
      // note : BoneCount actually returns 1 count more.
    begin
      n := FBonesMatrices.Count;
      for i := n to MasterObject.Skeleton.BoneCount - 2 do
        // note : BoneCount actually returns 1 count more.
      begin
        Bone := MasterObject.Skeleton.BoneByID(i);
        if Bone <> nil then
        begin
          Bmo := TBoneMatrixObj.Create;
          Bmo.BoneName := Bone.Name;
          Bmo.BoneIndex := Bone.BoneID;
          FBonesMatrices.AddObject(Bone.Name, Bmo);
        end;

      end;
    end;
  end;

  // fill FBonesMatrices list
  for i := 0 to FBonesMatrices.count - 1 do
  begin
    Bmo := TBoneMatrixObj(FBonesMatrices.Objects[i]);
    Bmo.Matrix := MasterObject.Skeleton.BoneByID(Bmo.BoneIndex).GlobalMatrix;
  end;
end;

// GetMasterObject
//

function TGLActorProxy.GetMasterActorObject: TGLActor;
begin
  Result := TGLActor(inherited MasterObject);
end;

function TGLActorProxy.GetLibMaterialName: TGLLibMaterialName;
begin
  Result := FMaterialLibrary.GetNameOfLibMaterial(FMasterLibMaterial);
  if Result = '' then
    Result := FTempLibMaterialName;
end;

function TGLActorProxy.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLActorProxy.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FMaterialLibrary then
      FMaterialLibrary := nil;
  end;
end;

function TGLActorProxy.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint, intersectNormal: PVector): Boolean;
begin
  if MasterObject <> nil then
    Result := RayCastIntersectEx(GetMasterActorObject, rayStart, rayVector,
      intersectPoint, intersectNormal)
  else
    Result := inherited RayCastIntersect(rayStart, rayVector, intersectPoint,
      intersectNormal);
end;

// Gain access to TGLDummyActor.DoAnimate().
type
  TGLDummyActor = class(TGLActor);

function TGLActorProxy.RayCastIntersectEx(RefActor: TGLActor; const rayStart,
  rayVector: TVector; intersectPoint, intersectNormal: PVector): Boolean;
var
  localRayStart, localRayVector: TVector;
  cf, sf, ef: Integer;
  cfd: Single;
  HaspooTransformation: boolean;
begin
  // Set RefObject frame as current ActorProxy frame
  with RefActor do
  begin
    // VARS FOR ACTOR TO ASSUME ACTORPROXY CURRENT ANIMATION FRAME
    cfd := RefActor.CurrentFrameDelta;
    cf := RefActor.CurrentFrame;
    sf := RefActor.startframe;
    ef := RefActor.endframe;
    RefActor.CurrentFrameDelta := self.CurrentFrameDelta;
    RefActor.SetCurrentFrameDirect(self.CurrentFrame);
    RefActor.StartFrame := self.StartFrame;
    RefActor.EndFrame := self.EndFrame;
    RefActor.CurrentFrame := self.CurrentFrame;

    // FORCE ACTOR TO ASSUME ACTORPROXY CURRENT ANIMATION FRAME
    TGLDummyActor(RefActor).DoAnimate();

    HaspooTransformation := pooTransformation in self.ProxyOptions;

    // transform RAYSTART
    SetVector(localRayStart, self.AbsoluteToLocal(rayStart));
    if not HaspooTransformation then
      SetVector(localRayStart, RefActor.LocalToAbsolute(localRayStart));

    // transform RAYVECTOR
    SetVector(localRayVector, self.AbsoluteToLocal(rayVector));
    if not HaspooTransformation then
      SetVector(localRayVector, RefActor.LocalToAbsolute(localRayVector));

    NormalizeVector(localRayVector);

    Result := RefActor.RayCastIntersect(localRayStart, localRayVector,
      intersectPoint, intersectNormal);
    if Result then
    begin
      if Assigned(intersectPoint) then
      begin
        if not HaspooTransformation then
          SetVector(intersectPoint^, RefActor.AbsoluteToLocal(intersectPoint^));
        SetVector(intersectPoint^, self.LocalToAbsolute(intersectPoint^));
      end;
      if Assigned(intersectNormal) then
      begin
        if not HaspooTransformation then
          SetVector(intersectNormal^,
            RefActor.AbsoluteToLocal(intersectNormal^));
        SetVector(intersectNormal^, self.LocalToAbsolute(intersectNormal^));
      end;
    end;

    // Return RefObject to it's old time
    CurrentFrameDelta := cfd;
    SetCurrentFrameDirect(cf);
    CurrentFrame := cf;
    startframe := sf;
    endframe := ef;

    // REVERT ACTOR TO ASSUME ORIGINAL ANIMATION FRAME
    TGLDummyActor(RefActor).DoAnimate();
  end;
end;

// SetAnimation
//

procedure TGLActorProxy.SetAnimation(const Value: TActorAnimationName);
var
  anAnimation: TActorAnimation;
begin
  // We first assign the value (for persistency support), then check it.
  FAnimation := Value;

  if Assigned(MasterObject) then
  begin
    anAnimation := GetMasterActorObject.Animations.FindName(Value);
    if Assigned(anAnimation) then
    begin
      FStartFrame := anAnimation.StartFrame;
      FEndFrame := anAnimation.EndFrame;
      FCurrentFrame := FStartFrame;
      FLastFrame := FCurrentFrame;
    end;
  end;
end;

procedure TGLActorProxy.SetStoredBoneNames(const Value: TStrings);
begin
  if value <> nil then
    FStoredBoneNames.Assign(Value);
end;

// SetMasterObject
//

procedure TGLActorProxy.SetMasterActorObject(const Value: TGLActor);
begin
  inherited SetMasterObject(Value);
  BoneMatricesClear;
end;

procedure TGLActorProxy.SetLibMaterialName(
  const Value: TGLLibMaterialName);
begin
  if FMaterialLibrary = nil then
  begin
    FTempLibMaterialName := Value;
    if not (csLoading in ComponentState) then
      raise ETexture.Create(glsErrorEx + glsMatLibNotDefined);
  end
  else
  begin
    FMasterLibMaterial := FMaterialLibrary.LibMaterialByName(Value);
    FTempLibMaterialName := '';
  end;
end;

procedure TGLActorProxy.SetMaterialLibrary(const Value: TGLMaterialLibrary);
begin
  if FMaterialLibrary <> Value then
  begin
    if FMaterialLibrary <> nil then
      FMaterialLibrary.RemoveFreeNotification(Self);
    FMaterialLibrary := Value;

    if FMaterialLibrary <> nil then
    begin
      FMaterialLibrary.FreeNotification(Self);
      if FTempLibMaterialName <> '' then
        SetLibMaterialName(FTempLibMaterialName);
    end
    else
    begin
      FTempLibMaterialName := '';
    end;
  end;
end;

procedure TGLActorProxy.SetOnBeforeRender(const Value: TGLProgressEvent);
begin
  FOnBeforeRender := Value;
end;

procedure TGLActorProxy.SetStoreBonesMatrix(const Value: boolean);
begin
  FStoreBonesMatrix := Value;
end;

{ TGLMaterialProxy }

constructor TGLMaterialProxy.Create(AOwner: TComponent);
begin
  inherited;
  // Nothing here.
end;

destructor TGLMaterialProxy.Destroy;
begin
  // Nothing here.
  inherited;
end;

procedure TGLMaterialProxy.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  gotMaster, masterGotEffects, oldProxySubObject: Boolean;
begin
  if FRendering then
    Exit;
  FRendering := True;
  try
    gotMaster := Assigned(MasterObject);
    masterGotEffects := gotMaster and (pooEffects in ProxyOptions)
      and (MasterObject.Effects.Count > 0);
    if gotMaster then
    begin
      if pooObjects in ProxyOptions then
      begin
        oldProxySubObject := ARci.proxySubObject;
        ARci.proxySubObject := True;
        if pooTransformation in ProxyOptions then
          GL.MultMatrixf(PGLFloat(MasterObject.MatrixAsAddress));

        if (FMasterLibMaterial <> nil) and (FMaterialLibrary <> nil) then
          GetMasterMaterialObject.Material.QuickAssignMaterial(
            FMaterialLibrary, FMasterLibMaterial);

        MasterObject.DoRender(ARci, ARenderSelf, MasterObject.Count > 0);
        ARci.proxySubObject := oldProxySubObject;
      end;
    end;
    // now render self stuff (our children, our effects, etc.)
    if ARenderChildren and (Count > 0) then
      Self.RenderChildren(0, Count - 1, ARci);
    if masterGotEffects then
      MasterObject.Effects.RenderPostEffects(ARci);
  finally
    FRendering := False;
  end;
  ClearStructureChanged;
end;

function TGLMaterialProxy.GetMasterLibMaterialName: TGLLibMaterialName;
begin
  Result := FMaterialLibrary.GetNameOfLibMaterial(FMasterLibMaterial);
  if Result = '' then
    Result := FTempLibMaterialName;
end;

function TGLMaterialProxy.GetMasterMaterialObject: TGLCustomSceneObject;
begin
  Result := TGLCustomSceneObject(inherited MasterObject);
end;

function TGLMaterialProxy.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLMaterialProxy.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FMaterialLibrary then
      FMaterialLibrary := nil;
  end;
end;

procedure TGLMaterialProxy.SetMasterLibMaterialName(
  const Value: TGLLibMaterialName);
begin
  if FMaterialLibrary = nil then
  begin
    FTempLibMaterialName := Value;
    if not (csLoading in ComponentState) then
      raise ETexture.Create(glsErrorEx + glsMatLibNotDefined);
  end
  else
  begin
    FMasterLibMaterial := FMaterialLibrary.LibMaterialByName(Value);
    FTempLibMaterialName := '';
  end;
end;

procedure TGLMaterialProxy.SetMasterMaterialObject(
  const Value: TGLCustomSceneObject);
begin
  inherited SetMasterObject(Value);
end;

procedure TGLMaterialProxy.SetMaterialLibrary(
  const Value: TGLMaterialLibrary);
begin
  if FMaterialLibrary <> Value then
  begin
    if FMaterialLibrary <> nil then
      FMaterialLibrary.RemoveFreeNotification(Self);
    FMaterialLibrary := Value;

    if FMaterialLibrary <> nil then
    begin
      FMaterialLibrary.FreeNotification(Self);
      if FTempLibMaterialName <> '' then
        SetMasterLibMaterialName(FTempLibMaterialName);
    end
    else
    begin
      FTempLibMaterialName := '';
    end;
  end;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  RegisterClasses([TGLColorProxy, TGLFreeFormProxy, TGLActorProxy,
    TGLMaterialProxy]);

end.

