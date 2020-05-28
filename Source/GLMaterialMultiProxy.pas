//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Implements a multi-proxy object, useful for discreet LOD.
   Allows assign a unique material for each proxy master.

    History :  
       30/08/10 - Yar - Fixed transformation in TGLMaterialMultiProxy.DoRender
       23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
       22/04/10 - Yar - Fixes after GLState revision
       10/04/08 - DaStr - Added a Delpi 5 interface bug work-around to
                              TGLMaterialMultiProxyMaster (BugTracker ID = 1938988)
       25/03/07 - Added GLCrossPlatform to uses for Delphi5 compatibility
       17/02/07 - DaStr - Initial version (contributed to GLScene)
          

  What changed compared to GLMultiProxy:
    1) Allows assign a unique material for each proxy master
    2) TGLMaterialMultiProxyMaster: FDistanceMin, FDistanceMax removed
    3) TGLMaterialMultiProxy = class(TGLBaseSceneObject)!!!
    4) TGLMaterialMultiProxyMaster.Visible removed
    5) TGLMaterialMultiProxy.MaterialLibrary added
    6) TGLMaterialMultiProxyMaster.MasterLibMaterial added
    7) TGLMaterialMultiProxyMasters.Add overloaded
    8) Implemented a new mechanizm of connecting TGLLibMaterial and TGLLibMaterialName
      (they are connected on assigning, not while rendering; full persistency support;
       allows to assign directly to TGLLibMaterial)
    9) VCL-style code formating


  Previous version history:
          v1.0    12 December  '2005  Creation (based on MultiProxy.pas)
          v1.0.1  18 June      '2006  Some IFDEFs added
          v1.0.2  20 December  '2006  STRANGE_GLSCENE_VERSION checks removed
          v1.1    17 February  '2007  Made design-time compatible
}

unit GLMaterialMultiProxy;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,

  GLScene, GLVectorGeometry, GLTexture, GLMaterial, GLSilhouette, GLStrings,
  GLCrossPlatform, GLPersistentClasses, GLRenderContextInfo, GLBaseClasses,
  GLContext , GLVectorTypes;

type

  TGLMaterialMultiProxy = class;

  // TGLMaterialMultiProxyMaster
  //
  { MasterObject description for a MultiProxy object. }
  TGLMaterialMultiProxyMaster = class(TGLInterfacedCollectionItem, IGLMaterialLibrarySupported)
  private
     
    FMasterObject: TGLBaseSceneObject;
    FMasterLibMaterial: TGLLibMaterial;
    FTempLibMaterialName: TGLLibMaterialName;
    FDistanceMin2, FDistanceMax2: Single;

    procedure SetMasterLibMaterialName(const Value: TGLLibMaterialName);
    function GetMasterLibMaterialName: TGLLibMaterialName;

    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
  protected
     
    function GetDisplayName: string; override;
    procedure SetMasterObject(const Val: TGLBaseSceneObject);
    procedure SetDistanceMin(const Val: Single);
    procedure SetDistanceMax(const Val: Single);
    function GetDistanceMin: Single;
    function GetDistanceMax: Single;

  public
     
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function OwnerObject: TGLMaterialMultiProxy;
    procedure NotifyChange;

    { Specifies the Material, that current master object will use.
       Provides a faster way to access FMasterLibMaterial, compared to
       MasterLibMaterialName }
    property MasterLibMaterial: TGLLibMaterial read FMasterLibMaterial write FMasterLibMaterial stored False;
  published
     
    { Specifies the Master object which will be proxy'ed. }
    property MasterObject: TGLBaseSceneObject read FMasterObject write SetMasterObject;
    { Specifies the Material, that current master object will use. }
    property MasterLibMaterialName: TGLLibMaterialName read GetMasterLibMaterialName write SetMasterLibMaterialName;
    { Minimum visibility Distance (inclusive). }
    property DistanceMin: Single read GetDistanceMin write SetDistanceMin;
    { Maximum visibility Distance (exclusive). }
    property DistanceMax: Single read GetDistanceMax write SetDistanceMax;
  end;

  // TGLMaterialMultiProxyMasters
  //
  { Collection of TGLMaterialMultiProxyMaster. }
  TGLMaterialMultiProxyMasters = class(TOwnedCollection)
  private
     

  protected
     
    procedure SetItems(index: Integer; const Val: TGLMaterialMultiProxyMaster);
    function GetItems(index: Integer): TGLMaterialMultiProxyMaster;
    procedure Update(Item: TCollectionItem); override;
    procedure Notification(AComponent: TComponent); virtual;
  public
     
    constructor Create(AOwner: TPersistent);

    function Add: TGLMaterialMultiProxyMaster; overload;
    function Add(Master: TGLBaseSceneObject; DistanceMin, DistanceMax: Single): TGLMaterialMultiProxyMaster; overload;
    function Add(Master: TGLBaseSceneObject; MasterLibMaterial: TGLLibMaterial; DistanceMin, DistanceMax: Single): TGLMaterialMultiProxyMaster; overload;
    property Items[index: Integer]: TGLMaterialMultiProxyMaster read GetItems write SetItems; default;

    procedure NotifyChange;
    procedure EndUpdate; override;
  end;

  // TGLMaterialMultiProxy
  //
   { Multiple Proxy object.
      This proxy has multiple Master objects, which are individually made visible
      depending on a Distance to the camera criterion. It can be used to implement
      discreet level of detail directly for static objects, or objects that
      go through cyclic animation.
      For dimensionsn raycasting and silhouette purposes, the first Master is used
      (item zero in the MasterObjects collection). }
  TGLMaterialMultiProxy = class(TGLBaseSceneObject)
  private
     
    FMasterObjects: TGLMaterialMultiProxyMasters;
    FRendering: Boolean; // internal use (loop protection)
    FMaterialLibrary: TGLMaterialLibrary;
    procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
  protected
     
    procedure SetMasterObjects(const Val: TGLMaterialMultiProxyMasters);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function PrimaryMaster: TGLBaseSceneObject;

  public
     
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var rci: TGLRenderContextInfo; renderSelf, renderChildren: Boolean); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean; override;
    function GenerateSilhouette(const silhouetteParameters: TGLSilhouetteParameters): TGLSilhouette; override;

  published
     
    property MasterObjects: TGLMaterialMultiProxyMasters read FMasterObjects write SetMasterObjects;
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;

    property ObjectsSorting;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property ShowAxes;
    property TurnAngle;
    property Up;
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
  end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

// ------------------
// ------------------ TGLMaterialMultiProxyMaster ------------------
// ------------------

// Create
//
constructor TGLMaterialMultiProxyMaster.Create(Collection: TCollection);
begin
  inherited Create(Collection);

end;

// Destroy
//
destructor TGLMaterialMultiProxyMaster.Destroy;
begin
  MasterObject := nil;
  inherited Destroy;
end;

 
//
procedure TGLMaterialMultiProxyMaster.Assign(Source: TPersistent);
begin
  if Source is TGLMaterialMultiProxyMaster then
  begin
    FMasterObject := TGLMaterialMultiProxyMaster(Source).FMasterObject;
    FTempLibMaterialName := TGLMaterialMultiProxyMaster(Source).FTempLibMaterialName;
    FDistanceMin2 := TGLMaterialMultiProxyMaster(Source).FDistanceMin2;
    FDistanceMax2 := TGLMaterialMultiProxyMaster(Source).FDistanceMax2;
    NotifyChange;
  end
  else
    inherited;
end;

// OwnerObject
//
function TGLMaterialMultiProxyMaster.OwnerObject: TGLMaterialMultiProxy;
begin
  if Collection = nil then
    Result := nil
  else
    Result := TGLMaterialMultiProxy(TGLMaterialMultiProxyMasters(Collection).GetOwner);
end;

// NotifyChange
//
procedure TGLMaterialMultiProxyMaster.NotifyChange;
begin
  TGLMaterialMultiProxyMasters(Collection).NotifyChange;
end;

// GetDisplayName
//
function TGLMaterialMultiProxyMaster.GetDisplayName: string;
begin
  if MasterObject <> nil then
    Result := MasterObject.Name
  else
    Result := '???';
  Result := Result + Format(' [%.2f; %.2f[', [DistanceMin, DistanceMax]);
end;

// SetMasterObject
//
procedure TGLMaterialMultiProxyMaster.SetMasterObject(const Val: TGLBaseSceneObject);
begin
  if FMasterObject <> Val then
  begin
    if Assigned(FMasterObject) then
      FMasterObject.RemoveFreeNotification(OwnerObject);
    FMasterObject := Val;
    if Assigned(FMasterObject) then
      FMasterObject.FreeNotification(OwnerObject);
    NotifyChange;
  end;
end;

// SetDistanceMin
//
procedure TGLMaterialMultiProxyMaster.SetDistanceMin(const Val: Single);
var
  tmp: Single;
begin
  tmp := Sqr(Val);
  if FDistanceMin2 <> tmp then
  begin
    FDistanceMin2 := tmp;
    NotifyChange;
  end;
end;

// SetDistanceMax
//
procedure TGLMaterialMultiProxyMaster.SetDistanceMax(const Val: Single);
var
  tmp: Single;
begin
  tmp := Sqr(Val);
  if FDistanceMax2 <> tmp then
  begin
    FDistanceMax2 := tmp;
    NotifyChange;
  end;
end;

// GetMaterialLibrary
//
function TGLMaterialMultiProxyMaster.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  if OwnerObject = nil then
    Result := nil
  else
    Result := OwnerObject.FMaterialLibrary;
end;

// GetDistanceMax
//
function TGLMaterialMultiProxyMaster.GetDistanceMax: Single;
begin
  Result := sqrt(FDistanceMax2);
end;

// GetDistanceMin
//
function TGLMaterialMultiProxyMaster.GetDistanceMin: Single;
begin
  Result := sqrt(FDistanceMin2);
end;

// SetMasterLibMaterialName
//
procedure TGLMaterialMultiProxyMaster.SetMasterLibMaterialName(
  const Value: TGLLibMaterialName);
begin
  if OwnerObject.FMaterialLibrary = nil then
  begin
    FTempLibMaterialName := Value;
    if not (csLoading in OwnerObject.ComponentState) then
      raise ETexture.Create(glsErrorEx + glsMatLibNotDefined);
  end
  else
  begin
    FMasterLibMaterial := OwnerObject.FMaterialLibrary.LibMaterialByName(Value);
    FTempLibMaterialName := '';
  end;
end;

// GetMasterLibMaterialName
//
function TGLMaterialMultiProxyMaster.GetMasterLibMaterialName: TGLLibMaterialName;
begin
  Result := OwnerObject.FMaterialLibrary.GetNameOfLibMaterial(FMasterLibMaterial);
  if Result = '' then
    Result := FTempLibMaterialName;
end;


// ------------------
// ------------------ TGLMaterialMultiProxyMasters ------------------
// ------------------

// Create
//
constructor TGLMaterialMultiProxyMasters.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TGLMaterialMultiProxyMaster);
end;

// SetItems
//
procedure TGLMaterialMultiProxyMasters.SetItems(index: Integer;
  const Val: TGLMaterialMultiProxyMaster);
begin
  inherited Items[index] := Val;
end;

// GetItems
//
function TGLMaterialMultiProxyMasters.GetItems(index: Integer): TGLMaterialMultiProxyMaster;
begin
  Result := TGLMaterialMultiProxyMaster(inherited Items[index]);
end;

// Update
//
procedure TGLMaterialMultiProxyMasters.Update(Item: TCollectionItem);
begin
  inherited;
  NotifyChange;
end;

// Add (simple)
//
function TGLMaterialMultiProxyMasters.Add: TGLMaterialMultiProxyMaster;
begin
  Result := (inherited Add) as TGLMaterialMultiProxyMaster;
end;

// Add (classic params)
//
function TGLMaterialMultiProxyMasters.Add(Master: TGLBaseSceneObject;
  DistanceMin, DistanceMax: Single): TGLMaterialMultiProxyMaster;
begin
  BeginUpdate;
  Result := (inherited Add) as TGLMaterialMultiProxyMaster;
  Result.MasterObject := Master;
  Result.DistanceMin := DistanceMin;
  Result.DistanceMax := DistanceMax;
  EndUpdate;
end;

// Notification
//
procedure TGLMaterialMultiProxyMasters.Notification(AComponent: TComponent);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Items[I] do
      if FMasterObject = AComponent then
        FMasterObject := nil;
end;

// NotifyChange
//
procedure TGLMaterialMultiProxyMasters.NotifyChange;
begin
  if (UpdateCount = 0) and (GetOwner <> nil) and (GetOwner is TGLUpdateAbleComponent) then
    TGLUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

// EndUpdate
//
procedure TGLMaterialMultiProxyMasters.EndUpdate;
begin
  inherited EndUpdate;
  // Workaround for a bug in VCL's EndUpdate
  if UpdateCount = 0 then
    NotifyChange;
end;


// Add
//
function TGLMaterialMultiProxyMasters.Add(Master: TGLBaseSceneObject;
  MasterLibMaterial: TGLLibMaterial;
  DistanceMin, DistanceMax: Single): TGLMaterialMultiProxyMaster;
begin
  BeginUpdate;
  Result := (inherited Add) as TGLMaterialMultiProxyMaster;
  Result.MasterObject := Master;
  Result.FMasterLibMaterial := MasterLibMaterial;
  Result.DistanceMin := DistanceMin;
  Result.DistanceMax := DistanceMax;
  EndUpdate;
end;

// ------------------
// ------------------ TGLMaterialMultiProxy ------------------
// ------------------

// Create
//
constructor TGLMaterialMultiProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FMasterObjects := TGLMaterialMultiProxyMasters.Create(Self);
end;

// Destroy
//
destructor TGLMaterialMultiProxy.Destroy;
begin
  inherited Destroy;
  FMasterObjects.Free;
end;

// Notification
//
procedure TGLMaterialMultiProxy.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    FMasterObjects.Notification(AComponent);
  end;
  inherited;
end;

// SetMasterObjects
//
procedure TGLMaterialMultiProxy.SetMasterObjects(const Val: TGLMaterialMultiProxyMasters);
begin
  FMasterObjects.Assign(Val);
  StructureChanged;
end;

 
//
procedure TGLMaterialMultiProxy.Assign(Source: TPersistent);
begin
  if Source is TGLMaterialMultiProxy then
    MasterObjects := TGLMaterialMultiProxy(Source).MasterObjects;
  inherited;
end;

// Render
//
procedure TGLMaterialMultiProxy.DoRender(var rci: TGLRenderContextInfo;
  renderSelf, renderChildren: Boolean);
var
  I:  Integer;
  oldProxySubObject: Boolean;
  mpMaster: TGLMaterialMultiProxyMaster;
  d2: Single;
begin
  if FRendering then
    Exit;
  FRendering := True;
  try
    d2 := VectorDistance2(rci.cameraPosition, AbsolutePosition);
    for I := 0 to MasterObjects.Count - 1 do
    begin
      mpMaster := MasterObjects[I];
      if (mpMaster.MasterObject <> nil) and (d2 >= mpMaster.FDistanceMin2) and
         (d2 < mpMaster.FDistanceMax2) then
      begin
        oldProxySubObject := rci.proxySubObject;
        rci.proxySubObject := True;
        with rci.PipelineTransformation do
          ModelMatrix := MatrixMultiply(mpMaster.MasterObject.Matrix, ModelMatrix);
        if (mpMaster.MasterObject is TGLCustomSceneObject) and (FMaterialLibrary <> nil) then
        begin
          TGLCustomSceneObject(mpMaster.MasterObject).Material.QuickAssignMaterial(
            FMaterialLibrary, mpMaster.FMasterLibMaterial);
        end;
        mpMaster.MasterObject.DoRender(rci, renderSelf, (mpMaster.MasterObject.Count > 0));
        rci.proxySubObject := oldProxySubObject;
      end;
    end;
    // now render self stuff (our children, our effects, etc.)
    if renderChildren and (Count > 0) then
      Self.RenderChildren(0, Count - 1, rci);
    //      if MasterGotEffects then
    //         FMasterObject.Effects.RenderPostEffects(Scene.CurrentBuffer, rci);
  finally
    FRendering := False;
  end;
  ClearStructureChanged;
end;

// PrimaryMaster
//
function TGLMaterialMultiProxy.PrimaryMaster: TGLBaseSceneObject;
begin
  if MasterObjects.Count > 0 then
    Result := MasterObjects[0].MasterObject
  else
    Result := nil;
end;

// AxisAlignedDimensions
//
function TGLMaterialMultiProxy.AxisAlignedDimensionsUnscaled: TVector;
var
  Master: TGLBaseSceneObject;
begin
  Master := PrimaryMaster;
  if Assigned(Master) then
    Result := Master.AxisAlignedDimensionsUnscaled
  else
    Result := inherited AxisAlignedDimensionsUnscaled;
end;

// RayCastIntersect
//
function TGLMaterialMultiProxy.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  localRayStart, localRayVector: TVector;
  Master: TGLBaseSceneObject;
begin
  Master := PrimaryMaster;
  if Assigned(Master) then
  begin
    SetVector(localRayStart, AbsoluteToLocal(rayStart));
    SetVector(localRayStart, Master.LocalToAbsolute(localRayStart));
    SetVector(localRayVector, AbsoluteToLocal(rayVector));
    SetVector(localRayVector, Master.LocalToAbsolute(localRayVector));
    NormalizeVector(localRayVector);

    Result := Master.RayCastIntersect(localRayStart, localRayVector,
      intersectPoint, intersectNormal);
    if Result then
    begin
      if Assigned(intersectPoint) then
      begin
        SetVector(intersectPoint^, Master.AbsoluteToLocal(intersectPoint^));
        SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
      end;
      if Assigned(intersectNormal) then
      begin
        SetVector(intersectNormal^, Master.AbsoluteToLocal(intersectNormal^));
        SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      end;
    end;
  end
  else
    Result := False;
end;

// GenerateSilhouette
//
function TGLMaterialMultiProxy.GenerateSilhouette(
  const silhouetteParameters: TGLSilhouetteParameters): TGLSilhouette;
var
  Master: TGLBaseSceneObject;
begin
  Master := PrimaryMaster;
  if Assigned(Master) then
    Result := Master.GenerateSilhouette(silhouetteParameters)
  else
    Result := nil;
end;

// SetMaterialLibrary
//
procedure TGLMaterialMultiProxy.SetMaterialLibrary(
  const Value: TGLMaterialLibrary);
var
  I: Integer;
begin
  if FMaterialLibrary <> Value then
  begin
    if FMaterialLibrary <> nil then
      FMaterialLibrary.RemoveFreeNotification(Self);
    FMaterialLibrary := Value;

    if FMaterialLibrary <> nil then
    begin
      FMaterialLibrary.FreeNotification(Self);
      if FMasterObjects.Count <> 0 then
        for I := 0 to FMasterObjects.Count - 1 do
          with FMasterObjects.GetItems(I) do
          begin
            if FTempLibMaterialName <> '' then
              SetMasterLibMaterialName(FTempLibMaterialName);
          end;
    end
    else
    begin
      if FMasterObjects.Count <> 0 then
        for I := 0 to FMasterObjects.Count - 1 do
          FMasterObjects.GetItems(I).FTempLibMaterialName := '';
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

  RegisterClasses([TGLMaterialMultiProxyMaster, TGLMaterialMultiProxyMasters,
                   TGLMaterialMultiProxy]);

end.

