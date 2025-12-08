{-----------------------------------------------------------------------------
 Unit Name: uVehicle

 Author:    Panagiotis Kakaletris (Orchestraman)

 Purpose:
  Implements Object Steerining Behaviours for GLScene OpenGL library.
 Bibliography:
  Based on "Steering Behaviors For Autonomous Characters" by Craig Reynolds.
  Visit http://www.red3d.com/cwr/steer/ for more information.
 Notes: Collision Code is based in GLFPSCollision unit part of GLScene OGL Library.
 History:
   5/jul/2004 - Orchestraman - First Creation
-----------------------------------------------------------------------------}

unit uVehicle;


interface

uses
  Classes, Contnrs, Dialogs,
  GLVectorGeometry,GLScene, GLXCollection, GLCoordinates,
  GLBehaviours, GLCollision, GLCadencer, GLVectorFileObjects, GLBaseClasses,
  GLManager;

type
  TSteeringBehaviours = (sbhSeek, sbhFlee, sbhPursuit, sbhEvasion, sbhOffsetPursuit, sbhArrival, sbhObstacleAvoidance,sbhWander);
  TGLSteeringBehaviours = set of TSteeringBehaviours;

  TGLBVehicle = class;
  TGLVehicleManager = class;

  TBaseSteerBehaviour = class;
  TSteerBehaviourClass = class of TBaseSteerBehaviour;

  // TBaseSteerBehaviour

  { Base Class for implementing Steering Behaviours}
  TBaseSteerBehaviour = class(TComponent)
  private
    FVehicle: TGLBVehicle;
    FSteerRatio: single;

  protected
    procedure SetVehicle(const AValue: TGLBVehicle); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySteerForce; virtual; abstract;
    property Vehicle: TGLBVehicle read FVehicle write SetVehicle;
    property Ratio: single read FSteerRatio write FSteerRatio;

  end;

  // TWanderSteer

  { Implementation of Wander Steering Behaviour}
  TWanderSteer = class(TBaseSteerBehaviour)
  private
    FWanderModifier: TVector;
    FRate, FStrength: double;

  protected
    procedure SetVehicle(const AValue: TGLBVehicle); override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySteerForce; override;
    property Rate: double read FRate write FRate;
    property Strength: double read FStrength write FStrength;
    property WanderModifier: TVector read FWanderModifier write FWanderModifier;

  end;

  // TSeekSteer

  { Implementation of Seek Steering Behaviour}
  TSeekSteer = class(TBaseSteerBehaviour)
  private
    FTarget: TGLBaseSceneObject;
    FTurnRate: single;
    procedure SetTarget(const Value: TGLBaseSceneObject);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySteerForce; override;
    property Target: TGLBaseSceneObject read FTarget write SetTarget;

  end;

  // TFleeSteer

  TFleeSteer = class(TBaseSteerBehaviour)
  private
    FTarget: TGLBaseSceneObject;
    procedure SetTarget(const Value: TGLBaseSceneObject);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySteerForce; override;
    property Target: TGLBaseSceneObject read FTarget write SetTarget;

  end;

  // TPursueSteer

  TPursueSteer = class(TBaseSteerBehaviour)
  private
    FTarget: TGLBaseSceneObject;
    procedure SetTarget(const Value: TGLBaseSceneObject);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySteerForce; override;
    property Target: TGLBaseSceneObject read FTarget write SetTarget;

  end;

  // TWorldCollisionSteer

  TWorldCollisionSteer = class(TBaseSteerBehaviour)
  private
    FMap: TGLFreeForm;
    FCollided: boolean;
    oldPosition, velocity: TVector;
    FTurnRate: single;
    procedure SetMap(const Value: TGLFreeForm);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function SphereSweepAndSlide(freeform: TGLFreeform; SphereStart: TVector; var aVelocity, newPosition: TVector; sphereRadius: single): boolean;
    procedure SetVehicle(const AValue: TGLBVehicle); override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySteerForce; override;
    property Map: TGLFreeForm read FMap write SetMap;
    property Collided: boolean read FCollided;
    property TurnRate: single read FTurnRate write FTurnRate;

  end;

  // TGLBVehicle

  TGLBVehicle = class(TGLBehaviour)
  private

    FSteerUpdateInterval: double;
    FMass: integer;
    FSpeed, FMaxForce, FMaxSpeed: double;
    FUp, FVelocity, FAccumulator: TGLCoordinates;
    FProgressTime: TProgressTimes;
    FAccumulatedTime: double;
    FManager: TGLVehicleManager;
    FGroupIndex: integer;
    FManagerName: string; // NOT persistent, temporarily used for persistence
    FSteerBehaviours: TObjectList;
    FGLSteeringBehaviours: TGLSteeringBehaviours;
    FSeekSteer: TSeekSteer;
    FWanderSteer: TWanderSteer;
    FPursueSteer: TPursueSteer;
    FFleeSteer: TFleeSteer;
    FWorldCollisionSteer: TWorldCollisionSteer;

    FCollisionObject: TGLBaseSceneObject;

  protected
    { Protected Declarations }
    procedure SetGLSteeringBehaviours(const Value: TGLSteeringBehaviours);
    procedure SetManager(const Value: TGLVehicleManager);
    procedure SetGroupIndex(const Value: integer);
    function GetVelocity: TGLCoordinates;
    procedure SetVelocity(const Value: TGLCoordinates);
    function GetSpeed: double;
    procedure SetSpeed(const Value: double);

    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;

  public

    constructor Create(aOwner: TGLXCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;

    procedure DoProgress(const progressTime: TProgressTimes); override;
    procedure DoSteering;

    property ProgressTime: TProgressTimes read FProgressTime write FProgressTime;
    property AccumulatedTime: double read FAccumulatedTime write FAccumulatedTime;

    property CollisionObject: TGLBaseSceneObject read FCollisionObject write FCollisionObject;
    property Accumulator: TGLCoordinates read FAccumulator;

    property Flee: TFleeSteer read FFleeSteer write FFleeSteer;
    property Seek: TSeekSteer read FSeekSteer write FSeekSteer;
    property Pursue: TPursueSteer read FPursueSteer write FPursueSteer;
    property Wander: TWanderSteer read FWanderSteer write FWanderSteer;
    property WorldCollision: TWorldCollisionSteer read FWorldCollisionSteer write FWorldCollisionSteer;

  published

    property Manager: TGLVehicleManager read FManager write SetManager;
    property GroupIndex: integer read FGroupIndex write SetGroupIndex;
    property Mass: integer read FMass write FMass;
    //      property Velocity: TGLCoordinates read GetVelocity write SetVelocity;
    property MaxForce: double read FMaxForce write FMaxForce;
    property MaxSpeed: double read FMaxSpeed write FMaxSpeed;
    property Speed: double read GetSpeed write SetSpeed;
    property SteeringBehaviours: TGLSteeringBehaviours read FGLSteeringBehaviours write SetGLSteeringBehaviours;
    property SteerUpdateInterval: double read FSteerUpdateInterval write FSteerUpdateInterval;
    property SteerBehaviours: TObjectList read FSteerBehaviours write FSteerBehaviours;
    property Up: TGLCoordinates read FUp write FUp;

  end;

  // TGLVehicleManager

  { Manager ðïõ äéá÷åéñßæåôáé ôá Vehicles}
  TGLVehicleManager = class(TComponent)
  private

    FSteerInterval: double;
    FClients: TList;
    FCadencer: TGLCadencer;
    FWorldCollisionMap: TGLFreeForm;
    procedure SetCadencer(const Value: TGLCadencer);
    function GetCadencer: TGLCadencer;
    procedure SetSteerInterval(const Value: double);
    procedure SetWorldCollisionMap(const Value: TGLFreeForm);

  protected
    { Protected Declarations }
    procedure RegisterClient(aClient: TGLBVehicle);
    procedure DeRegisterClient(aClient: TGLBVehicle);
    procedure DeRegisterAllClients;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoSteering;
    property Clients: TList read FClients;

  published

    property Cadencer: TGLCadencer read GetCadencer write SetCadencer;
    property SteerInterval: double read FSteerInterval write SetSteerInterval;
    property WorldCollisionMap: TGLFreeForm read FWorldCollisionMap write SetWorldCollisionMap;
  end;


{: Returns or creates the TGLBVehicle within the given behaviours.<p>
  This helper function is convenient way to access a TGLBVehicle. }
function GetOrCreateVehicle(behaviours: TGLBehaviours): TGLBVehicle; overload;
{: Returns or creates the TGLBVehicle within the given object's behaviours.<p>
  This helper function is convenient way to access a TGLBVehicle. }
function GetOrCreateVehicle(obj: TGLBaseSceneObject): TGLBVehicle; overload;

implementation

uses
  SysUtils;


// GetOrCreateVehicle (TGLBehaviours)

function GetOrCreateVehicle(behaviours: TGLBehaviours): TGLBVehicle;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TGLBVehicle);
  if i >= 0 then
    Result := TGLBVehicle(behaviours[i])
  else
    Result := TGLBVehicle.Create(behaviours);
end;

// GetOrCreateVehicle (TGLBaseSceneObject)

function GetOrCreateVehicle(obj: TGLBaseSceneObject): TGLBVehicle;
begin
  Result := GetOrCreateVehicle(obj.Behaviours);
end;

{ TGLVehicleManager }

// TGLVehicleManager.Create

constructor TGLVehicleManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClients := TList.Create;
  RegisterManager(Self);
  FSteerInterval := 0;
end;

// TGLVehicleManager.Destroy

destructor TGLVehicleManager.Destroy;
begin
  if Assigned(FCadencer) then
    FCadencer.RemoveFreeNotification(Self);
  FCadencer := nil;
  DeRegisterAllClients;
  DeRegisterManager(Self);
  FClients.Free;
  inherited Destroy;
end;

// TGLVehicleManager.DeRegisterAllClients

procedure TGLVehicleManager.DeRegisterAllClients;
var
  i: integer;
begin
  // Fast deregistration
  for i := 0 to FClients.Count - 1 do
    TGLBVehicle(FClients[i]).FManager := nil;
  FClients.Clear;
end;

// TGLVehicleManager.DeRegisterClient

procedure TGLVehicleManager.DeRegisterClient(aClient: TGLBVehicle);
begin
  if Assigned(aClient) then
  begin
    aClient.FManager := nil;
    FClients.Remove(aClient);
  end;
end;

// TGLVehicleManager.RegisterClient

procedure TGLVehicleManager.RegisterClient(aClient: TGLBVehicle);
begin
  if Assigned(aClient) then
    if FClients.IndexOf(aClient) < 0 then
    begin
      FClients.Add(aClient);
      aClient.FManager := Self;
    end;
end;

// TGLVehicleManager.DoSteering

procedure TGLVehicleManager.DoSteering;
var
  I: integer;
begin
  if FClients.Count > 0 then
  begin
    for I := 0 to FClients.Count - 1 do
    begin
      TGLBVehicle(FClients[I]).DoSteering;
    end;
  end;
end;


{ TGLBVehicle }

// TGLBVehicle.Create

constructor TGLBVehicle.Create(aOwner: TGLXCollection);
begin
  inherited Create(aOwner);

  FSteerUpdateInterval := 0;
  FAccumulatedTime := 0;
  FMass := 10;
  FSpeed := 1;
  FMaxForce := 1;
  FMaxSpeed := 1;

  FUp := TGLCoordinates.CreateInitialized(Self, VectorMake(0, 1, 0), csVector);

  FVelocity := TGLCoordinates.CreateInitialized(Self, VectorMake(1, 0, 1), csVector);
  FVelocity.Normalize;

  FAccumulator := TGLCoordinates.CreateInitialized(Self, VectorMake(1, 0, 1), csVector);
  FSteerBehaviours := TObjectList.Create(True);


  FWanderSteer := TWanderSteer.Create(nil);
  FWanderSteer.Vehicle := Self;
  FSteerBehaviours.Add(FWanderSteer);

  FSeekSteer := TSeekSteer.Create(nil);
  FSeekSteer.Vehicle := Self;
  FSteerBehaviours.Add(FSeekSteer);

  FFleeSteer := TFleeSteer.Create(nil);
  FFleeSteer.Vehicle := Self;
  FSteerBehaviours.Add(FFleeSteer);

  FPursueSteer := TPursueSteer.Create(nil);
  FFleeSteer.Vehicle := Self;
  FSteerBehaviours.Add(FPursueSteer);

end;

// TGLBVehicle.Destroy

destructor TGLBVehicle.Destroy;
begin
  Manager := nil;
  FreeAndNil(FSteerBehaviours);
  FWanderSteer := nil;
  FSeekSteer := nil;
  FPursueSteer := nil;
  FWorldCollisionSteer := nil;
  FreeAndNil(FAccumulator);
  FreeAndNil(FUp);
  inherited Destroy;
end;

// TGLBVehicle.SetManager

procedure TGLBVehicle.SetManager(const Value: TGLVehicleManager);
begin
  if Value <> FManager then
  begin
    if Assigned(FManager) then
      FManager.DeRegisterClient(Self);
    if Assigned(Value) then
    begin
      Value.RegisterClient(Self);
      Self.SteerUpdateInterval := Value.SteerInterval;

      FWorldCollisionSteer := TWorldCollisionSteer.Create(nil);
      FWorldCollisionSteer.Vehicle := Self;
      FWorldCollisionSteer.Map := Value.WorldCollisionMap;
      FSteerBehaviours.Add(FWorldCollisionSteer);
    end;
  end;
end;

// TGLBVehicle.SetGroupIndex

procedure TGLBVehicle.SetGroupIndex(const Value: integer);
begin
  FGroupIndex := Value;
end;

// TGLBVehicle.FriendlyName

class function TGLBVehicle.FriendlyName: string;
begin
  Result := 'Steering';
end;

class function TGLBVehicle.FriendlyDescription: string;
begin
  Result := 'Steering-behaviour registration';
end;

// TGLBVehicle.Assign

procedure TGLBVehicle.Assign(Source: TPersistent);
begin
  if Source is TGLBVehicle then
  begin
    Manager := TGLBVehicle(Source).Manager;
    Mass := TGLBVehicle(Source).Mass;
    Speed := TGLBVehicle(Source).Speed;
    MaxForce := TGLBVehicle(Source).MaxForce;
    MaxSpeed := TGLBVehicle(Source).MaxSpeed;
    GroupIndex := TGLBVehicle(Source).GroupIndex;
  end;
  inherited Assign(Source);
end;

// TGLBVehicle.Loaded

{ ÊÜíåé register ôï steering behaviour óôïí ðñþôï äéáèÝóéìï steering Manager ðïõ
  èá âñåß óôçí öüñìá.}
procedure TGLBVehicle.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TGLVehicleManager, FManagerName);
    if Assigned(mng) then
      Manager := TGLVehicleManager(mng);
    FManagerName := '';
  end;
end;

// TGLBVehicle.WriteToFiler

procedure TGLBVehicle.WriteToFiler(writer: TWriter);
begin
  with writer do
  begin
    WriteInteger(1); // ArchiveVersion 1, added FGroupIndex
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
    WriteInteger(FGroupIndex);
    WriteInteger(FMass);
    WriteFloat(FSpeed);
    WriteFloat(FMaxForce);
    WriteFloat(FMaxSpeed);
    FVelocity.WriteToFiler(writer);
  end;
end;

// TGLBVehicle.ReadFromFiler

procedure TGLBVehicle.ReadFromFiler(reader: TReader);
var
  archiveVersion: integer;
begin
  with reader do
  begin
    archiveVersion := ReadInteger;
    Assert(archiveVersion in [0..1]);
    FManagerName := ReadString;
    Manager := nil;
    if archiveVersion >= 1 then
      FGroupIndex := ReadInteger
    else
      FGroupIndex := 0;
    FMass := ReadInteger;
    FSpeed := ReadFloat;
    FMaxForce := ReadFloat;
    FMaxSpeed := ReadFloat;
    FVelocity.ReadFromFiler(reader);
  end;
end;

// TGLBVehicle.GetVelocity

function TGLBVehicle.GetVelocity: TGLCoordinates;
begin
  Result := FVelocity;
end;

// TGLBVehicle.SetVelocity

procedure TGLBVehicle.SetVelocity(const Value: TGLCoordinates);
begin
  FVelocity := Value;
end;

// TGLBVehicle.GetSpeed

function TGLBVehicle.GetSpeed: double;
begin
  Result := FSpeed;
end;

// TGLBVehicle.SetSpeed

procedure TGLBVehicle.SetSpeed(const Value: double);
begin
  FSpeed := Value;
end;

// TGLBVehicle.DoSteering

procedure TGLBVehicle.DoSteering;
var
  acceleration: double;
  newLeft: TVector;
begin
  if AccumulatedTime < SteerUpdateInterval then
    exit;
  FAccumulator.SetVector(OwnerBaseSceneObject.Direction.AsVector);
  FAccumulator.Normalize;
  //FAccumulator.AsVector := NullHmgVector;
  //FAccumulator.Scale(Speed * AccumulatedTime);

  with OwnerBaseSceneObject do
  begin

    //Åêôåëþ ôï Collision.
    FWorldCollisionSteer.ApplySteerForce;
    if not FWorldCollisionSteer.Collided then
    begin
      FSeekSteer.ApplySteerForce;
      FWanderSteer.ApplySteerForce;
      FFleeSteer.ApplySteerForce;
    end
    else
    begin
      FWanderSteer.WanderModifier := OwnerBaseSceneObject.Direction.AsVector;
    end;

    Direction.AddScaledVector(AccumulatedTime, FAccumulator.AsVector);

    //Õðïëïãßæù ôç äíóç ôïõ Up Vector ãéá íá ìÞí ãÝñíåé ôï áíôéêåßìåíï êáôÜ ôç óôñïöÞ ôïõ.
    VectorCrossProduct(VectorNormalize(Direction.DirectVector), FUp.DirectVector, newLeft);
    Up.AsVector := VectorCrossProduct(VectorNormalize(Direction.DirectVector), newLeft);

    acceleration := 1 / Mass;
    speed := Lerp(speed, MaxSpeed, acceleration);

    Move(speed * AccumulatedTime);

  end;
  AccumulatedTime := 0;
end;

// TGLVehicleManager.Notification

procedure TGLVehicleManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = Cadencer) then
    Cadencer := nil
  else
  if (Operation = opRemove) and (AComponent = FWorldCollisionMap) then
  begin
    FWorldCollisionMap.RemoveFreeNotification(Self);
    FWorldCollisionMap := nil;
  end
  else
    inherited;
end;

procedure TGLVehicleManager.SetCadencer(const Value: TGLCadencer);
begin
  if FCadencer = Value then
    exit;

  if Assigned(FCadencer) then
    FCadencer.RemoveFreeNotification(Self);

  FCadencer := Value;

  if FCadencer <> nil then
    FCadencer.FreeNotification(Self);
end;

function TGLVehicleManager.GetCadencer: TGLCadencer;
begin
  Result := FCadencer;
end;

{ TBaseSteerBehaviour }

constructor TBaseSteerBehaviour.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVehicle := nil;
  FSteerRatio := 1;
end;

procedure TBaseSteerBehaviour.SetVehicle(const AValue: TGLBVehicle);
begin
  FVehicle := AValue;
end;

{ TWanderSteer }

procedure TWanderSteer.ApplySteerForce;
var
  vWander: TVector;
  vStrength: TVector;
  vDesiredDirection: TVector;
const
  c2PI = 2 * pi;
begin
  with vehicle do
  begin
    MakeVector(vWander, VectorAdd(VectorMake(cos(random * c2PI) * FRate, ClampValue(cos(random * c2Pi) * FRate, -0.01 * FRate, 0.01 * FRate),
      cos(random * c2PI) * FRate), FWanderModifier));                                     // ÖôéÜ÷íù ôõ÷áßï ä/óìá ìåôáôüðéóçò.
    NormalizeVector(vWander);                                                   // Êáíïíéêïðïéþ óôçí ìïíÜäá.
    ScaleVector(vWander, 10);                                          // ÊÜíù scale óôï WanderRate.
    FWanderModifier := vWander;

    MakeVector(vStrength, OwnerBaseSceneObject.Direction.AsVector);
    NormalizeVector(vStrength);
    ScaleVector(vStrength, FStrength);

    VectorAdd(vStrength, vWander, vDesiredDirection);
    NormalizeVector(vDesiredDirection);

    VectorSubtract(vDesiredDirection, OwnerBaseSceneObject.Direction.AsVector, vDesiredDirection);
    //NormalizeVector(vDesiredDirection);

    FAccumulator.AddScaledVector(Ratio, vDesiredDirection);

  end;
end;

// TGLBVehicle.SetGLSteeringBehaviours

procedure TGLBVehicle.SetGLSteeringBehaviours(const Value: TGLSteeringBehaviours);
begin
  FGLSteeringBehaviours := Value;

end;

// TGLVehicleManager.SetSteerInterval

procedure TGLVehicleManager.SetSteerInterval(const Value: double);
var
  I: integer;
begin
  FSteerInterval := Value;
  for I := 0 to FClients.Count - 1 do
    TGLBVehicle(FClients.Items[I]).SteerUpdateInterval := FSteerInterval;
end;

// TGLBVehicle.DoProgress

procedure TGLBVehicle.DoProgress(const progressTime: TProgressTimes);
begin
  FProgressTime := progressTime;
  AccumulatedTime := AccumulatedTime + progressTime.deltaTime;
end;

constructor TWanderSteer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRate := 1;
  FStrength := 1;
end;

{ TSeekSteer }

// TSeekSteer.ApplySteerForce

procedure TSeekSteer.ApplySteerForce;
var
  vDesiredDirection: TVector;
  vDistance: TVector;
  lDistance: single;
begin
  if Assigned(FTarget) then
    with FVehicle do
    begin
      vDesiredDirection := VectorNormalize(VectorSubtract(OwnerBaseSceneObject.Position.AsVector, FTarget.Position.AsVector));

      vDistance := VectorSubtract(OwnerBaseSceneObject.Direction.AsVector, vDesiredDirection);
      lDistance := VectorLength(vDistance);
      FAccumulator.AddScaledVector(10 * FTurnRate * lDistance * Ratio, VectorNormalize(vDistance));
    end;
end;

// TSeekSteer.Create

constructor TSeekSteer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTurnRate := 0.3;
end;

// TSeekSteer.Notification

procedure TSeekSteer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FTarget) then
  begin
    AComponent.RemoveFreeNotification(Self);
    FTarget := nil;
  end
  else
    inherited;
end;

// TSeekSteer.SetTarget

procedure TSeekSteer.SetTarget(const Value: TGLBaseSceneObject);
begin
  if Assigned(FTarget) then
    FTarget.RemoveFreeNotification(Self);
  FTarget := Value;
  if Assigned(FTarget) then
    FTarget.FreeNotification(Self);
end;

// TWanderSteer.SetVehicle

procedure TWanderSteer.SetVehicle(const AValue: TGLBVehicle);
begin
  inherited SetVehicle(AValue);
  SetVector(FWanderModifier, Vehicle.OwnerBaseSceneObject.Direction.AsVector);
end;

{ TFleeSteer }

// TFleeSteer.ApplySteerForce

procedure TFleeSteer.ApplySteerForce;
var
  vDesiredDirection: TVector;
begin
  if Assigned(FTarget) then
    with FVehicle do
    begin
      vDesiredDirection := VectorNegate(VectorNormalize(VectorSubtract(OwnerBaseSceneObject.Position.AsVector, FTarget.Position.AsVector)));
      FAccumulator.AddScaledVector(0.3 * Speed * Ratio * VectorLength(VectorSubtract(OwnerBaseSceneObject.Direction.AsVector, vDesiredDirection)),
        VectorNormalize(VectorSubtract(OwnerBaseSceneObject.Direction.AsVector, vDesiredDirection)));



    end;
end;

// TFleeSteer.Create

constructor TFleeSteer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

// TFleeSteer.Notification

procedure TFleeSteer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FTarget) then
  begin
    AComponent.RemoveFreeNotification(Self);
    FTarget := nil;
  end
  else
    inherited;
end;

// TFleeSteer.SetTarget

procedure TFleeSteer.SetTarget(const Value: TGLBaseSceneObject);
begin
  if Assigned(FTarget) then
    FTarget.RemoveFreeNotification(Self);
  FTarget := Value;
  if Assigned(FTarget) then
    FTarget.FreeNotification(Self);
end;

{ TPursueSteer }

// TPursueSteer.ApplySteerForce

procedure TPursueSteer.ApplySteerForce;
var
  vDesiredDirection: TVector;
  vDistance: TVector;
  lDistance: single;
begin
  if Assigned(FTarget) then
    with FVehicle do
    begin
      vDesiredDirection := VectorNormalize(VectorSubtract(OwnerBaseSceneObject.Position.AsVector,
        FTarget.LocalToAbsolute(FTarget.FindChild('GLDummyCube2', True).Position.AsVector)));

      FTarget.FindChild('GLDummyCube2', True).Position.Z :=
        1 - 1 * VectorDotProduct(OwnerBaseSceneObject.Direction.AsVector, FTarget.Direction.AsVector) /
        VectorDistance(OwnerBaseSceneObject.Position.AsVector, FTarget.Position.AsVector);

      vDistance := VectorSubtract(OwnerBaseSceneObject.Direction.AsVector, vDesiredDirection);
      lDistance := VectorLength(vDistance);
      FAccumulator.AddScaledVector(Speed * Ratio * lDistance, VectorNormalize(vDistance));
      //Ratio := Ratio -  0.00005;
    end;
end;

// TPursueSteer.Create

constructor TPursueSteer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

// TPursueSteer.Notification

procedure TPursueSteer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FTarget) then
  begin
    AComponent.RemoveFreeNotification(Self);
    FTarget := nil;
  end
  else
    inherited;
end;

// TPursueSteer.SetTarget

procedure TPursueSteer.SetTarget(const Value: TGLBaseSceneObject);
begin
  if Assigned(FTarget) then
    FTarget.RemoveFreeNotification(Self);
  FTarget := Value;
  if Assigned(FTarget) then
    FTarget.FreeNotification(Self);
end;

{ TWorldCollisionSteer }

function TWorldCollisionSteer.SphereSweepAndSlide(freeform: TGLFreeform; SphereStart: TVector; var aVelocity, newPosition: TVector;
  sphereRadius: single): boolean;
var
  aoldPosition, ray: TVector;
  vel, slidedistance: single;
  intPoint, intNormal: TVector;
  newDirection, newRay, collisionPosition, pointOnSphere, point2OnSphere: TVector;
  i: integer;
  SphereRadiusRel: single;
begin
  SphereRadiusRel := SphereRadius / freeform.Scale.x; // ìðïñåß íá ãßíåé Scale.y, or Scale.z ÕðïèÝôïíôáò üôé åßíáé ôá ßäéá.

  aoldPosition := SphereStart;

  Result := True;

  //Ä/íóç óôçí ïðïßá êéíåßôáé ç óöÜéñá.
  ray := VectorSubtract(newPosition, aoldPosition);

  //  ray := Velocity;
  //  newPosition := VectorAdd(newPosition,ray);
  //Ôá÷ýôçôá ôçò óöáßñáò. ÌÝôñï ôïõ äéáíýóìáôïò ôçò èÝóçò ìå ôçí ðñïçãïýìåíç èÝóç.
  //Ôï êÜíù Ýôóé ãéá íá ìÞí åîáñôþíôáé ïé õðïëïãéóìïß áðï ôçí ôá÷ýôçôá ôïõ åðåîåñãáóôÞ.
  vel := VectorLength(ray);

  //áí ç óöáßñá äåí êéíåßôáé ôüôå äåí ÷ñåéÜæåôáé íá êÜíù ôßðïôá.
  // äéáöïñåôéêÜ åêôåëþ ìÝ÷ñé 7 loops

  if vel > 0 then
    for i := 0 to 6 do
    begin
      //Áí õðÜñ÷åé intersection ÷ñåéÜæïíôáé åðéðëÝïí õðïëïãéóìïß.
      if (freeform.OctreeSphereSweepIntersect(oldPosition, ray, vel, SphereRadiusRel, @intPoint, @intNormal)) then
      begin
        if VectorDistance2(aoldPosition, intPoint) <= sqr(SphereRadius) then
        begin
          //Ç óöáßñá äéáóôáõñþíåôáé ìå êÜðïéï ôñßãùíï.
          intNormal := VectorScale(VectorSubtract(aoldPosition, intPoint), 1.0001);
        end
        else
        begin
          //áí ç óöáßñá äåí äéáóôáõñþíåôáé ìå êÜðïéï ôñßãùíï.
          //intNormal := VectorSubtract(oldPosition,intPoint);  //Äåí åßíáé óùóôü áëëÜ äïõëåýåé êáëÜ ãéá ìéêñÜ time steps.
          //intNormal := VectorScale(VectorNormalize(intNormal), SphereRadius + 0.0001);
          if RayCastSphereInterSect(intPoint, VectorNormalize(VectorNegate(ray)), oldPosition, SphereRadius, PointOnSphere, Point2OnSphere) > 0 then
            intNormal := VectorScale(VectorSubtract(aoldPosition, PointOnSphere), 1.0001)
          //intNormal := VectorScale(VectorNormalize(VectorSubtract(oldPosition, PointOnSphere)), SphereRadius + 0.001) //VectorDistance(oldPosition, PointOnSphere));
          else
          begin
            //          Assert(False);  //Áõôü äåí èá óõìâåß ðïôÝ, ìüíï ãéá debuging.
            intNormal := VectorScale(VectorSubtract(aoldPosition, intPoint), 1.0001);
          end;

        end;

        //õðïëïãéóìüò ôïõ êÝíôñïõ ôçò óöáßñáò üôáí óõìâåß collision.
        collisionPosition := VectorAdd(intPoint, intNormal);
        oldPosition := collisionPosition;

        //Õðïëïãéóìüò ôçò áðüóôáóçò ðïõ äåí äéáíýèçêå åîáéôßáò ôïõ åìðïäßïõ.
        newRay := VectorSubtract(newPosition, collisionPosition);

        //Õðïëïãéóìüò ôçò íÝáò ä/íóçò áí ÷ôõðçóåé óå êÜðïéï åìðüäéï.
        newDirection := VectorCrossProduct(intNormal, VectorCrossProduct(newRay, intNormal));
        if VectorNorm(NewDirection) > 0 then
          NormalizeVector(newDirection);

        //õðïëïãéóìüò ôçò áðüóôáóçò ðïõ ðñÝðåé íá êõëßóåé (åîáñôÜôáé áðï ôï collision plane êáé ôï collision ray)
        SlideDistance := vectorDotProduct(newRay, newDirection);

        //õðïëïãéóìüò ôñéâÞò êáôÜ ôçí êßíçóç ìå ôï åìðüäéï. (äåí åßíáé óùóôü öõóéêÜ)
        //      if abs(SlideDistance) < 10 * deltaTime then SlideDistance := 0;
        ScaleVector(newDirection, SlideDistance);

        //õðïëïãéóìüò ôçò íÝáò èÝóçò óôçí ïðïßá êáôåõèýíåôáé ç óöáßñá.
        newPosition := VectorAdd(collisionPosition, newDirection);
        ray := newDirection;
        vel := VectorLength(ray);

        if i = 6 then
        begin
          newPosition := aoldPosition;
          break;
        end;

        //åëÝã÷ù ãéá ðïëý ìéêñÝò êéíÞóåéò (ð÷. üôáí êïëÞóåé óå ìéÜ ãùíßá)
        if vel < 1E-10 then
        begin
          newPosition := aoldPosition;
          break;
        end;

      end
      else //äåí Ýãéíå collision ïðüôå ôåñìáôßæù ôï loop.
      begin
        if i = 0 then
          Result := False;
        Break;
      end;
    end; //ôÝëïò i loop
 aVelocity := Ray; //ç ä/íóç ôçò íÝáò ôá÷ýôçôáò.
end;

// TWorldCollisionSteer.ApplySteerForce

procedure TWorldCollisionSteer.ApplySteerForce;
var
  vDesiredDirection, vDistance, newPosition: TVector;
  lDistance: single;
begin
  FCollided := False;
  if not Assigned(FMap) then
    exit;

  newPosition := FVehicle.OwnerBaseSceneObject.Position.AsVector;

  FCollided := SphereSweepAndSlide(FMap, oldPosition, velocity, newPosition, FVehicle.OwnerBaseSceneObject.boundingSphereRadius + 2.3);

  oldPosition := newPosition;

  if FCollided then
    with FVehicle do
    begin
      vDesiredDirection := VectorNormalize(VectorSubtract(OwnerBaseSceneObject.Position.AsVector, newPosition));

      vDistance := VectorSubtract(OwnerBaseSceneObject.Direction.AsVector, vDesiredDirection);
      lDistance := VectorLength(vDistance);

      //Ïôáí ãßíåôáé collision áöáéñþ 5% áðï ôçí ôá÷ýôçôá ôçò óöáßñáò.
      Speed := Speed * 0.9;
      FAccumulator.AddScaledVector(10 * FTurnRate * VectorLength(VectorSubtract(newPosition, FVehicle.OwnerBaseSceneObject.Position.AsVector)),
        VectorNormalize(VectorSubtract(newPosition, FVehicle.OwnerBaseSceneObject.Position.AsVector)));
    end;


  //  if FCollided then begin
  //    FVehicle.FAccumulator.AddScaledVector(4, VectorNormalize(VectorSubtract(newPosition, FVehicle.OwnerBaseSceneObject.Position.AsVector)));
  //    FVehicle.Speed := FVehicle.Speed * 0.95;
  //  end;

end;

// TWorldCollisionSteer.Create

constructor TWorldCollisionSteer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMap := nil;
  velocity := NullHmgVector;
  FTurnRate := 0.3;
end;

// TWorldCollisionSteer.Notification

procedure TWorldCollisionSteer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FMap) then
  begin
    AComponent.RemoveFreeNotification(Self);
    FMap := nil;
  end
  else
    inherited;
end;

// TWorldCollisionSteer.SetMap

procedure TWorldCollisionSteer.SetMap(const Value: TGLFreeForm);
begin
  if Assigned(FMap) then
    FMap.RemoveFreeNotification(Self);

  FMap := Value;

  if Assigned(FMap) and (FMap <> nil) then
    FMap.FreeNotification(Self);
end;

// TGLVehicleManager.SetWorldCollisionMap

procedure TGLVehicleManager.SetWorldCollisionMap(const Value: TGLFreeForm);
begin
  if Assigned(FWorldCollisionMap) then
  begin
    FWorldCollisionMap.RemoveFreeNotification(Self);
    FWorldCollisionMap := nil;
  end;

  FWorldCollisionMap := Value;

  if FWorldCollisionMap <> nil then
    FWorldCollisionMap.FreeNotification(Self);
end;

procedure TWorldCollisionSteer.SetVehicle(const AValue: TGLBVehicle);
begin
  inherited;
  oldPosition := FVehicle.OwnerBaseSceneObject.Position.AsVector;
end;

initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  // class registrations
  RegisterXCollectionItemClass(TGLBVehicle);

end.
