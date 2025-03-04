//
// The graphics engine GLXEngine. The unit of LZScene for Lazarus
//
{
   Classes and functions that make integration between verlets and glscene
   objects easy.
}
unit GLVerletClasses;

interface

uses
  GLVerletTypes, 
  GLVectorGeometry, 
  GLScene, 
  GLObjects, 
  GLVectorTypes;

type
  { Specialized verlet node that can be anchored to a GLScene object. If it's
     anchored and has the property "NailedDown" set, it will remain in the same
     relative position to the GLScene object.}
  TGLVerletNode = class(TVerletNode)
  private
    FRelativePosition: TAffineVector;
    FGLBaseSceneObject: TGLBaseSceneObject;
    procedure SetGLBaseSceneObject(const Value: TGLBaseSceneObject);
  protected
    procedure SetLocation(const Value: TAffineVector);override;
  public
    procedure Verlet(const vpt : TVerletProgressTimes); override;

    property GLBaseSceneObject : TGLBaseSceneObject read FGLBaseSceneObject write SetGLBaseSceneObject;
    property RelativePosition : TAffineVector read FRelativePosition write FRelativePosition;
  end;

  function CreateVCPlaneFromGLPlane(Plane : TGLPlane; VerletWorld : TGLVerletWorld; Offset : single) : TVCFloor;
//-------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------
implementation
//-------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------

function CreateVCPlaneFromGLPlane(Plane : TGLPlane; VerletWorld : TGLVerletWorld; Offset : single) : TVCFloor;
begin
  result := TVCFloor.Create(VerletWorld);
  with result do
  begin
    Normal := VectorNormalize(Plane.Direction.AsAffineVector);

    Location := VectorAdd(Plane.Position.AsAffineVector, VectorScale(Normal, Offset));
  end;
end;

{ TGLVerletNode }

procedure TGLVerletNode.SetGLBaseSceneObject(
  const Value: TGLBaseSceneObject);
begin
  FGLBaseSceneObject := Value;

  if Assigned(GLBaseSceneObject) and NailedDown then
    FRelativePosition := AffineVectorMake(GLBaseSceneObject.AbsoluteToLocal(VectorMake(FLocation, 1)));
end;

procedure TGLVerletNode.SetLocation(const Value: TAffineVector);
begin
  inherited;
  if Assigned(GLBaseSceneObject) and NailedDown then
    FRelativePosition := GLBaseSceneObject.AbsoluteToLocal(Value);
end;

procedure TGLVerletNode.Verlet(const vpt : TVerletProgressTimes);
begin
  if Assigned(GLBaseSceneObject) and NailedDown then
  begin
    FLocation := GLBaseSceneObject.LocalToAbsolute(FRelativePosition);
  end else
    inherited;
end;
end.
