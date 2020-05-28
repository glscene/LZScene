//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Implements a basic, stencil-based mirror (as in Mark Kilgard's demo).

   It is strongly recommended to read and understand the explanations in the
   materials/mirror demo before using this component.

  History :  
       23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
       22/04/10 - Yar - Fixes after GLState revision
       05/03/10 - DanB - More state added to TGLStateCache
       15/12/08- Paul Robello - corrected call to  FOnEndRenderingMirrors
       06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
       30/03/07 - DaStr - Added $I GLScene.inc
       28/03/07 - DaStr - Renamed parameters in some methods
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678658)
       18/07/04 - Orlando - added custom shapes
       13/02/03 - DanB - added TGLMirror.AxisAlignedDimensionsUnscaled
       13/11/02 - EG - Fixed TGLMirror.DoRender transform
       06/11/02 - EG - Fixed Stencil setup
       30/10/02 - EG - Added OnBegin/EndRenderingMirrors
       25/10/02 - EG - Fixed Stencil cleanup
       22/02/01 - EG - Fixed change notification,
                          Fixed special effects support (PFX, etc.)
       07/12/01 - EG - Creation
    
}
unit GLMirror;

interface

{$I GLScene.inc}

uses
  Classes,
  GLScene, GLVectorGeometry, OpenGLAdapter, OpenGLTokens, GLContext,
  GLMaterial, GLColor, GLRenderContextInfo,
  GLState
  , GLVectorTypes;


type

  // TMirrorOptions
  //
  TMirrorOption = (moUseStencil, moOpaque, moMirrorPlaneClip, moClearZBuffer);
  TMirrorOptions = set of TMirrorOption;

const
  cDefaultMirrorOptions = [moUseStencil];

type

  // TMirrorShapes  ORL
  TMirrorShapes = (msRect, msDisk);

  // TGLMirror
  //
  { A simple plane mirror.
     This mirror requires a stencil buffer for optimal rendering!
     The object is a mix between a plane and a proxy object, in that the plane
     defines the mirror's surface, while the proxy part is used to reference
     the objects that should be mirrored (it is legal to self-mirror, but no
     self-mirror visuals will be rendered).
     It is strongly recommended to read and understand the explanations in the
     materials/mirror demo before using this component. }
  TGLMirror = class(TGLSceneObject)
  private
     
    FRendering: Boolean;
    FMirrorObject: TGLBaseSceneObject;
    FWidth, FHeight: TGLFloat;
    FMirrorOptions: TMirrorOptions;
    FOnBeginRenderingMirrors, FOnEndRenderingMirrors: TNotifyEvent;

    FShape: TMirrorShapes; //ORL
    FRadius: TGLFloat; //ORL
    FSlices: TGLInt; //ORL

  protected
     
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetMirrorObject(const val: TGLBaseSceneObject);
    procedure SetMirrorOptions(const val: TMirrorOptions);
    procedure ClearZBufferArea(aBuffer: TGLSceneBuffer);

    procedure SetHeight(AValue: TGLFloat);
    procedure SetWidth(AValue: TGLFloat);

    procedure SetRadius(const aValue: Single); //ORL
    procedure SetSlices(const aValue: TGLInt); //ORL
    procedure SetShape(aValue: TMirrorShapes); //ORL
    function GetRadius: single; //ORL
    function GetSlices: TGLInt; //ORL

  public
     
    constructor Create(AOwner: TComponent); override;

    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure BuildList(var ARci: TGLRenderContextInfo); override;

    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;

  published
     
          { Selects the object to mirror.
             If nil, the whole scene is mirrored. }
    property MirrorObject: TGLBaseSceneObject read FMirrorObject write
      SetMirrorObject;
    { Controls rendering options.
        
        moUseStencil: mirror area is stenciled, prevents reflected
          objects to be visible on the sides of the mirror (stencil buffer
          must be active in the viewer)
        moOpaque: mirror is opaque (ie. painted with background color)
        moMirrorPlaneClip: a ClipPlane is defined to prevent reflections
          from popping out of the mirror (for objects behind or halfway through)
        moClearZBuffer: mirror area's ZBuffer is cleared so that background
          objects don't interfere with reflected objects (reflected objects
          must be rendered AFTER the mirror in the hierarchy). Works only
          along with stenciling.
        
    }
    property MirrorOptions: TMirrorOptions read FMirrorOptions write
      SetMirrorOptions default cDefaultMirrorOptions;

    property Height: TGLFloat read FHeight write SetHeight;
    property Width: TGLFloat read FWidth write SetWidth;

    { Fired before the object's mirror images are rendered. }
    property OnBeginRenderingMirrors: TNotifyEvent read FOnBeginRenderingMirrors
      write FOnBeginRenderingMirrors;
    { Fired after the object's mirror images are rendered. }
    property OnEndRenderingMirrors: TNotifyEvent read FOnEndRenderingMirrors
      write FOnEndRenderingMirrors;

    property Radius: TGLFloat read FRadius write SetRadius; //ORL
    property Slices: TGLInt read FSlices write SetSlices default 16; //ORL
    property Shape: TMirrorShapes read FShape write SetShape default msRect;
    //ORL
  end;

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

// ------------------
// ------------------ TGLMirror ------------------
// ------------------

// Create
//

constructor TGLMirror.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FWidth := 1;
  FHeight := 1;
  FMirrorOptions := cDefaultMirrorOptions;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  Material.FrontProperties.Diffuse.Initialize(VectorMake(1, 1, 1, 0.1));
  Material.BlendingMode := bmTransparency;

  FRadius := 1; //ORL
  FSlices := 16; //ORL
  Shape := msRect; //ORL
end;

// DoRender
//

procedure TGLMirror.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  oldProxySubObject: Boolean;
  refMat, curMat, ModelMat: TMatrix;
  clipPlane: TDoubleHmgPlane;
  bgColor: TColorVector;
  cameraPosBackup, cameraDirectionBackup: TVector;
  CurrentBuffer: TGLSceneBuffer;
begin
  if FRendering then
    Exit;
  FRendering := True;
  try
    oldProxySubObject := ARci.proxySubObject;
    ARci.proxySubObject := True;
    CurrentBuffer := TGLSceneBuffer(ARci.buffer);

    if VectorDotProduct(VectorSubtract(ARci.cameraPosition, AbsolutePosition),
      AbsoluteDirection) > 0 then
      with ARci.GLStates do
      begin

        // "Render" stencil mask
        if MirrorOptions <> [] then
        begin
          if (moUseStencil in MirrorOptions) then
          begin
            Enable(stStencilTest);
            ARci.GLStates.StencilClearValue := 0;
            GL.Clear(GL_STENCIL_BUFFER_BIT);
            SetStencilFunc(cfAlways, 1, 1);
            SetStencilOp(soReplace, soZero, soReplace);
          end;
          if (moOpaque in MirrorOptions) then
          begin
            bgColor := ConvertWinColor(CurrentBuffer.BackgroundColor);
            ARci.GLStates.SetGLMaterialColors(cmFront, bgColor, clrBlack,
              clrBlack, clrBlack, 0);
          end
          else
            SetGLColorWriting(False);

          Enable(stDepthTest);
          DepthWriteMask := False;

          BuildList(ARci);

          DepthWriteMask := True;
          if (moUseStencil in MirrorOptions) then
          begin
            SetStencilFunc(cfEqual, 1, 1);
            SetStencilOp(soKeep, soKeep, soKeep);
          end;

          if (moClearZBuffer in MirrorOptions) then
            ClearZBufferArea(CurrentBuffer);

          if not (moOpaque in MirrorOptions) then
            SetGLColorWriting(True);
        end;

        ARci.PipelineTransformation.Push;
        ARci.PipelineTransformation.ModelMatrix := IdentityHmgMatrix;

        Disable(stCullFace);
        Enable(stNormalize);

        if moMirrorPlaneClip in MirrorOptions then
        begin
          GL.Enable(GL_CLIP_PLANE0);
          SetPlane(clipPlane, PlaneMake(AffineVectorMake(AbsolutePosition),
            VectorNegate(AffineVectorMake(AbsoluteDirection))));
          GL.ClipPlane(GL_CLIP_PLANE0, @clipPlane);
        end;

        // Mirror lights
        refMat := MakeReflectionMatrix(
          AffineVectorMake(AbsolutePosition),
          AffineVectorMake(AbsoluteDirection));
        curMat := MatrixMultiply(refMat, ARci.PipelineTransformation.ViewMatrix);
        ARci.PipelineTransformation.ViewMatrix := curMat;
        Scene.SetupLights(CurrentBuffer.LimitOf[limLights]);

        // mirror geometry and render master
        cameraPosBackup := ARci.cameraPosition;
        cameraDirectionBackup := ARci.cameraDirection;
        ARci.cameraPosition := VectorTransform(ARci.cameraPosition, refMat);
        ARci.cameraDirection := VectorTransform(ARci.cameraDirection, refMat);

        // temporary fix? (some objects don't respect culling options, or ?)
        CullFaceMode := cmFront;
        if Assigned(FOnBeginRenderingMirrors) then
          FOnBeginRenderingMirrors(Self);
        if Assigned(FMirrorObject) then
        begin
          ModelMat := IdentityHmgMatrix;
          if FMirrorObject.Parent <> nil then
            MatrixMultiply(ModelMat, FMirrorObject.Parent.AbsoluteMatrix, ModelMat);
          MatrixMultiply(ModelMat, FMirrorObject.LocalMatrix^, ModelMat);
          ARci.PipelineTransformation.ModelMatrix := ModelMat;
          FMirrorObject.DoRender(ARci, ARenderSelf, FMirrorObject.Count > 0);
        end
        else
        begin
          Scene.Objects.DoRender(ARci, ARenderSelf, True);
        end;
        if Assigned(FOnEndRenderingMirrors) then
          FOnEndRenderingMirrors(Self);

        // Restore to "normal"
        ARci.cameraPosition := cameraPosBackup;
        ARci.cameraDirection := cameraDirectionBackup;
        ARci.GLStates.CullFaceMode := cmBack;
        ARci.PipelineTransformation.ReplaceFromStack;
        Scene.SetupLights(CurrentBuffer.LimitOf[limLights]);
        ARci.PipelineTransformation.Pop;
        if moMirrorPlaneClip in MirrorOptions then
          GL.Disable(GL_CLIP_PLANE0);
        ARci.GLStates.Disable(stStencilTest);

        ARci.proxySubObject := oldProxySubObject;

        // start rendering self
        if ARenderSelf then
        begin
          Material.Apply(ARci);
          repeat
            BuildList(ARci);
          until not Material.UnApply(ARci);
        end;

      end;

    if ARenderChildren then
      Self.RenderChildren(0, Count - 1, ARci);

    if Assigned(FMirrorObject) then
      FMirrorObject.Effects.RenderPostEffects(ARci);
  finally
    FRendering := False;
  end;
end;

// BuildList
//

procedure TGLMirror.BuildList(var ARci: TGLRenderContextInfo);
var
  hw, hh: TGLFloat;
  quadric: PGLUquadricObj;
begin
  if msRect = FShape then
  begin
    hw := FWidth * 0.5;
    hh := FHeight * 0.5;
    GL.Normal3fv(@ZVector);
    GL.Begin_(GL_QUADS);
    GL.Vertex3f(hw, hh, 0);
    GL.Vertex3f(-hw, hh, 0);
    GL.Vertex3f(-hw, -hh, 0);
    GL.Vertex3f(hw, -hh, 0);
    GL.End_;
  end
  else
  begin
    quadric := gluNewQuadric;
    gluDisk(Quadric, 0, FRadius, FSlices, 1); //radius. slices, loops
  end;
end;

// BuildList
//

procedure TGLMirror.ClearZBufferArea(aBuffer: TGLSceneBuffer);
var
  worldMat: TMatrix;
  p: TAffineVector;
begin
  with aBuffer do
  begin
    GL.PushMatrix;
    worldMat := Self.AbsoluteMatrix;
    GL.MatrixMode(GL_PROJECTION);
    GL.PushMatrix;
    GL.LoadIdentity;
    GL.Ortho(0, Width, 0, Height, 1, -1);
    GL.MatrixMode(GL_MODELVIEW);
    GL.LoadIdentity;

    with aBuffer.RenderingContext.GLStates do
    begin
      DepthFunc := cfAlways;
      SetGLColorWriting(False);
    end;

    GL.Begin_(GL_QUADS);
    p := WorldToScreen(VectorTransform(AffineVectorMake(Self.Width * 0.5,
      Self.Height * 0.5, 0), worldMat));
    GL.Vertex3f(p.V[0], p.V[1], 0.999);
    p := WorldToScreen(VectorTransform(AffineVectorMake(-Self.Width * 0.5,
      Self.Height * 0.5, 0), worldMat));
    GL.Vertex3f(p.V[0], p.V[1], 0.999);
    p := WorldToScreen(VectorTransform(AffineVectorMake(-Self.Width * 0.5,
      -Self.Height * 0.5, 0), worldMat));
    GL.Vertex3f(p.V[0], p.V[1], 0.999);
    p := WorldToScreen(VectorTransform(AffineVectorMake(Self.Width * 0.5,
      -Self.Height * 0.5, 0), worldMat));
    GL.Vertex3f(p.V[0], p.V[1], 0.999);
    GL.End_;

    with aBuffer.RenderingContext.GLStates do
    begin
      DepthFunc := cfLess;
      SetGLColorWriting(True);
    end;

    GL.MatrixMode(GL_PROJECTION);
    GL.PopMatrix;
    GL.MatrixMode(GL_MODELVIEW);
    GL.PopMatrix;
  end;
end;

// Notification
//

procedure TGLMirror.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FMirrorObject) then
    MirrorObject := nil;
  inherited;
end;

// SetMirrorObject
//

procedure TGLMirror.SetMirrorObject(const val: TGLBaseSceneObject);
begin
  if FMirrorObject <> val then
  begin
    if Assigned(FMirrorObject) then
      FMirrorObject.RemoveFreeNotification(Self);
    FMirrorObject := val;
    if Assigned(FMirrorObject) then
      FMirrorObject.FreeNotification(Self);
    NotifyChange(Self);
  end;
end;

// SetWidth
//

procedure TGLMirror.SetWidth(AValue: TGLFloat);
begin
  if AValue <> FWidth then
  begin
    FWidth := AValue;
    NotifyChange(Self);
  end;
end;

// SetHeight
//

procedure TGLMirror.SetHeight(AValue: TGLFloat);
begin
  if AValue <> FHeight then
  begin
    FHeight := AValue;
    NotifyChange(Self);
  end;
end;

 
//

procedure TGLMirror.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLMirror) then
  begin
    FWidth := TGLMirror(Source).FWidth;
    FHeight := TGLMirror(Source).FHeight;
    FMirrorOptions := TGLMirror(Source).FMirrorOptions;
    MirrorObject := TGLMirror(Source).MirrorObject;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TGLMirror.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result := VectorMake(0.5 * Abs(FWidth),
    0.5 * Abs(FHeight), 0);
end;

// SetMirrorOptions
//

procedure TGLMirror.SetMirrorOptions(const val: TMirrorOptions);
begin
  if FMirrorOptions <> val then
  begin
    FMirrorOptions := val;
    NotifyChange(Self);
  end;
end;

//ORL add-ons

// SetRadius
//

procedure TGLMirror.SetRadius(const aValue: Single);
begin
  if aValue <> FRadius then
  begin
    FRadius := aValue;
    StructureChanged;
  end;
end;

// GetRadius
//

function TGLMirror.GetRadius: single;
begin
  result := FRadius;
end;

// SetSlices
//

procedure TGLMirror.SetSlices(const aValue: TGLInt);
begin
  if aValue <> FSlices then
  begin
    if aValue > 2 then
      FSlices := aValue;
    StructureChanged;
  end
  else
  begin
  end;
end;

// GetSlices
//

function TGLMirror.GetSlices: TGLInt;
begin
  result := FSlices;
end;

// SetShape
//

procedure TGLMirror.SetShape(aValue: TMirrorShapes);
begin
  if aValue <> FShape then
  begin
    FShape := aValue;
    StructureChanged;
  end;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  RegisterClasses([TGLMirror]);

end.

