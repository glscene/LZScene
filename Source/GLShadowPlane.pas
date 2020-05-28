//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Implements a basic shadow plane.

   It is strongly recommended to read and understand the explanations in the
   materials/mirror demo before using this component.

  History :  
       10/11/12 - PW - Added CPP compatibility: changed vector arrays to records
       23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
       22/06/10 - Yar - Fixes after GLState revision
       05/03/10 - DanB - More state added to TGLStateCache
       22/02/10 - Yar - Optimization of switching states
       12/03/09 - DanB - Bug-fix for scissor test on recent NVidia drivers
       06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
       30/03/07 - DaStr - Added $I GLScene.inc
       28/03/07 - DaStr - Renamed parameters in some methods
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678658)
       02/08/04 - LR, YHC - BCB corrections: use record instead array
       23/03/04 - EG - Added spoTransparent
       29/11/03 - EG - Scissors turned of if camera is withing bounding volume
       30/10/02 - EG - Added OnBegin/EndRenderingShadows
       25/10/02 - EG - Fixed Stencil cleanup and shadow projection bug
       02/10/02 - EG - Added spoScissor
       23/09/02 - EG - Creation (from GLMirror and Mattias FagerLund ShadowPlane.pas)
    
}
unit GLShadowPlane;

interface

{$I GLScene.inc}

uses
  Classes,
  GLScene,
  GLVectorGeometry,
  GLObjects,
  GLCrossPlatform,
  GLColor,
  GLRenderContextInfo,
  GLState,
  GLTextureFormat;

type

  // TShadowPlaneOptions
  //
  TShadowPlaneOption = (spoUseStencil, spoScissor, spoTransparent, spoIgnoreZ);
  TShadowPlaneOptions = set of TShadowPlaneOption;

const
  cDefaultShadowPlaneOptions = [spoUseStencil, spoScissor];

type

  // TGLShadowPlane
  //
  { A simple shadow plane.
     This mirror requires a stencil buffer for optimal rendering!
     The object is a mix between a plane and a proxy object, in that the plane
     defines where the shadows are cast, while the proxy part is used to reference
     the objects that should be shadowing (it is legal to self-shadow, but no
     self-shadow visuals will be rendered). 
     If stenciling isn't used, the shadow will 'paint' the ShadowColor instead
     of blending it transparently.
     You can have lower quality shadow geometry: add a dummycube, set it to
     invisible (so it won't be rendered in the "regular" pass), and under
     it place another visible dummycube under which you have all your
     low quality objects, use it as shadowing object. Apply the same movements
     to the low-quality objects that you apply to the visible, high-quality ones.
     }
  TGLShadowPlane = class(TGLPlane)
  private
     
    FRendering: Boolean;
    FShadowingObject: TGLBaseSceneObject;
    FShadowedLight: TGLLightSource;
    FShadowColor: TGLColor;
    FShadowOptions: TShadowPlaneOptions;
    FOnBeginRenderingShadows, FOnEndRenderingShadows: TNotifyEvent;

  protected
     
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetShadowingObject(const val: TGLBaseSceneObject);
    procedure SetShadowedLight(const val: TGLLightSource);
    procedure SetShadowColor(const val: TGLColor);
    procedure SetShadowOptions(const val: TShadowPlaneOptions);

  public
     
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;

    procedure Assign(Source: TPersistent); override;

  published
     
          { Selects the object to mirror.
             If nil, the whole scene is mirrored. }
    property ShadowingObject: TGLBaseSceneObject read FShadowingObject write SetShadowingObject;
    { The light which casts shadows.
       The light must be enabled otherwise shadows won't be cast. }
    property ShadowedLight: TGLLightSource read FShadowedLight write SetShadowedLight;
    { The shadow's color.
       This color is transparently blended to make shadowed area darker. }
    property ShadowColor: TGLColor read FShadowColor write SetShadowColor;

    { Controls rendering options.
        
        spoUseStencil: plane area is stenciled, prevents shadowing
          objects to be visible on the sides of the mirror (stencil buffer
          must be active in the viewer too). It also allows shadows to
          be partial (blended).
        spoScissor: plane area is 'scissored', this should improve
          rendering speed by limiting rendering operations and fill rate,
          may have adverse effects on old hardware in rare cases
        spoTransparent: does not render the plane's material, may help
          improve performance if you're fillrate limited, are using the
          stencil, and your hardware has optimized stencil-only writes
        
    }
    property ShadowOptions: TShadowPlaneOptions read FShadowOptions write SetShadowOptions default cDefaultShadowPlaneOptions;

    { Fired before the shadows are rendered. }
    property OnBeginRenderingShadows: TNotifyEvent read FOnBeginRenderingShadows write FOnBeginRenderingShadows;
    { Fired after the shadows are rendered. }
    property OnEndRenderingShadows: TNotifyEvent read FOnEndRenderingShadows write FOnEndRenderingShadows;
  end;

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
uses OpenGLTokens, GLContext;
// ------------------
// ------------------ TGLShadowPlane ------------------
// ------------------

// Create
//

constructor TGLShadowPlane.Create(AOwner: Tcomponent);
const
  cDefaultShadowColor: TColorVector = (X:0; Y:0; Z:0; W:0.5);
begin
  inherited Create(AOwner);
  FShadowOptions := cDefaultShadowPlaneOptions;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FShadowColor := TGLColor.CreateInitialized(Self, cDefaultShadowColor);
end;

// Destroy
//

destructor TGLShadowPlane.Destroy;
begin
  inherited;
  FShadowColor.Free;
end;

// DoRender
//

procedure TGLShadowPlane.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  oldProxySubObject, oldIgnoreMaterials: Boolean;
  shadowMat: TMatrix;
  sr: TGLRect;
  CurrentBuffer: TGLSceneBuffer;
  ModelMat: TMatrix;
begin
  if FRendering then
    Exit;
  FRendering := True;
  try
    with ARci.GLStates do
    begin
      oldProxySubObject := ARci.proxySubObject;
      ARci.proxySubObject := True;
      CurrentBuffer := TGLSceneBuffer(ARci.buffer);

      if ARenderSelf and (VectorDotProduct(VectorSubtract(ARci.cameraPosition, AbsolutePosition), AbsoluteDirection) > 0) then
      begin

        if (spoScissor in ShadowOptions)
          and (PointDistance(ARci.cameraPosition) > BoundingSphereRadius) then
        begin
          sr := ScreenRect(CurrentBuffer);
          InflateGLRect(sr, 1, 1);
          IntersectGLRect(sr, GLRect(0, 0, ARci.viewPortSize.cx, ARci.viewPortSize.cy));
          GL.Scissor(sr.Left, sr.Top, sr.Right - sr.Left, sr.Bottom - sr.Top);
          Enable(stScissorTest);
        end;

        if (spoUseStencil in ShadowOptions) then
        begin
          StencilClearValue := 0;
          GL.Clear(GL_STENCIL_BUFFER_BIT);
          Enable(stStencilTest);
          SetStencilFunc(cfAlways, 1, 1);
          SetStencilOp(soReplace, soReplace, soReplace);
        end;

        // "Render"  plane and stencil mask
        if (spoTransparent in ShadowOptions) then
        begin
          SetGLColorWriting(False);
          DepthWriteMask := False;
          BuildList(ARci);
          SetGLColorWriting(True);
        end
        else
        begin
          Material.Apply(ARci);
          repeat
            BuildList(ARci);
          until not Material.UnApply(ARci);
        end;

        // Setup depth options
        if spoIgnoreZ in ShadowOptions then
          Disable(stDepthTest)
        else
          Enable(stDepthTest);
        DepthFunc := cfLEqual;

        if Assigned(FShadowedLight) then
        begin

          ARci.PipelineTransformation.Push;

          case ShadowedLight.LightStyle of
            lsParallel:
              begin
                shadowMat := MakeShadowMatrix(AbsolutePosition, AbsoluteDirection,
                  VectorScale(ShadowedLight.SpotDirection.AsVector, 1e10));
              end;
          else
            shadowMat := MakeShadowMatrix(AbsolutePosition, AbsoluteDirection,
              ShadowedLight.AbsolutePosition);
          end;

          ARci.PipelineTransformation.ViewMatrix := MatrixMultiply(
            shadowMat,
            ARci.PipelineTransformation.ViewMatrix);
          ARci.PipelineTransformation.ModelMatrix := IdentityHmgMatrix;

          Disable(stCullFace);
          Enable(stNormalize);
          SetPolygonOffset(-1, -1);
          Enable(stPolygonOffsetFill);

          oldIgnoreMaterials := ARci.ignoreMaterials;
          ARci.ignoreMaterials := True;
          ActiveTextureEnabled[ttTexture2D] := False;
          Disable(stLighting);
          Disable(stFog);

          GL.Color4fv(ShadowColor.AsAddress);

          if (spoUseStencil in ShadowOptions) then
          begin
            Enable(stBlend);
            Disable(stAlphaTest);
            SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
            SetStencilFunc(cfEqual, 1, 1);
            SetStencilOp(soKeep, soKeep, soZero);
          end
          else
            Disable(stBlend);

          if Assigned(FOnBeginRenderingShadows) then
            FOnBeginRenderingShadows(Self);
          if Assigned(FShadowingObject) then
          begin
            ModelMat := IdentityHmgMatrix;
            if FShadowingObject.Parent <> nil then
              MatrixMultiply(ModelMat, FShadowingObject.Parent.AbsoluteMatrix, ModelMat);
            MatrixMultiply(ModelMat, FShadowingObject.LocalMatrix^, ModelMat);
            ARci.PipelineTransformation.ModelMatrix := ModelMat;
            FShadowingObject.DoRender(ARci, True, (FShadowingObject.Count > 0));
          end
          else
          begin
            Scene.Objects.DoRender(ARci, True, True);
          end;
          if Assigned(FOnEndRenderingShadows) then
            FOnEndRenderingShadows(Self);

          ARci.ignoreMaterials := oldIgnoreMaterials;

          // Restore to "normal"
          ARci.PipelineTransformation.Pop;

        end;
        Disable(stStencilTest);
        Disable(stScissorTest);
        Disable(stPolygonOffsetFill);
      end;

      ARci.proxySubObject := oldProxySubObject;

      if ARenderChildren and (Count > 0) then
        Self.RenderChildren(0, Count - 1, ARci);
    end;
  finally
    FRendering := False;
  end;
end;

// Notification
//

procedure TGLShadowPlane.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FShadowingObject then
      ShadowingObject := nil
    else if AComponent = FShadowedLight then
      ShadowedLight := nil;
  end;
  inherited;
end;

// SetShadowingObject
//

procedure TGLShadowPlane.SetShadowingObject(const val: TGLBaseSceneObject);
begin
  if FShadowingObject <> val then
  begin
    if Assigned(FShadowingObject) then
      FShadowingObject.RemoveFreeNotification(Self);
    FShadowingObject := val;
    if Assigned(FShadowingObject) then
      FShadowingObject.FreeNotification(Self);
    NotifyChange(Self);
  end;
end;

// SetShadowedLight
//

procedure TGLShadowPlane.SetShadowedLight(const val: TGLLightSource);
begin
  if FShadowedLight <> val then
  begin
    if Assigned(FShadowedLight) then
      FShadowedLight.RemoveFreeNotification(Self);
    FShadowedLight := val;
    if Assigned(FShadowedLight) then
      FShadowedLight.FreeNotification(Self);
    NotifyChange(Self);
  end;
end;

// SetShadowColor
//

procedure TGLShadowPlane.SetShadowColor(const val: TGLColor);
begin
  FShadowColor.Assign(val);
end;

 
//

procedure TGLShadowPlane.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLShadowPlane) then
  begin
    FShadowOptions := TGLShadowPlane(Source).FShadowOptions;
    ShadowingObject := TGLShadowPlane(Source).ShadowingObject;
    ShadowedLight := TGLShadowPlane(Source).ShadowedLight;
    ShadowColor := TGLShadowPlane(Source).ShadowColor;
  end;
  inherited Assign(Source);
end;

// SetShadowOptions
//

procedure TGLShadowPlane.SetShadowOptions(const val: TShadowPlaneOptions);
begin
  if FShadowOptions <> val then
  begin
    FShadowOptions := val;
    NotifyChange(Self);
  end;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  RegisterClasses([TGLShadowPlane]);

end.

