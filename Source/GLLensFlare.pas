
// This unit is part of the GLScene Project, http://glscene.org

{
   Lens flare object.

  History :  
       10/11/12 - PW - Added CPP compatibility: changed vector arrays to records
       23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
       22/04/10 - Yar - Fixes after GLState revision
       05/03/10 - DanB - More state added to TGLStateCache
       13/03/09 - DanB - changed glReadPixels/glTexImage2D calls to glCopyTexImage2D
       10/10/08 - DanB - changed Lensflare buildlists to use rci.cameraPosition instead
                            of Scene.CurrentGLCamera.DistanceTo
       08/08/07 - Lin - Bugfix for AutoZTest:
                           Lensflare is no longer occluded by objects BEHIND the flare.
       06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
       30/03/07 - DaStr - Moved all UNSAFE_TYPE, UNSAFE_CODE checks to GLSCene.inc
       25/03/07 - DaStr - UNSAFE_TYPE and UNSAFE_CODE warnings are now ignored
       23/03/07 - DaStr - Added missing parameters in procedure's implementation
                             (thanks Burkhard Carstens) (Bugtracker ID = 1681409)
       22/03/07 - DaStr - Cleanup after previous fix - now object does not
                             igore its children in picking state
                             Removed "unsafe type/unsafe code" warnings
       15/03/07 - DaStr - Removed flicker that occured when LensFlare was
                             rendered in a picking state (BugTracker ID = 1681031)
       19/04/04 - EG - Fixed occlusion test and pojection matrix stack issues
       16/04/04 - EG - Added StreakAngle
       15/04/04 - EG - Texture-based Lens-flare moved to GLTexLensFlare,
                          replaced gradient arrays with design-time editable colors
       25/09/03 - EG - Increased occlusion testing robustness
       20/09/03 - EG - Can now use occlusion testing/query for AutoZTest
       19/09/03 - EG - Misc. cleanup, added PreRender
       18/08/03 - SG - Added TGLTextureLensFlare (Tobias Peirick)
       26/03/03 - EG - Framerate independant glow transitions (Tobias Peirick)
       08/12/02 - EG - Added AutoZTest
       29/10/02 - EG - Initial, added defaults and encapsulation,
                          fixed positionning, RandSeed now preserved,
                          minor speedup
  

   Author  : Tobias Peirick 
   eMail   : peirick@onlinehome.de 
   Homepage: http://www.TobSoft.de
}
Unit GLLensFlare;

Interface

{$I GLScene.inc}

Uses
  Classes, SysUtils,
  GLScene, GLVectorGeometry, GLObjects, OpenGLTokens,
  GLContext, GLColor, GLBaseClasses, GLRenderContextInfo, GLState,
  GLVectorTypes, GLUtils, GLTextureFormat, GLRandomGenerator;

Type

  // TFlareElement

  TFlareElement  = (feGlow, feRing, feStreaks, feRays, feSecondaries);
  TFlareElements = Set Of TFlareElement;

  { The actual gradients between two colors are, of course, calculated by OpenGL.
     The start and end colors of a gradient are stored to represent the color of
     lens flare elements. }
  TGLFlareGradient = Class(TGLUpdateAbleObject)
  Private

    FFromColor: TGLColor;
    FToColor:   TGLColor;

  Protected

    Procedure SetFromColor(Const val: TGLColor);
    Procedure SetToColor(Const val: TGLColor);

  Public

    Constructor Create(AOwner: TPersistent); Override;
    Constructor CreateInitialized(AOwner: TPersistent; Const fromColor, toColor: TColorVector);
    Destructor Destroy; Override;
    Procedure Assign(Source: TPersistent); Override;

  Published

    Property FromColor: TGLColor read FFromColor write SetFromColor;
    Property ToColor: TGLColor read FToColor write SetToColor;
  End;

Const
  cDefaultFlareElements = [feGlow, feRing, feStreaks, feRays, feSecondaries];

Type

  // TGLLensFlare

  TGLLensFlare = Class(TGLBaseSceneObject)
  Private

    FSize:      Integer;
    FDeltaTime: Single;
    FCurrSize:  Single;
    FSeed:      Integer;
    FSqueeze:   Single;
    FNumStreaks: Integer;
    FStreakWidth, FStreakAngle: Single;
    FNumSecs:   Integer;
    FResolution: Integer;
    FAutoZTest: Boolean;
    FElements:  TFlareElements;
    FSin20Res, FCos20Res: Array Of Single;
    FSinRes, FCosRes: Array Of Single;
    FTexRays:   TGLTextureHandle;
    FFlareIsNotOccluded: Boolean;
    FOcclusionQuery: TGLOcclusionQueryHandle;
    FGlowGradient: TGLFlareGradient;
    FRingGradient: TGLFlareGradient;
    FStreaksGradient: TGLFlareGradient;
    FRaysGradient: TGLFlareGradient;
    FSecondariesGradient: TGLFlareGradient;
    FDynamic:   Boolean;
    FPreRenderPoint: TGLRenderPoint;
    FCustomRNG : TGLRandomNumGenerator;
  Protected

    Procedure SetGlowGradient(Const val: TGLFlareGradient);
    Procedure SetRingGradient(Const val: TGLFlareGradient);
    Procedure SetStreaksGradient(Const val: TGLFlareGradient);
    Procedure SetRaysGradient(Const val: TGLFlareGradient);
    Procedure SetSecondariesGradient(Const val: TGLFlareGradient);
    Procedure SetSize(aValue: Integer);
    Procedure SetSeed(aValue: Integer);
    Procedure SetSqueeze(aValue: Single);
    Function StoreSqueeze: Boolean;
    Procedure SetNumStreaks(aValue: Integer);
    Procedure SetStreakWidth(aValue: Single);
    Function StoreStreakWidth: Boolean;
    Procedure SetStreakAngle(aValue: Single);
    Procedure SetNumSecs(aValue: Integer);
    Procedure SetResolution(aValue: Integer);
    Procedure SetAutoZTest(aValue: Boolean);
    Procedure SetElements(aValue: TFlareElements);
    Procedure SetDynamic(aValue: Boolean);
    Procedure SetPreRenderPoint(Const val: TGLRenderPoint);
    Procedure PreRenderEvent(Sender: TObject; Var rci: TGLRenderContextInfo);
    Procedure PreRenderPointFreed(Sender: TObject);

    // These are quite unusual in that they don't use an RCI, since
    // PreRender is done before proper rendering starts, but we do know
    // which RC is being used, so we can use this state cache
    Procedure SetupRenderingOptions(StateCache: TGLStateCache);

    Procedure RenderRays(StateCache: TGLStateCache; Const size: Single);
    Procedure RenderStreaks(StateCache: TGLStateCache);
    Procedure RenderRing;
    Procedure RenderSecondaries(Const posVector: TAffineVector);

  Public

    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure Notification(AComponent: TComponent; Operation: TOperation);
      Override;

    Procedure BuildList(Var rci: TGLRenderContextInfo); Override;
    Procedure DoProgress(Const progressTime: TProgressTimes); Override;

    { Prepares pre-rendered texture to speed up actual rendering.
       Will use the currently active context as scratch space, and will
       automatically do nothing if things have already been prepared,
       thus you can invoke it systematically in a Viewer.BeforeRender
       event f.i. }
    Procedure PreRender(activeBuffer: TGLSceneBuffer);
    { Access to the Flare's current size.
       Flares decay or grow back over several frames, depending on their
       occlusion status, and this property allows to track or manually
       alter this instantaneous size. }
    Property FlareInstantaneousSize: Single read FCurrSize write FCurrSize;

  Published

    Property GlowGradient: TGLFlareGradient read FGlowGradient write SetGlowGradient;
    Property RingGradient: TGLFlareGradient read FRingGradient;
    Property StreaksGradient: TGLFlareGradient read FStreaksGradient;
    Property RaysGradient: TGLFlareGradient read FRaysGradient;
    Property SecondariesGradient: TGLFlareGradient read FSecondariesGradient;

    // MaxRadius of the flare.
    Property Size: Integer read FSize write SetSize Default 50;
    // GLTSRandom.Random seed
    Property Seed: Integer read FSeed write SetSeed;
    // To create elliptic flares.
    Property Squeeze: Single read FSqueeze write SetSqueeze Stored StoreSqueeze;
    // Number of streaks.
    Property NumStreaks: Integer read FNumStreaks write SetNumStreaks Default 4;
    // Width of the streaks.
    Property StreakWidth: Single read FStreakWidth write SetStreakWidth Stored StoreStreakWidth;
    // Angle of the streaks (in degrees)
    Property StreakAngle: Single read FStreakAngle write SetStreakAngle;
    // Number of secondary flares.
    Property NumSecs: Integer read FNumSecs write SetNumSecs Default 8;
    // Number of segments used when rendering circles.
    Property Resolution: Integer read FResolution write SetResolution Default 64;
    { Automatically computes FlareIsNotOccluded depending on ZBuffer test.
       Not that the automated test may use test result from the previous
       frame into the next (to avoid a rendering stall). }
    Property AutoZTest: Boolean read FAutoZTest write SetAutoZTest Default True;
    { Is the LensFlare not occluded?.
       If false the flare will fade away, if true, it will fade in and stay.
       This value is automatically updated if AutoZTest is set. }
    Property FlareIsNotOccluded: Boolean read FFlareIsNotOccluded write FFlareIsNotOccluded;
    // Which elements should be rendered?
    Property Elements: TFlareElements read FElements write SetElements Default cDefaultFlareElements;
    { Is the flare size adjusted dynamically?
       If true, the flare size will be grown and reduced over a few frames
       when it switches between occluded and non-occluded states. This
       requires animation to be active, but results in a smoother appearance.
       When false, flare will either be at full size or hidden.
       The flare is always considered non-dynamic at design-time. }
    Property Dynamic: Boolean read FDynamic write FDynamic Default True;

    { PreRender point for pre-rendered flare textures.
       See PreRender method for more details. }
    Property PreRenderPoint: TGLRenderPoint read FPreRenderPoint write SetPreRenderPoint;

    Property ObjectsSorting;
    Property Position;
    Property Visible;
    Property OnProgress;
    Property Behaviours;
    Property Effects;
  End;

Implementation

{%region%=====[ TGLFlareGradient ]==============================================}

Constructor TGLFlareGradient.Create(AOwner: TPersistent);
Begin
  Inherited;
  FFromColor := TGLColor.Create(Self);
  FToColor := TGLColor.Create(Self);
End;

Constructor TGLFlareGradient.CreateInitialized(AOwner: TPersistent; Const fromColor, toColor: TColorVector);
Begin
  Create(AOwner);
  FFromColor.Initialize(fromColor);
  FToColor.Initialize(toColor);
End;

Destructor TGLFlareGradient.Destroy;
Begin
  FToColor.Free;
  FFromColor.Free;
  Inherited;
End;

Procedure TGLFlareGradient.Assign(Source: TPersistent);
Begin
  If Source Is TGLFlareGradient Then
  Begin
    FromColor := TGLFlareGradient(Source).FromColor;
    ToColor := TGLFlareGradient(Source).ToColor;
  End;
  Inherited;
End;

Procedure TGLFlareGradient.SetFromColor(Const val: TGLColor);
Begin
  FFromColor.Assign(val);
End;

Procedure TGLFlareGradient.SetToColor(Const val: TGLColor);
Begin
  FToColor.Assign(val);
End;

{%endregion%}

Constructor TGLLensFlare.Create(AOwner: TComponent);
Begin
  Inherited;
  // Set default parameters:
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FSize := 50;
  FCustomRNG := TGLRandomNumGenerator.create;

  FSeed :=1465;// GettickCount64; // 1465;
 // FCustomRNG.RandSeed := FSeed;
  FSqueeze := 1;
  FNumStreaks := 4;
  FStreakWidth := 2;
  FNumSecs := 8;
  FAutoZTest := True;
  FlareIsNotOccluded := True;
  FDynamic := True;

  SetResolution(64);

  // Render all elements by default.
  FElements := [feGlow, feRing, feStreaks, feRays, feSecondaries];
  // Setup default gradients:
  FGlowGradient := TGLFlareGradient.CreateInitialized(Self, VectorMake(1, 1, 0.8, 0.3), VectorMake(1, 0.2, 0, 0));
  FRingGradient := TGLFlareGradient.CreateInitialized(Self, VectorMake(0.5, 0.2, 0, 0.1), VectorMake(0.5, 0.4, 0, 0.1));
  FStreaksGradient := TGLFlareGradient.CreateInitialized(Self, VectorMake(1, 1, 1, 0.2), VectorMake(0.2, 0, 1, 0));
  FRaysGradient := TGLFlareGradient.CreateInitialized(Self, VectorMake(1, 0.8, 0.5, 0.05), VectorMake(0.5, 0.2, 0, 0));
  FSecondariesGradient := TGLFlareGradient.CreateInitialized(Self, VectorMake(0, 0.2, 1, 0), VectorMake(0, 0.8, 0.2, 0.15));

  FTexRays := TGLTextureHandle.Create;
End;

Destructor TGLLensFlare.Destroy;
Begin
  PreRenderPoint := nil;
  FGlowGradient.Free;
  FRingGradient.Free;
  FStreaksGradient.Free;
  FRaysGradient.Free;
  FSecondariesGradient.Free;
  FOcclusionQuery.Free;
  FTexRays.Free;
  FreeAndNil(FCustomRNG);
  Inherited;
End;

Procedure TGLLensFlare.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  If (Operation = opRemove) And (AComponent = FPreRenderPoint) Then
    PreRenderPoint := nil;
  Inherited;
End;

Procedure TGLLensFlare.SetupRenderingOptions(StateCache: TGLStateCache);
Begin
  With StateCache Do
  Begin
    Disable(stLighting);
    Disable(stDepthTest);
    Disable(stFog);
    Disable(stColorMaterial);
    Disable(stCullFace);
    DepthWriteMask := False;
    Enable(stBlend);
    SetBlendFunc(bfSrcAlpha, bfOne);
    Disable(stAlphaTest);
    PolygonMode := pmFill;
  End;
End;

Procedure TGLLensFlare.RenderRays(StateCache: TGLStateCache; Const size: Single);
Var
  i:   Integer;
  rnd: Single;
  Rand : Single;
Begin
{$IFDEF GLS_OPENGL_DEBUG}
  If GL.GREMEDY_string_marker Then
    GL.StringMarkerGREMEDY(14, 'LensFlare.Rays');
{$ENDIF}

  With StateCache Do
  Begin
    LineWidth := 1;
    Disable(stLineSmooth);
    Disable(stLineStipple);
  End;


  GL.Begin_(GL_LINES);
  rnd := 0;
  For i := 0 To Resolution * 20 - 1 Do
 Begin
    rand := FCustomRNG.Random;// random; //TSRand.Random;  // 138867;
    If (i And 1) <> 0 Then
      rnd := 1.5 * rand * size
    Else
      rnd := rand * size;

    GL.Color4fv(RaysGradient.FromColor.AsAddress);
    GL.Vertex2f(0, 0);
    GL.Color4fv(RaysGradient.ToColor.AsAddress);
    GL.Vertex2f(rnd * FCos20Res[i], rnd * FSin20Res[i] * Squeeze);
  End;
  GL.End_;
End;

Procedure TGLLensFlare.RenderStreaks(StateCache: TGLStateCache);
Var
  i: Integer;
  a, f, s, c: Single;
Begin
{$IFDEF GLS_OPENGL_DEBUG}
  If GL.GREMEDY_string_marker Then
    GL.StringMarkerGREMEDY(17, 'LensFlare.Streaks');
{$ENDIF}
  StateCache.Enable(stLineSmooth);
  StateCache.LineWidth := StreakWidth;
  a := c2PI / NumStreaks;
  f := 1.5 * FCurrSize;
  GL.Begin_(GL_LINES);
  For i := 0 To NumStreaks - 1 Do
  Begin
    SinCos(StreakAngle * cPIdiv180 + a * i, f, s, c);
    GL.Color4fv(StreaksGradient.FromColor.AsAddress);
    GL.Vertex3fv(@NullVector);
    GL.Color4fv(StreaksGradient.ToColor.AsAddress);
    GL.Vertex2f(c, Squeeze * s);
  End;
  GL.End_;
  StateCache.Disable(stLineSmooth);
End;

Procedure TGLLensFlare.RenderRing;
Var
  i: Integer;
  rW, s0, c0, s, c: Single;
Begin
{$IFDEF GLS_OPENGL_DEBUG}
  If GL.GREMEDY_string_marker Then
    GL.StringMarkerGREMEDY(14, 'LensFlare.Ring');
{$ENDIF}
  rW := FCurrSize * (1 / 15); // Ring width
  GL.Begin_(GL_QUADS);
  s0 := 0;
  c0 := 0.6;
  For i := 0 To Resolution - 1 Do
  Begin
    s := s0;
    c := c0;
    s0 := FSinRes[i] * 0.6 * Squeeze;
    c0 := FCosRes[i] * 0.6;

    GL.Color4fv(GlowGradient.ToColor.AsAddress);
    GL.Vertex2f((FCurrSize - rW) * c, (FCurrSize - rW) * s);
    GL.Color4fv(RingGradient.FromColor.AsAddress);
    GL.Vertex2f(FCurrSize * c, Squeeze * FCurrSize * s);

    GL.Vertex2f(FCurrSize * c0, FCurrSize * s0);
    GL.Color4fv(GlowGradient.ToColor.AsAddress);
    GL.Vertex2f((FCurrSize - rW) * c0, (FCurrSize - rW) * s0);

    GL.Color4fv(RingGradient.FromColor.AsAddress);
    GL.Vertex2f(FCurrSize * c, FCurrSize * s);
    GL.Vertex2f(FCurrSize * c0, FCurrSize * s0);

    GL.Color4fv(GlowGradient.ToColor.AsAddress);
    GL.Vertex2f((FCurrSize + rW) * c0, (FCurrSize + rW) * s0);
    GL.Vertex2f((FCurrSize + rW) * c, (FCurrSize + rW) * s);
  End;
  GL.End_;
End;

Procedure TGLLensFlare.RenderSecondaries(Const posVector: TAffineVector);
Var
  i, j: Integer;
  rnd:  Single;
  v:    TAffineVector;
  grad: TGLFlareGradient;
Begin
{$IFDEF GLS_OPENGL_DEBUG}
  If GL.GREMEDY_string_marker Then
    GL.StringMarkerGREMEDY(21, 'LensFlare.Secondaries');
{$ENDIF}
  // Other secondaries (plain gradiented circles, like the glow):
  For j := 1 To NumSecs Do
  Begin
    rnd := 2 * FCustomRNG.Random - 1;
    // If rnd < 0 then the secondary glow will end up on the other side
    // of the origin. In this case, we can push it really far away from
    // the flare. If  the secondary is on the flare's side, we pull it
    // slightly towards the origin to avoid it winding up in the middle
    // of the flare.
    If rnd < 0 Then
      v := VectorScale(posVector, rnd)
    Else
      v := VectorScale(posVector, 0.8 * rnd);
    If j Mod 3 = 0 Then
      grad := GlowGradient
    Else
      grad := SecondariesGradient;
    rnd := (FCustomRNG.Random + 0.1) * FCurrSize * 0.25;

    GL.Begin_(GL_TRIANGLE_FAN);
    GL.Color4fv(grad.FromColor.AsAddress);
    GL.Vertex2f(v.V[0], v.V[1]);
    GL.Color4fv(grad.ToColor.AsAddress);
    For i := 0 To Resolution - 1 Do
      GL.Vertex2f(FCosRes[i] * rnd + v.V[0], FSinRes[i] * rnd + v.V[1]);
    GL.End_;
  End;
End;

// BuildList


Procedure TGLLensFlare.BuildList(Var rci: TGLRenderContextInfo);
Var
  i: Integer;
  depth, dist: Single;
  posVector, v, rv: TAffineVector;
  screenPos: TAffineVector;
  flareInViewPort, dynamicSize: Boolean;
  oldSeed: Longword;
  projMatrix: TMatrix;
  CurrentBuffer: TGLSceneBuffer;
Begin
  If (rci.drawState = dsPicking) Then
  Begin
    If Count <> 0 Then
      Self.RenderChildren(0, Count - 1, rci);
    Exit;
  End;
  CurrentBuffer := TGLSceneBuffer(rci.buffer);

  SetVector(v, AbsolutePosition);
  // are we looking towards the flare?
  rv := VectorSubtract(v, PAffineVector(@rci.cameraPosition)^);
  If VectorDotProduct(rci.cameraDirection, rv) > 0 Then
  Begin
    // find out where it is on the screen.
    screenPos := CurrentBuffer.WorldToScreen(v);
    flareInViewPort := (screenPos.V[0] < rci.viewPortSize.cx) And (screenPos.V[0] >= 0)
                        And (screenPos.V[1] < rci.viewPortSize.cy) And (screenPos.V[1] >= 0);
  End
  Else
    flareInViewPort := False;

  dynamicSize := FDynamic And Not (csDesigning In ComponentState);
  If dynamicSize Then
  Begin
    // make the glow appear/disappear progressively
    If flareInViewPort And FlareIsNotOccluded Then
    Begin
      FCurrSize := FCurrSize + FDeltaTime * 10 * Size;
      If FCurrSize > Size Then
        FCurrSize := Size;
    End
    Else
    Begin
      FCurrSize := FCurrSize - FDeltaTime * 10 * Size;
      If FCurrSize < 0 Then
        FCurrSize := 0;
    End;
  End
  Else
  Begin
    If flareInViewPort And FlareIsNotOccluded Then
      FCurrSize := Size
    Else
      FCurrSize := 0;
  End;

  // Prepare matrices
 // rci.PipelineTransformation.Push;
 // rci.PipelineTransformation.ProjectionMatrix:=CurrentBuffer.BaseProjectionMatrix;

  //.ModelMatrix := IdentityHmgMatrix;
  GL.PushMatrix;
  GL.LoadMatrixf(@CurrentBuffer.BaseProjectionMatrix);

 GL.MatrixMode(GL_PROJECTION);
  GL.PushMatrix;
 // rci.PipelineTransformation.Push;

  projMatrix := IdentityHmgMatrix;
  projMatrix.V[0].V[0] := 2 / rci.viewPortSize.cx;
  projMatrix.V[1].V[1] := 2 / rci.viewPortSize.cy;
  GL.LoadMatrixf(@projMatrix);
//  rci.PipelineTransformation.ProjectionMatrix := projMatrix;

  MakeVector(posVector,
    screenPos.V[0] - rci.viewPortSize.cx * 0.5,
    screenPos.V[1] - rci.viewPortSize.cy * 0.5,
    0);



  If AutoZTest Then
  Begin
    If dynamicSize And (GL.HP_occlusion_test Or TGLOcclusionQueryHandle.IsSupported) Then
    Begin
      // hardware-based occlusion test is possible
      FlareIsNotOccluded := True;

      rci.GLStates.SetColorMask([]);
      rci.GLStates.Disable(stAlphaTest);
      rci.GLStates.DepthWriteMask := False;
      rci.GLStates.Enable(stDepthTest);
      rci.GLStates.DepthFunc := cfLEqual;

      If TGLOcclusionQueryHandle.IsSupported Then
      Begin
        // preferred method, doesn't stall rendering too badly
        If Not Assigned(FOcclusionQuery) Then
          FOcclusionQuery := TGLOcclusionQueryHandle.Create;
        FOcclusionQuery.AllocateHandle;
        If FOcclusionQuery.IsDataNeedUpdate Then
          FOcclusionQuery.NotifyDataUpdated
        Else
          FlareIsNotOccluded := (FOcclusionQuery.PixelCount <> 0);
        FOcclusionQuery.BeginQuery;
      End
      Else
      Begin
        // occlusion_test, stalls rendering a bit
        GL.Enable(GL_OCCLUSION_TEST_HP);
      End;

      GL.Begin_(GL_QUADS);
      GL.Vertex3f(posVector.V[0] + 2, posVector.V[1], 1);
      GL.Vertex3f(posVector.V[0], posVector.V[1] + 2, 1);
      GL.Vertex3f(posVector.V[0] - 2, posVector.V[1], 1);
      GL.Vertex3f(posVector.V[0], posVector.V[1] - 2, 1);
      GL.End_;

      If TGLOcclusionQueryHandle.IsSupported Then
        FOcclusionQuery.EndQuery
      Else
      Begin
        GL.Disable(GL_OCCLUSION_TEST_HP);
        GL.GetBooleanv(GL_OCCLUSION_TEST_RESULT_HP, @FFlareIsNotOccluded);
      End;

      rci.GLStates.DepthFunc := cfLEqual;
      rci.GLStates.SetColorMask(cAllColorComponents);
    End
    Else
    Begin
      //Compares the distance to the lensflare, to the z-buffer depth.
      //This prevents the flare from being occluded by objects BEHIND the light.
      depth := CurrentBuffer.PixelToDistance(Round(ScreenPos.V[0]), Round(rci.viewPortSize.cy - ScreenPos.V[1]));
      dist := VectorDistance(rci.cameraPosition, self.AbsolutePosition);
      FlareIsNotOccluded := ((dist - depth) < 1);
    End;
  End;

  If FCurrSize >= 0 Then
  Begin

    // Random seed must be backed up, otherwise LensFlare rendering will be different at each frame)
      FCustomRNG.ResetSeed;
    //  oldSeed := RandSeed;
    //  RandSeed := Seed;

    SetupRenderingOptions(rci.GLStates);

    If [feGlow, feStreaks, feRays, feRing] * Elements <> [] Then
    Begin
      GL.Translatef(posVector.V[0], posVector.V[1], posVector.V[2]);

      // Glow (a circle with transparent edges):
      If feGlow In Elements Then
      Begin
        GL.Begin_(GL_TRIANGLE_FAN);
        GL.Color4fv(GlowGradient.FromColor.AsAddress);
        GL.Vertex2f(0, 0);
        GL.Color4fv(GlowGradient.ToColor.AsAddress);
        For i := 0 To Resolution - 1 Do
          GL.Vertex2f(FCurrSize * FCosRes[i],
            Squeeze * FCurrSize * FSinRes[i]);
        GL.End_;
      End;

      If feStreaks In Elements Then
        RenderStreaks(rci.GLStates);

      // Rays (GLTSRandom.Random-length lines from the origin):
      If feRays In Elements Then
      Begin
        If FTexRays.Handle <> 0 Then
        Begin
        {$IFDEF GLS_OPENGL_DEBUG}
          If GL.GREMEDY_string_marker Then
            GL.StringMarkerGREMEDY(19, 'LensFlare.RaysQuad');
        {$ENDIF}
          rci.GLStates.TextureBinding[0, ttTexture2D] := FTexRays.Handle;
          rci.GLStates.ActiveTextureEnabled[ttTexture2D] := True;
          GL.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

          GL.Begin_(GL_QUADS);
          GL.TexCoord2f(0, 0);
          GL.Vertex2f(-FCurrSize, -FCurrSize);
          GL.TexCoord2f(1, 0);
          GL.Vertex2f(FCurrSize, -FCurrSize);
          GL.TexCoord2f(1, 1);
          GL.Vertex2f(FCurrSize, FCurrSize);
          GL.TexCoord2f(0, 1);
          GL.Vertex2f(-FCurrSize, FCurrSize);
          GL.End_;

          rci.GLStates.ActiveTextureEnabled[ttTexture2D] := False;
        End
        Else
         RenderRays(rci.GLStates, FCurrSize);
      End;

      If feRing In Elements Then
        RenderRing;

      GL.LoadMatrixf(@projMatrix);
    End;

    If feSecondaries In Elements Then
      RenderSecondaries(posVector);

   // RandSeed := oldSeed;
  End;

  GL.PopMatrix;
 // rci.PipelineTransformation.Pop;

  GL.MatrixMode(GL_MODELVIEW);
  GL.PopMatrix;
//  rci.PipelineTransformation.Pop;

  If Count > 0 Then
    Self.RenderChildren(0, Count - 1, rci);
End;

// DoProgress


Procedure TGLLensFlare.DoProgress(Const progressTime: TProgressTimes);
Begin
  Inherited;
  FDeltaTime := progressTime.deltaTime;
End;

// PreRender


Procedure TGLLensFlare.PreRender(activeBuffer: TGLSceneBuffer);
Var
  i, texSize, maxSize: Integer;
  stateCache: TGLStateCache;
Begin
  If FTexRays.Handle <> 0 Then Exit;
  With activeBuffer.RenderingContext Do
  Begin
    stateCache := GLStates;
    PipelineTransformation.Push;
    PipelineTransformation.ProjectionMatrix := CreateOrthoMatrix(0, activeBuffer.Width, 0, activeBuffer.Height, -1, 1);
    PipelineTransformation.ViewMatrix := IdentityHmgMatrix;
  End;
  SetupRenderingOptions(stateCache);

  texSize := RoundUpToPowerOf2(Size);
  If texSize < Size * 1.5 Then
    texSize := texSize * 2;
  GL.GetIntegerv(GL_MAX_TEXTURE_SIZE, @maxSize);
  If texSize > maxSize Then
    texSize := maxSize;

  stateCache.Disable(stBlend);
  GL.Color4f(0, 0, 0, 0);
  GL.Begin_(GL_QUADS);
  GL.Vertex2f(0, 0);
  GL.Vertex2f(texSize + 4, 0);
  GL.Vertex2f(texSize + 4, texSize + 4);
  GL.Vertex2f(0, texSize + 4);
  GL.End_;
  stateCache.Enable(stBlend);

  GL.Translatef(texSize * 0.5 + 2, texSize * 0.5 + 2, 0);
  RenderRays(stateCache, texSize * 0.5);

  FTexRays.AllocateHandle;
  stateCache.TextureBinding[0, ttTexture2D] := FTexRays.Handle;
  If GL.EXT_texture_edge_clamp Then
    i := GL_CLAMP_TO_EDGE
  Else
    i := GL_CLAMP;
  GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, i);
  GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, i);
  GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  GL.CopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 2, 2, texSize, texSize, 0);

  activeBuffer.RenderingContext.PipelineTransformation.Pop;

  GL.CheckError;
End;

// SetGlowGradient


Procedure TGLLensFlare.SetGlowGradient(Const val: TGLFlareGradient);
Begin
  FGlowGradient.Assign(val);
  StructureChanged;
End;

// SetRingGradient


Procedure TGLLensFlare.SetRingGradient(Const val: TGLFlareGradient);
Begin
  FRingGradient.Assign(val);
  StructureChanged;
End;

// SetStreaksGradient


Procedure TGLLensFlare.SetStreaksGradient(Const val: TGLFlareGradient);
Begin
  FStreaksGradient.Assign(val);
  StructureChanged;
End;

// SetRaysGradient


Procedure TGLLensFlare.SetRaysGradient(Const val: TGLFlareGradient);
Begin
  FRaysGradient.Assign(val);
  StructureChanged;
End;

// SetSecondariesGradient


Procedure TGLLensFlare.SetSecondariesGradient(Const val: TGLFlareGradient);
Begin
  FSecondariesGradient.Assign(val);
  StructureChanged;
End;

// SetSize


Procedure TGLLensFlare.SetSize(aValue: Integer);
Begin
  FSize := aValue;
  StructureChanged;
End;

// SetSeed


Procedure TGLLensFlare.SetSeed(aValue: Integer);
Begin
  FSeed := aValue;
  StructureChanged;
End;

// SetSqueeze


Procedure TGLLensFlare.SetSqueeze(aValue: Single);
Begin
  FSqueeze := aValue;
  StructureChanged;
End;

// StoreSqueeze


Function TGLLensFlare.StoreSqueeze: Boolean;
Begin
  Result := (FSqueeze <> 1);
End;

// SetNumStreaks


Procedure TGLLensFlare.SetNumStreaks(aValue: Integer);
Begin
  FNumStreaks := aValue;
  StructureChanged;
End;

// SetStreakWidth


Procedure TGLLensFlare.SetStreakWidth(aValue: Single);
Begin
  FStreakWidth := aValue;
  StructureChanged;
End;

// StoreStreakWidth


Function TGLLensFlare.StoreStreakWidth: Boolean;
Begin
  Result := (FStreakWidth <> 2);
End;

// SetStreakAngle


Procedure TGLLensFlare.SetStreakAngle(aValue: Single);
Begin
  FStreakAngle := aValue;
  StructureChanged;
End;

// SetNumSecs


Procedure TGLLensFlare.SetNumSecs(aValue: Integer);
Begin
  FNumSecs := aValue;
  StructureChanged;
End;

// SetResolution


Procedure TGLLensFlare.SetResolution(aValue: Integer);
Begin
  If FResolution <> aValue Then
  Begin
    FResolution := aValue;
    StructureChanged;
    SetLength(FSin20Res, 20 * FResolution);
    SetLength(FCos20Res, 20 * FResolution);
    PrepareSinCosCache(FSin20Res, FCos20Res, 0, 360);
    SetLength(FSinRes, FResolution);
    SetLength(FCosRes, FResolution);
    PrepareSinCosCache(FSinRes, FCosRes, 0, 360);
  End;
End;

// SetAutoZTest


Procedure TGLLensFlare.SetAutoZTest(aValue: Boolean);
Begin
  If FAutoZTest <> aValue Then
  Begin
    FAutoZTest := aValue;
    StructureChanged;
  End;
End;

// SetElements


Procedure TGLLensFlare.SetElements(aValue: TFlareElements);
Begin
  If FElements <> aValue Then
  Begin
    FElements := aValue;
    StructureChanged;
  End;
End;

// SetDynamic


Procedure TGLLensFlare.SetDynamic(aValue: Boolean);
Begin
  If aValue <> FDynamic Then
  Begin
    FDynamic := aValue;
    NotifyChange(Self);
  End;
End;

// SetPreRenderPoint


Procedure TGLLensFlare.SetPreRenderPoint(Const val: TGLRenderPoint);
Begin
  If val <> FPreRenderPoint Then
  Begin
    If Assigned(FPreRenderPoint) Then
      FPreRenderPoint.UnRegisterCallBack(Self.PreRenderEvent);
    FPreRenderPoint := val;
    If Assigned(FPreRenderPoint) Then
      FPreRenderPoint.RegisterCallBack(Self.PreRenderEvent,
        Self.PreRenderPointFreed);
  End;
End;

// PreRenderEvent


Procedure TGLLensFlare.PreRenderEvent(Sender: TObject; Var rci: TGLRenderContextInfo);
Begin
  PreRender(rci.buffer As TGLSceneBuffer);
End;

// PreRenderPointFreed


Procedure TGLLensFlare.PreRenderPointFreed(Sender: TObject);
Begin
  FPreRenderPoint := nil;
End;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
Initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterClasses([TGLLensFlare]);

End.



















