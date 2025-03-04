//
// The graphics engine GLXEngine. The unit of LZScene for Lazarus
//
{
   Implements projected textures through a GLScene object.
}
unit GLProjectedTextures;

interface

{$I GLScene.inc}

uses
  Classes,
  GLScene,
  GLTexture,
  OpenGLTokens,
  GLVectorGeometry,
  XOpenGL,
  GLRenderContextInfo,
  GLState;

type
  { Possible styles of texture projection. Possible values: 
      ptsOriginal: Original projection method (first pass,
         is default scene render, second pass is texture
         projection).
      ptsInverse: Inverse projection method (first pass
         is texture projection, sencond pass is regular scene
         render). This method is useful if you want to simulate
         lighting only through projected textures (the textures
         of the scene are "masked" into the white areas of
         the projection textures).
       }
  TGLProjectedTexturesStyle = (ptsOriginal, ptsInverse);

  TGLProjectedTextures = class;

  // TGLTextureEmmiter
  //
  { A projected texture emmiter.
     It's material property will be used as the projected texture.
     Can be places anywhere in the scene. }
  TGLTextureEmitter = class(TGLSceneObject)
  private
     
    FFOVy: single;
    FAspect: single;

  protected
     
    { Sets up the base texture matrix for this emitter
       Should be called whenever a change on its properties is made.}
    procedure SetupTexMatrix(var ARci: TGLRenderContextInfo);

  public
     
    constructor Create(AOwner: TComponent); override;

  published
     
    { Indicates the field-of-view of the projection frustum.}
    property FOVy: single read FFOVy write FFOVy;

    { x/y ratio. For no distortion, this should be set to
       texture.width/texture.height.}
    property Aspect: single read FAspect write FAspect;
  end;

  // TGLTextureEmitterItem
  //
  { Specifies an item on the TGLTextureEmitters collection. }
  TGLTextureEmitterItem = class(TCollectionItem)
  private
     
    FEmitter: TGLTextureEmitter;

  protected
     
    procedure SetEmitter(const val: TGLTextureEmitter);
    procedure RemoveNotification(aComponent: TComponent);
    function GetDisplayName: string; override;

  public
     
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;

  published
     
    property Emitter: TGLTextureEmitter read FEmitter write SetEmitter;

  end;

  // TGLTextureEmitters
  //
  { Collection of TGLTextureEmitter. }
  TGLTextureEmitters = class(TCollection)
  private
     
    FOwner: TGLProjectedTextures;

  protected
     
    function GetOwner: TPersistent; override;
    function GetItems(index: Integer): TGLTextureEmitterItem;
    procedure RemoveNotification(aComponent: TComponent);

  public
     
    procedure AddEmitter(texEmitter: TGLTextureEmitter);

    property Items[index: Integer]: TGLTextureEmitterItem read GetItems; default;

  end;

  // TGLProjectedTexture
  //
  { Projected Textures Manager.
     Specifies active texture Emitters (whose texture will be projected)
     and receivers (children of this object). }
  TGLProjectedTextures = class(TGLImmaterialSceneObject)
  private
     
    FEmitters: TGLTextureEmitters;
    FStyle: TGLProjectedTexturesStyle;

  public
     
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;

  published
     

    { List of texture emitters. }
    property Emitters: TGLTextureEmitters read FEmitters write FEmitters;

    { Indicates the style of the projected textures. }
    property Style: TGLProjectedTexturesStyle read FStyle write FStyle;
  end;

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
uses
  GLContext;
// ------------------
// ------------------ TGLTextureEmitter ------------------
// ------------------

// Create
//

constructor TGLTextureEmitter.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFOVy := 90;
  FAspect := 1;
end;

// SetupTexMatrix
//

procedure TGLTextureEmitter.SetupTexMatrix(var ARci: TGLRenderContextInfo);
const
  cBaseMat: TMatrix =
  (V:((X:0.5; Y:0;   Z:0; W:0),
          (X:0;   Y:0.5; Z:0; W:0),
          (X:0;   Y:0; Z:1; W:0),
          (X:0.5; Y:0.5; Z:0; W:1)));

var
  PM: TMatrix;
begin
  // Set the projector's "perspective" (i.e. the "spotlight cone"):.
  PM := MatrixMultiply(CreatePerspectiveMatrix(FFOVy, FAspect, 0.1, 1), cBaseMat);
  PM := MatrixMultiply(invAbsoluteMatrix, PM);
  Arci.GLStates.SetGLTextureMatrix(PM);
end;

// ------------------
// ------------------ TGLTextureEmitterItem ------------------
// ------------------

// Create
//

constructor TGLTextureEmitterItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

 
//

procedure TGLTextureEmitterItem.Assign(Source: TPersistent);
begin
  if Source is TGLTextureEmitterItem then
  begin
    FEmitter := TGLTextureEmitterItem(Source).FEmitter;
    TGLProjectedTextures(TGLTextureEmitters(Collection).GetOwner).StructureChanged;
  end;
  inherited;
end;

// SetCaster
//

procedure TGLTextureEmitterItem.SetEmitter(const val: TGLTextureEmitter);
begin
  if FEmitter <> val then
  begin
    FEmitter := val;
    TGLProjectedTextures(TGLTextureEmitters(Collection).GetOwner).StructureChanged;
  end;
end;

// RemoveNotification
//

procedure TGLTextureEmitterItem.RemoveNotification(aComponent: TComponent);
begin
  if aComponent = FEmitter then
    FEmitter := nil;
end;

// GetDisplayName
//

function TGLTextureEmitterItem.GetDisplayName: string;
begin
  if Assigned(FEmitter) then
  begin
    Result := '[TexEmitter] ' + FEmitter.Name;
  end
  else
    Result := 'nil';
end;

// ------------------
// ------------------ TGLTextureEmitters ------------------
// ------------------

// GetOwner
//

function TGLTextureEmitters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// GetItems
//

function TGLTextureEmitters.GetItems(index: Integer): TGLTextureEmitterItem;
begin
  Result := TGLTextureEmitterItem(inherited Items[index]);
end;

// RemoveNotification
//

procedure TGLTextureEmitters.RemoveNotification(aComponent: TComponent);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].RemoveNotification(aComponent);
end;

// AddEmitter
//

procedure TGLTextureEmitters.AddEmitter(texEmitter: TGLTextureEmitter);
var
  item: TGLTextureEmitterItem;
begin
  item := TGLTextureEmitterItem(self.Add);
  item.Emitter := texEmitter;
end;

// ------------------
// ------------------ TGLProjectedTextures ------------------
// ------------------

// Create
//

constructor TGLProjectedTextures.Create(AOwner: TComponent);
begin
  inherited Create(aOWner);
  FEmitters := TGLTextureEmitters.Create(TGLTextureEmitterItem);
  FEmitters.FOwner := self;
end;

// Destroy
//

destructor TGLProjectedTextures.Destroy;
begin
  FEmitters.Free;
  inherited destroy;
end;

// DoRender
//

procedure TGLProjectedTextures.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: boolean);
const
  PS: array[0..3] of GLfloat = (1, 0, 0, 0);
  PT: array[0..3] of GLfloat = (0, 1, 0, 0);
  PR: array[0..3] of GLfloat = (0, 0, 1, 0);
  PQ: array[0..3] of GLfloat = (0, 0, 0, 1);
var
  i: integer;
  emitter: TGLTextureEmitter;
begin
  if not (ARenderSelf or ARenderChildren) then
    Exit;
  if (csDesigning in ComponentState) then
  begin
    inherited;
    Exit;
  end;

  //First pass of original style: render regular scene
  if Style = ptsOriginal then
    self.RenderChildren(0, Count - 1, ARci);

  //generate planes
  GL.TexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
  GL.TexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
  GL.TexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
  GL.TexGeni(GL_Q, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);

  GL.TexGenfv(GL_S, GL_EYE_PLANE, @PS);
  GL.TexGenfv(GL_T, GL_EYE_PLANE, @PT);
  GL.TexGenfv(GL_R, GL_EYE_PLANE, @PR);
  GL.TexGenfv(GL_Q, GL_EYE_PLANE, @PQ);

  //options
  Arci.GLStates.Disable(stLighting);
  Arci.GLStates.DepthFunc := cfLEqual;
  Arci.GLStates.Enable(stBlend);
  GL.Enable(GL_TEXTURE_GEN_S);
  GL.Enable(GL_TEXTURE_GEN_T);
  GL.Enable(GL_TEXTURE_GEN_R);
  GL.Enable(GL_TEXTURE_GEN_Q);

  //second pass (original) first pass (inverse): for each emiter,
  //render projecting the texture summing all emitters
  for i := 0 to Emitters.Count - 1 do
  begin
    emitter := Emitters[i].Emitter;
    if not assigned(emitter) then
      continue;
    if not emitter.Visible then
      continue;

    emitter.Material.Apply(ARci);

    ARci.GLStates.Enable(stBlend);
    if Style = ptsOriginal then
    begin
      //on the original style, render blending the textures
      if emitter.Material.Texture.TextureMode <> tmBlend then
        ARci.GLStates.SetBlendFunc(bfDstColor, bfOne)
      else
        ARci.GLStates.SetBlendFunc(bfDstColor, bfZero);
    end
    else
    begin
      //on inverse style: the first texture projector should
      //be a regular rendering (i.e. no blending). All others
      //are "added" together creating an "illumination mask"
      if i = 0 then
        Arci.GLStates.SetBlendFunc(bfOne, bfZero)
      else
        ARci.GLStates.SetBlendFunc(bfOne, bfOne);
    end;

    //get this emitter's tex matrix
    emitter.SetupTexMatrix(ARci);
    repeat
      ARci.ignoreMaterials := true;
      Self.RenderChildren(0, Count - 1, ARci);
      ARci.ignoreMaterials := false;
    until not emitter.Material.UnApply(ARci);
  end;

  // LoseTexMatrix
  ARci.GLStates.SetBlendFunc(bfOne, bfZero);
  GL.Disable(GL_TEXTURE_GEN_S);
  GL.Disable(GL_TEXTURE_GEN_T);
  GL.Disable(GL_TEXTURE_GEN_R);
  GL.Disable(GL_TEXTURE_GEN_Q);

  GL.MatrixMode(GL_TEXTURE);
  GL.LoadIdentity;
  GL.MatrixMode(GL_MODELVIEW);

  ARci.GLStates.DepthFunc := cfLEqual;

  //second pass (inverse): render regular scene, blending it
  //with the "mask"
  if Style = ptsInverse then
  begin

    Arci.GLStates.Enable(stBlend);
    ARci.GLStates.SetBlendFunc(bfDstColor, bfSrcColor);

    //second pass: render everything, blending with what is
    //already there
    ARci.ignoreBlendingRequests := true;
    self.RenderChildren(0, Count - 1, ARci);
    ARci.ignoreBlendingRequests := false;

  end;
end;

initialization

  RegisterClasses([TGLTextureEmitter, TGLProjectedTextures]);

end.

