//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Registration unit for GLScene library components, property editors and
      IDE experts for Lazarus.
}
unit GLSceneRegister;

interface

{$I GLScene.inc}

uses
  LResources, 
  Resource,
  Classes, 
  GLObjectManager, 
  ComponentEditors,
  PropEdits, 
  LCLType,
  LazIDEIntf, 
  ProjectIntf, 
  ProjectResourcesIntf, 
  MacroIntf,
  Forms;

type

  TGLLibMaterialNameProperty = class(TStringProperty)
  public
     
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

// Auto-create for object manager
function ObjectManager: TGLObjectManager;

implementation

uses
  SysUtils, 
  Dialogs, 
  Graphics,
  GLVectorGeometry, 
  GLScene, 
  GLViewer, 
  GLFullScreenViewer, 
  GLBaseClasses,
  GLStrings, 
  GLCoordinates, 
  GLTexture, 
  GLMaterial, 
  GLScreen,
  GLCadencer, 
  GLTextureImageEditors, 
  GLColor, 
  GLCrossPlatform, 
  GLMaterialEx,
  GLObjects, 
  GLGeomObjects, 
  GLPolyhedron,
  GLAnimatedSprite, 
  GLExtrusion, 
  GLMultiPolygon,
  GLVectorFileObjects, 
  GLMesh, 
  GLTilePlane, 
  GLPortal,
  GLTerrainRenderer, 
  GLHeightData, 
  GLHeightTileFileHDS, 
  GLBumpmapHDS, 
  GLPerlin,
  GLTexturedHDS, 
  GLAsyncHDS, 
  GLShadowHDS,
  GLBitmapFont, 
  GLGraph, 
  GLWindowsFont,
  GLParticles, 
  GLParticleFX, 
  GLPerlinPFX, 
  GLLinePFX, 
  GLFireFX, 
  GLThorFX,
  GLEParticleMasksManager,
  GLSkydome, 
  GLSkyBox, 
  GLAtmosphere,
  GLHUDObjects, 
  GLGameMenu,
  GLConsole,
  GLWindows, 
  GLGui,
  GLLensFlare, 
  GLTexLensFlare, 
  GLMirror, 
  GLShadowPlane, 
  GLShadowVolume,
  GLzBuffer, 
  GLSLProjectedTextures, 
  GLProjectedTextures, 
  GLBlur,
  GLTrail, 
  GLPostEffects,
  GLTeapot, 
  GLTree, 
  GLWaterPlane,
  GLProxyObjects, 
  GLMultiProxy, 
  GLMaterialMultiProxy,
  GLTexCombineShader, 
  GLPhongShader, 
  GLUserShader, 
  GLSLShader,
  GLHiddenLineShader, 
  GLCelShader, 
  GLOutlineShader, 
  GLMultiMaterialShader,
  GLBumpShader, 
  GLSLDiffuseSpecularShader, 
  GLSLBumpShader,
  GLAsmShader, 
  GLShaderCombiner, 
  GLTextureSharingShader,
  GLImposter, 
  GLFeedback, 
  GLCollision, 
  GLScriptBase, 
  GLAsyncTimer, 
  GLDCE,
  GLFPSMovement, 
  GLMaterialScript, 
  GLNavigator, 
  GLSmoothNavigator,
  GLTimeEventsMgr, 
  GLApplicationFileIO, 
  GLVfsPAK, 
  GLSimpleNavigation,
  GLCameraController, 
  GLGizmo, 
  GLGizmoEx, 
  GLFBORenderer,
  GLSoundFileObjects, 
  GLSound, 
  GLCompositeImage, 
  GLSLog, 
  GLSLanguage,
  GLSArchiveManager, 
  GLUtils,

  // Image file formats
  DDSImage, HDRImage, O3TCImage,

  // Vector file formats
  GLFile3DS, GLFileASE, GLFileB3D, GLFileGL2, GLFileGTS, GLFileLMTS,
  GLFileLWO, GLFileMD2, GLFileMD3, GLFileMD5, GLFileMDC, GLFileMS3D, GLFileNMF,
  GLFileNurbs, GLFileOBJ, GLFilePLY, GLFileSMD, GLFileSTL,
  GLFileTIN, GLFileVRML, GLFileX,

  // Sound file formats
  GLFileWAV, GLFileMP3,

  // Raster file format
  GLFileDDS, GLFileO3TC, GLFileHDR, GLFileBMP, GLFileTGA,

  // Property editor forms
  FGLSceneEdit, FVectorEditor, FMaterialEditorForm, FRMaterialPreview,
  FLibMaterialPicker, FRTextureEdit, FRFaceEditor,
  FRColorEditor, FRTrackBarEdit, FShaderUniformEditor, FGUILayoutEditor;

var
  vObjectManager: TGLObjectManager;

function ObjectManager: TGLObjectManager;
begin
  if not Assigned(vObjectManager) then
    vObjectManager := TGLObjectManager.Create(nil);
  Result := vObjectManager;
end;

type
  TGLSceneViewerEditor = class(TComponentEditor)
  public
    
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
    function GetVerbCount: integer; override;
  end;

  TGLSceneEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
    function GetVerbCount: integer; override;
  end;

  // TResolutionProperty

  TResolutionProperty = class(TPropertyEditor)
  public
     
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  // TClassProperty

  TGLTextureProperty = class(TClassProperty)
  public
     
    function GetAttributes: TPropertyAttributes; override;
  end;

  // TGLTextureImageProperty

  TGLTextureImageProperty = class(TClassProperty)
  public
     
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TGLImageClassProperty

  TGLImageClassProperty = class(TClassProperty)
  public
     
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TGLColorProperty = class(TClassProperty)
  public
     
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure Edit; override;

    function ColorToBorderColor(aColor: TColorVector; selected: boolean): TColor;
    procedure ListMeasureWidth(const AValue: ansistring; Index: integer;
      ACanvas: TCanvas; var AWidth: integer); override;
    procedure ListMeasureHeight(const AValue: ansistring; Index: integer;
      ACanvas: TCanvas; var AHeight: integer); override;
    procedure ListDrawValue(const AValue: ansistring; Index: integer;
      ACanvas: TCanvas; const ARect: TRect; AState: TPropEditDrawState); override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      AState: TPropEditDrawState); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  // TSoundFileProperty

  TSoundFileProperty = class(TClassProperty)
  public
     
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  // TSoundNameProperty

  TSoundNameProperty = class(TStringProperty)
  public
     
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  // TGLCoordinatesProperty

  TGLCoordinatesProperty = class(TClassProperty)
  public
     
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TGLMaterialProperty

  TGLMaterialProperty = class(TClassProperty)
  public
     
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TReuseableDefaultEditor

   { Editor copied from DsgnIntf.
      Could have been avoided, if only that guy at Borland didn't chose to
      publish only half of the stuff (and that's not the only class with
      that problem, most of the subitems handling code in TGLSceneBaseObject is
      here for the same reason...), the "protected" wasn't meant just to lure
      programmers into code they can't reuse... Arrr! and he did that again
      in D6! Grrr... }

  // TGLMaterialLibraryEditor

  { Editor for material library. }

  TGLMaterialLibraryEditor = class(TDefaultComponentEditor)
  public
     
    procedure EditProperty(const Prop: TPropertyEditor; var Continue: boolean);
      override;
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
  end;

  // TGLAnimationNameProperty

  TGLAnimationNameProperty = class(TStringProperty)
  public
     
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(proc: TGetStrProc); override;
  end;

  // TGLSArchiveManagerEditor

  TGLSArchiveManagerEditor = class(TDefaultComponentEditor)
  public
     
    procedure Edit; override;
    procedure EditProperty(const Prop: TPropertyEditor; var Continue: boolean);
      override;
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
  end;

  // TGLMaterialComponentNameProperty


  TGLMaterialComponentNameProperty = class(TStringProperty)
  public
     
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TGLLibTextureNameProperty = class(TGLMaterialComponentNameProperty)
  public
     
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibSamplerNameProperty = class(TGLMaterialComponentNameProperty)
  public
     
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibCombinerNameProperty = class(TGLMaterialComponentNameProperty)
  public
     
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibShaderNameProperty = class(TGLMaterialComponentNameProperty)
  public
     
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibAttachmentNameProperty = class(TGLMaterialComponentNameProperty)
  public
     
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibAsmProgNameProperty = class(TGLMaterialComponentNameProperty)
  public
     
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  // TPictureFileProperty

  TPictureFileProperty = class(TStringProperty)
  public
     
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TShaderFileProperty

  TShaderFileProperty = class(TStringProperty)
  public
     
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TUniformAutoSetProperty

  TUniformAutoSetProperty = class(TPropertyEditor)
  private
    procedure PassUniform(const S: string);
  public
     
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TGLGUILayoutEditor
  //
  TGLGUILayoutEditor = class(TComponentEditor)
  public
     
    procedure Edit; override;

    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

//----------------- TGLSceneViewerEditor ---------------------------------------

// ExecuteVerb

procedure TGLSceneViewerEditor.ExecuteVerb(Index: integer);
var
  Source: TGLSceneViewer;
begin
  Source := Component as TGLSceneViewer;
  case Index of
    0: Source.Buffer.ShowInfo;
  end;
end;

// GetVerb

function TGLSceneViewerEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := 'Show context info';
  end;
end;

// GetVerbCount

function TGLSceneViewerEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

//----------------- TGLSceneEditor ---------------------------------------------

// Edit

procedure TGLSceneEditor.Edit;
begin
  with GLSceneEditorForm do
  begin
    SetScene(Self.Component as TGLScene, TComponentEditorDesigner(Self.Designer));
    Show;
  end;
end;

// ExecuteVerb

procedure TGLSceneEditor.ExecuteVerb(Index: integer);
begin
  case Index of
    0: Edit;
  end;
end;

// GetVerb

function TGLSceneEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := 'Show Scene Editor';
  end;
end;

// GetVerbCount

function TGLSceneEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

//----------------- TResolutionProperty ----------------------------------------

// GetAttributes

function TResolutionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

// GetValue

function TResolutionProperty.GetValue: string;
begin
{$IFDEF MSWINDOWS}
  Result := vVideoModes[GetOrdValue].Description;
{$ENDIF}
{$IFDEF GLS_X11_SUPPORT}
  //Testing!!!
  with vVideoModes[GetOrdValue]^ do
    Result := IntToStr(hdisplay) + ' x ' + IntToStr(vdisplay) + ', ' + '0 bpp';
{$ENDIF}
{$IFDEF Darwin}
  Result := '';
{$MESSAGE Warn 'Needs to be implemented'}
 {$ENDIF}

end;

// GetValues

procedure TResolutionProperty.GetValues(Proc: TGetStrProc);
var
  i: integer;
begin
{$IFDEF MSWINDOWS}
  for i := 0 to vNumberVideoModes - 1 do
    Proc(vVideoModes[i].Description);
{$ENDIF}
{$IFDEF GLS_X11_SUPPORT}
  for i := 0 to vNumberVideoModes - 1 do
    with vVideoModes[i]^ do
      Proc(IntToStr(hdisplay) + 'x' + IntToStr(vdisplay) + 'x' + '0');
{$ENDIF}
{$IFDEF Darwin}
{$MESSAGE Warn 'Needs to be implemented'}
{$ENDIF}
end;

// SetValue

procedure TResolutionProperty.SetValue(const Value: string);

const
  Nums = ['0'..'9'];

var
  XRes, YRes, BPP: integer;
  Pos, SLength: integer;
  TempStr: string;

begin
  if CompareText(Value, 'default') <> 0 then
  begin
    // initialize scanning
    TempStr := Trim(Value) + '|'; // ensure at least one delimiter
    SLength := Length(TempStr);
    XRes := 0;
    YRes := 0;
    BPP := 0;
    // contains the string something?
    if SLength > 1 then
    begin
      // determine first number
      for Pos := 1 to SLength do
        if not (TempStr[Pos] in Nums) then
          Break;
      if Pos <= SLength then
      begin
        // found a number?
        XRes := StrToInt(Copy(TempStr, 1, Pos - 1));
        // search for following non-numerics
        for Pos := Pos to SLength do
          if TempStr[Pos] in Nums then
            Break;
        Delete(TempStr, 1, Pos - 1); // take it out of the String
        SLength := Length(TempStr); // rest length of String
        if SLength > 1 then // something to scan?
        begin
          // determine second number
          for Pos := 1 to SLength do
            if not (TempStr[Pos] in Nums) then
              Break;
          if Pos <= SLength then
          begin
            YRes := StrToInt(Copy(TempStr, 1, Pos - 1));
            // search for following non-numerics
            for Pos := Pos to SLength do
              if TempStr[Pos] in Nums then
                Break;
            Delete(TempStr, 1, Pos - 1); // take it out of the String
            SLength := Length(TempStr); // rest length of String
            if SLength > 1 then
            begin
              for Pos := 1 to SLength do
                if not (TempStr[Pos] in Nums) then
                  Break;
              if Pos <= SLength then
                BPP := StrToInt(Copy(TempStr, 1, Pos - 1));
            end;
          end;
        end;
      end;
    end;
    SetOrdValue(GetIndexFromResolution(XRes, YRes, BPP));
  end
  else
    SetOrdValue(0);
end;

//----------------- TGLTextureProperty -----------------------------------------

function TGLTextureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties];
end;

//----------------- TGLTextureImageProperty ------------------------------------

// GetAttributes

function TGLTextureImageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

// Edit

procedure TGLTextureImageProperty.Edit;
begin
  if EditGLTextureImage(TGLTextureImage(GetObjectValue)) then
    Modified;
end;

//----------------- TGLImageClassProperty --------------------------------------

// GetAttributes

function TGLImageClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

// GetValues

procedure TGLImageClassProperty.GetValues(proc: TGetStrProc);
var
  i: integer;
  sl: TStrings;
begin
  sl := GetGLTextureImageClassesAsStrings;
  try
    for i := 0 to sl.Count - 1 do
      proc(sl[i]);
  finally
    sl.Free;
  end;
end;

// GetValue

function TGLImageClassProperty.GetValue: string;
begin
  Result := FindGLTextureImageClass(GetStrValue).FriendlyName;
end;

// SetValue

procedure TGLImageClassProperty.SetValue(const Value: string);
var
  tic: TGLTextureImageClass;
begin
  tic := FindGLTextureImageClassByFriendlyName(Value);
  if Assigned(tic) then
    SetStrValue(tic.ClassName)
  else
    SetStrValue('');
  Modified;
end;

//----------------- TGLColorproperty -----------------------------------------------------------------------------------

procedure TGLColorProperty.Edit;
var
  colorDialog: TColorDialog;
  glColor: TGLColor;
begin
  colorDialog := TColorDialog.Create(nil);
  try
    glColor := TGLColor(GetObjectValue);
    colorDialog.Color := ConvertColorVector(glColor.Color);
    if colorDialog.Execute then
    begin
      glColor.Color := ConvertWinColor(colorDialog.Color);
      Modified;
    end;
  finally
    colorDialog.Free;
  end;
end;

function TGLColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paValueList, paDialog];
end;

procedure TGLColorProperty.GetValues(Proc: TGetStrProc);
begin
  ColorManager.EnumColors(Proc);
end;

function TGLColorProperty.GetValue: string;
begin
  Result := ColorManager.GetColorName(TGLColor(GetObjectValue).Color);
end;

procedure TGLColorProperty.SetValue(const Value: string);
begin
  TGLColor(GetObjectValue).Color := ColorManager.GetColor(Value);
  Modified;
end;

// ColorToBorderColor

function TGLColorProperty.ColorToBorderColor(aColor: TColorVector;
  selected: boolean): TColor;
begin
  if (aColor.V[0] > 0.75) or (aColor.V[1] > 0.75) or (aColor.V[2] > 0.75) then
    Result := clBlack
  else if selected then
    Result := clWhite
  else
    Result := ConvertColorVector(AColor);
end;

procedure TGLColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  AState: TPropEditDrawState);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, GetOrdValue, ACanvas, ARect, [pedsSelected]);
end;

procedure TGLColorProperty.ListMeasureWidth(const AValue: ansistring;
  Index: integer; ACanvas: TCanvas; var AWidth: integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('M');
end;

procedure TGLColorProperty.ListMeasureHeight(const AValue: ansistring;
  Index: integer; ACanvas: TCanvas; var AHeight: integer);
begin
  // Nothing
end;

procedure TGLColorProperty.ListDrawValue(const AValue: ansistring;
  Index: integer; ACanvas: TCanvas; const ARect: TRect; AState: TPropEditDrawState);
var
  vRight: integer;
  vOldPenColor, vOldBrushColor: TColor;
  Color: TColorVector;
begin
  vRight := (ARect.Bottom - ARect.Top) + ARect.Left;
  with ACanvas do
  begin
    vOldPenColor := Pen.Color;
    vOldBrushColor := Brush.Color;

    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, vRight, ARect.Bottom);

    Color := ColorManager.GetColor(AValue);
    Brush.Color := ConvertColorVector(Color);
    Pen.Color := ColorToBorderColor(Color, pedsSelected in AState);

    Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, ARect.Bottom - 1);

    Brush.Color := vOldBrushColor;
    Pen.Color := vOldPenColor;
  end;
end;

// GetAttributes

function TSoundFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

// GetValue

function TSoundFileProperty.GetValue: string;
var
  sample: TGLSoundSample;
begin
  sample := GetComponent(0) as TGLSoundSample;
  if sample.Data <> nil then
    Result := '(' + sample.Data.ClassName + ')'
  else
    Result := '(empty)';
end;

// Edit

procedure TSoundFileProperty.Edit;
var
  ODialog: TOpenDialog;
  sample: TGLSoundSample;
  Desc, F: string;
begin
  sample := GetComponent(0) as TGLSoundSample;
  ODialog := TOpenDialog.Create(nil);
  try
    GetGLSoundFileFormats.BuildFilterStrings(TGLSoundFile, Desc, F);
    ODialog.Filter := Desc;
    if ODialog.Execute then
    begin
      sample.LoadFromFile(ODialog.FileName);
      Modified;
    end;
  finally
    ODialog.Free;
  end;
end;

//----------------- TSoundNameProperty -----------------------------------------

// GetAttributes

function TSoundNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

// GetValues

procedure TSoundNameProperty.GetValues(Proc: TGetStrProc);
var
  i: integer;
  Source: TGLBaseSoundSource;
begin
  Source := (GetComponent(0) as TGLBaseSoundSource);
  if Assigned(Source.SoundLibrary) then
    with Source.SoundLibrary do
      for i := 0 to Samples.Count - 1 do
        Proc(Samples[i].Name);
end;

//----------------- TGLCoordinatesProperty -------------------------------------

// GetAttributes

function TGLCoordinatesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

// Edit;

procedure TGLCoordinatesProperty.Edit;
var
  glc: TGLCoordinates;
  x, y, z: single;
begin
  glc := TGLCoordinates(GetObjectValue);
  x := glc.x;
  y := glc.y;
  z := glc.z;
  if VectorEditorForm.Execute(x, y, z) then
  begin
    glc.AsVector := VectorMake(x, y, z);
    Modified;
  end;
end;

//----------------- TGLMaterialProperty --------------------------------------------------------------------------------

// GetAttributes

function TGLMaterialProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

// Edit

procedure TGLMaterialProperty.Edit;
var
  ml: TGLMaterial;
begin
  ml := TGLMaterial(GetObjectValue);
  if FMaterialEditorForm.MaterialEditorForm.Execute(ml) then
    Modified;
end;

//----------------- TGLMaterialLibraryEditor --------------------------------------------------------------------------------

// EditProperty

procedure TGLMaterialLibraryEditor.EditProperty(const Prop: TPropertyEditor;
  var Continue: boolean);
begin
  BestEditEvent := 'MATERIALS';
  inherited;
end;

// ExecuteVerb

procedure TGLMaterialLibraryEditor.ExecuteVerb(Index: integer);
begin
  case Index of
    0: Edit;
  end;
end;

// GetVerb

function TGLMaterialLibraryEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := 'Show Material Library Editor';
  end;
end;

//----------------- TGLLibMaterialNameProperty ---------------------------------

// GetAttributes

function TGLLibMaterialNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

// Edit

procedure TGLLibMaterialNameProperty.Edit;
var
  buf: string;
  ml: TGLAbstractMaterialLibrary;
  obj: TPersistent;
  Int: IGLMaterialLibrarySupported;
begin

  buf := GetStrValue;
  obj := GetComponent(0);
  if Supports(Obj, IGLMaterialLibrarySupported, Int) then
    ml := Int.GetMaterialLibrary
  else
  begin
    ml := nil;
    Assert(False, 'oops, unsupported...');
  end;
  if not Assigned(ml) then
    ShowMessage('Select the material library first.')
  else if LibMaterialPicker.Execute(buf, ml) then
  begin
    SetStrValue(buf);
    Modified;

  end;
end;

//----------------- TGLAnimationNameProperty -----------------------------------

// GetAttributes

function TGLAnimationNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

// GetValues

procedure TGLAnimationNameProperty.GetValues(proc: TGetStrProc);
var
  i: integer;
  animControler: TGLAnimationControler;
  actor: TGLActor;
begin
  animControler := (GetComponent(0) as TGLAnimationControler);
  if Assigned(animControler) then
  begin
    actor := animControler.Actor;
    if Assigned(actor) then
      with actor.Animations do
      begin
        for i := 0 to Count - 1 do
          proc(Items[i].Name);
      end;
  end;
end;

//----------------- TGLSArchiveManagerEditor --------------------------------------------------------------------------------

// EditProperty

procedure TGLSArchiveManagerEditor.EditProperty(const Prop: TPropertyEditor;
  var Continue: boolean);
begin
  BestEditEvent := 'ARCHIVES';
  inherited;
end;
// Edit

procedure TGLSArchiveManagerEditor.Edit;
begin
  inherited;
end;

// ExecuteVerb

procedure TGLSArchiveManagerEditor.ExecuteVerb(Index: integer);
begin
  case Index of
    0: Edit;
  end;
end;

// GetVerb

function TGLSArchiveManagerEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := 'Show Archive Manager Editor';
  end;
end;

{$REGION 'TGLMaterialComponentNameProperty'}

procedure TGLMaterialComponentNameProperty.Edit;
var
  LOwner: IGLMaterialLibrarySupported;
  LItem: TGLBaseMaterialCollectionItem;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
  begin
    LItem := TGLMaterialLibraryEx(LOwner.GetMaterialLibrary).Components.GetItemByName(
      GetStrValue);
    if Assigned(LItem) then
      GlobalDesignHook.SelectOnlyThis(LItem);
    Modified;
  end;
end;

function TGLMaterialComponentNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TGLLibTextureNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
  begin
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc, TGLTextureImageEx);
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc,
      TGLFrameBufferAttachment);
  end;
end;

procedure TGLLibSamplerNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc, TGLTextureSampler);
end;

procedure TGLLibCombinerNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc, TGLTextureCombiner);
end;

procedure TGLLibShaderNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc, TGLShaderEx);
end;

procedure TGLLibAttachmentNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc,
      TGLFrameBufferAttachment);
end;

procedure TGLLibAsmProgNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc,
      TGLASMVertexProgram);
end;

{$ENDREGION}

{$REGION 'TPictureFileProperty'}

function TPictureFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TPictureFileProperty.Edit;
var
  LFileName: string;
begin
  LFileName := TGLTextureImageEx(GetComponent(0)).SourceFile;
  if OpenPictureDialog(LFileName) then
  begin
    SetStrValue(RelativePath(LFileName));
  end;
  Modified;
end;

{$ENDREGION}

{$REGION 'TPictureFileProperty'}

procedure TShaderFileProperty.Edit;
var
  ODialog: TOpenDialog;
begin
  ODialog := TOpenDialog.Create(nil);
  try
    ODialog.Filter := '*.glsl';
    if ODialog.Execute then
    begin
      SetStrValue(RelativePath(ODialog.FileName));
      Modified;
    end;
  finally
    ODialog.Free;
  end;
end;

function TShaderFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{$ENDREGION}

{$REGION 'TUniformAutoSetProperty'}

function TUniformAutoSetProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paFullWidthName];
end;

procedure TUniformAutoSetProperty.PassUniform(const S: string);
begin
  ShaderUniformEditor.AddUniform(TGLBaseShaderModel(GetComponent(0)).Uniforms[S]);
end;

function TUniformAutoSetProperty.GetValue: string;
begin
  Result := '';
end;

procedure TUniformAutoSetProperty.Edit;
var
  LOwner: TGLBaseShaderModel;
begin
  LOwner := TGLBaseShaderModel(GetComponent(0));
  if LOwner.Enabled and LOwner.IsValid then
  begin
    with ShaderUniformEditor do
    begin
      Clear;
      LOwner.MaterialLibrary.GetNames(AddTextureName, TGLTextureImageEx);
      LOwner.MaterialLibrary.GetNames(AddTextureName, TGLFrameBufferAttachment);
      LOwner.MaterialLibrary.GetNames(AddSamplerName, TGLTextureSampler);
      LOwner.GetUniformNames(PassUniform);
      Execute;
    end;
  end;
end;

{$ENDREGION}

{$REGION 'TGLGUILayoutEditor'}

procedure TGLGUILayoutEditor.Edit;
begin
  GUILayoutEditorForm.Execute(TGLGuiLayout(Self.Component));
end;

procedure TGLGUILayoutEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TGLGUILayoutEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Show Layout Editor';
  end;
end;

function TGLGUILayoutEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$ENDREGION}

function GetProjectTargetName: string;
begin
  Result := '$(TargetFile)';
  if not Assigned(IDEMacros) or not IDEMacros.SubstituteMacros(Result) then
    Result := '';
end;

procedure Register;
begin
  RegisterComponents('GLScene', [TGLScene, TGLSceneViewer, TGLMemoryViewer, 
  TGLMaterialLibrary, TGLMaterialLibraryEx, TGLCadencer, TGLGuiLayout, 
  TGLBitmapFont, TGLWindowsBitmapFont, TGLScriptLibrary, TGLSoundLibrary, 
  TGLFullScreenViewer]);

  RegisterComponents('GLScene PFX',
    [TGLCustomPFXManager, TGLPolygonPFXManager, TGLPointLightPFXManager,
    TGLCustomSpritePFXManager, TGLPerlinPFXManager, TGLLinePFXManager,
    TGLFireFXManager, TGLThorFXManager, TGLEParticleMasksManager]);

  RegisterComponents('GLScene Utils',
    [TGLAsyncTimer, TGLStaticImposterBuilder, TGLCollisionManager,
    TGLAnimationControler, TGLDCEManager, TGLFPSMovementManager,
    TGLMaterialScripter, TGLUserInterface, TGLNavigator, TGLSmoothNavigator,
    TGLSmoothUserInterface, TGLTimeEventsMGR, TGLApplicationFileIO,
    TGLVfsPAK, TGLSimpleNavigation, TGLCameraController, TGLGizmo,
    TGLGizmoEx, TGLSLogger, TGLSLanguage, TGLSArchiveManager]);

  RegisterComponents('GLScene Terrain',
    [TGLBitmapHDS, TGLCustomHDS, TGLHeightTileFileHDS, TGLBumpmapHDS,
    TGLPerlinHDS, TGLTexturedHDS, TGLAsyncHDS, TGLShadowHDS]);

  RegisterComponents('GLScene Shaders',
    [TGLTexCombineShader, TGLPhongShader, TGLUserShader, TGLHiddenLineShader,
    TGLCelShader, TGLOutlineShader, TGLMultiMaterialShader, TGLBumpShader,
    TGLSLShader, TGLSLDiffuseSpecularShader, TGLSLBumpShader, TGLAsmShader,
    TGLShaderCombiner, TGLTextureSharingShader]);

  RegisterComponentEditor(TGLSceneViewer, TGLSceneViewerEditor);
  RegisterComponentEditor(TGLScene, TGLSceneEditor);
  RegisterComponentEditor(TGLGUILayout, TGLGUILayoutEditor);

  RegisterClasses([TGLCoordinates]);

  RegisterComponentEditor(TGLMaterialLibrary, TGLMaterialLibraryEditor);
  RegisterComponentEditor(TGLSArchiveManager, TGLSArchiveManagerEditor);

  RegisterPropertyEditor(TypeInfo(TResolution), nil, '', TResolutionProperty);
  RegisterPropertyEditor(TypeInfo(TGLTexture), TGLMaterial, '', TGLTextureProperty);
  RegisterPropertyEditor(TypeInfo(TGLTextureImage), TGLTexture, '',
    TGLTextureImageProperty);
  RegisterPropertyEditor(TypeInfo(string), TGLTexture, 'ImageClassName',
    TGLImageClassProperty);

  RegisterPropertyEditor(TypeInfo(TGLSoundFile), TGLSoundSample, '',
    TSoundFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TGLBaseSoundSource, 'SoundName', 
  TSoundNameProperty);

  RegisterPropertyEditor(TypeInfo(TGLCoordinates), nil, '', TGLCoordinatesProperty);

  RegisterPropertyEditor(TypeInfo(TGLColor), nil, '', TGLColorProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterial), nil, '', TGLMaterialProperty);

  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterial,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLLibMaterial,
    'Texture2Name', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLSkyBox,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLEParticleMask,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLGameMenu,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterialMultiProxyMaster,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLSLBumpShader,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TSpriteAnimation,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterialProxy,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLActorProxy,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLFBORenderer, '',
    TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TActorAnimationName), TGLAnimationControler,
    '', TGLAnimationNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName),
    TGLTextureSharingShaderMaterial, 'LibMaterialName', TGLLibMaterialNameProperty);

  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLLibMaterialProperty,
    'NextPass', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName),
    TGLTextureProperties, 'LibTextureName', TGLLibTextureNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName),
    TGLTextureProperties, 'LibSamplerName', TGLLibSamplerNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName),
    TGLMultitexturingProperties, 'LibCombinerName', TGLLibCombinerNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName),
    TGLMultitexturingProperties, 'LibAsmProgName', TGLLibAsmProgNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel3,
    'LibVertexShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel3,
    'LibFragmentShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel4,
    'LibVertexShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel4,
    'LibFragmentShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel4,
    'LibGeometryShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel5,
    'LibVertexShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel5,
    'LibFragmentShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel5,
    'LibGeometryShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel5,
    'LibTessControlShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel5,
    'LibTessEvalShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TGLTextureImageEx, 'SourceFile',
    TPictureFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TGLShaderEx, 'SourceFile',
    TShaderFileProperty);
  RegisterPropertyEditor(TypeInfo(boolean), TGLBaseShaderModel,
    'AutoFillOfUniforms', TUniformAutoSetProperty);

  with ObjectManager do
  begin
    RegisterSceneObject(TGLCamera, 'Camera', '', HInstance);
    RegisterSceneObject(TGLLightSource, 'LightSource', '', HInstance);
    RegisterSceneObject(TGLDummyCube, 'DummyCube', '', HInstance);

    // Basic Geometry
    RegisterSceneObject(TGLSprite, 'Sprite', glsOCBasicGeometry, HInstance);
    RegisterSceneObject(TGLPoints, 'Points', glsOCBasicGeometry, HInstance);
    RegisterSceneObject(TGLLines, 'Lines', glsOCBasicGeometry, HInstance);
    RegisterSceneObject(TGLPlane, 'Plane', glsOCBasicGeometry, HInstance);
    RegisterSceneObject(TGLPolygon, 'Polygon', glsOCBasicGeometry, HInstance);
    RegisterSceneObject(TGLCube, 'Cube', glsOCBasicGeometry, HInstance);
    RegisterSceneObject(TGLFrustrum, 'Frustrum', glsOCBasicGeometry, HInstance);
    RegisterSceneObject(TGLSphere, 'Sphere', glsOCBasicGeometry, HInstance);
    RegisterSceneObject(TGLDisk, 'Disk', glsOCBasicGeometry, HInstance);
    RegisterSceneObject(TGLCone, 'Cone', glsOCBasicGeometry, HInstance);
    RegisterSceneObject(TGLCylinder, 'Cylinder', glsOCBasicGeometry, HInstance);
    RegisterSceneObject(TGLCapsule, 'Capsule', glsOCBasicGeometry, HInstance);
    RegisterSceneObject(TGLDodecahedron, 'Dodecahedron', glsOCBasicGeometry,
      HInstance);
    RegisterSceneObject(TGLIcosahedron, 'Icosahedron', glsOCBasicGeometry, HInstance);
    RegisterSceneObject(TGLOctahedron, 'Octahedron', glsOCBasicGeometry, HInstance);
    RegisterSceneObject(TGLTetrahedron, 'Tetrahedron', glsOCBasicGeometry, HInstance);
    RegisterSceneObject(TGLSuperellipsoid, 'Superellipsoid', glsOCBasicGeometry, HInstance);

    //Advanced geometry
    RegisterSceneObject(TGLAnimatedSprite, 'Animated Sprite',
      glsOCAdvancedGeometry, HInstance);
    RegisterSceneObject(TGLArrowLine, 'ArrowLine', glsOCAdvancedGeometry, HInstance);
    RegisterSceneObject(TGLArrowArc, 'ArrowArc', glsOCAdvancedGeometry, HInstance);
    RegisterSceneObject(TGLAnnulus, 'Annulus', glsOCAdvancedGeometry, HInstance);
    RegisterSceneObject(TGLExtrusionSolid, 'ExtrusionSolid',
      glsOCAdvancedGeometry, HInstance);
    RegisterSceneObject(TGLMultiPolygon, 'MultiPolygon', glsOCAdvancedGeometry, HInstance);
    RegisterSceneObject(TGLPipe, 'Pipe', glsOCAdvancedGeometry, HInstance);
    RegisterSceneObject(TGLRevolutionSolid, 'RevolutionSolid',
      glsOCAdvancedGeometry, HInstance);
    RegisterSceneObject(TGLTorus, 'Torus', glsOCAdvancedGeometry, HInstance);

    //Mesh objects
    RegisterSceneObject(TGLActor, 'Actor', glsOCMeshObjects, HInstance);
    RegisterSceneObject(TGLFreeForm, 'FreeForm', glsOCMeshObjects, HInstance);
    RegisterSceneObject(TGLMesh, 'Mesh', glsOCMeshObjects, HInstance);
    RegisterSceneObject(TGLTilePlane, 'TilePlane', glsOCMeshObjects, HInstance);
    RegisterSceneObject(TGLPortal, 'Portal', glsOCMeshObjects, HInstance);
    RegisterSceneObject(TGLTerrainRenderer, 'TerrainRenderer', glsOCMeshObjects, HInstance);

    //Graph-plotting objects
    RegisterSceneObject(TGLFlatText, 'FlatText', glsOCGraphPlottingObjects, HInstance);
    RegisterSceneObject(TGLHeightField, 'HeightField',
      glsOCGraphPlottingObjects, HInstance);
    RegisterSceneObject(TGLXYZGrid, 'XYZGrid', glsOCGraphPlottingObjects, HInstance);

    //Particle systems
    RegisterSceneObject(TGLParticles, 'Particles', glsOCParticleSystems, HInstance);
    RegisterSceneObject(TGLParticleFXRenderer, 'PFX Renderer',
      glsOCParticleSystems, HInstance);

    //Environment objects
    RegisterSceneObject(TGLEarthSkyDome, 'EarthSkyDome',
      glsOCEnvironmentObjects, HInstance);
    RegisterSceneObject(TGLSkyDome, 'SkyDome', glsOCEnvironmentObjects, HInstance);
    RegisterSceneObject(TGLSkyBox, 'SkyBox', glsOCEnvironmentObjects, HInstance);
    RegisterSceneObject(TGLAtmosphere, 'Atmosphere', glsOCEnvironmentObjects,
      HInstance);

    // HUD objects.
    RegisterSceneObject(TGLHUDSprite, 'HUD Sprite', glsOCHUDObjects, HInstance);
    RegisterSceneObject(TGLHUDText, 'HUD Text', glsOCHUDObjects, HInstance);
    RegisterSceneObject(TGLResolutionIndependantHUDText,
      'Resolution Independant HUD Text', glsOCHUDObjects, HInstance);
    RegisterSceneObject(TGLAbsoluteHUDText, 'Absolute HUD Text',
      glsOCHUDObjects, HInstance);
    RegisterSceneObject(TGLGameMenu, 'GameMenu', glsOCHUDObjects, HInstance);
    RegisterSceneObject(TGLConsole, 'Console', glsOCHUDObjects, HInstance);

    // GUI objects.
    RegisterSceneObject(TGLBaseControl, 'Root Control', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLPopupMenu, 'GLPopupMenu', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLForm, 'GLForm', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLPanel, 'GLPanel', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLButton, 'GLButton', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLCheckBox, 'GLCheckBox', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLEdit, 'GLEdit', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLLabel, 'GLLabel', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLAdvancedLabel, 'GLAdvancedLabel', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLScrollbar, 'GLScrollbar', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLStringGrid, 'GLStringGrid', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLCustomControl, 'GLBitmapControl', glsOCGuiObjects, HInstance);

    //Special objects
    RegisterSceneObject(TGLLensFlare, 'LensFlare', glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLTextureLensFlare, 'TextureLensFlare',
      glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLMirror, 'Mirror', glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLShadowPlane, 'ShadowPlane', glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLShadowVolume, 'ShadowVolume', glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLZShadows, 'ZShadows', glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLSLTextureEmitter, 'GLSL Texture Emitter',
      glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLSLProjectedTextures, 'GLSL Projected Textures',
      glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLTextureEmitter, 'Texture Emitter', glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLProjectedTextures, 'Projected Textures',
      glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLBlur, 'Blur', glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLMotionBlur, 'MotionBlur', glsOCSpecialObjects, HInstance);

    RegisterSceneObject(TGLTrail, 'GLTrail', glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLPostEffect, 'PostEffect', glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLPostShaderHolder, 'PostShaderHolder',
      glsOCSpecialObjects, HInstance);

    // Doodad objects.
    RegisterSceneObject(TGLTeapot, 'Teapot', glsOCDoodad, HInstance);
    RegisterSceneObject(TGLTree, 'Tree', glsOCDoodad, HInstance);
    RegisterSceneObject(TGLWaterPlane, 'WaterPlane', glsOCDoodad, HInstance);

    // Proxy objects.
    RegisterSceneObject(TGLProxyObject, 'ProxyObject', glsOCProxyObjects, HInstance);
    RegisterSceneObject(TGLColorProxy, 'ColorProxy', glsOCProxyObjects, HInstance);
    RegisterSceneObject(TGLFreeFormProxy, 'FreeFormProxy',
      glsOCProxyObjects, HInstance);
    RegisterSceneObject(TGLMaterialProxy, 'MaterialProxy',
      glsOCProxyObjects, HInstance);
    RegisterSceneObject(TGLActorProxy, 'ActorProxy', glsOCProxyObjects, HInstance);
    RegisterSceneObject(TGLMultiProxy, 'MultiProxy', glsOCProxyObjects, HInstance);
    RegisterSceneObject(TGLMaterialMultiProxy, 'MaterialMultiProxy',
      glsOCProxyObjects, HInstance);

    // Other objects.
    RegisterSceneObject(TGLDirectOpenGL, 'Direct OpenGL', '', HInstance);
    RegisterSceneObject(TGLRenderPoint, 'Render Point', '', HInstance);
    RegisterSceneObject(TGLImposter, 'Imposter Sprite', '', HInstance);
    RegisterSceneObject(TGLFeedback, 'OpenGL Feedback', '', HInstance);
    RegisterSceneObject(TGLFBORenderer, 'OpenGL FrameBuffer', '', HInstance);

  end;
end;

initialization

{$I ../../Resources/GLScene.lrs}

  GLColor.vUseDefaultColorSets := True;
  GLCoordinates.vUseDefaultCoordinateSets := True;
  GLCrossPlatform.IsDesignTime := True;
  GLCrossPlatform.vProjectTargetName := GetProjectTargetName;

finalization

  ObjectManager.Free;

end.

