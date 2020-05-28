//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Base Cg shader classes.

    History : 
       19/12/12 - PW - Added CPPB compatibility:  changed cg.lib's name of function
                          GetProfileString to GetProfileStringA
       26/03/11 - Yar - Added SetAsMatrix to TCgParameter
       23/08/10 - Yar - Replaced OpenGL1x to OpenGLTokens
       22/04/10 - Yar - Fixes after GLState revision
       24/07/09 - DaStr - TGLShader.DoInitialize() now passes rci
                              (BugTracker ID = 2826217)   
       15/03/08 - DaStr - Fixups for vIgnoreContextActivationFailures mode
                                                      (BugTracker ID = 1914782)
       24/03/07 - DaStr - Improved Cross-Platform compatibility
                                                      (BugTracker ID = 1684432)
       23/02/07 - DaStr- Added TCgProgram.ManualNotification
       23/02/07 - DaStr- Added TCadencableCustomCgShader
                            Added EGLCGShaderException
                            with TCustomCgShader:
                              Added ShaderSupported,
                              Moved all properties from "public" and "published"
                              to the "protected" section.
                              DoInitialize fixed to catch compilation errors
                            Moved registration to GLCgRegister.pas
                            Added more stuff to RegisterClasses()
       29/04/06 - PhP  - Fixed TCgProgram.Finalize (Achim Hannes)
       24/10/04 - NelC - Added SetAsScalar for boolean input, IncludeFilePath
       10/09/04 - NelC - Added global function IsCgProfileSupported
       07/09/04 - NelC - Added TCgProgram.Precision (Cg 1.3)
       07/09/04 - NelC - Added profiles VP40, FP40 (Cg 1.3 beta 2),
                            Added option OutputCompilerWarnings
       23/04/04 - NelC - Now ManageTexture is false by default (Cg 1.2.1)
       24/03/04 - NelC - Added GetLatestProfile
       21/03/04 - NelC - Added TCgFragmentProgram.ManageTexture (Cg 1.2)
                            Added TCustomCgShader.IsProfileSupported
       16/02/04 - NelC - Added TCgParameter.SetParameterPointer
       13/02/04 - NelC - Replaced two overloaded TCgProgram.SetParam's with
                            SetStateMatrix and SetTexture
       05/02/04 - NelC - Fixed type checking for Half and Fixed,
                            Added TCgParameter.SetToTextureOf
       01/02/04 - NelC - Now reports source CgProgram or CgParameter of errors
       20/01/04 - NelC - Updated shader event handlers with Sender object,
                            Fixed dynamic array passing bug in CheckValueType
       03/01/04 - NelC - Shortened event handler names using 'VP' and 'FP',
                            Added TCustomCgShader.LoadShaderPrograms, TCgProgram.SetParam,
                            Minor change in texture type checking
       01/08/03 - NelC - Simplified type checking in SetAsStateMatrix
       04/07/03 - NelC - Added TCustomCgShader.OnInitialize,
                            Moved VertexProgram & FragmentProgram of TCustomCgShader
                            to published for easy acccess from OnInitialize
       02/07/03 - NelC - Added more value-setting methods
       01/07/03 - NelC - TCgProgram.ListCompilation now outputs line breaks
       27/06/03 - NelC - Added value-setting functions for TCgParameter,
                            TCgProgram.DirectParamByName & DirectProfile, and
                            Profile property for TCgVertexProgram & TCgFragmentProgram
       24/06/03 - NelC - Initial adoptation to Cg 1.1 Final. Now automatically
                            uses latest hardware-supported profile and use callback
                            to show error message
       29/05/03 - RoC - Cg 1.1 Depreciated_api compatible
       25/09/02 - EG - Cg Beta 2/2.1 compatible, now uses ARBVP
       19/06/02 - EG - Improved OO wrapper
       18/06/02 - EG - Creation
    
}
unit GLCgShader;

interface

uses
  Classes, SysUtils,
   
  GLVectorGeometry, GLVectorLists, GLVectorTypes, GLTexture, GLStrings,
  GLCadencer, OpenGLTokens, GLCrossPlatform, GLContext, GLBaseClasses,
  GLRenderContextInfo, GLMaterial, GLTextureFormat,

  // CG
  Cg, CgGL;

{$Include GLScene.inc}

{.$DEFINE OutputCompilerWarnings}

{ Define OutputCompilerWarnings to output Cg compiler warnings to a file. Useful
  for detecting bugs caused by using uninitialized value, implicit type cast, etc. }

type
  EGLCGShaderException = class(EGLShaderException);

  TCustomCgShader = class;
  TCgProgram = class;
  TCgParameter = class;

  TCgApplyEvent = procedure (CgProgram : TCgProgram; Sender : TObject) of object;
  TCgUnApplyEvent = procedure (CgProgram : TCgProgram) of object;
  TCgShaderEvent = procedure (CgShader : TCustomCgShader) of object;

  TcgProgramType = (ptVertex, ptFragment);

  // Available vertex program profile
  TCgVPProfile = (vpDetectLatest, vp20, vp30, vp40, arbvp1);

  // Available fragment program profile
  TCgFPProfile = (fpDetectLatest, fp20, fp30, fp40, arbfp1);

  TPrecisionSetting = (psFull, psFast);

  // TCgProgram
  //
  { Wrapper around a Cg program. }
  TCgProgram = class (TGLUpdateAbleObject)
  private
     
    FCgContext : PcgContext;
    FCode : TStrings; // the Cg program itself
    FProgramName : String;
    FHandle : PCGprogram;
    FParams : TList;

    FOnApply : TCgApplyEvent;
    FOnUnApply : TCgUnApplyEvent;
    FOnProgramChanged : TNotifyEvent;

    FEnabled : boolean;
    FDetectProfile : boolean;
    FPrecision: TPrecisionSetting;
    procedure SetPrecision(const Value: TPrecisionSetting);
    function GetManualNotification: Boolean;
    procedure SetManualNotification(const Value: Boolean);

  protected
     
    FProgramType : TcgProgramType;
    FProfile : TcgProfile;

    procedure SetCode(const val : TStrings);
    procedure SetProgramName(const val : String);
    function GetParam(index : String) : TCgParameter;

    procedure AddParamsItem(const Param : PCGParameter);
    { Build a list of parameters used in the shader code.
       Iteratively queries all parameters so that we can manage and access them
       easily. Currently only collects leaf parameters i.e. data structure is
       not retrieved. }
    procedure BuildParamsList;
    procedure ClearParamsList;

  public
     
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    function GetLatestProfile: TcgProfile; virtual; abstract;

    procedure Initialize; dynamic;
    procedure Finalize;
    procedure Apply(var rci : TGLRenderContextInfo; Sender : TObject);
    procedure UnApply(var rci : TGLRenderContextInfo);

    { ParamByName returns CgParameter; returns nil if not found. }
    function ParamByName(const name : String) : TCgParameter;
    { Use Param instead of ParamByName if you want implicit check for the
       existence of your requested parameter. }
    property Param[index : String] : TCgParameter read GetParam;
    property Params : TList read FParams;

    // Returns a handle to a Cg parameter
    function DirectParamByName(const name : String) : PCGparameter;

    function ParamCount : Integer;
    function GetProfileStringA : string;

    procedure LoadFromFile(const fileName : String);

    procedure ListCompilation(Output : TStrings);
    procedure ListParameters(Output : TStrings);

    // shorthands for accessing parameters
    procedure SetParam(ParamName: string; SingleVal : Single); overload;
    procedure SetParam(ParamName: string; const Vector2fVal : TVector2f); overload;
    procedure SetParam(ParamName: string; const Vector3fVal : TVector3f); overload;
    procedure SetParam(ParamName: string; const Vector4fVal : TVector4f); overload;
    procedure SetStateMatrix(ParamName: string; matrix, Transform: Cardinal);
    procedure SetTexture(ParamName: string; TextureID : Cardinal);

    // retruns ShaderName.[program type].ProgramName
    function LongName : string;

    { Direct access to the profile. 
       Set Profile of the sub-classes to any but DetectLatest if you want to
       specify the profile directly. }
    property DirectProfile : TcgProfile read FProfile write FProfile;

    { DaStr: Seams, that this event is never called. Probably should be deleted... }
    property OnProgramChanged : TNotifyEvent read FOnProgramChanged write FOnProgramChanged;

    { If True, that shader is not reset when TCgProgram' parameters change. }
    property ManualNotification: Boolean read GetManualNotification write SetManualNotification default False;
  published
     
    property Code : TStrings read FCode write SetCode;
    property ProgramName : String read FProgramName write SetProgramName;
    property Enabled : boolean read FEnabled write FEnabled default True;
    { Precision controls data precision of GPU operation. 
       Possible options are 16-bit (psFast) or 32-bit (psFull). 16-bit operation
       is generally faster. }
    property Precision : TPrecisionSetting read FPrecision write SetPrecision default psFull;
    property OnApply : TCgApplyEvent read FOnApply write FOnApply;
    property OnUnApply : TCgUnApplyEvent read FOnUnApply write FOnUnApply;
  end;

  // TCgParameter
  //
  { Wrapper around a Cg parameter of the main program. }
  TCgParameter = class (TObject)
  private
     
    FOwner : TCgProgram;
    FName : String;
    FHandle      : PCGparameter;
    FValueType   : TCGtype; // e.g. CG_FLOAT
    FDirection   : TCGenum; // e.g. CG_INOUT
    FVariability : TCGenum; // e.g. CG_UNIFORM
  protected
     
    function TypeMismatchMessage : string;
    procedure CheckValueType(aType : TCGtype); overload;
    procedure CheckValueType(const types : array of TCGtype); overload;

    procedure CheckAllTextureTypes;
    procedure CheckAllScalarTypes;
    procedure CheckAllVector2fTypes;
    procedure CheckAllVector3fTypes;
    procedure CheckAllVector4fTypes;

    procedure SetAsVector2f(const val : TVector2f);
    procedure SetAsVector3f(const val : TVector3f);
    procedure SetAsVector4f(const val : TVector4f);
  public
     
    constructor Create; virtual;
    destructor Destroy; override;

    { Procedures for setting uniform pamareters.
       Implicitly check for data type. }
    procedure SetAsScalar(const val : Single); overload;
    procedure SetAsScalar(const val : boolean); overload;
    procedure SetAsVector(const val : TVector2f); overload;
    procedure SetAsVector(const val : TVector3f); overload;
    procedure SetAsVector(const val : TVector4f); overload;
    { This overloaded SetAsVector accepts open array as input. e.g.
       SetAsVector([0.1, 0.2]). Array length must between 1-4. }
    procedure SetAsVector(const val : array of single); overload;
    procedure SetAsStateMatrix(matrix, Transform: Cardinal);
    procedure SetAsMatrix(const val: TMatrix4f);

    { Procedures for dealing with texture pamareters.}
    // SetAsTexture checks for all texture types
    procedure SetAsTexture(TextureID : Cardinal);
    // SetAsTexture* check for specific type
    procedure SetAsTexture1D(TextureID : Cardinal);
    procedure SetAsTexture2D(TextureID : Cardinal);
    procedure SetAsTexture3D(TextureID : Cardinal);
    procedure SetAsTextureCUBE(TextureID : Cardinal);
    procedure SetAsTextureRECT(TextureID : Cardinal);
    { SetToTextureOf determines texture type on-the-fly.}
    procedure SetToTextureOf(LibMaterial  : TGLLibMaterial);

    procedure EnableTexture;
    procedure DisableTexture;

    { Procedures for setting varying parameters with an array of values.}
    procedure SetParameterPointer(Values : TVectorList); overload;
    procedure SetParameterPointer(Values : TAffineVectorList); overload;

    procedure EnableClientState;
    procedure DisableClientState;

    { LongName retruns ShaderName.[program type].ProgramName.ParamName. }
    function LongName : string;

    property Owner : TCgProgram read FOwner;
    property Name : String read FName;
    property ValueType : TCGtype read FValueType;
    property Handle : PCGparameter read FHandle write FHandle;
    property Direction : TCGenum read FDirection write FDirection;
    property Variability : TCGenum read FVariability write FVariability;

     cene-friendly properties
    property AsVector : TVector write SetAsVector4f; // position f.i.
    property AsAffineVector : TAffineVector write SetAsVector3f; // normal f.i.
    property AsVector2f : TVector2f write SetAsVector2f; // texCoord f.i.
  end;

  // TCgVertexProgram
  //
  TCgVertexProgram = class (TCgProgram)
  private
    FVPProfile : TCgVPProfile;
    procedure SetVPProfile(v : TCgVPProfile);
  public
     
    constructor Create(AOwner: TPersistent); override;
    function GetLatestProfile: TcgProfile;  override;
  published
    property Profile : TCgVPProfile read FVPProfile write SetVPProfile default vpDetectLatest;
  end;

  // TCgFragmentProgram
  //
  TCgFragmentProgram = class (TCgProgram)
  private
    FFPProfile : TCgFPProfile;
    FManageTexture : boolean;
    procedure SetFPProfile(v : TCgFPProfile);
    procedure SetManageTexture(const Value: boolean);

  public
     
    constructor Create(AOwner: TPersistent); override;
    procedure Initialize; override;
    function GetLatestProfile: TcgProfile; override;

  published
    property Profile : TCgFPProfile read FFPProfile write SetFPProfile default fpDetectLatest;
    // Switch for auto enabling of texture parameters (Cg 1.2 feature)
    // With Cg 1.2.1, default is OFF
    property ManageTexture : boolean read FManageTexture write SetManageTexture default false;
  end;

  // TCustomCgShader
  //
  TCustomCgShader = class (TGLShader)
  private
     
    FVertexProgram : TCgVertexProgram;
    FFragmentProgram : TCgFragmentProgram;

    FOnInitialize : TCgShaderEvent;

    FDesignEnable : Boolean;

  protected
     
    // Vertex Program
    procedure SetVertexProgram(const val : TCgVertexProgram);
    procedure SetOnApplyVertexProgram(const val : TCgApplyEvent);
    function GetOnApplyVertexProgram : TCgApplyEvent;
    procedure SetOnUnApplyVertexProgram(const val : TCgUnApplyEvent);
    function GetOnUnApplyVertexProgram : TCgUnApplyEvent;

    // Fragment Program
    procedure SetFragmentProgram(const val : TCgFragmentProgram);
    procedure SetOnApplyFragmentProgram(const val : TCgApplyEvent);
    function GetOnApplyFragmentProgram : TCgApplyEvent;
    procedure SetOnUnApplyFragmentProgram(const val : TCgUnApplyEvent);
    function GetOnUnApplyFragmentProgram : TCgUnApplyEvent;

    // OnInitialize
    function GetOnInitialize : TCgShaderEvent;
    procedure SetOnInitialize(const val : TCgShaderEvent);

    procedure DoInitialize(var rci : TGLRenderContextInfo; Sender : TObject); override;
    procedure DoFinalize; override;
    procedure DoApply(var rci : TGLRenderContextInfo; Sender : TObject); override;
    function  DoUnApply(var rci : TGLRenderContextInfo) : Boolean; override;

    // IsProfileSupported to be obsoleted by global function IsCgProfileSupported
    function IsProfileSupported(Profile: TcgProfile): boolean;

    { DaStr: Everything is moved here from the public and protected sections
             because I would like to shield end-users of descendant shader 
             classes from all this stuff. Those who want direct access
             to shader events and parameters should use the TCgShader class,
             where everything is published. }

    property OnApplyVP : TCgApplyEvent read GetOnApplyVertexProgram write SetOnApplyVertexProgram;
    property OnApplyFP : TCgApplyEvent read GetOnApplyFragmentProgram write SetOnApplyFragmentProgram;

    property OnUnApplyVP : TCgUnApplyEvent read GetOnUnApplyVertexProgram write SetOnUnApplyVertexProgram;
    property OnUnApplyFP : TCgUnApplyEvent read GetOnUnApplyFragmentProgram write SetOnUnApplyFragmentProgram;

    { OnInitialize can be use to set parameters that need to be set once only. See demo "Cg Texture" for example. }
    property OnInitialize : TCgShaderEvent read GetOnInitialize write SetOnInitialize;

    property DesignEnable : Boolean read FDesignEnable write FDesignEnable default False;
    property VertexProgram : TCgVertexProgram read FVertexProgram write SetVertexProgram;
    property FragmentProgram : TCgFragmentProgram read FFragmentProgram write SetFragmentProgram;

  public
     
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadShaderPrograms(const VPFilename, FPFilename : string);
    function ShaderSupported: Boolean; override;

  end;

  { Allows to use a Cadencer, which is used for noise generation in many shaders. }
  TCadencableCustomCgShader = class(TCustomCgShader)
  private
    FCadencer: TGLCadencer;
    procedure SetCadencer(const Value: TGLCadencer);
  protected
    procedure DoInitialize(var rci : TGLRenderContextInfo; Sender : TObject); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property Cadencer: TGLCadencer read FCadencer write SetCadencer;
  end;

  // TCgShader
  //
  TCgShader = class (TCustomCgShader)
  published
     
    property DesignEnable;
    property ShaderStyle;
    property FailedInitAction;
    
    property VertexProgram;
    property FragmentProgram;

    property OnApplyVP;
    property OnApplyFP;

    property OnUnApplyVP;
    property OnUnApplyFP;

    property OnInitialize;
  end;

// global variables/functions
var
  { Set IncludeFilePath to indicate where to find your include file for your
     Cg source files. This avoids error from the Cg Compiler when the current
     directory is not the right path as the shader is being compiled. }
  IncludeFilePath : string;
{$IFDEF OutputCompilerWarnings}
  { Edit the string WarningFilePath for the output filename. Default
     WarningFilePath is set to application path. }
  WarningFilePath : string;
{$ENDIF}

// Misc. global functions
  function IsCgProfileSupported(Profile: TcgProfile): boolean;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
  CgBoolean : array[false..true] of TCGbool = (CG_FALSE, CG_TRUE);

var
  vCgContextCount : Integer;
  CurCgProgram    : TCgProgram;  // for reporting error
{$IFDEF OutputCompilerWarnings}
  CompilerMsg     : TStringList; // useful for seeing compiler warnings
{$ENDIF}


// IsCgProfileSupported
//
function IsCgProfileSupported(Profile: TcgProfile): boolean;
begin
  result:=cgGLIsProfileSupported(Profile)=CG_TRUE;
end;

{$IFDEF OutputCompilerWarnings}
procedure RecordWarnings;
begin
  with CurCgProgram do
    CompilerMsg.Add('[' + LongName + '] ' + cgGetErrorString(cgGetError) + #10 + cgGetLastListing(FCgContext));
end;
{$ENDIF}

procedure ErrorCallBack; cdecl;
var  Msg : string;
begin
  with CurCgProgram do
    Msg:='[' + LongName + '] ' + String(cgGetErrorString(cgGetError)) + #10 + String(cgGetLastListing(FCgContext));
  raise EGLCGShaderException.Create(Msg);
end;

// ------------------
// ------------------ TCgProgram ------------------
// ------------------

// Create
//
constructor TCgProgram.Create(AOwner: TPersistent);
begin
  inherited;
  FCode := TStringList.Create;
  TStringList(FCode).OnChange := NotifyChange;
  FParams := TList.Create;
  FEnabled := true;
  FDetectProfile := true;
end;

// Destroy
//
destructor TCgProgram.Destroy;
begin
  inherited Destroy;
  Assert((FParams.Count=0), '[' + LongName + ']: bug! params unbound!');
  ClearParamsList;
  FParams.Free;
  FCode.Free;
end;

// SetCode
//
procedure TCgProgram.SetCode(const val : TStrings);
begin
  FCode.Assign(val);
end;

 
//
procedure TCgProgram.LoadFromFile(const fileName : String);
begin
  Code.LoadFromFile(fileName);
end;

// SetProgramName
//
procedure TCgProgram.SetProgramName(const val : String);
begin
  if val<>FProgramName then
  begin
    FProgramName:=val;
    if not GetManualNotification then
       NotifyChange(Self);
  end;
end;

// AddParamsItem
//
procedure TCgProgram.AddParamsItem(const Param: PCGParameter);
var
  newParamObj : TCgParameter;
begin
  newParamObj := TCgParameter.Create;
  with newParamObj do begin
    FOwner := Self;
    FName  := {StrPas}String(cgGetParameterName(Param));
    FHandle := Param;
    FValueType := cgGetParameterType(Param);
    FDirection := cgGetParameterDirection(Param);
    FVariability := cgGetParameterVariability(Param);
  end;
  FParams.Add(newParamObj);
end;

// BuildParamsList
//
procedure TCgProgram.BuildParamsList;
var
  CurParam : PCGParameter;
begin
  ClearParamsList;
  CurParam:=cgGetFirstLeafParameter(FHandle, CG_PROGRAM);

  // build params list
  while Assigned(CurParam) do begin
    AddParamsItem(CurParam);
    CurParam:=cgGetNextLeafParameter(CurParam);
  end;
end;

// ClearParamsList
//
procedure TCgProgram.ClearParamsList;
var
  i : Integer;
begin
  for i:=FParams.Count-1 downto 0 do
    TCgParameter(FParams[i]).Free;
  FParams.Clear;
end;

// GetParam
//
function TCgProgram.GetParam(index : String) : TCgParameter;
begin
  Result := ParamByName(index);
  Assert(Result<>nil, '['+LongName+']: Parameter "'+index+'" not found.');
end;

// ParamByName
//
function TCgProgram.ParamByName(const name : String) : TCgParameter;
var
  i : Integer;
begin
  Result := nil;
  for i:=0 to FParams.Count-1 do begin
    if TCgParameter(FParams.Items[i]).Name=name then begin
      Result := TCgParameter(FParams.Items[i]);
      Exit;
    end;
  end;
end;

// DirectParamByName
//
function TCgProgram.DirectParamByName(const name: String): PCGparameter;
begin
  result:=cgGetNamedParameter(FHandle, PCharCG(StringCG(name)));
end;

// ParamCount
//
function TCgProgram.ParamCount : Integer;
begin
  Result:=FParams.Count;
end;

// Initialize
//
procedure TCgProgram.Initialize;
var
  buf : StringCG;
  Arg : array of PCharCG;
  PArg : PPCharCG;
begin
  Assert(FCgContext=nil);

  buf := StringCG(Trim(Code.Text));
  if buf='' then exit;

  if Precision=psFast then begin
      setlength(Arg, 2);
      Arg[0]:=PCharCG('-fastprecision');
      Arg[1]:=nil;
      PArg:=@Arg[0];
    end
  else
    PArg:=nil;

// To force 'if' statement, use sth. like:
//      setlength(Arg, 3);
//      Arg[0]:=PChar('-ifcvt');
//      Arg[1]:=PChar('none');
//      Arg[2]:=nil;
//      PArg:=@Arg[0];

  // get a new context
  FCgContext := cgCreateContext;
  Inc(vCgContextCount);
  CurCgProgram:=self;
  try
    if IncludeFilePath<>'' then SetCurrentDir(IncludeFilePath);
    if FDetectProfile then FProfile:=GetLatestProfile;
    cgGLSetOptimalOptions(FProfile);
    if FProgramName='' then FProgramName:='main'; // default program name
    FHandle := cgCreateProgram( FCgContext, CG_SOURCE, PCharCG(buf), FProfile,
                                PCharCG(StringCG(FProgramName)), PArg);
    cgGLLoadProgram(FHandle);
    // build parameter list for the selected program
    BuildParamsList;
{$IFDEF OutputCompilerWarnings}
    RecordWarnings;
{$ENDIF}
  except
    cgDestroyContext(FCgContext);
    FCgContext := nil;
    Dec(vCgContextCount);
  raise;
  end;
end;

// Finalize
//
procedure TCgProgram.Finalize;
begin
  if not Assigned(FCgContext) then exit;

  FProgramName := '';
  ClearParamsList;
  cgDestroyContext(FCgContext);
  FCgContext := nil;
  FHandle := nil; // $added - 29/04/2006 - PhP
  Dec(vCgContextCount);
end;

// Apply
//
procedure TCgProgram.Apply(var rci : TGLRenderContextInfo; Sender : TObject);
begin
  if not Assigned(FHandle) then exit;
  if not FEnabled then exit;

  CurCgProgram:=self;

  cgGLBindProgram(FHandle);
  cgGLEnableProfile(FProfile);

  if Assigned(FOnApply) then FOnApply(Self, Sender);
end;

// UnApply
//
procedure TCgProgram.UnApply(var rci : TGLRenderContextInfo);
begin
  if not Assigned(FHandle) then exit;
  if not FEnabled then exit;

  if Assigned(FOnUnApply) then FOnUnApply(Self);

  cgGLDisableProfile(FProfile);
end;

// GetProfileString
//
function TCgProgram.GetProfileStringA: string;
begin
  result:=String(cgGetProfileString(FProfile));
end;

// ListParameters
//
procedure TCgProgram.ListParameters(Output: TStrings);
var i : integer;
begin
  Output.clear;
  for i:=0 to ParamCount-1 do
    output.add(TCgParameter(FParams[i]).Name);
end;

// ListCompilation
//
procedure TCgProgram.ListCompilation(Output: TStrings);

  procedure OutputAsTStrings(s : String);
  var i : integer;
  begin
    while Length(s) > 0 do begin
      I:=Pos(#10, s);
      if I = 0 then I:=255;
      Output.Add(Copy(s, 1, I-1));
      Delete(s, 1, I);
    end;
  end;

begin
  Output.BeginUpdate;
  Output.Clear;
  if FCgContext<>nil then
    OutputAsTStrings(String(cgGetProgramString(FHandle, CG_COMPILED_PROGRAM)))
  else
    Output.add('Cg program not yet initialized');
  Output.EndUpdate;
end;

// SetParam(*)
//
procedure TCgProgram.SetParam(ParamName: string; const Vector3fVal: TVector3f);
begin
  ParamByName(ParamName).SetAsVector3f(Vector3fVal);
end;

procedure TCgProgram.SetParam(ParamName: string; const Vector2fVal: TVector2f);
begin
  ParamByName(ParamName).SetAsVector2f(Vector2fVal);
end;

procedure TCgProgram.SetParam(ParamName: string; SingleVal: Single);
begin
  Param[ParamName].SetAsScalar(SingleVal);
end;

procedure TCgProgram.SetParam(ParamName: string; const Vector4fVal: TVector4f);
begin
  ParamByName(ParamName).SetAsVector4f(Vector4fVal);
end;

// SetStateMatrix
//
procedure TCgProgram.SetStateMatrix(ParamName: string; matrix, Transform: Cardinal);
begin
  ParamByName(ParamName).SetAsStateMatrix(matrix, Transform);
end;

// SetTexture
//
procedure TCgProgram.SetTexture(ParamName: string; TextureID: Cardinal);
begin
  ParamByName(ParamName).SetAsTexture(TextureID);
end;

// LongName
//
function TCgProgram.LongName: string;
const ProTypeStr : array[ptVertex..ptFragment] of string = ('VP', 'FP');
begin
 result:=(Owner as TCgShader).Name + '.' + ProTypeStr[FProgramType] + '.' + ProgramName;
end;

// SetPrecision
//
procedure TCgProgram.SetPrecision(const Value: TPrecisionSetting);
begin
  if FPrecision<>Value then begin
    FPrecision := Value;
    if not GetManualNotification then
       NotifyChange(Self);
  end;
end;

// GetManualNotification
//
function TCgProgram.GetManualNotification: Boolean;
begin
  Result := not Assigned(TStringList(FCode).OnChange);
end;

// SetManualNotification
//
procedure TCgProgram.SetManualNotification(const Value: Boolean);
begin
  if Value = GetManualNotification then Exit;
  if Value then
    TStringList(FCode).OnChange := nil
  else
    TStringList(FCode).OnChange := NotifyChange;
end;

// ------------------
// ------------------ TCgParameter ------------------
// ------------------

// Create
//
constructor TCgParameter.Create;
begin
   inherited;
end;

// Destroy
//
destructor TCgParameter.Destroy;
begin
   inherited;
end;

// LongName
//
function TCgParameter.LongName: string;
begin
  result:=Owner.LongName + '.' + FName;
end;

// TypeMismatchMessage
//
function TCgParameter.TypeMismatchMessage: string;
begin
  result:='[' + LongName + ']: Parameter type mismatch.';
end;

// CheckValueType
//
procedure TCgParameter.CheckValueType(aType : TCGtype);
begin
  Assert(aType=FValueType, TypeMismatchMessage);
end;

// CheckValueType
//
procedure TCgParameter.CheckValueType(const types : array of TCGtype);
  function DoCheck : Boolean;
  var
    i : Integer;
  begin
    Result := False;
    for i:=Low(types) to High(types) do
      if FValueType=types[i] then
      begin
        Result := True;
        Break;
      end;
  end;
begin
  Assert(DoCheck, TypeMismatchMessage);
end;

// CheckAll*Types

procedure TCgParameter.CheckAllScalarTypes;
begin
  CheckValueType([CG_FLOAT, CG_HALF, CG_FIXED, CG_BOOL]);
end;

procedure TCgParameter.CheckAllTextureTypes;
begin
  CheckValueType([CG_SAMPLER2D, CG_SAMPLER1D, CG_SAMPLERRECT, CG_SAMPLERCUBE, CG_SAMPLER3D]);
end;

procedure TCgParameter.CheckAllVector2fTypes;
begin
  CheckValueType([CG_FLOAT2, CG_HALF2, CG_FIXED2]);
end;

procedure TCgParameter.CheckAllVector3fTypes;
begin
  CheckValueType([CG_FLOAT3, CG_HALF3, CG_FIXED3]);
end;

procedure TCgParameter.CheckAllVector4fTypes;
begin
  CheckValueType([CG_FLOAT4, CG_HALF4, CG_FIXED4]);
end;

// SetAsScalar
//
procedure TCgParameter.SetAsScalar(const val : Single);
begin
  CheckAllScalarTypes;
  cgGLSetParameter1f(FHandle, val);
end;

procedure TCgParameter.SetAsScalar(const val: boolean);
const BoolToFloat : array[false..true] of single = (CG_FALSE, CG_TRUE);
begin
  SetAsScalar(BoolToFloat[val]);
end;

// SetAsVector*
//
procedure TCgParameter.SetAsVector2f(const val: TVector2f);
begin
  CheckAllVector2fTypes;
  cgGLSetParameter2fv(FHandle, @val);
end;

procedure TCgParameter.SetAsVector3f(const val: TVector3f);
begin
  CheckAllVector3fTypes;
  cgGLSetParameter3fv(FHandle, @val);
end;

procedure TCgParameter.SetAsVector4f(const val: TVector4f);
begin
  CheckAllVector4fTypes;
  cgGLSetParameter4fv(FHandle, @val);
end;

procedure TCgParameter.SetAsVector(const val: TVector2f);
begin
  SetAsVector2f(val);
end;

procedure TCgParameter.SetAsVector(const val: TVector3f);
begin
  SetAsVector3f(val);
end;

procedure TCgParameter.SetAsVector(const val: TVector4f);
begin
  SetAsVector4f(val);
end;

procedure TCgParameter.SetAsVector(const val: array of single);
begin
  case high(val) of
    0 : SetAsScalar(val[0]);
    1 : begin CheckAllVector2fTypes; cgGLSetParameter2fv(FHandle, @val); end;
    2 : begin CheckAllVector3fTypes; cgGLSetParameter3fv(FHandle, @val); end;
    3 : begin CheckAllVector4fTypes; cgGLSetParameter4fv(FHandle, @val); end;
  else
    assert(false, 'Vector length must be between 1 to 4');
  end;
end;

// SetAsTexture*
//
procedure TCgParameter.SetAsTexture(TextureID: Cardinal);
begin
  CheckAllTextureTypes;
  cgGLSetTextureParameter(FHandle, TextureID);
end;

procedure TCgParameter.SetAsTexture1D(TextureID: Cardinal);
begin
  CheckValueType(CG_SAMPLER1D);
  cgGLSetTextureParameter(FHandle, TextureID);
end;

procedure TCgParameter.SetAsTexture2D(TextureID: Cardinal);
begin
  CheckValueType(CG_SAMPLER2D);
  cgGLSetTextureParameter(FHandle, TextureID);
end;

procedure TCgParameter.SetAsTexture3D(TextureID: Cardinal);
begin
  CheckValueType(CG_SAMPLER3D);
  cgGLSetTextureParameter(FHandle, TextureID);
end;

procedure TCgParameter.SetAsTextureRECT(TextureID: Cardinal);
begin
  CheckValueType(CG_SAMPLERRECT);
  cgGLSetTextureParameter(FHandle, TextureID);
end;

procedure TCgParameter.SetAsTextureCUBE(TextureID: Cardinal);
begin
  CheckValueType(CG_SAMPLERCUBE);
  cgGLSetTextureParameter(FHandle, TextureID);
end;

// SetToTextureOf
//
procedure TCgParameter.SetToTextureOf(LibMaterial: TGLLibMaterial);
var TexType : TCGtype;
begin
  case LibMaterial.Material.Texture.Image.NativeTextureTarget of
    ttTexture2D : TexType:=CG_SAMPLER2D;
    ttTextureCUBE : TexType:=CG_SAMPLER2D;
    ttTextureRECT : TexType:=CG_SAMPLERRECT;
    ttTexture1D : TexType:=CG_SAMPLER1D;
    ttTexture3D : TexType:=CG_SAMPLER3D;
  else begin
      assert(false, 'Unknown texture target');
      TexType:=CG_SAMPLER2D; // to subpress compilation warning
    end;
  end;

  CheckValueType(TexType);

  cgGLSetTextureParameter(FHandle, LibMaterial.Material.Texture.Handle);
end;

// DisableTexture
//
procedure TCgParameter.DisableTexture;
begin
  CheckAllTextureTypes;
  cgGLDisableTextureParameter(FHandle);
end;

// EnableTexture
//
procedure TCgParameter.EnableTexture;
begin
  CheckAllTextureTypes;
  cgGLEnableTextureParameter(FHandle);
end;

// SetAsStateMatrix
//
procedure TCgParameter.SetAsStateMatrix(matrix, Transform : Cardinal);
// Assuming values of matrix types are contiguous to simplify the type checking
const
  MinFloatA = CG_FLOAT1x1; MaxFloatA = CG_FLOAT4x4;
  MinHalfA  = CG_HALF1x1;  MaxHalfA  = CG_HALF4x4;
  MinFixedA = CG_FIXED1x1; MaxFixedA = CG_FIXED4x4;
begin
  Assert( ( (FValueType>=MinFloatA) and (FValueType<=MaxFloatA) or
            (FValueType>=MinHalfA)  and (FValueType<=MaxHalfA)  or
            (FValueType>=MinFixedA) and (FValueType<=MaxFixedA) ), TypeMismatchMessage);
  cgGLSetStateMatrixParameter( Fhandle, matrix, Transform);
end;

procedure TCgParameter.SetAsMatrix(const val: TMatrix4f);
begin
  cgGLSetMatrixParameterfr(FHandle, @val);
end;

// DisableClientState
//
procedure TCgParameter.DisableClientState;
begin
  assert(FVariability = CG_VARYING);
  cgGLDisableClientState(FHandle);
end;

// EnableClientState
//
procedure TCgParameter.EnableClientState;
begin
  assert(FVariability = CG_VARYING);
  cgGLEnableClientState(FHandle);
end;

// SetParameterPointer
//
procedure TCgParameter.SetParameterPointer(Values: TAffineVectorList);
begin
  assert(FVariability = CG_VARYING);
  cgGLSetParameterPointer(FHandle, 3, GL_FLOAT, 0, Values.List);
end;

procedure TCgParameter.SetParameterPointer(Values: TVectorList);
begin
  assert(FVariability = CG_VARYING);
  cgGLSetParameterPointer(FHandle, 4, GL_FLOAT, 0, Values.List);
end;

// ------------------
// ------------------ TCgVertexProgram ------------------
// ------------------

// Create
//
constructor TCgVertexProgram.Create;
begin
  inherited;
  FProgramType := ptVertex;
  FVPProfile:=vpDetectLatest;
end;

// GetLatestProfile
//
function TCgVertexProgram.GetLatestProfile: TcgProfile;
begin
  result:=cgGLGetLatestProfile(CG_GL_VERTEX);
end;

procedure TCgVertexProgram.SetVPProfile(v: TCgVPProfile);
begin
  if FVPProfile=v then exit;
  FVPProfile:=v;
  case v of
    vp20   : FProfile := CG_PROFILE_VP20;
    vp30   : FProfile := CG_PROFILE_VP30;
    vp40   : FProfile := CG_PROFILE_VP40;    
    arbvp1 : FProfile := CG_PROFILE_ARBVP1;
  end;

  FDetectProfile:=v=vpDetectLatest;
end;

// ------------------
// ------------------ TCgFragmentProgram ------------------
// ------------------

// Create
//
constructor TCgFragmentProgram.Create;
begin
  inherited;
  FProgramType := ptFragment;
  FFPProfile:=fpDetectLatest;
  FManageTexture:=false;
end;

// SetManageTexture
//
procedure TCgFragmentProgram.SetManageTexture(const Value: boolean);
begin
  FManageTexture:=Value;
  if FCgContext<>nil then
    cgGLSetManageTextureParameters(@FCgContext, CgBoolean[FManageTexture]);
// If FCgContext = nil (i.e. program not yet initialized), set it in
// TCgFragmentProgram.Initialize
end;

// Initialize
//
procedure TCgFragmentProgram.Initialize;
begin
  inherited;
  if FManageTexture then // ManageTexture is off by default
    cgGLSetManageTextureParameters(@FCgContext, CgBoolean[FManageTexture]);
end;

// GetLatestProfile
//
function TCgFragmentProgram.GetLatestProfile: TcgProfile;
begin
  result:=cgGLGetLatestProfile(CG_GL_FRAGMENT);
end;

// SetFPProfile
//
procedure TCgFragmentProgram.SetFPProfile(v: TCgFPProfile);
begin
  if FFPProfile=v then exit;
  FFPProfile:=v;
  case v of
    fp20   : FProfile := CG_PROFILE_FP20;
    fp30   : FProfile := CG_PROFILE_FP30;
    fp40   : FProfile := CG_PROFILE_FP40;
    arbfp1 : FProfile := CG_PROFILE_ARBFP1;
  end;
  FDetectProfile:= v=fpDetectLatest;
end;

// ------------------
// ------------------ TCustomCgShader ------------------
// ------------------

// Create
//
constructor TCustomCgShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVertexProgram := TCgVertexProgram.Create(Self);
  FFragmentProgram := TCgFragmentProgram.Create(Self);
end;

// Destroy
//
destructor TCustomCgShader.Destroy;
begin
  inherited Destroy;
  FVertexProgram.Free;
  FFragmentProgram.Free;
end;

// SetVertexProgram
//
procedure TCustomCgShader.SetVertexProgram(const val : TCgVertexProgram);
begin
  FVertexProgram.Code := val.Code;
end;

// SetFragmentProgram
//
procedure TCustomCgShader.SetFragmentProgram(const val : TCgFragmentProgram);
begin
  FFragmentProgram.Code := val.Code;
end;

// SetOnApplyVertexProgram
//
procedure TCustomCgShader.SetOnApplyVertexProgram(const val : TCgApplyEvent);
begin
  FVertexProgram.OnApply := val;
end;

// GetOnApplyVertexProgram
//
function TCustomCgShader.GetOnApplyVertexProgram : TCgApplyEvent;
begin
  Result:=FVertexProgram.OnApply;
end;

// SetOnApplyFragmentProgram
//
procedure TCustomCgShader.SetOnApplyFragmentProgram(const val : TCgApplyEvent);
begin
  FFragmentProgram.OnApply:=val;
end;

// GetOnApplyFragmentProgram
//
function TCustomCgShader.GetOnApplyFragmentProgram : TCgApplyEvent;
begin
  Result:=FFragmentProgram.OnApply;
end;

// SetOnUnApplyVertexProgram
//
procedure TCustomCgShader.SetOnUnApplyVertexProgram(const val : TCgUnApplyEvent);
begin
  FVertexProgram.OnUnApply := val;
end;

// GetOnUnApplyVertexProgram
//
function TCustomCgShader.GetOnUnApplyVertexProgram : TCgUnApplyEvent;
begin
  Result:=FVertexProgram.OnUnApply;
end;

// SetOnUnApplyFragmentProgram
//
procedure TCustomCgShader.SetOnUnApplyFragmentProgram(const val : TCgUnApplyEvent);
begin
  FFragmentProgram.OnUnApply:=val;
end;

// GetOnUnApplyFragmentProgram
//
function TCustomCgShader.GetOnUnApplyFragmentProgram : TCgUnApplyEvent;
begin
  Result:=FFragmentProgram.OnUnApply;
end;

// GetOnInitialize
//
function TCustomCgShader.GetOnInitialize: TCgShaderEvent;
begin
  Result:=FOnInitialize;
end;

// SetOnInitialize
//
procedure TCustomCgShader.SetOnInitialize(const val: TCgShaderEvent);
begin
  FOnInitialize:=val;
end;

// DoInitialize
//
procedure TCustomCgShader.DoInitialize(var rci : TGLRenderContextInfo; Sender : TObject);
begin
  if (csDesigning in ComponentState) and (not FDesignEnable) then
    Exit;

  if not ShaderSupported then
  begin
    Enabled := False;
    HandleFailedInitialization;
  end
  else
    try
      FVertexProgram.Initialize;
      FFragmentProgram.Initialize;
      if Assigned(FOnInitialize) then FOnInitialize(Self);
    except
      on E: Exception do
      begin
        Enabled := False;
        HandleFailedInitialization(E.Message);
      end;
    end;
end;

// DoApply
//
procedure TCustomCgShader.DoApply(var rci : TGLRenderContextInfo; Sender : TObject);
begin
  if (csDesigning in ComponentState) and (not FDesignEnable) then
    Exit;
  FVertexProgram.Apply(rci, Sender);
  FFragmentProgram.Apply(rci, Sender);
end;

// DoUnApply
//
function TCustomCgShader.DoUnApply(var rci : TGLRenderContextInfo) : Boolean;
begin
  if (not (csDesigning in ComponentState)) or FDesignEnable then
  begin
    FVertexProgram.UnApply(rci);
    FFragmentProgram.UnApply(rci);
  end;
  Result := False;
end;

// DoFinalize
//
procedure TCustomCgShader.DoFinalize;
begin
  FVertexProgram.Finalize;
  FFragmentProgram.Finalize;
end;

// LoadShaderPrograms
//
procedure TCustomCgShader.LoadShaderPrograms(const VPFilename,
  FPFilename: string);
begin
  VertexProgram.LoadFromFile(VPFilename);
  FragmentProgram.LoadFromFile(FPFilename);
end;

// IsProfileSupported
//
function TCustomCgShader.IsProfileSupported(Profile: TcgProfile): boolean;
begin
  result:=cgGLIsProfileSupported(Profile)=CG_TRUE;
end;

// ShaderSupported
//
function TCustomCgShader.ShaderSupported: Boolean;
begin
  Result := (GL.ARB_shader_objects and GL.ARB_vertex_program and
             GL.ARB_vertex_shader and GL.ARB_fragment_shader);
end;


// ------------------
// ------------------ TCadencableCustomCgShader ------------------
// ------------------

// DoInitialize
//
procedure TCadencableCustomCgShader.DoInitialize(var rci : TGLRenderContextInfo; Sender : TObject);
begin
  if FCadencer = nil then
  begin
    Enabled := False;
    raise EGLCGShaderException.CreateFmt(glsErrorEx + glsCadencerNotDefinedEx, [ClassName]);
  end
  else
    inherited;
end;

// Notification
//
procedure TCadencableCustomCgShader.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent is TGLCadencer) and (Operation = opRemove) then
  begin
    FCadencer := nil;
    Enabled := False;
  end;
end;

// SetCadencer
//
procedure TCadencableCustomCgShader.SetCadencer(const Value: TGLCadencer);
begin
  if Value = FCadencer then Exit;

  if Value = nil then
    if Enabled then
      Enabled := False;

  if FCadencer <> nil then
    FCadencer.RemoveFreeNotification(Self);
  FCadencer := Value;
  if FCadencer <> nil then
    FCadencer.FreeNotification(Self);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  // class registrations
  RegisterClasses([TCgShader, TCustomCgShader, TCadencableCustomCgShader,
                   TCgFragmentProgram, TCgVertexProgram, TCgProgram]);

  cgSetErrorCallBack(ErrorCallBack);

{$IFDEF OutputCompilerWarnings}
  CompilerMsg:=TStringList.Create;
  // default WarningFilePath is set to app. path
  WarningFilePath := extractfilepath(Application.ExeName);
{$ENDIF}

finalization
{$IFDEF OutputCompilerWarnings}
  CompilerMsg.SaveToFile(WarningFilePath + 'CG_Warnings.txt');
  CompilerMsg.free;
{$ENDIF}

end.

