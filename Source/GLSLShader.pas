//
// This unit is part of the GLScene Project, http://glscene.org
//
{
    TGLSLShader is a wrapper for GLS shaders.

	 History :  
       09/02/13 - Yar - Added OnApplyEx, OnInitializeEx events where is TGLLibMaterial as Sender (thanks to Dmitriy Buharin)
       10/11/12 - PW - Added CPP compatibility: changed vector arrays to records
       18/02/11 - Yar - Fixed transform feedback varyings activation
       23/08/10 - Yar - Replaced OpenGL1x to OpenGLTokens
       02/06/10 - Yar - Replace OpenGL functions to OpenGLAdapter
                           Added unsigned integer uniforms
       22/04/10 - Yar - Fixes after GLState revision
       02/04/10 - Yar -  Added GetActiveAttribs to TGLCustomGLSLShader
       04/11/09 - DaStr - Added default value to TGLCustomGLSLShader.TransformFeedBackMode
       26/10/09 - DaStr - Updated GeometryShader support (thanks YarUnderoaker)
       24/08/09 - DaStr - Added GeometryShader support (thanks YarUnderoaker)
       24/07/09 - DaStr - Added support for TGLCustomShader.DebugMode
                             Fixed spelling mistake in TGLShaderUnAplyEvent
                             TGLShader.DoInitialize() now passes rci
                              (BugTracker ID = 2826217)
                             Bugfixed TGLCustomGLSLShader.DoInitialize() - now
                              shader cleanes up correctly if failed to initialize
       15/03/08 - DaStr - Fixups for vIgnoreContextActivationFailures mode
                                                      (BugTracker ID = 1914782)
       25/12/07 - DaStr - Fix-up for previous update (BugtrackerID = 1772477)
       12/08/07 - LC -    TGLSLShaderParameter.SetAsCustomTexture now restores
                              the active texture unit (BugtrackerID = 1772477)
       12/07/07 - DaStr - TGLSLInitializedShaderParameters removed because
                              even if implemented, it could not give
                              a significant performance increase
       30/03/07 - fig -   Changed OnInitialize event to be fired after
                              linking, but before validation. This can now be
                              used to set texture units for different sampler
                              types (1D/2D/3D) before validation, which fixes
                              a bug (or complies to strict validation) with ATI
                              drivers
       30/03/07 - DaStr - Bugfixed TGLCustomGLSLShader.DoUnApply
                              (Result was not initialized)
       20/03/07 - DaStr - TGLCustomGLSLShader now generates its own events
                             Added TGLSLShaderParameter
                             Added TGLCustomGLSLShader.DoInitialPass
                             Added TGLCustomGLSLShader.Param[]
       21/02/07 - DaStr - Initial version (contributed to GLScene)



    Previous version history:
      v1.0    11 March     '2006  Creation
      v1.1    06 August    '2006  TGLCustomGLSLShader.DoInitialize bugfixed
      v1.1.2  24 August    '2006  TGLCustomShader.SetParameterTexture[1-3]D added
      v1.1.4  09 September '2006  Fixed a memory leak which occured when
                                   enabling / disabling the shader several times
      v1.1.6  22 September '2006  DoUnApply fixed (suggested by Nelsol Chu)
      v1.2    04 November  '2006  function GetGLSLProg added (just in case)
                                  TGLSLShader has more published properties
                                  Bugfix in DoInitialize (when no shader is active)
                                  (Get/Set)ParameterTexture[1/2/3]DHandle added
                                  (Get/Set)ParameterCustomTextureHandle support added
      v1.2.4  22 November  '2006  TGLProgramHandle.Name is now used
                                  Assign() bugfixed
                                  Fixed a possible bug in DoInitialize
                                    (Handle was freed, but not nil'ed)

}
unit GLSLShader;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,

  GLVectorGeometry, GLVectorTypes, GLTexture, OpenGLTokens, GLContext, GLCustomShader,
  GLRenderContextInfo, GLTextureFormat, GLSLParameter;

type
  TGLSLShaderParameter = class;
  TGLCustomGLSLShader = class;
  EGLSLShaderException = class(EGLCustomShaderException);

  TGLSLShaderEvent = procedure(Shader: TGLCustomGLSLShader) of object;
  TGLSLShaderUnApplyEvent = procedure(Shader: TGLCustomGLSLShader;
                                     var ThereAreMorePasses: Boolean) of object;
  TGLSLShaderEventEx = procedure(Shader: TGLCustomGLSLShader;
    Sender: TObject) of object;

  TGLActiveAttrib = record
    Name: string;
    Size: GLInt;
    AType: TGLSLDataType;
    Location: Integer;
  end;

  TGLActiveAttribArray = array of TGLActiveAttrib;

  TGLCustomGLSLShader = class(TGLCustomShader)
  private
    FGLSLProg: TGLProgramHandle;
    FParam: TGLSLShaderParameter;
    FActiveVarying: TStrings;
    FTransformFeedBackMode: TGLTransformFeedBackMode;

    FOnInitialize: TGLSLShaderEvent;
    FOnApply: TGLSLShaderEvent;
    FOnUnApply: TGLSLShaderUnApplyEvent;
    FOnInitializeEx: TGLSLShaderEventEx;
    FOnApplyEx: TGLSLShaderEventEx;

    function GetParam(const Index: string): TGLSLShaderParameter;
    function GetDirectParam(const Index: Cardinal): TGLSLShaderParameter;
    procedure OnChangeActiveVarying(Sender: TObject);
  protected
    property OnApply: TGLSLShaderEvent read FOnApply write FOnApply;
    property OnUnApply: TGLSLShaderUnApplyEvent read FOnUnApply write FOnUnApply;
    property OnInitialize: TGLSLShaderEvent read FOnInitialize write FOnInitialize;
    property OnInitializeEx: TGLSLShaderEventEx read FOnInitializeEx write FOnInitializeEx;
    property OnApplyEx: TGLSLShaderEventEx read FOnApplyEx write FOnApplyEx;

    function GetGLSLProg: TGLProgramHandle; virtual;
    function GetCurrentParam: TGLSLShaderParameter; virtual;
    procedure SetActiveVarying(const Value: TStrings);
    procedure SetTransformFeedBackMode(const Value: TGLTransformFeedBackMode);
    procedure DoInitialize(var rci: TGLRenderContextInfo; Sender: TObject); override;
    procedure DoFinalize; override;
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ShaderSupported: Boolean; override;
    function GetActiveAttribs: TGLActiveAttribArray;

    property Param[const Index: string]: TGLSLShaderParameter read GetParam;
    property DirectParam[const Index: Cardinal]: TGLSLShaderParameter read GetDirectParam;
    property ActiveVarying: TStrings read FActiveVarying write SetActiveVarying;
    property TransformFeedBackMode: TGLTransformFeedBackMode read FTransformFeedBackMode write SetTransformFeedBackMode default tfbmInterleaved;
  end;


  { Wrapper around a parameter of a GLSL program. }
  TGLSLShaderParameter = class(TGLCustomShaderParameter)
  private
     
    FGLSLProg: TGLProgramHandle;
    FParameterID: GLInt;
  protected
     
    function GetAsVector1f: Single; override;
    function GetAsVector2f: TVector2f; override;
    function GetAsVector3f: TVector3f; override;
    function GetAsVector4f: TVector; override;

    function GetAsVector1i: Integer; override;
    function GetAsVector2i: TVector2i; override;
    function GetAsVector3i: TVector3i; override;
    function GetAsVector4i: TVector4i; override;

    function GetAsVector1ui: GLuint; override;
    function GetAsVector2ui: TVector2ui; override;
    function GetAsVector3ui: TVector3ui; override;
    function GetAsVector4ui: TVector4ui; override;

    procedure SetAsVector1f(const Value: Single); override;
    procedure SetAsVector2f(const Value: TVector2f); override;
    procedure SetAsVector3f(const Value: TVector3f); override;
    procedure SetAsVector4f(const Value: TVector4f); override;

    procedure SetAsVector1i(const Value: Integer); override;
    procedure SetAsVector2i(const Value: TVector2i); override;
    procedure SetAsVector3i(const Value: TVector3i); override;
    procedure SetAsVector4i(const Value: TVector4i); override;

    procedure SetAsVector1ui(const Value: GLuint); override;
    procedure SetAsVector2ui(const Value: TVector2ui); override;
    procedure SetAsVector3ui(const Value: TVector3ui); override;
    procedure SetAsVector4ui(const Value: TVector4ui); override;

    function GetAsMatrix2f: TMatrix2f; override;
    function GetAsMatrix3f: TMatrix3f; override;
    function GetAsMatrix4f: TMatrix4f; override;
    procedure SetAsMatrix2f(const Value: TMatrix2f); override;
    procedure SetAsMatrix3f(const Value: TMatrix3f); override;
    procedure SetAsMatrix4f(const Value: TMatrix4f); override;

    function GetAsCustomTexture(const TextureIndex: Integer;
      TextureTarget: TGLTextureTarget): Cardinal; override;
    procedure SetAsCustomTexture(const TextureIndex: Integer;
      TextureTarget: TGLTextureTarget; const Value: Cardinal); override;

    function GetAsUniformBuffer: GLenum; override;
    procedure SetAsUniformBuffer( UBO: GLenum); override;

   public
     // Nothing here ...yet.
   end;

  TGLSLShader = class(TGLCustomGLSLShader)
  published
    property FragmentProgram;
    property VertexProgram;
    property GeometryProgram;    

    property OnApply;
    property OnApplyEx;
    property OnUnApply;
    property OnInitialize;
    property OnInitializeEx;

    property ShaderStyle;
    property FailedInitAction;

    property ActiveVarying;
    property TransformFeedBackMode;
  end;


implementation

uses
  GLState;

{ TGLCustomGLSLShader }

procedure TGLCustomGLSLShader.DoApply(var rci: TGLRenderContextInfo; Sender: TObject);
begin
  FGLSLProg.UseProgramObject;
  if Assigned(FOnApply) then
    FOnApply(Self);
  if Assigned(FOnApplyEx) then
    FOnApplyEx(Self, Sender);
end;


procedure TGLCustomGLSLShader.DoInitialize(var rci: TGLRenderContextInfo; Sender: TObject);
const
  cBufferMode: array[tfbmInterleaved..tfbmSeparate] of GLenum = (
    GL_INTERLEAVED_ATTRIBS_EXT, GL_SEPARATE_ATTRIBS_EXT);
var
  i, NumVarying: Integer;
  sVaryings: array of AnsiString;
  pVaryings: array of PGLChar;
begin
  try
    if not ShaderSupported then
      HandleFailedInitialization
    else
    try
      FGLSLProg.AllocateHandle;
      if FGLSLProg.IsDataNeedUpdate then
      begin
        if Name <> '' then
          FGLSLProg.Name := Name
        else
          FGLSLProg.Name := ClassName;

        FGLSLProg.DetachAllObject;
        if VertexProgram.Enabled then
          FGLSLProg.AddShader(TGLVertexShaderHandle, VertexProgram.Code.Text, FDebugMode);
        if FragmentProgram.Enabled then
          FGLSLProg.AddShader(TGLFragmentShaderHandle, FragmentProgram.Code.Text, FDebugMode);
        if GeometryProgram.Enabled then
          FGLSLProg.AddShader(TGLGeometryShaderHandle, GeometryProgram.Code.Text, FDebugMode);

        if VertexProgram.Enabled or FragmentProgram.Enabled or GeometryProgram.Enabled then
        begin
          if GeometryProgram.Enabled then
          begin
            GL.ProgramParameteri(FGLSLProg.Handle, GL_GEOMETRY_INPUT_TYPE_EXT,
              cGLgsInTypes[GeometryProgram.InputPrimitiveType]);
            GL.ProgramParameteri(FGLSLProg.Handle, GL_GEOMETRY_OUTPUT_TYPE_EXT,
              cGLgsOutTypes[GeometryProgram.OutputPrimitiveType]);
            GL.ProgramParameteri(FGLSLProg.Handle, GL_GEOMETRY_VERTICES_OUT_EXT,
              GeometryProgram.VerticesOut);
          end;

          NumVarying := FActiveVarying.Count;
          if NumVarying > 0 then
          begin
            // Activate varying
            SetLength(sVaryings, NumVarying);
            SetLength(pVaryings, NumVarying);
            for i := 0 to NumVarying - 1 do
            begin
              sVaryings[i] := AnsiString(FActiveVarying.Strings[i]) + #0;
              pVaryings[i] := PAnsiChar( sVaryings[i] );
            end;
            GL.TransformFeedbackVaryings(
              FGLSLProg.Handle, NumVarying, @pVaryings[0],
              cBufferMode[FTransformFeedBackMode] );
          end;

          if (not FGLSLProg.LinkProgram) then
            raise EGLSLShaderException.Create(FGLSLProg.InfoLog);
        end;
        FGLSLProg.NotifyDataUpdated;
      end;
    except
      on E: Exception do
      begin
        Enabled := False;
        HandleFailedInitialization(E.Message);
      end;
    end;

  finally
    if Enabled then
    try
      if Assigned(FOnInitialize) then
      begin
        FGLSLProg.UseProgramObject;
        FOnInitialize(Self);
        FGLSLProg.EndUseProgramObject;
      end;
      if Assigned(FOnInitializeEx) then
      begin
        FGLSLProg.UseProgramObject;
        FOnInitializeEx(Self, Sender);
        FGLSLProg.EndUseProgramObject;
      end;
      if (not FGLSLProg.ValidateProgram) then
        raise EGLSLShaderException.Create(FGLSLProg.InfoLog);
    except
      on E: Exception do
      begin
        Enabled := False;
        HandleFailedInitialization(E.Message);
      end;
    end;
  end;
end;


function TGLCustomGLSLShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnUnApply) then
    FOnUnApply(Self, Result);
  if not Result then
    FGLSLProg.EndUseProgramObject;
end;


function TGLCustomGLSLShader.ShaderSupported: Boolean;
begin
  Result := (GL.ARB_shader_objects and GL.ARB_vertex_program and
             GL.ARB_vertex_shader and GL.ARB_fragment_shader);
end;

function TGLCustomGLSLShader.GetActiveAttribs: TGLActiveAttribArray;
var
  LRci: TGLRenderContextInfo;
  i, j: Integer;
  buff: array[0..127] of AnsiChar;
  len: GLsizei;
  max: GLInt;
  glType: GLEnum;
begin
  DoInitialize(LRci, Self);

  SetLength(Result, 16);
  j := 0;
  if FGLSLProg.Handle<>0 then
  begin
    GL.GetProgramiv(FGLSLProg.Handle, GL_ACTIVE_ATTRIBUTES, @max);
    for i := 0 to 16 - 1 do
    if i<max then
    begin
      GL.GetActiveAttrib(FGLSLProg.Handle, i, Length(buff), @len, @Result[j].Size,
        @glType, @buff[0]);
      if glType > 0 then
        with Result[j] do
        begin
          case glType of
            GL_FLOAT: AType := GLSLType1F;
            GL_FLOAT_VEC2: AType := GLSLType2F;
            GL_FLOAT_VEC3: AType := GLSLType3F;
            GL_FLOAT_VEC4: AType := GLSLType4F;
            GL_INT: AType := GLSLType1I;
            GL_INT_VEC2: AType := GLSLType2I;
            GL_INT_VEC3: AType := GLSLType3I;
            GL_INT_VEC4: AType := GLSLType4I;
            GL_UNSIGNED_INT: AType := GLSLType1UI;
            GL_UNSIGNED_INT_VEC2: AType := GLSLType2UI;
            GL_UNSIGNED_INT_VEC3: AType := GLSLType3UI;
            GL_UNSIGNED_INT_VEC4: AType := GLSLType4UI;
            GL_BOOL: AType := GLSLType1I;
            GL_BOOL_VEC2: AType := GLSLType2I;
            GL_BOOL_VEC3: AType := GLSLType3I;
            GL_BOOL_VEC4: AType := GLSLType4I;
            GL_FLOAT_MAT2: AType := GLSLTypeMat2F;
            GL_FLOAT_MAT3: AType := GLSLTypeMat3F;
            GL_FLOAT_MAT4: AType := GLSLTypeMat4F;
          end;
          Name := Copy(string(buff), 0, len);
          Location := i;
          Inc(j);
        end;
    end;
  end;
  SetLength(Result, j);
end;

procedure TGLCustomGLSLShader.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TGLCustomGLSLShader then
  begin
    FreeAndNil(FGLSLProg); //just free the handle for it to be recreated on next initialization
  end;
end;

procedure TGLCustomGLSLShader.DoFinalize;
begin
  inherited;
  if Assigned(FGLSLProg) then
    FGLSLProg.NotifyChangesOfData;
end;

function TGLCustomGLSLShader.GetGLSLProg: TGLProgramHandle;
begin
  Result := FGLSLProg;
end;

function TGLCustomGLSLShader.GetParam(
  const Index: string): TGLSLShaderParameter;
begin
  FParam.FParameterID := FGLSLProg.GetUniformLocation(Index);
  Result := FParam;
end;

function TGLCustomGLSLShader.GetDirectParam(
  const Index: Cardinal): TGLSLShaderParameter;
begin
  FParam.FParameterID := Index;
  Result := FParam;
end;

function TGLCustomGLSLShader.GetCurrentParam: TGLSLShaderParameter;
begin
  Result := FParam;
end;

constructor TGLCustomGLSLShader.Create(AOwner: TComponent);
begin
  inherited;
  FGLSLProg := TGLProgramHandle.Create;
  FParam := TGLSLShaderParameter.Create;
  FParam.FGLSLProg := FGLSLProg;
  FActiveVarying := TStringList.Create;
  TStringList(FActiveVarying).OnChange := OnChangeActiveVarying;
  FTransformFeedBackMode := tfbmInterleaved;
end;

destructor TGLCustomGLSLShader.Destroy;
begin
  FreeAndNil(FGLSLProg);
  FreeAndNil(FParam);
  FreeAndNil(FActiveVarying);
  inherited;
end;

procedure TGLCustomGLSLShader.SetActiveVarying(const Value: TStrings);
begin
  FActiveVarying.Assign(Value);
  NotifyChange(Self);
end;

procedure TGLCustomGLSLShader.SetTransformFeedBackMode(const Value: TGLTransformFeedBackMode);
begin
  if Value <> FTransformFeedBackMode then
  begin
    FTransformFeedBackMode := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLCustomGLSLShader.OnChangeActiveVarying(Sender: TObject);
begin
  NotifyChange(Self);
end;

{ TGLSLShaderParameter }

function TGLSLShaderParameter.GetAsCustomTexture(
  const TextureIndex: Integer; TextureTarget: TGLTextureTarget): Cardinal;
begin
  GL.GetUniformiv(FGLSLProg.Handle, TextureIndex, @Result);
end;

function TGLSLShaderParameter.GetAsMatrix2f: TMatrix2f;
begin
  GL.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsMatrix3f: TMatrix3f;
begin
  GL.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsMatrix4f: TMatrix4f;
begin
  GL.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector1f: Single;
begin
  GL.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector1i: Integer;
begin
  GL.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector2f: TVector2f;
begin
  GL.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector2i: TVector2i;
begin
  GL.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector3f: TVector3f;
begin
  GL.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector3i: TVector3i;
begin
  GL.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector4f: TVector;
begin
  GL.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector4i: TVector4i;
begin
  GL.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TGLSLShaderParameter.SetAsCustomTexture(
  const TextureIndex: Integer; TextureTarget: TGLTextureTarget;
  const Value: Cardinal);
begin
  CurrentGLContext.GLStates.TextureBinding[TextureIndex, TextureTarget] := Value;
  GL.Uniform1i(FParameterID, TextureIndex);
end;

procedure TGLSLShaderParameter.SetAsMatrix2f(const Value: TMatrix2f);
begin
  GL.UniformMatrix2fv(FParameterID, 1, False, @Value);
end;

procedure TGLSLShaderParameter.SetAsMatrix3f(const Value: TMatrix3f);
begin
  GL.UniformMatrix3fv(FParameterID, 1, False, @Value);
end;

procedure TGLSLShaderParameter.SetAsMatrix4f(const Value: TMatrix4f);
begin
  GL.UniformMatrix4fv(FParameterID, 1, False, @Value);
end;

procedure TGLSLShaderParameter.SetAsVector1f(const Value: Single);
begin
  GL.Uniform1f(FParameterID, Value);
end;

procedure TGLSLShaderParameter.SetAsVector1i(const Value: Integer);
begin
  GL.Uniform1i(FParameterID, Value);
end;

procedure TGLSLShaderParameter.SetAsVector2f(const Value: TVector2f);
begin
  GL.Uniform2f(FParameterID, Value.V[0], Value.V[1]);
end;

procedure TGLSLShaderParameter.SetAsVector2i(const Value: TVector2i);
begin
  GL.Uniform2i(FParameterID, Value.V[0], Value.V[1]);
end;

procedure TGLSLShaderParameter.SetAsVector3f(const Value: TVector3f);
begin
  GL.Uniform3f(FParameterID, Value.V[0], Value.V[1], Value.V[2]);
end;

procedure TGLSLShaderParameter.SetAsVector3i(const Value: TVector3i);
begin
  GL.Uniform3i(FParameterID, Value.V[0], Value.V[1], Value.V[2]);
end;

procedure TGLSLShaderParameter.SetAsVector4f(const Value: TVector4f);
begin
  GL.Uniform4f(FParameterID, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
end;

procedure TGLSLShaderParameter.SetAsVector4i(const Value: TVector4i);
begin
  GL.Uniform4i(FParameterID, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
end;

function TGLSLShaderParameter.GetAsUniformBuffer: GLenum;
begin
  GL.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector1ui: GLuint;
begin
  GL.GetUniformuiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TGLSLShaderParameter.SetAsVector1ui(const Value: GLuint);
begin
  GL.Uniform1ui(FParameterID, Value);
end;

function TGLSLShaderParameter.GetAsVector2ui: TVector2ui;
begin
  GL.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TGLSLShaderParameter.SetAsVector2ui(const Value: TVector2ui);
begin
  GL.Uniform2ui(FParameterID, Value.V[0], Value.V[1]);
end;

function TGLSLShaderParameter.GetAsVector3ui: TVector3ui;
begin
  GL.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TGLSLShaderParameter.SetAsVector3ui(const Value: TVector3ui);
begin
  GL.Uniform3ui(FParameterID, Value.V[0], Value.V[1], Value.V[2]);
end;

function TGLSLShaderParameter.GetAsVector4ui: TVector4ui;
begin
  GL.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TGLSLShaderParameter.SetAsVector4ui(const Value: TVector4ui);
begin
  GL.Uniform4ui(FParameterID, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
end;

procedure TGLSLShaderParameter.SetAsUniformBuffer(UBO: Cardinal);
begin
  CurrentGLContext.GLStates.UniformBufferBinding := UBO;
  GL.UniformBuffer(FGLSLProg.Handle, FParameterID, UBO);
end;

initialization
  RegisterClasses([TGLCustomGLSLShader, TGLSLShader]);

end.
