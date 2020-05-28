// This unit is part of the GLScene Project, http://glscene.org
//
{
   SEM shader : Spherical Environment Mapping
   The main idea of SEM is to get the UV coordinates (which are used to lookup the matCap texture)
   from the normal vector on the fragment instead of the original texture coordinates from the object.
   
   A material using SEM is very useful to highlight variations in the mesh: creases, bumps, even slow ondulations.
   It doesn't work that well on a cube, for instance. And does absolutely nothing on a sphere:
   SEM on a sphere is exactly the same as a planar projection of the matCap texture.

   At this time only one light source is supported

    History :  
     11/12/15 - J.Delauney - Creation

}


unit GLSLWaterShader;

interface

{$I ../GLScene.inc}

uses
  Classes,
   
  GLScene, GLCrossPlatform, OpenGLTokens, OpenGL1x, GLContext, GLRenderContextInfo,
  GLVectorGeometry, GLCoordinates,
  GLColor, GLTexture, GLMaterial,
  GLSLShader, GLCustomShader;

//TGLCustomGLSLSimpleWaterShader
//
{ Custom class for GLSLWaterShader. Very simple water shader }
Type
  TGLCustomGLSLWaterShader = class(TGLCustomGLSLShader)
  private

    FMaterialLibrary: TGLAbstractMaterialLibrary;

    FWaterTexture: TGLTexture;
    FWaterTexName   : TGLLibMaterialName;

    FWaterRefTexture: TGLTexture;
    FWaterRefTexName   : TGLLibMaterialName;

    FWaveTime : Single;
//    FSpecularPower: Single;
//    FLightPower: Single;

    function GetMaterialLibrary: TGLAbstractMaterialLibrary;

    procedure SetWaterTexTexture(const Value: TGLTexture);
    function GetWaterTexName: TGLLibMaterialName;
    procedure SetWaterTexName(const Value: TGLLibMaterialName);

    procedure SetWaterRefTexture(const Value: TGLTexture);
    function GetWaterRefTexName: TGLLibMaterialName;
    procedure SetWaterRefTexName(const Value: TGLLibMaterialName);


  protected
    procedure DoApply(var rci : TGLRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;

    procedure SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property WaveTime : Single Read FWaveTime Write FWaveTime;
    property MaterialLibrary: TGLAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
    property WaterTexture: TGLTexture read FWaterTexture write SetWaterTexTexture;
    property WaterTextureName: TGLLibMaterialName read GetWaterTexName write SetWaterTexName;
    property WaterRefTexture: TGLTexture read FWaterRefTexture write SetWaterRefTexture;
    property WaterRefTextureName: TGLLibMaterialName read GetWaterRefTexName write SetWaterRefTexName;

  end;

  TGLSLWaterShader = class(TGLCustomGLSLWaterShader)
  published

    property MaterialLibrary;
    property WaterTexture;
    property WaterTextureName;
    property WaterRefTexture;
    property WaterRefTextureName;

  end;
implementation

constructor TGLCustomGLSLWaterShader.Create(AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    clear;
    Add('uniform float waveTime; ');
    Add('varying vec3 vTexCoord; ');

    Add('void main() { ');
    Add('  gl_TexCoord[0] = gl_MultiTexCoord0; ');
    Add('  gl_TexCoord[1] = gl_MultiTexCoord1;');
    //Move the water...
    Add('gl_TexCoord[0].x += waveTime*0.25; ');
    Add('gl_TexCoord[0].y += waveTime-4.0; '); //Make the water move direction vary a little.
    // Normal in Eye Space
    Add('vec3 vEyeNormal = gl_NormalMatrix * gl_Normal; ');
    // Vertex position in Eye Space
    Add('vec4 vVert4 = gl_ModelViewMatrix * gl_Vertex; ');
    Add('vec3 vEyeVertex = normalize(vVert4.xyz / vVert4.w); ');
    Add('vec4 vCoords = vec4(reflect(vEyeVertex, vEyeNormal), 0.0); ');
    // Rotate by flipped camera
    Add('//vCoords = gl_ModelViewMatrixInverse * vCoords; ');
    Add('vTexCoord.xyz =normalize(vCoords.xyz); ');
    Add('  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
    clear;
    Add('uniform sampler2D waveTextureId; ');
    Add('uniform sampler2D waveTextureIdRef; ');
    Add('uniform float waveTime; ');
    Add('varying vec3 vTexCoord; ');

    Add('void main() { ');
    Add('  vec4 color1 = texture2D(waveTextureId, vec2(gl_TexCoord[0])); ');
    Add('  vec4 color2 = texture2D(waveTextureIdRef, vec2(vTexCoord)); ');


    Add('  gl_FragColor = 0.6 * vec4(color1 + color2) * vec4(0.0, 1.0, 1.0, 0.50); ');
    Add('} ');
  end;

  FWaveTime := 1.0;

end;

destructor TGLCustomGLSLWaterShader.Destroy;
begin

  inherited;
end;

procedure TGLCustomGLSLWaterShader.DoApply(var rci: TGLRenderContextInfo; Sender: TObject);
begin

  GetGLSLProg.UseProgramObject;


//  Param['SpecPower'].AsVector1f := FSpecularPower;
//  Param['LightIntensity'].AsVector1f := FLightPower;
  Param['waveTime'].AsVector1f := FWaveTime;
  Param['waveTextureId'].AsTexture2D[0] := FWaterTexture;
  Param['waveTextureIdRef'].AsTexture2D[1] := FWaterRefTexture;

end;

function TGLCustomGLSLWaterShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  gl.ActiveTexture(GL_TEXTURE0_ARB);
  //gl.ActiveTexture(GL_TEXTURE1_ARB);
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;


function TGLCustomGLSLWaterShader.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLCustomGLSLWaterShader.SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TGLAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TGLCustomGLSLWaterShader.SetWaterTexTexture(const Value: TGLTexture);
begin
  if FWaterTexture = Value then Exit;
  FWaterTexture := Value;
  NotifyChange(Self)
end;

function TGLCustomGLSLWaterShader.GetWaterTexName: TGLLibMaterialName;
begin
  Result := TGLMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FWaterTexture);
  if Result = '' then Result := FWaterTexName;
end;

procedure TGLCustomGLSLWaterShader.SetWaterTexName(const Value: TGLLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FWaterTexName = Value then Exit;
  FWaterTexName  := Value;

  FWaterTexture := TGLMaterialLibrary(FMaterialLibrary).TextureByName(FWaterTexName);
  NotifyChange(Self);
end;

procedure TGLCustomGLSLWaterShader.SetWaterRefTexture(const Value: TGLTexture);
begin
  if FWaterRefTexture = Value then Exit;
  FWaterRefTexture := Value;
  NotifyChange(Self)
end;

function TGLCustomGLSLWaterShader.GetWaterRefTexName: TGLLibMaterialName;
begin
  Result := TGLMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FWaterRefTexture);
  if Result = '' then Result := FWaterRefTexName;
end;

procedure TGLCustomGLSLWaterShader.SetWaterRefTexName(const Value: TGLLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FWaterRefTexName = Value then Exit;
  FWaterRefTexName  := Value;

  FWaterRefTexture := TGLMaterialLibrary(FMaterialLibrary).TextureByName(FWaterRefTexName);
  NotifyChange(Self);
end;



procedure TGLCustomGLSLWaterShader.Notification(AComponent: TComponent; Operation: TOperation);
var
  Index: Integer;
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FMaterialLibrary then
      if FMaterialLibrary <> nil then
      begin

        if FWaterTexture <> nil then
        begin
          Index := TGLMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FWaterTexture);
          if Index <> -1 then
            SetWaterTexTexture(nil);
        end;

        if FWaterRefTexture <> nil then
        begin
          Index := TGLMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FWaterRefTexture);
          if Index <> -1 then
            SetWaterRefTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;


end.
