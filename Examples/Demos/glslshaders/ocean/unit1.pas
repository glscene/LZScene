unit unit1;

interface

{$MODE Delphi}

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLViewer, GLScene, GLTexture, GLObjects,
  ComCtrls, GLContext, GLVectorGeometry, GLGeomObjects,
  GLCadencer, ExtCtrls, GLUserShader, GLGraph, GLSkydome,
  GLVectorLists, GLCrossPlatform, GLMaterial, GLCoordinates,
  GLRenderContextInfo, GLColor, OpenGLTokens;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera: TGLCamera;
    MatLib: TGLMaterialLibrary;
    GLLightSource1: TGLLightSource;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    GLSphere1: TGLSphere;
    DOInitialize: TGLDirectOpenGL;
    GLUserShader1: TGLUserShader;
    GLHeightField1: TGLHeightField;
    GLMemoryViewer1: TGLMemoryViewer;
    GLScene2: TGLScene;
    CameraCubeMap: TGLCamera;
    GLEarthSkyDome1: TGLEarthSkyDome;
    GLSphere2: TGLSphere;
    DOOceanPlane: TGLDirectOpenGL;
    procedure FormCreate(Sender: TObject);
    procedure DOInitializeRender(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure GLHeightField1GetHeight(const x, y: Single; var z: Single;
      var acolor: TColorVector; var texPoint: TTexPoint);
    procedure GLMemoryViewer1BeforeRender(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure GLUserShader1DoApply(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure GLUserShader1DoUnApply(Sender: TObject; Pass: Integer;
      var rci: TGLRenderContextInfo; var Continue: Boolean);
    procedure DOOceanPlaneRender(Sender: TObject;
      var rci: TGLRenderContextInfo);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    mx, my, dmx, dmy : Integer;
    programObject : TGLProgramHandle;
  end;

var
  Form1: TForm1;

implementation

uses
  GLTextureFormat, GLUtils;

{$R *.lfm}

const
   ocean_vp: string =
'#version 120' + #10#13 +
' uniform float Time;' + #10#13 +
' uniform vec4  EyePos;' + #10#13 +

' varying vec3   EyeVec;' + #10#13 +
' varying mat3 ObjToTangentSpace;' + #10#13 +

' const float cTexScale = 0.2;' + #10#13 +
' const float cBumpScale = 0.15;' + #10#13 +
' const float cBumpSpeed = 0.4;' + #10#13 +

 // Waves parameters

' #define NWAVES 3' + #10#13 +
' struct Wave {' + #10#13 +
'    float freq;' + #10#13 +  // 2*PI / wavelength
'    float amp;' + #10#13 +   // amplitude
'    float phase;' + #10#13 + // speed * 2*PI / wavelength
'    vec2 dir;' + #10#13 +
' };' + #10#13 +
' Wave wave[NWAVES];' + #10#13 +

' const float k = 2.0;' + #10#13 +

' Wave initWave(float freq, float amp, float phase, vec2 dir)' + #10#13 +
' {' + #10#13 +
'    Wave result;' + #10#13 +
'    result.freq = freq;' + #10#13 +
'    result.amp = amp;' + #10#13 +
'    result.phase = phase;' + #10#13 +
'    result.dir = dir;' + #10#13 +
'    return result;' + #10#13 +
' }' + #10#13 +

' vec2 evaluateWave(Wave w, vec2 pos, float t)' + #10#13 +
' {' + #10#13 +
'    vec2 result;' + #10#13 +

'    float wavePos = dot(w.dir, pos)*w.freq + t*w.phase;' + #10#13 +
'    float waveAmp = sin(wavePos)*0.5 + 0.5;' + #10#13 +

'    result.x = w.amp * pow(waveAmp, k);' + #10#13 +
'    result.y = k*w.freq*w.amp * pow(waveAmp, k-1.0) * cos(wavePos);' + #10#13 +

'    return result;' + #10#13 +
' }' + #10#13 +

' void main()' + #10#13 +
' {' + #10#13 +
'    vec4 P = gl_Vertex;' + #10#13 +

'    wave[0] = initWave( 0.2, 0.9, 12.0, vec2(1.0, 0.0) );' + #10#13 +
'    wave[1] = initWave( 0.3, 0.7, 9.0,  vec2(0.98, 0.2) );' + #10#13 +
'    wave[2] = initWave( 0.4, 0.5, 8.0,  vec2(0.99, -0.15) );' + #10#13 +

    // sum waves
'   vec2 dd = vec2(0.0, 0.0);' + #10#13 +
'   for(int i=0; i<NWAVES; i++) {' + #10#13 +
'      vec2 waveEval = evaluateWave(wave[i], P.xy, Time);' + #10#13 +
'      P.z += waveEval.x;' + #10#13 +
'      dd += waveEval.y * wave[i].dir;' + #10#13 +
'   }' + #10#13 +

'   gl_Position = gl_ModelViewProjectionMatrix * P;' + #10#13 +

    // compute tangent basis
'   vec3 B = vec3(1.0, 0.0, dd.x);' + #10#13 +
'   vec3 T = vec3(0.0, 1.0, dd.y);' + #10#13 +
'   vec3 N = vec3(-dd.x, -dd.y, 1);' + #10#13 +

    // compute the 3x3 tranform from tangent space to object space
    // first rows are the tangent and binormal scaled by the bump scale
'   ObjToTangentSpace[0] = cBumpScale * normalize(T);' + #10#13 +
'   ObjToTangentSpace[1] = cBumpScale * normalize(B);' + #10#13 +
'   ObjToTangentSpace[2] = normalize(N);' + #10#13 +

'   float texTime = Time*cBumpSpeed;' + #10#13 +
'   gl_TexCoord[0].xy = gl_Vertex.xy*cTexScale + texTime;' + #10#13 +
'   gl_TexCoord[1].xy = gl_Vertex.xy*(2.0*cTexScale) - texTime;' + #10#13 +

'   EyeVec = normalize(gl_Vertex.xyz - EyePos.xyz);' + #10#13 +
'}';

 ocean_fp =
'#version 120' + #10#13 +
'  uniform sampler2D NormalMap;' + #10#13 +
'  uniform samplerCube EnvironmentMap;' + #10#13 +

'  varying vec3 EyeVec;' + #10#13 +
'  varying mat3 ObjToTangentSpace;' + #10#13 +

'  const float cFresnelBias = 0.1;' + #10#13 +
'  const vec3 cDeepColor = vec3(0.0, 0.1, 0.2);' + #10#13 +
'  const vec3 cShallowColor = vec3(0.0, 0.2, 0.4);' + #10#13 +

'  const vec3 cNormalCorrection = vec3(-1.0, -1.0, 0.0);' + #10#13 +

'  void main()' + #10#13 +
'  {' + #10#13 +
   	// sum normal maps
'      vec3 t0 = texture2D(NormalMap, gl_TexCoord[0].xy).rgb;' + #10#13 +
'      vec3 t1 = texture2D(NormalMap, gl_TexCoord[1].xy).rgb;' + #10#13 +
'      vec3 normal = t0 + t1 + cNormalCorrection;' + #10#13 +

'      vec3 nW = normalize(ObjToTangentSpace * normal);' + #10#13 +

'      vec3 r = reflect(EyeVec, nW);' + #10#13 +
'      vec4 rColor = textureCube(EnvironmentMap, r);' + #10#13 +
'      float hdr = 1.0+5.0*rColor.a;' + #10#13 +

'      float  facing = 1.0-max(dot(normalize(-EyeVec), nW), 0.0);' + #10#13 +
'      vec3 waterColor = mix(cDeepColor, cShallowColor, facing);' + #10#13 +

'      float fresnel = cFresnelBias + (1.0-cFresnelBias)*pow(facing, 4.0);' + #10#13 +

'      gl_FragColor = vec4(waterColor + (fresnel*hdr)*rColor.rgb, 1.0);' + #10#13 +
'  }';


procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();

   // Load the cube map which is used both for environment and as reflection texture

   with matLib.LibMaterialByName('water').Material.Texture do begin
      Image.LoadFromFile(MediaPath+'\'+'noise.bmp');
   end;
   
   with matLib.LibMaterialByName('cubeMap').Material.Texture do begin
      ImageClassName:=TGLCubeMapImage.ClassName;
      with Image as TGLCubeMapImage do begin
         // Load all 6 texture map components of the cube map
         // The 'PX', 'NX', etc. refer to 'positive X', 'negative X', etc.
         // and follow the RenderMan specs/conventions
         Picture[cmtPX].LoadFromFile(MediaPath+'\'+'cm_left.jpg');
         Picture[cmtNX].LoadFromFile(MediaPath+'\'+'cm_right.jpg');
         Picture[cmtPY].LoadFromFile(MediaPath+'\'+'cm_top.jpg');
         Picture[cmtNY].LoadFromFile(MediaPath+'\'+'cm_bottom.jpg');
         Picture[cmtPZ].LoadFromFile(MediaPath+'\'+'cm_back.jpg');
         Picture[cmtNZ].LoadFromFile(MediaPath+'\'+'cm_front.jpg');
      end;
   end;

   SetCurrentDir(ExtractFilePath(Application.ExeName));
end;

procedure TForm1.DoInitializeRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  if not (GL.ARB_shader_objects and GL.ARB_vertex_program and GL.ARB_vertex_shader
    and GL.ARB_fragment_shader) then
  begin
    ShowMessage('Your hardware/driver doesn''t support GLSL and can''t execute this demo!');
    Halt;
  end;

  if DOInitialize.Tag <> 0 then
    Exit;
  DOInitialize.Tag := 1;

  GLSceneViewer1.Buffer.RenderingContext.Deactivate;
  GLMemoryViewer1.RenderCubeMapTextures(matLib.LibMaterialByName('cubeMap').Material.Texture);
  GLSceneViewer1.Buffer.RenderingContext.Activate;

  programObject := TGLProgramHandle.CreateAndAllocate;

  programObject.AddShader(TGLVertexShaderHandle, ocean_vp, True);
  programObject.AddShader(TGLFragmentShaderHandle, ocean_fp, True);

  if not programObject.LinkProgram then
    raise Exception.Create(programObject.InfoLog);

  if not programObject.ValidateProgram then
    raise Exception.Create(programObject.InfoLog);

  // initialize the heightmap
  with MatLib.LibMaterialByName('water') do
  begin
    PrepareBuildList;
    rci.GLStates.TextureBinding[0, ttTexture2D] := Material.Texture.Handle;
  end;

  // initialize the heightmap
  with MatLib.LibMaterialByName('cubeMap') do
  begin
    PrepareBuildList;
    rci.GLStates.TextureBinding[1, ttTextureCube] := Material.Texture.Handle;
  end;

  programObject.UseProgramObject;

  programObject.Uniform1i['NormalMap'] := 0;
  programObject.Uniform1i['EnvironmentMap'] := 1;

  programObject.EndUseProgramObject;

end;

procedure TForm1.GLUserShader1DoApply(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  camPos: TVector;
begin
  programObject.UseProgramObject;

  programObject.Uniform1f['Time'] := GLCadencer1.CurrentTime * 0.05;

  camPos := GLCamera.AbsolutePosition;
  programObject.Uniform4f['EyePos'] := camPos;
end;

procedure TForm1.GLUserShader1DoUnApply(Sender: TObject; Pass: Integer;
  var rci: TGLRenderContextInfo; var Continue: Boolean);
begin
  programObject.EndUseProgramObject;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    Inc(dmx, mx - x);
    Inc(dmy, my - y);
  end;
  mx := x;
  my := y;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if (dmx <> 0) or (dmy <> 0) then
  begin
    GLCamera.MoveAroundTarget(dmy * 0.3, dmx * 0.3);
    dmx := 0;
    dmy := 0;
  end;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := GLSceneViewer1.FramesPerSecondText;
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLHeightField1GetHeight(const x, y: Single; var z: Single;
      var acolor: TColorVector; var texPoint: TTexPoint);
begin
  z := 0;
end;

const
  cExtent = 200;
var
  vbo: TGLVBOArrayBufferHandle;
  nbVerts: Integer;

procedure TForm1.DOOceanPlaneRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  x, y: Integer;
  v: TTexPointList;
  cont: Boolean;
begin
  GLUserShader1DoApply(Self, rci);
  GL.EnableClientState(GL_VERTEX_ARRAY);

  if not Assigned(vbo) then
  begin
    v := TTexPointList.Create;

    v.Capacity := Sqr(cExtent + 1);
    y := -cExtent;
    while y < cExtent do
    begin
      x := -cExtent;
      while x <= cExtent do
      begin
        v.Add(y, x);
        v.Add(y + 2, x);
        Inc(x, 2);
      end;
      Inc(y, 2);
      v.Add(y, cExtent);
      v.Add(y, -cExtent);
    end;

    vbo := TGLVBOArrayBufferHandle.CreateAndAllocate();
    vbo.Bind;
    vbo.BufferData(v.List, v.DataSize, GL_STATIC_DRAW_ARB);
    nbVerts := v.Count;

    GL.VertexPointer(2, GL_FLOAT, 0, nil);
    GL.DrawArrays(GL_QUAD_STRIP, 0, nbVerts);

    vbo.UnBind;

    v.Free;
  end
  else
  begin
    vbo.Bind;

    GL.VertexPointer(2, GL_FLOAT, 0, nil);
    GL.DrawArrays(GL_TRIANGLE_STRIP, 0, nbVerts);

    vbo.UnBind;
  end;

  GL.DisableClientState(GL_VERTEX_ARRAY);
  GLUserShader1DoUnApply(Self, 0, rci, cont);
end;

procedure TForm1.GLMemoryViewer1BeforeRender(Sender: TObject);
begin
  GLMemoryViewer1.Buffer.RenderingContext.ShareLists(GLSceneViewer1.Buffer.RenderingContext);
end;

end.
