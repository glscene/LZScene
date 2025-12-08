unit umainform;

interface

uses
  LCLType,
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, fpimage,

  GLScene, GLCrossPlatform, GLLCLViewer, OpenGLTokens, OpenGLAdapter, GLContext,
  GLMaterial, GLCadencer, GLBitmapFont, GLWindowsFont, GLHUDObjects, GLCoordinates, GLObjects,
  GLVectorGeometry, GLRenderContextInfo,
  GLCustomShader, GLSLShader,GLUtils,// GLKeyBoard,
  GLCanvas,GLAsmShader, GLTexture;

type

  { TMainForm }

  TMainForm = class(TForm)
    Viewer: TGLSceneViewer;
    Scene: TGLScene;
    Cadencer: TGLCadencer;
    MaterialLibrary: TGLMaterialLibrary;
    DCCameras: TGLDummyCube;
    Camera1: TGLCamera;
    DriverInfo: TGLDummyCube;
    HUDText_Vendor: TGLResolutionIndependantHUDText;
    WindowsBitmapFont: TGLWindowsBitmapFont;
    HUDText_Renderer: TGLResolutionIndependantHUDText;
    HUDText_Version: TGLResolutionIndependantHUDText;
    HUDText_ExtVersion: TGLResolutionIndependantHUDText;
    HUDText_GLSLVersion: TGLResolutionIndependantHUDText;
    HUDText_NBExt: TGLResolutionIndependantHUDText;
    Background: TGLDummyCube;
    GLPlane1: TGLPlane;
    GLSLShader1: TGLSLShader;
    Timer1: TTimer;
    HUDText_Counter: TGLResolutionIndependantHUDText;
    ForeGround: TGLDummyCube;
    HUDLogo: TGLHUDSprite;
    HUDExtensions: TGLDirectOpenGL;
    Extra: TGLDummyCube;
    HUDHelp: TGLResolutionIndependantHUDText;   
    GLLightSource1: TGLLightSource;
    WindowsBitmapFont2: TGLWindowsBitmapFont;
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure GLSLShader1Apply(Shader: TGLCustomGLSLShader);
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure MaterialLibraryTextureNeeded(Sender: TObject;
      var textureFileName: string);
    procedure Timer1Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure HUDExtensionsRender(Sender: TObject; var rci: TGLRenderContextInfo);
   
  private
    { Déclarations privées }
    procedure HandleKeys(const deltaTime: Double);
  public
    { Déclarations publiques }
    VendorName  : String;
    RendererName : String;
    RendererVersion : String;
    ExtensionVersion : String;
    GLSLVersion : String;
    NBExtensions : String;
    ExtensionsList : TStrings;
    StartLine,MaxLines:Integer;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.HandleKeys(const deltaTime: Double);
begin
   //if IsKeyDown(VK_UP) then
   //begin
   //   if StartLine>0 then
   //     StartLine := StartLine - 1;
   //end
   //else
   //if IsKeyDown(VK_DOWN) then
   //begin
   //  if StartLine<(ExtensionsList.Count-1-MaxLines) then
   //     StartLine := StartLine + 1;
   //end;
   //
   //if IsKeyDown(VK_ESCAPE) then
   //begin
   //  Application.Terminate;
   //end

end;

procedure TMainForm.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  HandleKeys(deltaTime);
  Viewer.Invalidate;
end;

procedure TMainForm.MaterialLibraryTextureNeeded(Sender: TObject;
  var textureFileName: string);
begin

end;

procedure TMainForm.FormResize(Sender: TObject);
begin
 HUDLogo.Position.X := Viewer.Width - 150;
 HUDLogo.Position.Y := Viewer.Height - 70;
 maxlines:=round( ((Viewer.Height-30) - (Viewer.Height/3)) /12);
end;

procedure TMainForm.FormShow(Sender: TObject);
Var
 ExtStr:String;
 i : Integer;
 dc: HDC;
begin
 Timer1.Enabled:=true;
 Cadencer.Enabled:=true;
 HUDLogo.Position.X := Viewer.Width - 150;
 HUDLogo.Position.Y := Viewer.Height - 70;
 Viewer.Buffer.RenderingContext.Activate;
 try
   with Viewer.Buffer do
   begin
    VendorName :=  String(GL.GetString(GL_VENDOR));
    RendererName := String(GL.GetString(GL_RENDERER));
    RendererVersion := String(GL.GetString(GL_VERSION));
    ExtensionVersion := String(GL.GetString(GL_SHADING_LANGUAGE_VERSION_ARB));
    GLSLVersion :='#'+inttostr(GL_VERSION);
    ExtensionsList := TStringList.Create;
    ExtStr := String(GL.GetString(GL_EXTENSIONS));
    ExtensionsList.Clear;
    while Length(ExtStr) > 0 do
    begin
      i := Pos(' ', ExtStr);
      if i = 0 then
        i := 255;
      ExtensionsList.Add(Copy(ExtStr, 1, i - 1));
      Delete(ExtStr, 1, i);
    end;
    // Include WGL extensions
    {$IFDEF SUPPORT_WGL}
    if GL.W_ARB_extensions_string then
    begin
      dc := wglGetCurrentDC();
      ExtStr := String(GL.WGetExtensionsStringARB(dc));
      while Length(ExtStr) > 0 do
      begin
        i := Pos(' ', ExtStr);
        if i = 0 then
          i := 255;
        ExtensionsList.Add(Copy(ExtStr, 1, i - 1));
        Delete(ExtStr, 1, i);
      end;
    end;
    {$endif}
    NBExtensions := Inttostr(ExtensionsList.Count);
   end;
 finally
   HUDText_Vendor.Text      := 'Vendor             : ' + VendorName;
   HUDText_Renderer.Text    := 'Renderer           : ' + RendererName;
   HUDText_Version.Text     := 'Renderer Version   : ' + RendererVersion;
   HUDText_ExtVersion.Text  := 'OPENGL Version     : ' + ExtensionVersion;
   HUDText_GLSLVersion.Text := 'GLSL Version       : ' + GLSLVersion;
   HUDText_NbExt.Text       := 'Extensions supported : ' + NBExtensions;
 end;
 maxlines:=round( ((Viewer.Height-30) - (Viewer.Height/3)) /12);
 StartLine := 0;
 Viewer.Invalidate;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
 if (ord(key)=VK_UP) then
 begin
    if StartLine>0 then
      StartLine := StartLine - 1;
 end
 else
 if (ord(key)=VK_DOWN) then
 begin
   if StartLine<(ExtensionsList.Count-1-MaxLines) then
      StartLine := StartLine + 1;
 end;

 if (ord(key)=VK_ESCAPE) then
 begin
   Application.Terminate;
 end
end;

procedure TMainForm.GLSLShader1Apply(Shader: TGLCustomGLSLShader);
begin
  With Shader do
  begin
    param['resolution'].AsVector2f := Vector2fMake(viewer.Width, viewer.Height);
    param['time'].AsVector1f:= Cadencer.CurrentTime;
  end;
end;


procedure TMainForm.HUDExtensionsRender(Sender: TObject; var rci: TGLRenderContextInfo);
var
   i, x, ys,y : Integer;
   glc : TGLCanvas;
   r : TRect;
   acolor : TColor;
   ext:string;
begin
   ys:=round( Viewer.Height/3)-20;
   glc:=TGLCanvas.Create(Viewer.Width, Viewer.Height);

   with glc do
   begin
      PenWidth:=2;
      PenColor:=$00333333;
      PenAlpha:=0.7;
      r:=Rect(10,ys,Viewer.Width-10,Viewer.Height-30);
      FillRect(r.Left, r.Top, r.Right, r.Bottom);

      for i:=0 to MaxLines do
      begin
         acolor:=clWhite;
         x:=15;
         y:=ys+(12*i);
         ext:=ExtensionsList.Strings[StartLine+i];
         if pos('EXT',ext)>0 then acolor:=clSkyBlue;
         if pos('ARB',ext)>0 then acolor:=clYellow;
         if pos('NV',ext)>0 then acolor:=clMoneyGreen;
         if pos('WGL',ext)>0 then acolor:=clAqua;


         WindowsBitmapFont2.Font.Size :=8;
         WindowsBitmapFont2.TextOut(rci, x, y, ansitoutf8(ext), acolor);
      end;
   end;
   glc.Free;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
   // some stats
    HUDText_Counter.Text :=Format('%.2f FPS',[Viewer.FramesPerSecond]);
   Viewer.ResetPerformanceMonitor;
end;

end.
