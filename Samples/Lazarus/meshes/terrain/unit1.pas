{: Basic terrain rendering demo.<p>

   This demo showcases the TerrainRenderer, some of the SkyDome features
   and bits of 3D sound 'cause I got carried over ;)<br>
   The terrain HeightData is provided by a TGLBitmapHDS (HDS stands for
   "Height Data Source"), and displayed by a TGLTerrainRenderer.<p>

   The base terrain renderer uses a hybrid ROAM/brute-force approach to
   rendering terrain, by requesting height data tiles, then rendering them
   using either triangle strips (for those below "QualityDistance") or ROAM
   tessellation.<br>
   Note that if the terrain is wrapping in this sample (to reduce the required
   datasets size), the engine is *not* aware of it and does not exploit this
   fact in any way: it considers just an infinite terrain.<p>

   Controls:<ul>
   <li>Direction keys move the came nora (shift to speedup)
   <li>PageUp/PageDown move the camera up and down
   <li>Orient the camera freely by holding down the left button
   <li>Toggle wireframe mode with 'w'
   <li>Increase/decrease the viewing distance with '+'/'-'.
   <li>Increase/decrease CLOD precision with '*' and '/'.
   <li>Increase/decrease QualityDistance with '9' and '8'.
   <li>'n' turns on 'night' mode, 'd' turns back to 'day' mode.
   <li>Toggle star twinkle with 't' (night mode only)
   <li>'l' turns on/off the lens flares 
   </ul><p>

   When increasing the range, or moving after having increased the range you
   may notice a one-time slowdown, this originates in the base height data
   being duplicated to create the illusion of an "infinite" terrain (at max
   range the visible area covers 1024x1024 height samples, and with tiles of
   size 16 or less, this is a lot of tiles to prepare).<p>

   Misc. note: since the whole viewer is fully repainted at each frame,
   it was possible to set roNoColorBufferClear in the Viewer.Buffer.ContextOptions,
   which allows to gain a few more frames per second (try unsetting it). 
}
unit Unit1;

interface


uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLTerrainRenderer, GLObjects, GLHeightData, GLColor,
  ExtCtrls, GLCadencer, StdCtrls, GLTexture, GLHUDObjects, GLBitmapFont,
  GLSkydome, GLLCLViewer, GLVectorGeometry, GLLensFlare,
  GLCrossPlatform, GLMaterial, GLCoordinates, GLBaseClasses, GLState;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLBitmapHDS1: TGLBitmapHDS;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    TerrainRenderer1: TGLTerrainRenderer;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    BitmapFont1: TGLBitmapFont;
    HUDText1: TGLHUDText;
    SkyDome1: TGLSkyDome;
    SPMoon: TGLSprite;
    SPSun: TGLSprite;
    DCSound: TGLDummyCube;
    //GLSMBASS1: TGLSMBASS;
    TISound: TTimer;
    //GLSoundLibrary: TGLSoundLibrary;
    GLLensFlare: TGLLensFlare;
    GLDummyCube1: TGLDummyCube;
    InitialRenderPoint: TGLRenderPoint;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    //procedure TISoundTimer(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    mx, my : Integer;
    fullScreen : Boolean;
    FCamHeight : Single;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses GLKeyboard, GLUtils, LCLType;

procedure TForm1.FormCreate(Sender: TObject);
begin
   SetGLSceneMediaDir();
   // 8 MB height data cache
   // Note this is the data size in terms of elevation samples, it does not
   // take into account all the data required/allocated by the renderer
   GLBitmapHDS1.MaxPoolSize:=8*1024*1024;
   // specify height map data
   GLBitmapHDS1.Picture.LoadFromFile(MediaPath+'terrain.bmp');
   // load the texture maps
   GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile(MediaPath+'snow512.jpg');
   GLMaterialLibrary1.Materials[1].Material.Texture.Image.LoadFromFile(MediaPath+'detailmap.jpg');
   SPMoon.Material.Texture.Image.LoadFromFile(MediaPath+'moon.bmp');
   SPSun.Material.Texture.Image.LoadFromFile(MediaPath+'flare1.bmp');
   // apply texture map scale (our heightmap size is 256)
   TerrainRenderer1.TilesPerTexture:=256/TerrainRenderer1.TileSize;
   // load Bitmap Font
   BitmapFont1.Glyphs.LoadFromFile(MediaPath+'darkgold_font.bmp');
   // load and setup sound samples
 (*  with GLSoundLibrary.Samples do begin
      Add.LoadFromFile(MediaPath+'ChillyWind.mp3');
      Add.LoadFromFile(MediaPath+'howl.mp3');
   end;    *)
   // Could've been done at design time, but then it hurts the eyes ;)
   GLSceneViewer1.Buffer.BackgroundColor:=clWhite;
   // Move camera starting point to an interesting hand-picked location
   DummyCube1.Position.X:=570;
   DummyCube1.Position.Z:=-385;
   DummyCube1.Turn(90);
   // Initial camera height offset (controled with pageUp/pageDown)
   FCamHeight:=10;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   speed : Single;
begin
   // handle keypresses
   if IsKeyDown(VK_SHIFT) then
      speed:=5*deltaTime
   else speed:=deltaTime;
   with GLCamera1.Position do begin
      if IsKeyDown(VK_UP) then
         DummyCube1.Translate(Z*speed, 0, -X*speed);
      if IsKeyDown(VK_DOWN) then
         DummyCube1.Translate(-Z*speed, 0, X*speed);
      if IsKeyDown(VK_LEFT) then
         DummyCube1.Translate(-X*speed, 0, -Z*speed);
      if IsKeyDown(VK_RIGHT) then
         DummyCube1.Translate(X*speed, 0, Z*speed);
      if IsKeyDown(VK_PRIOR) then
         FCamHeight:=FCamHeight+10*speed;
      if IsKeyDown(VK_NEXT) then
         FCamHeight:=FCamHeight-10*speed;
      if IsKeyDown(VK_ESCAPE) then Close;
   end;
   // don't drop through terrain!
   with DummyCube1.Position do
      Y:=TerrainRenderer1.InterpolatedHeight(AsVector)+FCamHeight;
end;

// Standard mouse rotation & FPS code below

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x;
   my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then begin
      GLCamera1.MoveAroundTarget((my-y)*0.5, (mx-x)*0.5);
      mx:=x;
      my:=y;
   end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   HUDText1.Text:=Format('%.1f FPS - %d',
                         [GLSceneViewer1.FramesPerSecond, TerrainRenderer1.LastTriangleCount]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
   case Key of
      'w', 'W' : with GLMaterialLibrary1.Materials[0].Material do begin
         if PolygonMode=pmLines then
            PolygonMode:=pmFill
         else PolygonMode:=pmLines;
      end;
      '+' : if GLCamera1.DepthOfView<2000 then begin
         GLCamera1.DepthOfView:=GLCamera1.DepthOfView*1.2;
         with GLSceneViewer1.Buffer.FogEnvironment do begin
            FogEnd:=FogEnd*1.2;
            FogStart:=FogStart*1.2;
         end;
      end;
      '-' : if GLCamera1.DepthOfView>300 then begin
         GLCamera1.DepthOfView:=GLCamera1.DepthOfView/1.2;
         with GLSceneViewer1.Buffer.FogEnvironment do begin
            FogEnd:=FogEnd/1.2;
            FogStart:=FogStart/1.2;
         end;
      end;
      '*' : with TerrainRenderer1 do
         if CLODPrecision>20 then CLODPrecision:=Round(CLODPrecision*0.8);
      '/' : with TerrainRenderer1 do
         if CLODPrecision<1000 then CLODPrecision:=Round(CLODPrecision*1.2);
      '8' : with TerrainRenderer1 do
         if QualityDistance>40 then QualityDistance:=Round(QualityDistance*0.8);
      '9' : with TerrainRenderer1 do
         if QualityDistance<1000 then QualityDistance:=Round(QualityDistance*1.2);
      'n', 'N' : with SkyDome1 do if Stars.Count=0 then begin
         // turn on 'night' mode
         Bands[1].StopColor.AsWinColor:=RGB(0, 0, 16);
         Bands[1].StartColor.AsWinColor:=RGB(0, 0, 8);
         Bands[0].StopColor.AsWinColor:=RGB(0, 0, 8);
         Bands[0].StartColor.AsWinColor:=RGB(0, 0, 0);
         with Stars do begin
            AddRandomStars(700, clWhite, True);   // many white stars
            AddRandomStars(100, RGB(255, 200, 200), True);  // some redish ones
            AddRandomStars(100, RGB(200, 200, 255), True);  // some blueish ones
            AddRandomStars(100, RGB(255, 255, 200), True);  // some yellowish ones
         end;
         GLSceneViewer1.Buffer.BackgroundColor:=clBlack;
         with GLSceneViewer1.Buffer.FogEnvironment do begin
            FogColor.AsWinColor:=clBlack;
            FogStart:=-FogStart; // Fog is used to make things darker
         end;
         SPMoon.Visible:=True;
         SPSun.Visible:=False;
         GLLensFlare.Visible:=False;
      end;
      'd', 'D' : with SkyDome1 do if Stars.Count>0 then begin
         // turn on 'day' mode
         Bands[1].StopColor.Color:=clrNavy;
         Bands[1].StartColor.Color:=clrBlue;
         Bands[0].StopColor.Color:=clrBlue;
         Bands[0].StartColor.Color:=clrWhite;
         Stars.Clear;
         GLSceneViewer1.Buffer.BackgroundColor:=clWhite;
         with GLSceneViewer1.Buffer.FogEnvironment do begin
            FogColor.AsWinColor:=clWhite;
            FogStart:=-FogStart;
         end;
         GLSceneViewer1.Buffer.FogEnvironment.FogStart:=0;
         SPMoon.Visible:=False;
         SPSun.Visible:=True;
      end;
      't' : with SkyDome1 do begin
         if sdoTwinkle in Options then
            Options:=Options-[sdoTwinkle]
         else Options:=Options+[sdoTwinkle];
      end;
      'l' : with GLLensFlare do Visible:=(not Visible) and SPSun.Visible;
   end;
   Key:=#0;
end;


(* procedure TForm1.TISoundTimer(Sender: TObject);
var
   wolfPos : TVector;
   c, s : Single;
begin
   if not GLSMBASS1.Active then Exit;
   if SkyDome1.Stars.Count=0 then begin
      // wind blows around camera
      with GetOrCreateSoundEmitter(GLCamera1) do begin
         Source.SoundLibrary:=GLSoundLibrary;
         Source.SoundName:=GLSoundLibrary.Samples[0].Name;
         Source.Volume:=Random*0.5+0.5;
         Playing:=True;
      end;
   end else begin
      // wolf howl at some distance, at ground level
      wolfPos:=GLCamera1.AbsolutePosition;
      SinCos(Random*c2PI, 100+Random(1000), s, c);
      wolfPos.X:=wolfPos.X+c;
      wolfPos.Z:=wolfPos.Z+s;
      wolfPos.Y:=TerrainRenderer1.InterpolatedHeight(wolfPos);
      DCSound.Position.AsVector:=wolfPos;
      with GetOrCreateSoundEmitter(DCSound) do begin
         Source.SoundLibrary:=GLSoundLibrary;
         Source.SoundName:=GLSoundLibrary.Samples[1].Name;
         Source.MinDistance:=100;
         Source.MaxDistance:=4000;
         Playing:=True;
      end;
   end;
   TISound.Enabled:=False;
   TISound.Interval:=10000+Random(10000);
   TISound.Enabled:=True;
end; *)

// Test Code for InterpolatedHeight, use as a Button1's click event
{
procedure TForm1.Button1Click(Sender: TObject);
var
   x, y : Integer;
   sph : TGLSphere;
begin
   for x:=-5 to 5 do begin
      for y:=-5 to 5 do begin
         sph:=TGLSphere(GLScene1.Objects.AddNewChild(TGLSphere));
         sph.Position.X:=DummyCube1.Position.X+X*2;
         sph.Position.Z:=DummyCube1.Position.Z+Y*2;
         sph.Position.Y:=TerrainRenderer1.InterpolatedHeight(sph.Position.AsVector);
         sph.Radius:=0.5;
      end;
   end;
end; }

end.
