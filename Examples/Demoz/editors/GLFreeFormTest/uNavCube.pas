unit uNavCube;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  windows, Classes, SysUtils,
  Graphics, Forms, Controls,
  // GLS
  GLScene, GLObjects, GLHUDObjects, GLGeomObjects, GLMaterial, GLTexture,
  GLTextureFormat, GLVectorGeometry, GLKeyboard, GLContext, GLBaseClasses,
  GLLCLViewer, GLRenderContextInfo, keyboard;

type
  TGLNavCube = class(TGLBaseSceneObject)
  private
    FDelta, FFps, FTimer, FInactiveTime: single;
    FCube: TGLDummyCube;
    FSel: Integer;
    FSelPos: TVector;
    FCam, FNavCam: TGLCamera;
    FHud: TGLHUDSprite;
    FMem: TGLMemoryViewer;
    FViewer: TGLSceneViewer;
    FReady, FMouse: boolean;
    FMouseRotation: boolean;
    FAutoRotate : Boolean;
    FMousePos: TPoint;

    FPosAnimationStart: TVector;
    FPosAnimationEnd: TVector;

  public
    constructor CreateAsChild(aParentOwner: TGLBaseSceneObject); reintroduce;
    procedure DoProgress(const pt: TProgressTimes); override;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: boolean); override;

    property SceneViewer: TGLSceneViewer read FViewer write FViewer;
    property Camera: TGLCamera read FCam write FCam;
    property FPS: single read FFps write FFps;

    property ActiveMouse: boolean read FMouse write FMouse;
    property AutoRotate: boolean read FAutoRotate write FAutoRotate;
    property InactiveTime: single read FInactiveTime write FInactiveTime;
  end;

var
  sW2, sH2: Integer;

implementation

// constructor
//
constructor TGLNavCube.CreateAsChild(aParentOwner: TGLBaseSceneObject);

procedure genTex(s: string; mat: TGLMaterial);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.Width := 64;
  bmp.Height := 64;
  with bmp.Canvas do
  begin
    Font.Name := 'Verdana';
    Font.Size := 10;
    TextOut(32 - TextWidth(s) div 2, 24, s);
  end;
  mat.FrontProperties.Diffuse.SetColor(1, 1, 1);
  mat.Texture.Image.Assign(bmp);
  mat.Texture.Disabled := false;
  mat.Texture.FilteringQuality := tfAnisotropic;
  mat.Texture.TextureMode := tmModulate;
  bmp.Free;
end;

procedure SetColor(m: TGLMaterial; c: single);
begin
  m.FrontProperties.Diffuse.SetColor(c, c, 1);
end;

procedure addPlane(t: Integer; ttl: string; c, x, y, z, dx, dy, dz: single);
begin
  with TGLPlane.CreateAsChild(FCube) do
  begin
    tag := t;
    tagfloat := c;
    Position.SetPoint(x, y, z);
    Direction.SetVector(dx, dy, dz);
    genTex(ttl, Material);
  end;
end;

procedure addCube(t: Integer; c, x, y, z, sx, sy, sz: single);
begin
  with TGLCube.CreateAsChild(FCube) do
  begin
    tag := t;
    tagfloat := c;
    Position.SetPoint(x, y, z);
    Scale.SetVector(sx, sy, sz);
    SetColor(Material, c);
  end;
end;

begin
  inherited CreateAsChild(aParentOwner);
  FDelta := 2;
  FFps := 30;
  FTimer := 10;
  FMouse := True;
  FAutoRotate := False;
  FInactiveTime := 0;

  FHud := TGLHUDSprite.CreateAsChild(self);
  FHud.Width := 128;
  FHud.Height := 128;
  FHud.Material.BlendingMode := bmTransparency;
  with FHud.Material.Texture do
  begin
    Disabled := false;
    ImageClassName := 'TGLBlankImage';
    MinFilter := miNearest;
    TGLBlankImage(Image).Width := 128;
    TGLBlankImage(Image).Height := 128;
    TextureMode := tmReplace;
  end;
  FHud.Position.SetPoint(-200, 50, 0);

  FNavCam := TGLCamera.CreateAsChild(self);
  FNavCam.FocalLength := 55;
  FNavCam.TargetObject := self;

  FMem := TGLMemoryViewer.Create(aParentOwner);
  FMem.Width := 128;
  FMem.Height := 128;
  FMem.Camera := FNavCam;
  with FMem.Buffer do
  begin
    BackgroundAlpha := 0;
    Antialiasing := aa6x;
    ContextOptions := [roDestinationAlpha];
    Lighting := false;
  end;

  FCube := TGLDummyCube.CreateAsChild(self);
  FCube.Visible := false;

  with TGLDisk.CreateAsChild(FCube) do
  begin
    Position.SetPoint(0, -0.805, 0);
    Direction.SetVector(0, 1, 0);
    InnerRadius := 0.9;
    OuterRadius := 1.3;
    Slices := 60;
    Loops := 1;
    SetColor(Material, 0.6);
  end;

  with TGLDisk.CreateAsChild(FCube) do
  begin
    Position.SetPoint(0, -0.8, 0);
    Direction.SetVector(0, 1, 0);
    InnerRadius := 0.95;
    OuterRadius := 1.25;
    Slices := 60;
    Loops := 1;
    SetColor(Material, 1);
  end;

  addPlane(0, 'FRONT', 1, 0, 0, 0.7, 0, 0, 1);
  addPlane(1, 'RIGHT', 1, 0.7, 0, 0, 1, 0, 0);
  addPlane(2, 'LEFT', 1, -0.7, 0, 0, -1, 0, 0);
  addPlane(3, 'BACK', 1, 0, 0, -0.7, 0, 0, -1);
  addPlane(4, 'TOP', 1, 0, 0.7, 0, 0, 1, 0);
  addPlane(5, 'BOTTOM', 1, 0, -0.7, 0, 0, -1, 0);

  addCube(6, 0.9, 0, 0.6, 0.6, 1, 0.2, 0.2);
  addCube(7, 0.9, 0, 0.6, -0.6, 1, 0.2, 0.2);
  addCube(8, 0.9, 0, -0.6, 0.6, 1, 0.2, 0.2);
  addCube(9, 0.9, 0, -0.6, -0.6, 1, 0.2, 0.2);

  addCube(10, 0.9, 0.6, 0.6, 0, 0.2, 0.2, 1);
  addCube(11, 0.9, 0.6, -0.6, 0, 0.2, 0.2, 1);
  addCube(12, 0.9, -0.6, 0.6, 0, 0.2, 0.2, 1);
  addCube(13, 0.9, -0.6, -0.6, 0, 0.2, 0.2, 1);

  addCube(14, 0.9, 0.6, 0, 0.6, 0.2, 1, 0.2);
  addCube(15, 0.9, 0.6, 0, -0.6, 0.2, 1, 0.2);
  addCube(16, 0.9, -0.6, 0, 0.6, 0.2, 1, 0.2);
  addCube(17, 0.9, -0.6, 0, -0.6, 0.2, 1, 0.2);

  addCube(18, 0.8, 0.6, 0.6, 0.6, 0.2, 0.2, 0.2);
  addCube(19, 0.8, 0.6, 0.6, -0.6, 0.2, 0.2, 0.2);
  addCube(20, 0.8, 0.6, -0.6, 0.6, 0.2, 0.2, 0.2);
  addCube(21, 0.8, -0.6, 0.6, 0.6, 0.2, 0.2, 0.2);
  addCube(22, 0.8, 0.6, -0.6, -0.6, 0.2, 0.2, 0.2);
  addCube(23, 0.8, -0.6, -0.6, 0.6, 0.2, 0.2, 0.2);
  addCube(24, 0.8, -0.6, 0.6, -0.6, 0.2, 0.2, 0.2);
  addCube(25, 0.8, -0.6, -0.6, -0.6, 0.2, 0.2, 0.2);

end;

// DoProgress
//
procedure TGLNavCube.DoProgress(const pt: TProgressTimes);
const
  tb: array [0 .. 1] of array [0 .. 3] of TVector = (((x: 0; y: 20; z: 1;
    W: 0), (x: 1; y: 20; z: 0; W: 0), (x: 0; y: 20; z: - 1; W: 0), (x: - 1;
    y: 20; z: 0; W: 0)), ((x: 0; y: - 20; z: 1; W: 0), (x: 1; y: - 20; z: 0;
    W: 0), (x: 0; y: - 20; z: - 1; W: 0), (x: - 1; y: - 20; z: 0; W: 0)));
var
  mp: TPoint;
  mover: boolean;
  i: Integer;
  v0, v1, v2, v: TVector;
  obj: TGLBaseSceneObject;

procedure moveTo(trgv: TVector);
begin
  FPosAnimationStart := FCam.Position.AsVector;
  FPosAnimationEnd := FCam.TargetObject.AbsoluteToLocal
    (VectorScale(VectorNormalize(trgv), FCam.DistanceToTarget));
  FDelta := 0;
end;

begin
//  if not (csloading in componentstate) then
//  begin
  mp := FViewer.ScreenToClient(mouse.CursorPos);
  mover := (mp.x > FHud.Position.x - 64) and (mp.x < FHud.Position.x + 64) and
    (mp.y > FHud.Position.y - 64) and (mp.y < FHud.Position.y + 64);
  // mouse Down/Up
  if (mp.y<FViewer.Top) or (mp.x<FViewer.Left) or (mp.y>FViewer.Top+FViewer.Height) or (mp.x>FViewer.Left+FViewer.Width) then exit;
  if FDelta > 1 then
  begin
    if iskeydown(VK_LBUTTON) and (not FMouseRotation) then
    begin
      // selection > start auto rotation
      if mover and (FSel >= 0) then
      begin

        v := FCam.AbsoluteVectorToTarget;
        v.y := 0;
        if v.x < 0 then
          i := -1
        else
          i := 1;
        i := round((ArcCos(VectorAngleCosine(v, ZHmgPoint)) * i + PI) / PI
          * 2) mod 4;
        if (FSel = 4) or (FSel = 5) then
          moveTo(tb[FSel - 4][i])
        else
          moveTo(FSelPos);
        FInactiveTime := 0;
      end // start manual rotation
      else if FMouse then
      begin

        FMouseRotation := true;
        FMousePos := mouse.CursorPos;
        //FMousePos := point(sW2, sH2);
       // mouse.CursorPos := point(sW2, sH2);
        FInactiveTime := 0;
      end;
    end;
    // stop rotation, restore cursor
    if (not iskeydown(VK_LBUTTON)) and FMouseRotation and FMouse then
    begin
      FMouseRotation := false;
     // FMousePos := mouse.CursorPos;
     // mouse.CursorPos := FMousePos;

      FInactiveTime := 0;
    end;
  end
  // auto rotation progress
  else
  begin
    FDelta := FDelta + pt.deltaTime * 2;
    v := VectorLerp(FPosAnimationStart, FPosAnimationEnd,
      FDelta * FDelta * (3 - 2 * FDelta));
    v := VectorScale(VectorNormalize(v), VectorLength(FPosAnimationStart));
    if FDelta < 1 then
      FCam.Position.SetPoint(v)
    else
      FCam.Position.SetPoint(FPosAnimationEnd);

    v := VectorScale(VectorNormalize(v), 10);
    if FDelta < 1 then
      v := VectorScale(VectorNormalize(v), 10)
    else
      v := VectorScale(VectorNormalize(FPosAnimationEnd), 10);
    FNavCam.Position.SetPoint(v);

    for i := 2 to FCube.Count - 1 do
      with TGLSceneObject(FCube.Children[i]) do
        Material.FrontProperties.Diffuse.SetColor(tagfloat, tagfloat, 1);
    FInactiveTime := 0;
  end;
  FSel := -1;
  // manual rotation progress
  if FMouseRotation and FMouse then
  begin

    mp := mouse.CursorPos;
    if FCam <> nil then
    begin
      FCam.MoveAroundTarget((FMousePos.y - mp.y) * 0.02, (FMousePos.x - mp.x) * 0.02);
      FNavCam.MoveAroundTarget((FMousePos.y - mp.y) * 0.02, (FMousePos.x - mp.x) * 0.02);
//    FCam.MoveAroundTarget((sH2 - mp.y) * 0.2, (sW2 - mp.x) * 0.2);
//  FNavCam.MoveAroundTarget((sH2 - mp.y) * 0.2, (sW2 - mp.x) * 0.2);
    end;

   // mouse.CursorPos := point(sW2, sH2);

   FInactiveTime := 0;
  end
  else if FReady then
  begin
    // selection
    if mover and (FDelta > 1) then
    begin
      v0 := FNavCam.AbsolutePosition;
      v1 := FMem.Buffer.ScreenToVector(mp.x - round(FHud.Position.x) + 64,
        round(FHud.Position.y) - mp.y + 64);
      SetVector(v2, 99999, 99999, 99999);

      obj := nil;
      for i := 2 to FCube.Count - 1 do
        with TGLSceneObject(FCube.Children[i]) do
        begin
          Material.FrontProperties.Diffuse.SetColor(tagfloat, tagfloat, 1);
          if RayCastIntersect(v0, v1, @v) then
            if VectorDistance2(v2, v0) > VectorDistance2(v, v0) then
            begin
              SetVector(v2, v);
              FSel := FCube.Children[i].tag;
              FSelPos := FCube.Children[i].Position.AsVector;
              obj := FCube.Children[i];
            end;
        end;
      if FSel >= 0 then
      begin
        FViewer.cursor := -21;
        TGLSceneObject(obj).Material.FrontProperties.Diffuse.SetColor
          (1, 0.6, 0);
      end
      else
        FViewer.cursor := 0;
    end;
    v := VectorScale(VectorNormalize(FCam.AbsoluteVectorToTarget), 10);
    FNavCam.Position.SetPoint(VectorNegate(v));
    FInactiveTime := FInactiveTime + pt.deltaTime;
  end;
  // rendering
  FTimer := FTimer + pt.deltaTime;
  if FTimer > 1 / FFps then
  begin
    FTimer := FTimer - floor(FTimer * FFps) / FFps;
    FMem.Render(FCube);
    FMem.CopyToTexture(FHud.Material.Texture);
    FReady := true;
  end;
 // end;
end;

// DoRender (setup)
//
procedure TGLNavCube.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: boolean);
begin
  inherited;
  if (FCam = nil) and (scene.CurrentGLCamera <> nil) then
  begin
    FCam := scene.CurrentGLCamera;
    FNavCam.Position.SetPoint
      (VectorScale(VectorNormalize(FCam.Position.AsVector), 10));
  end;
  if FViewer <> nil then
    FHud.Position.SetPoint(FViewer.Width - 80, 50, 0);
end;

initialization

sW2 := screen.Width div 2;
sH2 := screen.Height div 2;

end.

