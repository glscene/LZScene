//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Material Preview frame.
}
// There was problems with GLSceneViewer and TFrame (при передаче handle to buffer),
// so now is using MemoryViewer

unit FRMaterialPreview;

interface

{$I GLScene.inc}

uses 
  lresources,
  Classes, 
  Forms, 
  StdCtrls, 
  Controls, 
  ExtCtrls,
  Graphics,
  GLScene, 
  GLObjects, 
  GLTexture, 
  GLHUDObjects, 
  GLTeapot,
  GLGeomObjects, 
  GLColor, 
  GLCoordinates,
  GLCrossPlatform, 
  GLMaterial,
  GLgraphics, 
  LCLIntf;


type

  TRMaterialPreview = class(TFrame)
    GLMemoryViewer1: TGLMemoryViewer;
    GLScene1: TGLScene;
    CBObject: TComboBox;
    Camera: TGLCamera;
    Cube: TGLCube;
    imgFull: TImage;
    Sphere: TGLSphere;
    LightSource: TGLLightSource;
    CBBackground: TComboBox;
    BackGroundSprite: TGLHUDSprite;
    Cone: TGLCone;
    Teapot: TGLTeapot;
    Timer1: TTimer;
    World: TGLDummyCube;
    Light: TGLDummyCube;
    FireSphere: TGLSphere;
    GLMaterialLibrary: TGLMaterialLibrary;
    procedure CBObjectChange(Sender: TObject);
    procedure CBBackgroundChange(Sender: TObject);
    procedure imgFullMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgFullMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgFullMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure imgFullResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

  private
    function GetMaterial: TGLMaterial;
    procedure SetMaterial(const Value: TGLMaterial);
     
  public
    Procedure Render;
     
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property Material : TGLMaterial read GetMaterial write SetMaterial;

  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
  MX, MY: Integer;

constructor TRMaterialPreview.Create(AOwner : TComponent);
begin
   inherited;
   BackGroundSprite.Position.X := imgFull.Width div 2;
   BackGroundSprite.Position.Y := imgFull.Height div 2;
   BackGroundSprite.Width := imgFull.Width;
   BackGroundSprite.Height := imgFull.Height;

   CBObject.ItemIndex:=0;     CBObjectChange(Self);
   CBBackground.ItemIndex:=0; CBBackgroundChange(Self);
end;

destructor TRMaterialPreview.Destroy;
begin
  inherited;
end;

procedure TRMaterialPreview.CBObjectChange(Sender: TObject);
var
   i : Integer;
begin
   i:=CBObject.ItemIndex;
   Cube.Visible   := I = 0;
   Sphere.Visible := I = 1;
   Cone.Visible   := I = 2;
   Teapot.Visible := I = 3;
end;

procedure TRMaterialPreview.CBBackgroundChange(Sender: TObject);
var
   bgColor : TColor;
begin
   case CBBackground.ItemIndex of
      1 : bgColor:=clWhite;
      2 : bgColor:=clBlack;
      3 : bgColor:=clBlue;
      4 : bgColor:=clRed;
      5 : bgColor:=clGreen;
   else
      bgColor:=clNone;
   end;
   with BackGroundSprite.Material do begin
      Texture.Disabled:=(bgColor<>clNone);
      FrontProperties.Diffuse.Color:=ConvertWinColor(bgColor);
   end;
end;

procedure TRMaterialPreview.imgFullMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MX := X;
  MY := Y;
end;

procedure TRMaterialPreview.imgFullMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssRight in Shift) and (ssLeft in Shift) then
    Camera.AdjustDistanceToTarget(1 - 0.01 * (MY - Y))
  else
  if (ssRight in Shift) or (ssLeft in Shift) then
    Camera.MoveAroundTarget(Y - MY, X - MX);

  MX := X;
  MY := Y;
end;

procedure TRMaterialPreview.imgFullMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
    Camera.AdjustDistanceToTarget(1 - 0.1 * (Abs(WheelDelta) / WheelDelta));
end;

procedure TRMaterialPreview.imgFullResize(Sender: TObject);
begin
  GLMemoryViewer1.Height:= imgFull.Height;
  GLMemoryViewer1.Width:= imgFull.Width;
end;

procedure TRMaterialPreview.Timer1Timer(Sender: TObject);
Var
  BitMap : TBitmap;
  Image  : TGLBitmap32;
  fWidth, fHeight:Integer;

begin
  GLMemoryViewer1.Render;
  Image := GLMemoryViewer1.Buffer.CreateSnapShot;
  Bitmap := Image.Create32BitsBitmap;
  try
    imgFull.Canvas.Brush.Color := clBlack;
    imgFull.Canvas.FillRect(imgFull.Canvas.ClipRect);
    fWidth :=imgFull.Width;
    fHeight:= imgFull.Height;
    imgFull.Canvas.StretchDraw(Rect(0, 0, fWidth, fHeight), Bitmap);{}
  finally
    Bitmap.Free;
    Image.Free;
  end;
end;

function TRMaterialPreview.GetMaterial: TGLMaterial;
begin
  Result := GLMaterialLibrary.Materials[0].Material;
end;

procedure TRMaterialPreview.SetMaterial(const Value: TGLMaterial);
begin
  GLMaterialLibrary.Materials[0].Material.Assign(Value);
end;

Procedure TRMaterialPreview.Render;
begin
  Timer1.OnTimer(self);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  {$i FRMaterialPreview.lrs}

end.



