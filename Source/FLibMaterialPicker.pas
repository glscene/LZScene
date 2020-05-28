//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Allows choosing a material in a material library
}

unit FLibMaterialPicker;

interface

{$I GLScene.inc}

uses
  lresources, 
  LCLIntf,
  Controls, 
  Classes, 
  Forms, 
  StdCtrls, 
  Buttons, 
  Dialogs, 
  Graphics,
  GLViewer, 
  GLMaterial, 
  GLScene, 
  GLObjects, 
  GLTexture,
  GLHUDObjects, 
  GLTeapot, 
  GLGeomObjects, 
  GLColor,
  GLCoordinates;

type

  { TLibMaterialPicker }

  TLibMaterialPicker = class(TForm)
    LBMaterials: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    BBOk: TBitBtn;
    BBCancel: TBitBtn;
    GLScene1: TGLScene;
    SceneViewer: TGLSceneViewer;
    CBObject: TComboBox;
    Camera: TGLCamera;
    Cube: TGLCube;
    Sphere: TGLSphere;
    LightSource: TGLLightSource;
    CBBackground: TComboBox;
    BackGroundSprite: TGLHUDSprite;
    Cone: TGLCone;
    Teapot: TGLTeapot;
    World: TGLDummyCube;
    Light: TGLDummyCube;
    FireSphere: TGLSphere;
    GLMaterialLibrary: TGLMaterialLibrary;
    procedure LBMaterialsClick(Sender: TObject);
    procedure LBMaterialsKeyPress(Sender: TObject; var Key: char);
    procedure LBMaterialsDblClick(Sender: TObject);
    procedure CBObjectChange(Sender: TObject);
    procedure CBBackgroundChange(Sender: TObject);
    procedure SceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure SceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure SceneViewerMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
  private
    function GetMaterial: TGLMaterial;
    procedure SetMaterial(const Value: TGLMaterial);
    property Material: TGLMaterial read GetMaterial write SetMaterial;
    
  public
    
    constructor Create(AOwner: TComponent); override;
    function Execute(var materialName: TGLLibMaterialName;
      materialLibrary: TGLAbstractMaterialLibrary): boolean;

  end;

function LibMaterialPicker: TLibMaterialPicker;
procedure ReleaseLibMaterialPicker;

//====================================================
implementation
//====================================================


var
  vLibMaterialPicker: TLibMaterialPicker;

function LibMaterialPicker: TLibMaterialPicker;
begin
  if not Assigned(vLibMaterialPicker) then
    vLibMaterialPicker := TLibMaterialPicker.Create(nil);
  Result := vLibMaterialPicker;
end;

procedure ReleaseLibMaterialPicker;
begin
  if Assigned(vLibMaterialPicker) then
  begin
    vLibMaterialPicker.Free;
    vLibMaterialPicker := nil;
  end;
end;

// Execute

function TLibMaterialPicker.Execute(var materialName: TGLLibMaterialName;
  materialLibrary: TGLAbstractMaterialLibrary)
: boolean;
begin
  with LBMaterials do
  begin
    materialLibrary.SetNamesToTStrings(LBMaterials.Items);
    ItemIndex := Items.IndexOf(materialName);
    if (ItemIndex < 0) and (Items.Count > 0) then
      ItemIndex := 0;
    BBOk.Enabled := (Items.Count > 0);
  end;
  LBMaterialsClick(Self);
  Result := (ShowModal = mrOk);
  if Result then
  begin
    with LBMaterials do
      if ItemIndex >= 0 then
        materialName := Items[ItemIndex]
      else
        materialName := '';
  end;
end;

var
  MX, MY: integer;

constructor TLibMaterialPicker.Create(AOwner: TComponent);
begin
  inherited;
   {$IFDEF UNIX}
  SceneViewer.Height := 160;
  SceneViewer.Top := 64;
   {$ENDIF}
  BackGroundSprite.Position.X := SceneViewer.Width div 2;
  BackGroundSprite.Position.Y := SceneViewer.Height div 2;
  BackGroundSprite.Width := SceneViewer.Width;
  BackGroundSprite.Height := SceneViewer.Height;

  CBObject.ItemIndex := 0;
  CBObjectChange(Self);
  CBBackground.ItemIndex := 0;
  CBBackgroundChange(Self);
end;

procedure TLibMaterialPicker.LBMaterialsClick(Sender: TObject);
begin
  with LBMaterials do
    if ItemIndex >= 0 then
      Material := TGLLibMaterial(Items.Objects[ItemIndex]).Material;
end;

procedure TLibMaterialPicker.LBMaterialsKeyPress(Sender: TObject; var Key: char);
begin
  LBMaterialsClick(Sender);
end;

procedure TLibMaterialPicker.LBMaterialsDblClick(Sender: TObject);
begin
  BBOk.Click;
end;

procedure TLibMaterialPicker.CBObjectChange(Sender: TObject);
var
  i: integer;
begin
  i := CBObject.ItemIndex;
  Cube.Visible := I = 0;
  Sphere.Visible := I = 1;
  Cone.Visible := I = 2;
  Teapot.Visible := I = 3;
end;

procedure TLibMaterialPicker.CBBackgroundChange(Sender: TObject);
var
  bgColor: TColor;
begin
  case CBBackground.ItemIndex of
    1: bgColor := clWhite;
    2: bgColor := clBlack;
    3: bgColor := clBlue;
    4: bgColor := clRed;
    5: bgColor := clGreen;
    else
      bgColor := clNone;
  end;
  with BackGroundSprite.Material do
  begin
    Texture.Disabled := (bgColor <> clNone);
    FrontProperties.Diffuse.Color := ConvertWinColor(bgColor);
  end;
end;

procedure TLibMaterialPicker.SceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  if (ssRight in Shift) and (ssLeft in Shift) then
    Camera.AdjustDistanceToTarget(1 - 0.01 * (MY - Y))
  else
  if (ssRight in Shift) or (ssLeft in Shift) then
    Camera.MoveAroundTarget(Y - MY, X - MX);

  MX := X;
  MY := Y;
end;

procedure TLibMaterialPicker.SceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  MX := X;
  MY := Y;
end;

procedure TLibMaterialPicker.SceneViewerMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  Camera.AdjustDistanceToTarget(1 - 0.1 * (Abs(WheelDelta) / WheelDelta));
end;

function TLibMaterialPicker.GetMaterial: TGLMaterial;
begin
  Result := GLMaterialLibrary.Materials[0].Material;
end;

procedure TLibMaterialPicker.SetMaterial(const Value: TGLMaterial);
begin
  GLMaterialLibrary.Materials[0].Material.Assign(Value);
end;

initialization

  {$I FLibMaterialPicker.lrs}

finalization
  ReleaseLibMaterialPicker;

end.

