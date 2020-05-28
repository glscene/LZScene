//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Editor window for a material (with preview).
  
}
unit FMaterialEditorForm;

interface

{$MODE DELPHI}
{$I GLScene.inc}

uses
  lresources, 
  Forms,
  ComCtrls, 
  StdCtrls, 
  Controls,
  Graphics,
  Classes, 
  Buttons, 
  TypInfo, 
  FRTrackBarEdit, 
  FRTextureEdit,
  FRColorEditor, 
  FRFaceEditor, 
  GLScene, 
  GLObjects, 
  GLTexture, 
  GLHUDObjects, 
  GLTeapot,
  GLGeomObjects, 
  GLColor, 
  GLLCLViewer, 
  GLCoordinates,
  GLCrossPlatform, 
  GLMaterial, 
  GLState;

type

  TMaterialEditorForm = class(TForm)
    CBPolygonMode: TComboBox;
    Label2: TLabel;
    PageControl1: TPageControl;
    TSFront: TTabSheet;
    TSBack: TTabSheet;
    TSTexture: TTabSheet;
    GroupBox1: TGroupBox;
    BBOk: TBitBtn;
    BBCancel: TBitBtn;
    RTextureEdit: TRTextureEdit;
    CBBlending: TComboBox;
    Label1: TLabel;
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
    procedure CBObjectChange(Sender: TObject);
    procedure CBBackgroundChange(Sender: TObject);
    procedure SceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure SceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure SceneViewerMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure OnMaterialChanged(Sender: TObject);
  public
    
    constructor Create(AOwner: TComponent); override;
    function Execute(AMaterial: TGLMaterial): boolean;
  end;

function MaterialEditorForm: TMaterialEditorForm;
procedure ReleaseMaterialEditorForm;

//=================================================
implementation
//=================================================

var
  vMaterialEditorForm: TMaterialEditorForm;
  FEFront: TRFaceEditor;
  FEBack: TRFaceEditor;
  MX, MY: integer;

function MaterialEditorForm: TMaterialEditorForm;
begin
  if not Assigned(vMaterialEditorForm) then
    vMaterialEditorForm := TMaterialEditorForm.Create(nil);
  Result := vMaterialEditorForm;
end;

procedure ReleaseMaterialEditorForm;
begin
  if Assigned(vMaterialEditorForm) then
  begin
    vMaterialEditorForm.Free;
    vMaterialEditorForm := nil;
  end;
end;

// Create

constructor TMaterialEditorForm.Create(AOwner: TComponent);
var
  I: integer;
begin
  inherited;
  for i := 0 to integer(High(TBlendingMode)) do
    CBBlending.Items.Add(GetEnumName(TypeInfo(TBlendingMode), i));
  for i := 0 to integer(High(TPolygonMode)) do
    CBPolygonMode.Items.Add(GetEnumName(TypeInfo(TPolygonMode), i));
  FEFront := TRFaceEditor.Create(self);
  FEFront.Parent := TSFront;
  FEFront.Name := 'FEFront';
  FEFront.Align := alClient;

  FEBack := TRFaceEditor.Create(self);
  FEBack.Parent := TSBack;
  FEBack.Name := 'FEBack';
  FEBack.Align := alClient;

  FEFront.OnChange := OnMaterialChanged;
  FEBack.OnChange := OnMaterialChanged;
  RTextureEdit.OnChange := OnMaterialChanged;

  BackGroundSprite.Position.X := SceneViewer.Width div 2;
  BackGroundSprite.Position.Y := SceneViewer.Height div 2;
  BackGroundSprite.Width := SceneViewer.Width;
  BackGroundSprite.Height := SceneViewer.Height;

  CBObject.ItemIndex := 0;
  CBObjectChange(Self);
  CBBackground.ItemIndex := 0;
  CBBackgroundChange(Self);
end;

// Execute

function TMaterialEditorForm.Execute(AMaterial: TGLMaterial): boolean;
begin
  with AMaterial do
  begin
    CBBlending.ItemIndex := integer(BlendingMode);
    CBPolygonMode.ItemIndex := integer(PolygonMode);
    FEFront.FaceProperties := FrontProperties;
    FEBack.FaceProperties := BackProperties;
    RTextureEdit.Texture := Texture;
  end;

  Result := (ShowModal = mrOk);
  if Result then
    with AMaterial do
    begin
      FrontProperties := FEFront.FaceProperties;
      BackProperties := FEBack.FaceProperties;
      Texture := RTextureEdit.Texture;
      BlendingMode := TBlendingMode(CBBlending.ItemIndex);
      PolygonMode := TPolygonMode(CBPolygonMode.ItemIndex);
    end;
end;

// OnMaterialChanged

procedure TMaterialEditorForm.OnMaterialChanged(Sender: TObject);
begin
  with GLMaterialLibrary.Materials[0].Material do
  begin
    FrontProperties := FEFront.FaceProperties;
    BackProperties := FEBack.FaceProperties;
    Texture := RTextureEdit.Texture;
    BlendingMode := TBlendingMode(CBBlending.ItemIndex);
    PolygonMode := TPolygonMode(CBPolygonMode.ItemIndex);
  end;
  // MPPreview.Render;
end;

procedure TMaterialEditorForm.CBObjectChange(Sender: TObject);
var
  i: integer;
begin
  i := CBObject.ItemIndex;
  Cube.Visible := I = 0;
  Sphere.Visible := I = 1;
  Cone.Visible := I = 2;
  Teapot.Visible := I = 3;
end;

procedure TMaterialEditorForm.CBBackgroundChange(Sender: TObject);
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

procedure TMaterialEditorForm.SceneViewerMouseMove(Sender: TObject;
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

procedure TMaterialEditorForm.SceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  MX := X;
  MY := Y;
end;

procedure TMaterialEditorForm.SceneViewerMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  Camera.AdjustDistanceToTarget(1 - 0.1 * (Abs(WheelDelta) / WheelDelta));
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  {$i FMaterialEditorForm.lrs}

finalization

  ReleaseMaterialEditorForm;

end.

