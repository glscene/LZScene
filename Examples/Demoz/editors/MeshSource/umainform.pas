unit umainform;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Contnrs, ComCtrls, Buttons, StdCtrls,
  //GLS
  GLLCLViewer, GLScene, GLVectorFileObjects, GLState,
  GLTexture, GLObjects, GLVectorGeometry, GLVectorTypes, OpenGLTokens,
  GLMaterial, GLCoordinates, GLCrossPlatform, GLBaseClasses, FileUtil;

type
  TModifierCube = class(TGLCube)
  public
    FVectorIndex : Integer;
    FMeshObjIndex : Integer;
    constructor Create(AOwner: TComponent); override;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    Label2: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    GLSceneViewer: TGLSceneViewer;
    GLScene: TGLScene;
    GLCamera: TGLCamera;
    GLFreeForm: TGLFreeForm;
    Label1: TLabel;
    cbPolygonMode: TComboBox;
    dcModifiers: TGLDummyCube;
    chbViewPoints: TCheckBox;
    StatusBar: TStatusBar;
    GroupBox1: TGroupBox;
    chbShowAxis: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    tbPos: TTrackBar;
    GroupBox2: TGroupBox;
    rbXY: TRadioButton;
    rbZY: TRadioButton;
    GLLightSource1: TGLLightSource;
    GroupBox3: TGroupBox;
    btnVertex: TBitBtn;
    btnNormals: TBitBtn;
    btnTextcoords: TBitBtn;
    btnGroups: TBitBtn;
    GLMaterialLibrary1: TGLMaterialLibrary;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewerMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure cbPolygonModeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure chbViewPointsClick(Sender: TObject);
    procedure GLSceneViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chbShowAxisClick(Sender: TObject);
    procedure tbPosChange(Sender: TObject);
    procedure GLSceneViewerBeforeRender(Sender: TObject);
    procedure btnVertexClick(Sender: TObject);
    procedure btnNormalsClick(Sender: TObject);
    procedure btnTextcoordsClick(Sender: TObject);
    procedure btnGroupsClick(Sender: TObject);
  private
    //Private declarations
  private
    { Private declarations }
    FOldX, FOldY      : Integer;
    FModifierList     : TObjectList;
    FSelectedModifier : TModifierCube;
    FMoveZ            : Boolean;
    FOldMouseWorldPos : TVector;

    {Create cubes used to modify vertex points}
    procedure SetVertexModifiers;
    {Populate statusbar with object information}
    procedure ShowModifierStatus(const aObj : TModifierCube);
    {Change the mesh vector property for the selected modifier.}
    procedure ChangeMeshVector(const aObj : TModifierCube; const aPos : TVector4f);
    {Identify mouse position in X, Y and Z axis}
    function MouseWorldPos(x, y : Integer) : TVector;
    {Strip redundent data, recalculate normals and faces}
    procedure StripAndRecalc;
    {Set Freeform's polygon mode: line, fill or points}
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  GLPersistentClasses, GLVectorLists,MeshData, GLMeshUtils,
  GLFile3DS,  GLColor, GLContext;

{$IFnDEF FPC}
  {$IFnDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

const
  {Default combobox index for startup}
  CLinePolyMode  = 1;
  {Scale dimention}
  CModifierDim   = 0.04;

var
  {Modifier colors}
  CModColorNormal : TColorVector;
  CModColorSelect : TColorVector;

constructor TModifierCube.Create(AOwner: TComponent);
begin
  inherited;
  {Set the modifiers initial size and color}
  CubeWidth  := CModifierDim;
  CubeHeight := CModifierDim;
  CubeDepth  := CModifierDim;
  Material.FrontProperties.Diffuse.Color := CModColorNormal;
end;


procedure TfrmMain.FormCreate(Sender: TObject);
var
  lsDir : String;
  lsFileName : String;
begin
  {Do initial setup}
  FModifierList := TObjectList.Create;
  CModColorNormal := clrCoral;
  CModColorSelect := clrSkyBlue;

  lsDir := ExtractFileDir(Application.ExeName);
  lsFileName := Format('%s\media\cube.3ds', [lsDir]);
  if FileExistsUTF8(lsFileName) { *Converti depuis FileExists* } then
  begin
    GLFreeForm.LoadFromFile(lsFileName);
    StripAndRecalc;
    SetVertexModifiers;
  end;

  cbPolygonMode.ItemIndex := CLinePolyMode;
end;

procedure TfrmMain.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lObj : TGLBaseSceneObject;
begin
  FOldX := X; FOldY := Y;
  {If selecting a different modifier, change the last one's color back to default}
  if Assigned(FSelectedModifier) then
     FSelectedModifier.Material.FrontProperties.Diffuse.Color := CModColorNormal;


  {Get selected objects}
  if not (ssCtrl in Shift) then
    Exit;

  {Check if selected object is a modifier.
   If so, change modifiers color as to indicated selected modifier.}  
  lObj := GLSceneViewer.Buffer.GetPickedObject(X, Y);
  if (lObj is TModifierCube) then
  begin
    FSelectedModifier := TModifierCube(lObj);
    FSelectedModifier.Material.FrontProperties.Diffuse.Color := CModColorSelect;
    FSelectedModifier.NotifyChange(FSelectedModifier);
    ShowModifierStatus(TModifierCube(lObj));

    FMoveZ := rbZY.Checked;
    FOldMouseWorldPos := MouseWorldPos(X, Y);
  end;
end;

procedure TfrmMain.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  lCurrentPos : TVector;
  lOldV       : TVector3f;
  lDiff       : TVector4f;
begin
  {If ctrl is not in use, move around freeform}
  if (ssLeft in Shift) and (not (ssCtrl in Shift)) then
  begin
    GLCamera.MoveAroundTarget(FOldY - Y, FOldX - X);
    FOldX := X; FOldY := Y;
    Exit;
  end;

  {Move modifier and change relevant vertex data}
  if (ssLeft in Shift) then
  begin
    FMoveZ := rbZY.Checked;

    lCurrentPos := MouseWorldPos(X, Y);
    if Assigned(FSelectedModifier) and (VectorNorm(FOldMouseWorldPos) <> 0) then
    begin
      MakeVector(lOldV, FSelectedModifier.Position.X, FSelectedModifier.Position.Y, FSelectedModifier.Position.Z);
      lDiff := VectorSubtract(lCurrentPos, FOldMouseWorldPos);
      FSelectedModifier.Position.Translate(lDiff);
      ChangeMeshVector(FSelectedModifier, lDiff);
    end;
    FOldMouseWorldPos := lCurrentPos;
  end;
end;

procedure TfrmMain.cbPolygonModeChange(Sender: TObject);
begin
  case cbPolygonMode.ItemIndex of
   0: GLFreeForm.Material.PolygonMode := pmFill;
   1: GLFreeForm.Material.PolygonMode := pmLines;
   2: GLFreeForm.Material.PolygonMode := pmPoints;
  end;


end;

procedure TfrmMain.SetVertexModifiers;
  procedure ScaleVector(var V1, V2 : TVector3F);
  begin
    V1.X := V1.X * V2.X;
    V1.Y := V1.Y * V2.Y;
    V1.Z := V1.Z * V2.Z;
  end;
var
  i, j : Integer;
  lVector, lScale : TVector3F;
  lModifier : TModifierCube;
begin
  FModifierList.Clear;
  GLScene.BeginUpdate;
  try
    with GLFreeForm.MeshObjects do
    begin
      for i := 0 to Count - 1 do
        for j := 0 to Items[i].Vertices.Count - 1 do
        begin
          lVector := Items[i].Vertices.Items[j];
          lModifier := TModifierCube.Create(nil);
          lModifier.FVectorIndex := j;
          lModifier.FMeshObjIndex := i;

          FModifierList.Add(lModifier);
          GLScene.Objects.AddChild(lModifier);

          lScale := GLFreeForm.Scale.AsAffineVector;
          ScaleVector(lVector, lScale);
          lModifier.Position.Translate(lVector);
        end;
    end;
  finally
    GLScene.EndUpdate;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FModifierList.Clear;
  FreeAndNil(FModifierList);
end;

procedure TfrmMain.chbViewPointsClick(Sender: TObject);
var
  i : Integer;
begin
  GLScene.BeginUpdate;
  try
    for i := 0 to FModifierList.Count - 1 do
      TModifierCube(FModifierList.Items[i]).Visible := chbViewPoints.Checked;
  finally
    GLScene.EndUpdate;
  end;
end;

procedure TfrmMain.ShowModifierStatus(const aObj: TModifierCube);
begin
  if aObj = nil then
    StatusBar.Panels[0].Text := ''
  else
    StatusBar.Panels[0].Text := Format('Modifier vector index [%d]', [aObj.FVectorIndex]);
end;

function TfrmMain.MouseWorldPos(x, y: Integer): TVector;
var
  v : TVector;
begin
  y := GLSceneViewer.Height - y;

  if Assigned(FSelectedModifier) then
  begin
    SetVector(v, x, y, 0);
    if FMoveZ then
      GLSceneViewer.Buffer.ScreenVectorIntersectWithPlaneXZ(v, FSelectedModifier.Position.Y, Result)
    else
      GLSceneViewer.Buffer.ScreenVectorIntersectWithPlaneXY(v, FSelectedModifier.Position.Z, Result);
  end
  else
    SetVector(Result, NullVector);
end;

procedure TfrmMain.ChangeMeshVector(const aObj : TModifierCube; const aPos : TVector4f);
var
  lVIndex,
  lMIndex  : Integer;
  v        : TVector3f;
begin
  if aObj = nil then
    Exit;

  lVIndex := aObj.FVectorIndex;
  lMIndex := aObj.FMeshObjIndex;

  {Get new vertex position, keep freeform scale in mind and redraw freeform.}
  MakeVector(v, aPos.X/CModifierDim, aPos.Y/CModifierDim, aPos.Z/CModifierDim);
  GLFreeForm.MeshObjects.Items[lMIndex].Vertices.TranslateItem(lVIndex, v);
  GLFreeForm.StructureChanged;
end;

procedure TfrmMain.GLSceneViewerMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FSelectedModifier) then
  begin
    FSelectedModifier.Material.FrontProperties.Diffuse.Color := CModColorNormal;
    FSelectedModifier := nil;
    {Recalculate structure and redraw freeform}
    StripAndRecalc;
    {Reset vertex modifiers and their data.}
    SetVertexModifiers;
  end;
end;

procedure TfrmMain.chbShowAxisClick(Sender: TObject);
begin
  dcModifiers.ShowAxes := TCheckBox(Sender).Checked;
end;

procedure TfrmMain.tbPosChange(Sender: TObject);
begin
  GLCamera.Position.Z := tbPos.Position;

end;

procedure TfrmMain.GLSceneViewerBeforeRender(Sender: TObject);
begin
  gl.Enable(GL_NORMALIZE);
end;

procedure TfrmMain.btnVertexClick(Sender: TObject);
var
  i, j    : Integer;
  lList   : TStringList;
  lVector : TVector3f;
begin
  lList := TStringList.Create;
  try
    with GLFreeForm.MeshObjects do
      for i := 0 to Count - 1 do
      begin
        lList.Add('For mesh object ' + IntToStr(i));
        for j := 0 to Items[i].Vertices.Count - 1 do
        begin
          lVector := Items[i].Vertices.Items[j];
          lList.Add(Format('%f %f %f', [lVector.X, lVector.Y, lVector.Z]));
        end;
      end;
    ShowMeshData(lList);
  finally
    FreeAndNil(lList);
  end;
end;

procedure TfrmMain.btnNormalsClick(Sender: TObject);
var
  i, j    : Integer;
  lList   : TStringList;
  lVector : TVector3f;
begin
  lList := TStringList.Create;
  try
    with GLFreeForm.MeshObjects do
      for i := 0 to Count - 1 do
      begin
        lList.Add('For mesh object ' + IntToStr(i));
        for j := 0 to Items[i].Normals.Count - 1 do
        begin
          lVector := Items[i].Normals.Items[j];
          lList.Add(Format('%f %f %f', [lVector.X, lVector.Y, lVector.Z]));
        end;
      end;
    ShowMeshData(lList);
  finally
    FreeAndNil(lList);
  end;
end;

procedure TfrmMain.btnTextcoordsClick(Sender: TObject);
var
  i, j    : Integer;
  lList   : TStringList;
  lVector : TVector3f;
begin
  lList := TStringList.Create;
  try
    with GLFreeForm.MeshObjects do
      for i := 0 to Count - 1 do
      begin
        lList.Add('For mesh object ' + IntToStr(i));
        for j := 0 to Items[i].TexCoords.Count - 1 do
        begin
          lVector := Items[i].TexCoords.Items[j];
          lList.Add(Format('%f %f %f', [lVector.X, lVector.Y, lVector.Z]));
        end;
      end;
    ShowMeshData(lList);
  finally
    FreeAndNil(lList);
  end;
end;

procedure TfrmMain.btnGroupsClick(Sender: TObject);
var
  i    : Integer;
  lList   : TStringList;
begin
  lList := TStringList.Create;
  try
    with GLFreeForm.MeshObjects do
      for i := 0 to Count - 1 do
      begin
        lList.Add('For mesh object ' + IntToStr(i));
        lList.Add(IntToStr(Items[i].TriangleCount));
      end;
    ShowMeshData(lList);
  finally
    FreeAndNil(lList);
  end;
end;

procedure TfrmMain.StripAndRecalc;
var
  lTrigList,
  lNormals    : TAffineVectorList;
  lIndices    : TIntegerList;
  lObj        : TGLMeshObject;
  lStrips     : TPersistentObjectList;

  lFaceGroup  : TFGVertexIndexList;
  i           : Integer;
begin
  // Extract raw triangle data to work with.
  lTrigList := GLFreeForm.MeshObjects.ExtractTriangles;

  // Builds a vector-count optimized indices list.
  lIndices := BuildVectorCountOptimizedIndices(lTrigList);
  // Alter reference/indice pair and removes unused reference values.
  RemapAndCleanupReferences(lTrigList, lIndices);
   // Calculate normals.
  lNormals := BuildNormals(lTrigList, lIndices);

  // Strip where posible.
  lStrips := StripifyMesh(lIndices, lTrigList.Count, True);

  // Clear current mesh object data.
  GLFreeForm.MeshObjects.Clear;

  // Setup new mesh object.
  lObj := TGLMeshObject.CreateOwned(GLFreeForm.MeshObjects);
  lObj.Vertices := lTrigList;
  lObj.Mode := momFaceGroups;
  lObj.Normals := lNormals;

  for i:=0 to lStrips.Count-1 do
  begin
    lFaceGroup := TFGVertexIndexList.CreateOwned(lObj.FaceGroups);
    lFaceGroup.VertexIndices := (lStrips[i] as TIntegerList);
    if i > 0 then
      lFaceGroup.Mode := fgmmTriangleStrip
    else
      lFaceGroup.Mode := fgmmTriangles;
    lFaceGroup.MaterialName:=IntToStr(i and 15);
  end;
  // Redraw freeform
  GLFreeForm.StructureChanged;

  lTrigList.Free;
  lNormals.Free;
  lIndices.Free;
end;

end.
