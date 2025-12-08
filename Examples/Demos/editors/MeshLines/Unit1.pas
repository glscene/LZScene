unit Unit1;

{$mode objfpc}{$h+}

interface

uses

  SysUtils,
  Classes,
  Math,
  Controls,
  Forms,
  Dialogs,
  Graphics,
  Buttons,
  ActnList,
  ComCtrls,
  StdCtrls,

ExtCtrls,

  GLScene,
  GLObjects,
  GLTexture,
  GLGeomObjects,
  GLVectorFileObjects,
  GLExtrusion,
  GLSLProjectedTextures,
  GLMultiMaterialShader,
  GLNodes,
  GLFileTGA,
  GLCadencer,
  GLVectorGeometry,
  GLMaterial,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  GLMeshLines, GLLCLViewer;

type
  TLineOperation = (loSelectLine, loNewLine, loInsertNode, loMoveNode);

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLSceneViewer1: TGLSceneViewer;
    GLDummyCube1: TGLDummyCube;
    GLCube1: TGLCube;
    GLPlane1: TGLPlane;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    Panel2: TPanel;
    Panel1: TPanel;
    TreeView1: TTreeView;
    LinePnl: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    Panel5: TPanel;
    Label2: TLabel;
    LineWidthEdt: TEdit;
    Label3: TLabel;
    LineBreakAngleEdt: TEdit;
    LineDivisionEdt: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LineTextureLengthEdt: TEdit;
    NodePnl: TPanel;
    Splitter2: TSplitter;
    Panel6: TPanel;
    NodeXEdt: TEdit;
    NodeYEdt: TEdit;
    NodeZEdt: TEdit;
    Panel7: TPanel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    TestPln: TPanel;
    Label1: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    UpDown4: TUpDown;
    Edit4: TEdit;
    Label4: TLabel;
    Button2: TButton;
    Button1: TButton;
    FunctionPnl: TPanel;
    LineTextureCorrectioncb: TCheckBox;
    LineSplinecb: TCheckBox;
    LineHidecb: TCheckBox;
    Label14: TLabel;
    NameEdt: TEdit;
    sbNewLine: TSpeedButton;
    sbDelNode: TSpeedButton;
    sbDelLine: TSpeedButton;
    sbEndLine: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    CheckBox1: TCheckBox;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button2Click(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure NodeXEdtChange(Sender: TObject);
    procedure NodeYEdtChange(Sender: TObject);
    procedure NodeZEdtChange(Sender: TObject);
    procedure LineWidthEdtChange(Sender: TObject);
    procedure LineTextureCorrectioncbClick(Sender: TObject);
    procedure LineSplinecbClick(Sender: TObject);
    procedure LineBreakAngleEdtChange(Sender: TObject);
    procedure LineDivisionEdtChange(Sender: TObject);
    procedure LineTextureLengthEdtChange(Sender: TObject);
    procedure LineHidecbClick(Sender: TObject);
    procedure NameEdtChange(Sender: TObject);
    procedure sbNewLineClick(Sender: TObject);
    procedure sbDelNodeClick(Sender: TObject);
    procedure sbDelLineClick(Sender: TObject);
    procedure sbEndLineClick(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
  private
    mx, my: single;
    FCurrentLine: TGLMeshLines;
    FSelectedNode: TGLNode;
    FLineItem: TLineItem;
    FUpdatingControls: Boolean;
    FNewLineItem: TLineItem;
    FLineOperation: TLineOperation;
    procedure TreeLineItemSelect(LineItem: TLineItem);
    procedure NewLine(StartPoint: TAffineVector);
    procedure EndLineOperation;
    procedure InsertNode(LineNode1, LineNode2: TLineNode);
    procedure DeleteLine(LineItem: TLineItem);
    procedure DeleteLineNode(lineNode: TLineNode);
    function NodeText(lineNode: TLineNode): String;
    function MakeNewLineName(LineItem: TLineItem): String;
    function AddLineToTree(LineItem: TLineItem): TTreeNode;
    function AddLineNodeToTree(lineNode: TLineNode): TTreeNode;
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.TreeLineItemSelect(LineItem: TLineItem);
begin
  if assigned(LineItem) then
  begin
    Caption := LineItem.Name;
    if assigned(TreeView1.Selected) then
      TreeView1.Selected.Collapse(True);
    if assigned(FCurrentLine.SelectedNode) then
    begin
      TreeView1.Selected := TTreeNode(FCurrentLine.SelectedNode.Data);
    end
    else
    begin
      TreeView1.Selected := TTreeNode(LineItem.Data);
      if assigned(TreeView1.Selected) then
        TreeView1.Selected.Expand(True);
    end;
  end;
end;

procedure TForm1.NewLine(StartPoint: TAffineVector);
begin
  FCurrentLine.BeginUpdate;
  if not assigned(FCurrentLine.SelectedNode) then
  begin
    FNewLineItem := FCurrentLine.Lines.Add;
    MakeNewLineName(FNewLineItem);
    AddLineToTree(FNewLineItem);
    FNewLineItem.SplineMode := lsmCubicSpline;
    FNewLineItem.TextureCorrection := True;
    FNewLineItem.BreakAngle := -1;
    FNewLineItem.Nodes.AddNode(StartPoint.X, 0, StartPoint.Z);
    AddLineNodeToTree(TLineNode(FNewLineItem.Nodes.Last));
  end;
  FNewLineItem.Nodes.AddNode(StartPoint.X, 0, StartPoint.Z + 0.1);
  FCurrentLine.SelectLineItem(TLineNode(FNewLineItem.Nodes.Last));
  FCurrentLine.EndUpdate;
end;

procedure TForm1.EndLineOperation;
begin
  case FLineOperation of
    loNewLine:
      begin
        if assigned(FNewLineItem) and (FLineOperation = loNewLine) then
        begin
          if FNewLineItem.Nodes.Count < 3 then
            DeleteLine(FNewLineItem)
          else
          begin
            FCurrentLine.SelectedNode.Free;
            // Change line to normal line if there are only 2 nodes. No point to waste space with a spline
            if FNewLineItem.Nodes.Count = 2 then
              FNewLineItem.SplineMode := lsmLines;
          end;
          FCurrentLine.EndUpdate;
        end;
        FNewLineItem := nil;
        // FNewLineNode := nil;
        FLineOperation := loSelectLine;
      end;
    loInsertNode:
      begin
        if assigned(FCurrentLine.SelectedNode) then
        begin
          FCurrentLine.SelectedNode.Free;
          FCurrentLine.EndUpdate;
        end;
      end;
    loMoveNode:
      begin
        // nothing
      end;
  end;
  FCurrentLine.DeselectLineNode;
  FLineOperation := loSelectLine;
end;

procedure TForm1.InsertNode(LineNode1, LineNode2: TLineNode);
var
  lID: Integer;
begin
  if assigned(FCurrentLine.SelectedLineItem) then
  begin
    lID := FCurrentLine.SelectedLineItem.Nodes.IndexOf(LineNode2);
    FCurrentLine.SelectLineItem
      (TLineNode(FCurrentLine.SelectedLineItem.Nodes.Insert(lID)));
    FLineOperation := loInsertNode;
  end;
end;

procedure TForm1.DeleteLine(LineItem: TLineItem);
begin
  FCurrentLine.DeselectLineItem;
  if assigned(LineItem) then
  begin
    if LineItem.Data <> nil then
      TTreeNode(LineItem.Data).Delete;
    FreeandNil(LineItem);
    FCurrentLine.EndUpdate;
  end;
end;

procedure TForm1.DeleteLineNode(lineNode: TLineNode);
begin
  if Assigned(lineNode) then
  begin
    if lineNode.Data <> nil then
      TTreeNode(lineNode.Data).Delete;
    FreeandNil(lineNode);
  end;
  FCurrentLine.DeselectLineNode;
end;

function TForm1.NodeText(lineNode: TLineNode): String;
begin
  Result := format('X: %f Y: %f Z: %f', [lineNode.X, lineNode.Y, lineNode.Z]);
end;

function TForm1.MakeNewLineName(LineItem: TLineItem): String;
begin
  Result := 'Line' + IntToStr(FCurrentLine.Lines.Count);
  LineItem.Name := Result;
end;

function TForm1.AddLineToTree(LineItem: TLineItem): TTreeNode;
var
  i: Integer;
begin
  Result := TreeView1.Items.AddChild(nil, LineItem.Name);
  Result.Data := LineItem;
  LineItem.Data := Result;
  for i := 0 to LineItem.Nodes.Count - 1 do
    AddLineNodeToTree(TLineNode(LineItem.Nodes[i]));
end;

function TForm1.AddLineNodeToTree(lineNode: TLineNode): TTreeNode;
var
  lLineItem: TLineItem;
  lTreeNode: TTreeNode;
  lIndex: Integer;
  i: Integer;
begin
  lTreeNode := nil;
  lLineItem := TLineItem(lineNode.collection.owner);
  lIndex := lLineItem.Nodes.IndexOf(lineNode);
  if lIndex = lLineItem.Nodes.Count - 1 then
  begin
    // This is the last node. Just add it
    Result := TreeView1.Items.AddChild(TTreeNode(lLineItem.Data),
      NodeText(lineNode));
  end
  else
  begin
    // find the treenode of the line nodes after this one.
    for i := lIndex to lLineItem.Nodes.Count - 1 do
    begin
      if TLineNode(lLineItem.Nodes[i]).Data <> nil then
      begin
        lTreeNode := TTreeNode(TLineNode(lLineItem.Nodes[i]).Data);
        break;
      end;
    end;
    if assigned(lTreeNode) then
      Result := TreeView1.Items.Insert(lTreeNode, NodeText(lineNode))
    else
      Result := TreeView1.Items.AddChild(TTreeNode(lLineItem.Data),
        NodeText(lineNode));
  end;
  Result.Data := lineNode;
  lineNode.Data := Result;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  rayStart, rayVector, iPoint, iNormal: TVector;
begin
  if (Button = TMouseButton.mbLeft) then
  begin
    mx := X;
    my := Y;
    SetVector(rayStart, TGLSceneViewer(Sender).Camera.AbsolutePosition);
    SetVector(rayVector, TGLSceneViewer(Sender).Buffer.ScreenToVector
      (AffineVectorMake(X, TGLSceneViewer(Sender).Height - Y, 0)));
    NormalizeVector(rayVector);

    case FLineOperation of
      loNewLine:
        begin
          GLPlane1.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal);
          if assigned(FCurrentLine.SelectedNode) then
            AddLineNodeToTree(FCurrentLine.SelectedNode);
          NewLine(AffineVectorMake(iPoint));
        end;
      loSelectLine:
        begin
          FCurrentLine.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal);
          TreeLineItemSelect(FCurrentLine.SelectLineItem(iPoint.X,
            iPoint.Z, 1.4));
        end;
      loInsertNode:
        begin
          FCurrentLine.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal);
          FCurrentLine.SelectLineItem(iPoint.X, iPoint.Z, 1.4);
          if assigned(FCurrentLine.SelectedLineItem) then
            InsertNode(FCurrentLine.Node1, FCurrentLine.Node2);
          if assigned(FCurrentLine.SelectedNode) then
            FCurrentLine.SelectedNode.AsAffineVector :=
              AffineVectorMake(iPoint.X, 0, iPoint.Z);
          TreeLineItemSelect(FCurrentLine.SelectedLineItem);
          FLineOperation := loMoveNode;
        end;
      loMoveNode:
        begin
          if assigned(FCurrentLine.SelectedNode) then
          begin
            FLineOperation := loSelectLine;
            AddLineNodeToTree(FCurrentLine.SelectedNode);
          end;
        end;
    end;
  end;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  rayStart, rayVector, iPoint, iNormal: TVector;
begin
  if assigned(FCurrentLine.SelectedNode) then
  begin
    SetVector(rayStart, TGLSceneViewer(Sender).Camera.AbsolutePosition);
    SetVector(rayVector, TGLSceneViewer(Sender).Buffer.ScreenToVector
      (AffineVectorMake(X, TGLSceneViewer(Sender).Height - Y, 0)));
    NormalizeVector(rayVector);
    case FLineOperation of
      loNewLine, loMoveNode:
        begin
          if GLPlane1.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal)
          then
          begin
            FCurrentLine.SelectedNode.AsAffineVector :=
              AffineVectorMake(iPoint.X, 0, iPoint.Z);
          end;
        end;
    end;
  end;

  if ssLeft in Shift then
  begin
    if Assigned(FCurrentLine.SelectedNode) then
    begin
      SetVector(rayStart, TGLSceneViewer(Sender).Camera.AbsolutePosition);
      SetVector(rayVector, TGLSceneViewer(Sender).Buffer.ScreenToVector
        (AffineVectorMake(X, TGLSceneViewer(Sender).Height - Y, 0)));
      NormalizeVector(rayVector);
      if GLPlane1.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal) then
      begin
        FCurrentLine.SelectedNode.AsVector := iPoint;
      end;
    end;
  end;

  if ssRight in Shift then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  lLineItem: TLineItem;
  i: Integer;
begin
  Randomize;
  FCurrentLine := TGLMeshLines(GLDummyCube1.AddNewChild(TGLMeshLines));
  FCurrentLine.BeginUpdate;
  FCurrentLine.NoZWrite := True;
  FCurrentLine.ShowNodes := True;
  FCurrentLine.Material.MaterialLibrary := GLMaterialLibrary1;
  FCurrentLine.Material.LibMaterialName := 'road';
  FCurrentLine.LightmapLibrary := GLMaterialLibrary1;
  FCurrentLine.LightmapIndex := 0;
  FCurrentLine.LightmapBounds.Left := -15;
  FCurrentLine.LightmapBounds.Right := 15;
  FCurrentLine.LightmapBounds.Top := 15;
  FCurrentLine.LightmapBounds.Bottom := -15;

  lLineItem := FCurrentLine.Lines.Add('Paul');
  lLineItem.Nodes.AddNode(-01.79, 00, 06.03);
  lLineItem.Nodes.AddNode(-00.81, 00, 02.55);
  lLineItem.Nodes.AddNode(-05.66, 00, 00.77);
  lLineItem.Nodes.AddNode(-07.43, 00, 03.13);
  lLineItem.Nodes.AddNode(01.77, 00, 07.98);
  lLineItem.SplineMode := lsmCubicSpline;
  lLineItem.Division := 10;
  lLineItem.TextureCorrection := True;
  lLineItem.BreakAngle := -1;
  lLineItem.TextureLength := 2;
  lLineItem.Width := 1;
  AddLineToTree(lLineItem);

  lLineItem := FCurrentLine.Lines.Add('van');
  lLineItem.Nodes.AddNode(02.45, 00, -02.5);
  lLineItem.Nodes.AddNode(04.95, 00, 02.34);
  lLineItem.Nodes.AddNode(00.26, 00, 01.38);
  lLineItem.SplineMode := lsmCubicSpline;
  lLineItem.Division := 10;
  lLineItem.TextureCorrection := True;
  lLineItem.BreakAngle := -1;
  lLineItem.TextureLength := 2;
  lLineItem.Width := 1;
  AddLineToTree(lLineItem);

  lLineItem := FCurrentLine.Lines.Add('Dinther1');
  lLineItem.Nodes.AddNode(-02.05, 00, -06.99);
  lLineItem.Nodes.AddNode(06.84, 00, -00.43);
  lLineItem.TextureCorrection := True;
  lLineItem.BreakAngle := -1;
  lLineItem.TextureLength := 2;
  lLineItem.Width := 1;
  AddLineToTree(lLineItem);

  lLineItem := FCurrentLine.Lines.Add('Dinther2');
  lLineItem.Nodes.AddNode(-01.63, 00, -05.57);
  lLineItem.Nodes.AddNode(01.13, 00, -08.43);
  lLineItem.Nodes.AddNode(04.61, 00, -07.22);
  lLineItem.Nodes.AddNode(07.26, 00, -04.43);
  lLineItem.Nodes.AddNode(05.64, 00, -00.32);
  lLineItem.SplineMode := lsmCubicSpline;
  lLineItem.Division := 10;
  lLineItem.TextureCorrection := True;
  lLineItem.BreakAngle := -1;
  lLineItem.TextureLength := 2;
  lLineItem.Width := 1;
  AddLineToTree(lLineItem);

  FCurrentLine.EndUpdate;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  if assigned(FLineItem) then
    FLineItem.BreakAngle := StrToIntDef(Edit1.text, -1);
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // Note that 1 wheel-step induces a WheelDelta of 120,
  // this code adjusts the distance to target with a 10% per wheel-step ratio
  GLSceneViewer1.Camera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  EndLineOperation;
  FCurrentLine.Clear;
  TreeView1.Items.Clear;
  FCurrentLine.StructureChanged;
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FSelectedNode := nil;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i, j: Integer;
  lLineItem: TLineItem;
  X, Y: single;
begin
  EndLineOperation;
  FCurrentLine.BeginUpdate;
  TreeView1.Items.BeginUpdate;
  for i := 0 to StrToIntDef(Edit1.text, 1) - 1 do
  begin
    lLineItem := FCurrentLine.Lines.Add;
    lLineItem.TextureCorrection := True;
    MakeNewLineName(lLineItem);
    X := -15 + Random * 30;
    Y := -15 + Random * 30;
    for j := 0 to StrToIntDef(Edit4.text, 1) - 1 do
    begin
      lLineItem.Nodes.AddNode(X + Random * 3 - 1.5, 0, Y + Random * 3 - 1.5);
    end;
    AddLineToTree(lLineItem);
    lLineItem.Division := 10;
    lLineItem.Width := 0.1;
    lLineItem.BreakAngle := -1;
    lLineItem.TextureLength := 0.1;
  end;
  FCurrentLine.EndUpdate;
  TreeView1.Items.EndUpdate;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := format('MeshLines - %.2f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  lLineItem: TLineItem;
  lLineNode: TLineNode;
begin
  if assigned(TreeView1.Selected) then
  begin
    if TreeView1.Selected.Data <> nil then
    begin
      FUpdatingControls := True;
      if TObject(TreeView1.Selected.Data) is TLineItem then
      begin
        lLineItem := TLineItem(TreeView1.Selected.Data);
        FCurrentLine.SelectLineItem(lLineItem);
        NodePnl.Visible := False;
        LinePnl.Visible := True;
        NameEdt.text := FCurrentLine.SelectedLineItem.Name;
        LineWidthEdt.text := FloatToStr(FCurrentLine.SelectedLineItem.Width);
        LineTextureCorrectioncb.Checked :=
          FCurrentLine.SelectedLineItem.TextureCorrection;
        LineSplinecb.Checked := FCurrentLine.SelectedLineItem.SplineMode =
          lsmCubicSpline;
        LineBreakAngleEdt.text :=
          FloatToStr(FCurrentLine.SelectedLineItem.BreakAngle);
        LineDivisionEdt.text :=
          FloatToStr(FCurrentLine.SelectedLineItem.Division);
        LineTextureLengthEdt.text :=
          FloatToStr(FCurrentLine.SelectedLineItem.TextureLength);
        LineHidecb.Checked := FCurrentLine.SelectedLineItem.Hide;
      end;
      if TObject(TreeView1.Selected.Data) is TLineNode then
      begin
        lLineNode := TLineNode(TreeView1.Selected.Data);
        FCurrentLine.SelectLineItem(lLineNode);
        NodePnl.Visible := True;
        LinePnl.Visible := False;
        NodeXEdt.text := FloatToStr(FCurrentLine.SelectedNode.X);
        NodeYEdt.text := FloatToStr(FCurrentLine.SelectedNode.Y);
        NodeZEdt.text := FloatToStr(FCurrentLine.SelectedNode.Z);
      end;
      FUpdatingControls := False;
    end;
  end;
end;

procedure TForm1.NodeXEdtChange(Sender: TObject);
begin
  if not FUpdatingControls and Assigned(FCurrentLine.SelectedNode) then
    FCurrentLine.SelectedNode.X := StrToFloatDef(NodeXEdt.text,
      FCurrentLine.SelectedNode.X);
end;

procedure TForm1.NodeYEdtChange(Sender: TObject);
begin
  if not FUpdatingControls and Assigned(FCurrentLine.SelectedNode) then
    FCurrentLine.SelectedNode.Y := StrToFloatDef(NodeYEdt.text,
      FCurrentLine.SelectedNode.Y);
end;

procedure TForm1.NodeZEdtChange(Sender: TObject);
begin
  if not FUpdatingControls and Assigned(FCurrentLine.SelectedNode) then
    FCurrentLine.SelectedNode.Z := StrToFloatDef(NodeZEdt.text,
      FCurrentLine.SelectedNode.Z);
end;

procedure TForm1.LineWidthEdtChange(Sender: TObject);
begin
  if not FUpdatingControls and Assigned(FCurrentLine.SelectedLineItem) then
    FCurrentLine.SelectedLineItem.Width := StrToFloatDef(LineWidthEdt.text,
      FCurrentLine.SelectedLineItem.Width);
end;

procedure TForm1.LineTextureCorrectioncbClick(Sender: TObject);
begin
  if not FUpdatingControls and Assigned(FCurrentLine.SelectedLineItem) then
    FCurrentLine.SelectedLineItem.TextureCorrection :=
      LineTextureCorrectioncb.Checked;
end;

procedure TForm1.LineSplinecbClick(Sender: TObject);
begin
  if not FUpdatingControls and Assigned(FCurrentLine.SelectedLineItem) then
    if LineSplinecb.Checked then
      FCurrentLine.SelectedLineItem.SplineMode := lsmCubicSpline
    else
      FCurrentLine.SelectedLineItem.SplineMode := lsmLines;
end;

procedure TForm1.LineBreakAngleEdtChange(Sender: TObject);
begin
  if not FUpdatingControls and Assigned(FCurrentLine.SelectedLineItem) then
    FCurrentLine.SelectedLineItem.BreakAngle :=
      StrToFloatDef(LineBreakAngleEdt.text,
      FCurrentLine.SelectedLineItem.BreakAngle);
end;

procedure TForm1.LineDivisionEdtChange(Sender: TObject);
begin
  if not FUpdatingControls and Assigned(FCurrentLine.SelectedLineItem) then
    FCurrentLine.SelectedLineItem.Division := StrToIntDef(LineDivisionEdt.text,
      FCurrentLine.SelectedLineItem.Division);
end;

procedure TForm1.LineTextureLengthEdtChange(Sender: TObject);
begin
  if not FUpdatingControls and Assigned(FCurrentLine.SelectedLineItem) then
    FCurrentLine.SelectedLineItem.TextureLength :=
      StrToFloatDef(LineTextureLengthEdt.text,
      FCurrentLine.SelectedLineItem.TextureLength);
end;

procedure TForm1.LineHidecbClick(Sender: TObject);
begin
  if not FUpdatingControls and Assigned(FCurrentLine.SelectedLineItem) then
    FCurrentLine.SelectedLineItem.Hide := LineHidecb.Checked;
end;

procedure TForm1.NameEdtChange(Sender: TObject);
begin
  if not FUpdatingControls and Assigned(FCurrentLine.SelectedLineItem) then
    FCurrentLine.SelectedLineItem.Name := NameEdt.text;
end;

procedure TForm1.sbNewLineClick(Sender: TObject);
begin
  EndLineOperation;
  FLineOperation := loNewLine;
end;

procedure TForm1.sbDelNodeClick(Sender: TObject);
begin
  EndLineOperation;
  if assigned(FCurrentLine.SelectedNode) then
    DeleteLineNode(FCurrentLine.SelectedNode);
end;

procedure TForm1.sbDelLineClick(Sender: TObject);
begin
  EndLineOperation;
  DeleteLine(FCurrentLine.SelectedLineItem);
end;

procedure TForm1.sbEndLineClick(Sender: TObject);
begin
  EndLineOperation;
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
begin
  EndLineOperation;
  FLineOperation := loInsertNode;
end;

end.
