unit Main;


interface

uses
  LCLType,  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLHUDObjects, GLObjects, GLGraph, GLScene, GLGeomObjects,
  GLCoordinates, GLCrossPlatform, GLLCLViewer, GLBitmapFont,
  GLWindowsFont, ComCtrls, GLParticles, GLColor, OpenGLTokens,
  uGlobal, Menus, GLBaseClasses, GLVectorGeometry, ExtDlgs ;

type
  TViewForm = class(TForm)
    GLSViewer: TGLSceneViewer;
    GLScene1: TGLScene;
    GLLight: TGLLightSource;
    CameraCube: TGLDummyCube;
    Camera: TGLCamera;
    GLxyGrid: TGLXYZGrid;
    Fields: TGLDummyCube;
    GLWinBmpFont: TGLWindowsBitmapFont;
    StatusBar: TStatusBar;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Options1: TMenuItem;
    DefaultLayout1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    Exit1: TMenuItem;
    Grid1: TMenuItem;
    GridColours1: TMenuItem;
    Evaluate1: TMenuItem;
    GLxzGrid: TGLXYZGrid;
    GLyzGrid: TGLXYZGrid;
    TargetCube: TGLDummyCube;
    xCoordLine: TGLLines;
    yCoordLine: TGLLines;
    zCoordLine: TGLLines;
    BoxLine1: TGLLines;
    BoxLine2: TGLLines;
    BoxLine3: TGLLines;
    BoxLine4: TGLLines;
    CoordText1: TMenuItem;
    YCoordsCube: TGLDummyCube;
    XCoordsCube: TGLDummyCube;
    ZCoordsCube: TGLDummyCube;
    xArrow: TGLArrowLine;
    yArrow: TGLArrowLine;
    Recent1: TMenuItem;
    DerivativeOps: TMenuItem;
    AddedField: TGLDummyCube;
    AddXLine: TGLLines;
    AddYLine: TGLLines;
    AddZLine: TGLLines;
    PlotColours1: TMenuItem;
    DerivativePlotColours1: TMenuItem;
    VolumeLines: TGLDummyCube;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure GLSViewerMouseDown(Sender: TObject; Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);
    procedure GLSViewerMouseMove(Sender: TObject;
                                  Shift: TShiftState; X, Y: Integer);
    procedure GLSViewerMouseUp(Sender: TObject; Button: TMouseButton;
                                Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DefaultLayout1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Grid1Click(Sender: TObject);
    procedure GridColours1Click(Sender: TObject);
    procedure Evaluate1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Saveas1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure CoordText1Click(Sender: TObject);
    procedure RecentFilesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DerivativeOpsClick(Sender: TObject);
    procedure PlotColours1Click(Sender: TObject);
    procedure DerivativePlotColours1Click(Sender: TObject);
  private
    { Private declarations }
    AtStart: Boolean;
    SelectedData: TPlotData;    { data used to evaluate dz/dx or dz/dy }
    procedure ShowCameraLocation;
    procedure ShowFocalLength;
    procedure ShowLightLocation;
    procedure DefaultView;
    procedure DefaultLayout;

    procedure Formulate(const x, y: Single; var z: Single;
              var color: TColorVector; var texPoint: TTexPoint);
    procedure CreateHeightFields(const n: integer);
    procedure PlotFunctions;
    procedure CreateAddedField;
    procedure PlotDerivativeField;
    procedure PlotIntegralField;
  public
    { Public declarations }
    MousePoint: TPoint;
    procedure ShowDisplacement;
    procedure UpdatePlot;
    procedure UpdateAdded;
    procedure ClearAddedField;
    procedure ClearAddedLines;
  end;

const
  crLightxz  = 1;
  crLightyz  = 2;
  crLightxy  = 3;
  crSlidexy  = 4;
  crSlideyz  = 5;
  crSlidexz  = 6;
  crRotate   = 7;
  crZoom     = 8;
  crHandMove = 9;
  crSlidezy  = 10;

var
  ViewForm: TViewForm;

implementation

{$R *.lfm}
{$R CURSORS.RES}

uses
  IniFiles,
  DateUtils,
  uParser, GLState, GLMaterial, Math, GLVectorTypes,
  GridOptions, Functions, Evaluate, CoordOptions,
  DerivativeOptions, GridColors, PlotColors, AddPlotColors;


function LoadCursorFromRes(CursorName:String):THandle;
var
   Cur: TCursorImage;
begin
   Cur := TCursorImage.Create;
   Cur.LoadFromResourceName(HInstance,CursorName);
   result := Cur.ReleaseHandle;
   Cur.Free;
end;

procedure TViewForm.FormCreate(Sender: TObject);
var
  aFolder: TFileName;

begin
  MainPath :=  ExtractFilePath(Application.ExeName);
  MainPath := IncludeTrailingPathDelimiter(MainPath);

  aFolder := Copy(MainPath, 1, 3)+'Plot 3D\';

  DataPath := aFolder + 'Examples\';
  if not DirectoryExists(DataPath) then ForceDirectories(DataPath);
  ImagePath := aFolder + 'Images\';
  if not DirectoryExists(ImagePath) then ForceDirectories(ImagePath);
  LayoutFName := aFolder + 'Layout.lay';
  RecentFName := aFolder + 'Recent.ini';


  if not DirectoryExists(ImagePath) then ForceDirectories(ImagePath);
  if not DirectoryExists(DataPath) then ForceDirectories(DataPath);

  Screen.Cursors[crLightxy] := LoadCursorFromRes('LIGHTXY');
  Screen.Cursors[crLightyz] := LoadCursorFromRes('LIGHTYZ');
  Screen.Cursors[crLightxz] := LoadCursorFromRes('LIGHTXZ');
  Screen.Cursors[crSlidexy] := LoadCursorFromRes('SLIDEXY');
  Screen.Cursors[crSlidexz] := LoadCursorFromRes('SLIDEXZ');
  Screen.Cursors[crSlideyz] := LoadCursorFromRes('SLIDEYZ');
  Screen.Cursors[crRotate]  := LoadCursorFromRes('ROTATE');
  Screen.Cursors[crZoom]    := LoadCursorFromRes('ZOOM');
  Screen.Cursors[crSlidezy] := LoadCursorFromRes('SLIDEZY');
  AtStart := True;
end;

procedure TViewForm.FormDestroy(Sender: TObject);
begin
  while Fields.Count > 0 do TGLHeightField(Fields.Children[0]).Free;
  ClearAddedField;
  ClearAddedLines;
end;

procedure TViewForm.FormShow(Sender: TObject);
var
  LayFile: File of TLayout;
  ini: TIniFile;
  i, c: integer;
  s: string;

begin
  if FileExists(LayoutFName) then
  begin
    try
      AssignFile(LayFile, LayoutFName);
      try
        Reset(LayFile);
        Read(LayFile, Layout);
      finally
        CloseFile(LayFile);
      end;

      with Layout do
      begin
        if IsMaximize then WindowState := wsMaximized
        else
        begin
          WindowState := wsNormal;
          Left := MainLeft;
          Top := MainTop;
          Width := MainWidth;
          Height := MainHeight;
        end;
        GraphFName := CurrentGraphFName;
        DataPath := CurrentDataPath;
        ImagePath := CurrentImagePath;

        with FunctionsForm do
        begin
          Left := FuncLeft;
          Top := FuncTop;
          Width := FuncWidth;
          Height := FuncHeight;
        end;

        if GridsVisible then GridOptionsForm.Show;
        with GridOptionsForm do
        begin
          Left := GridsLeft;
          Top := GridsTop;
        end;

        with GridColorsForm do
        begin
          Left := GridColorsLeft;
          Top := GridColorsTop;
        end;

        with PlotColorsForm do
        begin
          Left := PlotColorsLeft;
          Top := PlotColorsTop;
        end;

        if EvaluateVisible then EvaluateForm.Show;
        with EvaluateForm do
        begin
          Left := EvaluateLeft;
          Top := EvaluateTop;
        end;

        if CoordVisible then CoordsForm.Show;
        with CoordsForm do
        begin
          Left := CoordLeft;
          Top := CoordTop;
        end;

        with DerivativesForm do
        begin
          Left := DerivLeft;
          Top := DerivTop;
        end;

        with AddPlotColorsForm do
        begin
          Left := AddColorsLeft;
          Top := AddColorsTop;
        end;
      end;
      FunctionsForm.EditMinX.SetFocus;
    except
      MessageDlg('File Error! An Error has occurred when attempting to read'+
           #13#10'"'+LayoutFName+'".'+
           #13#10'The default layout will be used.',
           mtError, [mbOK], 0);
      DefaultLayout;
    end;
  end
  else DefaultLayout;

  if DataPath = '' then DataPath := MainPath + 'Examples\';
  if ImagePath = '' then ImagePath := MainPath + 'Images\';
  if not DirectoryExists(DataPath) then ForceDirectories(DataPath);
  if not DirectoryExists(ImagePath) then ForceDirectories(ImagePath);

  ShowCameraLocation;
{ focallength: right mouse drag up/down }
  ShowFocalLength;
{ displace origin: x axis: ctrl/left mouse drag left/right
                   y axis: alt/left mouse drag up/down
                   z axis: ctrl/left mouse drag up/down }
  ShowDisplacement;
{ move light: x axis: ctrl right mouse drag left/right
              y axis: alt right mouse drag up/down
              z axis: ctrl right mouse drag up/down }
  ShowLightLocation;

  FunctionsForm.ReadAndShowInitialData;
  Caption := GraphFName;
  Altered := False;
  CreateHeightFields(FunctionsForm.CheckListBox.Count);

  ini := TIniFile.Create(RecentFName);
  with ini do
  try
    c := ReadInteger(Name, 'RecentCount', 0);
    for i := 0 to c-1 do
    begin
      Recent1.Add(TMenuItem.Create(Self));
      Recent1.Items[i].Caption := ReadString(Name, IntToStr(i), '');
      Recent1.Items[i].OnClick := RecentFilesClick;
    end;
  finally
    Free;
  end;
end;

procedure TViewForm.FormActivate(Sender: TObject);
begin
  if AtStart then
  begin
    PlotFunctions;
    if GridColorsForm.Visible then GridColorsForm.ShowGridColorData;
    if PlotColorsForm.Visible then PlotColorsForm.ShowPlotColorData;
    AtStart := False;
  end;
end;

procedure TViewForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ini: TIniFile;
  i: integer;

begin
  ini := TIniFile.Create(RecentFName);
  with ini do
  try
    WriteInteger(Name, 'RecentCount', Recent1.Count);
    for i := 0 to Recent1.Count-1 do
    WriteString(Name, IntToStr(i), Recent1.Items[i].Caption);
  finally
    Free;
  end;
end;

procedure TViewForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  f: File of TLayout;

begin
  with Layout do
  begin
    IsMaximize := (Width >= Screen.Width) and (Height >= Screen.Height);
    MainLeft := Left;
    MainTop := Top;
    MainWidth := Width;
    MainHeight := Height;
    if GraphFName = '' then GraphFName := NewFName;
    CurrentGraphFName := GraphFName;
    CurrentDataPath := DataPath;
    CurrentImagePath := ImagePath;
    with FunctionsForm do
    begin
      FuncLeft := Left;
      FuncTop := Top;
      FuncWidth := Width;
      FuncHeight := Height;
    end;

    GridsVisible := GridOptionsForm.Visible;
    with GridOptionsForm do
    begin
      GridsLeft := Left;
      GridsTop := Top;
    end;

    with GridColorsForm do
    begin
      GridColorsLeft := Left;
      GridColorsTop := Top;
    end;

    with PlotColorsForm do
    begin
      PlotColorsLeft := Left;
      PlotColorsTop := Top;
    end;

    EvaluateVisible := EvaluateForm.Visible;
    with EvaluateForm do
    begin
      EvaluateLeft := Left;
      EvaluateTop := Top;
    end;

    CoordVisible := CoordsForm.Visible;
    with CoordsForm do
    begin
      CoordLeft := Left;
      CoordTop := Top;
    end;

    with DerivativesForm do
    begin
      DerivLeft := Left;
      DerivTop := Top;
      if Visible then Close;
    end;;

    with AddPlotColorsForm do
    begin
      AddColorsLeft := Left;
      AddColorsTop := Top;
      if Visible then Close;
    end;
  end;

  try
    AssignFile(f, LayoutFName);
    try
      Rewrite(f);
      write(f, Layout);
    finally
      CloseFile(f);
    end;
  except
    MessageDlg('File Error! An Error has occurred'+
         #13#10'when attempting to write to "'+LayoutFName+'".',
    mtError, [mbOK], 0);
  end;

  if Altered or GridColorsAltered or DerivativeAltered then
  begin
    case MessageDlg('The current graph''s data has been altered.'+
              #13#10'Do you wish to save the alterations ?', mtConfirmation,
                    [mbYes, mbNo, mbCancel], 0) of
    mrYes: FunctionsForm.SaveClick(Sender);
 mrCancel: begin
             CanClose := False;
             Exit;
           end;
    end;
  end;
end;

procedure TViewForm.FormKeyDown(Sender: TObject; var Key: Word;
                                 Shift: TShiftState);
var
  d: integer;

begin
  d := 0;
  case Key of
  VK_ADD:      d := -1; { zoom in }
  VK_SUBTRACT: d :=  1; { zoom out }
  end;
  if Key in [VK_ADD,  VK_SUBTRACT] then
  begin
    Screen.Cursor := crZoom;
    GLSViewer.SetFocus;
{ each step adjusts target distance by 2.5% another method to zoom in or out }
    Camera.AdjustDistanceToTarget(Power(1.025, d));
    ShowCameraLocation;
  end
  else
  case Key of
  VK_HOME, VK_NUMPAD7, 72: DefaultView; {'H'/'h' key}
(*-> = 39
<- = 37
^  = 38
|  = 40
-> and ^ = 33
<- and | = 35*)
  end;
  Key := 0;
end;

procedure TViewForm.FormKeyUp(Sender: TObject; var Key: Word;
                               Shift: TShiftState);
begin
  Screen.Cursor := crDefault;
end;

procedure TViewForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
            WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if (MousePoint.X >= GLSViewer.Left) and
     (MousePoint.X <= GLSViewer.Left + GLSViewer.Width) and
     (MousePoint.Y >= GLSViewer.Top) and
     (MousePoint.y <= GLSViewer.Top + GLSViewer.Height) then
  begin
{ a wheel step = WheelDelta/300; each step adjusts target distance by 2.5%
  another method to zoom in or out }
    GLSViewer.SetFocus;
    Camera.AdjustDistanceToTarget(Power(1.025, WheelDelta/300));
    ShowCameraLocation;
  end;
end;

procedure TViewForm.GLSViewerMouseDown(Sender: TObject; Button: TMouseButton;
                                        Shift: TShiftState; X, Y: Integer);
begin
  MousePoint.X := X;
  MousePoint.Y := Y;
  if ssShift in Shift then        { Shift key down }
  begin
    if ssLeft in Shift then Screen.Cursor := crZoom;
  end
  else if ssCtrl in Shift then    { Ctrl key down }
  begin
    if ssLeft in Shift then Screen.Cursor := crSlidexz
    else
    if ssRight in Shift then Screen.Cursor := crLightxz;
  end
  else if ssAlt in Shift then     { Alt key down }
  begin
    if ssLeft in Shift then Screen.Cursor := crSlidezy
    else
    if ssRight in Shift then Screen.Cursor := crLightxy;
  end
  else { no shift, ctrl or alt key }
  begin
    if Shift = [ssLeft] then Screen.Cursor := crRotate
    else
    if Shift = [ssRight] then Screen.Cursor := crZoom;
  end;
end;

procedure TViewForm.GLSViewerMouseMove(Sender: TObject;
                                        Shift: TShiftState; X, Y: Integer);
var
  dx, dy: integer;

begin { refer GLScene\Demos\interface\camera\Camera.dpr }
  if MousePoint.X = MaxInt then    { FileOpenDialog is visible }
  begin
    MousePoint.X := X;
    Exit;
  end;

  dx := MousePoint.X - X;
  dy := MousePoint.Y - Y;

  if ssShift in Shift then   { shift key down }
  begin
    if ssLeft in Shift then  { shift - left mouse button }
    begin
  { dy = a step which adjusts target distance by 1.25%; zoom in or out }
      with Camera do AdjustDistanceToTarget(Power(1.0125, dy));
      ShowCameraLocation;
    end;
  end
  else
  if ssCtrl in Shift then    { Ctrl key down }
  begin
    if ssLeft in Shift then  { Ctrl - left mouse button }
    begin
      TargetCube.Position.X :=
      TargetCube.Position.X - dx*GLxzGrid.XSamplingScale.Step/10;
      TargetCube.Position.Z :=
      TargetCube.Position.Z - dy*GLxzGrid.ZSamplingScale.Step*ViewData.xyGrid.zScale/10;
      ShowDisplacement;
    end;
    if ssRight in Shift then { Ctrl - right mouse button }
    begin
      GLLight.Position.Z := GLLight.Position.Z + dy/10;
      GLLight.Position.X := GLLight.Position.X + dx/10;
      ShowLightLocation;
    end;
  end
  else
  if ssAlt in Shift then     { Alt key down }
  begin
    if ssRight in Shift then { Alt - right mouse button }
    begin
      GLLight.Position.X := GLLight.Position.X + dx/10;
      GLLight.Position.Y := GLLight.Position.Y + dy/10;
      ShowLightLocation;
    end
    else
    if ssLeft in Shift then  { Alt - left mouse button }
    begin
      TargetCube.Position.Y :=
      TargetCube.Position.Y + dx*GLyzGrid.YSamplingScale.Step/10;
      TargetCube.Position.Z :=
      TargetCube.Position.Z - dy*GLyzGrid.ZSamplingScale.Step*ViewData.xyGrid.zScale/10;
      ShowDisplacement;
    end;
  end
  else  { no shift key }
  begin
    if Shift = [ssLeft] then
  { Left mouse button changes camera angle by moving around target }
    begin
      Camera.MoveAroundTarget(dy, dx);
      ShowCameraLocation;
    end;
    if Shift = [ssRight] then
    begin
  { Right mouse button alters the camera's focal length;
    zoom out or in by moving cursor up or down }
      with Camera do
      begin
        FocalLength  := FocalLength - dy;
        if FocalLength > 3000 then FocalLength := 3000;   { max focal length }
        if FocalLength < 10 then FocalLength := 10;       { min focal length }
      end;
      ShowFocalLength;       { display in statusbar palel }
    end;
  end;
  MousePoint.X := X;         { update mouse position }
  MousePoint.Y := Y;
end;

procedure TViewForm.GLSViewerMouseUp(Sender: TObject; Button: TMouseButton;
                                      Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

procedure TViewForm.New1Click(Sender: TObject);
begin
  FunctionsForm.New1Click(Sender);
end;

procedure TViewForm.Open1Click(Sender: TObject);
begin
  FunctionsForm.Open1Click(Sender);
end;

procedure TViewForm.Save1Click(Sender: TObject);
begin
  FunctionsForm.SaveClick(Sender);
end;

procedure TViewForm.Saveas1Click(Sender: TObject);
begin
  FunctionsForm.SaveAsClick(Sender);
end;

procedure TViewForm.ShowCameraLocation;
begin
  with Camera.Position do
  StatusBar.Panels[0].Text := 'Camera: '+FloatToStrF(X, ffNumber, 5, 2)+', '+
  FloatToStrF(Y, ffNumber, 5, 2)+', '+FloatToStrF(Z, ffNumber, 5, 2);
end;

procedure TViewForm.GridColours1Click(Sender: TObject);
begin
  GridColorsForm.Show;
end;

procedure TViewForm.CoordText1Click(Sender: TObject);
begin
  CoordsForm.Show;
end;

procedure TViewForm.ShowFocalLength;
begin
  with Camera do
  StatusBar.Panels[1].Text := 'f = '+FloatToStrF(FocalLength, ffnumber, 5, 2);
end;

procedure TViewForm.ShowDisplacement;
begin
  with TargetCube.Position do
  StatusBar.Panels[2].Text := 'Displaced: '+
  FloatToStrF(-X, ffNumber, 5, 2)+', '+FloatToStrF(-Y, ffNumber, 5, 2)+', '+
  FloatToStrF(-Z, ffNumber, 5, 2);
end;

procedure TViewForm.Evaluate1Click(Sender: TObject);
begin
  EvaluateForm.Show;
end;

procedure TViewForm.ShowLightLocation;
begin
  with GLLight.Position do
  StatusBar.Panels[3].Text := 'Light: '+
  FloatToStrF(X, ffNumber, 5, 2)+', '+FloatToStrF(Y, ffNumber, 5, 2)+', '+
  FloatToStrF(Z, ffNumber, 5, 2);
end;

procedure TViewForm.Grid1Click(Sender: TObject);
begin
  GridOptionsForm.Show;
end;

procedure TViewForm.DefaultLayout1Click(Sender: TObject);
begin
  DefaultLayout;
end;

procedure TViewForm.DefaultView;
begin
  CameraCube.Position.SetPoint(0, 0, 0);
  ShowDisplacement;
  Camera.FocalLength := 200;
  ShowFocalLength;
  Camera.Position.SetPoint(50, 50, 30);
  Camera.DepthOfView := 1000;
  ShowCameraLocation;
  GLLight.Position.SetPoint(50, 50, 50);
  ShowLightLocation;
  TargetCube.Position.SetPoint(0, 0, 0);
end;

procedure TViewForm.RecentFilesClick(Sender: TObject);
begin
  FunctionsForm.OpenRecentFile(TMenuItem(Sender).Caption);
end;

procedure TViewForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TViewForm.DefaultLayout;
begin
  WindowState := wsNormal;
  ViewForm.Left := 0;
  ViewForm.Top := 0;
  FunctionsForm.Width := 335;
  FunctionsForm.Height := 387;
  ViewForm.Width := Screen.Width - FunctionsForm.Width + 18;
  ViewForm.Height := Screen.Height - 40;
  FunctionsForm.Left := ViewForm.Left + ViewForm.Width - 14;
  FunctionsForm.Top := ViewForm.Top;
  GridOptionsForm.Left := FunctionsForm.Left +5;
  GridOptionsForm.Top := FunctionsForm.Top + FunctionsForm.Height - 6;
  EvaluateForm.Left := GridOptionsForm.Left;
  EvaluateForm.Top := GridOptionsForm.Top + GridOptionsForm.Height - 3;
  EvaluateForm.Show;
  GridOptionsForm.Show;
  GridColorsForm.Left := 20;
  GridColorsForm.Top := 80;
  PlotColorsForm.Left := 30;
  PlotColorsForm.Top := 100;
  AddPlotColorsForm.Left := 40;
  AddPlotColorsForm.Top := 120;
  FunctionsForm.EditMinX.SetFocus;
end;

procedure TViewForm.DerivativeOpsClick(Sender: TObject);
begin
  if PlotColorsForm.Visible then PlotColorsForm.Close;
  PlotColours1.Enabled := False;
  DerivativePlotColours1.Enabled := True;
  DerivativesForm.Show;
end;

procedure TViewForm.DerivativePlotColours1Click(Sender: TObject);
begin
  if PlotColorsForm.Visible then PlotColorsForm.Close;
  PlotColours1.Enabled := False;
  AddPlotColorsForm.Show;
end;

procedure TViewForm.PlotFunctions;
  procedure PlotFunction(i: Integer);
  begin
    with PlotData do
    begin
      with TGLHeightField(Fields.Children[i]) do
      begin
        with XSamplingScale do
        begin
          Step := xInc;
          Min := xMin;
          Max := xMax;
        end;

        with YSamplingScale do
        begin
          Step := yInc;
          Min := yMin;
          Max := yMax;
        end;

        case ViewMode of
          vmAmbient:ColorMode := hfcmAmbient;
vmAmbientandDiffuse:ColorMode := hfcmAmbientAndDiffuse;
          vmDiffuse:ColorMode := hfcmDiffuse;
         vmEmmision:ColorMode := hfcmEmission;
             vmNone:ColorMode := hfcmNone;
        end;

        case fxyMode of
        fxyFill:Material.PolygonMode := pmFill;
       fxyLines:Material.PolygonMode := pmLines;
      fxyPoints:Material.PolygonMode := pmPoints;
        end;
        OnGetHeight := Formulate;
      end;
      GLSViewer.Refresh;  { needed to display each zField data in list }
    end;
  end;

var
  i: integer;
  fxyParser: TfxyParser;
  PD: TPlotData;          { save the current PlotData of selected function }

begin             { PlotFunctions }
  Screen.Cursor := crHourGlass;
  PD := PlotData;
  fxyParser := TfxyParser.Create(0, 0);
  try
    with FunctionsForm.CheckListBox do
    for i := 0 to Items.Count -1 do if Checked[i] then
    begin
    { an item is checked; get the plot data }
      PlotData := TPlotDataObject(Items.Objects[i]).Data;
      PlotFunction(i);
    end;
  finally
    Screen.Cursor := crDefault;
    fxyParser.Destroy;
    PlotData := PD;       { restor the current PlotData }
  end;
end;

procedure TViewForm.Formulate(const x, y: Single; var z: Single;
                  var color: TColorVector; var texPoint: TTexPoint);
var
  e: byte;
  MaxZ, MinZ: TGLFloat;
  x1, x2, y1, y2, z1, z2: extended;

begin
  case AddedData.AddedAs of
    AddNone:      { no AddedData to plot; just plot the PlotData }
    begin
      z := ParseEvaluateFxy(x, y, PlotData.fxyStr, e);
      z := z*ViewData.xyGrid.zScale;
      with PlotData do
      begin
        MaxZ := zMax*ViewData.xyGrid.zScale;
        MinZ := zMin*ViewData.xyGrid.zScale;
        if zCap then
        begin
          if zLim and (z > MaxZ) then z := MaxZ;
          if zLim and (z < MinZ) then z := MinZ;
        end
        else
        if zLim and ((z < MinZ) or (z > MaxZ)) then z := NaN;
        VectorLerp(LowerColor, UpperColor, z*ColorBlend - ColorMove, color);
      end;
    end;

    AddDerivX:    { this is for partial derivative wrt x }
    begin
      x1 := x - AddedData.xInc;
      x2 := x + AddedData.xInc;
      z1 := ParseEvaluateFxy(x1, y, SelectedData.fxyStr, e);{ evaluate z1,x1,y }
      z2 := ParseEvaluateFxy(x2, y, SelectedData.fxyStr, e);{ evaluate z2,x2,y }
      z := (z2 - z1)/(x2 - x1);                         { z = slope wrt x axis }
      z := z*ViewData.xyGrid.zScale;
      with AddedData do
      begin
        MaxZ := zMax*ViewData.xyGrid.zScale;
        MinZ := zMin*ViewData.xyGrid.zScale;
        if zCap then
        begin
          if zLim and (z > MaxZ) then z := MaxZ;
          if zLim and (z < MinZ) then z := MinZ;
        end
        else
        if zLim and ((z < MinZ) or (z > MaxZ)) then z := NaN;
        VectorLerp(LowerColor, UpperColor, z*ColorBlend - ColorMove, color);
      end;
    end;

    AddDerivY:    { this is for partial derivative wrt y }
    begin
      y1 := y - AddedData.yInc;
      y2 := y + AddedData.yInc;
      z1 := ParseEvaluateFxy(x, y1, SelectedData.fxyStr, e);{ evaluate z1,x,y1 }
      z2 := ParseEvaluateFxy(x, y2, SelectedData.fxyStr, e);{ evaluate z2,x,y2 }
      z := (z2 - z1)/(y2 - y1);                         { z = slope wrt y axis }
      z := z*ViewData.xyGrid.zScale;
      with AddedData do
      begin
        MaxZ := zMax*ViewData.xyGrid.zScale;
        MinZ := zMin*ViewData.xyGrid.zScale;
        if zCap then
        begin
          if zLim and (z > MaxZ) then z := MaxZ;
          if zLim and (z < MinZ) then z := MinZ;
        end
        else
        if zLim and ((z < MinZ) or (z > MaxZ)) then z := NaN;
        VectorLerp(LowerColor, UpperColor, z*ColorBlend - ColorMove, color);
      end;
    end;

    AddVolume:    { this is for double integral }
    begin
      z := ParseEvaluateFxy(x, y, PlotData.fxyStr, e);
      with AddedData do
      begin
        z := z*ViewData.xyGrid.zScale;
        MaxZ := zMax*ViewData.xyGrid.zScale;
        MinZ := zMin*ViewData.xyGrid.zScale;
        if zCap then
        begin
          if zLim and (z > MaxZ) then z := MaxZ;
          if zLim and (z < MinZ) then z := MinZ;
        end
        else
        if zLim and ((z < MinZ) or (z > MaxZ)) then z := NaN;
        VectorLerp(LowerColor, UpperColor, z*ColorBlend - ColorMove, color);

        TGLLines.CreateAsChild(VolumeLines);
        with VolumeLines do
        begin
          with TGLLines(Children[Count -1]) do
          begin
            LineColor.AsWinColor := AddLineColor;
            LineWidth := AddLineWidth;
            NodesAspect := LnaInvisible;

            Nodes.Add;
            Nodes[0].X := x;      { start point }
            Nodes[0].Y := y;
            if zLim then Nodes[0].Z := MinZ else Nodes[0].Z := 0;

            Nodes.Add;
            Nodes[1].X := x;      { end point }
            Nodes[1].Y := y;
            Nodes[1].Z := z;
          end;
        end;
      end;
    end;
  end;    { case AddedData.AddedAs of... }
end;

procedure TViewForm.PlotDerivativeField;
var
  i: integer;
  FoundSelected: Boolean;

begin
  Screen.Cursor := crHourGlass;
  try
    i := 0;
    if FunctionsForm.CheckListBox.Count > 1 then { find Selected item }
    begin
      FoundSelected := False;
      while not FoundSelected and (i < FunctionsForm.CheckListBox.Count) do
      begin
        FoundSelected := FunctionsForm.CheckListBox.Selected[i];
        if not FoundSelected then Inc(i);
      end;
    end;

    with FunctionsForm.CheckListBox do
    SelectedData := TPlotDataObject(Items.Objects[i]).Data;

    with AddedData do
    begin
      with TGLHeightField(AddedField.Children[0]) do
      begin
        with XSamplingScale do
        begin
          Step := xInc;
          Min := xMin;
          Max := xMax;
        end;

        with YSamplingScale do
        begin
          Step := yInc;
          Min := yMin;
          Max := yMax;
        end;

        case ViewMode of
            vmAmbient:ColorMode := hfcmAmbient;
  vmAmbientandDiffuse:ColorMode := hfcmAmbientAndDiffuse;
            vmDiffuse:ColorMode := hfcmDiffuse;
           vmEmmision:ColorMode := hfcmEmission;
               vmNone:ColorMode := hfcmNone;
        end;

        case fxyMode of
        fxyFill:Material.PolygonMode := pmFill;
       fxyLines:Material.PolygonMode := pmLines;
      fxyPoints:Material.PolygonMode := pmPoints;
        end;
        OnGetHeight := Formulate;
      end;
      GLSViewer.Refresh;  { needed to display each zField data in list }
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TViewForm.PlotIntegralField;
  procedure PlotVolume;
  begin   { PlotVolume }
    with AddedData do
    begin
      with TGLHeightField(AddedField.Children[0]) do
      begin
        with XSamplingScale do
        begin
          Step := xInc;
          Min := xMin;
          Max := xMax;
        end;

        with YSamplingScale do
        begin
          Step := yInc;
          Min := yMin;
          Max := yMax;
        end;

        case ViewMode of
          vmAmbient:ColorMode := hfcmAmbient;
vmAmbientandDiffuse:ColorMode := hfcmAmbientAndDiffuse;
          vmDiffuse:ColorMode := hfcmDiffuse;
         vmEmmision:ColorMode := hfcmEmission;
             vmNone:ColorMode := hfcmNone;
        end;

        case fxyMode of
        fxyFill:Material.PolygonMode := pmFill;
       fxyLines:Material.PolygonMode := pmLines;
      fxyPoints:Material.PolygonMode := pmPoints;
        end;
        OnGetHeight := Formulate;
      end;
      GLSViewer.Refresh;  { needed to display each zField data in list }
    end;
  end;    { PlotVolume }

  procedure CalculateVolume;
  var
    e: byte;
    i, j, iCount, jCount: integer;
    x, y, z, x0, y0, a, VolPos, VolNeg: TGLFloat;

  begin   { CalculateVolume }
    with AddedData do
    begin
      a := xInc*yInc;                               { base area }
      iCount := round((xMax - xMin)/xInc) -1;       { number of x points }
      jCount := round((yMax - yMin)/yInc) -1;       { number of y points }
      VolPos := 0;
      VolNeg := 0;
      x0 := xMin + xInc/2;                          { base centre x0 }
      y0 := yMin + yInc/2;                          { base centre y0 }
      for j := 0 to jCount do
      begin
        y := y0 + j*yInc;                           { next column wrt y }
        for i := 0 to iCount do
        begin
          x := x0 + i*xInc;                         { next column wrt x }
          z := ParseEvaluateFxy(x, y, PlotData.fxyStr, e);

          if zLim then                              { zLimit applied }
          begin
            if (zMax >= 0) and (zMin <= 0) then     { above and below zero }
            begin
              if z > 0 then
              begin
                if z > zMax then VolPos := VolPos + a*zMax
                            else VolPos := VolPos + a*z;
              end
              else
              begin
                if z < zMin then VolNeg := VolNeg + a*zMin
                            else VolNeg := VolNeg + a*z;
              end;
            end
            else
            if zMin > 0 then                        { both above zero }
            begin
              if z >= zMin then
              begin
                if z > zMax then VolPos := VolPos + a*(zMax - zMin)
                            else VolPos := VolPos + a*(z - zMin);
              end;
            end
            else
            if zMax < 0 then                        { both below zero }
            begin
              if z <= zMax then
              begin
                if z < zMin then VolNeg := VolNeg + a*(zMax - zMin)
                            else VolNeg := VolNeg + a*(z - zMax);
              end;
            end;
          end
          else                                      { no zLimit applied }
          begin
            if z > 0 then VolPos := VolPos + a*z
                     else VolNeg := VolNeg + a*z;
          end;
        end;
      end;
    end;

    with DerivativesForm do
    begin
      PosVolLabel.Caption := 'Positive Volume: '+FloatToStr(VolPos);
      NegVolLabel.Caption := 'Negative Volume: '+FloatToStr(VolNeg);
      TotalLabel.Caption := 'Absolute Volume: '+FloatToStr(VolPos - VolNeg);
      VolumeLabel.Caption := 'Total Volume: '+FloatToStr(VolPos + VolNeg);
    end;
  end;    { CalculateVolume }

begin   { TViewForm.PlotIntegralField }
  Screen.Cursor := crHourGlass;
  with AddedData do
  begin
    PlotVolume;
    CalculateVolume;
  end;
  DerivativesForm.VolumeRB.Checked := False; //
  Screen.Cursor := crDefault;
end;    { TViewForm.PlotIntegralField }

procedure TViewForm.PlotColours1Click(Sender: TObject);
begin
  PlotColorsForm.Show;
end;

procedure TViewForm.CreateHeightFields(const n: integer);
var
  i: integer;

begin
  while Fields.Count > 0 do TGLHeightField(Fields.Children[0]).Free;
  for i := 0 to n -1 do
  begin
    TGLHeightField.CreateAsChild(Fields);
    TGLHeightField(
    Fields.Children[Fields.Count -1]).Material.BlendingMode := bmTransparency;
  end;
end;

procedure TViewForm.CreateAddedField;
begin
  ClearAddedField;
  TGLHeightField.CreateAsChild(AddedField);
  TGLHeightField(AddedField.Children[0]).Material.BlendingMode := bmTransparency;
end;

procedure TViewForm.UpdatePlot;
begin
  CreateHeightFields(FunctionsForm.CheckListBox.Count);
  PlotFunctions;
end;

procedure TViewForm.UpdateAdded;
begin
  CreateAddedField;
  AddXLine.LineColor.AsWinColor := AddedData.AddLineColor;
  AddYLine.LineColor.AsWinColor := AddedData.AddLineColor;
  AddZLine.LineColor.AsWinColor := AddedData.AddLineColor;
  AddXLine.LineWidth := AddedData.AddLineWidth;
  AddYLine.LineWidth := AddedData.AddLineWidth;
  AddZLine.LineWidth := AddedData.AddLineWidth;
  if AddedData.AddedAs = AddVolume then PlotIntegralField
  else PlotDerivativeField;
end;

procedure TViewForm.ClearAddedField;
begin
  with TGLHeightField(AddedField) do if Count > 0 then Children[0].Free;
end;

procedure TViewForm.ClearAddedLines;
begin
  Screen.Cursor := crHourGlass;
  with TGLLines(VolumeLines) do
  while Count > 0 do Children[Count -1].Free;
  Screen.Cursor := crDefault;
end;

end.
