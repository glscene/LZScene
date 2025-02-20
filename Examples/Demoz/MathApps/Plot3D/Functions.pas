unit Functions;


interface

uses
  LCLType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, Buttons, ComCtrls, Menus, uGlobal;

type
  TFunctionsForm = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    EditMinX: TEdit;
    EditMaxX: TEdit;
    EditdX: TEdit;
    EditMinY: TEdit;
    EditMaxY: TEdit;
    EditdY: TEdit;
    EditMinZ: TEdit;
    EditMaxZ: TEdit;
    zLimitCB: TCheckBox;
    LabelFunc: TLabel;
    Editfxy: TEdit;
    Label9: TLabel;
    AddButton: TSpeedButton;
    DeleteButton: TSpeedButton;
    CheckListBox: TCheckListBox;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save: TMenuItem;
    SaveAs: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    Label17: TLabel;
    ModeComboBox: TComboBox;
    StyleComboBox: TComboBox;
    ApplyBtn: TBitBtn;
    zCountLabel: TLabel;
    Centre: TSpeedButton;
    UpButton: TSpeedButton;
    DownButton: TSpeedButton;
    zCapCB: TCheckBox;
    GridValues: TSpeedButton;
    Label7: TLabel;
    EditNote: TEdit;
    procedure FormDestroy(Sender: TObject);
    procedure FloatKeyPress(Sender: TObject; var Key: Char);
    procedure fxyKeyPress(Sender: TObject; var Key: Char);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AddButtonClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure SaveAsClick(Sender: TObject);
    procedure CheckListBoxClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure zLimitCBClick(Sender: TObject);
    procedure ModeComboBoxChange(Sender: TObject);
    procedure StyleComboBoxChange(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure EditfxyClick(Sender: TObject);
    procedure CheckListBoxClickCheck(Sender: TObject);
    procedure EditdXKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditdYKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditMinXKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditMaxXKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditMinYKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditMaxYKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditMinZKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditMaxZKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditfxyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CentreClick(Sender: TObject);
    procedure UpButtonClick(Sender: TObject);
    procedure DownButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure zCapCBClick(Sender: TObject);
    procedure GridValuesClick(Sender: TObject);
    procedure PositiveKeyPress(Sender: TObject; var Key: Char);
    procedure EditNoteClick(Sender: TObject);
    procedure EditNoteKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    private
    { Private declarations }
    NewFile: Boolean;
    TooManyPoints: integer;
    function ReadData(const FName: TFileName): Boolean;
    procedure OpenSelectedFile(Sender: TObject; FName: TFileName);
    procedure WriteData(const FName: TFileName);
    function DefaultPlotData: TPlotData;
    function DefaultViewData: TViewData;
    function DefaultAddedData: TAddedData;
    procedure ShowData(Sender: TObject);
    procedure AddNewPlot;
    procedure CountzPoints;
    procedure UpdateTPlotDataObject;
    procedure AddRecent(const f: TFileName);
  public
    { Public declarations }
    procedure ReadAndShowInitialData;
    procedure OpenRecentFile(FName: TFileName);
  end;

var
  FunctionsForm: TFunctionsForm;

implementation

uses
  OpenGLTokens, Math, GLVectorGeometry, Main, uParser, GridOptions, GLColor,
  Evaluate, GLVectorTypes, CoordOptions, DerivativeOptions, GridColors,
  PlotColors;

{$R *.lfm}

procedure TFunctionsForm.AddButtonClick(Sender: TObject);
begin
  AddNewPlot;
end;

procedure TFunctionsForm.EditdXKeyUp(Sender: TObject; var Key: Word;
                                      Shift: TShiftState);
var
  x: TGlFloat;

begin
  try
    x := StrToFloat(EditdX.Text);
  except
    x := 1.0;
  end;
  PlotData.xInc := x;
  ApplyBtn.Visible := True;
  UpdateTPlotDataObject;
  CountzPoints;
end;

procedure TFunctionsForm.PositiveKeyPress(Sender: TObject; var Key: Char);
begin
  with Sender as TEdit do
  if not CharInSet(Key, ['+', '0'..'9', '.', #8]) then Key := #0;
end;

procedure TFunctionsForm.EditdYKeyUp(Sender: TObject; var Key: Word;
                                      Shift: TShiftState);
var
  y: TGLFloat;

begin
  try
    y := StrToFloat(EditdY.Text);
  except
    y := 1.0;
  end;
  PlotData.yInc := y;
  ApplyBtn.Visible := True;
  UpdateTPlotDataObject;
  CountzPoints;
end;

procedure TFunctionsForm.EditfxyClick(Sender: TObject);
begin
  ViewForm.MousePoint.X := Maxint;
end;

procedure TFunctionsForm.EditfxyKeyUp(Sender: TObject; var Key: Word;
                                       Shift: TShiftState);
var
  e: byte;

begin
  if Key <> 13 then
  begin
    PlotData.txtStr := Editfxy.Text;
    PlotData.fxyStr := ScanText(Editfxy.Text);
    ParseAndEvaluate(PlotData.fxyStr, e);

    with CheckListBox do if (Count > 0) and (ItemIndex > -1)
    then Items[ItemIndex] := PlotData.txtStr;
    ApplyBtn.Visible := e = 0;
    UpdateTPlotDataObject;
  end;
end;

procedure TFunctionsForm.EditKeyDown(Sender: TObject; var Key: Word;
                                      Shift: TShiftState);
begin
  if (Key = VK_DELETE) or (Key = VK_BACK) then ApplyBtn.Visible := True;
end;

procedure TFunctionsForm.fxyKeyPress(Sender: TObject; var Key: Char);
begin
  with Sender as TEdit do
  begin
    if not CharInSet(UpCase(Key), ParseSet) then
    begin
      Key := #0;
      Exit;
    end;
    if Key = '`' then Key := #176;
  end;
end;

procedure TFunctionsForm.GridValuesClick(Sender: TObject);
var
  DoApply: Boolean;

begin
  with GridOptionsForm do
  DoApply := xyLock.Checked and zLock.Checked and MinLock.Checked;

  if DoApply then
  begin
    with ViewData.xyGrid.xRange do
    begin
      EditMinX.Text := FloatToStrF(Minimum, ffGeneral, 7, 4);
      PlotData.xMin := Minimum;
      EditMaxX.Text := FloatToStrF(Maximum, ffGeneral, 7, 4);
      PlotData.xMax := Maximum;
    end;

    with ViewData.xyGrid.yRange do
    begin
      EditMinY.Text := FloatToStrF(Minimum, ffGeneral, 7, 4);
      PlotData.yMin := Minimum;
      EditMaxY.Text := FloatToStrF(Maximum, ffGeneral, 7, 4);
      PlotData.yMax := Maximum;
    end;

    with ViewData.xzGrid.zRange do
    begin
      EditMinZ.Text := FloatToStrF(Minimum, ffGeneral, 7, 4);
      PlotData.zMin := Minimum;
      EditMaxZ.Text := FloatToStrF(Maximum, ffGeneral, 7, 4);
      PlotData.zMax := Maximum;
    end;
    ApplyBtnClick(Sender);
  end;
end;

procedure TFunctionsForm.ModeComboBoxChange(Sender: TObject);
begin
  PlotData.ViewMode := TViewMode(ModeComboBox.ItemIndex);
  UpdateTPlotDataObject;
  ApplyBtnClick(Sender);
end;

procedure TFunctionsForm.New1Click(Sender: TObject);
var
  i: integer;
  s: string;

begin
  if Altered or GridColorsAltered or DerivativeAltered then
  case MessageDlg('The current graph''s data has been altered.'+
            #13#10'Do you wish to save the alterations ?', mtConfirmation,
                    [mbYes, mbNo, mbCancel], 0) of
    mrYes:SaveClick(Sender);
 mrCancel:Exit;
  end;
  s := DataPath + GraphFName; { save original FName }
  if s <> GraphFName then AddRecent(s);
  with CheckListBox do
  begin
    for i := 0 to Items.Count -1 do Items.Objects[i].Free;
    Clear;
  end;

  GraphFName := NewFName;
  Caption := GraphFName;
  ViewForm.Caption := GraphFName;
  GridOptionsForm.Caption := GraphFName;
  EvaluateForm.Caption := GraphFName;
  CoordsForm.Caption := GraphFName;

  NewFile := True;
  Altered := False;
  ViewData := DefaultViewData;
  PlotData := DefaultPlotData;
  AddedData := DefaultAddedData;

  if GridColorsForm.Visible then GridColorsForm.Close;
  if PlotColorsForm.Visible then PlotColorsForm.Close;
  if DerivativesForm.Visible then DerivativesForm.Close;
  ApplyBtnClick(Sender);
  with CheckListBox do
  begin
    AddItem(PlotData.txtStr, TPlotDataObject.Create(PlotData));
    ItemIndex := Count -1;
    Checked[ItemIndex] := True;
  end;
  ViewForm.TargetCube.Position.SetPoint(0, 0, 0);
  ShowData(Sender);
end;

procedure TFunctionsForm.Open1Click(Sender: TObject);
var
  i: integer;
  s: string;

begin
  if DerivativesForm.Visible then DerivativesForm.Close;
  if GridColorsForm.Visible then GridColorsForm.ShowGridColorData;
  if PlotColorsForm.Visible then PlotColorsForm.ShowPlotColorData;
  if Altered or GridColorsAltered or DerivativeAltered then
  case MessageDlg('The current graph''s data has been altered.'+
            #13#10'Do you wish to save the alterations ?', mtConfirmation,
                    [mbYes, mbNo, mbCancel], 0) of
    mrYes: SaveClick(Sender);
 mrCancel:Exit;
    mrNo:
    begin
      Altered := False;
      GridColorsAltered := False;
      DerivativeAltered := False;
      NewFile := False;
    end
  end;

  with OpenDialog do
  begin
    InitialDir := DataPath;
    if Execute then
    begin
      s := DataPath + GraphFName; { save original FName }
      DataPath := ExtractFilePath(FileName);
      DataPath := IncludeTrailingPathDelimiter(DataPath);
      ViewForm.MousePoint.X := Maxint;
      GraphFName := ExtractFileName(FileName);
      if s <> GraphFName then AddRecent(s);
      with CheckListBox do
      begin
        for i := 0 to Items.Count -1 do Items.Objects[i].Free;
        Clear;
      end;
      if ReadData(DataPath + GraphFName) then ShowData(Sender);
      ApplyBtnClick(Sender);
      Altered := False;
      GridColorsAltered := False;
      DerivativeAltered := False;
      if GridColorsForm.Visible then GridColorsForm.ShowGridColorData;
      if PlotColorsForm.Visible then PlotColorsForm.ShowPlotColorData;
    end;
  end;
end;

procedure TFunctionsForm.EditMaxXKeyUp(Sender: TObject; var Key: Word;
                                        Shift: TShiftState);
var
  x: TGLFloat;

begin
  try
    x := StrToFloat(EditMaxX.Text);
  except
    x := 1.0;
  end;
  PlotData.xMax := x;
  ApplyBtn.Visible := True;
  UpdateTPlotDataObject;
  CountzPoints;
end;

procedure TFunctionsForm.EditMaxYKeyUp(Sender: TObject; var Key: Word;
                                        Shift: TShiftState);
var
  y: TGLFloat;

begin
  try
    y := StrToFloat(EditMaxY.Text);
  except
    y := 1.0;
  end;
  PlotData.yMax := y;
  ApplyBtn.Visible := True;
  UpdateTPlotDataObject;
  CountzPoints;
end;

procedure TFunctionsForm.EditMaxZKeyUp(Sender: TObject; var Key: Word;
                                        Shift: TShiftState);
var
  z: TGLFloat;

begin
  try
    z := StrToFloat(EditMaxZ.Text);
  except
    z := 1.0;
  end;
  PlotData.zMax := z;
  ApplyBtn.Visible := True;
  UpdateTPlotDataObject;
  CountzPoints;
end;

procedure TFunctionsForm.EditMinXKeyUp(Sender: TObject; var Key: Word;
                                        Shift: TShiftState);
var
  x: TGLFloat;

begin
  try
    x := StrToFloat(EditMinX.Text);
  except
    x := -1.0;
  end;
  PlotData.xMin := x;
  ApplyBtn.Visible := True;
  UpdateTPlotDataObject;
  CountzPoints;
end;

procedure TFunctionsForm.EditMinYKeyUp(Sender: TObject; var Key: Word;
                                        Shift: TShiftState);
var
  y: TGLFloat;

begin
  try
    y := StrToFloat(EditMinY.Text);
  except
    y := -1.0;
  end;
  PlotData.yMin := y;
  ApplyBtn.Visible := True;
  UpdateTPlotDataObject;
  CountzPoints;
end;

procedure TFunctionsForm.EditMinZKeyUp(Sender: TObject; var Key: Word;
                                        Shift: TShiftState);
var
  z: TGLFloat;

begin
  try
    z := StrToFloat(EditMinZ.Text);
  except
    z := -1.0;
  end;
  PlotData.zMin := z;
  ApplyBtn.Visible := True;
  UpdateTPlotDataObject;
  CountzPoints;
end;

procedure TFunctionsForm.EditNoteClick(Sender: TObject);
begin
  ViewForm.MousePoint.X := Maxint;
end;

procedure TFunctionsForm.EditNoteKeyUp(Sender: TObject; var Key: Word;
                                        Shift: TShiftState);
begin
  if Key <> 13 then
  begin
    PlotData.NoteStr := EditNote.Text;
    with CheckListBox do if (Count > 0) and (ItemIndex > -1)
    then TPlotDataObject(Items.Objects[ItemIndex]).Data := PlotData;
    Altered := True;
  end;
end;

procedure TFunctionsForm.Exit1Click(Sender: TObject);
begin
  ViewForm.Exit1Click(Sender);
end;

procedure TFunctionsForm.FloatKeyPress(Sender: TObject; var Key: Char);
begin
  with Sender as TEdit do
  if not CharInSet(Key, AnyFloat) then Key := #0;
end;

procedure TFunctionsForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: integer;

begin
  for i := 0 to CheckListBox.Items.Count -1 do
                CheckListBox.Items.Objects[i].Free;
  CheckListBox.Clear;
end;

procedure TFunctionsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  ViewForm.Close;
  CanClose := not ViewForm.Visible;
end;

procedure TFunctionsForm.FormCreate(Sender: TObject);
begin

end;

procedure TFunctionsForm.FormDestroy(Sender: TObject);
var
  i: integer;

begin
  with CheckListBox do for i := 0 to Count -1 do Items.Objects[i].Free;
end;

function TFunctionsForm.DefaultPlotData: TPlotData;
begin
  with Result do
  begin
    fxyStr := '';                { function string }
    txtStr := '';                { txt -> fxy when scanned }
    NoteStr := '';               { describe function }
    xMin := -10.0;               { x limits and increment }
    xMax :=  10.0;
    xInc :=   1.0;
    yMin := -10.0;               { y limits and increment }
    yMax :=  10.0;
    yInc :=   1.0;
    zMin := -10.0;               { z limits }
    zMax :=  10.0;
    zLim := False;               { false = z limits do not apply }
    zCap := False;               { above zLim values := NAN }

    UpperColor := VectorMake(255, 0, 0, 1000);
    LowerColor := VectorMake(0, 0, 255, 1000);

    ColorBlend := 0.5;
    ColorMove := 0.0;
    ViewMode := vmAmbient;
    fxyMode := fxyFill;
  end;
end;

function TFunctionsForm.DefaultViewData: TViewData;
begin
  with Result do
  begin
    CameraCubeAt := VectorMake(0, 0, 0);
    CameraAt := VectorMake(50, 50, 30);
    fLength := 200;
    LightAt := VectorMake(50, 50, 50);
    ViewDepth := 1000;
    BackColor := clActiveCaption;
    with xyGrid do
    begin
      Color := clrBlue;
      with xRange do
      begin
        Maximum :=  10;
        Minimum := -10;
        Step := 1;
      end;
      with yRange do
      begin
        Maximum :=  10;
        Minimum := -10;
        Step := 1;
      end;
      zPosition := -10;
      zScale := 1;
      IsVisible := True;
      IsChecked := True;
    end;

    with xzGrid do
    begin
      Color := clrGreen;
      with xRange do
      begin
        Maximum :=  10;
        Minimum := -10;
        Step := 1;
      end;
      with zRange do
      begin
        Maximum :=  10;
        Minimum := -10;
        Step := 1;
      end;
      yPosition := -10;
      IsVisible := True;
      IsChecked := True;
    end;

    with yzGrid do
    begin
      Color := clrRed;
      with yRange do
      begin
        Maximum :=  10;
        Minimum := -10;
        Step := 1;
      end;
      with zRange do
      begin
        Maximum :=  10;
        Minimum := -10;
        Step := 1;
      end;
      xPosition := -10;
      IsVisible := True;
      IsChecked := True;
    end;

    xEvaluate := 0.0;
    yEvaluate := 0.0;
    CoordChecked := False;
    ToGridsChecked := False;
    dzdx_dyChecked := False;
    BoxChecked := False;
    CoordWidth := 1;
    CoordColor := clBlack;
    BoxLnWidth := 1;
    BoxLnColor := clrBlack;

    TextVisible := False;
    TextFontN := 'Tahoma';
    TextFontSz := 10;
    xPosYMax := False;
    xPosZMax := False;
    xTextColor := clBlack;

    yPosXMax := False;
    yPosZMax := False;
    yTextColor := clBlack;

    TextVisible := True;
    TextFontN := 'Tahoma';    { ViewForm.GLWinBitmapFont }
    TextFontSz := 20;

    xPosYMax := False;
    xPosZMax := False;
    xTextColor := clRed;

    yPosXMax := False;
    yPosZMax := False;
    yTextColor := clGreen;

    zPosXMax := False;
    zPosYMax := False;
    zTextColor := clBlue;
    ArrowSize := 1.0;
  end;
end;

function TFunctionsForm.DefaultAddedData: TAddedData;
begin
  with Result do
  begin
    xMin := -10.0;               { x limits and increment }
    xMax :=  10.0;
    xInc :=   1.0;
    yMin := -10.0;               { y limits and increment }
    yMax :=  10.0;
    yInc :=   1.0;
    zMin := -10.0;               { z limits }
    zMax :=  10.0;
    zLim := False;               { false = z limits do not apply }
    zCap := False;               { above zLim values := NAN }

    UpperColor := VectorMake(255, 0, 0, 1000);
    LowerColor := VectorMake(0, 0, 255, 1000);

    ColorBlend := 0.5;
    ColorMove := 0.0;
    ViewMode := vmAmbient;
    fxyMode := fxyFill;

    AddLineWidth := 1;
    AddLineColor := clBlack;
    AddedAs := AddNone;
  end;
end;

procedure TFunctionsForm.CountzPoints;
var
  n: integer;

begin
  with PlotData do
  n := round((xMax - xMin + xInc)/xInc)*round((yMax - yMin + yInc)/yInc);
  TooManyPoints := round(Log10(n));
  case TooManyPoints of
  0..2:ZCountLabel.Font.Color := clLime;
     3:ZCountLabel.Font.Color := clTeal;
     4:ZCountLabel.Font.Color := clGreen;
     5:ZCountLabel.Font.Color := clBlue;
  else ZCountLabel.Font.Color := clRed;
  end;
  if TooManyPoints > 6
  then ZCountLabel.Caption := 'Too many Points: ' + FloatToStrF(n, ffnumber, 8, 0)+' !'
  else ZCountLabel.Caption := 'N° Points = ' + FloatToStrF(n, ffnumber, 8, 0);
end;

procedure TFunctionsForm.UpdateTPlotDataObject;
begin
  with CheckListBox do
  TPlotDataObject(Items.Objects[ItemIndex]).Data := PlotData;
end;

procedure TFunctionsForm.DeleteButtonClick(Sender: TObject);
var
  i: integer;

begin
  if CheckListBox.Count > 1 then
  begin
    with CheckListBox do
    begin
      i := ItemIndex;
      with Items.Objects[i] as TPlotDataObject do Free;
      Items.Delete(i);
      if i > Count -1 then i := Count -1;
      ItemIndex := i;
      PlotData := TPlotDataObject(Items.Objects[ItemIndex]).Data;
      DeleteButton.Enabled := Count > 1;
    end;
    ApplyBtnClick(Sender);
  end
end;

procedure TFunctionsForm.DownButtonClick(Sender: TObject);
var
  i: integer;

begin
  with CheckListBox do
  begin
    i := ItemIndex;
    if i < Count -1 then Items.Move(i, i+1);
    ItemIndex := i+1;
  end;
  CheckListBoxClick(Sender);
  Altered := True;
end;

function TFunctionsForm.ReadData(const FName: TFileName): Boolean;
  function StrToVector(s: string): TVector;
  var
    t: string;
    i: integer;

  begin
    i := Pos(',', s);
    t := Copy(s, 1, i -1);
    Result.X := StrToFloat(t);

    s := Copy(s, i +1, Length(s));
    i := Pos(',', s);
    t := Copy(s, 1, i -1);
    Result.Y := StrToFloat(t);

    s := Copy(s, i +1, Length(s));
    Result.Z := StrToFloat(s);
  end;

  function StrToColor(s: string): TColorVector;
  var
    t: string;
    i: integer;

  begin
    i := Pos(',', s);
    t := Copy(s, 1, i -1);
    Result.X := StrToFloat(t);

    s := Copy(s, i +1, Length(s));
    i := Pos(',', s);
    t := Copy(s, 1, i -1);
    Result.Y := StrToFloat(t);

    s := Copy(s, i +1, Length(s));
    i := Pos(',', s);
    t := Copy(s, 1, i -1);
    Result.Z := StrToFloat(t);

    s := Copy(s, i +1, Length(s));
    Result.W := StrToFloat(s);
  end;

  function StrToRange(s: string): TRange;
  var
    t: string;
    i: integer;

  begin
    i := Pos(',', s);
    t := Copy(s, 1, i -1);
    Result.Maximum := StrToFloat(t);

    s := Copy(s, i +1, Length(s));
    i := Pos(',', s);
    t := Copy(s, 1, i -1);
    Result.Minimum := StrToFloat(t);

    s := Copy(s, i +1, Length(s));
    Result.Step := StrToFloat(s);
  end;

var
  s, t: string;
  f: TextFile;
  i, j, k, n: integer;

begin
  Result := False;
  if FileExists(FName) then
  begin
    AssignFile(f, FName);
    try
      Reset(f);
      Readln(f, s);
      j := 0;
      i := Pos(#9, s);

      while i > 0 do
      begin
        t := Copy(s, 1, i -1);
        s := Copy(s, i +1, Length(s));
        i := Pos(#9, s);
        Inc(j);

        with ViewData do
        case j of
        1:CameraCubeAt := StrToVector(t);
        2:CameraAt := StrToVector(t);
        3:fLength := StrToFloat(t);
        4:LightAt := StrToVector(t);
        5:ViewDepth := StrToFloat(t);
        6:BackColor := StrToInt(t);

        7:xyGrid.Color := StrToColor(t);
        8:xyGrid.xRange := StrToRange(t);
        9:xyGrid.yRange := StrToRange(t);
       10:xyGrid.zPosition := StrToFloat(t);
       11:xyGrid.zScale := StrToFloat(t);
       12:xyGrid.IsVisible := StrToBool(t);
       13:xyGrid.IsChecked := StrToBool(t);

       14:xzGrid.Color := StrToColor(t);
       15:xzGrid.xRange := StrToRange(t);
       16:xzGrid.zRange := StrToRange(t);
       17:xzGrid.yPosition := StrToFloat(t);
       18:xzGrid.IsVisible := StrToBool(t);
       19:xzGrid.IsChecked := StrToBool(t);

       20:yzGrid.Color := StrToColor(t);
       21:yzGrid.yRange := StrToRange(t);
       22:yzGrid.zRange := StrToRange(t);
       23:yzGrid.xPosition := StrToFloat(t);
       24:yzGrid.IsVisible := StrToBool(t);
       25:yzGrid.IsChecked := StrToBool(t);
       26:ViewForm.TargetCube.Position.AsVector := StrToVector(t);

       27:xEvaluate := StrToFloat(t);
       28:yEvaluate := StrToFloat(t);
       29:CoordChecked := StrToBool(t);
       30:ToGridsChecked := StrToBool(t);
       31:dzdx_dyChecked := StrToBool(t);
       32:BoxChecked := StrToBool(t);
       33:CoordWidth := StrToInt(t);
       34:CoordColor := StrToInt(t);
       35:BoxLnWidth := StrToInt(t);
       36:BoxLnColor := StrToColor(t);
       37:TextVisible := StrToBool(t);
       38:TextFontN := t;
       39:TextFontSz := StrToInt(t);
       40:xPosYMax := StrToBool(t);
       41:xPosZMax := StrToBool(t);
       42:xTextColor := StrToInt(t);
       43:yPosXMax := StrToBool(t);
       44:yPosZMax := StrToBool(t);
       45:yTextColor := StrToInt(t);
       46:zPosXMax := StrToBool(t);
       47:zPosYMax := StrToBool(t);
       48:zTextColor := StrToInt(t);
       49:ArrowSize := StrTofloat(t);
        end;
      end;
      n := StrToInt(s);  { number of functions }
      for k := 0 to n -1 do
      begin{ read each function's data }
        Readln(f, s);
        j := 0;
        i := Pos(#9, s);

        while i > 0 do
        begin
          t := Copy(s, 1, i -1);
          s := Copy(s, i +1, Length(s));
          i := Pos(#9, s);
          Inc(j);

          with PlotData do
          case j of
          1:fxyStr := t;
          2:txtStr := t;
          3:NoteStr := t;
          4:xMin := StrToFloat(t);
          5:xMax := StrToFloat(t);
          6:xInc := StrToFloat(t);
          7:yMin := StrToFloat(t);
          8:yMax := StrToFloat(t);
          9:yInc := StrToFloat(t);
         10:zMin := StrToFloat(t);
         11:zMax := StrToFloat(t);
         12:zLim := StrToBool(t);
         13:zcap := StrToBool(t);
         14:UpperColor := StrToColor(t);
         15:LowerColor := StrToColor(t);
         16:ColorBlend := StrToFloat(t);
         17:ColorMove := StrToFloat(t);
         18:ViewMode := TViewMode(StrToInt(t));
         19:fxyMode := TfxyMode(StrToInt(t));
          end;
        end;
        with CheckListBox do
        begin
          AddItem(PlotData.txtStr, TPlotDataObject.Create(PlotData));
          Checked[Count -1] := StrToBool(s);
        end;
      end;

      Readln(f, s);
      j := 0;
      i := Pos(#9, s);

      while i > 0 do
      begin
        t := Copy(s, 1, i -1);
        s := Copy(s, i +1, Length(s));
        i := Pos(#9, s);
        Inc(j);
        with AddedData do
        case j of
        1:xMin := StrToFloat(t);
        2:xMax := StrToFloat(t);
        3:xInc := StrToFloat(t);
        4:yMin := StrToFloat(t);
        5:yMax := StrToFloat(t);
        6:yInc := StrToFloat(t);
        7:zMin := StrToFloat(t);
        8:zMax := StrToFloat(t);
        9:zLim := StrToBool(t);
       10:zcap := StrToBool(t);
       11:UpperColor := StrToColor(t);
       12:LowerColor := StrToColor(t);
       13:ColorBlend := StrToFloat(t);
       14:ColorMove := StrToFloat(t);
       15:ViewMode := TViewMode(StrToInt(t));
       16:fxyMode := TfxyMode(StrToInt(t));
       17:AddLineWidth := StrToInt(t);
       18:AddLineColor := StrToInt(t);
       19:AddedAs := TAddedType(StrToInt(t));
        end;
      end;

    except
      MessageDlg('File Error! An Error has occurred'+
           #13#10'when attempting to read "'+FName+'".',
      mtError, [mbOK], 0);
      CloseFile(f);
      Exit;
    end;
    CloseFile(f);
    Result := True;
    CheckListBox.ItemIndex := CheckListBox.Count -1;
    NewFile := not Result;
  end;
end;

procedure TFunctionsForm.SaveClick(Sender: TObject);
begin
  if NewFile then SaveAsClick(Sender) else WriteData(DataPath + GraphFName);
end;

procedure TFunctionsForm.SaveAsClick(Sender: TObject);
begin
  with SaveDialog do
  begin
    InitialDir := DataPath;
    FileName := GraphFName;
    if Execute then
    begin
      GraphFName := ExtractFileName(FileName);
      DataPath := ExtractFilePath(FileName);
      DataPath := IncludeTrailingPathDelimiter(DataPath);
      WriteData(DataPath + GraphFName);
    end;
  end;
  Caption := GraphFName;
  ViewForm.Caption := GraphFName;
  GridOptionsForm.Caption := GraphFName;
  EvaluateForm.Caption := GraphFName;
  CoordsForm.Caption := GraphFName;
end;

procedure TFunctionsForm.WriteData(const FName: TFileName);
  function VectorToStr(v: TVector): string;
  begin
    Result := FloatToStr(v.X)+','+FloatToStr(v.Y)+','+FloatToStr(v.Z);
  end;

  function ColorToStr(c: TColorVector): string;
  begin
    Result :=
    FloatToStr(c.X)+','+FloatToStr(c.Y)+','+FloatToStr(c.Z)+','+FloatToStr(c.W);
  end;

  function RangetoStr(r: TRange): string;
  begin
    Result :=
    FloatToStr(r.Maximum)+','+FloatToStr(r.Minimum)+','+FloatTostr(r.Step);
  end;

var
  f: TextFile;
  s: string;
  i: integer;

begin
  try
    AssignFile(f, FName);
    try
      Rewrite(f);   { write tab delimited data }
      with ViewData do
      begin
        CameraCubeAt := ViewForm.CameraCube.Position.AsVector;
        CameraAt := ViewForm.Camera.Position.AsVector;
        fLength := ViewForm.Camera.FocalLength;
        LightAt := ViewForm.GLLight.Position.AsVector;
        ViewDepth := ViewForm.Camera.DepthOfView;
        BackColor := ViewForm.GLSViewer.Buffer.BackgroundColor;

        s := VectorToStr(CameraCubeAt)+#9+VectorToStr(CameraAt)+#9+
        FloatToStr(fLength)+#9+VectorToStr(LightAt)+#9+
        FloatToStr(ViewDepth)+#9+IntToStr(BackColor)+#9+
                            {xy}
        ColorToStr(xyGrid.Color)+#9+
        RangeToStr(xyGrid.xRange)+#9+RangeToStr(xyGrid.yRange)+#9+
        FloatToStr(xyGrid.zPosition)+#9+FloatToStr(xyGrid.zScale)+#9+
        BoolToStr(xyGrid.IsVisible)+#9+BoolToStr(xyGrid.IsChecked)+#9+
                            {xz}
        ColorToStr(xzGrid.Color)+#9+
        RangeToStr(xzGrid.xRange)+#9+RangeToStr(xzGrid.zRange)+#9+
        FloatToStr(xzGrid.yPosition)+#9+BoolToStr(xzGrid.IsVisible)+#9+
        BoolToStr(xzGrid.IsChecked)+#9+
                            {yz}
        ColorToStr(yzGrid.Color)+#9+
        RangeToStr(yzGrid.yRange)+#9+RangeToStr(yzGrid.zRange)+#9+
        FloatToStr(yzGrid.xPosition)+#9+BoolToStr(yzGrid.IsVisible)+#9+
        BoolToStr(yzGrid.IsChecked)+#9+
                            { Target}
        VectorToStr(ViewForm.TargetCube.Position.AsVector)+#9+

        FloatToStr(xEvaluate)+#9+FloatToStr(yEvaluate)+#9+
        BoolToStr(CoordChecked)+#9+BoolToStr(ToGridsChecked)+#9+
        BoolToStr(dzdx_dyChecked)+#9+BoolToStr(BoxChecked)+#9+
        IntToStr(CoordWidth)+#9+IntToStr(CoordColor)+#9+
        IntToStr(BoxLnWidth)+#9+ColorToStr(BoxLnColor)+#9+
        BoolToStr(TextVisible)+#9+TextFontN+#9+IntToStr(TextFontSz)+#9+
        BoolToStr(xPosYMax)+#9+BoolToStr(xPosZMax)+#9+IntToStr(xTextColor)+#9+
        BoolToStr(yPosXMax)+#9+BoolToStr(yPosZMax)+#9+IntToStr(yTextColor)+#9+
        BoolToStr(zPosXMax)+#9+BoolToStr(zPosYMax)+#9+IntToStr(zTextColor)+#9+
        FloatToStr(ArrowSize)+#9+
        IntToStr(CheckListBox.Count);  { number of functions }
      end;
      writeln(f, s);

      for i := 0 to CheckListBox.Count -1 do { each function }
      begin
        PlotData := TPlotDataObject(CheckListBox.Items.Objects[i]).Data;
        with PlotData do
        begin
          s := fxyStr+#9+txtStr+#9+NoteStr+#9+
          FloatToStr(xMin)+#9+FloatToStr(xMax)+#9+FloatToStr(xInc)+#9+
          FloatToStr(yMin)+#9+FloatToStr(yMax)+#9+FloatToStr(yInc)+#9+
          FloatToStr(zMin)+#9+FloatToStr(zMax)+#9+BoolToStr(zLim)+#9+
          BoolToStr(zCap)+#9+ColorToStr(UpperColor)+#9+ColorToStr(LowerColor)+#9+
          FloatToStr(ColorBlend)+#9+FloatToStr(ColorMove)+#9+IntToStr(Ord(ViewMode))+#9+
          IntToStr(Ord(fxyMode))+#9+BoolToStr(CheckListBox.Checked[i]);
        end;
        writeln(f, s);
      end;

      with AddedData do
      begin
        s := FloatToStr(xMin)+#9+FloatToStr(xMax)+#9+FloatToStr(xInc)+#9+
        FloatToStr(yMin)+#9+FloatToStr(yMax)+#9+FloatToStr(yInc)+#9+
        FloatToStr(zMin)+#9+FloatToStr(zMax)+#9+BoolToStr(zLim)+#9+
        BoolToStr(zCap)+#9+ColorToStr(UpperColor)+#9+ColorToStr(LowerColor)+#9+
        FloatToStr(ColorBlend)+#9+FloatToStr(ColorMove)+#9+IntToStr(Ord(ViewMode))+#9+
        IntToStr(Ord(fxyMode))+#9+IntToStr(AddLineWidth)+#9+IntToStr(AddLineColor)+#9+
        IntToStr(Ord(AddedAs));
      end;
      writeln(f, s);
    finally
      Flush(f);
      CloseFile(f);
    end;
  except
    MessageDlg('File Error! An Error has occurred'+
         #13#10'when attempting to write to "'+GraphFName+'".',
    mtError, [mbOK], 0);
  end;
  NewFile := False;
  Altered := False;
  Layout.CurrentGraphFName := GraphFName;
end;

procedure TFunctionsForm.zCapCBClick(Sender: TObject);
begin
  PlotData.zCap := zCapCB.Checked;
  UpdateTPlotDataObject;
  Applybtn.Visible := True;
end;

procedure TFunctionsForm.zLimitCBClick(Sender: TObject);
begin
  PlotData.zLim := zLimitCB.Checked;
  UpdateTPlotDataObject;
  Applybtn.Visible := True;
end;

procedure TFunctionsForm.ShowData(Sender: TObject);
begin
  with ViewData do
  begin
    if not(Sender is TCheckListBox) then
    begin
      ViewForm.CameraCube.Position.AsVector := CameraCubeAt;
      ViewForm.Camera.Position.AsVector := CameraAt;
      ViewForm.Camera.FocalLength := fLength;
      ViewForm.GLLight.Position.AsVector := LightAt;
      ViewForm.Camera.DepthOfView := ViewDepth;
      ViewForm.GLSViewer.Buffer.BackgroundColor := BackColor;
    end;

    GridOptionsForm.EditViewDepth.Text := FloatToStrF(ViewDepth, ffGeneral, 7, 4);

                            {xy}
    GridOptionsForm.EditxyGridMinx.Text := FloatToStrF(xyGrid.xRange.Minimum, ffGeneral, 7, 4);
    ViewForm.GLxyGrid.XSamplingScale.Min := xyGrid.xRange.Minimum;
    GridOptionsForm.EditxyGridMiny.Text := FloatToStrF(xyGrid.yRange.Minimum, ffGeneral, 7, 4);
    ViewForm.GLxyGrid.YSamplingScale.Min := xyGrid.yRange.Minimum;

    GridOptionsForm.EditxyGridMaxx.Text := FloatToStrF(xyGrid.xRange.Maximum, ffGeneral, 7, 4);
    ViewForm.GLxyGrid.XSamplingScale.Max := xyGrid.xRange.Maximum;
    GridOptionsForm.EditxyGridMaxy.Text := FloatToStrF(xyGrid.yRange.Maximum, ffGeneral, 7, 4);
    ViewForm.GLxyGrid.YSamplingScale.Max := xyGrid.yRange.Maximum;

    GridOptionsForm.EditxyGridStpx.Text := FloatToStrF(xyGrid.xRange.Step, ffGeneral, 7, 4);
    ViewForm.GLxyGrid.XSamplingScale.Step := xyGrid.xRange.Step;
    GridOptionsForm.EditxyGridStpy.Text := FloatToStrF(xyGrid.yRange.Step, ffGeneral, 7, 4);
    ViewForm.GLxyGrid.YSamplingScale.Step := xyGrid.yRange.Step;

    GridOptionsForm.EditxyGridPosz.Text := FloatToStrF(xyGrid.zPosition, ffGeneral, 7, 4);
    ViewForm.GLxyGrid.Position.Z := xyGrid.zPosition*xyGrid.zScale;

    GridOptionsForm.EditzScale.Text := FloatToStrF(xyGrid.zScale, ffGeneral, 7, 4);
    ViewForm.GLxzGrid.Scale.Z := xyGrid.zScale;
    ViewForm.GLyzGrid.Scale.Z := xyGrid.zScale;

    ViewForm.GLxyGrid.LineColor.SetColor(xyGrid.Color.X, xyGrid.Color.Y,
                                         xyGrid.Color.Z, xyGrid.Color.W);

                            {xz}
    GridOptionsForm.EditxzGridMinx.Text := FloatToStrF(xzGrid.xRange.Minimum, ffGeneral, 7, 4);
    ViewForm.GLxzGrid.XSamplingScale.Min := xzGrid.xRange.Minimum;
    GridOptionsForm.EditxzGridMinz.Text := FloatToStrF(xzGrid.zRange.Minimum, ffGeneral, 7, 4);
    ViewForm.GLxzGrid.ZSamplingScale.Min := xzGrid.zRange.Minimum;

    GridOptionsForm.EditxzGridMaxx.Text := FloatToStrF(xzGrid.xRange.Maximum, ffGeneral, 7, 4);
    ViewForm.GLxzGrid.XSamplingScale.Max := xzGrid.xRange.Maximum;
    GridOptionsForm.EditxzGridMaxz.Text := FloatToStrF(xzGrid.zRange.Maximum, ffGeneral, 7, 4);
    ViewForm.GLxzGrid.ZSamplingScale.Max := xzGrid.zRange.Maximum;

    GridOptionsForm.EditxzGridStpx.Text := FloatToStrF(xzGrid.xRange.Step, ffGeneral, 7, 4);
    ViewForm.GLxzGrid.XSamplingScale.Step := xzGrid.xRange.Step;
    GridOptionsForm.EditxzGridStpz.Text := FloatToStrF(xzGrid.zRange.Step, ffGeneral, 7, 4);
    ViewForm.GLxzGrid.ZSamplingScale.Step := xzGrid.zRange.Step;

    GridOptionsForm.EditxzGridPosy.Text := FloatToStrF(xzGrid.yPosition, ffGeneral, 7, 4);
    ViewForm.GLxzGrid.Position.Y := xzGrid.yPosition;

    ViewForm.GLxzGrid.LineColor.SetColor(xzGrid.Color.X, xzGrid.Color.Y,
                                         xzGrid.Color.Z, xzGrid.Color.W);

                            {yz}
    GridOptionsForm.EdityzGridMiny.Text := FloatToStrF(yzGrid.yRange.Minimum, ffGeneral, 7, 4);
    ViewForm.GLyzGrid.YSamplingScale.Min := yzGrid.yRange.Minimum;
    GridOptionsForm.EdityzGridMinz.Text := FloatToStrF(yzGrid.zRange.Minimum, ffGeneral, 7, 4);
    ViewForm.GLyzGrid.ZSamplingScale.Min := yzGrid.zRange.Minimum;

    GridOptionsForm.EdityzGridMaxy.Text := FloatToStrF(yzGrid.yRange.Maximum, ffGeneral, 7, 4);
    ViewForm.GLyzGrid.YSamplingScale.Max := yzGrid.yRange.Maximum;
    GridOptionsForm.EdityzGridMaxz.Text := FloatToStrF(yzGrid.zRange.Maximum, ffGeneral, 7, 4);
    ViewForm.GLyzGrid.ZSamplingScale.Max := yzGrid.zRange.Maximum;

    GridOptionsForm.EdityzGridStpy.Text := FloatToStrF(yzGrid.yRange.Step, ffGeneral, 7, 4);
    ViewForm.GLyzGrid.YSamplingScale.Step := yzGrid.yRange.Step;
    GridOptionsForm.EdityzGridStpz.Text := FloatToStrF(yzGrid.zRange.Step, ffGeneral, 7, 4);
    ViewForm.GLyzGrid.ZSamplingScale.Step := yzGrid.zRange.Step;

    GridOptionsForm.EdityzGridPosx.Text := FloatToStrF(yzGrid.xPosition, ffGeneral, 7, 4);
    ViewForm.GLyzGrid.Position.X := yzGrid.xPosition;

    ViewForm.GLyzGrid.LineColor.SetColor(yzGrid.Color.X, yzGrid.Color.Y,
                                         yzGrid.Color.Z, yzGrid.Color.W);

    ViewForm.BoxLine1.LineColor.SetColor(BoxLnColor.X, BoxLnColor.Y,
                                         BoxLnColor.Z, BoxLnColor.W);
    ViewForm.BoxLine2.LineColor := ViewForm.BoxLine1.LineColor;
    ViewForm.BoxLine3.LineColor := ViewForm.BoxLine1.LineColor;
    ViewForm.BoxLine4.LineColor := ViewForm.BoxLine1.LineColor;

    GridOptionsForm.xyGridCB.Checked := xyGrid.IsVisible;
    GridOptionsForm.xyLock.Checked := xyGrid.IsChecked;
    ViewForm.GLxyGrid.Visible := xyGrid.IsVisible;

    GridOptionsForm.xzGridCB.Checked := xzGrid.IsVisible;
    GridOptionsForm.zLock.Checked := xzGrid.IsChecked;
    ViewForm.GLxzGrid.Visible := xzGrid.IsVisible;

    GridOptionsForm.yzGridCB.Checked := yzGrid.IsVisible;
    GridOptionsForm.MinLock.Checked := yzGrid.IsChecked;
    ViewForm.GLyzGrid.Visible := yzGrid.IsVisible;

    GridOptionsForm.EditzScale.Text := FloatToStrF(xyGrid.zScale, ffGeneral, 7, 4);
    ViewForm.GLxzGrid.Scale.Z := xyGrid.zScale;
    ViewForm.GLyzGrid.Scale.Z := xyGrid.zScale;

    GridOptionsForm.BoxOutlineCB.Checked := BoxChecked;
    GridOptionsForm.EditBoxLnWidth.Text := IntToStr(BoxLnWidth);
    GridOptionsForm.DrawOutline(BoxChecked);

    EvaluateForm.EditX.Text := FloatToStrF(xEvaluate, ffGeneral, 7, 4);
    EvaluateForm.EditY.Text := FloatToStrF(yEvaluate, ffGeneral, 7, 4);
    EvaluateForm.EditZ.Text := '';
    EvaluateForm.EditCoordWidth.Text := IntToStr(CoordWidth);
    EvaluateForm.Coordinates.Checked := CoordChecked;
    EvaluateForm.ToGrids.Checked := ToGridsChecked;
    EValuateForm.dzdx_dzdy.Checked := dzdx_dyChecked;
    EvaluateForm.ColorDialog.Color := CoordColor;
    EvaluateForm.UpdateCoords;
    EvaluateForm.UpdateEvaluate;

    CoordsForm.ShowCoordsCB.Checked := TextVisible;
    CoordsForm.FontButton.Caption := 'Font:'+' '+TextFontN+' '+IntToStr(TextFontSz);
    ViewForm.GLWinBmpFont.Font.Name := TextFontN;
    ViewForm.GLWinBmpFont.Font.Size := TextFontSz;
    CoordsForm.xMaxYCB.Checked := xPosYMax;
    CoordsForm.xMaxZCB.Checked := xPosZMax;

    CoordsForm.yMaxXCB.Checked := yPosXMax;
    CoordsForm.yMaxZCB.Checked := yPosZMax;

    CoordsForm.zMaxYCB.Checked := zPosYMax;
    CoordsForm.zMaxXCB.Checked := zPosXMax;

    CoordsForm.UpdateCoordText;
  end;

  with PlotData do
  begin
    Editfxy.Text := txtStr;
    EditNote.Text := NoteStr;

    EditMinX.Text := FloatToStrF(xMin, ffGeneral, 7, 4);
    EditMaxX.Text := FloatToStrF(xMax, ffGeneral, 7, 4);
    EditdX.Text := FloatToStrF(xInc, ffGeneral, 7, 4);

    EditMinY.Text := FloatToStrF(yMin, ffGeneral, 7, 4);
    EditMaxY.Text := FloatToStrF(yMax, ffGeneral, 7, 4);
    EditdY.Text := FloatToStrF(yInc, ffGeneral, 7, 4);

    EditMinZ.Text := FloatToStrF(zMin, ffGeneral, 7, 4);
    EditMaxZ.Text := FloatToStrF(zMax, ffGeneral, 7, 4);
    zLimitCB.Checked := zLim;
    zCapCB.Checked := zCap;

    ModeComboBox.ItemIndex := Ord(ViewMode);
    StyleComboBox.ItemIndex := Ord(fxyMode);
  end;
  Caption := GraphFName;
  ViewForm.Caption := GraphFName;
  GridOptionsForm.Caption := GraphFName;
  EvaluateForm.Caption := GraphFName;
  CoordsForm.Caption := GraphFName;
  CountzPoints;
end;

procedure TFunctionsForm.StyleComboBoxChange(Sender: TObject);
begin
  PlotData.fxyMode := TfxyMode(StyleComboBox.ItemIndex);
  UpdateTPlotDataObject;
  ApplyBtnClick(Sender);
end;

procedure TFunctionsForm.UpButtonClick(Sender: TObject);
var
  i: integer;

begin
  with CheckListBox do
  begin
    i := ItemIndex;
    if i > 0 then Items.Move(i, i-1);
    if i > 1 then ItemIndex := i-1 else ItemIndex := 0;
  end;
  CheckListBoxClick(Sender);
  Altered := True;
end;

procedure TFunctionsForm.AddNewPlot;
begin
  with CheckListBox do
  begin
    AddItem(PlotData.txtStr, TPlotDataObject.Create(PlotData));
    ItemIndex := Count -1;
    Checked[ItemIndex] := True;  { initially this item is checked }
  end;
  Editfxy.SetFocus;
  Editfxy.SelLength := 0;
end;

procedure TFunctionsForm.ApplyBtnClick(Sender: TObject);
var
  v: TGLFloat;
  s: string;

begin
  if TooManyPoints > 6 then Exit;

  with PlotData do
  begin
    if xMin > xMax then  { swap }
    begin
      v := xMin;
      xMin := xMax;
      xMax := v;
      s := EditMinX.Text;
      EditMinX.Text := EditMaxX.Text;
      EditMaxX.Text := s;
      UpdateTPlotDataObject;
    end;
    if yMin > yMax then  { swap }
    begin
      v := yMin;
      yMin := yMax;
      yMax := v;
      s := EditMinY.Text;
      EditMinY.Text := EditMaxY.Text;
      EditMaxY.Text := s;
      UpdateTPlotDataObject;
    end;
  end;

  AddedData.AddedAs := AddNone;

  ViewForm.UpdatePlot;
  EvaluateForm.UpdateEvaluate;
  ApplyBtn.Visible := False;
  Altered := True;
end;

procedure TFunctionsForm.CentreClick(Sender: TObject);
var
  x, y, z: TGLFloat;

begin
  with ViewForm do
  begin
    MousePoint.X := Maxint;
    x := PlotData.xMax + PlotData.xMin;
    y := PlotData.yMax + PlotData.yMin;
    z := PlotData.zMax + PlotData.zMin;
    TargetCube.Position.SetPoint(x/2, y/2, z/2);
  end;
  Altered := True;
  ViewForm.ShowDisplacement;
end;

procedure TFunctionsForm.CheckListBoxClick(Sender: TObject);
begin
  with CheckListBox do
  begin
    PlotData := TPlotDataObject(Items.Objects[ItemIndex]).Data;
    if Checked[ItemIndex] then EvaluateForm.UpdateEvaluate;
  end;
  ShowData(Sender);
  if PlotColorsForm.Visible then PlotColorsForm.ShowPlotColorData;
  if DerivativesForm.Visible then DerivativesForm.ApplyBtn.Visible := True;
end;

procedure TFunctionsForm.CheckListBoxClickCheck(Sender: TObject);
begin
  ApplyBtn.Visible := True;
end;

procedure TFunctionsForm.ReadAndShowInitialData;
begin
  if not ReadData(DataPath + GraphFName) then
  begin
    GraphFName := NewFName;
    NewFile := True;
    ViewData := DefaultViewData;
    PlotData := DefaultPlotData;
    AddedData := DefaultAddedData;
    with CheckListBox do
    begin
      AddItem(PlotData.txtStr, TPlotDataObject.Create(PlotData));
      ItemIndex := Count -1;
      Checked[ItemIndex] := True;  { initially this item is checked }
    end;
    ViewForm.TargetCube.Position.SetPoint(0, 0, 0);
  end;

  Caption := GraphFName;
  ShowData(Self);
end;

procedure TFunctionsForm.OpenRecentFile(FName: TFileName);
var
  i: integer;

begin
  if DerivativesForm.Visible then DerivativesForm.Close;
  if FileExists(FName) then
  begin
    if Altered or GridColorsAltered then
    case MessageDlg('The current graph''s data has been altered.'+
              #13#10'Do you wish to save the alterations ?', mtConfirmation,
                      [mbYes, mbNo, mbCancel], 0) of
      mrYes:SaveClick(Self);
   mrCancel:Exit;
      mrNo:
      begin
        Altered := False;
        GridColorsAltered := False;
        DerivativeAltered := False;
        NewFile := False;
      end;
    end;
    AddRecent(FName);
    DataPath := ExtractFilePath(FName);
    DataPath := IncludeTrailingPathDelimiter(DataPath);
    ViewForm.MousePoint.X := Maxint;
    GraphFName := ExtractFileName(FName);

    with CheckListBox do
    begin
      for i := 0 to Items.Count -1 do Items.Objects[i].Free;
      Clear;
    end;

    if ReadData(FName) then ShowData(Self);
    if GridColorsForm.Visible then GridColorsForm.ShowGridColorData;
    if PlotColorsForm.Visible then PlotColorsForm.ShowPlotColorData;

    ApplyBtnClick(Self);
    Altered := False;
    GridColorsAltered := False;
    DerivativeAltered := False;
  end
  else MessageDlg('The file '+FName+' does not exist!', mtError, [mbOK], 0);
end;

procedure TFunctionsForm.AddRecent(const f: TFileName);
var
  i: integer;
  Found: Boolean;

begin
  with ViewForm.Recent1 do
  begin
    Found := False;
    i := 0;
    while (i < Count) and not Found do
    begin
      Found := Items[i].Caption = DataPath + GraphFName;
      Inc(i);
    end;
    if Found then Delete(i -1);
    Insert(0, TMenuItem.Create(Self));
    Items[0].Caption := DataPath + GraphFName;
    Items[0].OnClick := ViewForm.RecentFilesClick;
    if Count > 16 then Delete(Count -1);
  end;
end;

procedure TFunctionsForm.OpenSelectedFile(Sender: TObject; FName: TFileName);
var
  i: integer;

begin
  if FileExists(FName) then
  begin
    Screen.Cursor := crHourglass;
    with CheckListBox do
    begin
      for i := 0 to Items.Count -1 do Items.Objects[i].Free;
      Clear;
    end;

    GraphFName := ExtractFileName(FName);

    if ReadData(DataPath + GraphFName) then
    begin
      Caption := GraphFName;
      ViewForm.Caption := GraphFName;
      GridOptionsForm.Caption := GraphFName;
      ShowData(Sender);
      NewFile := False;
      Altered := False;
    { NewFont needed to initialize GLWinFont.GetCharWidth
      if the font has been altered, which may or may not be the case,
      so do it anyway }
      ViewForm.GLSViewer.Buffer.BackgroundColor := ViewData.BackColor;
      ViewForm.GLSViewer.Invalidate;
    end;
    Screen.Cursor := crDefault;
  end
  else
  begin
    MessageDlg('The file'+FName+
         #13#10'Could not be found.', mtError, [mbOK], 0);
    Screen.Cursor := crDefault;
  end;
end;

end.
