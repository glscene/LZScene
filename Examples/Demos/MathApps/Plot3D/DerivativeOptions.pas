unit DerivativeOptions;



interface

uses
   SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  ComCtrls, OpenGLTokens;

type
  TDerivativesForm = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    zCountLabel: TLabel;
    GridValues: TSpeedButton;
    PlotValues: TSpeedButton;
    EditMinX: TEdit;
    EditMaxX: TEdit;
    EditdX: TEdit;
    EditMinY: TEdit;
    EditMaxY: TEdit;
    EditdY: TEdit;
    EditMinZ: TEdit;
    EditMaxZ: TEdit;
    zLimitCB: TCheckBox;
    ModeComboBox: TComboBox;
    StyleComboBox: TComboBox;
    zCapCB: TCheckBox;
    ApplyBtn: TBitBtn;
    CloseBtn: TBitBtn;
    DerivXRB: TRadioButton;
    DerivYRB: TRadioButton;
    VolumeRB: TRadioButton;
    Label7: TLabel;
    EditAddLineWidth: TEdit;
    ColorButton: TSpeedButton;
    ColorDialog: TColorDialog;
    VolumeLabel: TLabel;
    PosVolLabel: TLabel;
    NegVolLabel: TLabel;
    TotalLabel: TLabel;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FloatKeyPress(Sender: TObject; var Key: Char);
    procedure IncKeyPress(Sender: TObject; var Key: Char);
    procedure EditMinXKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditMaxXKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditdXKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditMinYKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditMaxYKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditdYKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditMinZKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditMaxZKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure zLimitCBClick(Sender: TObject);
    procedure zCapCBClick(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure GridValuesClick(Sender: TObject);
    procedure PlotValuesClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure StyleComboBoxChange(Sender: TObject);
    procedure ModeComboBoxChange(Sender: TObject);
    procedure DerivXRBClick(Sender: TObject);
    procedure DerivYRBClick(Sender: TObject);
    procedure VolumeRBClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditAddLineWidthKeyPress(Sender: TObject; var Key: Char);
    procedure EditAddLineWidthKeyUp(Sender: TObject; var Key: Word;
                                  Shift: TShiftState);
    procedure ColorButtonClick(Sender: TObject);
    procedure PlotValuesMouseUp(Sender: TObject; Button: TMouseButton;
                                 Shift: TShiftState; X, Y: Integer);
    procedure GridValuesMouseUp(Sender: TObject; Button: TMouseButton;
                                 Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    TooManyPoints: Boolean;
    procedure CountzPoints;
  public
    { Public declarations }
  end;

var
  DerivativesForm: TDerivativesForm;

implementation

{$R *.lfm}
uses
  uGlobal,
  uParser, GLVectorGeometry, Math,
  GridOptions, Main, Evaluate, AddPlotColors;

procedure TDerivativesForm.EditdXKeyUp(Sender: TObject; var Key: Word;
                                        Shift: TShiftState);
var
  x: TGlFloat;

begin
  try
    x := StrToFloat(EditdX.Text);
  except
    x := 1.0;
  end;
  AddedData.xInc := x;
//ApplyBtn.Visible := AddedData.AddedAs > AddNone;
  DerivativeAltered := True;
  CountzPoints;
end;

procedure TDerivativesForm.EditdYKeyUp(Sender: TObject; var Key: Word;
                                        Shift: TShiftState);
var
  y: TGLFloat;

begin
  try
    y := StrToFloat(EditdY.Text);
  except
    y := 1.0;
  end;
  AddedData.yInc := y;
//ApplyBtn.Visible := AddedData.AddedAs > AddNone;
  DerivativeAltered := True;
  CountzPoints;
end;

procedure TDerivativesForm.EditKeyDown(Sender: TObject; var Key: Word;
                                        Shift: TShiftState);
begin
//if (Key = VK_DELETE) or (Key = VK_BACK)
//then ApplyBtn.Visible := AddedData.AddedAs > AddNone;
end;

procedure TDerivativesForm.EditAddLineWidthKeyPress(Sender: TObject;
                                                var Key: Char);
begin
  with Sender as TEdit do
  if not CharInSet(Key, ['0'..'9', #8]) then Key := #0
end;

procedure TDerivativesForm.EditAddLineWidthKeyUp(Sender: TObject; var Key: Word;
                                               Shift: TShiftState);
var
  w: integer;

begin
  try
    w := StrToInt(EditAddLineWidth.Text);
  except
    w := 3;
  end;

  AddedData.AddLineWidth := w;
  ViewForm.AddXLine.LineWidth := w;
  ViewForm.AddYLine.LineWidth := w;
  ViewForm.AddZLine.LineWidth := w;
  DerivativeAltered := True;
//ApplyBtn.Visible := AddedData.AddedAs = AddVolume;
end;

procedure TDerivativesForm.EditMaxXKeyUp(Sender: TObject; var Key: Word;
                                          Shift: TShiftState);
var
  x: TGLFloat;

begin
  try
    x := StrToFloat(EditMaxX.Text);
  except
    x := 1.0;
  end;
  AddedData.xMax := x;
//ApplyBtn.Visible := AddedData.AddedAs > AddNone;
  DerivativeAltered := True;
  CountzPoints;
end;

procedure TDerivativesForm.EditMaxYKeyUp(Sender: TObject; var Key: Word;
                                          Shift: TShiftState);
var
  y: TGLFloat;

begin
  try
    y := StrToFloat(EditMaxY.Text);
  except
    y := 1.0;
  end;
  AddedData.yMax := y;
//ApplyBtn.Visible := AddedData.AddedAs > AddNone;
  DerivativeAltered := True;
  CountzPoints;
end;

procedure TDerivativesForm.EditMaxZKeyUp(Sender: TObject; var Key: Word;
                                          Shift: TShiftState);
var
  z: TGLFloat;

begin
  try
    z := StrToFloat(EditMaxZ.Text);
  except
    z := 1.0;
  end;
  AddedData.zMax := z;
//ApplyBtn.Visible := AddedData.AddedAs > AddNone;
  DerivativeAltered := True;
  CountzPoints;
end;

procedure TDerivativesForm.EditMinXKeyUp(Sender: TObject; var Key: Word;
                                          Shift: TShiftState);
var
  x: TGLFloat;

begin
  try
    x := StrToFloat(EditMinX.Text);
  except
    x := -1.0;
  end;
  AddedData.xMin := x;
//ApplyBtn.Visible := AddedData.AddedAs > AddNone;
  DerivativeAltered := True;
  CountzPoints;
end;

procedure TDerivativesForm.EditMinYKeyUp(Sender: TObject; var Key: Word;
                                          Shift: TShiftState);
var
  y: TGLFloat;

begin
  try
    y := StrToFloat(EditMinY.Text);
  except
    y := -1.0;
  end;
  AddedData.yMin := y;
//ApplyBtn.Visible := AddedData.AddedAs > AddNone;
  DerivativeAltered := True;
  CountzPoints;
end;

procedure TDerivativesForm.EditMinZKeyUp(Sender: TObject; var Key: Word;
                                          Shift: TShiftState);
var
  z: TGLFloat;

begin
  try
    z := StrToFloat(EditMinZ.Text);
  except
    z := -1.0;
  end;
  AddedData.zMin := z;
//ApplyBtn.Visible := AddedData.AddedAs > AddNone;
  DerivativeAltered := True;
  CountzPoints;
end;

procedure TDerivativesForm.FormCloseQuery(Sender: TObject;
                                    var CanClose: Boolean);
begin
  AddedData.AddedAs := AddNone;
  ViewForm.ClearAddedField;
  ViewForm.ClearAddedLines;
  ViewForm.AddXLine.Visible := False;
  ViewForm.AddYLine.Visible := False;
  ViewForm.AddZLine.Visible := False;
  ViewForm.PlotColours1.Enabled := True;
  ViewForm.DerivativePlotColours1.Enabled := False;
  if AddPlotColorsForm.Visible then AddPlotColorsForm.Close;
  Altered := Altered or DerivativeAltered;
end;

procedure TDerivativesForm.FormShow(Sender: TObject);
begin
  Caption := GraphFName;
  with AddedData do
  begin
    EditMinX.Text := FloatToStrF(xMin, ffGeneral, 7, 4);
    EditMaxX.Text := FloatToStrF(xMax, ffGeneral, 7, 4);
    Editdx.Text := FloatToStrF(xInc, ffGeneral, 7, 4);
    EditMinY.Text := FloatToStrF(yMin, ffGeneral, 7, 4);
    EditMaxY.Text := FloatToStrF(yMax, ffGeneral, 7, 4);
    Editdy.Text := FloatToStrF(yInc, ffGeneral, 7, 4);
    EditMinZ.Text := FloatToStrF(zMin, ffGeneral, 7, 4);
    EditMaxZ.Text := FloatToStrF(zMax, ffGeneral, 7, 4);
    zLimitCB.Checked := zLim;
    zCapCB.Checked := zCap;
    ModeComboBox.ItemIndex := Ord(ViewMode);
    StyleComboBox.ItemIndex := Ord(fxyMode);
    if AddLineWidth < 1 then AddLineWidth := 1;
    EditAddLineWidth.Text := IntToStr(AddLineWidth);
    ModeComboBox.ItemIndex := Ord(ViewMode);
    StyleComboBox.ItemIndex := Ord(fxyMode);
  end;
  AddPlotColorsForm.ShowPlotColorData;
//ApplyBtn.Visible := VolumeRB.Checked;
  if DerivXRB.Checked or DerivyRB.Checked then ApplyBtnClick(Sender);
  DerivativeAltered := False;
  CountzPoints;
end;

procedure TDerivativesForm.GridValuesClick(Sender: TObject);
begin
  with ViewData.xyGrid.xRange do
  begin
    EditMinX.Text := FloatToStrF(Minimum, ffGeneral, 7, 4);
    AddedData.xMin := Minimum;
    EditMaxX.Text := FloatToStrF(Maximum, ffGeneral, 7, 4);
    AddedData.xMax := Maximum;
    EditdX.Text := FloatToStrF(Step, ffGeneral, 7, 4);
    AddedData.xInc := Step;
  end;

  with ViewData.xyGrid.yRange do
  begin
    EditMinY.Text := FloatToStrF(Minimum, ffGeneral, 7, 4);
    AddedData.yMin := Minimum;
    EditMaxY.Text := FloatToStrF(Maximum, ffGeneral, 7, 4);
    AddedData.yMax := Maximum;
    EditdY.Text := FloatToStrF(Step, ffGeneral, 7, 4);
    AddedData.yInc := Step;
  end;

  with ViewData.xzGrid.zRange do
  begin
    EditMinZ.Text := FloatToStrF(Minimum, ffGeneral, 7, 4);
    AddedData.zMin := Minimum;
    EditMaxZ.Text := FloatToStrF(Maximum, ffGeneral, 7, 4);
    AddedData.zMax := Maximum;
  end;
  DerivativeAltered := True;
  CountzPoints;
end;

procedure TDerivativesForm.GridValuesMouseUp(Sender: TObject;
          Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//ApplyBtn.Visible := AddedData.AddedAs > AddNone;
end;

procedure TDerivativesForm.IncKeyPress(Sender: TObject; var Key: Char);
begin
  with Sender as TEdit do
  if not CharInSet(Key, PosFloat) then Key := #0;
end;

procedure TDerivativesForm.ModeComboBoxChange(Sender: TObject);
begin
  AddedData.ViewMode := TViewMode(ModeComboBox.ItemIndex);
//ApplyBtn.Visible := AddedData.AddedAs > AddNone;
  ApplyBtnClick(Sender);
end;

procedure TDerivativesForm.FloatKeyPress(Sender: TObject; var Key: Char);
begin
  with Sender as TEdit do
  if not CharInSet(Key, AnyFloat) then Key := #0;
end;

procedure TDerivativesForm.PlotValuesClick(Sender: TObject);
begin
  EditMinx.Text := FloatToStrF(PlotData.xMin, ffGeneral, 7, 4);
  AddedData.xMin := PlotData.xMin;
  EditMaxx.Text := FloatToStrF(PlotData.xMax, ffGeneral, 7, 4);
  AddedData.xMax := PlotData.xMax;
  EditdX.Text := FloatToStrF(PlotData.xInc, ffGeneral, 7, 4);
  AddedData.xInc := PlotData.xInc;
  EditMiny.Text := FloatToStrF(PlotData.yMin, ffGeneral, 7, 4);
  AddedData.yMin := PlotData.yMin;
  EditMaxy.Text := FloatToStrF(PlotData.yMax, ffGeneral, 7, 4);
  AddedData.yMax := PlotData.yMax;
  EditdY.Text := FloatToStrF(PlotData.yInc, ffGeneral, 7, 4);
  AddedData.yInc := PlotData.yInc;
  EditMinz.Text := FloatToStrF(PlotData.zMin, ffGeneral, 7, 4);
  AddedData.zMin := PlotData.zMin;
  EditMaxz.Text := FloatToStrF(PlotData.zMax, ffGeneral, 7, 4);
  AddedData.zMax := PlotData.zMax;
  zLimitCB.Checked := PlotData.zLim;
  AddedData.zLim := PlotData.zLim;
  zCapCB.Checked := PlotData.zCap;
  AddedData.zCap := PlotData.zCap;
  CountzPoints;
end;

procedure TDerivativesForm.PlotValuesMouseUp(Sender: TObject;
          Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//ApplyBtn.Visible := AddedData.AddedAs > AddNone;
end;

procedure TDerivativesForm.StyleComboBoxChange(Sender: TObject);
begin
  AddedData.fxyMode := TfxyMode(StyleComboBox.ItemIndex);
//ApplyBtn.Visible := AddedData.AddedAs > AddNone;
  ApplyBtnClick(Sender);
end;

procedure TDerivativesForm.VolumeRBClick(Sender: TObject);
begin
  AddedData.AddedAs := AddVolume;
//ApplyBtn.Visible := not TooManyPoints;
  ApplyBtnClick(Sender);
end;

procedure TDerivativesForm.DerivXRBClick(Sender: TObject);
begin
  ViewForm.ClearAddedLines;
  AddedData.AddedAs := AddDerivX;
  DerivXRB.Checked := False;
  ApplyBtnClick(Sender);
end;

procedure TDerivativesForm.DerivYRBClick(Sender: TObject);
begin
  ViewForm.ClearAddedLines;
  AddedData.AddedAs := AddDerivY;
  DerivYRB.Checked := False;
  ApplyBtnClick(Sender);
end;

procedure TDerivativesForm.zCapCBClick(Sender: TObject);
begin
  ViewForm.ClearAddedLines;
  AddedData.zCap := zCapCB.Checked;
//ApplyBtn.Visible := AddedData.AddedAs = AddVolume;
end;

procedure TDerivativesForm.zLimitCBClick(Sender: TObject);
begin
  AddedData.zLim := zLimitCB.Checked;
//ApplyBtn.Visible := AddedData.AddedAs = AddVolume;
end;

procedure TDerivativesForm.ApplyBtnClick(Sender: TObject);
var
  v: TGLFloat;
  s: string;

begin
  if TooManyPoints then Exit;

  Screen.Cursor := crHourGlass;
  with AddedData do
  begin
    if xMin > xMax then  { swap }
    begin
      v := xMin;
      xMin := xMax;
      xMax := v;
      s := EditMinX.Text;
      EditMinX.Text := EditMaxX.Text;
      EditMaxX.Text := s;
    end;
    if yMin > yMax then  { swap }
    begin
      v := yMin;
      yMin := yMax;
      yMax := v;
      s := EditMinY.Text;
      EditMinY.Text := EditMaxY.Text;
      EditMaxY.Text := s;
    end;
    if zMin > zMax then  { swap }
    begin
      v := zMin;
      zMin := zMax;
      zMax := v;
      s := EditMinZ.Text;
      EditMinZ.Text := EditMaxZ.Text;
      EditMaxZ.Text := s;
    end;

    if DerivXRB.Checked then AddedAs := AddDerivX else
    if DerivYRB.Checked then AddedAs := AddDerivY else
    if VolumeRB.Checked then AddedAs := AddVolume;
  end;

  PosVolLabel.Caption := '';
  NegVolLabel.Caption := '';
  TotalLabel.Caption := '';
  VolumeLabel.Caption := '';
  ViewForm.ClearAddedField;
  ViewForm.ClearAddedLines;
  ViewForm.UpdateAdded;
  EvaluateForm.UpdateEvaluate;
  ApplyBtn.Visible := False;
  Screen.Cursor := crDefault;
end;

procedure TDerivativesForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TDerivativesForm.ColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AddedData.AddLineColor;
  if ColorDialog.Execute then
  begin
    AddedData.AddLineColor := ColorDialog.Color;

    with ViewForm do
    begin
      with AddXLine do LineColor.AsWinColor := AddedData.AddLineColor;
      with AddYLine do LineColor.AsWinColor := AddedData.AddLineColor;
      with AddZLine do LineColor.AsWinColor := AddedData.AddLineColor;
    end;

    DerivativeAltered := True;
    ApplyBtnClick(Sender);
  end;
end;

procedure TDerivativesForm.CountzPoints;
var
  n, c: integer;

begin
  with AddedData do
  n := round((xMax - xMin)/xInc +1)*round((yMax - yMin)/yInc +1);
  c := round(Log10(n));
  case c of
  0..1:ZCountLabel.Font.Color := clLime;
     2:ZCountLabel.Font.Color := clTeal;
     3:ZCountLabel.Font.Color := clGreen;
     4:ZCountLabel.Font.Color := clBlue;
  else ZCountLabel.Font.Color := clRed;
  end;
  TooManyPoints := c > 4;
  if TooManyPoints then
  ZCountLabel.Caption := 'Too many Points: ' + FloatToStrF(n, ffnumber, 8, 0)+' !'
  else ZCountLabel.Caption := 'N° Points = ' + FloatToStrF(n, ffnumber, 8, 0);
end;

end.
