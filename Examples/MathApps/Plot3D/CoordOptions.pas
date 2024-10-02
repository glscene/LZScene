unit CoordOptions;



interface

uses
  SysUtils,  Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls;

type
  TCoordsForm = class(TForm)
    ColorDialog: TColorDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;

    xMaxYCB: TCheckBox;
    xMaxZCB: TCheckBox;
    yMaxXCB: TCheckBox;
    yMaxZCB: TCheckBox;
    zMaxXCB: TCheckBox;
    zMaxYCB: TCheckBox;

    xColorBtn: TSpeedButton;
    yColorBtn: TSpeedButton;
    zColorBtn: TSpeedButton;

    ShowCoordsCB: TCheckBox;
    FontDialog: TFontDialog;
    FontButton: TButton;
    CloseBitBtn: TBitBtn;
    procedure xColorBtnClick(Sender: TObject);
    procedure yColorBtnClick(Sender: TObject);
    procedure zColorBtnClick(Sender: TObject);
    procedure FontButtonClick(Sender: TObject);
    procedure ShowCoordsCBClick(Sender: TObject);
    procedure CloseBitBtnClick(Sender: TObject);

    procedure xMaxYCBClick(Sender: TObject);
    procedure xMaxZCBClick(Sender: TObject);
    procedure yMaxXCBClick(Sender: TObject);
    procedure yMaxZCBClick(Sender: TObject);
    procedure zMaxXCBClick(Sender: TObject);
    procedure zMaxYCBClick(Sender: TObject);
  private
    { Private declarations }
    procedure ClearXCoordsCube;
    procedure ClearYCoordsCube;
    procedure ClearZCoordsCube;
  public
    { Public declarations }
    procedure UpdateCoordText;
  end;

var
  CoordsForm: TCoordsForm;

implementation

{$R *.lfm}

uses
  Main, GLVectorTypes, OpenGLTokens, GLBitmapFont, GLVectorGeometry,
  GLCrossPlatform, uGlobal, GridOptions, Evaluate;

procedure TCoordsForm.UpdateCoordText;
var
  ScaleFactor: TGLFloat;
  CoordStep: TGLFloat;
  CoordMin: TGLFloat;
  CoordMax: TGLFloat;

  CurrentXCoord: TGLFloat;
  CurrentYCoord: TGLFloat;
  CurrentZCoord: TGLFloat;

  CurrentFlatText: TGLFlatText;

  procedure CalculateScaleFactor;
  begin
    with ViewData.xyGrid do
    ScaleFactor := xRange.Maximum - xRange.Minimum +
                   yRange.Maximum - yRange.Minimum;
    with ViewData.yzGrid.zRange do
    ScaleFactor := ScaleFactor + (Maximum - Minimum)*ViewData.xyGrid.zScale;
    ScaleFactor := ScaleFactor/3000;
  end;

begin
  if ShowCoordsCB.Checked then
  begin
    CalculateScaleFactor;
  { draw X coords }
    ClearXCoordsCube;
    with ViewData.xyGrid do
    begin
      if xMaxYCB.Checked then CurrentYCoord := yRange.Maximum
                         else CurrentYCoord := yRange.Minimum;
      CoordMax := xRange.Maximum;
      CoordMin := xRange.Minimum;
      CoordStep := xRange.Step;
    end;

    with ViewData do if xMaxZCB.Checked
    then CurrentZCoord := yzGrid.zRange.Maximum * xyGrid.zScale
    else CurrentZCoord := yzGrid.zRange.Minimum * xyGrid.zScale;

    CurrentXCoord := CoordMin;
    while CurrentXCoord <= CoordMax do
    begin
      TGLFlatText.CreateAsChild(ViewForm.XCoordsCube);
      with ViewForm.XCoordsCube do
      begin
        CurrentFlatText := TGLFlatText(Children[Count -1]);
        with CurrentFlatText do
        begin
          BitmapFont := ViewForm.GLWinBmpFont;
          if xMaxYCB.Checked
          then Direction.AsVector := VectorMake(0, 1, 0)
          else Direction.AsVector := VectorMake(0, -1, 0);
          Up.AsVector := VectorMake(0, 0, 1);
          if xMaxZCB.Checked then Layout := tlBottom { locate at z maximum }
          else Layout := tlTop; { or tlBottom, tlCenter }
          ModulateColor.AsWinColor := ViewData.xTextColor;
          Position.AsVector := VectorMake(CurrentXCoord, CurrentYCoord, CurrentZCoord);
          Scale.AsVector := VectorMake(ScaleFactor, ScaleFactor, 0);
          Text := FloatToStr(CurrentXCoord);
        end;
      end;
      CurrentXCoord := CurrentXCoord + CoordStep;
    end;

  { draw Y coords }
    ClearYCoordsCube;
    with ViewData.xyGrid do
    begin
      if yMaxXCB.Checked then CurrentXCoord := xRange.Maximum
                         else CurrentXCoord := xRange.Minimum;
      CoordMax := yRange.Maximum;
      CoordMin := yRange.Minimum;
      CoordStep := yRange.Step;
    end;

    with ViewData do if yMaxZCB.Checked
    then CurrentZCoord := yzGrid.zRange.Maximum * xyGrid.zScale
    else CurrentZCoord := yzGrid.zRange.Minimum * xyGrid.zScale;

    CurrentYCoord := CoordMin;
    while CurrentYCoord <= CoordMax do
    begin
      TGLFlatText.CreateAsChild(ViewForm.YCoordsCube);
      with ViewForm.YCoordsCube do
      begin
        CurrentFlatText := TGLFlatText(Children[Count -1]);
        with CurrentFlatText do
        begin
          BitmapFont := ViewForm.GLWinBmpFont;
          if yMaxXCB.Checked
          then Direction.AsVector := VectorMake(1, 0, 0)
          else Direction.AsVector := VectorMake(-1, 0, 0);
          Up.AsVector := VectorMake(0, 0, 1);
          if yMaxZCB.Checked then Layout := tlBottom { locate at z maximum }
          else Layout := tlTop; { or tlBottom, tlCenter }
          ModulateColor.AsWinColor := ViewData.yTextColor;
          Position.AsVector := VectorMake(CurrentXCoord, CurrentYCoord, CurrentZCoord);
          Scale.AsVector := VectorMake(ScaleFactor, ScaleFactor, 0);
          Text := FloatToStr(CurrentYCoord);
        end;
      end;
      CurrentYCoord := CurrentYCoord + CoordStep;
    end;

  { draw Z coords }
    ClearZCoordsCube;
    with ViewData.xzGrid do
    begin
      if zMaxXCB.Checked then CurrentXCoord := xRange.Maximum
                         else CurrentXCoord := xRange.Minimum;
      CoordMax := zRange.Maximum;
      CoordMin := zRange.Minimum;
      CoordStep := zRange.Step;
    end;

    with ViewData do if zMaxYCB.Checked
    then CurrentYCoord := xyGrid.yRange.Maximum
    else CurrentYCoord := xyGrid.yRange.Minimum;

    CurrentZCoord := CoordMin;
    while CurrentZCoord <= CoordMax do
    begin
      TGLFlatText.CreateAsChild(ViewForm.ZCoordsCube);
      with ViewForm.ZCoordsCube do
      begin
        CurrentFlatText := TGLFlatText(Children[Count -1]);
        with CurrentFlatText do
        begin
          BitmapFont := ViewForm.GLWinBmpFont;
          if zMaxXCB.Checked then
          begin
            if not zMaxYCB.Checked
            then Direction.AsVector := VectorMake(0, -1, 0);
          end
          else
          begin
            if not zMaxYCB.Checked
            then Direction.AsVector := VectorMake(-1, 0, 0)
            else Direction.AsVector := VectorMake(0, 1, 0);
          end;
          Up.AsVector := VectorMake(0, 0, 1);
          Layout := tlCenter;
          ModulateColor.AsWinColor := ViewData.zTextColor;
          Position.AsVector := VectorMake(CurrentXCoord, CurrentYCoord,
                                          CurrentZCoord*ViewData.xyGrid.zScale);

          Scale.AsVector := VectorMake(ScaleFactor, ScaleFactor, 0);
          if CurrentZCoord < 0 then Text :=  ' '+FloatToStr(CurrentZCoord)
                               else Text := '  '+FloatToStr(CurrentZCoord);
        end;
      end;
      CurrentZCoord := CurrentZCoord + CoordStep;
    end;
    Altered := True;
  end;
end;

procedure TCoordsForm.ClearXCoordsCube;
var
  i: integer;

begin
  i := ViewForm.XCoordsCube.Count;
  while i > 0 do
  begin
    ViewForm.XCoordsCube.Children[i -1].Free;
    i := ViewForm.XCoordsCube.Count;
  end;
end;

procedure TCoordsForm.ClearYCoordsCube;
var
  i: integer;

begin
  i := ViewForm.YCoordsCube.Count;
  while i > 0 do
  begin
    ViewForm.YCoordsCube.Children[i -1].Free;
    i := ViewForm.YCoordsCube.Count;
  end;
end;

procedure TCoordsForm.ClearZCoordsCube;
var
  i: integer;

begin
  i := ViewForm.ZCoordsCube.Count;
  while i > 0 do
  begin
    ViewForm.ZCoordsCube.Children[i -1].Free;
    i := ViewForm.ZCoordsCube.Count;
  end;
end;

procedure TCoordsForm.CloseBitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TCoordsForm.FontButtonClick(Sender: TObject);
begin
  FontDialog.Font := ViewForm.GLWinBmpFont.Font;
  FontDialog.Font.Name := ViewData.TextFontN;
  FontDialog.Font.Size := ViewData.TextFontSz;
  if FontDialog.Execute then
  begin
    ViewForm.GLWinBmpFont.Font := FontDialog.Font;
    ViewData.TextFontN := FontDialog.Font.Name;
    ViewData.TextFontSz := FontDialog.Font.Size;
    FontButton.Caption := 'Font:'+' '+ViewData.TextFontN+' '+
                             IntToStr(ViewData.TextFontSz);
    UpdateCoordText;
   end;
end;

procedure TCoordsForm.ShowCoordsCBClick(Sender: TObject);
begin
  ViewForm.XCoordsCube.Visible := ShowCoordsCB.Checked;
  ViewForm.YCoordsCube.Visible := ShowCoordsCB.Checked;
  ViewForm.ZCoordsCube.Visible := ShowCoordsCB.Checked;
  if Active then
  begin
    UpdateCoordText;
    ViewData.TextVisible := ShowCoordsCB.Checked;
  end;
end;

procedure TCoordsForm.xColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := ViewData.xTextColor;
  if ColorDialog.Execute then
  begin
    ViewData.xTextColor := ColorDialog.Color;
    UpdateCoordText;
    EvaluateForm.DoEvaluate;
  end;
end;

procedure TCoordsForm.xMaxYCBClick(Sender: TObject);
begin
  ViewData.xPosYMax := xMaxYCB.Checked;
  UpdateCoordText;
end;

procedure TCoordsForm.xMaxZCBClick(Sender: TObject);
begin
  ViewData.xPosZMax := xMaxZCB.Checked;
  UpdateCoordText;
end;

procedure TCoordsForm.yColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := ViewData.yTextColor;
  if ColorDialog.Execute then
  begin
    ViewData.yTextColor := ColorDialog.Color;
    UpdateCoordText;
    EvaluateForm.DoEvaluate;
  end;
end;

procedure TCoordsForm.yMaxXCBClick(Sender: TObject);
begin
  ViewData.yPosXMax := yMaxXCB.Checked;
  UpdateCoordText;
end;

procedure TCoordsForm.yMaxZCBClick(Sender: TObject);
begin
  ViewData.yPosZMax := yMaxZCB.Checked;
  UpdateCoordText;
end;

procedure TCoordsForm.zColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := ViewData.zTextColor;
  if ColorDialog.Execute then
  begin
    ViewData.zTextColor := ColorDialog.Color;
    UpdateCoordText;
  end;
end;

procedure TCoordsForm.zMaxXCBClick(Sender: TObject);
begin
  ViewData.zPosXMax := zMaxXCB.Checked;
  UpdateCoordText;
end;

procedure TCoordsForm.zMaxYCBClick(Sender: TObject);
begin
  ViewData.zPosYMax := zMaxYCB.Checked;
  UpdateCoordText;
end;

end.
