unit Evaluate;

{$MODE Delphi}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, OpenGL1x, OpenGLTokens, GLVectorGeometry,
  Menus, ComCtrls;

type
  TEvaluateForm = class(TForm)
    GroupBox1: TGroupBox;
    Labe1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    EditX: TEdit;
    EditY: TEdit;
    EditZ: TEdit;
    Editdzdx: TEdit;
    Editdzdy: TEdit;
    Coordinates: TCheckBox;
    BitBtn1: TBitBtn;
    ToGrids: TCheckBox;
    EditCoordWidth: TEdit;
    ColorButton: TSpeedButton;
    ColorDialog: TColorDialog;
    dzdx_dzdy: TCheckBox;
    PopupMenu1: TPopupMenu;
    Slope1: TMenuItem;
    Degree1: TMenuItem;
    Radian1: TMenuItem;
    Label1: TLabel;
    EditArrow: TEdit;
    procedure FloatKeyPress(Sender: TObject; var Key: Char);
    procedure BitBtn1Click(Sender: TObject);
    procedure ColorButtonClick(Sender: TObject);
    procedure IntPress(Sender: TObject; var Key: Char);
    procedure EditXKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditYKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditCoordWidthKeyUp(Sender: TObject; var Key: Word;
                                   Shift: TShiftState);
    procedure CoordinatesClick(Sender: TObject);
    procedure ToGridsClick(Sender: TObject);
    procedure dzdx_dzdyClick(Sender: TObject);
    procedure MenuClick(Sender: TObject);
    procedure EditArrowKeyPress(Sender: TObject; var Key: Char);
    procedure EditArrowKeyUp(Sender: TObject; var Key: Word;
                              Shift: TShiftState);
  private
    { Private declarations }
    zValue: TGLFloat;
    dzdx, dzdy: TGLFloat;
    CurrentTag: integer;
    procedure ShowCoords(const x, y, z: TGLFloat; const c, t: Boolean);
  public
    { Public declarations }
    procedure UpdateEvaluate;
    procedure UpdateCoords;
    procedure DoEvaluate;
  end;

var
  EvaluateForm: TEvaluateForm;

implementation

uses
  Math, GLVectorTypes,
  IniFiles, uGlobal, Main, uParser, Functions, DerivativeOptions;

{$R *.lfm}

procedure TEvaluateForm.FloatKeyPress(Sender: TObject; var Key: Char);
begin
  with Sender as TEdit do
  if not CharInSet(Key, AnyFloat) then Key := #0;
end;

procedure TEvaluateForm.EditXKeyUp(Sender: TObject; var Key: Word;
                                    Shift: TShiftState);
var
  v: TGLFloat;

begin
  try
    v := StrToFloat(EditX.Text);
  except
    v := 0.0;
  end;
  ViewData.xEvaluate := v;
  DoEvaluate;
  with ViewData do ShowCoords(xEvaluate, yEvaluate, zValue*xyGrid.zScale,
                              CoordChecked, ToGridsChecked);
  Altered := True;
end;

procedure TEvaluateForm.EditYKeyUp(Sender: TObject; var Key: Word;
                                    Shift: TShiftState);
var
  v: TGLFloat;

begin
  try
    v := StrToFloat(EditY.Text);
  except
    v := 0.0;
  end;
  ViewData.yEvaluate := v;
  DoEvaluate;
  with ViewData do ShowCoords(xEvaluate, yEvaluate, zValue*xyGrid.zScale,
                              CoordChecked, ToGridsChecked);
  Altered := True;
end;

procedure TEvaluateForm.DoEvaluate;
var
  e: byte;
  x0, y0, x1, y1, z1, x2, y2, z2, dx, dy, dz, Sz: extended;
  VectorPos, VectorXDir, VectorYDir: TVector;

begin
  if Visible then
  begin
    with PlotData do                                //x1,y2---------------x2,y2
    begin                                           //  |                   |
      x0 := ViewData.xEvaluate;                     //  |                   |
      y0 := ViewData.yEvaluate;                     //  |        zValue     |
      dx := xInc;                                   //  |         .         |
      dy := yInc;                                   //  |       x0,y0       |
      x1 := x0 - dx;                                //  |                   |
      y1 := y0 - dy;                                //  |                   |
      x2 := x0 + dx;                                //  |                   |
      y2 := y0 + dy;                                //x1,y1---------------x2,y1
    end;

    zValue := ParseEvaluateFxy(x0, y0, PlotData.fxyStr, e);{ evaluate z, x0,y0 }
    EditZ.Text := FloatTostr(zValue);
    VectorPos := VectorMake(x0, y0, zValue*ViewData.xyGrid.zScale);

    z1 := ParseEvaluateFxy(x1, y0, PlotData.fxyStr, e);   { evaluate z1, x1,y0 }
    z2 := ParseEvaluateFxy(x2, y0, PlotData.fxyStr, e);   { evaluate z2, x2,y0 }
    dz := z2 - z1;
    dzdx := dz/(x2 - x1);                            { dzdx = slope wrt x axis }

    VectorXDir := VectorMake(x2 - x1, 0, dz*ViewData.xyGrid.zScale);
    case CurrentTag of
    0:Editdzdx.Text := FloatToStrF(dzdx, ffNumber, 12, 8);
    1:Editdzdx.Text := FloatToStrF(RadToDeg(ArcTan(dzdx)), ffNumber, 12, 8)+'°';
    2:Editdzdx.Text := FloatToStrF(ArcTan(dzdx), ffNumber, 12, 8)+' Rad.';
    end;

    z1 := ParseEvaluateFxy(x0, y1, PlotData.fxyStr, e);   { evaluate z1, x0,y1 }
    z2 := ParseEvaluateFxy(x0, y2, PlotData.fxyStr, e);   { evaluate z2, x0,y2 }
    dz := z2 - z1;
    dzdy := dz/(y2 - y1);                            { dzdy = slope wrt y axis }

    VectorYDir := VectorMake(0, y2 - y1, dz*ViewData.xyGrid.zScale);
    case CurrentTag of
    0:Editdzdy.Text := FloatToStrF(dzdy, ffNumber, 12, 8);
    1:Editdzdy.Text := FloatToStrF(RadToDeg(ArcTan(dzdy)), ffNumber, 12, 8)+'°';
    2:Editdzdy.Text := FloatToStrF(ArcTan(dzdy), ffNumber, 12, 8)+' Rad.';
    end;

    if dzdx_dzdy.Checked then
    begin
      with ViewData.xyGrid do if xRange.Step < yRange.Step
      then Sz := xRange.Step else Sz := yRange.Step;
      with ViewForm.xArrow do
      begin
        with ViewData do
        begin
          Scale.AsVector := VectorMake(ArrowSize*Sz/2, ArrowSize*Sz/2, ArrowSize*Sz);
          Material.FrontProperties.Diffuse.AsWinColor := xTextColor;
        end;
        Direction.AsVector := VectorXDir;
        Position.AsVector := VectorPos;
      end;

      with ViewForm.yArrow do
      begin
        with ViewData do
        begin
          Scale.AsVector := VectorMake(ArrowSize*Sz/2, ArrowSize*Sz/2, ArrowSize*Sz);
          Material.FrontProperties.Diffuse.AsWinColor := yTextColor;
        end;
        Direction.AsVector := VectorYDir;
        Position.AsVector := VectorPos;
      end;
    end;
  end;
end;

procedure TEvaluateForm.dzdx_dzdyClick(Sender: TObject);
begin
  ViewData.dzdx_dyChecked := dzdx_dzdy.Checked;
  ViewForm.xArrow.Visible := dzdx_dzdy.Checked;
  ViewForm.yArrow.Visible := dzdx_dzdy.Checked;
  DoEvaluate;
  Altered := True;
end;

procedure TEvaluateForm.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure TEvaluateForm.EditArrowKeyPress(Sender: TObject; var Key: Char);
begin
  with Sender as TEdit do
  if not CharInSet(Key, PosFloat) then Key := #0;
end;

procedure TEvaluateForm.EditArrowKeyUp(Sender: TObject; var Key: Word;
                                        Shift: TShiftState);
var
  w: TGLFloat;

begin
  try
    w := StrToFloat(EditArrow.Text);
  except
    w := 1;
  end;
  ViewData.ArrowSize := w;
  Altered := True;
  DoEvaluate;
end;

procedure TEvaluateForm.EditCoordWidthKeyUp(Sender: TObject; var Key: Word;
                                             Shift: TShiftState);
var
  w: integer;

begin
  try
    w := StrToInt(EditCoordWidth.Text);
  except
    w := 3;
  end;
  ViewData.CoordWidth := w;
  ViewForm.xCoordLine.LineWidth := w;
  ViewForm.yCoordLine.LineWidth := w;
  ViewForm.zCoordLine.LineWidth := w;
  Altered := True;
end;

procedure TEvaluateForm.IntPress(Sender: TObject; var Key: Char);
begin
  with Sender as TEdit do
  if not CharInSet(Key, ['0'..'9', #8]) then Key := #0
end;

procedure TEvaluateForm.ColorButtonClick(Sender: TObject);
begin
  if ColorDialog.Execute then
  begin
    ViewData.CoordColor := ColorDialog.Color;
    UpdateCoords;
    Altered := True;
  end;
end;

procedure TEvaluateForm.CoordinatesClick(Sender: TObject);
begin
  ViewData.CoordChecked := Coordinates.Checked;
  DoEvaluate;
  with ViewData do ShowCoords(xEvaluate, yEvaluate, zValue*xyGrid.zScale,
                              CoordChecked, ToGridsChecked);
  if DerivativesForm.Visible and not Coordinates.Checked then
  begin
    ViewForm.AddXLine.Visible := False;
    ViewForm.AddYLine.Visible := False;
    ViewForm.AddZLine.Visible := False;
  end;

  Altered := True;
end;

procedure TEvaluateForm.ToGridsClick(Sender: TObject);
begin
  ViewData.ToGridsChecked := ToGrids.Checked;
  DoEvaluate;
  with ViewData do ShowCoords(xEvaluate, yEvaluate, zValue*xyGrid.zScale,
                              CoordChecked, ToGridsChecked);
  Altered := True;
end;

procedure TEvaluateForm.ShowCoords(const x, y, z: TGLFloat; const c, t: Boolean);
begin
  with ViewForm do
  begin
    if c then                  { show coords }
    begin
      with xCoordLine do
      begin
        if t then              { to grids }
        begin
          Position.SetPoint(GLyzGrid.Position.x, y, z);
          Nodes[0].X := x - GLyzGrid.Position.x;
        end
        else                   { +- step }
        begin
          Position.SetPoint(x - GLxyGrid.XSamplingScale.Step, y, z);
          Nodes[0].X := 2*GLxyGrid.XSamplingScale.Step;
        end;
        Visible := True;
      end;

      with yCoordLine do
      begin
        if t then              { to grids }
        begin
          Position.SetPoint(x, GLxzGrid.Position.y, z);
          Nodes[0].Y := y - GLxzGrid.Position.y;
        end
        else                   { +- step }
        begin
          Position.SetPoint(x, y - GLxyGrid.YSamplingScale.Step, z);
          Nodes[0].Y := 2*GLxyGrid.YSamplingScale.Step;
        end;
        Visible := True;
      end;

      with zCoordLine do
      begin
        if t then              { to grids }
        begin
          Position.SetPoint(x, y, GLxyGrid.Position.z);
          Nodes[0].Z := z - GLxyGrid.Position.z;
        end
        else                   { +- step }
        begin
          Position.SetPoint(x, y, z - GLxzGrid.ZSamplingScale.Step);
          Nodes[0].Z := 2*GLxzGrid.ZSamplingScale.Step;
        end;
        Visible := True;
      end;

    { only after DerivativesForm.ApplyBtnClick if AddedAs.AddedAs = AddVolume }
      if DerivativesForm.Visible then
      begin
        case AddedData.AddedAs of
          AddDerivX:
          begin
            with AddXLine do        { xCoord }
            begin
              if t then             { to grids }
              begin
                Position.SetPoint(GLyzGrid.Position.x, y, dzdx*ViewData.xyGrid.zScale);
                Nodes[0].AsVector := VectorMake(x - GLyzGrid.Position.x, 0, 0);
              end
              else                  { +- step }
              begin
                Position.SetPoint(x - GLxyGrid.XSamplingScale.Step, y, dzdx*ViewData.xyGrid.zScale);
                Nodes[0].AsVector := VectorMake(2*GLxyGrid.XSamplingScale.Step, 0, 0);
              end;
              Visible := True;
            end;

            with AddYLine do         { yCoord }
            begin
              if t then              { to grids }
              begin
                Position.SetPoint(x, GLxzGrid.Position.y, dzdx*ViewData.xyGrid.zScale);
                Nodes[0].AsVector := VectorMake(0, y - GLxzGrid.Position.y, 0);
              end
              else                   { +- step }
              begin
                Position.SetPoint(x, y - GLxyGrid.YSamplingScale.Step, dzdx*ViewData.xyGrid.zScale);
                Nodes[0].AsVector := VectorMake(0, 2*GLxyGrid.YSamplingScale.Step, 0);
              end;
              Visible := True;
            end;

            with AddZLine do         { zCoord }
            begin
              if t then              { to grids }
              begin
                Position.SetPoint(x, y, GLxyGrid.Position.z);
                Nodes[0].AsVector := VectorMake(0, 0, dzdx*ViewData.xyGrid.zScale - GLxyGrid.Position.z);
              end
              else                   { +- step }
              begin
                Position.SetPoint(x, y, dzdx*ViewData.xyGrid.zScale - GLxzGrid.ZSamplingScale.Step);
                Nodes[0].AsVector := VectorMake(0, 0, 2*GLxzGrid.ZSamplingScale.Step);
              end;
              Visible := True;
            end;

          end;  { AddDerivX }

          AddDerivY:
          begin
            with AddXLine do        { xCoord }
            begin
              if t then             { to grids }
              begin
                Position.SetPoint(GLyzGrid.Position.x, y, dzdy*ViewData.xyGrid.zScale);
                Nodes[0].AsVector := VectorMake(x - GLyzGrid.Position.x, 0, 0);
              end
              else                  { +- step }
              begin
                Position.SetPoint(x - GLxyGrid.XSamplingScale.Step, y, dzdy*ViewData.xyGrid.zScale);
                Nodes[0].AsVector := VectorMake(2*GLxyGrid.XSamplingScale.Step, 0, 0);
              end;
              Visible := True;
            end;

            with AddYLine do         { yCoord }
            begin
              if t then              { to grids }
              begin
                Position.SetPoint(x, GLxzGrid.Position.y, dzdy*ViewData.xyGrid.zScale);
                Nodes[0].AsVector := VectorMake(0, y - GLxzGrid.Position.y, 0);
              end
              else                   { +- step }
              begin
                Position.SetPoint(x, y - GLxyGrid.YSamplingScale.Step, dzdy*ViewData.xyGrid.zScale);
                Nodes[0].AsVector := VectorMake(0, 2*GLxyGrid.YSamplingScale.Step, 0);
              end;
              Visible := True;
            end;

            with AddZLine do         { zCoord }
            begin
              if t then              { to grids }
              begin
                Position.SetPoint(x, y, GLxyGrid.Position.z);
                Nodes[0].AsVector := VectorMake(0, 0, dzdy*ViewData.xyGrid.zScale - GLxyGrid.Position.z);
              end
              else                   { +- step }
              begin
                Position.SetPoint(x, y, dzdy*ViewData.xyGrid.zScale - GLxzGrid.ZSamplingScale.Step);
                Nodes[0].AsVector := VectorMake(0, 0, 2*GLxzGrid.ZSamplingScale.Step);
              end;
              Visible := True;
            end;

          end;  { AddDerivY }

          AddVolume:
          begin
            AddXLine.Visible := False;
            AddYLine.Visible := False;
            AddZLine.Visible := False;
          end;  { AddVolume }
        end;  { case AddedData.AddedAs of... }
      end;  { if DerivativesForm.Visible then... }
    end   { if c then show coords }
    else
    begin { coordinate lines not visible }
      xCoordLine.Visible := False;
      yCoordLine.Visible := False;
      zCoordLine.Visible := False;
    end;
  end;

  with ViewData do
  begin
    EditArrow.Text := FloatToStr(ArrowSize);
    EditCoordWidth.Text := IntToStr(CoordWidth);
    ViewForm.xCoordLine.LineWidth := CoordWidth;
    ViewForm.yCoordLine.LineWidth := CoordWidth;
    ViewForm.zCoordLine.LineWidth := CoordWidth;
  end;
end;

procedure TEvaluateForm.MenuClick(Sender: TObject);
begin
  if CurrentTag <> TMenuItem(Sender).Tag then
  begin
    case CurrentTag of
      0:Slope1.Checked  := False;
      1:Degree1.Checked := False;
      2:Radian1.Checked := False;
    end;
    with Sender as TMenuItem do
    begin
      Checked := True;
      CurrentTag := Tag;
    end;
    DoEvaluate;
  end;
end;

procedure TEvaluateForm.UpdateEvaluate;
begin
  DoEvaluate;
  with ViewData do ShowCoords(xEvaluate, yEvaluate, zValue*xyGrid.zScale,
                              CoordChecked, ToGridsChecked);
end;

procedure TEvaluateForm.UpdateCoords;
begin
  with ViewForm do
  begin
    with xCoordLine do LineColor.AsWinColor := ViewData.CoordColor;
    with yCoordLine do LineColor.AsWinColor := ViewData.CoordColor;
    with zCoordLine do LineColor.AsWinColor := ViewData.CoordColor;
  end;
end;

end.
