unit GridColors;


interface

uses
  LCLType,
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ComCtrls,
  StdCtrls, Buttons;

type
  TGridColorsForm = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    xyRed: TTrackBar;
    xyGreen: TTrackBar;
    xyBlue: TTrackBar;
    xyAlpha: TTrackBar;
    GroupBox2: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    xzRed: TTrackBar;
    xzGreen: TTrackBar;
    xzBlue: TTrackBar;
    xzAlpha: TTrackBar;
    GroupBox3: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    yzRed: TTrackBar;
    yzGreen: TTrackBar;
    yzBlue: TTrackBar;
    yzAlpha: TTrackBar;
    GroupBox4: TGroupBox;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    BoxRed: TTrackBar;
    BoxGreen: TTrackBar;
    BoxBlue: TTrackBar;
    BoxAlpha: TTrackBar;
    BackColorBtn: TSpeedButton;
    BitBtn1: TBitBtn;
    ColorDialog: TColorDialog;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure xyRedChange(Sender: TObject);
    procedure xyGreenChange(Sender: TObject);
    procedure xyBlueChange(Sender: TObject);
    procedure xyAlphaChange(Sender: TObject);
    procedure xzRedChange(Sender: TObject);
    procedure xzGreenChange(Sender: TObject);
    procedure xzBlueChange(Sender: TObject);
    procedure xzAlphaChange(Sender: TObject);
    procedure yzRedChange(Sender: TObject);
    procedure yzGreenChange(Sender: TObject);
    procedure yzBlueChange(Sender: TObject);
    procedure yzAlphaChange(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BackColorBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BoxRedChange(Sender: TObject);
    procedure BoxGreenChange(Sender: TObject);
    procedure BoxBlueChange(Sender: TObject);
    procedure BoxAlphaChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowGridColorData;
  end;

var
  GridColorsForm: TGridColorsForm;

implementation

uses
   uGlobal, Main;

{$R *.lfm}

procedure TGridColorsForm.BackColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := ViewData.BackColor;
  if ColorDialog.Execute then
  begin
    ViewData.BackColor := ColorDialog.Color;
    ViewForm.GLSViewer.Buffer.BackgroundColor := Viewdata.BackColor;
    Altered := True;
  end;
end;

procedure TGridColorsForm.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure TGridColorsForm.FormCloseQuery(Sender: TObject;
                                   var CanClose: Boolean);
begin
  if GridColorsAltered then
  begin
    case MessageDlg('The current graph''s color data has been altered.'+
              #13#10'Do you wish to save the alterations ?', mtConfirmation,
                    [mbYes, mbNo, mbCancel], 0) of
    mrYes: Altered := Altered or GridColorsAltered;
 mrCancel: begin
             CanClose := False;
             Exit;
           end;
    end;
  end;
end;

procedure TGridColorsForm.FormKeyDown(Sender: TObject; var Key: Word;
                                       Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

procedure TGridColorsForm.FormShow(Sender: TObject);
begin
  ShowGridColorData;
  GridColorsAltered := False;
end;

procedure TGridColorsForm.xyRedChange(Sender: TObject);
begin
  ViewForm.GLxyGrid.LineColor.Red := xyRed.Position/255;
  ViewData.xyGrid.Color.X := ViewForm.GLxyGrid.LineColor.Red;
  GridColorsAltered := True;
end;    { TGridColorsForm.xyRedChange }

procedure TGridColorsForm.xyGreenChange(Sender: TObject);
begin
  ViewForm.GLxyGrid.LineColor.Green := xyGreen.Position/255;
  ViewData.xyGrid.Color.Y := ViewForm.GLxyGrid.LineColor.Green;
  GridColorsAltered := True;
end;    { TGridColorsForm.xyGreenChange }

procedure TGridColorsForm.xyBlueChange(Sender: TObject);
begin
  ViewForm.GLxyGrid.LineColor.Blue := xyBlue.Position/255;
  ViewData.xyGrid.Color.Z := ViewForm.GLxyGrid.LineColor.Blue;
  GridColorsAltered := True;
end;    { TGridColorsForm.xyBlueChange }

procedure TGridColorsForm.xyAlphaChange(Sender: TObject);
begin
  ViewForm.GLxyGrid.LineColor.Alpha := xyAlpha.Position/1000;
  ViewData.xyGrid.Color.W := ViewForm.GLxyGrid.LineColor.Alpha;
  GridColorsAltered := True;
end;    { TGridColorsForm.xyAlphaChange }

procedure TGridColorsForm.xzRedChange(Sender: TObject);
begin
  ViewForm.GLxzGrid.LineColor.Red := xzRed.Position/255;
  ViewData.xzGrid.Color.X := ViewForm.GLxzGrid.LineColor.Red;
  GridColorsAltered := True;
end;    { TGridColorsForm.xzRedChange }

procedure TGridColorsForm.xzGreenChange(Sender: TObject);
begin
  ViewForm.GLxzGrid.LineColor.Green := xzGreen.Position/255;
  ViewData.xzGrid.Color.Y := ViewForm.GLxzGrid.LineColor.Green;
  GridColorsAltered := True;
end;    { TGridColorsForm.xzGreenChange }

procedure TGridColorsForm.xzBlueChange(Sender: TObject);
begin
  ViewForm.GLxzGrid.LineColor.Blue := xzBlue.Position/255;
  ViewData.xzGrid.Color.Z := ViewForm.GLxzGrid.LineColor.Blue;
  GridColorsAltered := True;
end;    { TGridColorsForm.xzBlueChange }

procedure TGridColorsForm.xzAlphaChange(Sender: TObject);
begin
  ViewForm.GLxzGrid.LineColor.Alpha := xzAlpha.Position/1000;
  ViewData.xzGrid.Color.W := ViewForm.GLxzGrid.LineColor.Alpha;
  GridColorsAltered := True;
end;    { TGridColorsForm.xzAlphaChange }

procedure TGridColorsForm.yzRedChange(Sender: TObject);
begin
  ViewForm.GLyzGrid.LineColor.Red := yzRed.Position/255;
  ViewData.yzGrid.Color.X := ViewForm.GLyzGrid.LineColor.Red;
  GridColorsAltered := True;
end;    { TGridColorsForm.yzRedChange }

procedure TGridColorsForm.yzGreenChange(Sender: TObject);
begin
  ViewForm.GLyzGrid.LineColor.Green := yzGreen.Position/255;
  ViewData.yzGrid.Color.Y := ViewForm.GLyzGrid.LineColor.Green;
  GridColorsAltered := True;
end;    { TGridColorsForm.yzGreenChange }

procedure TGridColorsForm.yzBlueChange(Sender: TObject);
begin
  ViewForm.GLyzGrid.LineColor.Blue := yzBlue.Position/255;
  ViewData.yzGrid.Color.Z := ViewForm.GLyzGrid.LineColor.Blue;
  GridColorsAltered := True;
end;    { TGridColorsForm.yzBlueChange }

procedure TGridColorsForm.yzAlphaChange(Sender: TObject);
begin
  ViewForm.GLyzGrid.LineColor.Alpha := yzAlpha.Position/1000;
  ViewData.yzGrid.Color.W := ViewForm.GLyzGrid.LineColor.Alpha;
  GridColorsAltered := True;
end;    { TGridColorsForm.yzAlphaChange }

procedure TGridColorsForm.BoxRedChange(Sender: TObject);
begin
  ViewForm.BoxLine1.LineColor.Red := BoxRed.Position/255;
  ViewForm.BoxLine2.LineColor := ViewForm.BoxLine1.LineColor;
  ViewForm.BoxLine3.LineColor := ViewForm.BoxLine1.LineColor;
  ViewForm.BoxLine4.LineColor := ViewForm.BoxLine1.LineColor;
  ViewData.BoxLnColor.X := ViewForm.BoxLine1.LineColor.Red;
  GridColorsAltered := True;
end;    { TGridColorsForm.BoxRedChange }

procedure TGridColorsForm.BoxGreenChange(Sender: TObject);
begin
  ViewForm.BoxLine1.LineColor.Green := BoxGreen.Position/255;
  ViewForm.BoxLine2.LineColor := ViewForm.BoxLine1.LineColor;
  ViewForm.BoxLine3.LineColor := ViewForm.BoxLine1.LineColor;
  ViewForm.BoxLine4.LineColor := ViewForm.BoxLine1.LineColor;
  ViewData.BoxLnColor.Y := ViewForm.BoxLine1.LineColor.Green;
  GridColorsAltered := True;
end;    { TGridColorsForm.BoxGreenChange }

procedure TGridColorsForm.BoxBlueChange(Sender: TObject);
begin
  ViewForm.BoxLine1.LineColor.Blue := BoxBlue.Position/255;
  ViewForm.BoxLine2.LineColor := ViewForm.BoxLine1.LineColor;
  ViewForm.BoxLine3.LineColor := ViewForm.BoxLine1.LineColor;
  ViewForm.BoxLine4.LineColor := ViewForm.BoxLine1.LineColor;
  ViewData.BoxLnColor.Z := ViewForm.BoxLine1.LineColor.Blue;
  GridColorsAltered := True;
end;    { TGridColorsForm.BoxBlueChange }

procedure TGridColorsForm.BoxAlphaChange(Sender: TObject);
begin
  ViewForm.BoxLine1.LineColor.Alpha := BoxAlpha.Position/1000;
  ViewData.BoxLnColor.W := ViewForm.BoxLine1.LineColor.Alpha;
  ViewForm.BoxLine2.LineColor := ViewForm.BoxLine1.LineColor;
  ViewForm.BoxLine3.LineColor := ViewForm.BoxLine1.LineColor;
  ViewForm.BoxLine4.LineColor := ViewForm.BoxLine1.LineColor;
  GridColorsAltered := True;
end;    { TGridColorsForm.BoxAlphaChange }

    { Public declarations }
procedure TGridColorsForm.ShowGridColorData;
begin
  Caption := GraphFName;
  GridColorsAltered := False;

  with ViewForm.GLxyGrid.LineColor do
  begin
    xyRed.Position := round(Red*255);
    xyGreen.Position := round(Green*255);
    xyBlue.Position := round(Blue*255);
    xyAlpha.Position := round(Alpha*1000);
  end;
  with ViewForm.GLxzGrid.LineColor do
  begin
    xzRed.Position := round(Red*255);
    xzGreen.Position := round(Green*255);
    xzBlue.Position := round(Blue*255);
    xzAlpha.Position := round(Alpha*1000);
  end;
  with ViewForm.GLyzGrid.LineColor do
  begin
    yzRed.Position := round(Red*255);
    yzGreen.Position := round(Green*255);
    yzBlue.Position := round(Blue*255);
    yzAlpha.Position := round(Alpha*1000);
  end;
  with ViewForm do
  begin
    BoxRed.Position := round(BoxLine1.LineColor.Red*255);
    BoxGreen.Position := round(BoxLine1.LineColor.Green*255);
    BoxBlue.Position := round(BoxLine1.LineColor.Blue*255);
    BoxAlpha.Position := round(BoxLine1.LineColor.Alpha*1000);
  end;
end;

end.
