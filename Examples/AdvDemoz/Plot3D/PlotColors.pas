unit PlotColors;



interface

uses
  LCLType,
  SysUtils,  Classes,
  Graphics,Controls, Forms, Dialogs, StdCtrls, Buttons,
  ComCtrls;

type
  TPlotColorsForm = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    UpperRed: TTrackBar;
    UpperGreen: TTrackBar;
    UpperBlue: TTrackBar;
    UpperAlpha: TTrackBar;
    GroupBox2: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LowerRed: TTrackBar;
    LowerGreen: TTrackBar;
    LowerBlue: TTrackBar;
    LowerAlpha: TTrackBar;
    UpperLowerLock: TCheckBox;
    EditBlend: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    EditMove: TEdit;
    ApplyBtn: TBitBtn;
    BitBtn1: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure UpperRedChange(Sender: TObject);
    procedure UpperGreenChange(Sender: TObject);
    procedure UpperBlueChange(Sender: TObject);
    procedure UpperAlphaChange(Sender: TObject);
    procedure LowerRedChange(Sender: TObject);
    procedure LowerGreenChange(Sender: TObject);
    procedure LowerBlueChange(Sender: TObject);
    procedure LowerAlphaChange(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UpperLowerLockClick(Sender: TObject);
    procedure EditBlendKeyDown(Sender: TObject; var Key: Word;
                                Shift: TShiftState);
    procedure EditBlendKeyPress(Sender: TObject; var Key: Char);
    procedure EditBlendKeyUp(Sender: TObject; var Key: Word;
                              Shift: TShiftState);
    procedure EditMoveKeyPress(Sender: TObject; var Key: Char);
    procedure EditMoveKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowPlotColorData;
  end;

var
  PlotColorsForm: TPlotColorsForm;

implementation

uses
   uGlobal, Main, Functions, OpenGLTokens, DerivativeOptions;

{$R *.lfm}

procedure TPlotColorsForm.ApplyBtnClick(Sender: TObject);
begin
  with FunctionsForm.CheckListBox do
  TPlotDataObject(Items.Objects[ItemIndex]).Data := PlotData;
  ViewForm.UpdatePlot;
  ApplyBtn.Visible := False;
  Altered := True;
end;

procedure TPlotColorsForm.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure TPlotColorsForm.EditBlendKeyDown(Sender: TObject; var Key: Word;
                                            Shift: TShiftState);
begin
  if (Key = VK_DELETE) or (Key = VK_BACK) then ApplyBtn.Visible := True;
end;

procedure TPlotColorsForm.EditBlendKeyPress(Sender: TObject; var Key: Char);
begin
  if CharInSet(Key, PosFloat) then ApplyBtn.Visible := True
  else Key := #0;
end;

procedure TPlotColorsForm.EditBlendKeyUp(Sender: TObject; var Key: Word;
                                          Shift: TShiftState);
var
  x: TGLFloat;

begin
  try
    x := StrToFloat(EditBlend.Text);
  except
    x := 1.0;
  end;
  PlotData.ColorBlend := x;
  ApplyBtn.Visible := True;
end;


procedure TPlotColorsForm.EditMoveKeyPress(Sender: TObject; var Key: Char);
begin
  if CharInSet(Key, AnyFloat) then ApplyBtn.Visible := True
  else Key := #0;
end;

procedure TPlotColorsForm.EditMoveKeyUp(Sender: TObject; var Key: Word;
                                         Shift: TShiftState);
var
  x: TGLFloat;

begin
  try
    x := StrToFloat(EditMove.Text);
  except
    x := 0.0;
  end;
  PlotData.ColorMove := x;
  ApplyBtn.Visible := True;
end;

procedure TPlotColorsForm.FormCloseQuery(Sender: TObject;
                                   var CanClose: Boolean);
begin
  if ApplyBtn.Visible then
  begin
    case MessageDlg('The current graph''s color data has been altered.'+
              #13#10'Do you wish to save the alterations ?', mtConfirmation,
                    [mbYes, mbNo, mbCancel], 0) of
    mrYes: ApplyBtnClick(Sender);
 mrCancel: begin
             CanClose := False;
             Exit;
           end;
    end;
  end;
end;

procedure TPlotColorsForm.FormKeyDown(Sender: TObject; var Key: Word;
                                       Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

procedure TPlotColorsForm.FormShow(Sender: TObject);
begin
  UpperLowerLock.Checked := False;
  ShowPlotColorData;
  ApplyBtn.Visible := False;
end;

procedure TPlotColorsForm.UpperRedChange(Sender: TObject);
begin
  PlotData.UpperColor.X := UpperRed.Position/225;
  if UpperLowerLock.Checked then LowerRed.Position := UpperRed.Position;
  ApplyBtn.Visible := True;
end;

procedure TPlotColorsForm.UpperGreenChange(Sender: TObject);
begin
  PlotData.UpperColor.Y := UpperGreen.Position/225;
  if UpperLowerLock.Checked then LowerGreen.Position := UpperGreen.Position;
  ApplyBtn.Visible := True;
end;

procedure TPlotColorsForm.UpperLowerLockClick(Sender: TObject);
begin
  if UpperLowerLock.Checked then
  begin
    LowerRed.Position := UpperRed.Position;
    LowerGreen.Position := UpperGreen.Position;
    LowerBlue.Position := UpperBlue.Position;
    LowerAlpha.Position := UpperAlpha.Position;
  end;
end;

procedure TPlotColorsForm.UpperBlueChange(Sender: TObject);
begin
  PlotData.UpperColor.Z := UpperBlue.Position/225;
  if UpperLowerLock.Checked then LowerBlue.Position := UpperBlue.Position;
  ApplyBtn.Visible := True;
end;

procedure TPlotColorsForm.UpperAlphaChange(Sender: TObject);
begin
  PlotData.UpperColor.W := UpperAlpha.Position/1000;
  if UpperLowerLock.Checked then LowerAlpha.Position := UpperAlpha.Position;
  ApplyBtn.Visible := True;
end;

procedure TPlotColorsForm.LowerRedChange(Sender: TObject);
begin
  PlotData.LowerColor.X := LowerRed.Position/225;
  if UpperLowerLock.Checked then UpperRed.Position := LowerRed.Position;
  ApplyBtn.Visible := True;
end;

procedure TPlotColorsForm.LowerGreenChange(Sender: TObject);
begin
  PlotData.LowerColor.Y := LowerGreen.Position/225;
  if UpperLowerLock.Checked then UpperGreen.Position := LowerGreen.Position;
  ApplyBtn.Visible := True;
end;

procedure TPlotColorsForm.LowerBlueChange(Sender: TObject);
begin
  PlotData.LowerColor.Z := LowerBlue.Position/225;
  if UpperLowerLock.Checked then UpperBlue.Position := LowerBlue.Position;
  ApplyBtn.Visible := True;
end;

procedure TPlotColorsForm.LowerAlphaChange(Sender: TObject);
begin
  PlotData.LowerColor.W := LowerAlpha.Position/1000;
  if UpperLowerLock.Checked then UpperAlpha.Position := LowerAlpha.Position;
  ApplyBtn.Visible := True;
end;

    { Public declarations }
procedure TPlotColorsForm.ShowPlotColorData;
begin
  UpperRed.Position := round(PlotData.UpperColor.X*255);
  UpperGreen.Position := round(PlotData.UpperColor.Y*255);
  UpperBlue.Position := round(PlotData.UpperColor.Z*255);
  UpperAlpha.Position := round(PlotData.UpperColor.W*1000);

  LowerRed.Position := round(PlotData.LowerColor.X*255);
  LowerGreen.Position := round(PlotData.LowerColor.Y*255);
  LowerBlue.Position := round(PlotData.LowerColor.Z*255);
  LowerAlpha.Position := round(PlotData.LowerColor.W*1000);

  EditBlend.Text := FloatToStrF(PlotData.ColorBlend, ffGeneral, 7, 4);
  EditMove.Text := FloatToStrF(Plotdata.ColorMove, ffGeneral, 7, 4);
end;

end.
