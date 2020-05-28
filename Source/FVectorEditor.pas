//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Editor for a vector.
}

unit FVectorEditor;

interface

{$I GLScene.inc}

uses  
  lresources,
  Classes, 
  SysUtils,
  Forms, 
  StdCtrls, 
  ExtCtrls, 
  Buttons, 
  Graphics, 
  Controls,
  GLVectorGeometry, 
  GLUtils;


type
  TVectorEditorForm = class(TForm)
    EDx: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EDy: TEdit;
    EDz: TEdit;
    BBok: TBitBtn;
    BBcancel: TBitBtn;
    IMx: TImage;
    IMy: TImage;
    IMz: TImage;
    SpeedButton1: TSpeedButton;
    SBmX: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SBmY: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SBmZ: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SBUnit: TSpeedButton;
    SpeedButton9: TSpeedButton;
    Bevel1: TBevel;
    SBInvert: TSpeedButton;
    procedure TBxClick(Sender: TObject);
    procedure TByClick(Sender: TObject);
    procedure TBzClick(Sender: TObject);
    procedure TBnullClick(Sender: TObject);
    procedure EDxChange(Sender: TObject);
    procedure EDyChange(Sender: TObject);
    procedure EDzChange(Sender: TObject);
    procedure SBmXClick(Sender: TObject);
    procedure SBmYClick(Sender: TObject);
    procedure SBmZClick(Sender: TObject);
    procedure SBUnitClick(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SBInvertClick(Sender: TObject);
  private
    vx, vy, vz : Single;
    procedure TestInput(edit : TEdit; imError : TImage; var dest : Single);
  public
    function Execute(var x, y, z : Single) : Boolean;
  end;

function VectorEditorForm : TVectorEditorForm;
procedure ReleaseVectorEditorForm;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
  vVectorEditorForm : TVectorEditorForm;

function VectorEditorForm : TVectorEditorForm;
begin
	if not Assigned(vVectorEditorForm) then
      vVectorEditorForm:=TVectorEditorForm.Create(nil);
	Result:=vVectorEditorForm;
end;

procedure ReleaseVectorEditorForm;
begin
	if Assigned(vVectorEditorForm) then begin
	   vVectorEditorForm.Free; vVectorEditorForm:=nil;
	end;
end;

// Execute
//
function TVectorEditorForm.Execute(var x, y, z : Single) : Boolean;
begin
   // setup dialog fields
   vx:=x;
   vy:=y;
   vz:=z;
   EDx.Text:=FloatToStr(vx);
   EDy.Text:=FloatToStr(vy);
   EDz.Text:=FloatToStr(vz);
   // show the dialog
   Result:=(ShowModal=mrOk);
   if Result then begin
      x:=vx;
      y:=vy;
      z:=vz;
   end;
end;

procedure TVectorEditorForm.TestInput(edit : TEdit; imError : TImage; var dest : Single);
begin
   if Visible then begin
      try
         dest:=StrToFloat(edit.Text);
         imError.Visible:=False;
      except
         imError.Visible:=True;
      end;
      BBOk.Enabled:=not (IMx.Visible or IMy.Visible or IMz.Visible);
   end;
end;

procedure TVectorEditorForm.TBxClick(Sender: TObject);
begin
   EDx.Text:='1'; EDy.Text:='0'; EDz.Text:='0';
end;

procedure TVectorEditorForm.TByClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='1'; EDz.Text:='0';
end;

procedure TVectorEditorForm.TBzClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='0'; EDz.Text:='1';
end;

procedure TVectorEditorForm.TBnullClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='0'; EDz.Text:='0';
end;

procedure TVectorEditorForm.EDxChange(Sender: TObject);
begin
   TestInput(EDx, IMx, vx);
end;

procedure TVectorEditorForm.EDyChange(Sender: TObject);
begin
   TestInput(EDy, IMy, vy);
end;

procedure TVectorEditorForm.EDzChange(Sender: TObject);
begin
   TestInput(EDz, IMz, vz);
end;

procedure TVectorEditorForm.SBmXClick(Sender: TObject);
begin
   EDx.Text:='-1'; EDy.Text:='0'; EDz.Text:='0';
end;

procedure TVectorEditorForm.SBmYClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='-1'; EDz.Text:='0';
end;

procedure TVectorEditorForm.SBmZClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='0'; EDz.Text:='-1';
end;

procedure TVectorEditorForm.SBUnitClick(Sender: TObject);
begin
   EDx.Text:='1'; EDy.Text:='1'; EDz.Text:='1';
end;

procedure TVectorEditorForm.SpeedButton9Click(Sender: TObject);
var
   v : TAffineVector;
begin
   SetVector(v, GLUtils.StrToFloatDef(EDx.Text, 0), GLUtils.StrToFloatDef(EDy.Text, 0), GLUtils.StrToFloatDef(EDz.Text, 0));
   if VectorLength(v)=0 then
      v:=NullVector
   else NormalizeVector(v);
   EDx.Text:=FloatToStr(v.V[0]);
   EDy.Text:=FloatToStr(v.V[1]);
   EDz.Text:=FloatToStr(v.V[2]);
end;

procedure TVectorEditorForm.SBInvertClick(Sender: TObject);
var
   v : TAffineVector;
begin
   SetVector(v, GLUtils.StrToFloatDef(EDx.Text, 0), GLUtils.StrToFloatDef(EDy.Text, 0), GLUtils.StrToFloatDef(EDz.Text, 0));
   NegateVector(v);
   EDx.Text:=FloatToStr(v.V[0]);
   EDy.Text:=FloatToStr(v.V[1]);
   EDz.Text:=FloatToStr(v.V[2]);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
  {$i FVectorEditor.lrs}
finalization

   ReleaseVectorEditorForm;

end.



