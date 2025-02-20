unit AtomicRotationFrm;

{$MODE objfpc}{$H+}

{As the data is 'global' this changes it...
 The stage[1].rot_x,y,z is processed by the Cadencer
 rotating the Dummycubes (they are holding the Spheres)}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  GLObjects,
  ComCtrls, StdCtrls, ExtCtrls, Buttons;

type
  TAtomicRotationForm = class(TForm)
    AtomicLevelsRg: TRadioGroup;
    TrackBarX: TTrackBar;
    TrackBarY: TTrackBar;
    TrackBarZ: TTrackBar;
    ColorDialog1: TColorDialog;
    SpeedButton1: TSpeedButton;
    LevelLineCB: TCheckBox;
    procedure AtomicLevelsRgClick(Sender: TObject);
    procedure TrackBarXChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure LevelLineCBClick(Sender: TObject);
{    procedure AllIinesCBClick(Sender: TObject);}
  private
     
  public
     
  end;

var
  AtomicRotationForm: TAtomicRotationForm;
  LinesBusy:Boolean;
implementation

uses AtomsFrm;

{$R *.lfm}

procedure TAtomicRotationForm.FormCreate(Sender: TObject);
begin
  TrackBarX.Position:=Round(stage[1].rot_x*100);
  TrackBarY.Position:=Round(stage[1].rot_y*100);
  TrackBarZ.Position:=Round(stage[1].rot_z*100);
  LinesBusy:=False;
end;

procedure TAtomicRotationForm.AtomicLevelsRgClick(Sender: TObject);
begin
  If AtomicLevelsRg.ItemIndex = 7 then
  begin
    TrackBarX.Position:=Round(AAtomForm.WhiteLight.Position.X);
    TrackBarY.Position:=Round(AAtomForm.WhiteLight.Position.Y);
    TrackBarZ.Position:=Round(AAtomForm.WhiteLight.Position.Z);
  end else
  begin
    TrackBarX.Position:=Round(stage[AtomicLevelsRg.ItemIndex+1].rot_x*100);
    TrackBarY.Position:=Round(stage[AtomicLevelsRg.ItemIndex+1].rot_y*100);
    TrackBarZ.Position:=Round(stage[AtomicLevelsRg.ItemIndex+1].rot_z*100);
    If LevelLine then LevelLineLevel:= AtomicLevelsRg.ItemIndex+1;
  end;
end;

procedure TAtomicRotationForm.TrackBarXChange(Sender: TObject);
begin
  If AtomicLevelsRg.ItemIndex = 7 then
  begin
  AAtomForm.WhiteLight.Position.SetPoint(TrackBarX.Position,
                                         TrackBarY.Position,
                                         TrackBarZ.Position);
{  (TGLCoordinates)
    (AAtomForm.WhiteLight.Position.X):=TrackBarX.Position;
    (AAtomForm.WhiteLight.Position.Y):=TrackBarY.Position;
    (AAtomForm.WhiteLight.Position.Z):=TrackBarZ.Position;}
  end else
  begin
    stage[AtomicLevelsRg.ItemIndex+1].rot_x:=TrackBarX.Position/100;
    stage[AtomicLevelsRg.ItemIndex+1].rot_y:=TrackBarY.Position/100;
    stage[AtomicLevelsRg.ItemIndex+1].rot_z:=TrackBarZ.Position/100;
  end;
end;

procedure TAtomicRotationForm.SpeedButton1Click(Sender: TObject);
begin
  If (AtomicLevelsRg.ItemIndex< 7) then {do not paint light}
  if ColorDialog1.Execute then
  {CastColor(Leveled:Integer;NewColor:TColor); }
  AAtomForm.CastColor((AtomicLevelsRg.ItemIndex+1),ColorDialog1.Color);
end;

procedure TAtomicRotationForm.LevelLineCBClick(Sender: TObject);
{var i:Integer;}
begin
  If (not LinesBusy) then begin
  LinesBusy:=True;
  If (AtomicLevelsRg.ItemIndex< 7) then
  begin
  LevelLineLevel:= AtomicLevelsRg.ItemIndex+1;
  LevelLine := (not LevelLine);
  LevelLineCB.Checked :=LevelLine;
  MaxLines:=36;
  end;
  If (LevelLine=False) then
  begin
    LevelLineLevel:=0;
    MaxLines:=36;
    AAtomForm.DoLevelLineLevel;
     //method to clear lines... changed to move to inside,
     //this broke program whenever lines were turned on again
     // another way would be to make a 1st point again, as in create.
    {For i:=0 to AAtomForm.Lines1.Nodes.Count-1 do
                AAtomForm.Lines1.Nodes[0].Free;
     Lines1.AddNode(0, 0, 0);            }
  end;
  LinesBusy:=False;
  end;
end;

// Tried to make ALL levels have lines...
// will require having 7 line objects
// put off till turned into an Atom class
// with each level having a Line, Color, ...
{procedure TAtomicRotationForm.AllIinesCBClick(Sender: TObject);
begin
  If (AtomicLevelsRg.ItemIndex< 7) then
  If (LevelLine) then
  begin
    LevelLineLevel:= 9;
    MaxLines:=156;
  end;
end;  }

end.
