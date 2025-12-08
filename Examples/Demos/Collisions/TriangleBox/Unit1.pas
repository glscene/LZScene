{
  TriangleBoxIntersect Demo

  History:
  29/01/07 - DaStr - Initial version (by dikoe Kenguru)
}

unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLObjects, GLCadencer, GLVectorFileObjects, ExtCtrls,
  StdCtrls, GLLCLViewer, GLVectorGeometry, GLGraph, GLGeomObjects,
  GLCrossPlatform, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    Viewer: TGLSceneViewer;
    GLScene: TGLScene;
    GLCadencer: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLLightSource2: TGLLightSource;
    DCCamTarget: TGLDummyCube;
    Panel2: TPanel;
    GLCube1: TGLCube;
    GLXYZGrid1: TGLXYZGrid;
    GLLines1: TGLLines;
    CheckBox6: TCheckBox;
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Button2: TButton;
    GLPolygon1: TGLPolygon;
    CheckBox4: TCheckBox;
    GLPoints1: TGLPoints;
    CheckBox5: TCheckBox;
    GLLines2: TGLLines;
    procedure GLCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Button2Click(Sender: TObject);
    procedure MakeRandomData;
    procedure Button1Click(Sender: TObject);
    procedure DrawResult;
    procedure CheckBox4Click(Sender: TObject);
  private
    mdx, mdy : Integer;
    BoxPos, BoxScale,
    MinExtend, MaxExtend : TAffineVector;
    TriangePos           : array [0..2] of TAffineVector;
  public
  end;

var
  Form1 : TForm1;

implementation

{$R *.lfm}

const
  SizePos   = 10;
  ScaleSize = 3;
  TrigRect  = 10;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  BoxScale := XYZVector;
end;

procedure TForm1.MakeRandomData;
var
  i : Integer;
begin
    // Change position.
  if CheckBox1.Checked then
    BoxPos   := AffineVectorMake( Random*SizePos -SizePos/2,
                                  Random*SizePos -SizePos/2,
                                  Random*SizePos -SizePos/2  );
    // Change scale.
  if CheckBox2.Checked then
    BoxScale := AffineVectorMake( Random*ScaleSize +ScaleSize/2,
                                  Random*ScaleSize +ScaleSize/2,
                                  Random*ScaleSize +ScaleSize/2 );
    // Change triange.
  if CheckBox3.Checked then
    for i := 0 to 2 do
      TriangePos[i] := AffineVectorMake( Random*TrigRect -TrigRect/2,
                                         Random*TrigRect -TrigRect/2,
                                         Random*TrigRect -TrigRect/2 );
   // Calc extends.
  MinExtend := VectorSubtract(BoxPos, VectorScale(BoxScale, 0.5));
  MaxExtend := VectorAdd(     BoxPos, VectorScale(BoxScale, 0.5));
end;

procedure TForm1.DrawResult;
var
  i : Integer;
begin
  GLPolygon1.Nodes.Clear;
  GLPolygon1.AddNode(TriangePos[0]);
  GLPolygon1.AddNode(TriangePos[1]);
  GLPolygon1.AddNode(TriangePos[2]);

  GLPoints1.Positions.Clear;
  GLPoints1.Colors.Add(1, 0, 0, 1);
  for i := 0 to 2 do
    GLPoints1.Positions.Add(TriangePos[i]);

  GLLines2.Nodes.Clear;
  GLLines2.Nodes.AddNode(TriangePos[0]);
  GLLines2.Nodes.AddNode(TriangePos[1]);
  GLLines2.Nodes.AddNode(TriangePos[2]);
  GLLines2.Nodes.AddNode(TriangePos[0]);

  DCCamTarget.Position.SetPoint(BoxPos);
  DCCamTarget.Scale.SetVector(BoxScale);
  GLCube1.Position.SetPoint(BoxPos);
  GLCube1.Scale.SetVector(BoxScale);
end;



  // Find next with intersection
procedure TForm1.Button1Click(Sender: TObject);
var
  IterCnt : Integer;
  Res1    : Boolean;
begin
  IterCnt := 0;
  repeat
    MakeRandomData;
    Res1 :=  IntersectTriangleBox( TriangePos[0], TriangePos[1], TriangePos[2],
                                   MinExtend, MaxExtend);
    IterCnt := IterCnt +1;
    if IterCnt >= 10000 then begin
      DrawResult;
      ShowMessage('Intersection not found!');
      exit;
    end;
  until Res1;
  DrawResult;
end;


  // Find next without intersection.
procedure TForm1.Button2Click(Sender: TObject);
var
  IterCnt : Integer;
  Res1    : Boolean;
begin
  IterCnt := 0;
  repeat
    MakeRandomData;
    Res1 :=  IntersectTriangleBox( TriangePos[0], TriangePos[1], TriangePos[2],
                                   MinExtend, MaxExtend);
    IterCnt := IterCnt +1;
    if IterCnt >= 10000 then begin
      DrawResult;
      ShowMessage('Intersection not found!');
      exit;
    end;
  until not Res1;
  DrawResult;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  GLCube1.Visible    := CheckBox4.Checked;
  GLXYZGrid1.Visible := CheckBox6.Checked;
  GLLines1.Visible   := CheckBox5.Checked;
end;
 
procedure TForm1.GLCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if Form1.Active then Viewer.Invalidate
end;

procedure TForm1.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Shift = [ssLeft] then GLCamera1.MoveAroundTarget(mdy -y, mdx -x);
  mdx := x;
  mdy := y;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.02, WheelDelta/120));
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  GLCamera1.FocalLength := MinInteger(Height, Width) / 10;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then close;
end;

end.