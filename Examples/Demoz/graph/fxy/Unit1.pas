{: Basic demo for the TGLHeightField and TGLXYZGrid objects.<p>

   HeightFields are used to materialize z=f(x, y) surfaces, you can use it to
   render anything from math formulas to statistics. Most important properties
   of an height field are its sampling scales (X & Y) that determine the extents
   and the resolution of the base grid.<p>

   The component will then invoke it OnGetHeight event to retrieve Z values for
   all of the grid points (values are retrieved only once for each point). Each
   point may have an additionnal color and texture coordinate.<p>

   Three XYZ grids are used to materialize planes, and have been colored to match
   the axis colors. Two controls have been provided to move the XY grid (blue)
   up or down, and center or bound-align the XZ and YZ grids.<p>

   The heightfield component takes care of all the tessellation, so there is not
   much in the code of the unit. Check the advanced "heightfield" sample for
   more dynamic uses.
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ComCtrls, GLObjects, GLGraph, GLScene, GLVectorGeometry, GLVectorTypes,
  GLLCLViewer, GLCrossPlatform, GLCoordinates;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    HeightField1: TGLHeightField;
    YZGrid: TGLXYZGrid;
    XZGrid: TGLXYZGrid;
    XYGrid: TGLXYZGrid;
    CBCentered: TCheckBox;
    TBXYPosition: TTrackBar;
    Label1: TLabel;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CBCenteredClick(Sender: TObject);
    procedure TBXYPositionChange(Sender: TObject);
    procedure HeightField1GetHeight(const x, y: Single; var z: Single;
      var color: TVector4f; var texPoint: TTexPoint);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.HeightField1GetHeight(const x, y: Single; var z: Single;
  var color: TVector4f; var texPoint: TTexPoint);
begin
   z:=VectorNorm(x, y);
   z:=cos(z*12)/(2*(z*6.28+1));
end;

procedure TForm1.CBCenteredClick(Sender: TObject);
begin
   if CBCentered.Checked then begin
      XZGrid.YSamplingScale.Origin:=0;
      YZGrid.XSamplingScale.Origin:=0;
   end else begin
      XZGrid.YSamplingScale.Origin:=-1;
      YZGrid.XSamplingScale.Origin:=-1;
   end;
end;

procedure TForm1.TBXYPositionChange(Sender: TObject);
begin
   XYGrid.ZSamplingScale.Origin:=-(TBXYPosition.Position/10);
end;

// following code takes care of camera movement, see camera & movement demos
// for explanations and more samples

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x; my:=y;
   end;
end;

end.
