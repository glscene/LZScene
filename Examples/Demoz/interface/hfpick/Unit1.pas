{: A crude but effective way of "picking" in an height field.<p>

   This demo implements a simple way to "pick" a clicked sector in an
   heightfield, using it for a basic "3D paint". In "paint" mode,
   the left button will paint in blue and the right one in red.
   The "rotate" mode allows you to move around the height field
   to continue your painting from a different angle.<br>
   No you can't save your art ;)<p>

   The picking is performed by getting the 3D coordinates from the
   2D mouse coordinates via PixelRayToWorld (which reads the depth
   buffer), and converting those absolute 3D coordinates to local
   coordinates of the HeightField, the last steps are then obvious.<br>
   This method is approximate in that its precision highly depends on
   that of the ZBuffer. It will also fail if some objects "obstructs"
   picking (prevent the read of a proper ZBuffer value).<p>

   Some simple improvements left as an exercice to the reader:<ul>
   <li>add a "terraform" mode allowing to raise and lower height values
   <li>add more colors to the paint mode
   <li>allow painting the top and bottom of the heightfield with different colors
   <li>painting to a texture instead of a grid
   </ul>
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLGraph, GLLCLViewer, GLVectorGeometry, GLVectorTypes,
  GLTexture, GLObjects, StdCtrls, ExtCtrls, GLColor, GLCrossPlatform,
  GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    GLCamera1: TGLCamera;
    HeightField: TGLHeightField;
    GLLightSource1: TGLLightSource;
    Panel1: TPanel;
    RBPaint: TRadioButton;
    RadioButton2: TRadioButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure HeightFieldGetHeight(const x, y: Single; var z: Single;
      var color: TVector4f; var texPoint: TTexPoint);
    procedure GLSceneViewerMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
    grid : array [-5..5, -5..5] of TColor;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
   ix, iy : Integer;
begin
   // initialize grid color to white/gray (checked pattern)
   for ix:=-5 to 5 do for iy:=-5 to 5 do
      if ((ix xor iy) and 1)=0 then
         grid[ix, iy]:=clWhite
      else grid[ix, iy]:=clSilver;
end;

procedure TForm1.HeightFieldGetHeight(const x, y: Single; var z: Single;
  var color: TVector4f; var texPoint: TTexPoint);
var
   ix, iy : Integer;
begin
   // Nothing fancy here, the color is directly taken from the grid,
   // and the z function is a basic cosinus. The '+0.01' are to take
   // rounding issues out of the equation.
   ix:=Round(ClampValue(x+0.01, -5, 5));
   iy:=Round(ClampValue(y+0.01, -5, 5));
   color:=ConvertWinColor(grid[ix, iy]);
   z:=Cos(VectorLength(x, y)*1.5);
end;

procedure TForm1.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   v : TAffineVector;
   ix, iy : Integer;
begin
   mx:=x;
   my:=y;
   if RBPaint.Checked then begin
      // In Paint mode
      // get absolute 3D coordinates of the point below the mouse
      v:=GLSceneViewer.Buffer.PixelRayToWorld(x, y);
      // convert to heightfield local coordinates
      v:=HeightField.AbsoluteToLocal(v);
      // convert that local coords to grid pos
      ix:=Round(v.X);
      iy:=Round(v.Y);
      // if we are in the grid...
      if (ix>=-5) and (ix<=5) and (iy>=-5) and (iy<=5) then begin
         // show last coord in the caption bar
         Caption:=Format('Last coord. : %d %d', [ix, iy]);
         // and paint blue or red depending on the button
         if Button=TMouseButton(mbLeft) then
            grid[ix, iy]:=clBlue
         else grid[ix, iy]:=clRed;
         // Height field changed, rebuild it!
         HeightField.StructureChanged;
      end;
   end;
end;

procedure TForm1.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if RBPaint.Checked then begin
      // in paint mode, paint if a button is pressed
      if ssLeft in Shift then
         GLSceneViewerMouseDown(Sender, TMouseButton(mbLeft), Shift, x, y)
      else if ssRight in Shift then
         GLSceneViewerMouseDown(Sender, TMouseButton(mbRight), Shift, x, y);
   end else begin
      // rotate mode
      if Shift<>[] then begin
         GLCamera1.MoveAroundTarget(my-y, mx-x);
         mx:=x;
         my:=y;
      end;
   end;
end;

end.
