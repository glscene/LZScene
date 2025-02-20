{: Dynamic sample for the ExtrusionSolid.<p>

   In this sample we extrude a complex solid made of an outer star-like contour
   and an inner square cutout that is moves around. The TGLExtrusionSolid takes
   care of the calculations, so all that is left is defining the contours
   (one in the FormCreate event, and the other in the Cadencer.Progress event).<p>

   Be aware that for TGLExtrusionSolid, like TGLMultiPolygon, the way you describe
   your polygons IS important:<ul>
   <li>the polygons must be in the X, Y plane
   <li>if all your polygons are defined in a counterclockwise manner, the first
       will define the solid outer, the second, third etc. will be the cutouts.
   </ul>
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLCadencer, GLScene, GLExtrusion, GLVectorGeometry, ExtCtrls, GLMultiPolygon,
  GLLCLViewer, GLCrossPlatform, GLCoordinates, GLBaseClasses;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    ExtrusionSolid: TGLExtrusionSolid;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
   i : Integer;
   r, x, y : Single;
const
   cSteps = 16;
begin
   // a small star contour
   with ExtrusionSolid.Contours do begin
      with Add.Nodes do for i:=0 to cSteps do begin
         r:=2+(i and 1)*2;
         SinCos(i*c2PI/cSteps, y, x);
         AddNode(x*r, y*r, 0);
      end;
      // add an empty contour for the square cutout (see progress event)
      Add;
   end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   x, y : Single;
begin
   // Make our Extrusion roll
   ExtrusionSolid.Roll(deltaTime*10);

   // At each frame, we drop the cutout and make a new.
   // Note that we could also have defined it once in the FormCreate and then moved
   // it around with the TGLNodes methods.
   SinCos(newTime, 2, y, x);
   with ExtrusionSolid.Contours do begin
      Items[1].Free;
      with Add.Nodes do begin
         AddNode(x-1, y-1, 0);
         AddNode(x+1, y-1, 0);
         AddNode(x+1, y+1, 0);
         AddNode(x-1, y+1, 0);
      end;
   end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   // Standard FPS counter
   Caption:=Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

end.