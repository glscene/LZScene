{: A basic sample to demonstrate how transparency & Z-buffer work/fight together.<p>

   In this sample, only the sphere are transparent. The form has a few options
   that allow to adjust in which order objects are rendered, and what kind of
   transparency is used.<p>

   Transparency in GLScene is activated by setting Material.Blending to either
   'bmTransparency' or 'bmAdditive', AND giving a <1.0 value to the Diffuse
   color alpha channel (alpha = 0.0 means absolute transparency, alpha = 1.0
   means absolute opacity).<p>

   How do Z-Buffer & transparency work ? When point has to be rendered, OpenGL
   first checks its distance to the camera, the "Z" axis. If the distance
   check is successfull (the new point is closer), it is rendered, and if
   the point is part of a "transparent" object, then OpenGL will mix the existing
   point's color with our new point's color. If the Z check fails, OpenGL doesn't
   even bother about checking transparency.<p>

   This is why, if you want to render transparent objects, you must make sure
   you render the farthest objects first, to give transparency a chance.
   However this effect can be usefull if you want to render mixed, half-transparent
   half-opaque objects.<p>

   They are two ways to order objects in GLScene :<ul>
   <li>ordering : can be done at design-time in the editor or at runtime with
      MoveUp/MoveDown methods
   <li>sorting : adjust the ObjectSorting property (see help for more details)
   </ul>
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLObjects, StdCtrls, GLCadencer, GLBehaviours, GLTexture,
  GLVectorGeometry, GLLCLViewer, GLGeomObjects, GLCoordinates, GLCrossPlatform,
  GLBaseClasses, GLMaterial, GLRenderContextInfo;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Cone1: TGLCone;
    DCCentral: TGLDummyCube;
    CentralSphere: TGLSphere;
    Torus1: TGLTorus;
    Label1: TLabel;
    RBSTC: TRadioButton;
    RBTSC: TRadioButton;
    RBTCS: TRadioButton;
    OrbitingSphere1: TGLSphere;
    GLCadencer1: TGLCadencer;
    Label2: TLabel;
    Label3: TLabel;
    CBSorting: TCheckBox;
    CBAdditive: TCheckBox;
    BaseDummyCube: TGLDummyCube;
    OrbitingSphere2: TGLSphere;
    procedure RBSTCClick(Sender: TObject);
    procedure RBTSCClick(Sender: TObject);
    procedure RBTCSClick(Sender: TObject);
    procedure CBAdditiveClick(Sender: TObject);
    procedure CBSortingClick(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.RBSTCClick(Sender: TObject);
begin
   // we have 3 objects, move up twice and we're on the top !
   CentralSphere.MoveUp;
   CentralSphere.MoveUp;
end;

procedure TForm1.RBTSCClick(Sender: TObject);
begin
   // we have 3 objects, move down twice and we're on the top,
   // then once down, we're in the middle !
   CentralSphere.MoveUp;
   CentralSphere.MoveUp;
   CentralSphere.MoveDown;
end;

procedure TForm1.RBTCSClick(Sender: TObject);
begin
   // we have 3 objects, move down twice and we're on the bottom !
   CentralSphere.MoveDown;
   CentralSphere.MoveDown;
end;

procedure TForm1.CBAdditiveClick(Sender: TObject);
begin
   // adjust blending mode for both orbiting spheres
   if CBAdditive.Checked then
      OrbitingSphere1.Material.BlendingMode:=bmAdditive
   else OrbitingSphere1.Material.BlendingMode:=bmTransparency;
   OrbitingSphere2.Material.BlendingMode:=OrbitingSphere1.Material.BlendingMode;
end;

procedure TForm1.CBSortingClick(Sender: TObject);
begin
   // adjust sorting on the parent object
   if CBSorting.Checked then
      BaseDummyCube.ObjectsSorting:=osRenderFarthestFirst
   else BaseDummyCube.ObjectsSorting:=osNone;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   alpha : Double;
begin
   // move the spheres
   alpha:=DegToRad(newTime*60);
   OrbitingSphere1.Position.SetPoint(1.5*cos(alpha), 1.5*sin(alpha), 1.5*sin(alpha));
   alpha:=alpha+PI/2;
   OrbitingSphere2.Position.SetPoint(1.5*cos(alpha), 1.5*sin(alpha), 1.5*sin(alpha));
end;

end.
