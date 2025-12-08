{: Visibility culling demo/test/sample.<p>

   This sample is used to test and showcase the efficiency (or inefficiency) of
   visibility in various cases. Be aware that the sample may be slow loading
   (the same mesh is loaded multiple times to put some stress).<br>
   In each of the tests, a "square grid" of objects is created and made visible,
   the camera points at the center of the square, making most of the objects
   off-screen. Visibility culling detects that and does not render the off-screen
   or too-far away objects.<p>

   <ul>
   <li>Spheres: this is the default setting, and one in which culling is
      completely inefficient on a T&L board or good OpenGL ICD, mainly because
      the spheres are rendered with build lists that already have some visibility
      culling built-in. If culling is efficient for you in this case, well be
      happy it is, but start looking for a newer graphics board ;)
   <li>Actors: this one is culling friendly, and your framerate can more than
      double by choosing "ObjectBased" mode. This is due to the fact that the
      actor geometry must be resent at each frame, thus limiting T&L capability
      (the AGP stands in the way...). A culled object's geometry is not sent
      at all, and that can reduce the AGP and graphics driver load drastically.
   </ul>
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLObjects, GLCadencer, GLVectorFileObjects, ExtCtrls,
  StdCtrls, GLLCLViewer, GLTexture, GLCrossPlatform, GLMaterial,
  GLCoordinates, GLBaseClasses, GLRenderContextInfo;

type
  TForm1 = class(TForm)
    Viewer: TGLSceneViewer;
    RBNone: TRadioButton;
    RBObject: TRadioButton;
    RBHierarchical: TRadioButton;
    Label1: TLabel;
    GLScene: TGLScene;
    GLCadencer: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DCTarget: TGLDummyCube;
    Timer1: TTimer;
    DCSpheres: TGLDummyCube;
    DCActors: TGLDummyCube;
    Label2: TLabel;
    Panel1: TPanel;
    RBSpheres: TRadioButton;
    RBActors: TRadioButton;
    ACReference: TGLActor;
    GLMaterialLibrary: TGLMaterialLibrary;
    procedure GLCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RBNoneClick(Sender: TObject);
    procedure RBSpheresClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses GLFileMD2, GLUtils;

procedure TForm1.FormCreate(Sender: TObject);
var
   i, j : Integer;
   newSphere : TGLSphere;
   newActor : TGLActor;
begin
   SetGLSceneMediaDir();
   // Spheres are used as standalone, high-polycount objects
   // that are highly T&L friendly
   for i:=-4 to 4 do for j:=-4 to 4 do begin
      newSphere:=(DCSpheres.AddNewChild(TGLSphere) as TGLSphere);
      newSphere.Position.SetPoint(i*5, 0, j*5);
      newSphere.Slices:=32;
      newSphere.Stacks:=32;
   end;
   // Actors are used as standalone, med-polycount objects
   // that aren't T&L friendly (all geometry must be sent to
   // the hardware at each frame)
   GLMaterialLibrary.Materials[0].Material.Texture.Image.LoadFromFile('waste.jpg');
   ACReference.LoadFromFile('waste.md2');
   for i:=-3 to 3 do for j:=-3 to 3 do begin
      newActor:=(DCActors.AddNewChild(TGLActor) as TGLActor);
      newActor.Assign(ACReference);
      newActor.Position.SetPoint(i*10, 0, j*10);
      newActor.CurrentFrame:=(i+2)+(j+2)*5;
   end;
   ACReference.Visible:=False;
end;

procedure TForm1.RBSpheresClick(Sender: TObject);
begin
   DCSpheres.Visible:=RBSpheres.Checked;
   DCActors.Visible:=RBActors.Checked;
end;

procedure TForm1.GLCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   Viewer.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.1f FPS', [Viewer.FramesPerSecond]);
   Viewer.ResetPerformanceMonitor;
end;

procedure TForm1.RBNoneClick(Sender: TObject);
begin
   if RBObject.Checked then
      GLScene.VisibilityCulling:=vcObjectBased
   else if RBHierarchical.Checked then
      GLScene.VisibilityCulling:=vcHierarchical
   else GLScene.VisibilityCulling:=vcNone;
end;

end.
