{: Basic interactive object picking<p>

  This is a bare bones sample on the use of the GetPickedObject function.
  Two events are handled : OnMouseMove triggers a color change (grey/red) when
  the mouse is moved over an object, and a message popups when an object is
  clicked in OnMouseDown.<p>

  In a real world proggie, both events should make use of the oldPick variable
  (since you can't click what is not under the mouse, the GetPickedObject in
  OnMouseDown returns what we already have in oldPick, set during the last
  OnMouseMove).
}
unit Unit1;

interface

uses
  Forms, GLScene, GLObjects, GLTexture, Classes, Controls, Dialogs,
  GLLCLViewer, GLGeomObjects, GLColor, GLCrossPlatform, GLCoordinates,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    Sphere: TGLSphere;
    Cylinder: TGLCylinder;
    Torus: TGLTorus;
    Cone: TGLCone;
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    { Déclarations privées }
    oldPick: TGLCustomSceneObject;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  pick: TGLCustomSceneObject;
begin
  // find what's under the mouse
  pick := (GLSceneViewer1.Buffer.GetPickedObject(x, y) as TGLCustomSceneObject);
  // if it has changed since last MouseMove...
  if (pick <> oldPick) then
  begin
    // ...turn to black previous "hot" object...
    if Assigned(oldPick) then
      oldPick.Material.FrontProperties.Emission.Color := clrBlack;
    // ...and heat up the new selection...
    if Assigned(pick) then
      pick.Material.FrontProperties.Emission.Color := clrRed;
    // ...and don't forget it !
    oldPick := pick;
  end;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  pick: TGLCustomSceneObject;
begin
  // if an object is picked...
  pick := (GLSceneViewer1.Buffer.GetPickedObject(x, y) as TGLCustomSceneObject);
  if Assigned(pick) then
  begin
    // ...turn it to yellow and show its name
    pick.Material.FrontProperties.Emission.Color := clrYellow;
    ShowMessage('You clicked the ' + pick.Name);
  end;
end;

end.
