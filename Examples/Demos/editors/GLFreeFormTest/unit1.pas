unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,
  // GLScene
  GLScene,  GLBaseClasses, GLCrossPlatform, GLPersistentClasses, GLState, OpenGLTokens, GLLCLViewer,
  GLCoordinates, GLObjects, GLGeomObjects,
  GLVectorTypes, GLVectorGeometry, GLVectorLists, GLVectorFileObjects,
  GLTexture, GLMaterial, GLCadencer,
  GLMeshOptimizer, GLHiddenLineShader, GLMeshObjectHelper,uNavCube;// GLFreeFormEx;


Type
  TGLCameraMode =(cmFree,cmTop,cmBottom,cmLeft,cmRight,cmFront,cmBack);
Type
  TGLSceneEditingHelper = class(TComponent)
    private
      FMouseInViewer : Boolean;
      FGLSceneViewer : TGLSceneViewer;
      FCameraTarget : TGLCustomSceneObject ;
      FCameraMode : TGLCameraMode;

      procedure setGLSceneViewer(AValue:TGLSceneViewer);

    protected
      lastMouseWorldPos : TVector;
      mx, my : Integer;

      procedure DoMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
      procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure DoMouseMove(Sender: TObject; Shift: TShiftState;X, Y: Integer);
      procedure DoMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure DoMouseEnter(Sender: TObject);
      procedure DoMouseLeave(Sender: TObject);


    public
      constructor Create(AOwner: TComponent);override;
      function MouseWorldPos(const X, Y: Integer; isy: boolean = false): TVector;

      property Viewer : TGLSceneViewer Read FGLsceneViewer Write setGLsceneViewer;
      property CameraTarget : TGLCustomSceneObject read FCameraTarget write FCameraTarget;
      property CameraMode : TGLCameraMode Read FCameraMode write FCameraMode;
      property MouseInViewer : Boolean read FMouseInViewer;
  end;

type

  { TForm1 }
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    ComboBox1: TComboBox;
    DCHelpers: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    DCWorld: TGLDummyCube;
    DCCameraTarget: TGLDummyCube;
    GLHiddenLineShader1: TGLHiddenLineShader;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);

      procedure GLMaterialLibrary1TextureNeeded(Sender: TObject; var textureFileName: string);
    procedure BuildTorus(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    ff: TGLFreeForm;
    SceneHelper : TGLSceneEditingHelper;
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  ncube : TGLNavCube;

implementation

{$R *.lfm}

uses math;


{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);

begin
  ff:=TGLFreeForm.CreateAsChild(DCWorld);


  ff.MaterialLibrary := GLMaterialLibrary1;
  ff.Material.LibMaterialName := 'LibMaterial';

  SceneHelper:=TGLSceneEditingHelper.Create(self);
  sceneHelper.Viewer := GLSceneViewer1;
  sceneHelper.CameraTarget := DCCameraTarget;
  sceneHelper.CameraMode := cmFree;

  //cad
  GLCadencer1.Enabled:= true;
  glsceneviewer1.Enabled := true;
  ncube := TGLNavCube.CreateAsChild(DCHelpers);//(GLScene1.Objects);
  ncube.SceneViewer := glsceneviewer1;
  ncube.ActiveMouse := true;
  ncube.Camera:=GLCamera1;
  ncube.FPS := 30;



end;

procedure TForm1.Button1Click(Sender: TObject);
var mo:TGLCustomEditingMeshObject;
begin
 // ff.BuildMeshCube(2,2,2,[cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight]);

  mo:=TGLCustomEditingMeshObject.Create;
  mo.MakeCube(2,2,2);
  mo.FaceGroups.Items[0].MaterialName := 'LibMaterial';
  //makeCylinder(10);
  //.BuildGrid(10,10);
  //BuildCube(2,2,2,[cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight]);
  ff.MeshObjects.Add(mo);

  ff.StructureChanged;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.Button2Click(Sender: TObject);
var mo:TGLCustomEditingMeshObject;
begin
  mo:=TGLCustomEditingMeshObject.Create;
  mo.MakeSphere(2,16,16,true);
 // mo.SetMaterial('LibMaterial');
  ff.MeshObjects.Add(mo);


  ff.StructureChanged;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.Button3Click(Sender: TObject);
var mo:TGLCustomEditingMeshObject;
begin
  mo:=TGLCustomEditingMeshObject.Create;
  mo.MakeCylinderEx(3,2,4,16,32,false,true);
  mo.FaceGroups.Items[0].MaterialName := 'LibMaterial';
  ff.MeshObjects.Add(mo);
  ff.StructureChanged;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.Button4Click(Sender: TObject);
var mo:TGLCustomEditingMeshObject;
begin
  mo:=TGLCustomEditingMeshObject.Create;
  mo.MakeGrid(3,3,4,4);
  mo.FaceGroups.Items[0].MaterialName := 'LibMaterial';
  ff.MeshObjects.Add(mo);
  ff.StructureChanged;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
//  ff.Optimize(vDefaultMeshOptimizerOptions);
  ff.StructureChanged;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.GLMaterialLibrary1TextureNeeded(Sender: TObject; var textureFileName: string);
begin

end;

procedure TForm1.BuildTorus(Sender: TObject);
var mo:TGLCustomEditingMeshObject;
begin
  mo:=TGLCustomEditingMeshObject.Create;
  mo.MakeTorus(2,3,16,16);
  ff.MeshObjects.Add(mo);
  ff.StructureChanged;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  Case Combobox1.ItemIndex of
    2: ff.Material.PolygonMode := pmFill;
    0: ff.Material.PolygonMode := pmLines;
    1: ff.Material.PolygonMode := pmPoints;
  end;
end;

Constructor TGLSceneEditingHelper.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TGLSceneEditingHelper.setGLSceneViewer(AValue:TGLSceneViewer);
begin
  if FGLSceneViewer = AValue then exit;
  FGLSceneViewer := AValue;
  FGLSceneViewer.OnMouseEnter := @DoMouseEnter;
  FGLSceneViewer.OnMouseLeave := @DoMouseLeave;
  FGLSceneViewer.OnMouseDown := @DoMouseDown;
  FGLSceneViewer.OnMouseUp := @DoMouseUp;
  FGLSceneViewer.OnMouseMove := @DoMouseMove;
  FGLSceneViewer.OnMouseWheel := @DoMouseWheel;
end;

function TGLSceneEditingHelper.MouseWorldPos(const X, Y: Integer; isy: boolean = false): TVector;
var
  v: TVector;
  InvertedY: Integer;
begin

  InvertedY := FGLSceneViewer.Height - Y;

  SetVector(v, X, InvertedY, 0);
 if not isy then
  FGLSceneViewer.Buffer.ScreenVectorIntersectWithPlaneXZ
   (v, FCameraTarget.AbsolutePosition.y, Result)
   else
     FGLSceneViewer.Buffer.ScreenVectorIntersectWithPlaneXY
       (v, FCameraTarget.AbsolutePosition.z, Result)
end;

procedure TGLSceneEditingHelper.DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := x;
  my := y;
  lastMouseWorldPos := MouseWorldPos(x,y, (ssCtrl in Shift));
end;

procedure TGLSceneEditingHelper.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var dx,dy : single;
   // newPos, v: TVector;
begin

  // Rotation uniquement en mode "Free"
  if (ssMiddle in Shift) and (FCameraMode = cmFree) then
	begin
    dx := -(mx - x)*0.25;
    dy := (my - y)*0.25;
	  FGLSceneViewer.Camera.MoveAroundTarget(dy, dx);
	end;

  // Deplacements
  if (ssRight in Shift) and (ssCtrl in Shift) then
  begin
    dx := -(mx - x)*0.1;
    dy := (my - y)*0.1;
    Case FCameraMode of

      cmFront,cmBack :
        begin
          FGLSceneViewer.Camera.MoveInEyeSpace(0,dx,dy);
          FGLSceneViewer.Camera.MoveTargetInEyeSpace(0,dx,dy);
        end;
      cmLeft,cmRight :
        begin
          FGLSceneViewer.Camera.MoveInEyeSpace(dx,0,dy);
          FGLSceneViewer.Camera.MoveTargetInEyeSpace(dx,0,dy);
        end;
      cmTop,cmBottom :
        begin
          FGLSceneViewer.Camera.MoveInEyeSpace(dy,dx,0);
          FGLSceneViewer.Camera.MoveTargetInEyeSpace(dy,dx,0);
        end;
      cmFree :
        begin
          if (ssShift in Shift) then
          begin
            FGLSceneViewer.Camera.MoveInEyeSpace(dy,0,0);
            FGLSceneViewer.Camera.MoveTargetInEyeSpace(dy,0,0);
          end
          else
          begin
            FGLSceneViewer.Camera.MoveInEyeSpace(0,dx,dy);
            FGLSceneViewer.Camera.MoveTargetInEyeSpace(0,dx,dy);
          end;
        end;
    end;
    lastMouseWorldPos := MouseWorldPos(x,y, not((ssCtrl in Shift)));
  end;
  mx:=x;
  my:=y;
end;

procedure TGLSceneEditingHelper.DoMouseWheel(Sender: TObject;Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;var Handled: Boolean);
begin
   FGLSceneViewer.Camera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TGLSceneEditingHelper.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 // lastMouseWorldPos := MouseWorldPos(x,y, (ssCtrl in Shift));
end;

procedure TGLSceneEditingHelper.DoMouseEnter(Sender: TObject);
begin
  FMouseInViewer := True;
end;

procedure TGLSceneEditingHelper.DoMouseLeave(Sender: TObject);
begin
  FMouseInViewer := False;
end;

end.

