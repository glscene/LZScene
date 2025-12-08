{: Demo of Constructive Solid Geometry in GLScene.

   Its kept very simple, you can use mouse to rotate view(drag)
   and mousewheel to zoom/unzoom.<p>

   The CSG system uses BSP to optimize what triangles it considers.
   Its kept on a mesh basis to simplyfy things, it allways generates new BSP's,
   even the meshes allready had BSP optimization.

   The demo uses the polyhedron.3ds, resource from the GLScene pack.

   Author: Joen Joensen.
   Contributed to the GLScene community.

   Features: CSG_Union, CSG_Subtraction, CSG_Intersection.

	<b>History : </b><font size=-1><ul>
      <li>29/11/03 - JAJ - Sometimes a single tri is messed up...
                          (often(1/3) happends on 2 triangles in this demo when using intersection)
	</ul></font>

	<b>History : </b><font size=-1><ul>
      <li>29/11/03 - JAJ - Created and Submitted to GLScene.
	</ul></font>
}
unit MainFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLVectorFileObjects, StdCtrls, GLBSP, GLMeshCSG, GLLCLViewer,
  GLObjects, GLTexture, GLFile3ds, ExtCtrls, ComCtrls, GLCrossPlatform,
  GLMaterial, GLCoordinates, GLBaseClasses, GLState, GLSimpleNavigation;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLFreeForm1: TGLFreeForm;
    GLCamera1: TGLCamera;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLFreeForm2: TGLFreeForm;
    GLFreeForm3: TGLFreeForm;
    GLLightSource1: TGLLightSource;
    GLSimpleNavigation1: TGLSimpleNavigation;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CheckBox1: TCheckBox;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
// Demo starts here above is just navigation.
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    mx : Integer;
    my : Integer;
    Drag : Boolean;
  end;

var
  Form1: TForm1;

implementation

uses GLVectorGeometry, GLUtils;

{$R *.lfm}

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Drag := true;
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Drag := False;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  If drag then
  Begin
    GLCamera1.MoveAroundTarget(my-y,mx-x);
  End;
  mx := x;
  my := y;
end;

procedure TForm1.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(1.1);
end;

procedure TForm1.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(1/1.1);
end;

// Demo starts here above is just navigation.
           
procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  // scaled 40
  GLFreeForm1.LoadFromFile('polyhedron.3ds');

  // scaled 20, position.x = 16
  GLFreeForm2.LoadFromFile('polyhedron.3ds');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  GLFreeForm3.MeshObjects.Clear;
  GLFreeForm3.StructureChanged;

  GLFreeForm1.Material.PolygonMode := pmFill;
  GLFreeForm2.Material.PolygonMode := pmFill;
end;

procedure TForm1.Button2Click(Sender: TObject);
Var
  Mesh : TGLMeshObject;
begin
  GLFreeForm3.MeshObjects.Clear;

  If GLFreeForm3.MeshObjects.Count = 0 then
    TGLMeshObject.CreateOwned(GLFreeForm3.MeshObjects).Mode := momFaceGroups;

  Mesh := GLFreeForm3.MeshObjects[0];

  CSG_Operation(GLFreeForm1.MeshObjects.Items[0],GLFreeForm2.MeshObjects.Items[0],CSG_Union,Mesh,'1','2');
  GLFreeForm3.StructureChanged;

  GLFreeForm1.Material.PolygonMode := pmLines;
  GLFreeForm2.Material.PolygonMode := pmLines;
end;

procedure TForm1.Button3Click(Sender: TObject);
Var
  Mesh : TGLMeshObject;
begin
  GLFreeForm3.MeshObjects.Clear;

  If GLFreeForm3.MeshObjects.Count = 0 then
    TGLMeshObject.CreateOwned(GLFreeForm3.MeshObjects).Mode := momFaceGroups;

  Mesh := GLFreeForm3.MeshObjects[0];

  CSG_Operation(GLFreeForm1.MeshObjects.Items[0],GLFreeForm2.MeshObjects.Items[0],CSG_Subtraction,Mesh,'1','2');
  GLFreeForm3.StructureChanged;

  GLFreeForm1.Material.PolygonMode := pmLines;
  GLFreeForm2.Material.PolygonMode := pmLines;
end;

procedure TForm1.Button4Click(Sender: TObject);
Var
  Mesh : TGLMeshObject;
begin
  GLFreeForm3.MeshObjects.Clear;

  If GLFreeForm3.MeshObjects.Count = 0 then
    TGLMeshObject.CreateOwned(GLFreeForm3.MeshObjects).Mode := momFaceGroups;

  Mesh := GLFreeForm3.MeshObjects[0];

  CSG_Operation(GLFreeForm2.MeshObjects.Items[0],GLFreeForm1.MeshObjects.Items[0],CSG_Subtraction,Mesh,'1','2');
  GLFreeForm3.StructureChanged;

  GLFreeForm1.Material.PolygonMode := pmLines;
  GLFreeForm2.Material.PolygonMode := pmLines;
end;

procedure TForm1.Button5Click(Sender: TObject);
Var
  Mesh : TGLMeshObject;
begin
  GLFreeForm3.MeshObjects.Clear;

  If GLFreeForm3.MeshObjects.Count = 0 then
    TGLMeshObject.CreateOwned(GLFreeForm3.MeshObjects).Mode := momFaceGroups;

  Mesh := GLFreeForm3.MeshObjects[0];

  CSG_Operation(GLFreeForm1.MeshObjects.Items[0],GLFreeForm2.MeshObjects.Items[0],CSG_Intersection,Mesh,'1','2');
  GLFreeForm3.StructureChanged;

  GLFreeForm1.Material.PolygonMode := pmLines;
  GLFreeForm2.Material.PolygonMode := pmLines;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  If CheckBox1.Checked then
  Begin
    GLMaterialLibrary1.Materials[0].Material.PolygonMode := pmFill;
    GLMaterialLibrary1.Materials[1].Material.PolygonMode := pmFill;
  End else
  Begin
    GLMaterialLibrary1.Materials[0].Material.PolygonMode := pmLines;
    GLMaterialLibrary1.Materials[1].Material.PolygonMode := pmLines;
  End;
  GLFreeForm3.StructureChanged;
end;

end.
