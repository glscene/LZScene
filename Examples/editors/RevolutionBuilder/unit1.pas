unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, GLScene, GLLCLViewer, GLSimpleNavigation,
  GLVectorGeometry, GLRevolutionMeshObject, GLColor, GLObjects, GLVectorFileObjects,
  GLGeomObjects, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  GLRenderContextInfo, GLMaterial, GLHiddenLineShader;

type

  { TForm1 }

  TForm1 = class(TForm)
    chkEdges: TCheckBox;
    chkSolid: TCheckBox;
    chkWireFrame: TCheckBox;
    chkGenQuads: TCheckBox;
    chkNormals: TCheckBox;
    GLArrowLine1: TGLArrowLine;
    GLCamera1: TGLCamera;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLDummyCube1: TGLDummyCube;
    GLFreeForm1: TGLFreeForm;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    radCylinder: TRadioButton;
    radCone: TRadioButton;
    radInvertedCone: TRadioButton;
    radConeCyclinder: TRadioButton;
    radDrilledHole: TRadioButton;
    radCapsule: TRadioButton;
    radFillet: TRadioButton;
    radFillet2: TRadioButton;
    radFilletTrans: TRadioButton;
    radMeshTrans: TRadioButton;
    radTorus: TRadioButton;
    radSphere: TRadioButton;
    radTopDomedCap: TRadioButton;
    radPipeFlange: TRadioButton;
    radTube: TRadioButton;
    radSteppedShaft: TRadioButton;
    radTipTest: TRadioButton;
    StatusBar1: TStatusBar;
    TrackBar1: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure chkEdgesClick(Sender: TObject);
    procedure chkGenQuadsChange(Sender: TObject);
    procedure chkSolidClick(Sender: TObject);
    procedure chkWireFrameChange(Sender: TObject);
    procedure chkNormalsChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure RadioButton1Change(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
    procedure RadioButton3Change(Sender: TObject);
    procedure RadioButton4Change(Sender: TObject);
    procedure RadioButton5Change(Sender: TObject);
    procedure RadioButton6Change(Sender: TObject);
    procedure RadioButton7Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    fSectors: integer;
    fSlices: integer;
    fShader: TGLHiddenLineShader;
    procedure Generate;


  public

  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Generate;
end;

procedure TForm1.chkEdgesClick(Sender: TObject);
begin
   GLSceneViewer1.Invalidate;
end;

procedure TForm1.chkGenQuadsChange(Sender: TObject);
begin

end;

procedure TForm1.chkSolidClick(Sender: TObject);
begin
  If chkSolid.Checked then
    fShader.Solid:=True
  else
    fShader.Solid:=False;

  GLSceneViewer1.Invalidate;
end;

procedure TForm1.chkWireFrameChange(Sender: TObject);
begin
  If chkWireFrame.Checked then
    fShader.Enabled:=True
  else
    fShader.Enabled:=False;

  chkSolid.Enabled:=fShader.Enabled;
  chkEdges.Enabled:=fShader.Enabled;

  GLSceneViewer1.Invalidate;
end;

procedure TForm1.chkNormalsChange(Sender: TObject);
begin
   GLSceneViewer1.Invalidate;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  fShader := TGLHiddenLineShader.Create(GLMaterialLibrary1);
  fShader.Enabled:=False;
  fShader.Solid := True;
  //fShader.
  fShader.BackgroundColor.Initialize(ConvertWinColor(GLSceneViewer1.Buffer.BackgroundColor));
  fShader.FrontLine.Width:=1;
  fShader.BackLine.Width:=1;
  fshader.BackLine.ForceMaterial:=True;
  fshader.FrontLine.ForceMaterial:=True;
  GLMaterialLibrary1.Materials[0].shader := fShader;
  fSectors := 12;
  fSlices := 8;
  Generate;
end;

procedure TForm1.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  if chkNormals.Checked then
    if GLFreeForm1.MeshObjects.Count > 0 then
      TGLRevolutionMeshObject(GLFreeForm1.MeshObjects[0]).RenderNormals;
  if chkEdges.Checked and chkedges.Enabled then
    if GLFreeForm1.MeshObjects.Count > 0 then
      if  TGLRevolutionMeshObject(GLFreeForm1.MeshObjects[0]).EdgeCount > 0 then
        TGLRevolutionMeshObject(GLFreeForm1.MeshObjects[0]).RenderEdges;
end;

procedure TForm1.RadioButton1Change(Sender: TObject);
begin
  fSectors := 6;
  Generate;
end;

procedure TForm1.RadioButton2Change(Sender: TObject);
begin
  fSectors := 12;
  Generate;
end;

procedure TForm1.RadioButton3Change(Sender: TObject);
begin
  fSectors := 17;
  Generate;
end;

procedure TForm1.RadioButton4Change(Sender: TObject);
begin
  fSlices := 4;
  Generate;
end;

procedure TForm1.RadioButton5Change(Sender: TObject);
begin
  fSlices := 8;
  Generate;
end;

procedure TForm1.RadioButton6Change(Sender: TObject);
begin
  fSlices := 12;
  Generate;
end;

procedure TForm1.RadioButton7Change(Sender: TObject);
begin

end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  GLDummyCube1.Position.Z := TrackBar1.Position;
end;

procedure TForm1.Generate;
Var
  Mesh : TGLRevolutionMeshObject;

begin

  GLFreeForm1.MeshObjects.Clear;
  mesh :=  TGLRevolutionMeshObject.CreateOwned(GLFreeForm1.MeshObjects);
  mesh.Sectors := fSectors;
  if chkGenQuads.Checked then
    TFGVertexIndexList(mesh.FaceGroups[0]).Mode := fgmmQuads;



  if radCylinder.Checked then
  begin
    // Cylinder
    mesh.MakeTip(5, 20, 20, True, False);
    mesh.MakeConicSection(5, 5, 20, 0, fSlices, False);
    mesh.MakeTip(5, 0, 0, False, False);
  end;

  if radInvertedCone.Checked then
  begin
    // inverted cone
    mesh.MakeTip(12, 20, 20, True, False);
    mesh.MakeConicSection(12, 5, 20, 0, fSlices, False);
    mesh.MakeTip(5, 0, 0, False, False);
  end;

  if radCone.Checked then
  begin
    // cone
    mesh.MakeTip(5, 20, 20, True, False);
    mesh.MakeConicSection(5, 12, 20, 0, fSlices, False);
    mesh.MakeTip(12, 0, 0, False, False);
  end;

  if radConeCyclinder.Checked then
  begin
    mesh.MakeTip(4, 20, 18, True, False);
    mesh.MakeConicSection(4, 12, 20, 10, fSlices, False);
    mesh.MakeConicSection(12, 12, 10, 0, fSlices, False);
   mesh.MakeTip(12, 0, -4, False, False);
  end;

  if radTipTest.Checked then
  begin
   mesh.MakeTip(4, 20, 22, True, False);
  end;

  if radSteppedShaft.Checked then
  begin
    mesh.MakeTip(5, 20, 20, True, False);
    mesh.MakeConicSection(5, 5, 20, 10, fSlices, False);
    mesh.MakeConicSection(5, 10, 10, 10, fSlices, False);
    mesh.MakeConicSection(10, 10, 10, 0, fSlices, False);
    mesh.MakeTip(10, 0, 0, False, False);
  end;

  if radDrilledHole.Checked then
  begin
    // top is not the highest point but the uppermost of the two points where
    // the revolution lines meet the z axis. Here the tip is lower than
    // radial points and creates a depression.
    mesh.MakeTip(6, 9, 6, True, False);
    // the concept of going from top to bottom allows us to define 'inner'
    // surfaces by making topz lower than bottom z for the conic section
    mesh.MakeConicSection(6, 6, 9, 19, fSlices, False);
    // here the z height is the same and only the radii differ producing
    // a meshed horizontal surface
    mesh.MakeConicSection(6, 12, 19, 19, 2, False);
    // outer cyclinder
    mesh.MakeConicSection(12, 12, 19, 0, fSlices * 2, False);
    // to make a finer mesh on the bottom of this object use a horizontal mesh
    // as above
    mesh.MakeConicSection(12, 4, 0, 0, 4, False);
    // and cap it with a triangle fan in the same plane
    // as this is on the same plane we want to reuse the last ring of points
    // in this defintion so set UsePrevious to true so we do not get two rings
    // of congruent points with the same normal.
    mesh.MakeTip(4, 0, 0, False, True);
    // we have made 6 calls but as one of them used UsePrevious we only have
    // 5 faces.

  end;

  if radTube.Checked then
  begin
    mesh.MakeConicSection(6, 6, 0, 20, fSlices, False);
    mesh.MakeConicSection(6, 8, 20, 20, 1, False);
    mesh.MakeConicSection(8, 8, 20, 0, fSlices, False);
    mesh.MakeConicSection(8, 6, 0, 0, 1, False);
  end;

  if radPipeFlange.Checked then
  begin
    mesh.MakeConicSection(6, 6, 0, 22, fSlices, False);
    mesh.MakeConicSection(6, 12, 22, 22, 3, False);
    mesh.MakeConicSection(12, 12, 22, 20, 1, False);
    mesh.MakeConicSection(12, 8, 20, 20, 1, False);
    mesh.MakeConicSection(6, 8, 20, 20, 1, False);
    mesh.MakeConicSection(8, 8, 20, 0, fSlices, False);
    mesh.MakeConicSection(8, 6, 0, 0, 1, False);
  end;

  if radTopDomedCap.Checked then
    mesh.MakeDomedCap(10, 10, fSlices, True);

  if radCapsule.Checked then
  begin
    mesh.MakeDomedCap(12, 20, fSlices, True);
    mesh.MakeConicSection(12, 12, 20, 0, fSlices, True);
    mesh.MakeDomedCap(12, 0, fSlices, False);
  end;

  if radSphere.Checked then
  begin
    mesh.MakeDomedCap(12, 0, fSlices, True);
    mesh.MakeDomedCap(12, 0, fSlices, False);
  end;

  if radTorus.Checked then
    mesh.maketorus(20, 6, 10, fSlices * 2);

  if radFillet.Checked then
  begin
    mesh.MakeDomedCap(7, 20, fSlices, True);
    mesh.MakeConicSection(7,7,20,16,fSlices, False);
    mesh.MakeFillet(10,3,16,fslices,tfqSW,False);
    mesh.MakeConicSection(10,20,13,13,fSlices,false);
    mesh.MakeFillet(20,3,10,fslices,tfqNE,False);
    mesh.MakeConicSection(23,23,10,0,fSlices, False);
    mesh.MakeFillet(20,3,0,fslices,tfqSE,False);
    mesh.MakeConicSection(20,10,-3,-3,fSlices,false);
    mesh.MakeFillet(10,3,-6,fslices,tfqNW,False);
    mesh.MakeConicSection(7,7,-6,-16,fSlices, False);
    mesh.MakeDomedCap(7, -16, fSlices, False);
  end;


  // this is the same as above but with no duplicate vertices.
  // saving of 9 * Sectors vertex definitions.
  if radFillet2.Checked then
  begin
    mesh.MakeDomedCap(7, 20, fSlices, True);
    mesh.MakeConicSection(7,7,20,16,fSlices, True);
    mesh.MakeFillet(10,3,16,fslices,tfqSW,True);
    mesh.MakeConicSection(10,20,13,13,fSlices,True);
    mesh.MakeFillet(20,3,10,fslices,tfqNE,True);
    mesh.MakeConicSection(23,23,10,0,fSlices, True);
    mesh.MakeFillet(20,3,0,fslices,tfqSE,True);
    mesh.MakeConicSection(20,10,-3,-3,fSlices,True);
    mesh.MakeFillet(10,3,-6,fslices,tfqNW,True);
    mesh.MakeConicSection(7,7,-6,-16,fSlices, True);
    mesh.MakeDomedCap(7, -16, fSlices, False);
  end;

  if radMeshTrans.Checked then
  begin
    mesh.UsesQuadMesh:=True;
    // Mesh transitions only valid for triangular meshes
    TFGVertexIndexList(mesh.FaceGroups[0]).Mode := fgmmTriangles;
    mesh.MakeConicSection(10,10,12,8,2, False);
    mesh.MakeTransitionFiner(10, 10, 8, 6, True);
    mesh.MakeConicSection(10,10,6,2,2,  True);
    mesh.MakeTransitionFiner(10, 10, 2, 1, True);
    mesh.MakeConicSection(10,10,1, -3, 6,  True);

    mesh.MakeTransitionCoarser(10,10,-3,-4, True);
    mesh.MakeConicSection(10, 10, -4, -8, 2,  True);

    mesh.MakeTransitionCoarser(10,10,-8, -10, True);
    mesh.MakeConicSection(10, 10, -10, -14, 2,  True);

    // show this in line mode
    chkWireFrame.Checked:=True;
  end;


  if radFilletTrans.Checked then
  begin
    mesh.UsesQuadMesh:=True;
    // Mesh transitions only valid for triangular meshes
    TFGVertexIndexList(mesh.FaceGroups[0]).Mode := fgmmTriangles;
    mesh.MakeDomedCap(7, 20, 4, True);
    mesh.MakeConicSection(7,7,20,18,1, True);
    mesh.MakeTransitionFiner(7, 7, 18, 16, True);
    mesh.MakeFillet(10,3,16,5,tfqSW,True);
    mesh.MakeConicSection(10,19,13,13,4,True);
    mesh.MakeTransitionFiner(19, 20, 13, 13, True);
    mesh.MakeFillet(20,3,10,5,tfqNE,True);
    mesh.MakeTransitionCoarser(23,23,10, 9, True);
    mesh.MakeConicSection(23,23,9,1,3, True);
    mesh.MakeTransitionFiner(23, 23, 1, 0, True);
    mesh.MakeFillet(20,3,0,5,tfqSE,True);
    mesh.MakeTransitionCoarser(20,19,-3, -3, True);

    mesh.MakeConicSection(19,10,-3,-3, 4, True);
    mesh.MakeFillet(10,3,-6,5,tfqNW,True);
    mesh.MakeTransitionCoarser(7,7,-8, -9, True);
    mesh.MakeConicSection(7,7,-9,-16,3, True);
    mesh.MakeDomedCap(7, -16, 4, False);
  end;


  StatusBar1.SimpleText := format(
     'MeshBuilder - %d Tris - %d Vertices - %d Topopogy Faces - %d Edges',
     [mesh.TriangleCount, mesh.Vertices.Count, mesh.TopoFaces, mesh.EdgeCount]);

  GLSceneViewer1.Invalidate;
end;


end.

