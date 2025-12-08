unit Unit1;

{$mode objfpc}{$h+}

interface

uses

  SysUtils,Classes,
  Graphics, Controls, Forms, Dialogs,
   
   GLScene, GLObjects, GLMesh, GLTexture, GLVectorTypes,
  GLVectorGeometry, GLVectorFileObjects, GLMaterial, GLCoordinates,
  GLCrossPlatform, GLBaseClasses, GLColor, GLLCLViewer, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLCamera1: TGLCamera;
    GLMesh1: TGLMesh;
    GLFreeForm1: TGLFreeForm;
    GLM1: TGLMaterialLibrary;
    Panel1: TPanel;
    Image1: TImage;
    Edit1: TEdit;
    Button4: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    procedure Loadimage_Draw_2(img: string; tarmesh: TGLFreeForm);
     
  public
     
  end;

var
  lMeshObj: TGLMeshObject;
  viewX, viewY: integer;
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget(viewY - Y, viewX - X);
  end;

  if ssRight in Shift then
  begin
    // GLCamera1.MoveAroundTarget(viewY - y, viewX - x);
    GLDummyCube1.Position.X := GLDummyCube1.Position.X + (viewX - X) * 0.25;
    GLDummyCube1.Position.Z := GLDummyCube1.Position.Z - (viewY - Y) * 0.25;
    GLCamera1.Position.X := GLCamera1.Position.X + (viewX - X) * 0.25;
    GLCamera1.Position.Z := GLCamera1.Position.Z - (viewY - Y) * 0.25;

  end;

  viewX := X;
  viewY := Y;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  viewX := X;
  viewY := Y;
end;

procedure TForm1.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(0.9);
end;

procedure TForm1.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(1.1);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i, f: integer;
  co: TVector4F;
begin
  f:=0;

  for i := 0 to lMeshObj.Colors.Count - 1 do
  begin
// for c:=(i-1)*127*2 to (i)*127*2 do
// begin
    co.X := lMeshObj.Vertices.Items[i].Y * 25.5 / 255; // (random(150)+100)/255;
    co.Y := lMeshObj.Vertices.Items[i].Y * 25.5 / 255; // (random(150)+100)/255;
    co.Z := 0.8; // lMeshObj.Vertices.Items[i][0];   //(random(150)+100)/255;
    co.W := 0.8; // lMeshObj.Vertices.Items[i][0];   //(random(150)+100)/255;

    lMeshObj.Colors.Items[i] := co;
    inc(f);
// end;//c

  end; // i
  // button2.Caption:=IntToStr(f);
  GLFreeForm1.StructureChanged;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Loadimage_Draw_2('.\bmp\terrain_128_2.bmp', GLFreeForm1);
  Button2.Enabled := True;
  Button4.Enabled := True;
end;

procedure TForm1.Loadimage_Draw_2(img: string; tarmesh: TGLFreeForm);
var
  i, c: integer;
  p1, p2, p3, p4: TVertexData;
  vect: TVector4F;
  col1, col2, col3, col4: TVector4F;
  lFaceGroup: TFGVertexIndexList;
  BmpDir: String;

begin
  Image1.Picture.LoadFromFile(img);
  tarmesh.MeshObjects.Clear;

  lMeshObj := TGLMeshObject.CreateOwned(tarmesh.MeshObjects);
  lMeshObj.Mode := momFaceGroups;
  lFaceGroup := TFGVertexIndexList.CreateOwned(lMeshObj.FaceGroups);

  for i := 0 to 126 do // Image1.Picture.Width-1 do
  begin
    for c := 0 to 126 do // Image1.Picture.Height-1 do
    begin
      vect := ConvertWinColor(Image1.Canvas.Pixels[i, c]);
      p1.coord.X := i;
      p1.coord.Y := ((vect.X * 4) + (vect.Y * 3) + (vect.Z * 2));
      p1.coord.Z := c;
      col1.X := (random(150) + 100) / 255;
      col1.Y := (random(150) + 100) / 255;
      col1.Z := (random(150) + 100) / 255;
      //p1.color.W := (random(150)+100)/255;
      //
      vect := ConvertWinColor(Image1.Canvas.Pixels[i + 1, c]);
      p2.coord.X := i + 1;
      p2.coord.Y := ((vect.X * 4) + (vect.Y * 3) + (vect.Z * 2));
      p2.coord.Z := c;
      col2.X := (random(150) + 100) / 255;
      col2.Y := (random(150) + 100) / 255;
      col2.Z := (random(150) + 100) / 255;
      //p2.color.W := (random(150)+100)/255;
      //
      vect := ConvertWinColor(Image1.Canvas.Pixels[i, c + 1]);
      p3.coord.X := i;
      p3.coord.Y := ((vect.X * 4) + (vect.Y * 3) + (vect.Z * 2));
      p3.coord.Z := c + 1;
      col3.X := (random(150) + 100) / 255;
      col3.Y := (random(150) + 100) / 255;
      col3.Z := (random(150) + 100) / 255;
      //p3.color.W := (random(150)+100)/255;
      //
      vect := ConvertWinColor(Image1.Canvas.Pixels[i + 1, c + 1]);
      p4.coord.X := i + 1;
      p4.coord.Y := ((vect.X * 4) + (vect.Y * 3) + (vect.Z * 2));
      p4.coord.Z := c + 1;
      col4.X := (random(150) + 100) / 255;
      col4.Y := (random(150) + 100) / 255;
      col4.Z := (random(150) + 100) / 255;
(*
       lMeshObj.Vertices.Add(p1.coord,p4.coord,p2.coord); //
       lMeshObj.Vertices.Add(p1.coord,p3.coord,p4.coord); //
 *)
      lMeshObj.Vertices.Add(p1.coord);
      lMeshObj.Vertices.Add(p2.coord);
      lMeshObj.Vertices.Add(p3.coord);
      lMeshObj.Vertices.Add(p4.coord);

      lMeshObj.Colors.Add(col1);
      lMeshObj.Colors.Add(col2);
      lMeshObj.Colors.Add(col3);
      lMeshObj.Colors.Add(col4);

      lMeshObj.TexCoords.Add((i + 1) / 128, (c + 1) / 128);  //
      lMeshObj.TexCoords.Add((i + 2) / 128, (c + 1) / 128);  //
      lMeshObj.TexCoords.Add((i + 1) / 128, (c + 2) / 128);  //
      lMeshObj.TexCoords.Add((i + 2) / 128, (c + 2) / 128);  //

      lFaceGroup.Add(lMeshObj.Vertices.Count - 2);
      lFaceGroup.Add(lMeshObj.Vertices.Count - 1);
      lFaceGroup.Add(lMeshObj.Vertices.Count - 3);

      lFaceGroup.Add(lMeshObj.Vertices.Count - 3);
      lFaceGroup.Add(lMeshObj.Vertices.Count - 4);
      lFaceGroup.Add(lMeshObj.Vertices.Count - 2);

    end; // c
  end; // i
  tarmesh.Position.X := -Image1.Picture.Width * 0.5;
  tarmesh.Position.Z := -Image1.Picture.Height * 0.5;

  GLM1.Materials.Add;
  GLM1.Materials.Items[0].Material.Texture.Disabled := False;

  BmpDir := GetCurrentDir()+'\bmp';
  img := BmpDir+'\terrain_128_2.bmp';
  GLM1.Materials.Items[0].Material.Texture.Image.LoadFromFile(img);
  GLM1.Materials[0].Material.Texture.Image.Assign(Image1.Picture);

  tarmesh.StructureChanged;
  GLMesh1.Vertices.Clear;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  i: integer;
  co: TVector4F;
begin
  // f:=0;
  for i := 0 to lMeshObj.Colors.Count - 1 do
  begin
    if (i > 2) then
    begin
      if abs(lMeshObj.Vertices.Items[i - 1].Y - lMeshObj.Vertices.Items[i].Y) >
        strtofloat(Edit1.Text) then
      begin
        co.X := 1;
        co.Y := 0;
        co.Z := 0;
        co.W := 0;
        lMeshObj.Colors.Items[i - 3] := co;
        lMeshObj.Colors.Items[i - 2] := co;
        lMeshObj.Colors.Items[i - 1] := co;
        lMeshObj.Colors.Items[i] := co;
      end
      else
      begin
        co.X := lMeshObj.Vertices.Items[i].Y * 25.5 / 255;
        co.Y := lMeshObj.Vertices.Items[i].Y * 25.5 / 255;
        co.Z := 0.8;
        co.W := 0.8;
        lMeshObj.Colors.Items[i] := co;
      end;
    end;
  end;
  GLFreeForm1.StructureChanged;
end;

end.
