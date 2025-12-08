unit Unit1;

{$mode objfpc}{$H+}

interface

uses

  Classes,SysUtils,  Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ExtDlgs,

  //GLScene
  GLScene, GLVectorFileObjects, GLLCLViewer, GLVectorLists,
  GLVectorGeometry, GLTexture, GLObjects, GLFile3ds,
  GLCoordinates, GLCrossPlatform;

type

  { TForm1 }
  TForm1 = class(TForm)
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLFreeForm1: TGLFreeForm;
    Panel1: TPanel;
    GLSceneViewer1: TGLSceneViewer;
    Image1: TImage;
    Button3: TButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx,my : integer;
    procedure GetTexCoordsWireframe;
    procedure ConvertFacegroups;
  end;

  TAxis = 0..2;

var
  Form1: TForm1;

procedure UVPlanarMapping(MeshObject:TGLMeshObject; min,max : TAffineVector; Axis: TAxis = 2);
procedure UVCubicMapping(MeshObject:TGLMeshObject; min,max : TAffineVector; Axis: TAxis = 2);
procedure UVCylindricalMapping(MeshObject:TGLMeshObject; min,max : TAffineVector; Axis: TAxis = 2; MaxUDiff: Single = 0.75);
procedure UVSphericalMapping(MeshObject:TGLMeshObject; min,max : TAffineVector; Axis: TAxis = 2; MaxUDiff: Single = 0.75);
// The MaxUDiff parameter is used as a check to help avoid wrapping
// issues that Cylindrical and Spherical methods have. It should
// represent the largest difference between U coordinate values
// you would expect in the mesh being loaded.

implementation


  {$R *.lfm}

 uses math;

procedure UVPlanarMapping(MeshObject:TGLMeshObject; min,max : TAffineVector; Axis: TAxis = 2);
var
  i,j : integer;
  P,center : TAffineVector;
  u,v : single;
  fg : TFGVertexNormalTexIndexList;
begin with MeshObject do begin
  center:=VectorScale(VectorAdd(max,min),0.5);

  TexCoords.Clear;
  for i:=0 to FaceGroups.Count-1 do begin
    fg:=TFGVertexNormalTexIndexList(FaceGroups.Items[i]);
    fg.TexCoordIndices.Clear;

    for j:=0 to fg.VertexIndices.Count-1 do begin
      P:=VectorSubtract(Vertices.Items[fg.VertexIndices.Items[j]],center);
      case Axis of
        0 : begin
          u:=(P.Y/(max.Y-min.Y));
          v:=(P.Z/(max.Z-min.Z));
        end;
        1 : begin
          u:=(P.X/(max.X-min.X));
          v:=(P.Z/(max.Z-min.Z));
        end;
        2 : begin
          u:=(P.X/(max.X-min.X));
          v:=(P.Y/(max.Y-min.Y));
        end;
      end;
      u:=u+0.5;
      v:=v+0.5;
      fg.TexCoordIndices.Add(TexCoords.FindOrAdd(AffineVectorMake(u,v,0)));
    end;
  end;
end; end;

procedure UVCubicMapping(MeshObject:TGLMeshObject; min,max : TAffineVector; Axis: TAxis = 2);
var
  Plane : array[0..5] of TAffineVector;
  i,j,k : integer;
  P,N,center : TAffineVector;
  u,v,dp,MaxDotProd,uoff,voff : single;
  PlaneIdx : integer;
  fg  : TFGVertexNormalTexIndexList;
  temp : single;
begin with MeshObject do begin
  center:=VectorScale(VectorAdd(max,min),0.5);

  case Axis of
    0 : begin
      Plane[0]:=VectorNegate(YVector);  // Left
      Plane[1]:=XVector;                // Front
      Plane[2]:=ZVector;                // Top
      Plane[3]:=VectorNegate(Plane[0]); // Right
      Plane[4]:=VectorNegate(Plane[1]); // Back
      Plane[5]:=VectorNegate(Plane[2]); // Bottom
    end;

    1 : begin
      Plane[0]:=XVector;                // Left
      Plane[1]:=YVector;                // Front
      Plane[2]:=ZVector;                // Top
      Plane[3]:=VectorNegate(Plane[0]); // Right
      Plane[4]:=VectorNegate(Plane[1]); // Back
      Plane[5]:=VectorNegate(Plane[2]); // Bottom
    end;

    2 : begin
      Plane[0]:=XVector;                // Left
      Plane[1]:=ZVector;                // Front
      Plane[2]:=VectorNegate(YVector);  // Top
      Plane[3]:=VectorNegate(Plane[0]); // Right
      Plane[4]:=VectorNegate(Plane[1]); // Back
      Plane[5]:=VectorNegate(Plane[2]); // Bottom
    end;
  end;

  for i:=0 to FaceGroups.Count-1 do begin
    fg:=TFGVertexNormalTexIndexList(FaceGroups.Items[i]);
    fg.TexCoordIndices.Clear;

    if fg.Mode<>fgmmTriangles then exit;
    for j:=0 to (fg.VertexIndices.Count div 3)-1 do begin
      N:=NullVector;
      for k:=0 to 2 do
        N:=VectorAdd(N,Normals.Items[fg.VertexIndices.Items[j*3+k]]);
      N:=VectorScale(N,1/3);

      PlaneIdx:=0;
      MaxDotProd:=VectorDotProduct(N,Plane[0]);
      for k:=1 to 5 do begin
        dp:=VectorDotProduct(N,Plane[k]);
        if dp>MaxDotProd then begin
          PlaneIdx:=k;
          MaxDotProd:=dp;
        end;
      end;

      for k:=0 to 2 do begin
        P:=VectorSubtract(Vertices.Items[fg.VertexIndices.Items[j*3+k]],center);

        if Axis=0 then begin
          temp:=P.X;
          P.X:=-P.Y;
          P.Y:=temp;
        end;
        if Axis=2 then begin
          temp:=P.Y;
          P.Y:=P.Z;
          P.Z:=-temp;
        end;

        case PlaneIdx of
          0 : begin
            u:=1-((P.Z/(max.Z-min.Z))+0.5);
            v:=(P.Y/(max.Y-min.Y))+0.5;
            uoff:=2/3;
            voff:=2/4;
          end;
          1 : begin
            u:=(P.X/(max.X-min.X))+0.5;
            v:=1-((P.Z/(max.Z-min.Z))+0.5);
            uoff:=1/3;
            voff:=3/4;
          end;
          2 : begin
            u:=(P.X/(max.X-min.X))+0.5;
            v:=(P.Y/(max.Y-min.Y))+0.5;
            uoff:=1/3;
            voff:=2/4;
          end;
          3 : begin
            u:=(P.Z/(max.Z-min.Z))+0.5;
            v:=(P.Y/(max.Y-min.Y))+0.5;
            uoff:=0;
            voff:=2/4;
          end;
          4 : begin
            u:=((P.X/(max.X-min.X))+0.5);
            v:=((P.Z/(max.Z-min.Z))+0.5);
            uoff:=1/3;
            voff:=1/4;
          end;
          5 : begin
            u:=((P.X/(max.X-min.X))+0.5);
            v:=1-((P.Y/(max.Y-min.Y))+0.5);
            uoff:=1/3;
            voff:=0;
          end;
        end;

        if u>1 then u:=1;
        if u<0 then u:=0;
        if v>1 then v:=1;
        if v<0 then v:=0;

        fg.TexCoordIndices.Add(TexCoords.FindOrAdd(AffineVectorMake((u/3+uoff),(v/4+voff),0)));
      end;
    end;
  end;
end; end;

procedure UVCylindricalMapping(MeshObject:TGLMeshObject; min,max : TAffineVector; Axis: TAxis = 2; MaxUDiff: Single = 0.75);
var
  i,j,k       : integer;
  P,Pn,center : TAffineVector;
  triU        : array[0..2] of single;
  u,v         : single;
  fg          : TFGVertexNormalTexIndexList;
begin with MeshObject do begin
  center:=VectorScale(VectorAdd(max,min),0.5);

  TexCoords.Clear;
  for i:=0 to FaceGroups.Count-1 do begin
    fg:=TFGVertexNormalTexIndexList(FaceGroups.Items[i]);
    fg.TexCoordIndices.Clear;

    for j:=0 to fg.VertexIndices.Count-1 do begin
      P:=VectorSubtract(Vertices.Items[fg.VertexIndices.Items[j]],center);
      Pn:=VectorNormalize(P);
      case Axis of
        0 : begin
          u:=arctan2(Pn.Z,-Pn.Y);
          v:=(P.X/(max.X-min.X));
        end;
        1 : begin
          u:=arctan2(-Pn.X,Pn.Z);
          v:=(P.Y/(max.Y-min.Y));
        end;
        2 : begin
          u:=arctan2(-Pn.X,-Pn.Y);
          v:=(P.Z/(max.Z-min.Z));
        end;
      end;
      u:=1-u/(2*Pi);
      v:=v+0.5;
      if (u<0) or (u>1) then
        u:=u-floor(u);

      if (j>=3) and (j mod 3 = 0) then begin
        if (triU[1]-triU[0]>MaxUDiff) or (triU[2]-triU[0]>MaxUDiff) then begin
          k:=TexCoords.FindOrAdd(AffineVectorMake(triU[0]+1,v,0));
          fg.TexCoordIndices.Items[j-3]:=k;
        end;
        if (triU[0]-triU[1]>MaxUDiff) or (triU[2]-triU[1]>MaxUDiff) then begin
          k:=TexCoords.FindOrAdd(AffineVectorMake(triU[1]+1,v,0));
          fg.TexCoordIndices.Items[j-2]:=k;
        end;
        if (triU[0]-triU[2]>MaxUDiff) or (triU[1]-triU[2]>MaxUDiff) then begin
          k:=TexCoords.FindOrAdd(AffineVectorMake(triU[2]+1,v,0));
          fg.TexCoordIndices.Items[j-1]:=k;
        end;
      end;
      triU[j mod 3]:=u;

      fg.TexCoordIndices.Add(TexCoords.FindOrAdd(AffineVectorMake(u,v,0)));
    end;
  end;
end; end;

procedure UVSphericalMapping(MeshObject:TGLMeshObject; min,max : TAffineVector; Axis: TAxis = 2; MaxUDiff: Single = 0.75);
var
  i,j,k    : integer;
  P,center : TAffineVector;
  triU     : array[0..2] of single;
  u,v      : single;
  fg       : TFGVertexNormalTexIndexList;
begin with MeshObject do begin
  center:=VectorScale(VectorAdd(max,min),0.5);

  TexCoords.Clear;
  for i:=0 to FaceGroups.Count-1 do begin
    fg:=TFGVertexNormalTexIndexList(FaceGroups.Items[i]);
    fg.TexCoordIndices.Clear;

    for j:=0 to fg.VertexIndices.Count-1 do begin
      P:=VectorNormalize(VectorSubtract(Vertices.Items[fg.VertexIndices.Items[j]],center));

      case Axis of
        0 : begin
          u:=arctan2(P.Z,-P.Y);
          v:=arctan(P.X/sqrt(P.Y*P.Y+P.Z*P.Z));
        end;
        1 : begin
          u:=arctan2(-P.X,P.Z);
          v:=arctan(P.Y/sqrt(P.X*P.X+P.Z*P.Z));
        end;
        2 : begin
          u:=arctan2(-P.X,-P.Y);
          v:=arctan(P.Z/sqrt(P.X*P.X+P.Y*P.Y));
        end;
      end;
      u:=1-u/(2*Pi);
      v:=1-abs(0.5-v/Pi);
      if (u<0) or (u>1) then
        u:=u-floor(u);

      if (j>=3) and (j mod 3 = 0) then begin
        if (triU[1]-triU[0]>MaxUDiff) or (triU[2]-triU[0]>MaxUDiff) then begin
          k:=TexCoords.FindOrAdd(AffineVectorMake(triU[0]+1,v,0));
          fg.TexCoordIndices.Items[j-3]:=k;
        end;
        if (triU[0]-triU[1]>MaxUDiff) or (triU[2]-triU[1]>MaxUDiff) then begin
          k:=TexCoords.FindOrAdd(AffineVectorMake(triU[1]+1,v,0));
          fg.TexCoordIndices.Items[j-2]:=k;
        end;
        if (triU[0]-triU[2]>MaxUDiff) or (triU[1]-triU[2]>MaxUDiff) then begin
          k:=TexCoords.FindOrAdd(AffineVectorMake(triU[2]+1,v,0));
          fg.TexCoordIndices.Items[j-1]:=k;
        end;
      end;
      triU[j mod 3]:=u;

      fg.TexCoordIndices.Add(TexCoords.FindOrAdd(AffineVectorMake(u,v,0)));
    end;
  end;
end; end;

procedure TForm1.FormCreate(Sender: TObject);
var
  s : single;
  m,i,j : integer;
  fg : TFGVertexNormalTexIndexList;
begin
  Image1.Canvas.FillRect(Rect(0,0,Image1.Width,Image1.Height));
  GLFreeForm1.LoadFromFile('cube.3ds');
  ConvertFacegroups;
  GLFreeForm1.Material.Texture.Image.LoadFromFile('GLScene.bmp');
  GLFreeForm1.Material.Texture.Disabled:=False;
  s:=2.5/(GLFreeForm1.BoundingSphereRadius);
    GLFreeForm1.Scale.SetVector(s,s,s);
  GLFreeForm1.Pitch(90);
  ComboBox1Change(Self);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my-y,mx-x);
  mx:=X; my:=Y;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx:=X; my:=Y;
end;

procedure TForm1.GetTexCoordsWireframe;
var
  i,j,x,y,x0,y0 : integer;
  fg : TFGVertexNormalTexIndexList;
begin
  Image1.Canvas.FillRect(Rect(0,0,Image1.Width,Form1.Image1.Height));
  Image1.Canvas.Pen.Color:=clBlack;
  with Form1.GLFreeForm1.MeshObjects.Items[0] do begin
    for i:=0 to FaceGroups.Count-1 do begin
      fg:=TFGVertexNormalTexIndexList(FaceGroups.Items[i]);
      for j:=0 to fg.TexCoordIndices.Count-1 do begin
        if j=0 then begin
          x0:=round(TexCoords.Items[fg.TexCoordIndices.Items[j]].X*Form1.Image1.Width);
          y0:=round(TexCoords.Items[fg.TexCoordIndices.Items[j]].Y*Form1.Image1.Height);
          Image1.Canvas.MoveTo(x0,Image1.Height-y0);
        end else begin
          x:=round(TexCoords.Items[fg.TexCoordIndices.Items[j]].X*Form1.Image1.Width);
          y:=round(TexCoords.Items[fg.TexCoordIndices.Items[j]].Y*Form1.Image1.Height);
          Image1.Canvas.LineTo(x,Image1.Height-y);
        end;
      end;
      Image1.Canvas.LineTo(x0,Image1.Height-y0);
    end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  s : single;
begin
  if OpenDialog1.Execute then begin
    GLFreeForm1.LoadFromFile(OpenDialog1.FileName);
    ConvertFacegroups;
    GLFreeForm1.Scale.SetVector(1,1,1);
    s:=2.5/(GLFreeForm1.BoundingSphereRadius);
    GLFreeForm1.Scale.SetVector(s,s,s);
    ComboBox1Change(Self);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    GLFreeForm1.Material.Texture.Image.LoadFromFile(OpenPictureDialog1.FileName);
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
var
  min,max,center : TAffineVector;
  mo : TGLMeshObject;
begin
  mo:=GLFreeForm1.MeshObjects.Items[0];
  mo.GetExtents(min,max);
  case ComboBox2.ItemIndex of
    0 : UVPlanarMapping(mo,min,max,Combobox1.ItemIndex);
    1 : UVCubicMapping(mo,min,max,Combobox1.ItemIndex);
    2 : UVCylindricalMapping(mo,min,max,Combobox1.ItemIndex,0.8);
    3 : UVSphericalMapping(mo,min,max,Combobox1.ItemIndex,0.8);
  end;

  GetTexCoordsWireframe;
  GLFreeForm1.StructureChanged;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  ComboBox1Change(Self);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if Checkbox1.Checked then
    GLFreeForm1.NormalsOrientation:=mnoInvert
  else
    GLFreeForm1.NormalsOrientation:=mnoDefault;
end;

procedure TForm1.ConvertFacegroups;
var
  fg : TFGVertexNormalTexIndexList;
  i,j,m : integer;
begin
  for m:=0 to GLFreeForm1.MeshObjects.Count-1 do
  with GLFreeForm1.MeshObjects.Items[m] do begin
    for i:=0 to FaceGroups.Count-1 do begin
      if (FaceGroups.Items[i] is TFGVertexIndexList) then begin
        fg:=TFGVertexNormalTexIndexList.CreateOwned(FaceGroups);
        fg.Mode:=TFGVertexIndexList(FaceGroups.Items[i]).Mode;
        fg.VertexIndices.Assign(TFGVertexIndexList(FaceGroups.Items[i]).VertexIndices);
        fg.NormalIndices.Assign(fg.VertexIndices);
        j:=FaceGroups.IndexOf(fg);
        FaceGroups.Exchange(i,j);
        FaceGroups.DeleteAndFree(j);
      end;
    end;
  end;
end;

end.
