unit GLMeshObjectHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  GLScene, OpenGLTokens, GLVectorTypes,GLVectorGeometry, GLTexture,
  GLMaterial, GLMesh, GLVectorLists, GLPersistentClasses, GLOctree, GLGeometryBB,
  GLApplicationFileIO, GLSilhouette, GLContext, GLColor, GLRenderContextInfo,
  GLCoordinates, GLBaseClasses, GLTextureFormat,
  GLObjects,GLVectorFileObjects;


const
  vAxisX : TAffineVector = (X:1; Y:0; Z:0);
  vAxisY : TAffineVector = (X:0; Y:1; Z:0);
  vAxisZ : TAffineVector = (X:0; Y:0; Z:1);

Type
  TGLMeshObjectPrimitive = (momClear,mopCube,mopSphere,moGeoSphere,mopCylinder, mopPyramid, mopTube, moFrustrum,mopTorus);


//Type
//  TGLCustomMeshObjectPrimitiveSettings = class;

Type
  TGLCustomEditingMeshObject = class(TGLMeshObject)
    private
      FCurrentSelectedVertex : TVector;
      FCurrentSelectedVertexIndex : Integer;

      FFaceList : TFGVertexIndexList;


    protected
    public
      constructor Create;override;
     // constructor Create(aPrimitive : TGLMeshObjectPrimitive); virtual; override;
     // constructor Create(aPrimitive : TGLMeshObjectPrimitive;aPrimitiveSettings : TGLCustomMeshObjectPrimitiveSettings); virtual; override;
      destructor Destruct;

      procedure MakePlane(width,depth:Single);
      procedure MakeGrid(width,depth: single;stacks,slices:integer);
      procedure MakeDisk(innerradius,outerradius,startangle,sweepangle:single;loops,slices:integer);

      procedure MakeCube(width,height,depth:single);
      procedure MakeSphere(radius:single;rings,sectors:integer;IsDome:Boolean=false);
      // procedure MakeDome
      procedure MakeCylinderEx(topRadius,bottomRadius,height:single;stacks,slices:integer; DrawTop,DrawBottom:Boolean);

      procedure MakeCylinder(radius,height:single;stacks,slices:integer);
      procedure MakeCone(radius,height:single;stacks,slices:integer);
      procedure MakePyramid(radius,height:single;stacks:integer);

      procedure MakeTorus(InnerRadius,outerRadius : single;slices,rings : integer);

      procedure MakeTube(innerradius,outerradius,height:single;stacks,slices:integer);
      //procedure MakeCapsule

      function AddVertice(x,y,z:single;const NoDuplicate:boolean = False):integer;
      function AddVertice(v:TAffineVector;const NoDuplicate:boolean = false):integer;overload;
      procedure ExchangeVertices(vi1,vi2:Integer);
      procedure MoveVertice(vi:integer;x,y,z:Single);
      procedure setVertice(vi:integer;x,y,z:Single);
      function getVertice(vi:integer):TAffineVector;
      procedure RotateVertice(vi:integer;x, y, z : single);
      procedure RotateVerticeAroundX(vi:integer; x : single);
      procedure RotateVerticeAroundY(vi:integer; y : single);
      procedure RotateVerticeAroundZ(vi:integer; z : single);
      procedure DeleteVertice(vi:Integer);

      procedure AddEdge;
      procedure DeleteEdge;
      procedure MoveEdge;

      procedure AddFace(v1,v2,v3:TVector);
      procedure AddFace(v1,v2,v3:Integer); overload;
      procedure AddQuadFace(v1,v2,v3,v4:Integer);

      procedure SetMaterial(AValue:String);
      procedure WeldVertex;

  end;
implementation

uses math;

constructor TGLCustomEditingMeshObject.Create;
begin
  inherited Create;
  FFaceList := TFGVertexIndexList.CreateOwned(self.FaceGroups);
  FFaceList.Mode :=fgmmQuads; //fgmmTriangles;
  Mode := momFaceGroups;

end;

//constructor TGLCustomEditingMeshObject.Create(aPrimitive : TGLMeshObjectPrimitive);
//begin
//end;
//
//constructor TGLCustomEditingMeshObject.Create(aPrimitive : TGLMeshObjectPrimitive;aPrimitiveSettings : TGLCustomMeshObjectPrimitiveSettings);
//begin
//end;

destructor TGLCustomEditingMeshObject.Destruct;
begin
  FFaceList.Free;

end;

function TGLCustomEditingMeshObject.AddVertice(x,y,z:single;const NoDuplicate:boolean = false):integer;
var i:integer;
begin
  // Becarefull it's seem with Quad face, the faces cannot have two same vertice
  // When it's the case the application freeze
  if (NoDuplicate) and (vertices.Count>0) then
  begin
    for i:=0 to vertices.Count-1 do
    begin
      if ((vertices.Items[i].x=x) and (vertices.Items[i].y=y) and (vertices.Items[i].z=z)) then
        result := i
      else
        result:=vertices.Add(x,y,z);
    end;
  end
  else
    result:=vertices.Add(x,y,z);
end;

function TGLCustomEditingMeshObject.AddVertice(v:TAffineVector;const NoDuplicate:boolean = false):integer;
var i:integer;
begin
  // Becarefull it's seem with Quad face, the faces cannot have two same vertice
  // When it's the case the application freeze
  //if (NoDuplicate) and (vertices.Count>0) then
  //begin
  //  for i:=0 to vertices.Count-1 do
  //  begin
  //    if ((vertices.Items[i] = v)) then
  //      result := i
  //    else
  //      result:=vertices.Add(v);
  //  end;
  //end
  //else
    result:=vertices.Add(v);
end;

procedure TGLCustomEditingMeshObject.ExchangeVertices(vi1,vi2:Integer);
begin
  vertices.Exchange(vi1,vi2);
end;

procedure TGLCustomEditingMeshObject.MoveVertice(vi:integer;x,y,z:Single);
Var nv : TAffineVector;
begin
  setVector(nv,vertices.Items[vi].X + x,vertices.Items[vi].Y + y, vertices.Items[vi].Z + z);
  vertices.Items[vi] := nv;

end;

procedure TGLCustomEditingMeshObject.SetVertice(vi:integer;x,y,z:Single);
Var nv : TAffineVector;
begin
  setVector(nv,x, y, z);
  vertices.Items[vi] := nv;
end;

procedure TGLCustomEditingMeshObject.RotateVertice(vi:integer;x, y, z : single);
begin
 VectorRotateAroundX( vertices.Items[vi], x);
 VectorRotateAroundY( vertices.Items[vi], y);
 VectorRotateAroundZ( vertices.Items[vi], z);
end;

procedure TGLCustomEditingMeshObject.RotateVerticeAroundX(vi:integer; x : single);
begin
 VectorRotateAroundX( vertices.Items[vi], x);
end;

procedure TGLCustomEditingMeshObject.RotateVerticeAroundY(vi:integer; y : single);
begin
 VectorRotateAroundY( vertices.Items[vi], y);
end;

procedure TGLCustomEditingMeshObject.RotateVerticeAroundZ(vi:integer; z : single);
begin
 VectorRotateAroundZ( vertices.Items[vi], z);
end;

procedure TGLCustomEditingMeshObject.DeleteVertice(vi:Integer);
begin
  vertices.Delete(vi);
end;

function TGLCustomEditingMeshObject.getVertice(vi:Integer):TAffineVector;
begin
  result:=vertices.Items[vi];
end;

procedure TGLCustomEditingMeshObject.AddEdge;
begin
end;

procedure TGLCustomEditingMeshObject.DeleteEdge;
begin
end;

procedure TGLCustomEditingMeshObject.MoveEdge;
begin
end;

procedure TGLCustomEditingMeshObject.AddFace(v1,v2,v3:TVector);
var
  i1,i2,i3:integer;
begin
 i1:= AddVertice(v1.X,v1.Y,v1.Z);
 i2:= AddVertice(v2.X,v2.Y,v2.Z);
 i3:= AddVertice(v3.X,v3.Y,v3.Z);
 FFaceList.VertexIndices.Add(i1,i2,i3);
end;

procedure TGLCustomEditingMeshObject.AddFace(v1,v2,v3:Integer);
begin
 FFaceList.VertexIndices.Add(v1,v2,v3);
end;
procedure TGLCustomEditingMeshObject.AddQuadFace(v1,v2,v3,v4:Integer);
begin
 FFaceList.VertexIndices.Add(v1);
 FFaceList.VertexIndices.Add(v2);
 FFaceList.VertexIndices.Add(v3);
 FFaceList.VertexIndices.Add(v4);
end;

procedure TGLCustomEditingMeshObject.MakePlane(width,depth:Single);
var
  x,z : single;
begin
  x:= width / 2.0;
  z:= depth / 2.0;
  Normals.Add(0,1,0);
  TexCoords.Add(0,0);
  Addvertice(-x,0,-z);

  Normals.Add(0,1,0);
  TexCoords.Add(0,1);
  Addvertice(-x,0,z);

  Normals.Add(0,1,0);
  TexCoords.Add(1,0);
  Addvertice(x,0,z);

  Normals.Add(0,1,0);
  TexCoords.Add(1,1);
  Addvertice(x,0,-z);

  AddQuadFace(0,1,2,3);
end;

procedure TGLCustomEditingMeshObject.MakeSphere(radius:single;rings,sectors:integer;IsDome:Boolean=false);
var
  R,S,x,y,z : single;
  mr,rr,ss : integer;
begin

  R := 1./(rings-1);

  S := 1./(sectors-1);
  if IsDome then
     mr := (rings div 2)
  else
    mr:= rings;
  for rr := 0 to mr-1 do
     for ss := 0 to sectors-1 do
     begin

       y := sin( -cPIDiv2 + cPI * rr * R );

       x := cos(c2PI * ss * S) * sin( cPI * rr * R );
       z := sin(c2PI * ss * S) * sin( cPI * rr * R );

       AddVertice(x * radius,y * radius,z * radius);
       Normals.Add(x, y, z);
       TexCoords.Add(ss*R, rr*R);
      end;

  // Generate quads for each face.
  for rr := 0 to mr-2 do
     for ss := 0 to sectors-2 do
     begin
       // Corners of quads should be in CCW order.
       AddQuadFace((rr + 0) * sectors + (ss + 0),
                   (rr + 0) * sectors + (ss + 1),
                   (rr + 1) * sectors + (ss + 1),
                   (rr + 1) * sectors + (ss + 0));

     end;
//  buildNormals(FFaceList.VertexIndices,momTriangles);
end;

procedure TGLCustomEditingMeshObject.MakeCube(width,height,depth:single);
var
  w,h,d:single;
begin
  w := width/2.0;
  h := height/2.0;
  d := depth/2.0;

  AddVertice(-w,h,-d); //0
  AddVertice(w,h,-d);  //1
  AddVertice(w,h,d); //2
  AddVertice(-w,h,d); //3

  AddVertice(-w,-h,-d); //4
  AddVertice(w,-h,-d);  //5
  AddVertice(w,-h,d); //6
  AddVertice(-w,-h,d); //7


  //Top
  //Normals.add(0,1,0);
  AddQuadFace(3,2,1,0);
  //Bottom
  //Normals.add(0,-1,0);
  AddQuadFace(4,5,6,7);
  //Front
  //Normals.add(0,0,1);
  AddQuadFace(7,6,2,3);
  //Back
  //Normals.add(0,0,-1);
  AddQuadFace(5,4,0,1);
  //Left
  //Normals.add(-1,0,0);
  AddQuadFace(4,7,3,0);
  //Right
 // Normals.add(1,0,0);
  AddQuadFace(6,5,1,2);

  buildNormals(FFaceList.VertexIndices,momTriangles);
end;

procedure TGLCustomEditingMeshObject.MakeCylinderEx(topRadius,bottomRadius,height:single;stacks,slices:integer; DrawTop,DrawBottom:Boolean);
var
  fv1,cv1,pv1 : integer;
  fv2,cv2,pv2 : integer;
  angstart,angstop,steph,stepv : extended;
  h : single;
  V1, V2, N1 : TAffineVector;
  i,j:integer;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Extended;
   uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1 : Single;
  y,rad1,rad2:extended;
begin
   h := height /2.0;
   AngStart:=DegToRad(0);
   AngStop:=DegToRad(360);
   StepH:=(AngStop - AngStart) / Slices;
   StepV:=height / Stacks;

  //if cysides in parts then
  //begin
   Phi:=h;
   Phi2:=Phi-StepV;
   uTexFactor:=1/Slices;
   vTexFactor:=1/Stacks;

   for J:=0 to Stacks-1 do begin
      Theta:=AngStart;
      rad1 := topradius+((bottomradius-topradius)/stacks)*j;
      rad2 := topradius+((bottomradius-topradius)/stacks)*(j+1);
      V1.V[1]:= phi;//   -h+(j*stepv);//  SinP;
      V2.V[1]:= phi2;//-h+((j+1)*stepv);//  SinP2;
      vTexCoord0:=1-j*vTexFactor;
      vTexCoord1:=1-(j+1)*vTexFactor;

      pv1 := -1;
      cv1 := -1;

      pv2 := -1;
      cv2 := -1;
      for i:=0 to Slices do begin

         SinCos(Theta, SinT, CosT);
         V1.V[0]:=Sint * rad1;//SinT; x
         V1.V[2]:=Cost * rad1;//CosT;  z

         V2.V[0]:=Sint * rad2;// SinT;
         V2.V[2]:=cost * rad2;//CosT;

         uTexCoord:=i*uTexFactor;
         TexCoords.Add(uTexCoord, vTexCoord0);
         Normals.Add(v1);
         pv1 := cv1;
         cv1 := addVertice(v1.X,v1.Y,v1.Z);

         TexCoords.Add(uTexCoord, vTexCoord1);
         Normals.Add(v2);
         pv2 := cv2;
         cv2 :=addVertice(v2.X,v2.Y,v2.Z);

         if pv1 <> -1 then
         begin
            AddQuadFace(pv1,pv2,cv2,cv1);
         end;

         Theta:=Theta+StepH;
      end;
      Phi:=Phi2;
      Phi2:=Phi2 - StepV;
   end;
 // end;
      if ((DrawBottom) and  (bottomRadius>0)) then
      begin
      TexCoords.add(0.5,0.5);
      fv1 := addVertice(0,-h,0);
      N1:=YVector;
      V1.V[1]:=-h;
      Theta:=AngStop;
      cv1 := -1;
      pv1 := -1;
      for I:=0 to Slices do begin
         SinCos(Theta, SinT, CosT);
         V1.V[0]:=SinT*bottomradius;
         V1.V[2]:=CosT*bottomradius;
         TexCoords.Add(SinT*0.5+0.5, CosT*0.5+0.5);
         Normals.add(n1);
         pv1 := cv1;
         cv1 := addVertice(v1.X,v1.Y,v1.Z);
         if pv1 <> -1 then
         begin
           addQuadFace(fv1,pv1,cv1,fv1);
         end;
         Theta:=Theta - StepH;
      end;
      end;

      if ((DrawTop) and  (topRadius>0)) then
      begin
      TexCoords.add(0.5,0.5);
      fv1 := addVertice(0,h,0);
      N1:=YVector;
      V1.V[1]:=h;
      Theta:=AngStop;
      cv1 := -1;
      pv1 := -1;
      for I:=0 to Slices do begin
         SinCos(Theta, SinT, CosT);
         V1.V[0]:=SinT*topradius;
         V1.V[2]:=CosT*topradius;
         TexCoords.Add(SinT*0.5+0.5, CosT*0.5+0.5);
         Normals.add(n1);
         pv1 := cv1;
         cv1 := addVertice(v1.X,v1.Y,v1.Z);
         if pv1 <> -1 then
         begin
            addQuadFace(fv1,cv1,pv1,fv1);
         end;
         Theta:=Theta - StepH;
      end;
      end;

END;

procedure TGLCustomEditingMeshObject.MakeCone(radius,height:single;stacks,slices:integer);
begin
  MakeCylinderEx(0,radius,height,stacks,slices,false,true);
end;

procedure TGLCustomEditingMeshObject.MakeCylinder(radius,height:single;stacks,slices:integer);
begin
  MakeCylinderEx(radius,radius,height,stacks,slices,true,true);
end;

procedure TGLCustomEditingMeshObject.MakePyramid(radius,height:single;stacks:integer);
begin
  MakeCylinderEx(0,radius,height,stacks,4,false,true);
end;

procedure TGLCustomEditingMeshObject.MakeGrid(width,depth: single;stacks,slices:integer);
var
  i,j,vertCnt,faceCnt : integer;
  stepx,stepz,midx,midz,z,x:single;
  cv1,pv1 : integer;
  cv2,pv2 : integer;
  uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1 : Single;
  V1, V2 : TAffineVector;
begin
  vertCnt:=0;
  midx :=width / 2.0;
  midz :=depth / 2.0;
  StepX:=width / Slices;
  StepZ:=depth / Stacks;
  uTexFactor:=1/Slices;
  vTexFactor:=1/Stacks;
  V1.V[1]:= 0;
  V2.V[1]:= 0;
  x:=-midx;
  z:=midz;
	for  i := 0 to stacks-1 do
  begin
    pv1 := -1;
    cv1 := -1;
    pv2 := -1;
    cv2 := -1;
    vTexCoord0:=1-i*vTexFactor;
    vTexCoord1:=1-(i+1)*vTexFactor;

		for j := 0 to slices do
    begin

      V1.V[0]:= x;
      V1.V[2]:= z;

      V2.V[0]:=x;
      V2.V[2]:=z-stepZ;

      uTexCoord:=i*uTexFactor;
      //TexCoords.Add(uTexCoord, vTexCoord0);
      Normals.Add(0,1,0);
      pv1 := cv1;
      cv1 := addVertice(v1.X,v1.Y,v1.Z);

      //TexCoords.Add(uTexCoord, vTexCoord1);
      Normals.Add(0,1,0);
      pv2 := cv2;
      cv2 :=addVertice(v2.X,v2.Y,v2.Z);

      if pv1 <> -1 then
      begin
         AddQuadFace(pv1,pv2,cv2,cv1);
      end;
     // z:=z-stepZ;
     // x:=x+stepX;
      x:=x+stepX;
		end;
    x:=-midx;
    z:=z-stepZ;
  end;
end;

procedure TGLCustomEditingMeshObject.MakeDisk(innerradius,outerradius,startangle,sweepangle:single;loops,slices:integer);
var
  fv1,cv1,pv1 : integer;
  fv2,cv2,pv2 : integer;
  rad1,rad2,angstart,angstop,steph,stepv : extended;
  uvrad1,uvrad2,uvstep :extended;
  i,j:integer;
  V1, V2, N1 : TAffineVector;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Extended;
begin

   AngStart:=DegToRad(startangle);
   AngStop:=DegToRad(sweepangle);
   StepH:=(AngStop - AngStart) / Slices;
   StepV:=(outerradius-innerradius) / loops;
   uvstep := 0.5 / loops;

   N1:=YVector;
//   n1[1] := -n1[1];
   V1.V[2]:=0;
   V2.V[2]:=0;
   rad1 := outerradius;
   for J:=0 to loops-1 do
   begin
    rad2 := rad1-stepv;
    Theta:=AngStop;
    cv1 := -1;
    pv1 := -1;
    cv2 := -1;
    pv2 := -1;
    uvrad1 := (uvstep)*j;
    uvrad2 := (uvstep)*(j+1);

    for i := 0 to slices do
    begin
         SinCos(Theta, SinT, CosT);
         V1.V[1]:=SinT*rad1;
         V1.V[0]:=CosT*rad1;
         V2.V[1]:=SinT*rad2;
         V2.V[0]:=CosT*rad2;
//         mo.TexCoords.Add(SinT*0.5+0.5, CosT*0.5+0.5);
         TexCoords.Add(SinT*uvrad1+0.5, CosT*uvrad1+0.5);
         TexCoords.Add(SinT*uvrad2+0.5, CosT*uvrad2+0.5);
         Normals.add(n1);
         Normals.add(n1);

         pv1 := cv1;
         cv1 := addVertice(v1);
         pv2 := cv2;
         cv2 := addVertice(v2);

         if pv1 <> -1 then
         begin
            AddQuadFace(pv1,pv2,cv2,cv1);
         end;

         Theta:=Theta - StepH;
    end;
    rad1 := rad1-stepv;
   end;
end;

procedure TGLCustomEditingMeshObject.MakeTube(innerradius,outerradius,height:single;stacks,slices:integer);
begin
   // Top
   MakeDisk(innerradius,outerradius,0,360,1,slices);
   // MoveAtTop
   // Inner
   MakeCylinderEx(innerRadius,InnerRadius,height,stacks,slices,false,false);
   // Outer
   MakeCylinderEx(outerRadius,outerRadius,height,stacks,slices,false,false);
   //Bottom
   MakeDisk(innerradius,outerradius,0,360,1,slices);
   // MoveAtBottom
end;

procedure TGLCustomEditingMeshObject.MakeTorus(InnerRadius,outerRadius : single;slices,rings : integer);
var
  i, j, ip0,ip1,ip2,ip3 : integer;
  theta, phi, theta1, phi1 : Single;
  p0, p1, p2, p3 : TAffineVector;
  n0, n1, n2, n3 : TAffineVector;
begin
  for i := 0 to rings - 1 do
  begin
    theta := i * c2PI / rings;
    theta1 := (i + 1) * c2PI / rings;
    for j := 0 to Slices - 1 do
    begin
      phi := j *c2PI / Slices;
      phi1 := (j + 1) * c2PI / Slices;

		  p0.x := cos(theta) * (OuterRadius + innerRadius * cos(phi));
      p0.y := -sin(theta) * (OuterRadius + innerRadius * cos(phi));
      p0.z := innerRadius * sin(phi);
      n0.X := cos(theta) * (cos(phi));
      n0.Y := -sin(theta) * (cos(phi));
      n0.Z := sin(phi);
      Normals.Add(n0.x,n0.y,n0.z);
      ip0:=AddVertice(p0);

		  p1.X := cos(theta1) * (OuterRadius + innerRadius * cos(phi));
      p1.Y := -sin(theta1) * (OuterRadius + innerRadius * cos(phi));
      p1.Z := innerRadius * sin(phi);
      n1.X := cos(theta1) * (cos(phi));
      n1.Y := -sin(theta1) * (cos(phi));
      n1.Z := sin(phi);
      Normals.Add(n1.x,n1.y,n1.z);
      ip1:=AddVertice(p1);

		  p2.X := cos(theta1) * (OuterRadius + innerRadius * cos(phi1));
      p2.Y := -sin(theta1) * (OuterRadius + innerRadius * cos(phi1));
      p2.Z := innerRadius * sin(phi1);
      n2.X := cos(theta1) * (cos(phi1));
      n2.Y := -sin(theta1) * (cos(phi1));
      n2.Z := sin(phi1);
      Normals.Add(n2.x,n2.y,n2.z);
      ip2:=AddVertice(p2);

      p3.X := cos(theta) * (OuterRadius + innerRadius * cos(phi1));
      p3.Y := -sin(theta) * (OuterRadius + innerRadius * cos(phi1));
      p3.Z := innerRadius * sin(phi1);
      n3.X := cos(theta) * (cos(phi1));
      n3.Y := -sin(theta) * (cos(phi1));
      n3.Z := sin(phi1);
      Normals.Add(n3.x,n3.y,n3.z);
      ip3:=AddVertice(p3);

      AddQuadFace(ip0,ip1,ip2,ip3);

    end;
  end;
end;

procedure TGLCustomEditingMeshObject.SetMaterial(AVAlue:String);
begin
 FFaceList.MaterialName := 'LibMaterial';
end;

procedure TGLCustomEditingMeshObject.WeldVertex;
begin
end;
end.

