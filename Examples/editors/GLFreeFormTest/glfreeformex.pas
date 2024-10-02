// GLFreeFromEx
{: Extend TGLFreeForm for GLScene.<p>
  <p>
  TGLFreeFormEx : Extend the GLFreeFrom for a enhanced Editing

  Original Author: J.Delauney.
  Contributed to the GLScene community.

  <b>Credits : </b><font size=-1><ul>
    <li> Build Functions by : 05/04/04 - GAK - Created, based on GLMeshbuilder, kept separate in case of problems
        Version is currently very rough, distinct problems around the normals (as you can see
        I've pretty much just lifted the buildlist of the original object in many cases).
    <li>
        Currently covers basic geometry :
          GLCube
          GLSphere
          GLPlane
          GLFrustrum
          GLCylinder
          GLCone
          GLDisk
          GLDodecahedron
  </ul></font>

	<b>History : </b><font size=-1><ul>
      <li>10/11/15 - EG - Creation
	</ul></font>
}
unit GLfreeFormEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, graphics,
  // GLS
  GLScene, OpenGLTokens, GLPersistentClasses, GLObjects, GLGeomObjects,
  GLVectorTypes,  GLVectorGeometry, GLVectorLists, GLVectorFileObjects,
  GLMeshUtils,GLMeshOptimizer, GLMeshCSG;

Type
  TGLFreeFormEx = class;
  TArrayOfFreeFrom = Array of TGLFreeForm;
  TAxis = 0..2;
Type
  TGLFreeFormEx = class(TGLFreeForm)
    private
    protected
    public
      {Create new TMeshObject }
      function CreateMeshObject: TMeshObject;
      { Add a TMeshObject }
      procedure AddMesh(AMesh:TMeshObject);
      { Import All MeshObjects From a TGLFreeForm }
      procedure ImportFromFreeForm(AFreeForm:TGLFreeForm);
      { Extract All Mesh Object in an array of TGLFreeFormEx }
      //function ExtractMeshObjects(out count):TArrayOfFreeFrom;

      { Merge with an another TGLFreeForm (all meshes object are merged together) }
      procedure MergeWith(const AFreeForm: TGLFreeForm);
      { Merge all mesh objects in the TGLFreeForm into One}
     // procedure Merge;

      {Build Cube }
      procedure BuildMeshCube(width,height,depth:single;parts:TCubeParts);
      {Build Sphere }
      procedure BuildMeshSphere(radius:single;stacks,slices,start,stop,top,bottom:integer;topcap,bottomcap:TCapType);
      {Build Plane }
      procedure BuildMeshPlane(width,height:single;xtiles,ytiles:integer;xoffset,xscope,yoffset,yscope:single;style:TGLPlaneStyles);
      {Build Frustrum }
      procedure BuildMeshFrustrum(height,apexheight,basedepth,basewidth:single;parts:TFrustrumParts);
      {Build Cylinder }
      procedure BuildMeshCylinder(topradius,bottomradius,height:single;stacks,slices:integer;parts:TCylinderParts);
      {Build Dodecahedron }
      //procedure BuildMeshDodecahedron();
      {Build Disk }
      procedure BuildMeshDisk(innerradius,outerradius,startangle,sweepangle:single;loops,slices:integer);
      {Build MeshObject from TGLSceneObject }
      procedure BuildMeshObjectFrom(source:TGLSceneObject);
      {Build Mesh From Bitmap aka Terrain }
      procedure BuildMeshFromBitmap(ABitmap : TBitmap; resx, resy : Integer; Ascale : TAffineVector);
      { CSG Support Operations with internals Meshes Objects}
      procedure ProcessCSG(obj1, obj2: String; Operation: TCSGOperation;DeleteSource:Boolean=False);
      { CSG Support Operation with an internal Mesh Object and an External Mesh Object }
      procedure ProcessCSG(obj1 : String; obj2: TMeshObject; Operation: TCSGOperation; DeleteSource:Boolean=False);overload;

      //procedure ExtrudeFace(MeshObject:TMeshObject;FaceIdx:Integer;ExtrudePos:TVector);
      //procedure AddVertex(MeshObject:TMeshObject;x,y,z:single);
      //procedure MakeFace(MeshObject:TMeshObject;VertexIdx1,VertexIdx2,VertexIdx3:Integer);
      //procedure MakeQuadFace(MeshObject:TMeshObject;VertexIdx1,VertexIdx2,VertexIdx3,VertexIdx4:Integer);

      // procedure WeldPointsMesh(MeshObject : TMeshObject;aWeldDistance: Single=0.0000001; aThreshold: Single=35.0);
      // procedure WeldPoints(aWeldDistance: Single=0.0000001; aThreshold: Single=35.0);
      {Subdivide a Mesh Object }
      procedure SubdivideMesh(AMeshObject:TMeshObject;SubdivideValue:Integer);
      {Subdivide  All Meshes Objects in the FreeFrom  }
      procedure Subdivide(SubdivideValue:Integer);
      {Strip redundent data, recalculate normals and faces}
      procedure StripAndRecalc;
      {Smooth Faces of a Mesh Object and Weld Points}
      procedure FacesSmoothMesh(aMeshObj: TMeshObject; aWeldDistance: Single=0.0000001; aThreshold: Single=35.0; InvertNormals:boolean=false);
      {Smooth Faces of All Meshes Objects in the FreeForm }
      procedure FacesSmooth(aWeldDistance: Single=0.0000001; aThreshold: Single=35.0; InvertNormals:boolean=false);
      {Optimize a Mesh Object}
      procedure OptimizeMesh(aMeshObject : TMeshObject; options : TMeshOptimizerOptions);
      {Optimize All Meshes Objects in the FreeFrom }
      procedure Optimize(options : TMeshOptimizerOptions);



      { prepare Facegroup for UVMapping }
      procedure PrepareUVMapping;
      { Make Planar UVMapping }
      procedure MakeUVPlanarMapping(MeshObject:TMeshObject; min,max : TAffineVector; Axis: TAxis = 2);
      { Make Cube UVMapping }
      procedure MakeUVCubicMapping(MeshObject:TMeshObject; min,max : TAffineVector; Axis: TAxis = 2);
      { Make Cylindrical UVMapping }
      procedure MakeUVCylindricalMapping(MeshObject:TMeshObject; min,max : TAffineVector; Axis: TAxis = 2; MaxUDiff: Single = 0.75);
      { Make Sphere UVMapping }
      procedure MakeUVSphericalMapping(MeshObject:TMeshObject; min,max : TAffineVector; Axis: TAxis = 2; MaxUDiff: Single = 0.75);

      {Return True if an Object collide with the FreeFrom }
      function IsCollideWith(AnObject:TGLSceneObject;out tests,Hits:Integer):Boolean;

      //procedure MakeSymmetricMesh(AMeshObject:TMeshObject;AnAxe:TAxis = 2);
      //procedure MakeSymmetric(AnAxe:TAxis = 2);

      // procedure SetMeshMaterial(MeshObject:TMeshObject;LibMaterial:TGLLibMaterial;MaterialName:String);

      //function getMeshVertices
      //function getMeshNormals
      //function getMeshTextCoords
      //function getFaceGroups
      //function getMeshFaceCount;
      //function getFaceCount;

  end;

implementation

uses math;

function TGLFreeFormEx.CreateMeshObject:TMeshObject;
var
  mo : TMeshObject;
begin
  mo :=TMeshObject.CreateOwned(self.MeshObjects);
  mo.Mode := momFaceGroups;
  result := mo;
end;

procedure TGLFreeFormEx.AddMesh(AMesh:TMeshobject);
begin
  MeshObjects.Add(AMesh);
end;

procedure TGLFreeFormEx.ImportFromFreeForm(AFreeForm:TGLFreeForm);
Var i:Integer;
begin
  For i:= 0 to AFreeForm.MeshObjects.Count-1 do
  begin
    AddMesh(AFreeForm.MeshObjects.Items[i]);
  end;
end;

//function TGLFreeFormEx.ExtractMeshObjects(out count):TArrayOfFreeFromEx;
//begin
//
//end;

procedure TGLFreeFormEx.BuildMeshCube(width,height,depth:single;parts:TCubeParts);
var
  fg : TFGVertexIndexList;
  w,h,d:single;
  fv : integer;
  mo : TMeshObject;

  procedure AddFaceGroup(st:integer);
  begin
     fg.add(st);
     fg.add(st+1);
     fg.add(st+2);
     fg.add(st+2);
     fg.add(st+3);
     fg.add(st);
  end;

begin

  mo:=CreateMeshObject;

  w := width/2.0;
  h := height/2.0;
  d := depth/2.0;
  FG := TFGVertexIndexList.CreateOwned(mo.FaceGroups);


  // front
  if cpFront in Parts then
  begin
    mo.Normals.add(0,0,d);
    fv := mo.Vertices.add(-w,-h, d);
        mo.Vertices.add( w,-h, d);
        mo.Vertices.add( w, h, d);
        mo.Vertices.add(-w, h, d);
        mo.TexCoords.Add(0, 0);
        mo.TexCoords.Add(1, 0);
      mo.TexCoords.Add(1, 1);
      mo.TexCoords.Add(0, 1);
    AddFaceGroup(fv);
  end;

  // back
  if cpback in Parts then
  begin
    mo.Normals.add(0,0,-d);
    fv := mo.Vertices.add(-w,-h,-d);
        mo.Vertices.add(-w, h,-d);
        mo.Vertices.add( w, h,-d);
        mo.Vertices.add( w,-h,-d);
        mo.TexCoords.Add(1, 0);
        mo.TexCoords.Add(1, 1);
        mo.TexCoords.Add(0, 1);
        mo.TexCoords.Add(0, 0);
    AddFaceGroup(fv);
  end;

  // top
  if cptop in Parts then
  begin
    mo.Normals.add(0,d,0);
    mo.Normals.add(0,d,0);
    fv := mo.Vertices.add(-w, h,-d);
        mo.Vertices.add(-w, h, d);
        mo.Vertices.add( w, h, d);
        mo.Vertices.add( w, h,-d);
        mo.TexCoords.Add(0, 1);
        mo.TexCoords.Add(0, 0);
        mo.TexCoords.Add(1, 0);
        mo.TexCoords.Add(1, 1);
    AddFaceGroup(fv);
  end;

  // bottom
  if cpbottom in Parts then
  begin
    mo.Normals.add(0,-d,0);
    fv := mo.Vertices.add(-w,-h,-d);
        mo.Vertices.add( w,-h,-d);
        mo.Vertices.add( w,-h, d);
        mo.Vertices.add(-w,-h, d);
        mo.TexCoords.Add(1, 1);
        mo.TexCoords.Add(0, 1);
        mo.TexCoords.Add(0, 0);
        mo.TexCoords.Add(1, 0);
    AddFaceGroup(fv);
  end;

  // right
  if cpright in Parts then
  begin
    mo.Normals.add(d,0,d);
    fv := mo.Vertices.add( w,-h,-d);
        mo.Vertices.add( w, h,-d);
        mo.Vertices.add( w, h, d);
        mo.Vertices.add( w,-h, d);
        mo.TexCoords.Add(1, 0);
        mo.TexCoords.Add(1, 1);
        mo.TexCoords.Add(0, 1);
        mo.TexCoords.Add(0, 0);
    AddFaceGroup(fv);
  end;

  // left
  if cpleft in Parts then
  begin
    mo.Normals.add(-d,0,0);
    fv := mo.Vertices.add(-w,-h,-d);
        mo.Vertices.add(-w,-h, d);
        mo.Vertices.add(-w, h, d);
        mo.Vertices.add(-w, h,-d);
        mo.TexCoords.Add(0, 0);
        mo.TexCoords.Add(1, 0);
        mo.TexCoords.Add(1, 1);
        mo.TexCoords.Add(0, 1);
    AddFaceGroup(fv);
  end;
end;

procedure TGLFreeFormEx.BuildMeshSphere(radius:single;stacks,slices,start,stop,top,bottom:integer;topcap,bottomcap:TCapType);
var
  mo : TMeshObject;
  fg : TFGVertexIndexList;
  fv1,cv1,pv1 : integer;
  fv2,cv2,pv2 : integer;

   V1, V2, N1 : TAffineVector;
   AngTop, AngBottom, AngStart, AngStop, StepV, StepH : Extended;
   SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Extended;
   uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1 : Single;
   I, J: Integer;
   DoReverse: Boolean;
begin

  mo:=CreateMeshObject;

  FG := TFGVertexIndexList.CreateOwned(mo.FaceGroups);
   DoReverse:=false;
   // common settings
   AngTop:=DegToRad(top);
   AngBottom:=DegToRad(Bottom);
   AngStart:=DegToRad(Start);
   AngStop:=DegToRad(Stop);
   StepH:=(AngStop - AngStart) / Slices;
   StepV:=(AngTop - AngBottom) / Stacks;

   if (Top < 90) and (TopCap in [ctCenter, ctFlat]) then
   begin
      SinCos(AngTop, SinP, CosP);
      sinp := sinp;// * radius;
      cosp := cosp;// * radius;
      mo.TexCoords.Add(0.5,0.5);

      if DoReverse then
         mo.Normals.Add(0,-1,0)
      else mo.Normals.add(0,1,0);

      mo.Normals.Add(0,1,0); //??
      if TopCap = ctCenter then
        fv1 := mo.Vertices.Add(0,0,0)
      else
      begin
         fv1 := mo.vertices.add(0,sinp,0);
         N1:=YVector;

         if DoReverse then
            N1.V[1]:=-N1.V[1];
      end;

      V1.V[1]:=SinP;//*radius;
      Theta:=AngStart;
      pv1 := -1;
      cv1 := -1;
      for I:=0 to Slices do
      begin
         SinCos(Theta, SinT, CosT);
         V1.V[0]:=CosP*SinT;//*radius;
         V1.V[2]:=CosP*CosT;//*radius;
         if TopCap=ctCenter then
         begin
            N1:=VectorPerpendicular(YVector, V1);
            //if DoReverse then NegateVector(N1);
         end;
         pv1 := cv1;
         cv1 := mo.Vertices.Add(v1);
         if pv1 <> -1 then
         begin
            fg.Add(fv1);
            fg.Add(pv1);
            fg.add(cv1);
         end;
         mo.Normals.Add(n1);
         mo.TexCoords.Add(SinT*0.5+0.5, CosT*0.5+0.5);

         Theta:=Theta + StepH;
      end;

   end;

   // main body
   Phi:=AngTop;
   Phi2:=Phi-StepV;
   uTexFactor:=1/Slices;
   vTexFactor:=1/Stacks;

   for J:=0 to Stacks-1 do begin
      Theta:=AngStart;
      SinCos(Phi, SinP, CosP);
      SinCos(Phi2, SinP2, CosP2);
      V1.V[1]:=SinP;
      V2.V[1]:=SinP2;
      vTexCoord0:=1-j*vTexFactor;
      vTexCoord1:=1-(j+1)*vTexFactor;

      pv1 := -1;
      cv1 := -1;
      pv2 := -1;
      cv2 := -1;
      for i:=0 to Slices do begin

         SinCos(Theta, SinT, CosT);
         V1.V[0]:=CosP * SinT;
         V2.V[0]:=CosP2 * SinT;
         V1.V[2]:=CosP * CosT;
         V2.V[2]:=CosP2 * CosT;

         uTexCoord:=i*uTexFactor;
         mo.TexCoords.Add(uTexCoord, vTexCoord0);
         if DoReverse then begin
            N1:=VectorNegate(V1);
            mo.Normals.Add(n1);
         end
         else
         begin
            mo.Normals.Add(v1);
         end;
         pv1 := cv1;
         cv1 := mo.Vertices.Add(v1);

         mo.TexCoords.Add(uTexCoord, vTexCoord1);
         if DoReverse then
         begin
            N1:=VectorNegate(V2);
            mo.Normals.Add(n1);
         end
         else
         begin

            mo.Normals.Add(v2);
         end;
         pv2 := cv2;
         cv2 := mo.Vertices.Add(v2);

         if pv1 <> -1 then
         begin
            fg.Add(pv1);
            fg.Add(cv2);
            fg.add(cv1);
            fg.Add(cv2);
            fg.Add(pv1);
            fg.add(pv2);
         end;

         Theta:=Theta+StepH;
      end;
      Phi:=Phi2;
      Phi2:=Phi2 - StepV;
   end;

   // bottom cap
   if (Bottom > -90) and (BottomCap in [ctCenter, ctFlat]) then
   begin
      SinCos(AngBottom, SinP, CosP);
      mo.TexCoords.add(0.5,0.5);
      if DoReverse then
         mo.Normals.add(0,1,0)
      else mo.Normals.add(0,-1,0);
      if BottomCap = ctCenter then
         fv1 := mo.Vertices.add(0,0,0)
      else
      begin
         fv1 := mo.Vertices.add(0,sinp,0);
         if DoReverse then
            MakeVector(N1, 0, -1, 0)
         else N1:=YVector;
      end;
      V1.V[1]:=SinP;
      Theta:=AngStop;
      cv1 := -1;
      pv1 := -1;
      for I:=0 to Slices do begin
         SinCos(Theta, SinT, CosT);
         V1.V[0]:=CosP * SinT;
         V1.V[2]:=CosP * CosT;
         if TopCap = ctCenter then
         begin
            N1:=VectorPerpendicular(AffineVectorMake(0, -1, 0), V1);
            if DoReverse then NegateVector(N1);
         end;
         mo.TexCoords.Add(SinT*0.5+0.5, CosT*0.5+0.5);
         mo.Normals.add(n1);

         pv1 := cv1;
         cv1 := mo.Vertices.Add(v1);

         if pv1 <> -1 then
         begin
            fg.Add(fv1);
            fg.Add(pv1);
            fg.add(cv1);
         end;
         Theta:=Theta - StepH;
      end;
   end;

end;

procedure TGLFreeFormEx.BuildMeshPlane(width,height:single;xtiles,ytiles:integer;xoffset,xscope,yoffset,yscope:single;style:TGLPlaneStyles);
var
  mo : TMeshObject;
  fg : TFGVertexIndexList;
  fv1,cv1,pv1 : integer;
  fv2,cv2,pv2 : integer;

   hw, hh, posXFact, posYFact, pX, pY0, pY1 : TGLFloat;
   tx0, tx1, ty0, ty1, texSFact, texTFact : TGLFloat;
   texS, texT0, texT1 : TGLFloat;
   x, y : Integer;
begin
  mo:=CreateMeshObject;
  FG := TFGVertexIndexList.CreateOwned(mo.FaceGroups);

   hw:=Width*0.5;
   hh:=Height*0.5;

   mo.Normals.Add(zvector);

   // determine tex coords extents
   if psTileTexture in Style then
   begin
      tx0:=XOffset;
      tx1:=XTiles*XScope+XOffset;
      ty0:=YOffset;
      ty1:=YTiles*YScope+YOffset;
   end
   else
   begin
      tx0:=0;
      ty0:=tx0;
      tx1:=XScope;
      ty1:=YScope;
   end;

   if psSingleQuad in Style then
   begin
      mo.TexCoords.Add(tx1,ty1);
      mo.Vertices.add(hw,hh);
      mo.TexCoords.Add(tx0,ty1);
      mo.Vertices.add(-hw,hh);
      mo.TexCoords.Add(tx0,ty0);
      mo.Vertices.add(-hw,-hh);
      mo.TexCoords.Add(tx1,ty0);
      mo.Vertices.add(hw,-hh);
      fg.Add(1);
      fg.Add(2);
      fg.Add(0);
      fg.Add(0);
      fg.Add(2);
      fg.Add(3);
   end
   else
   begin
      // multi-quad plane (actually built from tri-strips)
      texSFact:=(tx1-tx0)/XTiles;
      texTFact:=(ty1-ty0)/YTiles;
      posXFact:=Width/XTiles;
      posYFact:=Height/YTiles;
      texT0:=0;
      pY0:=-hh;
      for y:=0 to YTiles-1 do
      begin
         texT1:=(y+1)*texTFact;
         pY1:=(y+1)*posYFact-hh;

         cv1 := -1;
         cv2 := -1;
         for x:=0 to XTiles do
         begin
            texS:=tx0+x*texSFact;
            pX:=x*posXFact-hw;
            mo.TexCoords.add(texs,text1);
            pv1 := cv1;
            cv1 := mo.Vertices.Add(px,py1);
            mo.TexCoords.add(texs,text0);
            pv2 := cv2;
            cv2 := mo.Vertices.add(px,py0);

            if pv1 <> -1 then
            begin
              fg.Add(pv1);
              fg.Add(cv2);
              fg.add(cv1);
              fg.Add(cv2);
              fg.Add(pv1);
              fg.add(pv2);
            end;
         end;
         texT0:=texT1;
         pY0:=pY1;
      end;
   end;
end;

procedure TGLFreeFormEx.BuildMeshFrustrum(height,apexheight,basedepth,basewidth:single;parts:TFrustrumParts);
var
  mo : TMeshObject;
  fg : TFGVertexIndexList;
  fv1,cv1,pv1 : integer;

  HBW, HBD: TGLFloat; // half of width, half of depth at base
  HTW, HTD: TGLFloat; // half of width, half of depth at top of frustrum
  Sign: TGLFloat;     // +1 or -1
  Angle: TGLFloat;    // in radians
  ASin, ACos: TGLFloat;
begin
  mo := CreateMeshObject;
  FG := TFGVertexIndexList.CreateOwned(mo.FaceGroups);


//  if FNormalDirection = ndInside then
//    Sign := -1
//  else
    Sign := 1;
  HBW := BaseWidth * 0.5;
  HBD := BaseDepth * 0.5;
  HTW := HBW * (ApexHeight - Height) / ApexHeight;
  HTD := HBD * (ApexHeight - Height) / ApexHeight;


  if [fpFront, fpBack] * Parts <> [] then
  begin
    Angle := Arctan(ApexHeight/ HBD); // angle of front plane with bottom plane
    SinCos(Angle, ASin, ACos);
    if fpFront in Parts then
    begin
      mo.Normals.Add(0,Sign * ACos, Sign * ASin);
      mo.TexCoords.Add(xytexpoint); fv1 := mo.Vertices.add( HTW, Height, HTD);
      mo.TexCoords.Add(ytexpoint);         mo.Vertices.add(-HTW, Height, HTD);
      mo.TexCoords.Add(nulltexpoint);      mo.Vertices.add(-HBW, 0, HBD);
      mo.TexCoords.Add(xtexpoint);         mo.Vertices.add( HBW, 0, HBD);
      fg.Add(fv1);
      fg.Add(fv1+1);
      fg.Add(fv1+2);
      fg.Add(fv1+2);
      fg.Add(fv1+3);
      fg.Add(fv1+0);
    end;

    if fpBack in Parts then
    begin
      mo.Normals.Add(0, Sign * ACos, -Sign * ASin);
      mo.TexCoords.Add(ytexpoint); fv1 := mo.Vertices.add( HTW, Height,-HTD);
      mo.TexCoords.Add(nulltexpoint);     mo.Vertices.add( HBW, 0,-HBD);
      mo.TexCoords.Add(xtexpoint);        mo.Vertices.add(-HBW, 0,-HBD);
      mo.TexCoords.Add(xytexpoint);       mo.Vertices.add(-HTW, Height,-HTD);
      fg.Add(fv1);
      fg.Add(fv1+1);
      fg.Add(fv1+2);
      fg.Add(fv1+2);
      fg.Add(fv1+3);
      fg.Add(fv1+0);
    end;
  end;

  if [fpLeft, fpRight] * Parts <> [] then
  begin
    Angle := Arctan(ApexHeight/ HBW); // angle of side plane with bottom plane
    SinCos(Angle, ASin, ACos);
    if fpLeft in Parts then
    begin
      mo.Normals.Add(-Sign * ASin, Sign * ACos, 0);
      mo.TexCoords.Add(xytexpoint); fv1 := mo.Vertices.add(-HTW, Height, HTD);
      mo.TexCoords.Add(ytexpoint);         mo.Vertices.add(-HTW, Height,-HTD);
      mo.TexCoords.Add(nulltexpoint);      mo.Vertices.add(-HBW, 0,-HBD);
      mo.TexCoords.Add(xtexpoint);         mo.Vertices.add(-HBW, 0, HBD);
      fg.Add(fv1);
      fg.Add(fv1+1);
      fg.Add(fv1+2);
      fg.Add(fv1+2);
      fg.Add(fv1+3);
      fg.Add(fv1+0);
    end;

    if fpRight in Parts then
    begin
      mo.Normals.Add(Sign * ASin, Sign * ACos, 0);
      mo.TexCoords.Add(ytexpoint); fv1 := mo.Vertices.add( HTW, Height,HTD);
      mo.TexCoords.Add(nulltexpoint);     mo.Vertices.add( HBW, 0,HBD);
      mo.TexCoords.Add(xtexpoint);        mo.Vertices.add( HBW, 0,-HBD);
      mo.TexCoords.Add(xytexpoint);       mo.Vertices.add( HTW, Height,-HTD);
      fg.Add(fv1);
      fg.Add(fv1+1);
      fg.Add(fv1+2);
      fg.Add(fv1+2);
      fg.Add(fv1+3);
      fg.Add(fv1+0);

    end;
  end;

  if (fpTop in Parts) and (Height < ApexHeight) then
  begin
      mo.Normals.Add(0, Sign, 0);
      mo.TexCoords.Add(ytexpoint);    fv1 := mo.Vertices.add(-HTW, Height,-HTD);
      mo.TexCoords.Add(nulltexpoint);        mo.Vertices.add(-HTW, Height, HTD);
      mo.TexCoords.Add(xtexpoint);           mo.Vertices.add( HTW, Height, HTD);
      mo.TexCoords.Add(xytexpoint);          mo.Vertices.add( HTW, Height,-HTD);
      fg.Add(fv1);
      fg.Add(fv1+1);
      fg.Add(fv1+2);
      fg.Add(fv1+2);
      fg.Add(fv1+3);
      fg.Add(fv1+0);
  end;

  if fpBottom in Parts then
  begin
      mo.Normals.Add(0, -Sign, 0);
      mo.TexCoords.Add(nulltexpoint);  fv1 := mo.Vertices.add(-HBW, 0,-HBD);
      mo.TexCoords.Add(xtexpoint);            mo.Vertices.add( HBW, 0,-HBD);
      mo.TexCoords.Add(xytexpoint);           mo.Vertices.add( HBW, 0, HBD);
      mo.TexCoords.Add(ytexpoint);            mo.Vertices.add(-HBW, 0, HBD);
      fg.Add(fv1);
      fg.Add(fv1+1);
      fg.Add(fv1+2);
      fg.Add(fv1+2);
      fg.Add(fv1+3);
      fg.Add(fv1+0);

  end;


end;

procedure TGLFreeFormEx.BuildMeshCylinder(topradius,bottomradius,height:single;stacks,slices:integer;parts:TCylinderParts);
var
  mo : TMeshObject;
  fg : TFGVertexIndexList;
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
  mo := CreateMeshObject;
  FG := TFGVertexIndexList.CreateOwned(mo.FaceGroups);
   h := height /2.0;

   AngStart:=DegToRad(0);
   AngStop:=DegToRad(360);
   StepH:=(AngStop - AngStart) / Slices;
   StepV:=height / Stacks;

  if cysides in parts then
  begin
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
         mo.TexCoords.Add(uTexCoord, vTexCoord0);
         mo.Normals.Add(v1);
         pv1 := cv1;
         cv1 := mo.Vertices.Add(v1);

         mo.TexCoords.Add(uTexCoord, vTexCoord1);
         mo.Normals.Add(v2);
         pv2 := cv2;
         cv2 := mo.Vertices.Add(v2);

         if pv1 <> -1 then
         begin
            fg.Add(pv1);
            fg.Add(cv2);
            fg.add(cv1);

            fg.Add(cv2);
            fg.Add(pv1);
            fg.add(pv2);
         end;

         Theta:=Theta+StepH;
      end;
      Phi:=Phi2;
      Phi2:=Phi2 - StepV;
   end;
  end;

  if cybottom in parts then
  begin
      mo.TexCoords.add(0.5,0.5);
      fv1 := mo.Vertices.add(0,-h,0);
      N1:=YVector;
      V1.V[1]:=-h;
      Theta:=AngStop;
      cv1 := -1;
      pv1 := -1;
      for I:=0 to Slices do begin
         SinCos(Theta, SinT, CosT);
         V1.V[0]:=SinT*bottomradius;
         V1.V[2]:=CosT*bottomradius;
         mo.TexCoords.Add(SinT*0.5+0.5, CosT*0.5+0.5);
         mo.Normals.add(n1);
         pv1 := cv1;
         cv1 := mo.Vertices.Add(v1);
         if pv1 <> -1 then
         begin
            fg.Add(fv1);
            fg.Add(pv1);
            fg.add(cv1);
         end;
         Theta:=Theta - StepH;
      end;
  end;

  if cytop in parts then
  begin
      mo.TexCoords.add(0.5,0.5);
      fv1 := mo.Vertices.add(0,h,0);
      N1:=YVector;
      V1.V[1]:=h;
      Theta:=AngStop;
      cv1 := -1;
      pv1 := -1;
      for I:=0 to Slices do begin
         SinCos(Theta, SinT, CosT);
         V1.V[0]:=SinT*topradius;
         V1.V[2]:=CosT*topradius;
         mo.TexCoords.Add(SinT*0.5+0.5, CosT*0.5+0.5);
         mo.Normals.add(n1);
         pv1 := cv1;
         cv1 := mo.Vertices.Add(v1);
         if pv1 <> -1 then
         begin
            fg.Add(fv1);
            fg.Add(cv1);
            fg.add(pv1);
         end;
         Theta:=Theta - StepH;
      end;
  end;

END;

//procedure TGLFreeFormEx.BuildMeshDodecahedron();
//const
//   A = 1.61803398875*0.3; // (Sqrt(5)+1)/2
//   NA = -1.61803398875*0.3;
//   B = 0.61803398875*0.3; // (Sqrt(5)-1)/2
//   NB = 0.61803398875*0.3;
//   C = 1*0.3;
//   NC = -1*0.3;
//const
//   vertices : packed array [0..19] of packed array [0..2] of single = //TAffineVector =
//      ((NA,  0,  B), (NA,  0, NB), ( A,  0, NB), (A,  0,  B),
//       ( B, NA,  0), (NB, NA,  0), (NB,  A,  0), (B,  A,  0),
//       ( 0,  B, NA), ( 0, NB, NA), ( 0, NB,  A), (0,  B,  A),
//       (NC, NC,  C), (NC, NC, NC), ( C, NC, NC), (C, NC,  C),
//       (NC,  C,  C), (NC,  C, NC), ( C,  C, NC), (C,  C,  C));
//
//   polygons : packed array [0..11] of packed array [0..4] of Byte =
//      (( 0, 12, 10, 11, 16),  ( 1, 17,  8,  9, 13),
//       ( 2, 14,  9,  8, 18),  ( 3, 19, 11, 10, 15),
//       ( 4, 14,  2,  3, 15),  ( 5, 12,  0,  1, 13),
//       ( 6, 17,  1,  0, 16),  ( 7, 19,  3,  2, 18),
//       ( 8, 17,  6,  7, 18),  ( 9, 14,  4,  5, 13),
//       (10, 12,  5,  4, 15),  (11, 19,  7,  6, 16));
//
//var
//  mo : TMeshObject;
//  fg : TFGVertexIndexList;
//  fv1,cv1,pv1 : integer;
//  fv2,cv2,pv2 : integer;
//
//   i, j : Integer;
//   n : TAffineVector;
//   faceIndices : PByteArray;
//begin
//  mo := CreateMeshObject;
//  FG := TFGVertexIndexList.CreateOwned(mo.FaceGroups);
//
//   for i:=0 to 11 do begin
//      faceIndices:=@polygons[i, 0];
//
//      n:=CalcPlaneNormal(vertices[faceIndices[0]],
//                         vertices[faceIndices[1]],
//                         vertices[faceIndices[2]]);
//      mo.Normals.Add(n);
//      for j:=0 to 4 do
//      begin
//         cv1 := mo.Vertices.add(vertices[faceIndices.V[j]]);  ;
//         if j = 0 then fv1 := cv1;
//      end;
//      fg.add(fv1);
//      fg.add(fv1+1);
//      fg.add(fv1+2);
//
//      fg.add(fv1);
//      fg.add(fv1+2);
//      fg.add(fv1+3);
//
//      fg.add(fv1);
//      fg.add(fv1+3);
//      fg.add(fv1+4);
//
//      fg.add(fv1);
//      fg.add(fv1+1);
//      fg.add(fv1+2);
//
//   end;
//end;

procedure TGLFreeFormEx.BuildMeshDisk(innerradius,outerradius,startangle,sweepangle:single;loops,slices:integer);
var
  mo : TMeshObject;
  fg : TFGVertexIndexList;
  fv1,cv1,pv1 : integer;
  fv2,cv2,pv2 : integer;
  rad1,rad2,angstart,angstop,steph,stepv : extended;
  uvrad1,uvrad2,uvstep :extended;
  i,j:integer;
  V1, V2, N1 : TAffineVector;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Extended;
begin
   mo := CreateMeshObject;
   FG := TFGVertexIndexList.CreateOwned(mo.FaceGroups);

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
         mo.TexCoords.Add(SinT*uvrad1+0.5, CosT*uvrad1+0.5);
         mo.TexCoords.Add(SinT*uvrad2+0.5, CosT*uvrad2+0.5);
         mo.Normals.add(n1);
         mo.Normals.add(n1);

         pv1 := cv1;
         cv1 := mo.Vertices.Add(v1);
         pv2 := cv2;
         cv2 := mo.Vertices.Add(v2);
         if pv1 <> -1 then
         begin
            fg.Add(pv1);
            fg.Add(cv2);
            fg.add(cv1);
            fg.Add(cv2);
            fg.Add(pv1);
            fg.add(pv2);
         end;
         Theta:=Theta - StepH;
    end;
    rad1 := rad1-stepv;
   end;
end;

procedure  TGLFreeFormEx.BuildMeshObjectFrom(source:TGLSceneObject);
var
  cp : TCylinderParts;
Begin

  if source is TGLCube then
  begin
      with source as tglcube do
      begin
        BuildMeshCube(cubewidth,cubeheight,CubeDepth,parts);
      end;
  end;

  if source is TGLsphere then
  begin
      with source as tglsphere do
      begin
        BuildMeshsphere(Radius,Stacks,Slices,Start,Stop,top,bottom,topcap,bottomcap);
        // temp fix, until I work out why the radius is not working in the creation routine
        self.scale.X := self.scale.x*radius;
        self.scale.y := self.scale.y*radius;
        self.scale.z := self.scale.z*radius;
      end;
  end;

  if source is tglplane then
  begin
    with source as tglplane do
    begin
      buildmeshplane(width,height,xtiles,ytiles,xoffset,xscope,yoffset,yscope,style);
    end;
  end;

  if source is tglfrustrum then
  begin
    with source as tglfrustrum do
    begin
      buildmeshfrustrum(height,ApexHeight,BaseDepth,BaseWidth,parts);
    end;
  end;

  if source is tglcone then
  begin
    with source as tglcone do
    begin
      cp := [];
      if cosides in parts then cp := cp + [cySides];
      if cobottom in parts then cp := cp + [cyBottom];
      buildmeshcylinder(0,BottomRadius,Height,Stacks,Slices,cp);
    end;
  end;

  if source is tglcylinder then
  begin
    with source as tglcylinder do
    begin
      buildmeshcylinder(topradius,BottomRadius,Height,Stacks,Slices,parts);
    end;
  end;

  //if source is TGLDodecahedron then
  //begin
  //  with source as TGLDodecahedron do
  //  begin
  //    buildmeshdodecahedron();
  //  end;
  //end;

  if source is TGLdisk then
  begin
    with source as TGLdisk do
    begin
      buildmeshdisk(InnerRadius,OuterRadius,StartAngle,SweepAngle,Loops,Slices);
    end;
  end;

  self.StructureChanged;
end;

// Scale value = AffineVectorMake(4/bmp.Width, 4/bmp.Width, 1/1024)
procedure TGLFreeFormEx.BuildMeshFromBitmap(ABitmap : TBitmap; resx, resy : Integer;ascale : TAffineVector) ;
var
  mo:TMeshObject;
  i, j, x, y : Integer;
  col, colx, coly, colxy : Byte;
  fg : TFGVertexIndexList;
  xr, yr, c : Single;
begin
  mo:= self.CreateMeshObject();

  // Build the vertices
  for j:=0 to resy-1 do begin
    for i:=0 to resx-1 do begin
      // Nearest neighbour
      x:=Round(i*ABitmap.Width/resx);
      y:=Round(j*ABitmap.Height/resy);
      col:=ABitmap.Canvas.Pixels[x,y] and $FF;

      // Add the vertex
      mo.Vertices.Add((i-(resx div 2))*ascale.V[0],
                          (j-(resy div 2))*ascale.V[1],
                          col*ascale.V[2]);
    end;
  end;

  // Create the facegroup and build the vertex indices
  fg:=TFGVertexIndexList.CreateOwned(mo.FaceGroups);
  for j:=0 to resy-2 do begin
    for i:=0 to resx-2 do begin
      fg.VertexIndices.Add(i+resx*j, (i+1)+resx*j, (i+1)+resx*(j+1));
      fg.VertexIndices.Add(i+resx*j, (i+1)+resx*(j+1), i+resx*(j+1));
    end;
  end;

  // Build the normals
  mo.BuildNormals(fg.VertexIndices, momTriangles);
end;

procedure TGLFreeFormEx.StripAndRecalc;
var
  lTrigList,
  lNormals    : TAffineVectorList;
  lIndices    : TIntegerList;
  lObj        : TMeshObject;
  lStrips     : TPersistentObjectList;

  lFaceGroup  : TFGVertexIndexList;
  i           : Integer;
begin
  // Extract raw triangle data to work with.
  lTrigList := MeshObjects.ExtractTriangles;

  // Builds a vector-count optimized indices list.
  lIndices := BuildVectorCountOptimizedIndices(lTrigList);
  // Alter reference/indice pair and removes unused reference values.
  RemapAndCleanupReferences(lTrigList, lIndices);
   // Calculate normals.
  lNormals := BuildNormals(lTrigList, lIndices);

  // Strip where posible.
  lStrips := StripifyMesh(lIndices, lTrigList.Count, True);

  // Clear current mesh object data.
  MeshObjects.Clear;

  // Setup new mesh object.
  lObj := TMeshObject.CreateOwned(MeshObjects);
  lObj.Vertices := lTrigList;
  lObj.Mode := momFaceGroups;
  lObj.Normals := lNormals;

  for i:=0 to lStrips.Count-1 do
  begin
    lFaceGroup := TFGVertexIndexList.CreateOwned(lObj.FaceGroups);
    lFaceGroup.VertexIndices := (lStrips[i] as TIntegerList);
    if i > 0 then
      lFaceGroup.Mode := fgmmTriangleStrip
    else
      lFaceGroup.Mode := fgmmTriangles;
    lFaceGroup.MaterialName:=IntToStr(i and 15);
  end;
  // Redraw freeform
  Self.StructureChanged;

  lTrigList.Free;
  lNormals.Free;
  lIndices.Free;
end;

procedure TGLFreeFormEx.FacesSmoothMesh(aMeshObj: TMeshObject; aWeldDistance: Single=0.0000001; aThreshold: Single=35.0; InvertNormals:boolean=false);
begin
 GLMeshOptimizer.FacesSmooth(aMeshObj,aWeldDistance,aThreshold,InvertNormals);
end;

procedure TGLFreeFormEx.FacesSmooth(aWeldDistance: Single=0.0000001; aThreshold: Single=35.0; InvertNormals:boolean=false);
Var i:Integer;
begin
  for i:=0 to MeshObjects.Count-1 do begin
       FacesSmoothMesh(MeshObjects.Items[i], aWeldDistance, aThreshold, InvertNormals);
    end;
end;

procedure TGLFreeFormEx.OptimizeMesh(aMeshObject : TMeshObject; options : TMeshOptimizerOptions);
begin
 GLMeshOptimizer.OptimizeMesh(aMeshObject,options);
end;

procedure TGLFreeFormEx.Optimize(options : TMeshOptimizerOptions);
begin
 GLMeshOptimizer.OptimizeMesh(Self.MeshObjects,options);
end;

procedure TGLFreeFormEx.ProcessCSG(obj1, obj2: String; Operation: TCSGOperation;DeleteSource:Boolean=False);
Var
  objM1,ObjM2,Resm : TMeshObject;
begin
  objM1:=MeshObjects.FindMeshByName(obj1);
  objM2:=MeshObjects.FindMeshByName(obj2);
  ResM := CreateMeshObject;
  CSG_Operation(objM1, objM2, Operation, ResM,'','');
  If DeleteSource then
  begin
    FreeAndNil(objM1);
    FreeAndNil(objM2);
  end;
end;

procedure TGLFreeFormEx.ProcessCSG(obj1: String; obj2:TMeshObject; Operation: TCSGOperation;DeleteSource:Boolean=False);
Var
  objM1,Resm : TMeshObject;
begin
  objM1:=MeshObjects.FindMeshByName(obj1);
  ResM := CreateMeshObject;
  CSG_Operation(objM1, obj2, Operation, ResM,'','');
  If DeleteSource then
  begin
    FreeAndNil(objM1);
  end;
end;

procedure TGLFreeFormEx.MergeWith(const AFreeForm: TGLFreeForm);//const MatLib: TGLMaterialLibrary);
var
  I,
  //matIdx,
  iMeshIdx: Integer;
  stream: TMemoryStream;
  lobj: TMeshObject;
 // mat: TGLLibMaterial;
begin
 // Assert(Assigned(ASource), 'MergeFreeFormTo: ASource not Assigned.');
  Assert(Assigned(AFreeForm), 'MergeFreeFormTo: ADest not Assigned.');
 // Assert(Assigned(MatLib), 'MergeFreeFormTo: MatLib not Assigned.');
 // matIdx := Matlib.Materials.Count;
  stream := TMemoryStream.Create;
  try

      for iMeshIdx := 0 to AFreeForm.MeshObjects.Count - 1 do
      begin
        stream.Clear;
        AFreeForm.MeshObjects.Items[iMeshIdx].SaveToStream(stream);
        lobj := TMeshObject.CreateOwned(Self.MeshObjects);
        stream.Seek(0, 0);
        lobj.LoadFromStream(stream);

        lobj.Vertices.TransformAsVectors(AFreeForm.AbsoluteMatrix);
        lobj.Normals.TransformAsVectors(AFreeForm.AbsoluteMatrix);
        lobj.Translate(AFreeForm.Position.AsAffineVector);
        //mat := MatLib.Materials.Add;
        //mat.Assign(MeshObjects.Items[iMeshIdx].FaceGroups[0].MaterialCache);
        //mat.Name := 'Material_' + IntToStr(matIdx);
        Self.StructureChanged;
  //      lobj.FaceGroups[0].MaterialName := mat.Name;
  //        lobj.FaceGroups[0].PrepareMaterialLibraryCache(MatLib);
  //      Self.StructureChanged;
  //      inc(matIdx);
      end;
  finally
    stream.free; stream := nil;
  end;
end;

procedure TGLFreeFormEx.SubdivideMesh(AMeshObject:TMeshObject;SubdivideValue:Integer); //2,4,8,16,32,64....
var
   i, j : Integer;
   tris, norms, tex, buf, morphTris, morphNorms : TAffineVectorList;
   indices, texIndices : TIntegerlist;
   firstRemap, subdivideRemap, bufRemap : TIntegerList;
   t : Int64;
begin

      tex:=TAffineVectorList.Create;

      tris:=AMeshObject.ExtractTriangles(tex);

      indices:=BuildVectorCountOptimizedIndices(tris);
      firstRemap:=TIntegerList(indices.CreateClone);
      RemapAndCleanupReferences(tris, indices);

      norms:=BuildNormals(tris, indices);

      // subdivide geometry
      SubdivideTriangles(SubdivideValue*0.1, tris, indices, norms);

      texIndices:=BuildVectorCountOptimizedIndices(tex);
      RemapAndCleanupReferences(tex, texIndices);

      // subdivide texture space
      SubdivideTriangles(0, tex, texIndices);

      // Re-expand everything
      buf:=TAffineVectorList.Create;
      try
         ConvertIndexedListToList(tris, indices, buf);
         tris.Assign(buf);
         buf.Count:=0;
         ConvertIndexedListToList(norms, indices, buf);
         norms.Assign(buf);
         buf.Count:=0;
         ConvertIndexedListToList(tex, texIndices, buf);
         tex.Assign(buf);
      finally
         buf.Free;
      end;

      // Pack & Optimize the expanded stuff
      indices.Free;
      indices:=BuildVectorCountOptimizedIndices(tris, norms, tex);
      subdivideRemap:=TIntegerList(indices.CreateClone);
      RemapReferences(norms, indices);
      RemapReferences(tex, indices);
      RemapAndCleanupReferences(tris, indices);

      IncreaseCoherency(indices, 13);

      with AMeshObject as TMorphableMeshObject do
      begin

         bufRemap:=TIntegerList.Create;
         for j:=0 to MorphTargets.Count-1 do
         begin
            MorphTo(j);

            morphTris:=ExtractTriangles;
            bufRemap.Assign(firstRemap);
            RemapAndCleanupReferences(morphTris, bufRemap);

            morphNorms:=GLMeshUtils.BuildNormals(morphTris, bufRemap);

            SubdivideTriangles(SubdivideValue*0.1, morphTris, bufRemap, morphNorms);

            buf:=TAffineVectorList.Create;
            try
               ConvertIndexedListToList(morphTris, bufRemap, buf);
               morphTris.Assign(buf);
               ConvertIndexedListToList(morphNorms, bufRemap, buf);
               morphNorms.Assign(buf);
            finally
               buf.Free;
            end;
            RemapReferences(morphTris, subdivideRemap);
            RemapReferences(morphNorms, subdivideRemap);

            MorphTargets[j].Vertices:=morphTris;
            MorphTargets[j].Normals:=morphNorms;

            morphTris.Free;
            morphNorms.Free;
         end;
         bufRemap.Free;

         Vertices:=tris;
         Normals:=norms;
         TexCoords:=tex;
         FaceGroups.Clear;
         with TFGVertexIndexList.CreateOwned(FaceGroups) do
         begin
            VertexIndices:=indices;
            Mode:=fgmmTriangles;
         end;
      end;

      texIndices.Free;
      subdivideRemap.Free;
      firstRemap.Free;
      tex.Free;
      indices.Free;
      norms.Free;
      tris.Free;


   Self.StructureChanged;
end;

procedure TGLFreeFormEx.Subdivide(SubdivideValue:Integer); //2,4,8,16,32,64....
var
   i, j : Integer;
   tris, norms, tex, buf, morphTris, morphNorms : TAffineVectorList;
   indices, texIndices : TIntegerlist;
   firstRemap, subdivideRemap, bufRemap : TIntegerList;
   t : Int64;
begin

   for i:=0 to MeshObjects.Count-1 do
   begin
      tex:=TAffineVectorList.Create;
      with MeshObjects[i] do
      begin
         tris:=ExtractTriangles(tex);
      end;
      indices:=BuildVectorCountOptimizedIndices(tris);
      firstRemap:=TIntegerList(indices.CreateClone);
      RemapAndCleanupReferences(tris, indices);

      norms:=BuildNormals(tris, indices);

      // subdivide geometry
      SubdivideTriangles(SubdivideValue*0.1, tris, indices, norms);

      texIndices:=BuildVectorCountOptimizedIndices(tex);
      RemapAndCleanupReferences(tex, texIndices);

      // subdivide texture space
      SubdivideTriangles(0, tex, texIndices);

      // Re-expand everything
      buf:=TAffineVectorList.Create;
      try
         ConvertIndexedListToList(tris, indices, buf);
         tris.Assign(buf);
         buf.Count:=0;
         ConvertIndexedListToList(norms, indices, buf);
         norms.Assign(buf);
         buf.Count:=0;
         ConvertIndexedListToList(tex, texIndices, buf);
         tex.Assign(buf);
      finally
         buf.Free;
      end;

      // Pack & Optimize the expanded stuff
      indices.Free;
      indices:=BuildVectorCountOptimizedIndices(tris, norms, tex);
      subdivideRemap:=TIntegerList(indices.CreateClone);
      RemapReferences(norms, indices);
      RemapReferences(tex, indices);
      RemapAndCleanupReferences(tris, indices);

      IncreaseCoherency(indices, 13);

      with MeshObjects[i] do
      begin// as TMorphableMeshObject do begin

         bufRemap:=TIntegerList.Create;
         //for j:=0 to MorphTargets.Count-1 do begin
         //   MorphTo(j);

            morphTris:=ExtractTriangles;
            bufRemap.Assign(firstRemap);
            RemapAndCleanupReferences(morphTris, bufRemap);

            morphNorms:=GLMeshUtils.BuildNormals(morphTris, bufRemap);

            SubdivideTriangles(SubdivideValue*0.1, morphTris, bufRemap, morphNorms);

            buf:=TAffineVectorList.Create;
            try
               ConvertIndexedListToList(morphTris, bufRemap, buf);
               morphTris.Assign(buf);
               ConvertIndexedListToList(morphNorms, bufRemap, buf);
               morphNorms.Assign(buf);
            finally
               buf.Free;
            end;
            RemapReferences(morphTris, subdivideRemap);
            RemapReferences(morphNorms, subdivideRemap);

            //MorphTargets[j].
            Vertices:=morphTris;
            //MorphTargets[j].
            Normals:=morphNorms;
            TexCoords:=tex;
            FaceGroups.Clear;
            with TFGVertexIndexList.CreateOwned(FaceGroups) do begin
               VertexIndices:=indices;
               Mode:=fgmmTriangles;
            end;
            morphTris.Free;
            morphNorms.Free;
         end;
         bufRemap.Free;

        // Vertices:=tris;
        // Normals:=norms;

      end;

      texIndices.Free;
      subdivideRemap.Free;
      firstRemap.Free;
      tex.Free;
      indices.Free;
      norms.Free;
      tris.Free;


   Self.StructureChanged;
end;

function TGLFreeFormEx.ISCollideWith(AnObject:TGLSceneObject;out tests,Hits:Integer):Boolean;
const
  cResolution = 32;
var
  x, y, z :Integer; //Hits, Tests : integer;
  step : single;
  Point : TVector;
  brad : single;
begin
  // Note - this could be speeded up enourmously by using a proprietary method
  // instead of OctreePointInMesh - which is recalculated for each node where
  // it could be calculated once per column (cutting down run time by aprox.
  // 1/cResolution).

  // Increasing the bounding sphere radius to generate a box that's guaranteed
  // to encompas the entire Object
  brad := AnObject.BoundingSphereRadius*1.42;
  step := brad/(cResolution+1);
  Hits := 0; Tests := 0;
  result:=false;
  for x := 0 to cResolution-1 do
  begin
    for y := 0 to cResolution-1 do
      for z := 0 to cResolution-1 do
      begin
        Point := VectorAdd(AnObject.Position.AsVector, VectorMake(x*step-brad/2, y*step-brad/2, z*step-brad/2));
        inc(Tests);
        if OctreePointInMesh(Point) then
        begin
          inc(Hits);
          result:=true;
        end;
      end;
  end;
end;


procedure TGLFreeFormEx.PrepareUVMapping;
var
  fg : TFGVertexNormalTexIndexList;
  i,j,m : integer;
begin
  for m:=0 to MeshObjects.Count-1 do
  with MeshObjects.Items[m] do
  begin
    for i:=0 to FaceGroups.Count-1 do
    begin
      if (FaceGroups.Items[i] is TFGVertexIndexList) then
      begin
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

procedure TGLFreeFormEx.MakeUVPlanarMapping(MeshObject:TMeshObject; min,max : TAffineVector; Axis: TAxis = 2);
var
  i,j : integer;
  P,center : TAffineVector;
  u,v : single;
  fg : TFGVertexNormalTexIndexList;
begin
  with MeshObject do
  begin
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
  end;
end;

procedure TGLFreeFormEx.MakeUVCubicMapping(MeshObject:TMeshObject; min,max : TAffineVector; Axis: TAxis = 2);
var
  Plane : array[0..5] of TAffineVector;
  i,j,k : integer;
  P,N,center : TAffineVector;
  u,v,dp,MaxDotProd,uoff,voff : single;
  PlaneIdx : integer;
  fg  : TFGVertexNormalTexIndexList;
  temp : single;
begin
  with MeshObject do
  begin
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
  end;
end;

procedure TGLFreeFormEx.MakeUVCylindricalMapping(MeshObject:TMeshObject; min,max : TAffineVector; Axis: TAxis = 2; MaxUDiff: Single = 0.75);
var
  i,j,k       : integer;
  P,Pn,center : TAffineVector;
  triU        : array[0..2] of single;
  u,v         : single;
  fg          : TFGVertexNormalTexIndexList;
begin
  with MeshObject do
  begin
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
  end;
end;

procedure TGLFreeFormEx.MakeUVSphericalMapping(MeshObject:TMeshObject; min,max : TAffineVector; Axis: TAxis = 2; MaxUDiff: Single = 0.75);
var
  i,j,k    : integer;
  P,center : TAffineVector;
  triU     : array[0..2] of single;
  u,v      : single;
  fg       : TFGVertexNormalTexIndexList;
begin
  with MeshObject do
  begin
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
  end;
end;

//procedure TForm1.GetTexCoordsWireframe;
//var
//  i,j,x,y,x0,y0 : integer;
//  fg : TFGVertexNormalTexIndexList;
//begin
//  Image1.Canvas.FillRect(Rect(0,0,Image1.Width,Form1.Image1.Height));
//  Image1.Canvas.Pen.Color:=clBlack;
//  with Form1.GLFreeForm1.MeshObjects.Items[0] do begin
//    for i:=0 to FaceGroups.Count-1 do begin
//      fg:=TFGVertexNormalTexIndexList(FaceGroups.Items[i]);
//      for j:=0 to fg.TexCoordIndices.Count-1 do begin
//        if j=0 then begin
//          x0:=round(TexCoords.Items[fg.TexCoordIndices.Items[j]].X*Form1.Image1.Width);
//          y0:=round(TexCoords.Items[fg.TexCoordIndices.Items[j]].Y*Form1.Image1.Height);
//          Image1.Canvas.MoveTo(x0,Image1.Height-y0);
//        end else begin
//          x:=round(TexCoords.Items[fg.TexCoordIndices.Items[j]].X*Form1.Image1.Width);
//          y:=round(TexCoords.Items[fg.TexCoordIndices.Items[j]].Y*Form1.Image1.Height);
//          Image1.Canvas.LineTo(x,Image1.Height-y);
//        end;
//      end;
//      Image1.Canvas.LineTo(x0,Image1.Height-y0);
//    end;
//  end;
//end;


Initialization
  RegisterClasses([TGLFreeFormEx]);
end.

