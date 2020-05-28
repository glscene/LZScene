//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Quake2 MD2 vector file format implementation.

	 History : 
       17/01/14 - PW - Bugfixed for XE5
       16/01/14 - PW - Added $I GLScene.inc
       15/01/14 - PW - Creation
	 
}
unit GLFileDAE;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,
  GLVectorFileObjects, GLApplicationFileIO{, Fmx.FileDAE};

type
   // TGLDAEVectorFile
   //
   { The DAE vector file (COLLADA actor file).
      Stores a set of "frames" describing the different postures of the actor,
      it may be animated by TGLActor. The "Skin" must be loaded indepentendly
      (the whole mesh uses a single texture bitmap).}
   TGLDAEVectorFile = class(TGLVectorFile)
      public
          
         class function Capabilities : TGLDataFileCapabilities; override;
         procedure LoadFromStream(aStream : TStream); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLDAEVectorFile ------------------
// ------------------

// Capabilities
//
class function TGLDAEVectorFile.Capabilities : TGLDataFileCapabilities;
begin
   Result:=[dfcRead];
end;

// LoadFromStream
//
procedure TGLMD2VectorFile.LoadFromStream(aStream : TStream);
var
   i, j : Integer;
   DAEFile : TFileDAE;
   mesh : TMorphableMeshObject;
   faceGroup : TFGIndexTexCoordList;
   morphTarget : TMeshMorphTarget;
begin
   DAEFile:=TFileDAE.Create;
   DAEFile.LoadFromStream(aStream);
   try
      // retrieve mesh data
      mesh:=TMorphableMeshObject.CreateOwned(Owner.MeshObjects);
      with mesh, DAEFile do begin
         Mode:=momFaceGroups;
         faceGroup:=TFGIndexTexCoordList.CreateOwned(FaceGroups);
         with faceGroup do begin
            MaterialName:='';
            VertexIndices.Capacity:=iTriangles*3;
            TexCoords.Capacity:=iTriangles*3;
            // copy the face list
            for i:=0 to iTriangles-1 do with IndexList[i] do begin
               Add(a, a_s, -a_t);
               Add(b, b_s, -b_t);
               Add(c, c_s, -c_t);
            end;
         end;
         // retrieve frames data (morph targets)
         for i:=0 to iFrames-1 do begin
            morphTarget:=TMeshMorphTarget.CreateOwned(MorphTargets);
            with morphTarget do begin
               Name:='Frame'+IntToStr(i);
               Vertices.Capacity:=iVertices;
               for j:=0 to iVertices-1 do
                  Vertices.Add(VertexList[i][j]);
               BuildNormals(faceGroup.VertexIndices, momTriangles);
            end;
         end;
      end;
      if GetOwner is TGLActor then with TGLActor(GetOwner).Animations do begin
         Clear;
         with DAEFile do for i:=0 to frameNames.Count-1 do with Add do begin
            Name:=frameNames[i];
            Reference:=aarMorph;
            StartFrame:=Integer(frameNames.Objects[i]);
            if i<frameNames.Count-1 then
               EndFrame:=Integer(frameNames.Objects[i+1])-1
            else EndFrame:=iFrames-1;
         end;
      end;
      if mesh.MorphTargets.Count>0 then
         mesh.MorphTo(0);
   finally
      DAEFile.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('dae', 'COLLADA model files', TGLDAEVectorFile);

end.
