//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Code for loading animated MDC files into GLScene FreeForms
   and Actors.

  This file format uses in Return To Castle Wolfenstein instead
  of MD3 files. It has got all MD3 features (such as Tag frames)
  plus very small data!

  Original code by Osman Turan (osmanturancom@yahoo.com)

   History :
   06/06/10 - Yar - Fixes for Linux x64
   08/11/09 - DaStr - Compatibility fix for FPC
  (thanks Predator) (BugtrackerID = 2893580)
   16/10/08 - UweR - Compatibility fix for Delphi 2009
   31/03/07 - DaStr - Added $I GLScene.inc
   11/05/04 - SG - Added to CVS
   07/02/04 - OT - Creation (Osman Turan)

}
unit GLFileMDC;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,
   
  GLVectorFileObjects, GLMaterial, GLApplicationFileIO,
  GLVectorGeometry;

const
  MDCFILE_IDENTITY = 'IDPC';
  MDCFILE_VERSION = 2;

  MDC_BASEVERTEX_FACTOR = 0.015625; // 1/64;
  MDC_COMPVERTEX_FACTOR = 0.046875; // 3/64;

type
  TMDCPoint = array [0 .. 2] of Single;
  TMDCAngle = TMDCPoint;

  TMDCFileHeader = packed record
    Ident: array [0 .. 3] of AnsiChar;
    Version: Cardinal;
    Name: array [0 .. 63] of AnsiChar;
    Flags: Cardinal;
    NumFrames: Cardinal;
    NumTags: Cardinal;
    NumSurfaces: Cardinal;
    NumSkins: Cardinal;
    OffsetBorderFrames: Cardinal;
    OffsetTagNames: Cardinal;
    OffsetTagFrames: Cardinal;
    OffsetSurfaces: Cardinal;
    OffsetEnd: Cardinal;
  end;

  TMDCBorderFrame = packed record
    BBMin, BBMax: TMDCPoint;
    LocalOrigin: TMDCPoint;
    Radius: Single;
    Name: array [0 .. 15] of AnsiChar;
  end;

  PMDCTagName = ^TMDCTagName;

  TMDCTagName = packed record
    Name: array [0 .. 63] of AnsiChar;
  end;

  PMDCTagFrame = ^TMDCTagFrame;

  TMDCTagFrame = packed record
    TagPosition: array [0 .. 2] of Word; // or ShortInt?
    TagAngle: array [0 .. 2] of Word; // or ShortInt?
  end;

  TMDCTag = packed record
    TagName: PMDCTagName;
    TagFrame: PMDCTagFrame;
  end;

  TMDCSurfaceHeader = packed record
    Ident: array [0 .. 3] of AnsiChar;
    Name: array [0 .. 63] of AnsiChar;
    Flags: Cardinal;
    NumCompFrames: Cardinal;
    NumBaseFrames: Cardinal;
    NumSkins: Cardinal;
    NumVertices: Cardinal;
    NumTriangles: Cardinal;
    OffsetTriangles: Cardinal;
    OffsetSkins: Cardinal;
    OffsetTexCoords: Cardinal;
    OffsetBaseVerts: Cardinal;
    OffsetCompVerts: Cardinal;
    OffsetFrameBaseFrames: Cardinal;
    OffsetFrameCompFrames: Cardinal;
    OffsetEnd: Cardinal;
  end;

  TMDCTriangle = array [0 .. 2] of Cardinal;

  TMDCSkin = packed record
    Shader: array [0 .. 63] of AnsiChar;
    Flags: Cardinal;
  end;

  TMDCTexCoord = array [0 .. 1] of Single;

  TMDCBaseVertex = array [0 .. 3] of SmallInt;

  TMDCCompVertex = array [0 .. 3] of Byte;

  TMDCBaseFrame = packed record
    BaseVertices: array of TMDCBaseVertex;
  end;

  TMDCCompFrame = packed record
    CompVertices: array of TMDCCompVertex;
  end;

type
  TGLMDCVectorFile = class(TGLVectorFile)
  public
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure LoadFromStream(AStream: TStream); override;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLMDCVectorFile ------------------
// ------------------

// Capabilities
//
class function TGLMDCVectorFile.Capabilities: TGLDataFileCapabilities;
begin
  Result := [DfcRead];
end;

// LoadFromStream
//
procedure TGLMDCVectorFile.LoadFromStream(AStream: TStream);

type
  PPackedNormal = ^TPackedNormal;
  TPackedNormal = array [0 .. 1] of Byte;

var
  I, J, K, NumVerts, Numtris: Integer;
  Mesh: TMorphableMeshObject;
  FaceGroup: TFGIndexTexCoordList;
  MorphTarget: TMeshMorphTarget;

  function UnpackNormal(Pn: TPackedNormal): TAffineVector;
  var
    Lat, Lng: Single;
  begin
    // The MDC normal is a latitude/longitude value that needs
    // to be calculated into cartesian space.
    Lat := (Pn[0]) * (2 * Pi) / 255;
    Lng := (Pn[1]) * (2 * Pi) / 255;
    Result.V[0] := Cos(Lat) * Sin(Lng);
    Result.V[1] := Sin(Lat) * Sin(Lng);
    Result.V[2] := Cos(Lng);
  end;

  procedure AllocateMaterial(Meshname: string);
  var
    LibMat: TGLLibMaterial;
  begin
    // If a material library is assigned to the actor/freeform the
    // mesh name will be added as a material.
    if Assigned(Owner.MaterialLibrary) then
      with Owner.MaterialLibrary do
      begin
        if Assigned(Materials.GetLibMaterialByName(Meshname)) then
          Exit;
        LibMat := Materials.Add;
        LibMat.Name := Meshname;
        LibMat.Material.Texture.Disabled := False;
      end;
  end;

var
  Fileheader: TMDCFileHeader;
  Surfheader: TMDCSurfaceHeader;
  Borderframes: array of TMDCBorderFrame;
  Baseframetable, Compframetable: array of Word;
  Baseframe: TMDCBaseFrame;
  Compframe: TMDCCompFrame;
  Xyz, Normal: TAffineVector;
  St: array of array [0 .. 1] of Single;
  Triangles: array of TMDCTriangle;
  FrameOffset: Cardinal;
begin
  AStream.Read(Fileheader, SizeOf(Fileheader));
  Assert(Fileheader.Ident = MDCFILE_IDENTITY, 'Incorrect MDC file Ident');
  Assert(Fileheader.Version = MDCFILE_VERSION, 'Incorrect MDC version number');

  try
    AStream.Seek(Fileheader.OffsetBorderFrames, SoFromBeginning);
    SetLength(Borderframes, Fileheader.NumFrames);
    AStream.Read(Borderframes[0], SizeOf(TMDCBorderFrame) *
      Fileheader.NumFrames);

    FrameOffset := Fileheader.OffsetSurfaces;

    for I := 0 to Fileheader.NumSurfaces - 1 do
    begin
      // read header
      AStream.Seek(FrameOffset, SoFromBeginning);
      AStream.Read(Surfheader, SizeOf(TMDCSurfaceHeader));

      // triangles for this surface
      SetLength(Triangles, Surfheader.NumTriangles);
      AStream.Seek(FrameOffset + Surfheader.OffsetTriangles, SoFromBeginning);
      AStream.Read(Triangles[0], SizeOf(TMDCTriangle) *
        Surfheader.NumTriangles);

      // texture coordinates for this surface
      SetLength(St, Surfheader.NumVertices);
      AStream.Seek(FrameOffset + Surfheader.OffsetTexCoords, SoFromBeginning);
      AStream.Read(St[0], 2 * SizeOf(Single) * Surfheader.NumVertices);

      // base frame table for this surface (for only loading)
      SetLength(Baseframetable, Fileheader.NumFrames);
      AStream.Seek(FrameOffset + Surfheader.OffsetFrameBaseFrames,
        SoFromBeginning);
      AStream.Read(Baseframetable[0], SizeOf(Word) * Fileheader.NumFrames);
      // compressed frame table for this surface (for only loading)
      SetLength(Compframetable, Fileheader.NumFrames);
      AStream.Seek(FrameOffset + Surfheader.OffsetFrameCompFrames,
        SoFromBeginning);
      AStream.Read(Compframetable[0], SizeOf(Word) * Fileheader.NumFrames);

      Mesh := TMorphableMeshObject.CreateOwned(Owner.MeshObjects);
      // easiest way to convert a char array to string ;)
      Mesh.Name := Trim(string(PChar(Surfheader.Name[0])));
      with Mesh do
      begin
        Mode := MomFaceGroups;
        FaceGroup := TFGIndexTexCoordList.CreateOwned(FaceGroups);
        with FaceGroup do
        begin
          AllocateMaterial(Mesh.Name);
          MaterialName := Mesh.Name;
          NumTris := Surfheader.NumTriangles;
          VertexIndices.Capacity := NumTris * 3;
          TexCoords.Capacity := NumTris * 3;
          // Get the vertex indices and texture coordinates
          for J := 0 to Surfheader.NumTriangles - 1 do
          begin
            Add(Triangles[J, 0], St[Triangles[J, 0]][0],
              1 - St[Triangles[J, 0]][1]);
            Add(Triangles[J, 2], St[Triangles[J, 2]][0],
              1 - St[Triangles[J, 2]][1]);
            Add(Triangles[J, 1], St[Triangles[J, 1]][0],
              1 - St[Triangles[J, 1]][1]);
          end;
        end;

        // Get the mesh data for each morph frame
        for J := 0 to Fileheader.NumFrames - 1 do
        begin
          MorphTarget := TMeshMorphTarget.CreateOwned(MorphTargets);
          MorphTarget.Name := Trim(string(Pchar(Surfheader.Name[0]))) + '[' +
            IntToStr(J) + ']';
          NumVerts := Surfheader.NumVertices;
          MorphTarget.Vertices.Capacity := NumVerts;

          // base frames
          SetLength(Baseframe.BaseVertices, Surfheader.NumVertices);
          AStream.Seek(FrameOffset + Surfheader.OffsetBaseVerts + Baseframetable
            [J] * Surfheader.NumVertices * 8, SoFromBeginning);
          AStream.Read(Baseframe.BaseVertices[0], SizeOf(TMDCBaseVertex) *
            Surfheader.NumVertices);

          // compressed frames
          if Compframetable[J] <> $FFFF then // is there a valid frame?
          begin
            SetLength(Compframe.CompVertices, Surfheader.NumVertices);
            AStream.Seek(FrameOffset + Surfheader.OffsetCompVerts +
              Compframetable[J] * Surfheader.NumVertices * 4, SoFromBeginning);
            AStream.Read(Compframe.CompVertices[0], SizeOf(TMDCCompVertex) *
              Surfheader.NumVertices);
          end;

          for K := 0 to Surfheader.NumVertices - 1 do
          begin
            Xyz.V[0] :=
              (Baseframe.BaseVertices[K, 0] * MDC_BASEVERTEX_FACTOR) +
              Borderframes[J].Localorigin[0];
            Xyz.V[1] :=
              (Baseframe.BaseVertices[K, 1] * MDC_BASEVERTEX_FACTOR) +
              Borderframes[J].Localorigin[1];
            Xyz.V[2] :=
              (Baseframe.BaseVertices[K, 2] * MDC_BASEVERTEX_FACTOR) +
              Borderframes[J].Localorigin[2];
            Normal := UnpackNormal
              (PPackedNormal(@Baseframe.BaseVertices[K, 3])^);

            if Compframetable[J] <> $FFFF then
            begin
              Xyz.V[0] := Xyz.V[0] +
                ((Compframe.CompVertices[K, 0] - 128) * MDC_COMPVERTEX_FACTOR);
              Xyz.V[1] := Xyz.V[1] +
                ((Compframe.CompVertices[K, 1] - 128) * MDC_COMPVERTEX_FACTOR);
              Xyz.V[2] := Xyz.V[2] +
                ((Compframe.CompVertices[K, 2] - 128) * MDC_COMPVERTEX_FACTOR);
              // FIXME:
              // I'm sure compframe.CompVertices[3] points a packed normal.
              // And it must be add the current normal like xyz.
              // But, I don't know a way to unpacked this value
              // I found a precalculated normal list in RTCW 1.41 mod source (q_math.c)
              //
              // NUMVERTEXNORMALS = 162
              // vec3_t bytedirs[NUMVERTEXNORMALS] = {
              // {-0.525731, 0.000000, 0.850651}, (...)
              //
              // But, I had noticed some compframe.CompVertices[3] value is bigger
              // than NUMVERTEXNORMALS constant. So, there must be another list.
              // Can you find it?
              // Osman Turan (osmanturancom@yahoo.com)
            end;

            // all id Sofware based games uses Z axis as up instead of Y. So, convert them
            MorphTarget.Vertices.Add(Xyz.V[0], Xyz.V[2], -Xyz.V[1]);
            MorphTarget.Normals.Add(Normal.V[0], Normal.V[2],
              -Normal.V[1]);
          end;
        end;
      end;

      FrameOffset := FrameOffset + Surfheader.OffsetEnd;

      if Mesh.MorphTargets.Count > 0 then
        Mesh.MorphTo(0);
    end;
  finally
    // save memory free space
    Borderframes := nil;
    Baseframetable := nil;
    Compframetable := nil;
    St := nil;
    Triangles := nil;
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

RegisterVectorFileFormat('mdc', 'MDC files', TGLMDCVectorFile);

end.
