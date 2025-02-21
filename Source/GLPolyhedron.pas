//
// The graphics engine GLXEngine. The unit of LZScene for Lazarus
//
{
   Standard polyhedrons.
}
unit GLPolyhedron;

interface

uses
  Classes,
  GLScene, 
  GLVectorGeometry, 
  GLRenderContextInfo;

type
  { A Dodecahedron.
     The dodecahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TGLDodecahedron = class(TGLSceneObject)
  public
     
    procedure BuildList(var rci: TGLRenderContextInfo); override;
  end;
  { A Icosahedron.
     The icosahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TGLIcosahedron = class(TGLSceneObject)
  public
     
    procedure BuildList(var rci: TGLRenderContextInfo); override;
  end;

  // TGLOctahedron
  //
  { A Octahedron.
     The octahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TGLOctahedron = class(TGLSceneObject)
  public
     
    procedure BuildList(var rci: TGLRenderContextInfo); override;
  end;

  // TGLTetrahedron
  //
  { A Tetrahedron.
     The tetrahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TGLTetrahedron = class(TGLSceneObject)
  public
     
    procedure BuildList(var rci: TGLRenderContextInfo); override;
  end;


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses GLObjects;

// ------------------
// ------------------ TGLDodecahedron ------------------
// ------------------

// BuildList
//

procedure TGLDodecahedron.BuildList(var rci: TGLRenderContextInfo);
begin
  DodecahedronBuildList;
end;

// ------------------
// ------------------ TGLIcosahedron ------------------
// ------------------

// BuildList
//

procedure TGLIcosahedron.BuildList(var rci: TGLRenderContextInfo);
begin
  IcosahedronBuildList;
end;

//--------------------
//--------------------  TGLOctahedron ------------------------
//--------------------

// BuildList
//
procedure TGLOctahedron.BuildList(var rci: TGLRenderContextInfo);
begin
  OctahedronBuildList;
end;

//--------------------
//--------------------  TGLTetrahedron ------------------------
//--------------------

// BuildList
//
procedure TGLTetrahedron.BuildList(var rci: TGLRenderContextInfo);
begin
  TetrahedronBuildList;
end;

initialization
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  RegisterClasses([TGLDodecahedron, TGLIcosahedron, TGLOctahedron, TGLTetrahedron]);

end.

