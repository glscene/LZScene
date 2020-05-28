//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  Polygonising a scalar field by construction of isosurfaces 

Algorithms
----------
Marching Cubes
 - Exploits a coarser Mesh then Marching Tetrahedra but produces less triangles
 - based on "Marching Cubes: A High Resolution 3D Surface
   Construction Algorithm" by W.E.Lorensen and H.E.Cline
 - patent free since 2005

Marching Tetrahedra
 - Finer Mesh, better feature preservation
 - based on "A new tetrahedral tesselation scheme for isosurface generation"
   by S.L.Chan and E.O.Purisima
 - patent free

Lookuptables
 - by Paul Bourke (http://paulbourke.net/geometry/polygonise/)

Overall
 - Simple Data Structures to store Mesh. Vertices are calculated and stored twice
   or even more often.

	 History :  
      05/08/12 - PW - Adapted to use with GLScene v.1.2 and later
	    12/06/04 - Wolf Blecher - Created, first implementation (Blechwolf@myrealbox.com)
	 
}

unit GLIsosurface;

interface

uses
  GLVectorGeometry;

{$I GLScene.inc}
  
type
  TSingle3DArray = array of array of array of Single;
  TVertexArray = array of TVertex;
  TIntegerArray = array of Integer;

// TIsoSurfaceExtractor
//
{ 3D Isosurface extractor class.
   This class allows to calculate and extract isosurfaces from scalar field
   voxel models using a given isovalue.
}
  TIsoSurfaceExtractor = class(TObject)
  private
    Data: TSingle3DArray;
    Dimensions: array ['x' .. 'z'] of Integer;

    function BuildIndex(var ADatavals: array of Single; Isovalue: Single): word;
    function Interpolate(V0, V1: TAffineVector;
      var Val0, Val1, Isovalue: Single): TVertex;

  public
    constructor Create(); overload;
    constructor Create(Xdim, Ydim, Zdim: Integer; var AData: TSingle3DArray); overload;
    destructor Destroy();

    procedure AssignData(Xdim, Ydim, Zdim: Integer;
      var AData: TSingle3DArray);

    procedure MarchingCubes(Isovalue: Single; out Vertices: TVertexArray;
      out Triangles: TIntegerArray);
    procedure MarchingTetrahedra(Isovalue: Single; out Vertices: TVertexArray;
      out Triangles: TIntegerArray);
  end;

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
implementation
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

const
 (*
     4----4------5
    /|          /|
   7 |         5 |
  /  |        /  |
 7-----6----6    |
 |   8       |   9
 |   |       |   |
 |   0----0--|---1
 11 /        10 /
 | 3         | 1
 |/          |/
 3-----2-----2
 *)
// Marching Cube TriTable
//
  MC_TRITABLE: array [0 .. 255, 0 .. 15] of Integer =
    ((-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 1, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 8, 3, 9, 8, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 3, 1, 2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (9, 2, 10, 0, 2, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (2, 8, 3, 2, 10, 8, 10, 9, 8, -1, -1, -1, -1, -1, -1, -1),
    (3, 11, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 11, 2, 8, 11, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 9, 0, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 11, 2, 1, 9, 11, 9, 8, 11, -1, -1, -1, -1, -1, -1, -1),
    (3, 10, 1, 11, 10, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 10, 1, 0, 8, 10, 8, 11, 10, -1, -1, -1, -1, -1, -1, -1),
    (3, 9, 0, 3, 11, 9, 11, 10, 9, -1, -1, -1, -1, -1, -1, -1),
    (9, 8, 10, 10, 8, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 3, 0, 7, 3, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 1, 9, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 1, 9, 4, 7, 1, 7, 3, 1, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 10, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (3, 4, 7, 3, 0, 4, 1, 2, 10, -1, -1, -1, -1, -1, -1, -1),
    (9, 2, 10, 9, 0, 2, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1),
    (2, 10, 9, 2, 9, 7, 2, 7, 3, 7, 9, 4, -1, -1, -1, -1),
    (8, 4, 7, 3, 11, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (11, 4, 7, 11, 2, 4, 2, 0, 4, -1, -1, -1, -1, -1, -1, -1),
    (9, 0, 1, 8, 4, 7, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1),
    (4, 7, 11, 9, 4, 11, 9, 11, 2, 9, 2, 1, -1, -1, -1, -1),
    (3, 10, 1, 3, 11, 10, 7, 8, 4, -1, -1, -1, -1, -1, -1, -1),
    (1, 11, 10, 1, 4, 11, 1, 0, 4, 7, 11, 4, -1, -1, -1, -1),
    (4, 7, 8, 9, 0, 11, 9, 11, 10, 11, 0, 3, -1, -1, -1, -1),
    (4, 7, 11, 4, 11, 9, 9, 11, 10, -1, -1, -1, -1, -1, -1, -1),
    (9, 5, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (9, 5, 4, 0, 8, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 5, 4, 1, 5, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (8, 5, 4, 8, 3, 5, 3, 1, 5, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 10, 9, 5, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (3, 0, 8, 1, 2, 10, 4, 9, 5, -1, -1, -1, -1, -1, -1, -1),
    (5, 2, 10, 5, 4, 2, 4, 0, 2, -1, -1, -1, -1, -1, -1, -1),
    (2, 10, 5, 3, 2, 5, 3, 5, 4, 3, 4, 8, -1, -1, -1, -1),
    (9, 5, 4, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 11, 2, 0, 8, 11, 4, 9, 5, -1, -1, -1, -1, -1, -1, -1),
    (0, 5, 4, 0, 1, 5, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1),
    (2, 1, 5, 2, 5, 8, 2, 8, 11, 4, 8, 5, -1, -1, -1, -1),
    (10, 3, 11, 10, 1, 3, 9, 5, 4, -1, -1, -1, -1, -1, -1, -1),
    (4, 9, 5, 0, 8, 1, 8, 10, 1, 8, 11, 10, -1, -1, -1, -1),
    (5, 4, 0, 5, 0, 11, 5, 11, 10, 11, 0, 3, -1, -1, -1, -1),
    (5, 4, 8, 5, 8, 10, 10, 8, 11, -1, -1, -1, -1, -1, -1, -1),
    (9, 7, 8, 5, 7, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (9, 3, 0, 9, 5, 3, 5, 7, 3, -1, -1, -1, -1, -1, -1, -1),
    (0, 7, 8, 0, 1, 7, 1, 5, 7, -1, -1, -1, -1, -1, -1, -1),
    (1, 5, 3, 3, 5, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (9, 7, 8, 9, 5, 7, 10, 1, 2, -1, -1, -1, -1, -1, -1, -1),
    (10, 1, 2, 9, 5, 0, 5, 3, 0, 5, 7, 3, -1, -1, -1, -1),
    (8, 0, 2, 8, 2, 5, 8, 5, 7, 10, 5, 2, -1, -1, -1, -1),
    (2, 10, 5, 2, 5, 3, 3, 5, 7, -1, -1, -1, -1, -1, -1, -1),
    (7, 9, 5, 7, 8, 9, 3, 11, 2, -1, -1, -1, -1, -1, -1, -1),
    (9, 5, 7, 9, 7, 2, 9, 2, 0, 2, 7, 11, -1, -1, -1, -1),
    (2, 3, 11, 0, 1, 8, 1, 7, 8, 1, 5, 7, -1, -1, -1, -1),
    (11, 2, 1, 11, 1, 7, 7, 1, 5, -1, -1, -1, -1, -1, -1, -1),
    (9, 5, 8, 8, 5, 7, 10, 1, 3, 10, 3, 11, -1, -1, -1, -1),
    (5, 7, 0, 5, 0, 9, 7, 11, 0, 1, 0, 10, 11, 10, 0, -1),
    (11, 10, 0, 11, 0, 3, 10, 5, 0, 8, 0, 7, 5, 7, 0, -1),
    (11, 10, 5, 7, 11, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (10, 6, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 3, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (9, 0, 1, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 8, 3, 1, 9, 8, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1),
    (1, 6, 5, 2, 6, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 6, 5, 1, 2, 6, 3, 0, 8, -1, -1, -1, -1, -1, -1, -1),
    (9, 6, 5, 9, 0, 6, 0, 2, 6, -1, -1, -1, -1, -1, -1, -1),
    (5, 9, 8, 5, 8, 2, 5, 2, 6, 3, 2, 8, -1, -1, -1, -1),
    (2, 3, 11, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (11, 0, 8, 11, 2, 0, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1),
    (0, 1, 9, 2, 3, 11, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1),
    (5, 10, 6, 1, 9, 2, 9, 11, 2, 9, 8, 11, -1, -1, -1, -1),
    (6, 3, 11, 6, 5, 3, 5, 1, 3, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 11, 0, 11, 5, 0, 5, 1, 5, 11, 6, -1, -1, -1, -1),
    (3, 11, 6, 0, 3, 6, 0, 6, 5, 0, 5, 9, -1, -1, -1, -1),
    (6, 5, 9, 6, 9, 11, 11, 9, 8, -1, -1, -1, -1, -1, -1, -1),
    (5, 10, 6, 4, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 3, 0, 4, 7, 3, 6, 5, 10, -1, -1, -1, -1, -1, -1, -1),
    (1, 9, 0, 5, 10, 6, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1),
    (10, 6, 5, 1, 9, 7, 1, 7, 3, 7, 9, 4, -1, -1, -1, -1),
    (6, 1, 2, 6, 5, 1, 4, 7, 8, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 5, 5, 2, 6, 3, 0, 4, 3, 4, 7, -1, -1, -1, -1),
    (8, 4, 7, 9, 0, 5, 0, 6, 5, 0, 2, 6, -1, -1, -1, -1),
    (7, 3, 9, 7, 9, 4, 3, 2, 9, 5, 9, 6, 2, 6, 9, -1),
    (3, 11, 2, 7, 8, 4, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1),
    (5, 10, 6, 4, 7, 2, 4, 2, 0, 2, 7, 11, -1, -1, -1, -1),
    (0, 1, 9, 4, 7, 8, 2, 3, 11, 5, 10, 6, -1, -1, -1, -1),
    (9, 2, 1, 9, 11, 2, 9, 4, 11, 7, 11, 4, 5, 10, 6, -1),
    (8, 4, 7, 3, 11, 5, 3, 5, 1, 5, 11, 6, -1, -1, -1, -1),
    (5, 1, 11, 5, 11, 6, 1, 0, 11, 7, 11, 4, 0, 4, 11, -1),
    (0, 5, 9, 0, 6, 5, 0, 3, 6, 11, 6, 3, 8, 4, 7, -1),
    (6, 5, 9, 6, 9, 11, 4, 7, 9, 7, 11, 9, -1, -1, -1, -1),
    (10, 4, 9, 6, 4, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 10, 6, 4, 9, 10, 0, 8, 3, -1, -1, -1, -1, -1, -1, -1),
    (10, 0, 1, 10, 6, 0, 6, 4, 0, -1, -1, -1, -1, -1, -1, -1),
    (8, 3, 1, 8, 1, 6, 8, 6, 4, 6, 1, 10, -1, -1, -1, -1),
    (1, 4, 9, 1, 2, 4, 2, 6, 4, -1, -1, -1, -1, -1, -1, -1),
    (3, 0, 8, 1, 2, 9, 2, 4, 9, 2, 6, 4, -1, -1, -1, -1),
    (0, 2, 4, 4, 2, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (8, 3, 2, 8, 2, 4, 4, 2, 6, -1, -1, -1, -1, -1, -1, -1),
    (10, 4, 9, 10, 6, 4, 11, 2, 3, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 2, 2, 8, 11, 4, 9, 10, 4, 10, 6, -1, -1, -1, -1),
    (3, 11, 2, 0, 1, 6, 0, 6, 4, 6, 1, 10, -1, -1, -1, -1),
    (6, 4, 1, 6, 1, 10, 4, 8, 1, 2, 1, 11, 8, 11, 1, -1),
    (9, 6, 4, 9, 3, 6, 9, 1, 3, 11, 6, 3, -1, -1, -1, -1),
    (8, 11, 1, 8, 1, 0, 11, 6, 1, 9, 1, 4, 6, 4, 1, -1),
    (3, 11, 6, 3, 6, 0, 0, 6, 4, -1, -1, -1, -1, -1, -1, -1),
    (6, 4, 8, 11, 6, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (7, 10, 6, 7, 8, 10, 8, 9, 10, -1, -1, -1, -1, -1, -1, -1),
    (0, 7, 3, 0, 10, 7, 0, 9, 10, 6, 7, 10, -1, -1, -1, -1),
    (10, 6, 7, 1, 10, 7, 1, 7, 8, 1, 8, 0, -1, -1, -1, -1),
    (10, 6, 7, 10, 7, 1, 1, 7, 3, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 6, 1, 6, 8, 1, 8, 9, 8, 6, 7, -1, -1, -1, -1),
    (2, 6, 9, 2, 9, 1, 6, 7, 9, 0, 9, 3, 7, 3, 9, -1),
    (7, 8, 0, 7, 0, 6, 6, 0, 2, -1, -1, -1, -1, -1, -1, -1),
    (7, 3, 2, 6, 7, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (2, 3, 11, 10, 6, 8, 10, 8, 9, 8, 6, 7, -1, -1, -1, -1),
    (2, 0, 7, 2, 7, 11, 0, 9, 7, 6, 7, 10, 9, 10, 7, -1),
    (1, 8, 0, 1, 7, 8, 1, 10, 7, 6, 7, 10, 2, 3, 11, -1),
    (11, 2, 1, 11, 1, 7, 10, 6, 1, 6, 7, 1, -1, -1, -1, -1),
    (8, 9, 6, 8, 6, 7, 9, 1, 6, 11, 6, 3, 1, 3, 6, -1),
    (0, 9, 1, 11, 6, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (7, 8, 0, 7, 0, 6, 3, 11, 0, 11, 6, 0, -1, -1, -1, -1),
    (7, 11, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (7, 6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (3, 0, 8, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 1, 9, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (8, 1, 9, 8, 3, 1, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1),
    (10, 1, 2, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 10, 3, 0, 8, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1),
    (2, 9, 0, 2, 10, 9, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1),
    (6, 11, 7, 2, 10, 3, 10, 8, 3, 10, 9, 8, -1, -1, -1, -1),
    (7, 2, 3, 6, 2, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (7, 0, 8, 7, 6, 0, 6, 2, 0, -1, -1, -1, -1, -1, -1, -1),
    (2, 7, 6, 2, 3, 7, 0, 1, 9, -1, -1, -1, -1, -1, -1, -1),
    (1, 6, 2, 1, 8, 6, 1, 9, 8, 8, 7, 6, -1, -1, -1, -1),
    (10, 7, 6, 10, 1, 7, 1, 3, 7, -1, -1, -1, -1, -1, -1, -1),
    (10, 7, 6, 1, 7, 10, 1, 8, 7, 1, 0, 8, -1, -1, -1, -1),
    (0, 3, 7, 0, 7, 10, 0, 10, 9, 6, 10, 7, -1, -1, -1, -1),
    (7, 6, 10, 7, 10, 8, 8, 10, 9, -1, -1, -1, -1, -1, -1, -1),
    (6, 8, 4, 11, 8, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (3, 6, 11, 3, 0, 6, 0, 4, 6, -1, -1, -1, -1, -1, -1, -1),
    (8, 6, 11, 8, 4, 6, 9, 0, 1, -1, -1, -1, -1, -1, -1, -1),
    (9, 4, 6, 9, 6, 3, 9, 3, 1, 11, 3, 6, -1, -1, -1, -1),
    (6, 8, 4, 6, 11, 8, 2, 10, 1, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 10, 3, 0, 11, 0, 6, 11, 0, 4, 6, -1, -1, -1, -1),
    (4, 11, 8, 4, 6, 11, 0, 2, 9, 2, 10, 9, -1, -1, -1, -1),
    (10, 9, 3, 10, 3, 2, 9, 4, 3, 11, 3, 6, 4, 6, 3, -1),
    (8, 2, 3, 8, 4, 2, 4, 6, 2, -1, -1, -1, -1, -1, -1, -1),
    (0, 4, 2, 4, 6, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 9, 0, 2, 3, 4, 2, 4, 6, 4, 3, 8, -1, -1, -1, -1),
    (1, 9, 4, 1, 4, 2, 2, 4, 6, -1, -1, -1, -1, -1, -1, -1),
    (8, 1, 3, 8, 6, 1, 8, 4, 6, 6, 10, 1, -1, -1, -1, -1),
    (10, 1, 0, 10, 0, 6, 6, 0, 4, -1, -1, -1, -1, -1, -1, -1),
    (4, 6, 3, 4, 3, 8, 6, 10, 3, 0, 3, 9, 10, 9, 3, -1),
    (10, 9, 4, 6, 10, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 9, 5, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 3, 4, 9, 5, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1),
    (5, 0, 1, 5, 4, 0, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1),
    (11, 7, 6, 8, 3, 4, 3, 5, 4, 3, 1, 5, -1, -1, -1, -1),
    (9, 5, 4, 10, 1, 2, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1),
    (6, 11, 7, 1, 2, 10, 0, 8, 3, 4, 9, 5, -1, -1, -1, -1),
    (7, 6, 11, 5, 4, 10, 4, 2, 10, 4, 0, 2, -1, -1, -1, -1),
    (3, 4, 8, 3, 5, 4, 3, 2, 5, 10, 5, 2, 11, 7, 6, -1),
    (7, 2, 3, 7, 6, 2, 5, 4, 9, -1, -1, -1, -1, -1, -1, -1),
    (9, 5, 4, 0, 8, 6, 0, 6, 2, 6, 8, 7, -1, -1, -1, -1),
    (3, 6, 2, 3, 7, 6, 1, 5, 0, 5, 4, 0, -1, -1, -1, -1),
    (6, 2, 8, 6, 8, 7, 2, 1, 8, 4, 8, 5, 1, 5, 8, -1),
    (9, 5, 4, 10, 1, 6, 1, 7, 6, 1, 3, 7, -1, -1, -1, -1),
    (1, 6, 10, 1, 7, 6, 1, 0, 7, 8, 7, 0, 9, 5, 4, -1),
    (4, 0, 10, 4, 10, 5, 0, 3, 10, 6, 10, 7, 3, 7, 10, -1),
    (7, 6, 10, 7, 10, 8, 5, 4, 10, 4, 8, 10, -1, -1, -1, -1),
    (6, 9, 5, 6, 11, 9, 11, 8, 9, -1, -1, -1, -1, -1, -1, -1),
    (3, 6, 11, 0, 6, 3, 0, 5, 6, 0, 9, 5, -1, -1, -1, -1),
    (0, 11, 8, 0, 5, 11, 0, 1, 5, 5, 6, 11, -1, -1, -1, -1),
    (6, 11, 3, 6, 3, 5, 5, 3, 1, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 10, 9, 5, 11, 9, 11, 8, 11, 5, 6, -1, -1, -1, -1),
    (0, 11, 3, 0, 6, 11, 0, 9, 6, 5, 6, 9, 1, 2, 10, -1),
    (11, 8, 5, 11, 5, 6, 8, 0, 5, 10, 5, 2, 0, 2, 5, -1),
    (6, 11, 3, 6, 3, 5, 2, 10, 3, 10, 5, 3, -1, -1, -1, -1),
    (5, 8, 9, 5, 2, 8, 5, 6, 2, 3, 8, 2, -1, -1, -1, -1),
    (9, 5, 6, 9, 6, 0, 0, 6, 2, -1, -1, -1, -1, -1, -1, -1),
    (1, 5, 8, 1, 8, 0, 5, 6, 8, 3, 8, 2, 6, 2, 8, -1),
    (1, 5, 6, 2, 1, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 3, 6, 1, 6, 10, 3, 8, 6, 5, 6, 9, 8, 9, 6, -1),
    (10, 1, 0, 10, 0, 6, 9, 5, 0, 5, 6, 0, -1, -1, -1, -1),
    (0, 3, 8, 5, 6, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (10, 5, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (11, 5, 10, 7, 5, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (11, 5, 10, 11, 7, 5, 8, 3, 0, -1, -1, -1, -1, -1, -1, -1),
    (5, 11, 7, 5, 10, 11, 1, 9, 0, -1, -1, -1, -1, -1, -1, -1),
    (10, 7, 5, 10, 11, 7, 9, 8, 1, 8, 3, 1, -1, -1, -1, -1),
    (11, 1, 2, 11, 7, 1, 7, 5, 1, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 3, 1, 2, 7, 1, 7, 5, 7, 2, 11, -1, -1, -1, -1),
    (9, 7, 5, 9, 2, 7, 9, 0, 2, 2, 11, 7, -1, -1, -1, -1),
    (7, 5, 2, 7, 2, 11, 5, 9, 2, 3, 2, 8, 9, 8, 2, -1),
    (2, 5, 10, 2, 3, 5, 3, 7, 5, -1, -1, -1, -1, -1, -1, -1),
    (8, 2, 0, 8, 5, 2, 8, 7, 5, 10, 2, 5, -1, -1, -1, -1),
    (9, 0, 1, 5, 10, 3, 5, 3, 7, 3, 10, 2, -1, -1, -1, -1),
    (9, 8, 2, 9, 2, 1, 8, 7, 2, 10, 2, 5, 7, 5, 2, -1),
    (1, 3, 5, 3, 7, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 7, 0, 7, 1, 1, 7, 5, -1, -1, -1, -1, -1, -1, -1),
    (9, 0, 3, 9, 3, 5, 5, 3, 7, -1, -1, -1, -1, -1, -1, -1),
    (9, 8, 7, 5, 9, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (5, 8, 4, 5, 10, 8, 10, 11, 8, -1, -1, -1, -1, -1, -1, -1),
    (5, 0, 4, 5, 11, 0, 5, 10, 11, 11, 3, 0, -1, -1, -1, -1),
    (0, 1, 9, 8, 4, 10, 8, 10, 11, 10, 4, 5, -1, -1, -1, -1),
    (10, 11, 4, 10, 4, 5, 11, 3, 4, 9, 4, 1, 3, 1, 4, -1),
    (2, 5, 1, 2, 8, 5, 2, 11, 8, 4, 5, 8, -1, -1, -1, -1),
    (0, 4, 11, 0, 11, 3, 4, 5, 11, 2, 11, 1, 5, 1, 11, -1),
    (0, 2, 5, 0, 5, 9, 2, 11, 5, 4, 5, 8, 11, 8, 5, -1),
    (9, 4, 5, 2, 11, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (2, 5, 10, 3, 5, 2, 3, 4, 5, 3, 8, 4, -1, -1, -1, -1),
    (5, 10, 2, 5, 2, 4, 4, 2, 0, -1, -1, -1, -1, -1, -1, -1),
    (3, 10, 2, 3, 5, 10, 3, 8, 5, 4, 5, 8, 0, 1, 9, -1),
    (5, 10, 2, 5, 2, 4, 1, 9, 2, 9, 4, 2, -1, -1, -1, -1),
    (8, 4, 5, 8, 5, 3, 3, 5, 1, -1, -1, -1, -1, -1, -1, -1),
    (0, 4, 5, 1, 0, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (8, 4, 5, 8, 5, 3, 9, 0, 5, 0, 3, 5, -1, -1, -1, -1),
    (9, 4, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 11, 7, 4, 9, 11, 9, 10, 11, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 3, 4, 9, 7, 9, 11, 7, 9, 10, 11, -1, -1, -1, -1),
    (1, 10, 11, 1, 11, 4, 1, 4, 0, 7, 4, 11, -1, -1, -1, -1),
    (3, 1, 4, 3, 4, 8, 1, 10, 4, 7, 4, 11, 10, 11, 4, -1),
    (4, 11, 7, 9, 11, 4, 9, 2, 11, 9, 1, 2, -1, -1, -1, -1),
    (9, 7, 4, 9, 11, 7, 9, 1, 11, 2, 11, 1, 0, 8, 3, -1),
    (11, 7, 4, 11, 4, 2, 2, 4, 0, -1, -1, -1, -1, -1, -1, -1),
    (11, 7, 4, 11, 4, 2, 8, 3, 4, 3, 2, 4, -1, -1, -1, -1),
    (2, 9, 10, 2, 7, 9, 2, 3, 7, 7, 4, 9, -1, -1, -1, -1),
    (9, 10, 7, 9, 7, 4, 10, 2, 7, 8, 7, 0, 2, 0, 7, -1),
    (3, 7, 10, 3, 10, 2, 7, 4, 10, 1, 10, 0, 4, 0, 10, -1),
    (1, 10, 2, 8, 7, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 9, 1, 4, 1, 7, 7, 1, 3, -1, -1, -1, -1, -1, -1, -1),
    (4, 9, 1, 4, 1, 7, 0, 8, 1, 8, 7, 1, -1, -1, -1, -1),
    (4, 0, 3, 7, 4, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 8, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (9, 10, 8, 10, 11, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (3, 0, 9, 3, 9, 11, 11, 9, 10, -1, -1, -1, -1, -1, -1, -1),
    (0, 1, 10, 0, 10, 8, 8, 10, 11, -1, -1, -1, -1, -1, -1, -1),
    (3, 1, 10, 11, 3, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 11, 1, 11, 9, 9, 11, 8, -1, -1, -1, -1, -1, -1, -1),
    (3, 0, 9, 3, 9, 11, 1, 2, 9, 2, 11, 9, -1, -1, -1, -1),
    (0, 2, 11, 8, 0, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (3, 2, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (2, 3, 8, 2, 8, 10, 10, 8, 9, -1, -1, -1, -1, -1, -1, -1),
    (9, 10, 2, 0, 9, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (2, 3, 8, 2, 8, 10, 0, 1, 8, 1, 10, 8, -1, -1, -1, -1),
    (1, 10, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 3, 8, 9, 1, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 9, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 3, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1));

// Marching Cube EdgeTable
//
  MC_EDGETABLE: array [0 .. 11, 0 .. 1] of Integer = ((0, 1), (1, 2), (2, 3),
    (3, 0), (4, 5), (5, 6), (6, 7), (7, 4), (0, 4), (1, 5), (2, 6), (3, 7));

(*
        + 0
         /|\
        / | \
       /  |  0
      3   |   \
     /    2    \
    /     |     \
   +----4--------+ 1
  3 \     |     /
     \    |    /
      5   |   1
       \  |  /
        \ | /
         \|/
        + 2
*)

// Marching Tetrahedra TriTable
//
  MT_TRITABLE: array [0 .. 15, 0 .. 6] of Integer =
    ((-1, -1, -1, -1, -1, -1, -1), (2, 3, 0, -1, -1, -1, -1),
    (4, 1, 0, -1, -1, -1, -1), (2, 4, 1, 3, 4, 2, -1),
    (5, 2, 1, -1, -1, -1, -1), (5, 3, 0, 1, 5, 0, -1), (5, 2, 0, 4, 5, 0, -1),
    (3, 4, 5, -1, -1, -1, -1), (5, 4, 3, -1, -1, -1, -1),
    (0, 5, 4, 0, 2, 5, -1), (0, 5, 1, 0, 3, 5, -1), (1, 2, 5, -1, -1, -1, -1),
    (2, 4, 3, 1, 4, 2, -1), (0, 1, 4, -1, -1, -1, -1),
    (0, 3, 2, -1, -1, -1, -1), (-1, -1, -1, -1, -1, -1, -1));

// Marching Tetrahedra EdgeTable
//
  MT_EDGETABLE: array [0 .. 5, 0 .. 1] of Integer = ((0, 1), (1, 2), (2, 0),
    (0, 3), (1, 3), (2, 3));

// Marching Tetrahedra CubeSplit
//
  MT_CUBESPLIT: array [0 .. 5, 0 .. 3] of Integer = ((0, 5, 1, 6), (0, 1, 2, 6),
    (0, 2, 3, 6), (0, 3, 7, 6), (0, 7, 4, 6), (0, 4, 5, 6));

{-------------------------------------------------------------------------
 Class IsoSurfaceExtractor
 Purpose: Extract an Isosurface from volume dataset for given Isovalue
 -------------------------------------------------------------------------}

// Build Index depending on whether the edges are outside or inside the surface
//
function TIsoSurfaceExtractor.BuildIndex(var ADatavals: array of Single;
  Isovalue: Single): word;
var
  i: Integer;
  val: word;
begin
  val := 1;
  Result := 0;
  for i := 1 to Length(ADatavals) do
  begin
    if ADatavals[i - 1] <= Isovalue then // Edge inside surface
      Result := Result + val;
    val := val * 2;
  end;
end;

// Compute intersection point of edge and surface by linear interpolation
//
function TIsoSurfaceExtractor.Interpolate(V0, V1: TAffineVector;
  var Val0, Val1, Isovalue: Single): TVertex;
var
  Diff: Single;
  i: Integer;
begin
  if Val0 <= Isovalue then
  begin
    Diff := Val0 / (Val0 + Val1);
    for i := 0 to 2 do
      Interpolate.V[i] := V0.V[i] + Diff * (V1.V[i] - V0.V[i]);
  end
  else
  begin
    Diff := Val1 / (Val0 + Val1);
    for i := 0 to 2 do
      Interpolate.V[i] := V1.V[i] + Diff * (V0.V[i] - V1.V[i]);
  end;
end;

// Launch Marching Tetrahedra
//
procedure TIsoSurfaceExtractor.MarchingTetrahedra(Isovalue: Single; out Vertices: TVertexArray;
  out Triangles: TIntegerArray);
var
  i, j, k: Integer;
  index: word;
  CubeVertices: array of TAffineVector;
  Tetrahedron: array [0 .. 3] of TAffineVector;
  DataTetra: array [0 .. 3] of Single;

  // Add Triangle to List
  procedure AppendTri();
  var
    edge: byte;
    Ver1, Ver2, Ver3: TVertex;
    VListlength: Integer;
    TListlength: Integer;
  begin
    edge := 0;
    while MT_TRITABLE[index, edge] <> -1 do
    begin
      Ver1 := Interpolate(Tetrahedron[MT_EDGETABLE[MT_TRITABLE[index, edge], 0]],
        Tetrahedron[MT_EDGETABLE[MT_TRITABLE[index, edge], 1]],
        DataTetra[MT_EDGETABLE[MT_TRITABLE[index, edge], 0]],
        DataTetra[MT_EDGETABLE[MT_TRITABLE[index, edge], 1]], Isovalue);
      Ver2 := Interpolate(Tetrahedron[MT_EDGETABLE[MT_TRITABLE[index, edge + 1], 0]
        ], Tetrahedron[MT_EDGETABLE[MT_TRITABLE[index, edge + 1], 1]],
        DataTetra[MT_EDGETABLE[MT_TRITABLE[index, edge + 1], 0]],
        DataTetra[MT_EDGETABLE[MT_TRITABLE[index, edge + 1], 1]], Isovalue);
      Ver3 := Interpolate(Tetrahedron[MT_EDGETABLE[MT_TRITABLE[index, edge + 2], 0]
        ], Tetrahedron[MT_EDGETABLE[MT_TRITABLE[index, edge + 2], 1]],
        DataTetra[MT_EDGETABLE[MT_TRITABLE[index, edge + 2], 0]],
        DataTetra[MT_EDGETABLE[MT_TRITABLE[index, edge + 2], 1]], Isovalue);
      VListlength := Length(Vertices) + 3;
      TListlength := Length(Triangles) + 3;
      SetLength(Vertices, VListlength);
      SetLength(Triangles, TListlength);
      Vertices[VListlength - 3] := Ver1;
      Vertices[VListlength - 2] := Ver2;
      Vertices[VListlength - 1] := Ver3;
      Triangles[TListlength - 3] := VListlength - 3;
      Triangles[TListlength - 2] := VListlength - 2;
      Triangles[TListlength - 1] := VListlength - 1;
      edge := edge + 3;
    end;
  end;

// Split Cube in 6 Tetrahedrons and process each tetrahedron
//
  procedure SplitCube();
  var
    i, j: Integer;
  begin
    for i := 0 to 5 do
    begin
      for j := 0 to 3 do
      begin
        Tetrahedron[j] := CubeVertices[MT_CUBESPLIT[i, j]];
        DataTetra[j] := Data[Trunc(Tetrahedron[j].V[0]), Trunc(Tetrahedron[j].V[1]),
          Trunc(Tetrahedron[j].V[2])];
      end;
      index := BuildIndex(DataTetra, Isovalue);
      AppendTri();
    end;
  end;

begin
(*
      1----2
     /|   /|
    0----3 |
    | 5--|-6
    |/   |/
    4----7
*)
  SetLength(CubeVertices, 8);
  for k := 0 to Dimensions['z'] - 2 do
  begin
    for j := 0 to Dimensions['y'] - 2 do
    begin
      for i := 0 to Dimensions['x'] - 2 do
      begin
        CubeVertices[0] := AffineVectorMake(i, j, k);
        CubeVertices[1] := AffineVectorMake(i, j, k + 1);
        CubeVertices[2] := AffineVectorMake(i + 1, j, k + 1);
        CubeVertices[3] := AffineVectorMake(i + 1, j, k);
        CubeVertices[4] := AffineVectorMake(i, j + 1, k);
        CubeVertices[5] := AffineVectorMake(i, j + 1, k + 1);
        CubeVertices[6] := AffineVectorMake(i + 1, j + 1, k + 1);
        CubeVertices[7] := AffineVectorMake(i + 1, j + 1, k);

        SplitCube();
      end; // for k
    end; // for j
  end; // for i
end; // ccMT

constructor TIsoSurfaceExtractor.Create;
begin
  inherited;
end;

constructor TIsoSurfaceExtractor.Create(Xdim, Ydim, Zdim: Integer;
  var AData: TSingle3DArray);
begin
  Create();
  AssignData(Xdim, Ydim, Zdim, AData);
end;

destructor TIsoSurfaceExtractor.Destroy;
begin
  inherited;
end;

procedure TIsoSurfaceExtractor.AssignData(Xdim, Ydim, Zdim: Integer;
  var AData: TSingle3DArray);
begin
  Dimensions['x'] := Xdim;
  Dimensions['y'] := Ydim;
  Dimensions['z'] := Zdim;

  Data := AData;
end;

// Launch Marching Cubes
//
procedure TIsoSurfaceExtractor.MarchingCubes(Isovalue: Single; out Vertices: TVertexArray;
  out Triangles: TIntegerArray);
var
  i, j, k: Integer;
  index: word;
  CubeVertices: array [0 .. 7] of TAffineVector;
  CubeData: array [0 .. 7] of Single;

  procedure AppendTri();
  var
    edge: byte;
    Ver1, Ver2, Ver3: TVertex;
    VListlength: Integer;
    TListlength: Integer;
  begin
    edge := 0;
    while MC_TRITABLE[index, edge] <> -1 do
    begin
      ver1 := Interpolate(CubeVertices[MC_EDGETABLE[MC_TRITABLE[index, edge], 0]],
        CubeVertices[MC_EDGETABLE[MC_TRITABLE[index, edge], 1]],
        CubeData[MC_EDGETABLE[MC_TRITABLE[index, edge], 0]],
        CubeData[MC_EDGETABLE[MC_TRITABLE[index, edge], 1]], Isovalue);
      ver2 := Interpolate(CubeVertices[MC_EDGETABLE[MC_TRITABLE[index, edge + 1], 0]],
        CubeVertices[MC_EDGETABLE[MC_TRITABLE[index, edge + 1], 1]],
        CubeData[MC_EDGETABLE[MC_TRITABLE[index, edge + 1], 0]],
        CubeData[MC_EDGETABLE[MC_TRITABLE[index, edge + 1], 1]], Isovalue);
      ver3 := Interpolate(CubeVertices[MC_EDGETABLE[MC_TRITABLE[index, edge + 2], 0]],
        CubeVertices[MC_EDGETABLE[MC_TRITABLE[index, edge + 2], 1]],
        CubeData[MC_EDGETABLE[MC_TRITABLE[index, edge + 2], 0]],
        CubeData[MC_EDGETABLE[MC_TRITABLE[index, edge + 2], 1]], Isovalue);
      VListlength := Length(Vertices) + 3;
      TListlength := Length(Triangles) + 3;
      SetLength(Vertices, VListlength);
      SetLength(Triangles, TListlength);
      Vertices[VListlength - 3] := Ver1;
      Vertices[VListlength - 2] := Ver2;
      Vertices[VListlength - 1] := Ver3;
      Triangles[TListlength - 3] := VListlength - 3;
      Triangles[TListlength - 2] := VListlength - 2;
      Triangles[TListlength - 1] := VListlength - 1;
      edge := edge + 3;
    end;
  end;

begin
(*
       7----6
      /|   /|
     3----2 |
    | 4--|-5
    |/   |/
    0----1
*)
  for i := 0 to Dimensions['x'] - 2 do
  begin
    for j := 1 to Dimensions['y'] - 1 do
    begin
      for k := 0 to Dimensions['z'] - 2 do
      begin
        CubeVertices[0] := AffineVectorMake(i, j, k);
        CubeVertices[1] := AffineVectorMake(i + 1, j, k);
        CubeVertices[2] := AffineVectorMake(i + 1, j - 1, k);
        CubeVertices[3] := AffineVectorMake(i, j - 1, k);
        CubeVertices[4] := AffineVectorMake(i, j, k + 1);
        CubeVertices[5] := AffineVectorMake(i + 1, j, k + 1);
        CubeVertices[6] := AffineVectorMake(i + 1, j - 1, k + 1);
        CubeVertices[7] := AffineVectorMake(i, j - 1, k + 1);
        CubeData[0] := Data[i, j, k];
        CubeData[1] := Data[i + 1, j, k];
        CubeData[2] := Data[i + 1, j - 1, k];
        CubeData[3] := Data[i, j - 1, k];
        CubeData[4] := Data[i, j, k + 1];
        CubeData[5] := Data[i + 1, j, k + 1];
        CubeData[6] := Data[i + 1, j - 1, k + 1];
        CubeData[7] := Data[i, j - 1, k + 1];

        index := BuildIndex(CubeData, Isovalue);
        AppendTri();
      end; // for k
    end; // for j
  end; // for i
end;

end.
