//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Defines vector types for geometry only aiming to imply
   compatibility of GLScene for Delphi with C+Builder.
   Do not include any other units in uses clause 

	 History :  
     01/11/13 - PW - Fixed XE5 error: E2376 static can only be used on non-virtual class methods
     12/12/12 - PW - Added TGLVector's and TGLMatrix's types
     11/11/11 - PW - Creation. Defined TGLPoint, TGLPolygon and TGLPolyhedron types

}
unit GLTypes;

interface

type
  TGLBox = record
    ALeft, ATop, ANear, ARight, ABottom, AFar: Single;
  end;

//-----------------------
//Point types
//-----------------------
type

  PGLPoint2D = ^TGLPoint2D;
  TGLPoint2D = record
    X: Single;
    Y: Single;
    public
      function Create(X, Y : Single): TGLPoint2D;{$IF (FPC_VERSION < 3)} static; {$ENDIF}
      procedure SetPosition(const X, Y : Single);
      function Add(const APoint2D: TGLPoint2D): TGLPoint2D;
      function Length: Single; //distance to origin
      function Distance(const APoint2D : TGLPoint2D) : Single;
      procedure Offset(const ADeltaX, ADeltaY : Single);
  end;

  PGLPoint3D = ^TGLPoint3D;
  TGLPoint3D = record
    X: Single;
    Y: Single;
    Z: Single;
    public
      function Create(X, Y, Z: Single): TGLPoint3D; {$IF (FPC_VERSION < 3)} static; {$ENDIF}
      procedure SetPosition(const X, Y, Z : Single);
      function Add(const AGLPoint3D: TGLPoint3D): TGLPoint3D;
      function Length: Single; //distance to origin
      function Distance(const APoint3D : TGLPoint3D) : Single;
      procedure Offset(const ADeltaX, ADeltaY, ADeltaZ : Single);
  end;

  TGLPoint2DArray = array of TGLPoint2D;
  TGLPoint3DArray = array of TGLPoint3D;


//-----------------------
//Polygon types
//-----------------------
  TGLPolygon2D = TGLPoint2DArray;
  TGLPolygon3D = TGLPoint3DArray;

const
   ClosedPolygon2D: TGLPoint2D = (X: $FFFF; Y: $FFFF);
   ClosedPolygon3D: TGLPoint3D = (X: $FFFF; Y: $FFFF; Z: $FFFF);


//-----------------------
//Polyhedron types
//-----------------------
type
  TGLPolyhedron = array of TGLPolygon3D;

//-----------------------
// Vector types
//-----------------------
  TGLVector2DType = array [0..1] of Single;
  TGLVector3DType = array [0..2] of Single;

  TGLVector2D = record
    private
      function Add(const AVector2D: TGLVector2D): TGLVector2D;
      function Norm: Single;
    public
      function Create(const AX, AY, AW : Single): TGLVector2D; {$IF (FPC_VERSION < 3)} static; {$ENDIF}
      function Length: Single;
    case Integer of
      0: (V: TGLVector2DType;);
      1: (X: Single;
          Y: Single;
          W: Single;)
  end;

  TGLVector3D = record
    private
      function Add(const AVector3D: TGLVector3D): TGLVector3D;
      function Norm: Single;
    public
      function Create(const AX, AY, AZ, AW : Single): TGLVector3D; {$IF (FPC_VERSION < 3)} static; {$ENDIF}
      function Length: Single;
    case Integer of
      0: (V: TGLVector3DType;);
      1: (X: Single;
          Y: Single;
          Z: Single;
          W: Single;)
  end;

//-----------------------
// Matrix types
//-----------------------
  TGLMatrix2DType = array[0..3] of TGLVector2D;
  {$NODEFINE TGLMatrix2DType}
  (*$HPPEMIT END OPENNAMESPACE*)
  (*$HPPEMIT END 'typedef TGLVector2D TGLMatrix2DArray[4];'*)
  (*$HPPEMIT END CLOSENAMESPACE*)
  TGLMatrix3DType = array[0..3] of TGLVector3D;
  {$NODEFINE TGLMatrix3DType}
  (*$HPPEMIT END OPENNAMESPACE*)
  (*$HPPEMIT END 'typedef TGLVector3D TGLMatrix3DType[4];'*)
  (*$HPPEMIT END CLOSENAMESPACE*)

  TGLMatrix2D = record
  private
  public
    case Integer of
      0: (M: TGLMatrix2DType;);
      1: (e11, e12, e13: Single;
          e21, e22, e23: Single;
          e31, e32, e33: Single);
  end;

  TGLMatrix3D = record
  private
  public
    case Integer of
      0: (M: TGLMatrix3DType;);
      1: (e11, e12, e13, e14: Single;
          e21, e22, e23, e24: Single;
          e31, e32, e33, e34: Single;
          e41, e42, e43, e44: Single);
  end;

  TGLMatrix2DArray = array of TGLMatrix2D;
  TGLMatrix3DArray = array of TGLMatrix3D;

type
   TGLMesh2DVertex = packed record
    X, Y: Single;
    NX, NY: Single;
    tU, tV: Single;
  end;

   TGLMesh3DVertex = packed record
    X, Y, Z: Single;
    NX, NY, NZ: Single;
    tU, tV: Single;
  end;

  TGLMesh2D = array of TGLMesh2DVertex;
  TGLMesh3D = array of TGLMesh3DVertex;


  TGLQuaternion3D = record
    ImPart: TGLVector3D;
    RePart: Single;
  end;

//---------------------------------------------------------------
//---------------------------------------------------------------
//---------------------------------------------------------------


implementation

{ TGLPoint2D }

function TGLPoint2D.Create(X, Y : Single): TGLPoint2D;
begin
  Result.X := X;
  Result.Y := Y;
end;

procedure TGLPoint2D.SetPosition(const X, Y: Single);
begin
  Self.X := X;
  Self.Y := Y;
end;

function TGLPoint2D.Length: Single;
begin
  Result := Sqrt(Self.X * Self.X + Self.Y * Self.Y);
end;

function TGLPoint2D.Add(const APoint2D: TGLPoint2D): TGLPoint2D;
begin
  Result.SetPosition(Self.X + APoint2D.X, Self.Y + APoint2D.Y);
end;

function TGLPoint2D.Distance(const APoint2D: TGLPoint2D): Single;
begin
  Result := Sqrt(Sqr(Self.X - APoint2D.X) +  Sqr(Self.Y - APoint2D.Y));
end;

procedure TGLPoint2D.Offset(const ADeltaX, ADeltaY: Single);
begin
  Self.X := Self.X + ADeltaX;
  Self.Y := Self.Y + ADeltaY;
end;

{ TGLPoint3D }

function TGLPoint3D.Create(X, Y, Z: Single): TGLPoint3D;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function TGLPoint3D.Add(const AGLPoint3D: TGLPoint3D): TGLPoint3D;
begin
  Result.X := Self.X + AGLPoint3D.X;
  Result.Y := Self.Y + AGLPoint3D.Y;
  Result.Z := Self.Z + AGLPoint3D.Z;
end;

function TGLPoint3D.Distance(const APoint3D: TGLPoint3D): Single;
begin
  Result := Self.Length - APoint3D.Length;
end;

function TGLPoint3D.Length: Single;
begin
  Result := Sqrt(Self.X * Self.X + Self.Y * Self.Y + Self.Z * Self.Z);
end;

procedure TGLPoint3D.Offset(const ADeltaX, ADeltaY, ADeltaZ: Single);
begin
  Self.X := Self.X + ADeltaX;
  Self.Y := Self.Y + ADeltaY;
  Self.Z := Self.Z + ADeltaZ;
end;

procedure TGLPoint3D.SetPosition(const X, Y, Z: Single);
begin
  Self.X := X;
  Self.Y := Y;
  Self.Z := Z;
end;

{ TGLVector2D }

function TGLVector2D.Create(const AX, AY, AW: Single): TGLVector2D;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.W := AW;
end;

function TGLVector2D.Add(const AVector2D: TGLVector2D): TGLVector2D;
begin
  Result.X := Self.X + AVector2D.X;
  Result.Y := Self.Y + AVector2D.Y;
  Result.W := 1.0;
end;

function TGLVector2D.Length: Single;
begin
  Result := Sqrt(Self.Norm);
end;

function TGLVector2D.Norm: Single;
begin
  Result := (Self.X * Self.X) + (Self.Y * Self.Y);
end;

{ TGLVector3D }

function TGLVector3D.Create(const AX, AY, AZ, AW: Single): TGLVector3D;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
  Result.W := AW;
end;

function TGLVector3D.Add(const AVector3D: TGLVector3D): TGLVector3D;
begin
  Result.X := Self.X + AVector3D.X;
  Result.Y := Self.Y + AVector3D.Y;
  Result.Z := Self.Z + AVector3D.Z;
  Result.W := 1.0;
end;

function TGLVector3D.Norm: Single;
begin
  Result := (Self.X * Self.X) + (Self.Y * Self.Y) + (Self.Z * Self.Z);
end;

function TGLVector3D.Length: Single;
begin
  Result := Sqrt(Self.Norm);
end;


end.
