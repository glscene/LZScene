//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Skeleton colliders for defining and controlling ODE geoms.

    History : 
       10/11/12 - PW - Added CPP compatibility: restored records with arrays instead of vector arrays
       17/11/09 - DaStr - Improved Unix compatibility
                             (thanks Predator) (BugtrackerID = 2893580)
       12/04/08 - DaStr - Cleaned up uses section
                            (thanks Sandor Domokos) (BugtrackerID = 1808373)
       06/02/08 - Mrqzzz - Upgrade to ODE 0.9 (replaced references, and
                             CCilinder (ode 0.8) with Capsule(ode 0.9))
       02/08/04 - LR, YHC - BCB corrections: use record instead array
       04/12/03 - SG - Creation.
    
}
unit GLODESkeletonColliders;

interface

uses
  Classes, GLPersistentClasses, GLVectorGeometry, GLVectorFileObjects, ODEImport;

type
  
  // TSCODEBase
  //
  { Base ODE skeleton collider class. }
  TSCODEBase = class(TGLSkeletonCollider)
    private
      FGeom : PdxGeom;

    public
      procedure WriteToFiler(writer : TVirtualWriter); override;
      procedure ReadFromFiler(reader : TVirtualReader); override;

      procedure AddToSpace(Space : PdxSpace); virtual;
      procedure AlignCollider; override;

      { The geoms are created through the AddToSpace procedure. }
      property Geom : PdxGeom read FGeom;
  end;

  // TSCODESphere
  //
  { Sphere shaped ODE geom in a skeleton collider. }
  TSCODESphere = class(TSCODEBase)
    private
      FRadius : TdReal;

    protected
      procedure SetRadius(const val : TdReal);

    public
      constructor Create; override;
      procedure WriteToFiler(writer : TVirtualWriter); override;
      procedure ReadFromFiler(reader : TVirtualReader); override;
      procedure AddToSpace(Space : PdxSpace); override;

      property Radius : TdReal read FRadius write SetRadius;
  end;

  // TSCODECCylinder
  //
  { Capsule (sphere capped cylinder) shaped ODE geom in a skeleton 
     collider. }
  TSCODECCylinder = class(TSCODEBase)
    private
      FRadius,
      FLength : Single;

    protected
      procedure SetRadius(const val : Single);
      procedure SetLength(const val : Single);

    public
      constructor Create; override;
      procedure WriteToFiler(writer : TVirtualWriter); override;
      procedure ReadFromFiler(reader : TVirtualReader); override;
      procedure AddToSpace(Space : PdxSpace); override;

      property Radius : Single read FRadius write SetRadius;
      property Length : Single read FLength write SetLength;
  end;

  // TSCODEBox
  //
  { Box shaped ODE geom in a skeleton collider. }
  TSCODEBox = class(TSCODEBase)
    private
      FBoxWidth,
      FBoxHeight,
      FBoxDepth : TdReal;

    protected
      procedure SetBoxWidth(const val : TdReal);
      procedure SetBoxHeight(const val : TdReal);
      procedure SetBoxDepth(const val : TdReal);

    public
      constructor Create; override;
      procedure WriteToFiler(writer : TVirtualWriter); override;
      procedure ReadFromFiler(reader : TVirtualReader); override;
      procedure AddToSpace(Space : PdxSpace); override;

      property BoxWidth : TdReal read FBoxWidth write SetBoxWidth;
      property BoxHeight : TdReal read FBoxHeight write SetBoxHeight;
      property BoxDepth : TdReal read FBoxDepth write SetBoxDepth;
  end;

{ After loading call this function to add all the geoms in a
   skeleton collider list to a given ODE space. }
procedure AddSCODEGeomsToODESpace(
  colliders : TGLSkeletonColliderList; space : PdxSpace);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ Global methods ------------------
// ------------------

// AddSCODEGeomsToODESpace
//
procedure AddSCODEGeomsToODESpace(
  colliders : TGLSkeletonColliderList; space : PdxSpace);
var
  i : Integer;
begin
  for i:=0 to colliders.Count-1 do
    if colliders[i] is TSCODEBase then
      TSCODEBase(Colliders[i]).AddToSpace(space);
end;

// ------------------
// ------------------ TSCODEBase ------------------
// ------------------

// WriteToFiler
//
procedure TSCODEBase.WriteToFiler(writer : TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do begin
    WriteInteger(0); // Archive Version 0
  end;
end;

// ReadFromFiler
//
procedure TSCODEBase.ReadFromFiler(reader : TVirtualReader);
var
  archiveVersion : integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion:=reader.ReadInteger;
  if archiveVersion=0 then with reader do
    // Nothing yet
  else RaiseFilerException(archiveVersion);
end;

// AddToSpace
//
procedure TSCODEBase.AddToSpace(Space : PdxSpace);
begin
  AlignCollider;
end;

// AlignCollider
//
procedure TSCODEBase.AlignCollider;
var
  R : TdMatrix3;
  Mat : TMatrix;
begin
  inherited;
  if Assigned(FGeom) then begin
    Mat:=GlobalMatrix;
    dGeomSetPosition(FGeom,Mat.V[3].V[0],Mat.V[3].V[1],Mat.V[3].V[2]);
    R[0]:=Mat.V[0].V[0]; R[1]:=Mat.V[1].V[0]; R[2]:= Mat.V[2].V[0]; R[3]:= 0;
    R[4]:=Mat.V[0].V[1]; R[5]:=Mat.V[1].V[1]; R[6]:= Mat.V[2].V[1]; R[7]:= 0;
    R[8]:=Mat.V[0].V[2]; R[9]:=Mat.V[1].V[2]; R[10]:=Mat.V[2].V[2]; R[11]:=0;
    dGeomSetRotation(FGeom,R);
  end;
end;


// ------------------
// ------------------ TSCODESphere ------------------
// ------------------

// Create
//
constructor TSCODESphere.Create;
begin
  inherited;
  FRadius:=0.5;
  AlignCollider;
end;

// WriteToFiler
//
procedure TSCODESphere.WriteToFiler(writer : TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do begin
    WriteInteger(0); // Archive Version 0
    WriteFloat(FRadius);
  end;
end;

// ReadFromFiler
//
procedure TSCODESphere.ReadFromFiler(reader : TVirtualReader);
var
  archiveVersion : integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion:=reader.ReadInteger;
  if archiveVersion=0 then with reader do
    Radius:=ReadFloat
  else RaiseFilerException(archiveVersion);
end;

// AddToSpace
//
procedure TSCODESphere.AddToSpace(Space : PdxSpace);
begin
  FGeom:=dCreateSphere(Space, FRadius);
  inherited;
end;

// SetRadius
//
procedure TSCODESphere.SetRadius(const val : TdReal);
begin
  if val<>FRadius then begin
    FRadius:=val;
    if Assigned(FGeom) then
      dGeomSphereSetRadius(Geom, TdReal(FRadius));
  end;
end;


// ------------------
// ------------------ TSCODECCylinder ------------------
// ------------------

// Create
//
constructor TSCODECCylinder.Create;
begin
  inherited;
  FRadius:=0.5;
  FLength:=1;
  AlignCollider;
end;

// WriteToFiler
//
procedure TSCODECCylinder.WriteToFiler(writer : TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do begin
    WriteInteger(0); // Archive Version 0
    WriteFloat(FRadius);
    WriteFloat(FLength);
  end;
end;

// ReadFromFiler
//
procedure TSCODECCylinder.ReadFromFiler(reader : TVirtualReader);
var
  archiveVersion : integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion:=reader.ReadInteger;
  if archiveVersion=0 then with reader do begin
    Radius:=ReadFloat;
    Length:=ReadFloat;
  end else RaiseFilerException(archiveVersion);
end;

// AddToSpace
//
procedure TSCODECCylinder.AddToSpace(Space : PdxSpace);
begin
  FGeom:=dCreateCapsule(Space,FRadius,FLength);
  inherited;
end;

// SetRadius
//
procedure TSCODECCylinder.SetRadius(const val : Single);
begin
  if val<>FRadius then begin
    FRadius:=val;
    if Assigned(FGeom) then
      dGeomCapsuleSetParams(FGeom,FRadius,FLength);
  end;
end;

// SetLength
//
procedure TSCODECCylinder.SetLength(const val : Single);
begin
  if val<>FLength then begin
    FLength:=val;
    if Assigned(FGeom) then
      dGeomCapsuleSetParams(FGeom,FRadius,FLength);
  end;
end;

// ------------------
// ------------------ TSCODEBox ------------------
// ------------------

// Create
//
constructor TSCODEBox.Create;
begin
  inherited;
  FBoxWidth:=1;
  FBoxHeight:=1;
  FBoxDepth:=1;
  AlignCollider;
end;

// WriteToFiler
//
procedure TSCODEBox.WriteToFiler(writer : TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do begin
    WriteInteger(0); // Archive Version 0
    WriteFloat(FBoxWidth);
    WriteFloat(FBoxHeight);
    WriteFloat(FBoxDepth);
  end;
end;

// ReadFromFiler
//
procedure TSCODEBox.ReadFromFiler(reader : TVirtualReader);
var
  archiveVersion : integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion:=reader.ReadInteger;
  if archiveVersion=0 then with reader do begin
    BoxWidth:=ReadFloat;
    BoxHeight:=ReadFloat;
    BoxDepth:=ReadFloat;
  end else RaiseFilerException(archiveVersion);
end;

// AddToSpace
//
procedure TSCODEBox.AddToSpace(Space : PdxSpace);
begin
  FGeom:=dCreateBox(Space, FBoxWidth, FBoxHeight, FBoxDepth);
  inherited;
end;

// SetBoxWidth
//
procedure TSCODEBox.SetBoxWidth(const val : TdReal);
begin
  if val<>FBoxWidth then begin
    FBoxWidth:=val;
    if Assigned(FGeom) then
      dGeomBoxSetLengths(Geom,
        TdReal(FBoxWidth),
        TdReal(FBoxHeight),
        TdReal(FBoxDepth));
  end;
end;

// SetBoxHeight
//
procedure TSCODEBox.SetBoxHeight(const val : TdReal);
begin
  if val<>FBoxHeight then begin
    FBoxHeight:=val;
    if Assigned(FGeom) then
      dGeomBoxSetLengths(Geom,
        TdReal(FBoxWidth),
        TdReal(FBoxHeight),
        TdReal(FBoxDepth));
  end;
end;

// SetBoxDepth
//
procedure TSCODEBox.SetBoxDepth(const val : TdReal);
begin
  if val<>FBoxDepth then begin
    FBoxDepth:=val;
    if Assigned(FGeom) then
      dGeomBoxSetLengths(Geom,
        TdReal(FBoxWidth),
        TdReal(FBoxHeight),
        TdReal(FBoxDepth));
  end;
end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  RegisterClasses([TSCODEBase,TSCODESphere,TSCODECCylinder,TSCODEBox]);

end.
