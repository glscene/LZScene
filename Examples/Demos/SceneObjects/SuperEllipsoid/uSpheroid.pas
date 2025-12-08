unit uSpheroid;

interface

{$I GLScene.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  //GLS
  GLVectorGeometry,
  GLVectorTypes,
  GLScene,
  OpenGLAdapter,
  OpenGLTokens,
  GLVectorLists,
  GLCrossPlatform,
  GLContext,
  GLSilhouette,
  GLColor,
  GLRenderContextInfo,
  GLBaseClasses,
  GLNodes,
  GLCoordinates;

type
  TAngleLimit1 = -90 .. 90;
  TAngleLimit2 = 0 .. 360;
  TCapType = (ctNone, ctCenter, ctFlat);

  // TNormalSmoothing
  //
  { : Determines how and if normals are smoothed.<p>
    - nsFlat : facetted look<br>
    - nsSmooth : smooth look<br>
    - nsNone : unlighted rendering, usefull for decla texturing }
  TNormalSmoothing = (nsFlat, nsSmooth, nsNone);

  // TGLQuadricObject
  //
  { : Base class for quadric objects.<p>
    Introduces some basic Quadric interaction functions (the actual quadric
    math is part of the GLU library). }
  TGLQuadricObject = class(TGLSceneObject)
  private
     
    FNormals: TNormalSmoothing;
    FNormalDirection: TNormalDirection;

  protected
    { Protected Declarations }
    procedure SetNormals(aValue: TNormalSmoothing);
    procedure SetNormalDirection(aValue: TNormalDirection);
    procedure SetupQuadricParams(quadric: PGLUquadricObj);
    procedure SetNormalQuadricOrientation(quadric: PGLUquadricObj);
    procedure SetInvertedQuadricOrientation(quadric: PGLUquadricObj);

  public
     
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

  published
     
    property Normals: TNormalSmoothing read FNormals write SetNormals
      default nsSmooth;
    property NormalDirection: TNormalDirection read FNormalDirection
      write SetNormalDirection default ndOutside;
  end;

  // TGLSuperellipsoid
  //
  { : A Superellipsoid object.<p>
    The Superellipsoid can have top and bottom caps,
    as well as being just a slice of Superellipsoid. }
  TGLSuperellipsoid = class(TGLQuadricObject)
  private
     
    FRadius, FVCurve, FHCurve: TGLFloat;
    FSlices, FStacks: TGLInt;
    FTop: TAngleLimit1;
    FBottom: TAngleLimit1;
    FStart: TAngleLimit2;
    FStop: TAngleLimit2;
    FTopCap, FBottomCap: TCapType;
    FVCheck, FHCheck: Boolean;
    procedure SetBottom(aValue: TAngleLimit1);
    procedure SetBottomCap(aValue: TCapType);
    procedure SetRadius(const aValue: TGLFloat);
    procedure SetVCurve(const aValue: TGLFloat);
    procedure SetHCurve(const aValue: TGLFloat);
    procedure SetSlices(aValue: TGLInt);
    procedure SetStart(aValue: TAngleLimit2);
    procedure SetStop(aValue: TAngleLimit2);
    procedure SetStacks(aValue: TGLInt);
    procedure SetTop(aValue: TAngleLimit1);
    procedure SetTopCap(aValue: TCapType);
    procedure SetVCheck(aValue: Boolean);
    procedure SetHCheck(aValue: Boolean);
  public
     
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TGLRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
     intersectPoint: PVector = nil; intersectNormal: PVector = nil)
     : Boolean; override;

    function GenerateSilhouette(const silhouetteParameters
     : TGLSilhouetteParameters): TGLSilhouette; override;
  published
     
    property Bottom: TAngleLimit1 read FBottom write SetBottom default -90;
    property BottomCap: TCapType read FBottomCap write SetBottomCap
      default ctNone;
    property Radius: TGLFloat read FRadius write SetRadius;
    property VCurve: TGLFloat read FVCurve write SetVCurve;
    property HCurve: TGLFloat read FHCurve write SetHCurve;
    property Slices: TGLInt read FSlices write SetSlices default 16;
    property Stacks: TGLInt read FStacks write SetStacks default 16;
    property Start: TAngleLimit2 read FStart write SetStart default 0;
    property Stop: TAngleLimit2 read FStop write SetStop default 360;
    property Top: TAngleLimit1 read FTop write SetTop default 90;
    property TopCap: TCapType read FTopCap write SetTopCap default ctNone;
    property VCheck: Boolean read FVCheck write SetVCheck default True;
    property HCheck: Boolean read FHCheck write SetHCheck default True;
  end;

implementation

// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------

uses
  GLSpline,
  XOpenGL,
  GLState;

const
  cDefaultPointSize: Single = 1.0;

// ------------------
// ------------------ TGLQuadricObject ------------------
// ------------------

// Create
//

constructor TGLQuadricObject.Create(AOwner: TComponent);
begin
  inherited;
  FNormals := nsSmooth;
  FNormalDirection := ndOutside;
end;

// SetNormals
//

procedure TGLQuadricObject.SetNormals(aValue: TNormalSmoothing);
begin
  if aValue <> FNormals then
  begin
    FNormals := aValue;
    StructureChanged;
  end;
end;

// SetNormalDirection
//

procedure TGLQuadricObject.SetNormalDirection(aValue: TNormalDirection);
begin
  if aValue <> FNormalDirection then
  begin
    FNormalDirection := aValue;
    StructureChanged;
  end;
end;

// SetupQuadricParams
//

procedure TGLQuadricObject.SetupQuadricParams(quadric: PGLUquadricObj);
const
  cNormalSmoothinToEnum: array [nsFlat .. nsNone] of TGLEnum = (GLU_FLAT,
    GLU_SMOOTH, GLU_NONE);
begin
  gluQuadricDrawStyle(quadric, GLU_FILL);
  gluQuadricNormals(quadric, cNormalSmoothinToEnum[FNormals]);
  SetNormalQuadricOrientation(quadric);
  gluQuadricTexture(quadric, True);
end;

// SetNormalQuadricOrientation
//

procedure TGLQuadricObject.SetNormalQuadricOrientation(quadric: PGLUquadricObj);
const
  cNormalDirectionToEnum: array [ndInside .. ndOutside] of TGLEnum =
    (GLU_INSIDE, GLU_OUTSIDE);
begin
  gluQuadricOrientation(quadric, cNormalDirectionToEnum[FNormalDirection]);
end;

// SetInvertedQuadricOrientation
//

procedure TGLQuadricObject.SetInvertedQuadricOrientation
  (quadric: PGLUquadricObj);
const
  cNormalDirectionToEnum: array [ndInside .. ndOutside] of TGLEnum =
    (GLU_OUTSIDE, GLU_INSIDE);
begin
  gluQuadricOrientation(quadric, cNormalDirectionToEnum[FNormalDirection]);
end;

// Assign
//

procedure TGLQuadricObject.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLQuadricObject) then
  begin
    FNormals := TGLQuadricObject(Source).FNormals;
    FNormalDirection := TGLQuadricObject(Source).FNormalDirection;
  end;
  inherited Assign(Source);
end;

// ------------------
// ------------------ TGLSuperellipsoid ------------------
// ------------------

// Create
//

constructor TGLSuperellipsoid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRadius := 0.5;
  FVCurve := 1.0;
  FHCurve := 1.0;
  FSlices := 16;
  FStacks := 16;
  FTop := 90;
  FBottom := -90;
  FStart := 0;
  FStop := 360;
end;

procedure TGLSuperellipsoid.BuildList(var rci: TGLRenderContextInfo);
var
  CosPvc, CosNPvc, SinPvc, SinNPvc, CosThc, SinThc: TGLFloat;

  tVc, tHc: TGLInt;
  v1, V2, N1: TAffineVector;
  AngTop, AngBottom, AngStart, AngStop, StepV, StepH: double;
  SinP, CosP, SinNP, CosNP, SinT, CosT, Phi, NextPhi, Theta: double;
  uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1: TGLFloat;
  i, j: Integer;
  DoReverse: Boolean;

begin
  DoReverse := (FNormalDirection = ndInside);
  rci.GLStates.PushAttrib([sttPolygon]);
  if DoReverse then
    rci.GLStates.InvertGLFrontFace;

  // common settings
  AngTop := DegToRadian(1.0 * FTop);
  AngBottom := DegToRadian(1.0 * FBottom);
  AngStart := DegToRadian(1.0 * FStart);
  AngStop := DegToRadian(1.0 * FStop);
  StepH := (AngStop - AngStart) / FSlices;
  StepV := (AngTop - AngBottom) / FStacks;

  GL.PushMatrix;
  GL.Scalef(Radius, Radius, Radius);

{ when the Power function uses even integer only produce positive points }
  tVc := trunc(VCurve);
  tHc := trunc(HCurve);
  if not Odd(tVc) then VCurve := VCurve + 1e-6;
  if not Odd(tHc) then HCurve := HCurve - 1e-6;

  // top cap
  if (FTop < 90) and (FTopCap in [ctCenter, ctFlat]) then
  begin
    glBegin(GL_TRIANGLE_FAN);
    SinCosine(AngTop, SinP, CosP);

    if (Sign(SinP) = 1) or (tVc = VCurve) then
      SinPvc :=   Power( SinP, VCurve)
    else
      SinPvc :=  -Power(-SinP, VCurve);

    if (Sign(CosP) = 1) or (tVc = VCurve) then
      CosPvc :=  Power( CosP, VCurve)
    else
      CosPvc := -Power(-CosP, VCurve);

    xgl.TexCoord2f(0.5, 0.5);
    if DoReverse then
      GL.Normal3f(0, -1, 0)
    else
      GL.Normal3f(0, 1, 0);

    if FTopCap = ctCenter then
      glVertex3f(0, 0, 0)
    else
    begin  { FTopCap = ctFlat }
      glVertex3f(0, SinPvc, 0);
      N1 := YVector;
      if DoReverse then
        N1.V[1] := -N1.V[1];
    end;  { FTopCap = ctFlat }
    if VCheck then
      v1.V[1] := SinPvc
    else
      v1.V[1] := SinP;  //***

    Theta := AngStart;
    for i := 0 to FSlices do
    begin
      SinCos(Theta, SinT, CosT);

      if (Sign(SinT) = 1) or (tHc = HCurve) then
        SinThc :=  Power( SinT, HCurve)
      else
        SinThc := -Power(-SinT, HCurve);

      if (Sign(CosT) = 1) or (tHc = HCurve) then
        CosThc :=  Power( CosT, HCurve)
      else
        CosThc := -Power(-CosT, HCurve);

      if VCheck and HCheck then
      begin
        v1.V[0] := CosPvc * SinThc;  //***
        v1.V[2] := CosPvc * CosThc;  //***
      end
      else if HCheck then
      begin
        v1.V[0] := CosP * SinThc;  //***
        v1.V[2] := CosP * CosThc;  //***
      end
      else
      begin
        v1.V[0] := CosP * SinT;  //***
        v1.V[2] := CosP * CosT;  //***
      end;

      if FTopCap = ctCenter then
      begin
        N1 := VectorPerpendicular(YVector, v1);
        if DoReverse then
          NegateVector(N1);
      end;
      xgl.TexCoord2f(SinThc * 0.5 + 0.5, CosThc * 0.5 + 0.5);
      GL.Normal3fv(@N1);
      glVertex3fv(@v1);
      Theta := Theta + StepH;
    end;
    GL.End_;
  end;

  // main body
  Phi := AngTop;
  NextPhi := Phi - StepV;
  uTexFactor := 1 / FSlices;
  vTexFactor := 1 / FStacks;

  for j := 0 to FStacks - 1 do
  begin
    Theta := AngStart;

    SinCos(Phi, SinP, CosP);
    if (Sign(SinP) = 1) or (tVc = VCurve) then
      SinPvc :=   Power( SinP, VCurve)
    else
      SinPvc :=  -Power(-SinP, VCurve);

    SinCos(NextPhi, SinNP, CosNP);
    if (Sign(SinNP) = 1) or (tVc = VCurve) then
      SinNPvc :=  Power( SinNP, VCurve)
    else
      SinNPvc := -Power(-SinNP, VCurve);

    if VCheck then
    begin
      v1.V[1] := SinPvc;  //***
      v2.V[1] := SinNPvc;  //***
    end
    else
    begin
      v1.V[1] := SinP;  //***
      v2.V[1] := SinNP;  //***
    end;

    vTexCoord0 := 1 - j * vTexFactor;
    vTexCoord1 := 1 - (j + 1) * vTexFactor;

    glBegin(GL_TRIANGLE_STRIP);

      if (Sign(CosP) = 1) or (tVc = VCurve) then
        CosPvc :=  Power( CosP, VCurve)
      else
        CosPvc := -Power(-CosP, VCurve);

      if (Sign(CosNP) = 1) or (tVc = VCurve) then
        CosNPvc :=  Power( CosNP, VCurve)
      else
        CosNPvc := -Power(-CosNP, VCurve);

    for i := 0 to FSlices do
    begin
      SinCos(Theta, SinT, CosT);
      if (Sign(SinT) = 1) or (tHc = HCurve) then
        SinThc :=  Power( SinT, HCurve)
      else
        SinThc := -Power(-SinT, HCurve);

      if (Sign(CosT) = 1) or (tHc = HCurve) then
        CosThc :=  Power( CosT, HCurve)
      else
        CosThc := -Power(-CosT, HCurve);

      if VCheck and HCheck then
      begin
        v1.V[0] := CosPvc * SinThc;  //***
        V2.V[0] := CosNPvc * SinThc;  //***
        v1.V[2] := CosPvc * CosThc;  //***
        V2.V[2] := CosNPvc * CosThc;  //***
      end
      else if HCheck then
      begin
        v1.V[0] := CosP * SinThc;  //***
        V2.V[0] := CosNP * SinThc;  //***
        v1.V[2] := CosP * CosThc;  //***
        V2.V[2] := CosNP * CosThc;  //***
      end
      else
      begin
        v1.V[0] := CosP * SinT;  //***
        V2.V[0] := CosNP * SinT;  //***
        v1.V[2] := CosP * CosT;  //***
        V2.V[2] := CosNP * CosT;  //***
      end;

      uTexCoord := i * uTexFactor;
      xgl.TexCoord2f(uTexCoord, vTexCoord0);
      if DoReverse then
      begin
        N1 := VectorNegate(v1);
        GL.Normal3fv(@N1);
      end
      else
        GL.Normal3fv(@v1);
      glVertex3fv(@v1);

      xgl.TexCoord2f(uTexCoord, vTexCoord1);
      if DoReverse then
      begin
        N1 := VectorNegate(V2);
        GL.Normal3fv(@N1);
      end
      else
        GL.Normal3fv(@V2);
      glVertex3fv(@V2);

      Theta := Theta + StepH;
    end;
    GL.End_;
    Phi := NextPhi;
    NextPhi := NextPhi - StepV;
  end;

  // bottom cap
  if (FBottom > -90) and (FBottomCap in [ctCenter, ctFlat]) then
  begin
    glBegin(GL_TRIANGLE_FAN);
    SinCos(AngBottom, SinP, CosP);

    if (Sign(SinP) = 1) or (tVc = VCurve) then
      SinPvc :=   Power( SinP, VCurve)
    else
      SinPvc :=  -Power(-SinP, VCurve);

    if (Sign(CosP) = 1) or (tVc = VCurve) then
      CosPvc :=  Power( CosP, VCurve)
    else
      CosPvc := -Power(-CosP, VCurve);

    xgl.TexCoord2f(0.5, 0.5);
    if DoReverse then
      GL.Normal3f(0, 1, 0)
    else
      GL.Normal3f(0, -1, 0);

    if FBottomCap = ctCenter then
      glVertex3f(0, 0, 0)
    else
    begin   { FBottomCap = ctFlat }
      if VCheck then
        glVertex3f(0, SinPvc, 0)  //***
      else
        glVertex3f(0, SinP, 0);  //***

      N1 := YVector;
      if not DoReverse then N1.V[1] := -N1.V[1];
    end;    { fBottomCap = ctFlat }

    if VCheck then
      v1.V[1] := SinPvc //***
    else
      v1.V[1] := SinP; //***

    Theta := AngStop;

    for i := 0 to FSlices do
    begin
      SinCos(Theta, SinT, CosT);

      if (Sign(SinT) = 1) or (tHc = HCurve) then
        SinThc :=  Power( SinT, HCurve)
      else
        SinThc := -Power(-SinT, HCurve);

      if (Sign(CosT) = 1) or (tHc = HCurve) then
        CosThc :=  Power( CosT, HCurve)
      else
        CosThc := -Power(-CosT, HCurve);

      if VCheck and HCheck then
      begin
        v1.V[0] := CosPvc * SinThc;  //***
        v1.V[2] := CosPvc * CosThc;  //***
      end
      else if HCheck then
      begin
        v1.V[0] := CosP * SinThc;  //***
        v1.V[2] := CosP * CosThc;  //***
      end
      else
      begin
        v1.V[0] := CosP * SinT;  //***
        v1.V[2] := CosP * CosT;  //***
      end;

      if FBottomCap = ctCenter then
      begin
        N1 := VectorPerpendicular(AffineVectorMake(0, -1, 0), v1);
        if DoReverse then
          NegateVector(N1.V);
      end;
      if HCheck then
        xgl.TexCoord2f(SinThc * 0.5 + 0.5, CosThc * 0.5 + 0.5)  //***
      else
        xgl.TexCoord2f(SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);  //***

      GL.Normal3fv(@N1);
      glVertex3fv(@v1);
      Theta := Theta - StepH;
    end;
    GL.End_;
  end;

  if DoReverse then
    rci.GLStates.InvertGLFrontFace;
  GL.PopMatrix;
  rci.GLStates.PopAttrib;
end;

// RayCastIntersect
// This will probably not work;
// RayCastSphereIntersect -> RayCastSuperellipsoidIntersect ??????

function TGLSuperellipsoid.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  i1, i2: TVector;
  localStart, localVector: TVector;
begin
  // compute coefficients of quartic polynomial
  SetVector(localStart, AbsoluteToLocal(rayStart));
  SetVector(localVector, AbsoluteToLocal(rayVector));
  NormalizeVector(localVector);
  if RayCastSphereIntersect(localStart, localVector, NullHmgVector, Radius, i1,
    i2) > 0 then
  begin
    Result := True;
    if Assigned(intersectPoint) then
      SetVector(intersectPoint^, LocalToAbsolute(i1));
    if Assigned(intersectNormal) then
    begin
      i1.W := 0; // vector transform
      SetVector(intersectNormal^, LocalToAbsolute(i1));
    end;
  end
  else
    Result := False;
end;

// GenerateSilhouette
// This will probably not work;

function TGLSuperellipsoid.GenerateSilhouette(const silhouetteParameters
  : TGLSilhouetteParameters): TGLSilhouette;
var
  i, j: Integer;
  s, C, angleFactor: Single;
  sVec, tVec: TAffineVector;
  Segments: Integer;
begin
  Segments := MaxInteger(FStacks, FSlices);

  // determine a local orthonormal matrix, viewer-oriented
  sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, XVector);
  if VectorLength(sVec) < 1E-3 then
    sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, YVector);
  tVec := VectorCrossProduct(silhouetteParameters.SeenFrom, sVec);
  NormalizeVector(sVec);
  NormalizeVector(tVec);
  // generate the silhouette (outline and capping)
  Result := TGLSilhouette.Create;
  angleFactor := (2 * PI) / Segments;
  for i := 0 to Segments - 1 do
  begin
    SinCosine(i * angleFactor, FRadius, s, C);
    Result.vertices.AddPoint(VectorCombine(sVec, tVec, s, C));
    j := (i + 1) mod Segments;
    Result.Indices.Add(i, j);
    if silhouetteParameters.CappingRequired then
      Result.CapIndices.Add(Segments, i, j)
  end;
  if silhouetteParameters.CappingRequired then
    Result.vertices.Add(NullHmgPoint);
end;

// SetBottom
//

procedure TGLSuperellipsoid.SetBottom(aValue: TAngleLimit1);
begin
  if FBottom <> aValue then
  begin
    FBottom := aValue;
    StructureChanged;
  end;
end;

// SetBottomCap
//

procedure TGLSuperellipsoid.SetBottomCap(aValue: TCapType);
begin
  if FBottomCap <> aValue then
  begin
    FBottomCap := aValue;
    StructureChanged;
  end;
end;

// SetRadius
//

procedure TGLSuperellipsoid.SetRadius(const aValue: TGLFloat);
begin
  if aValue <> FRadius then
  begin
    FRadius := aValue;
    StructureChanged;
  end;
end;

// SetVCurve
//

procedure TGLSuperellipsoid.SetVCurve(const aValue: TGLFloat);
begin
  if aValue <> FVCurve then
  begin
    FVCurve := aValue;
    StructureChanged;
  end;
end;

// SetHCurve
//

procedure TGLSuperellipsoid.SetHCurve(const aValue: TGLFloat);
begin
  if aValue <> FHCurve then
  begin
    FHCurve := aValue;
    StructureChanged;
  end;
end;

// SetSlices
//

procedure TGLSuperellipsoid.SetSlices(aValue: Integer);
begin
  if aValue <> FSlices then
  begin
    if aValue <= 0 then
      FSlices := 1
    else
      FSlices := aValue;
    StructureChanged;
  end;
end;

// SetStacks
//

procedure TGLSuperellipsoid.SetStacks(aValue: TGLInt);
begin
  if aValue <> FStacks then
  begin
    if aValue <= 0 then
      FStacks := 1
    else
      FStacks := aValue;
    StructureChanged;
  end;
end;

// SetStart
//

procedure TGLSuperellipsoid.SetStart(aValue: TAngleLimit2);
begin
  if FStart <> aValue then
  begin
    Assert(aValue <= FStop);
    FStart := aValue;
    StructureChanged;
  end;
end;

// SetStop
//

procedure TGLSuperellipsoid.SetStop(aValue: TAngleLimit2);
begin
  if FStop <> aValue then
  begin
    Assert(aValue >= FStart);
    FStop := aValue;
    StructureChanged;
  end;
end;

// SetTop
//

procedure TGLSuperellipsoid.SetTop(aValue: TAngleLimit1);
begin
  if FTop <> aValue then
  begin
    FTop := aValue;
    StructureChanged;
  end;
end;

// SetTopCap
//

procedure TGLSuperellipsoid.SetTopCap(aValue: TCapType);
begin
  if FTopCap <> aValue then
  begin
    FTopCap := aValue;
    StructureChanged;
  end;
end;

// SetVCheck
//

procedure TGLSuperellipsoid.SetVCheck(aValue: Boolean);
begin
  if FVCheck <> aValue then
  begin
    FVCheck := aValue;
    StructureChanged;
  end;
end;

// SetHCheck
//

procedure TGLSuperellipsoid.SetHCheck(aValue: Boolean);
begin
  if FHCheck <> aValue then
  begin
    FHCheck := aValue;
    StructureChanged;
  end;
end;

// Assign
//

procedure TGLSuperellipsoid.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLSuperellipsoid) then
  begin
    FRadius := TGLSuperellipsoid(Source).FRadius;
    FSlices := TGLSuperellipsoid(Source).FSlices;
    FStacks := TGLSuperellipsoid(Source).FStacks;
    FBottom := TGLSuperellipsoid(Source).FBottom;
    FTop := TGLSuperellipsoid(Source).FTop;
    FStart := TGLSuperellipsoid(Source).FStart;
    FStop := TGLSuperellipsoid(Source).FStop;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TGLSuperellipsoid.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.X := Abs(FRadius);
  Result.Y := Result.X;
  Result.Z := Result.X;
  Result.W := 0;
end;

end.
