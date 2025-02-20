// Fountain v1.1
// Fountain demo created by Dave Gravel
// http://www.k00m.sexidude.com
// lucifers23@hotmail.com
{******************************************************************************}
 // [2005-06-09]: Fountain Unit last change by Dave Gravel
 // [2014-07-21]: Fountain Unit last change by Pavel Vassiliev
{******************************************************************************}
unit Fountain_Unit;



interface

uses
   GL, SysUtils,
  Variants, Classes, Graphics, Controls, Forms,
  Dialogs,  ExtCtrls,


  //GLScene
  GLScene, GLObjects, GLVectorGeometry, GLTexture,// OpenGL1x,
  GLVectorTypes, GLRenderContextInfo;

const
  F_GRAVITY = 9.81;
{******************************************************************************}
 // [2005-06-09]: TFctCondition last change by Dave Gravel
{******************************************************************************}
type
  TFctCondition = function( const ptCondition: Pointer ): Boolean;
  TpNode = ^TNode;
  TNode = record
    Info: Pointer;
    Next: TpNode;
  end;

const
  SIZE_NODE = SizeOf( TNode );
{******************************************************************************}
 // [2005-06-09]: TListSPTR last change by Dave Gravel
{******************************************************************************}
type
  TListSPTR = class
    private
      Head: TpNode;
      Final: TpNode;
      Current: TpNode;
      Count: Cardinal;
      SizeInfo: Cardinal;
    public
      constructor Create( _SizeInfo: Cardinal );
      destructor Destroy; override;
      function Add( New: Pointer ): boolean;
      function CurrentModify( Modification: Pointer ): boolean;
      function DeleteIf( FctCondition: TFctCondition ): integer;
      function DeleteCurrent: boolean;
      procedure Clear;
      function GetNbCount: cardinal;
      function GetCurrent( Information: Pointer ): boolean;
      function GetFirst( Information: Pointer ): boolean;
      function GetLast( Information: Pointer ): boolean;
      function GetNext( Information: Pointer ): boolean;
  end;
{******************************************************************************}
 // [2005-06-09]: TParticle last change by Dave Gravel
{******************************************************************************}
type
  pParticle = ^TParticle;
  TParticle = record
    Pos: TAffineVector;
    Accel: TAffineVector;
    Velocity: single;
    Times: double;
    Life: single;
    AngleStart: single;
    Bounding: integer;
    Width: single;
    Color: TAffineVector;
    ColorDiff: TAffineVector;
  end;

const
  SIZE_STR_PARTICLE = Sizeof( TParticle );
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy last change by Dave Gravel
{******************************************************************************}
type
  TGLFountainDummy = class( TGLImmaterialSceneObject )
    protected
      FActived: boolean;
      FNbParticles: integer;
      FMaxParticles: integer;
      FVelocityMax: integer;
      FVelocityMin: integer;
      FAngleStart: integer;
      FFloor: single;
      FFountainSize: single;
      FParticlesSizeMax: integer;
      FParticlesSizeMin: integer;
      FBoundingFactor: single;
      FParticleMass: single;
      FTimesFactor: double;
      FLifeFactor: single;
      FBounding: boolean;
      FColorStart: longint;
      FColorEnd: longint;
      FNewTime : double;
      FDeltaTime : double;
      function GetActived: boolean;
      procedure SetActived( const Activ: boolean );
      function GetNbParticles: integer;
      function GetMaxParticles: integer;
      procedure SetMaxParticles( const Max: integer );
      function GetVelocityMax: integer;
      procedure SetVelocityMax( const VeloMax: integer );
      function GetVelocityMin: integer;
      procedure SetVelocityMin( const VeloMin: integer );
      function GetAngleStart: integer;
      procedure SetAngleStart( const AngleS: integer );
      function GetFloor: single;
      procedure SetFloor( const TheFloor: single );
      function GetFountainSize: single;
      procedure SetFountainSize( const FountainSize: single );
      function GetParticlesSizeMax: integer;
      procedure SetParticlesSizeMax( const PartMax: integer );
      function GetParticlesSizeMin: integer;
      procedure SetParticlesSizeMin( const PartMin: integer );
      function GetBoundingFact: single;
      procedure SetBoundingFact( const BoundSize: single );
      function GetParticlesMass: single;
      procedure SetParticlesMass( const Mass: single );
      function GetTimesFactor: double;
      procedure SetTimesFactor( const TimesFact: double );
      function GetLifeFactor: single;
      procedure SetLifeFactor( const LifeFact: single );
      function GetBounding: boolean;
      procedure SetBounding( const Bound: boolean );
      function GetColorStart: longint;
      procedure SetColorStart( const ColStart: longint );
      function GetColorEnd: longint;
      procedure SetColorEnd( const ColEnd: longint );
    private
      LsParticles: TListSPTR;
      TabCos, TabSin: array[0..360] of double;
      RD, GD, BD, RF, GF, BF: Byte;
      procedure initFountain;
      function AddParticle : boolean;
      procedure DeleteParticle;
      procedure CalculBoundPosParticles;
      procedure CalculPosParticles;
      procedure DrawParticles( rci : TGLRenderContextInfo );
      procedure Animation( rci : TGLRenderContextInfo );
      procedure UpdateFountain;
    public
      procedure DoRender( var rci : TGLRenderContextInfo; renderSelf, renderChildren : Boolean ); override;
//      procedure DoProgress( const progressTime : TProgressTimes ); override;
      constructor Create( AOwner: TComponent ); override;
      destructor Destroy; override;
    published
      property Actived: boolean read FActived write SetActived;
      property NbParticles: integer read GetNbParticles;
      property MaxParticles: integer read GetMaxParticles write SetMaxParticles;
      property VelocityMax: integer read GetVelocityMax write SetVelocityMax;
      property VelocityMin: integer read GetVelocityMin write SetVelocityMin;
      property AngleInit: integer read GetAngleStart write SetAngleStart;
      property Floor: single read GetFloor write SetFloor;
      property ParticlesSizeMax: integer read GetParticlesSizeMax write SetParticlesSizeMax;
      property ParticlesSizeMin: integer read GetParticlesSizeMin write SetParticlesSizeMin;
      property BoundingFactor: single read GetBoundingFact write SetBoundingFact;
      property ParticleMass: single read GetParticlesMass write SetParticlesMass;
      property TimesFactor: double read GetTimesFactor write SetTimesFactor;
      property LifeFactor: single read GetLifeFactor write SetLifeFactor;
      property Bounding: boolean read GetBounding write SetBounding;
      property ColorStart: longint read GetColorStart write SetColorStart;
      property ColorEnd: longint read GetColorEnd write SetColorEnd;
  end;

implementation
{******************************************************************************}
 // [2005-06-09]: TListSPTR.Create last change by Dave Gravel
{******************************************************************************}
constructor TListSPTR.Create( _SizeInfo: Cardinal );
begin
  inherited Create;
  Clear;
  SizeInfo := _SizeInfo;
end;
{******************************************************************************}
 // [2005-06-09]: TListSPTR.Destroy last change by Dave Gravel
{******************************************************************************}
destructor TListSPTR.Destroy;
begin
  inherited Destroy;
  Clear;
end;
{******************************************************************************}
 // [2005-06-09]: TListSPTR.Add last change by Dave Gravel
{******************************************************************************}
function TListSPTR.Add( New: Pointer ): boolean;
var
  p: TpNode;
begin
  GetMem( p, SIZE_NODE );
  FillChar( p^, SIZE_NODE, 0 );
  Result := p <> Nil;
  if ( Result ) then
  begin
    GetMem( p^.Info, SizeInfo );
    FillChar( p^.Info^, SizeInfo, 0 );
    Result := ( p^.Info <> nil );
    if ( Result ) then
    begin
      p^.Next := Head;
      Head := p;
      Current := p;
      Move( New^, p^.Info^, SizeInfo );
      Inc( Count );
    end;
  end
end;
{******************************************************************************}
 // [2005-06-09]: TListSPTR.CurrentModify last change by Dave Gravel
{******************************************************************************}
function TListSPTR.CurrentModify( Modification: Pointer ): boolean;
begin
  Result := ( Current <> nil ) and ( Modification <> nil );
  if Result then
    Move( Modification^, Current^.Info^, SizeInfo );
end;
{******************************************************************************}
 // [2005-06-09]: TListSPTR.DeleteCurrent last change by Dave Gravel
{******************************************************************************}
function TListSPTR.DeleteCurrent: boolean;
var
  p: TpNode;
  pContinue: TpNode;
Begin
  Result := ( Current <> nil ) and ( Count > 0 );
  if Result then
  begin
    p := Current;
    if ( p = Head ) then
    begin
      Head := p^.Next;
      Current := Current^.Next;
      FreeMem( p^.Info, SizeInfo );
      FreeMem( p, SIZE_NODE );
      Dec( Count );
    end else
    begin
      pContinue := Head;
      while ( pContinue <> nil ) and ( pContinue^.Next <> p ) do
        pContinue := pContinue^.Next;
        if ( pContinue <> nil ) then
        begin
          pContinue^.Next := p^.Next;
          Current := Current^.Next;
          FreeMem( p^.Info, SizeInfo );
          FreeMem( p, SIZE_NODE );
          Dec( Count );
        end;
      end;
   end;
end;
{******************************************************************************}
 // [2005-06-09]: TListSPTR.DeleteIf last change by Dave Gravel
{******************************************************************************}
function TListSPTR.DeleteIf( FctCondition: TFctCondition ): integer;
var
  p, GCurrent: TpNode;
begin
  Result := 0;
  GCurrent := Current;
  p := Head;
  while ( p <> nil ) do
  begin
    if FctCondition( p^.Info ) then
    begin
      Current := p;
      DeleteCurrent;
      p := Current;
      Inc( Result );
    end else
      p := p^.Next;
    end;
  Current := GCurrent;
end;
{******************************************************************************}
 // [2005-06-09]: TListSPTR.Clear last change by Dave Gravel
{******************************************************************************}
procedure TListSPTR.Clear;
var
   pAClean: TpNode;
begin
  if ( Head <> nil ) then
  begin
    while ( Head <> nil ) do
    begin
      pAClean := Head;
      Head := pAClean^.Next;
      FreeMem( pAClean^.Info, SizeInfo );
      FreeMem( pAClean, SIZE_NODE );
    end;
  end;
  Head := nil;
  Final := nil;
  Current := nil;
  Count := 0;
end;
{******************************************************************************}
 // [2005-06-09]: TListSPTR.GetNbCount last change by Dave Gravel
{******************************************************************************}
function TListSPTR.GetNbCount: cardinal;
begin
  Result := Count;
end;
{******************************************************************************}
 // [2005-06-09]: TListSPTR.GetCurrent last change by Dave Gravel
{******************************************************************************}
function TListSPTR.GetCurrent( Information: Pointer ): boolean;
Begin
  Result := ( Head <> nil ) and ( Information <> nil ) and ( Current <> nil );
  if Result then
    Move( Current^.Info^, Information^, SizeInfo );
end;
{******************************************************************************}
 // [2005-06-09]: TListSPTR.GetFirst last change by Dave Gravel
{******************************************************************************}
function TListSPTR.GetFirst( Information: Pointer ): boolean;
begin
  Result := ( Head <> nil ) and ( Information <> nil );
  if Result then
  begin
    Move( Head^.Info^, Information^, SizeInfo );
    Current := Head;
  end;
end;
{******************************************************************************}
 // [2005-06-09]: TListSPTR.GetLast last change by Dave Gravel
{******************************************************************************}
function TListSPTR.GetLast( Information: Pointer ): boolean;
begin
  Result := ( Final <> nil ) and ( Information <> nil );
  if Result then
  begin
    Move( Final^.Info^, Information^, SizeInfo );
    Current := Final;
  end;
end;
{******************************************************************************}
 // [2005-06-09]: TListSPTR.GetNext last change by Dave Gravel
{******************************************************************************}
function TListSPTR.GetNext( Information: Pointer ): boolean;
begin
  Result := ( Count > 0 ) and ( Current^.Next <> nil ) and ( Information <> nil );
  if Result then
  begin
    Move( Current^.Next^.Info^, Information^, SizeInfo );
    Current := Current^.Next;
  end;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.Create last change by Dave Gravel
{******************************************************************************}
constructor TGLFountainDummy.Create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  FNewTime := 0.0;
  FDeltaTime := 0.0;
  FActived := True;
  FNbParticles := 0;
  FMaxParticles := 500;
  FVelocityMin := 14;
  FVelocityMax := 15;
  FAngleStart := 360;
  FFloor := 0.0;
  FFountainSize := 0.2;
  FParticlesSizeMin := 20;
  FParticlesSizeMax := 40;
  FBoundingFactor := 55;
  FParticleMass := 5.0;
  FTimesFactor := 0.005;
  FLifeFactor := 0.005;
  FBounding := False;
  SetColorStart( $FF0000 );
  SetColorEnd( $FF0000 );
  initFountain;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.initFountain last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.initFountain;
var
  i: integer;
begin
  for i := 0 to 360 do
  begin
    TabCos[i] := Cos( i );
    TabSin[i] := Sin( i );
  end;
  Randomize;
  LsParticles := TListSPTR.Create( SIZE_STR_PARTICLE );
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.UpdateFountain; last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.UpdateFountain;
begin
  FNbParticles := 0;
  if assigned( LsParticles ) then
    LsParticles.Free;
  initFountain;
  NotifyChange( self );
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.AddParticle last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.AddParticle : boolean;
var
  PTime: TParticle;
begin
  Result := ( FActived ) and ( NbParticles < FMaxParticles );
  if Result then
  begin
    with PTime do
    begin
      Pos.X := 0.0;
      Pos.Y := FFloor;
      Pos.Z := 0.0;
      AngleStart := Random( FAngleStart );
      Velocity := ( Random( FVelocityMax - FVelocityMin ) + FVelocityMin ) * 0.1;
      Accel.X := TabCos[Round( AngleStart )] * Velocity * FFountainSize;
      Accel.Y := 0.0;
      Accel.Z := TabSin[Round( AngleStart )] * Velocity * FFountainSize;
      Times := 0.0;
      Life := 1.0;
      if FBounding then
        Bounding := 0
      else
        Bounding := 1;
      Width := ( Random( FParticlesSizeMax - FParticlesSizeMin ) + FParticlesSizeMin ) * 0.1;
      Color := AffineVectorMake( RD Div 255, GD Div 255, BD Div 255 );
      ColorDiff := AffineVectorMake( ( RF - RD ) / ( 1 / FLifeFactor ) / 255,
                                    ( GF - GD ) / ( 1 / FLifeFactor ) / 255,
                                    ( BF - BD ) / ( 1 / FLifeFactor ) / 255);
      end;
      Result := LsParticles.Add( @PTime );
      if Result then
        inc( FNbParticles );
  end;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.DeleteParticle last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.DeleteParticle;
  function LifeCheckParticle( const Particle: Pointer ): boolean;
  begin
    Result := ( TParticle( Particle^ ).Bounding > 0 ) and ( TParticle( Particle^ ).Life <= 0 )
  end;
begin
  if ( FActived ) then
    FNbParticles := FNbParticles - LsParticles.DeleteIf( @LifeCheckParticle );
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.CalculBoundPosParticles last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.CalculBoundPosParticles;
var
  RoadParticle: TParticle;
  BoundFactor: single;
begin
  if ( FActived ) then
  begin
    if LsParticles.GetFirst( @RoadParticle ) then
      repeat
    with RoadParticle do
    begin
      if ( Pos.Y < FFloor ) then
      begin
        if ( Life > 0 )  then
        begin
          Times := 0.0;
          BoundFactor := ( Velocity * FBoundingFactor * 0.01 );
          Velocity := Velocity - BoundFactor;
          Pos.X := Pos.X + Accel.X - BoundFactor;
          Pos.Z := Pos.Z + Accel.Z - BoundFactor;
          Pos.Y := FFloor;
          inc( Bounding );
        end
        end else
        begin
          if Bounding > 0 then
            Life := Life - FLifeFactor;
          Pos.X := Pos.X + Accel.X;
          Pos.Y := ( Pos.Y + Times + Velocity ) - ( F_GRAVITY + FParticleMass ) * Sqr( Times );
          Pos.Z := Pos.Z + Accel.Z;
        end;
        Color := VectorAdd( Color, ColorDiff );
        Times := Times + FTimesFactor;
        end;
        LsParticles.CurrentModify( @RoadParticle );
      until not
        LsParticles.GetNext( @RoadParticle );
  end;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.CalculPosParticles last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.CalculPosParticles;
var
  RoadParticle: TParticle;
begin
  if ( FActived ) then
  begin
    if LsParticles.GetFirst( @RoadParticle ) then
      repeat
    with RoadParticle do
    begin
      if ( Pos.Y >= FFloor ) then
      begin
        Life := Life - FLifeFactor;
        Pos.X := Pos.X + Accel.X;
        Pos.Y := ( Pos.Y + Times + Velocity ) -
        ( F_GRAVITY + FParticleMass ) * Sqr( Times );
        Pos.Z := Pos.Z + Accel.Z;
      end else
        Life := Life - FLifeFactor;
        Color := VectorAdd( Color, ColorDiff );
        Times := Times + FTimesFactor;
      end;
      LsParticles.CurrentModify( @RoadParticle );
    Until Not
      LsParticles.GetNext( @RoadParticle );
  end;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.DrawParticles last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.DrawParticles( rci : TGLRenderContextInfo );
var
  RoadParticle: TParticle;
  GMatrix: array[0..15] of GlFloat;
  VRight, VUp: TVector3f;
begin
  if LsParticles.GetFirst( @RoadParticle ) then
    repeat
    with RoadParticle do
    begin
      glGetFloatv( GL_MODELVIEW_MATRIX, @GMatrix );
      VRight := AffineVectorMake( GMatrix[00], GMatrix[04], GMatrix[08] );
      VUp := AffineVectorMake( GMatrix[01], GMatrix[05], GMatrix[09] );
      NormalizeVector( VRight );
      NormalizeVector( VUp );
      ScaleVector( VRight, Width / 2 );
      ScaleVector( VUp, Width / 2 );
      glColor4f( Color.X, Color.Y, Color.Z, Life );
      glbegin( GL_QUADS );
      glTexCoord2f( 0, 0 );
      glVertex3d( Pos.X - ( VRight.X + VUp.X ),
        Pos.Y - ( VRight.Y + VUp.Y ),
        Pos.Z - ( VRight.Z + VUp.Z ) );
      glTexCoord2f( 1, 0 );
      glVertex3d( Pos.X + ( VRight.X - VUp.X ),
        Pos.Y + ( VRight.Y - VUp.Y ),
        Pos.Z + ( VRight.Z - VUp.Z ) );
      glTexCoord2f( 1, 1 );
      glVertex3d( Pos.X + ( VRight.X + VUp.X ),
        Pos.Y + ( VRight.Y + VUp.Y ),
        Pos.Z + ( VRight.Z + VUp.Z ) );
      glTexCoord2f( 0, 1 );
      glVertex3d( Pos.X - ( VRight.X - VUp.X ),
        Pos.Y - ( VRight.Y - VUp.Y ),
        Pos.Z - ( VRight.Z - VUp.Z ) );
      glend();
    end;
    LsParticles.CurrentModify( @RoadParticle );
  Until Not
    LsParticles.GetNext( @RoadParticle );
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.Animation last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.Animation( rci : TGLRenderContextInfo );
begin
  AddParticle;
  DeleteParticle;
  if FBounding then
    CalculBoundPosParticles
  else
    CalculPosParticles;
  glPushMatrix;
    glEnable( GL_TEXTURE_2D );
    glBindTexture( GL_TEXTURE_2D, Material.Texture.Handle );
    glDepthMask(GLboolean(FALSE )); { *Converti depuis glDepthMask* }
    glEnable( GL_BLEND );
    glBlendFunc( GL_SRC_ALPHA, GL_ONE );
    glCullFace( GL_BACK );
    glEnable( GL_CULL_FACE );
    glDisable( GL_LIGHTING );
      DrawParticles( rci );
    glDisable( GL_TEXTURE_2D );
    glDisable( GL_BLEND );
    glDepthMask(GLboolean(TRUE )); { *Converti depuis glDepthMask* }
    glEnable( GL_LIGHTING );
    glDisable( GL_CULL_FACE );
  glPopMatrix;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.GetActived last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.GetActived: boolean;
begin
  Result := FActived;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.SetActived last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.SetActived( const Activ: boolean );
begin
  FActived := Activ;
  UpdateFountain;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.GetNbParticles last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.GetNbParticles: integer;
begin
  Result := FNbParticles;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.GetMaxParticles last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.GetMaxParticles: integer;
begin
  Result := FMaxParticles;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.SetMaxParticles last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.SetMaxParticles( const Max: integer );
begin
  FMaxParticles := Max;
  UpdateFountain;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.GetVelocityMax last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.GetVelocityMax: integer;
begin
  Result := FVelocityMax;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.SetVelocityMax last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.SetVelocityMax( const VeloMax: integer );
begin
  if ( VeloMax > FVelocityMin ) then
    FVelocityMax := VeloMax
  else
    FVelocityMax := FVelocityMin + 1;
  UpdateFountain;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.GetVelocityMin last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.GetVelocityMin: integer;
begin
  Result := FVelocityMin;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.SetVelocityMin last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.SetVelocityMin( const VeloMin: integer );
begin
  if ( VeloMin < FVelocityMax ) then
    FVelocityMin := VeloMin
  else
    FVelocityMin := FVelocityMax - 1;
  UpdateFountain;  
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.GetAngleStart last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.GetAngleStart: integer;
begin
  Result := FVelocityMin;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.SetAngleStart last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.SetAngleStart( const AngleS: integer );
begin
  if ( AngleS >= 0 ) and ( AngleS <= 360 ) then
    FAngleStart := AngleS
  else
    FAngleStart := 360;
  UpdateFountain;  
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.GetFloor last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.GetFloor: single;
begin
  Result := FFloor;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.SetFloor last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.SetFloor( const TheFloor: single );
begin
  FFloor := TheFloor;
  UpdateFountain;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.GetFountainSize last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.GetFountainSize: single;
begin
  Result := FFountainSize;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.SetFountainSize last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.SetFountainSize( const FountainSize: single );
begin
  FFountainSize := FountainSize;
  UpdateFountain;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.GetParticlesSizeMax last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.GetParticlesSizeMax: integer;
begin
  Result := FParticlesSizeMax;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.SetParticlesSizeMax last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.SetParticlesSizeMax( const PartMax: integer );
begin
  if ( PartMax > FParticlesSizeMin ) then
    FParticlesSizeMax := PartMax
  else
    FParticlesSizeMax := FParticlesSizeMin + 1;
  UpdateFountain;  
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.GetParticlesSizeMin last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.GetParticlesSizeMin: integer;
begin
  Result := FParticlesSizeMin;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.SetParticlesSizeMin last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.SetParticlesSizeMin( const PartMin: integer );
begin
  if ( PartMin < FParticlesSizeMax ) then
    FParticlesSizeMin := PartMin
  else
    FParticlesSizeMin := FParticlesSizeMax - 1;
  UpdateFountain;  
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.GetBoundingFact last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.GetBoundingFact: single;
begin
  Result := FBoundingFactor;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.SetBoundingFact last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.SetBoundingFact( const BoundSize: single );
begin
  if ( BoundSize >= 0 ) and ( BoundSize <= 100 ) then
    FBoundingFactor := BoundSize
  else
    FBoundingFactor := 100;
  UpdateFountain;  
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.GetParticlesMass last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.GetParticlesMass: single;
begin
  Result := FParticleMass;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.SetParticlesMass last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.SetParticlesMass( const Mass: single );
begin
  FParticleMass := Mass;
  UpdateFountain;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.GetTimesFactor last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.GetTimesFactor: double;
begin
  Result := FTimesFactor;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.SetTimesFactor last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.SetTimesFactor( const TimesFact: double );
begin
  FTimesFactor := TimesFact;
  UpdateFountain;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.GetLifeFactor last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.GetLifeFactor: single;
begin
  Result := FLifeFactor;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.SetLifeFactor last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.SetLifeFactor( const LifeFact: single );
begin
  if LifeFact > 0 then
    FLifeFactor := LifeFact
  else
    FLifeFactor := 0.005;
  UpdateFountain;  
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.GetBounding last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.GetBounding: boolean;
begin
  Result := FBounding;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.SetBounding last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.SetBounding( const Bound: boolean );
begin
  FBounding := Bound;
  UpdateFountain;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.GetColorStart last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.GetColorStart: longint;
begin
  Result := FColorStart;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.SetColorStart last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.SetColorStart( const ColStart: longint );
begin
  FColorStart := ColStart;
  RD := FColorStart;
  GD := FColorStart Shr 8;
  BD := FColorStart Shr 16;
  UpdateFountain;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.GetColorEnd last change by Dave Gravel
{******************************************************************************}
function TGLFountainDummy.GetColorEnd: longint;
begin
  Result := FColorEnd;
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.SetColorEnd last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.SetColorEnd( const ColEnd: longint );
begin
  FColorEnd := ColEnd;
  RF := FColorEnd;
  GF := FColorEnd Shr 8;
  BF := FColorEnd Shr 16;
  UpdateFountain;
end;

{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.DoRender last change by Dave Gravel
{******************************************************************************}
procedure TGLFountainDummy.DoRender( var rci : TGLRenderContextInfo; renderSelf, renderChildren : Boolean );
begin
  Animation( rci );
  if renderChildren then
    Self.RenderChildren( 0, Count - 1, rci );
end;
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy.Destroy last change by Dave Gravel
{******************************************************************************}
destructor TGLFountainDummy.Destroy;
begin
  FNbParticles := 0;
  LsParticles.Free;
  DeleteChildren;
  inherited Destroy;
end;
{******************************************************************************}
initialization
{******************************************************************************}
 // [2005-06-09]: TGLFountainDummy last change by Dave Gravel
{******************************************************************************}
RegisterClass( TGLFountainDummy );

end.
