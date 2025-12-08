(*====< GLZArrayClasses.pas >===================================================@br
  @created(2017-04-17)
  @author(J.Delauney (BeanzMaster) - Peter Dyson (Dicepd) )
  Historique : @br
  @unorderedList(
    @item(21/01/2018 : Creation  )
  )
--------------------------------------------------------------------------------@br

  @bold(Description :)@br
  Generics base classes for managing array thrue pointer

  ------------------------------------------------------------------------------@br
  @bold(Notes) : @br

  ------------------------------------------------------------------------------@br
  @bold(BUGS :)@br
  @unorderedList(
     @item()
  )
  ------------------------------------------------------------------------------@br
  @bold(TODO :)@br
  @unorderedList(
     @item()
  )

  ------------------------------------------------------------------------------@br
  @bold(Credits :)
   @unorderedList(
     @item(FPC/Lazarus)
   )

  ------------------------------------------------------------------------------@br
  @bold(LICENCE :) MPL / GPL @br
  @br
 *==============================================================================*)
unit GLZArrayClasses;

{$mode objfpc}{$H+}

{$IFDEF CPU64}
  {$CODEALIGN LOCALMIN=16} // ??? needed here ????
{$ENDIF}

interface

uses
  Classes, SysUtils;
  //GLZTypes, GLZClasses, GLZPersistentClasses;

const
  cDefaultListGrowthDelta = 16;

Type

  { TGLZBaseArray }

  generic TGLZBaseArray<T> = class //(TGLZPersistentObject)
  private
    F: boolean;
    FTagString : string;

    type
      PT = ^ T;
      TArr = array of T;
      PArr = ^TArr;

    procedure SetCount(AValue : SizeUInt);


  protected
    var
      {$CODEALIGN RECORDMIN=16}
      FData: TArr;  // The base list pointer (untyped)
      {$CODEALIGN RECORDMIN=4}

      FCapacity:SizeUInt;
      FDataSize:SizeUInt;
      FItemSize:SizeUInt; // Must be defined in subclasses
      FGrowthDelta: Integer;
      FParentData: Pointer;
      FHandle: SizeUInt;
      FIsDirty: boolean;

      FRevision: LongWord;
      FCount: SizeUInt;

      FPosition: SizeUInt;
      FFirstDone: Boolean;


    Function GetData: Pointer; inline;
    function GetValue(Position: SizeUInt): T; inline;
   procedure SetValue(Position : SizeUInt; const AValue : T);

    function GetMutable(Position: SizeUInt): PT; inline;
    procedure IncreaseCapacity; inline;

    procedure SetCapacity(NewCapacity: SizeUInt); virtual;

    //  persistency support.
    //procedure ReadItemsData(AReader : TReader); virtual;
    //procedure WriteItemsData(AWriter : TWriter); virtual;
    //procedure DefineProperties(AFiler: TFiler); override;

  public
  { Public Declarations }
    constructor Create; //override;
    constructor Create(Reserved: SizeUInt); overload;
    constructor CreateParented(AParentData: Pointer; Reserved: SizeUInt); overload;
    destructor Destroy; override;
    //procedure Assign(Src: TPersistent); override;

    //procedure WriteToFiler(writer: TVirtualWriter); override;
    //procedure ReadFromFiler(reader: TVirtualReader); override;

    function DataSize: SizeUInt; // size of the list
    function ItemSize: Byte; // Size of 1 item

    // Management
    function Add(const Value: T):SizeUInt; inline;
    procedure Insert(Position: SizeUInt; const Value: T); inline;
    procedure Delete(Position : SizeUInt); inline;

    procedure Exchange(index1, index2: SizeUInt); inline;
    //procedure Move(curIndex, newIndex: SizeUInt); inline;
    procedure Reverse; inline;

    //procedure AddNulls(nbVals: Cardinal); inline;
    //procedure InsertNulls(Position : SizeUInt; nbVals: Cardinal); inline;

    { Empties the list without altering capacity. }
    procedure Flush;  inline;
    { Empties the list and release. }
    procedure Clear; inline;

    // LIFO
    procedure Push(const Value: T);inline;
    function Pop: T; inline;

    // Array Iterators
    function First: T; inline;
    function Last: T; inline;
    function Next: T; inline;
    function Prev: T; inline;
    function Current : T; inline;
    function MoveNext:Boolean; inline;
    function MovePrev:Boolean; inline;
    function MoveFirst:Boolean; inline;
    function MoveLast:Boolean; inline;
    function GetPosition : SizeUInt;
    function Seek(const pos : SizeUInt; const StartAt : Byte) : boolean;  inline;
    function MoveTo(Position:SizeUInt) : Boolean; inline;
    function IsEndOfArray : Boolean; inline;

    // Array Rasterizer
    // function Scan(CallBack):Boolean;
    // function ScanNext(CallBack):Boolean;
    // function ScanPrev(CallBack):Boolean;

    // function ScanMany(nbItem,CallBack):Boolean;
    // function ScanTo(Position,CallBack):Boolean;

    // function ScanAll(CallBack):Boolean;
    // function ScanRange(From, To, CallBack):Boolean;

    // Array Utils

    // function CompareItems(Index1, index2, comparefunc): Integer;
    // procedure Sort(Const Direction : byte);
    // procedure Merge(AnotherArray: TGLZBaseArray<T>);
    // function Clone : TGLZBaseArray<T>;
    // function Extract(From, Nb : SizeUInt): TGLZBaseArray<T>;

    // Extra funcs for management
    // function InsertItemsAt(Pos:SizeUInt; AnArray : TGLZBaseArray<T>):Boolean;
    // function InsertItemsAtEnd
    // function InsertItemsAtFirst
    // procedure DeleteItems(Index: SizeUIntr; nbVals: Cardinal); inline;

    // Properties
   { Nb of items in the list. When assigning a Count, added items are reset to zero. }
    property Count: SizeUInt read FCount write SetCount;
    { Current list capacity.Not persistent. }
    property Capacity: SizeUInt read FCapacity write SetCapacity;
    { List growth granularity. Not persistent. }
    property GrowthDelta: Integer read FGrowthDelta write FGrowthDelta;

    property TagString: string read FTagString write FTagString;
    { Increase by one after every content changes. }
    property Revision: LongWord read FRevision write FRevision;

    property ParentData : Pointer read FParentData;
    property Data : Pointer read GetData;
    property Handle : SizeUInt read FHandle;
    property IsDirty : boolean read FIsDirty write f;
    property Items[i : SizeUInt]: T read getValue write SetValue;// default;
    property Mutable[i : SizeUInt]: PT read getMutable;
  end;

  { TGLZBaseArray2D }

  generic TGLZBaseArrayMap2D<T> = class(specialize TGLZBaseArray<T>)
  private

    function GetValue2D(x, y : SizeUInt): T;
    procedure SetValue2D(x, y : SizeUInt; AValue: T);
  protected
    FRows, FCols : SizeUInt;

  public
    constructor Create(Rows, Cols: SizeUInt); overload;
    constructor CreateParented(AParentData: Pointer; Rows, Cols: SizeUInt); overload;

    function MoveTo(Row : Integer; Position : Integer) : Boolean; overload;

    property Items[x,y : SizeUInt]: T read GetValue2D write SetValue2D;
    property RowCount : SizeUInt read FRows;
    property ColCount : SizeUInt read FCols;

  end;

  //generic TGLZBaseArrayMap3D<T> = class(specialize TGLZBaseArray<T>)
  //private
  //  function GetValue3D(x, y, z : SizeUInt): T;
  //  procedure SetValue3D(x, y, z : SizeUInt; AValue: T);
  //published
  //public
  //  constructor Create(Rows, Cols, DCols : SizeUInt); overload;
  //  constructor CreateParented(AParentData: Pointer; Rows, Cols, DCols: SizeUInt); overload;
  //  property Items[x,y,z : SizeUInt]: T read GetValue3D write SetValue3D;
  //end;
  //
  //generic TGLZBaseArrayMap4D<T> = class(specialize TGLZBaseArray<T>)
  //private
  //  function GetValue4D(x, y, z, w : SizeUInt): T;
  //  procedure SetValue4D(x, y, z, w : SizeUInt; AValue: T);
  //published
  //public
  //  constructor Create(Rows, Cols, DCols, TCols: SizeUInt); overload;
  //  constructor CreateParented(AParentData: Pointer; Rows, Cols, DCols, TCols: SizeUInt); overload;
  //  property Items[x,y,z,w : SizeUInt]: T read GetValue4D write SetValue4D;
  //end;


implementation
{$ifdef DEBUGLOG}
uses GLZLogger;
{$endif}
{%region%=====[ TGLZBaseArray ]=================================================}

procedure TGLZBaseArray.SetCount(AValue : SizeUInt);
begin
  {$ifdef DEBUG}
    Assert(AValue >= 0);
  {$endif}
  if FCount = AValue then Exit;
  if AValue> FCapacity then SetCapacity(AValue);
  //if (AValue > FCount) and (bloSetCountResetsMemory in FOptions) then
  // FillChar(FBaseList[FItemSize * FCount], (Val - FCount) * FItemSize, 0);
  FCount := AValue;
  Inc(FRevision);
end;

function TGLZBaseArray.GetData : Pointer;
begin
  Result := @FData;
end;

function TGLZBaseArray.GetValue(Position : SizeUInt) : T;
begin
  {$ifdef DEBUG}
     Assert((position < size) and (position>=0), SVectorPositionOutOfRange);
  {$endif}
  Result := FData[Position];
end;

procedure TGLZBaseArray.SetValue(Position : SizeUInt; const AValue : T);
begin
  {$ifdef DEBUG}
     Assert((position < size) and (position>=0), SVectorPositionOutOfRange);
  {$endif}
  {$ifdef DEBUGLOG}
  GlobalLogger.LogStatus('Set value at : '+Inttostr(Position));
  {$endif}
  //if FData[Position] = AValue then exit;
  FData[Position] := AValue;
end;

function TGLZBaseArray.GetMutable(Position : SizeUInt) : PT;
begin
  {$ifdef DEBUG}
     Assert((position < size) and (position>=0), SVectorPositionOutOfRange);
  {$endif}
  Result := @FData[Position];
end;

procedure TGLZBaseArray.IncreaseCapacity;
begin
  if FCapacity=0 then SetCapacity(1)
  else
    SetCapacity(FCapacity+FGrowthDelta);
end;

procedure TGLZBaseArray.SetCapacity(NewCapacity : SizeUInt);
begin
  if FCapacity = newCapacity then exit;
  //if bloExternalMemory in FOptions then
  //begin
  //  Exclude(FOptions, bloExternalMemory);
  //  FBaseList := nil;
  //end;
  //ReallocMem(FBaseList, newCapacity * FItemSize);
  FCapacity := newCapacity;
  SetLength(FData, FCapacity);
  Inc(FRevision);
end;

constructor TGLZBaseArray.Create;
begin
  inherited Create;
  FCapacity:=0;
 // FItemSize:=Sizeof(T); // Must be defined in subclasses  ????
  FGrowthDelta:= cDefaultListGrowthDelta;
  FParentData:=nil;
  FHandle:=0;
  FIsDirty:=false;
  FRevision:=0;
  FCount:=0;
  FPosition:=0;
  FFirstDone:=false;
end;

constructor TGLZBaseArray.Create(Reserved : SizeUInt);
begin
  Create;
  FDataSize:=Reserved*ItemSize;
  SetCapacity(Reserved);
end;

constructor TGLZBaseArray.CreateParented(AParentData : Pointer; Reserved : SizeUInt);
begin
  Create(Reserved);
  FParentData := AParentData;
end;

destructor TGLZBaseArray.Destroy;
begin
  Clear;
  //SetLength(FData, 0);
  FData := nil;
  inherited Destroy;
end;

function TGLZBaseArray.DataSize : SizeUInt;
begin
  Result := FCount * ItemSize; //FDataSize;
end;

function TGLZBaseArray.ItemSize : Byte;
begin
  Result := Sizeof(T); //FItemSize;
end;

function TGLZBaseArray.Add(const Value : T) : SizeUInt;
begin

  Result := FCount;
  if Result >= FCapacity then IncreaseCapacity;
  FData[Result] := Value;

  Inc(FCount);
end;

procedure TGLZBaseArray.Insert(Position : SizeUInt; const Value : T);
begin
  {$ifdef DEBUG}
      Assert(Position < FCount);
  {$endif}
  if FCount = FCapacity then IncreaseCapacity;
  if Position < FCount then
    System.Move(FData[Position], FData[Position + 1], (FCount - Position) * FItemSize);
  FData[Position] := Value;
  Inc(FCount);
end;

procedure TGLZBaseArray.Delete(Position : SizeUInt);
begin
  {$ifdef DEBUG}
    Assert(Position < FCount-1);
  {$endif}
  Dec(FCount);
  System.Move(FData[(Position + 1)],  // * FItemSize],
      FData[Position],                // * FItemSize],
      (FCount - Position));           // * FItemSize);
  Inc(FRevision);
end;

procedure TGLZBaseArray.Exchange(index1, index2 : SizeUInt);
var
  temp : T;
begin
  {$ifdef DEBUG}
    Assert((Index1 < FCount) and (Index2 < FCount));
  {$endif}
  temp := FData[index1];
  FData[index1] := FData[index2];
  FData[index2] := temp;
  Inc(FRevision);
end;

//procedure TGLZBaseArray.Move(curIndex, newIndex : SizeUInt);
//begin
//
//end;

procedure TGLZBaseArray.Reverse;
var
  s, e: Integer;
begin
  s := 0;
  e := FCount - 1;
  while s < e do
  begin
    Exchange(s, e);
    Inc(s);
    Dec(e);
  end;
  Inc(FRevision);
end;

//procedure TGLZBaseArray.AddNulls(nbVals : Cardinal);
//begin
//
//end;
//
//procedure TGLZBaseArray.InsertNulls(Position : SizeUInt; nbVals : Cardinal);
//begin
//
//end;

procedure TGLZBaseArray.Flush;
begin
  SetCount(0);
end;

procedure TGLZBaseArray.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

//procedure TGLZBaseArray.AdjustCapacityToAtLeast(const size: Integer);
//begin
//  if FCapacity < Size then SetCapacity(size);
//end;

procedure TGLZBaseArray.Push(const Value : T);
begin
  Add(Value);
end;

function TGLZBaseArray.Pop : T;
begin
 Result := FData[FCount-1];
end;

function TGLZBaseArray.First : T;
begin
  Result := FData[0];
end;

function TGLZBaseArray.Last : T;
begin
  Result := Pop;
end;

function TGLZBaseArray.Next : T;
begin
  Result := FData[FPosition];
  if (FPosition < FCount) then Inc(FPosition);
end;

function TGLZBaseArray.Prev : T;
begin
  Result := FData[FPosition];
  if (FPosition > 0) then Dec(FPosition);
end;

function TGLZBaseArray.Current : T;
begin
 Result := FData[FPosition];
end;

function TGLZBaseArray.MoveNext : Boolean;
begin
  Result := false;
  if (FPosition >= FCount-1) then exit;
  Result := True;
  Inc(FPosition);
end;

function TGLZBaseArray.MovePrev : Boolean;
begin
  Result := false;
  if (FPosition <= 0 ) then exit;
  Result := True;
  Dec(FPosition);
end;

function TGLZBaseArray.MoveFirst : Boolean;
begin
 {$ifdef DEBUG}
    Assert(FCount>0);
  {$endif}
  result := true;
  FPosition := 0;
end;

function TGLZBaseArray.MoveLast : Boolean;
begin
  {$ifdef DEBUG}
     Assert(FCount>0);
  {$endif}
  result := true;
  FPosition := FCount-1;
end;

function TGLZBaseArray.GetPosition : SizeUInt;
begin
  Result := FPosition;
end;

function TGLZBaseArray.Seek(const pos : SizeUInt; const StartAt : Byte) : boolean;
var
  newpos : SizeUInt;
begin
  {$ifdef DEBUG}
    Assert(Position < FCount);
  {$endif}
  result := true;
  Case StartAt of
    0: newpos := Pos; // From Beginning
    1:
    begin
      newpos := (FPosition-1) + Pos; // From Current positon
      if newpos >= FCount then
      begin
        //newpos := FCount-1;
        result := false;
      end;
    end;
    2:
    begin
      newpos := (FCount-1) - Pos; // From End;
      if newpos=0 then
      begin
        //newpos := 0;
        result := false;
      end;
    end;
    else newpos := pos;
  end;
  if result then FPosition := newpos;
end;

function TGLZBaseArray.MoveTo(Position:SizeUInt) : Boolean;
begin
  result:= Self.Seek(Position, 0);
end;

function TGLZBaseArray.IsEndOfArray : Boolean;
begin
  result := (FPosition >= FCount);
end;

{%endregion%}

{%region%=====[ TGLZBaseArrayMap2D ]============================================}

function TGLZBaseArrayMap2D.GetValue2D(x, y : SizeUInt) : T;
begin
  {$ifdef DEBUG}
    assert((x<FCols) and (y<FRows));
  {$endif}
  Result := FData[y*FCols+X];
end;

procedure TGLZBaseArrayMap2D.SetValue2D(x, y : SizeUInt; AValue : T);
var
  pos : SizeUint;
begin
  {$ifdef DEBUG}
    assert((x<FCols) and (y<FRows));
  {$endif}
  pos := (y*FCols+x);
  {$ifdef DEBUGLOG}
  GlobalLogger.LogStatus('NbRows, NbCols : '+Inttostr(FRows)+', '+Inttostr(FCols));
  GlobalLogger.LogStatus('Set to : '+Inttostr(x)+', '+Inttostr(y));
  GlobalLogger.LogStatus('Set at : '+Inttostr(pos));
  {$endif}
  if FData[pos] = AValue then exit;
  FData[pos] := AValue;
end;

constructor TGLZBaseArrayMap2D.Create(Rows, Cols : SizeUInt);
begin
  Inherited Create(Rows*Cols);
  FRows := Rows;
  FCols := Cols;
end;

constructor TGLZBaseArrayMap2D.CreateParented(AParentData : Pointer; Rows, Cols : SizeUInt);
begin
  Inherited CreateParented(AParentData, Rows*Cols);
end;

function TGLZBaseArrayMap2D.MoveTo(Row : Integer; Position : Integer) : Boolean;
begin
  result := Inherited MoveTo(Row*Position);
end;

{%endregion%}

end.

