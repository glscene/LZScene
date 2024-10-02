unit uParser;


{ This parser is used only for z = f(x,y) heightfield 3D graphs }

interface

uses
  Classes;

const
  Pi: extended = 3.1415926535897932385;
  PiOn2: extended = 1.5707963267948966192;
  twoPi: extended = 6.2831853071795864769;
  PiOn180: extended = 0.017453292519943296;

  ParseSet: Set of Char =
           [' ', '!', '(', ')', '*', '+', '-', '.', ',', '/', '0'..'9',
            'A'..'E', 'G'..'I', 'L', 'N'..'U', 'X', 'Y', '^', '`', #8];

type
  TFuncDef = class
  public
    function DefName: string; virtual; abstract;
    function Eval(x: extended): extended; virtual; abstract;
  end;

  TVarDef = class
  public
    VarName: string;
    Value: extended;
  end;

  TCalculus = class
  public
    function Eval: extended; virtual; abstract;
  end;

  TfxyParser = class(TObject)
    constructor Create(x, y: extended);
    destructor Destroy; override;
  private
    FunctionList: TList;
    VariableList: TList;
    function FunctionOf(i: integer): TFuncDef;
    function VariableOf(i: integer): TVarDef;
    function CheckBrackets(const s: string): Boolean;
    function CompileExpresion(const s: string; var Error: byte): TCalculus;
    function FactorCompile(const s: string; var Error: byte): TCalculus;
    function SimpleCompile(const s: string; var Error: byte): TCalculus;
    procedure ClearLists;
  public
    VarX, VarY: TVarDef;
    Calculus: TCalculus;
    ErrorByte: byte;
    function Compile(s: string; var Error: byte): TCalculus;
    procedure AddVar(v: TVarDef);
    procedure ConstructLists;
  end;

  TConst = class(TCalculus)
    constructor Create(c: extended);
  public
    function Eval: extended; override;
  private
    Val: extended;
  end;

  TVar = class(TCalculus)
  public
    constructor Create(v: TVarDef);
    function Eval: extended; override;
  protected
    Def: TVarDef;
  end;

  TFunc = class(TCalculus)
    constructor Create(v: TCalculus; f: TFuncDef);
    destructor Destroy; override;
  public
    function Eval: extended; override;
  protected
    Variable: TCalculus;
    Def: TFuncDef;
  end;

  TOperator = class(TCalculus)
    constructor Create(c1, c2: TCalculus);
    destructor Destroy; override;
  public
  protected
    e1, e2: TCalculus;
  end;

  TMinus = class(TOperator)
  public
    function Eval: extended; override;
  end;

  TSum = class(TOperator)
  public
    function Eval: extended; override;
  end;

  TProduct = class(TOperator)
  public
    function Eval: extended; override;
  end;

  TDivision = class(TOperator)
  public
    function Eval: extended; override;
  end;

  TPower = class(TOperator)
  public
    function Eval: extended; override;
  end;

  TFactorial = class(TOperator)
  public
    function Eval: extended; override;
  end;

  TDegToRad = class(TOperator)
  public
    function Eval: extended; override;
  end;

  TAbs = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TInt = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TRound = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TSqr = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TSqrt = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TSin = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TCos = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TTan = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TCsc = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TSec = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TCot = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TArcSin = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TArcCos = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TArcTan = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TArcCsc = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TArcSec = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TArcCot = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TLn = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TExp = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TExp1 = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TLog10 = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TLog2 = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TSinh = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TCosh = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TTanh = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TCsch = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TSech = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TCoth = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TArcSinh = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TArcCosh = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TArcTanh = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TArcCsch = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TArcSech = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

  TArcCoth = class(TFuncDef)
  public
    function DefName: string; override;
    function Eval(x: extended): extended; override;
  end;

function ScanText(const s: string): string;
function ParseAndEvaluate(const aText: string; var e: byte): extended;
function ParseEvaluateFxy(const aVarX, aVarY: extended;
                          const aText: string; var e: byte): extended;

implementation

uses
  uGlobal,
  Main,
  Math,
  GLVectorGeometry,
  SysUtils;

{ TCalculus Class }
constructor TConst.Create(c: extended);
begin
  Val := c;
end;

function TConst.Eval: extended;
begin
  Result := Val;
end;

constructor TVar.Create(v: TVarDef);
begin
  Def := v;
end;

function TVar.Eval: extended;
begin
  Result := Def.value;
end;

constructor TFunc.Create(v: TCalculus; f: TFuncDef);
begin
  Variable := v;
  Def := f;
end;

destructor TFunc.Destroy;
begin
  Variable.Free;
end;

function TFunc.Eval: extended;
begin
  Result := Def.Eval(Variable.Eval);
end;

constructor TOperator.Create(c1, c2: TCalculus);
begin
  e1 := c1;
  e2 := c2;
end;

destructor TOperator.Destroy;
begin
  e1.Free;
  e2.Free;
end;
{ TCalculus Class }

{ TfxyParser }
constructor TfxyParser.Create(x, y: extended);
begin
  inherited Create;
  FunctionList := TList.Create;
  VariableList := TList.Create;

  ConstructLists;

  VarX := TVarDef.Create;
  VarX.VarName := 'x';
  VarX.Value := x;
  addVar(VarX);

  VarY := TVarDef.Create;
  VarY.VarName := 'y';
  VarY.Value := y;
  addVar(VarY);
end;

destructor TfxyParser.Destroy;
begin
  ClearLists;
  inherited Destroy;
end;

function TfxyParser.FunctionOf(i: integer): TFuncDef;
begin
  Result := TFuncDef(FunctionList.Items[i]);
end;

function TfxyParser.VariableOf(i: integer): TVarDef;
begin
  Result := TVarDef(VariableList.Items[i]);
end;

function TfxyParser.CheckBrackets(const s: string): Boolean;
var
  i, j, c1, c2: integer;

begin
  c1 := 0;
  c2 := 0;
  i := 1;
  j := Length(s);
  while i <= j do
  begin
    if s[i] = '(' then Inc(c1);
    if s[i] = ')' then Inc(c2);
    Inc(i);
  end;
  Result := c1 = c2;
end;

function TfxyParser.CompileExpresion(const s: string; var Error: byte): TCalculus;
var
  i: integer;
  e1: byte;
  e2: byte;
  c1, c2: TCalculus;

begin
  if s = '' then
  begin
    Error := 3;
    Result := nil;
    Exit;
  end;

  if not CheckBrackets(s) then
  begin
    Error := 1;
    Result := nil;
    Exit;
  end;

 {----- -factor -----}
  if s[1] = '-' then
  begin
    c1 := FactorCompile(copy(s, 2, length(s)-1), e1);
    if e1 = 0 then
    begin
      c2 := TConst.Create(0);
      Result := TMinus.Create(c2, c1);
      Error := 0;
      Exit;
    end;
  end;

 {----- exp+factor -----}
 {----- exp-factor -----}
 {----- exp!factor -----}
 {----- exp°factor -----}
  for i := length(s) downto 1 do
  begin
    case s[i] of
 '+': begin
        c1 := CompileExpresion(copy(s, 1, i -1), e1);
        if e1 = 0 then
        begin
          c2 := FactorCompile(copy(s, i +1, length(s) -i), e2);
          if e2 = 0 then
          begin
            Result := TSum.Create(c1, c2);
            Error := 0;
            Exit;
          end
          else c1.Free;
        end;
      end;
 '-': begin
        c1 := CompileExpresion(copy(s, 1, i -1), e1);
        if e1 = 0 then
        begin
          c2 := FactorCompile(copy(s, i +1, length(s) -i), e2);
          if e2 = 0 then
          begin
            Result := TMinus.Create(c1, c2);
            Error := 0;
            Exit;
          end
          else c1.Free;
        end;
      end;
 '!': begin
        c1 := CompileExpresion(copy(s, 1, i -1), e1);
        if e1 = 0 then
        begin
          c2 := FactorCompile(copy(s, 1, i -1), e2);
          if e2 = 0 then
          begin
            Result := TFactorial.Create(c1, c2);
            Error := 0;
            Exit;
          end
          else c1.Free;
        end;
      end;
 char(#176): begin
        c1 := CompileExpresion(copy(s, 1, i -1), e1);
        if e1 = 0 then
        begin
          c2 := FactorCompile(copy(s, 1, i -1), e2);
          if e2 = 0 then
          begin
            Result := TDegToRad.Create(c1, c2);
            Error := 0;
            Exit;
          end
          else c1.Free;
        end;
      end;
    end;  { case s[i] of... }
  end;  { for i := length(s) downto 1 do... }
  Result := FactorCompile(s, Error);
end;

function TfxyParser.FactorCompile(const s: string; var Error: byte): TCalculus;
var
  i: integer;
  e1, e2: byte;
  c1, c2: TCalculus;

begin
  if s = '' then
  begin
    Error := 3;
    Result := nil;
    Exit;
  end;

  if not CheckBrackets(s) then
  begin
    Error := 1;
    Result := nil;
    Exit;
  end;

 {----- factor*simple -----}
 {----- factor/simple -----}
  for i := length(s) downto 1 do
  begin
    case s[i] of
 '*': begin
        c1 := FactorCompile(copy(s, 1, i -1), e1);
        if e1 = 0 then
        begin
          c2 := SimpleCompile(copy(s, i +1, length(s) -i), e2);
          if e2 = 0 then
          begin
            Result := TProduct.Create(c1, c2);
            Error := 0;
            Exit;
          end
          else c1.Free;
        end;
      end;
 '/': begin
        c1 := FactorCompile(copy(s, 1, i -1), e1);
        if e1 = 0 then
        begin
          c2 := SimpleCompile(copy(s, i +1, length(s) -i), e2);
          if e2 = 0 then
          begin
            Result := TDivision.Create(c1, c2);
            Error := 0;
            Exit;
          end
          else c1.Free;
        end;
      end;
    end;  { case s[i] of... }
  end;  { for i := length(s) downto 1 do... }
  Result := SimpleCompile(s, Error);
end;

function TfxyParser.SimpleCompile(const s: string; var Error: byte): TCalculus;
var
  i: integer;
  e1, e2: byte;
  c1, c2: TCalculus;
  d: extended;

begin
  if s = '' then
  begin
    Error := 3;
    Result := nil;
    Exit;
  end;

  if not CheckBrackets(s) then
  begin
    Error := 1;
    Result := nil;
    Exit;
  end;

 {----- const -----}
  Val(s, d, i);
  if i = 0 then
  begin
    Result := TConst.Create(d);
    Error := 0;
    Exit;
  end;

 {----- (exp) -----}
  if (s[1] = '(') and (s[length(s)] = ')') then
  begin
    c1 := CompileExpresion(copy(s, 2, length(s)-2), e1);
    if e1 = 0 then
    begin
      Result := c1;
      Error := 0;
      Exit;
    end;
  end;

 {----- VarName -----}
  for i := 0 to VariableList.Count -1 do
  begin
    if s = VariableOf(i).VarName then
    begin
      Result := TVar.Create(VariableOf(i));
      Error := 0;
      Exit;
    end;
  end;

 {----- DefNameFunc(exp) -----}
  for i := 0 to FunctionList.Count -1 do
  begin
    if (Pos(FunctionOf(i).DefName + '(', s) = 1) and (s[length(s)] = ')')
    then
    begin
      c1 := CompileExpresion(copy(s, length(FunctionOf(i).DefName) +2,
                         length(s) - length(FunctionOf(i).DefName) -2), e1);
      if e1 = 0 then
      begin
        Result := TFunc.Create(c1, FunctionOf(i));
        Error := 0;
        Exit;
      end;
    end;
  end;

 {----- simple^simple -----}
  for i := 1 to length(s) do
  begin
    case s[i] of
 '^': begin
        c1 := SimpleCompile(copy(s, 1, i -1), e1);
        if e1 = 0 then
        begin
          c2 := SimpleCompile(copy(s, i +1, length(s) -i), e2);
          if e2 = 0 then
          begin
            Result := TPower.Create(c1, c2);
            Error := 0;
            Exit;
          end
          else c1.Free;
        end;
      end;
    end;  { case s[i] of... }
  end;  { for i := 1 to length(s) do... }

  Error := 2;
  Result := nil;
end;

function TfxyParser.Compile(s: string; var Error: byte): TCalculus;
begin
  Result := CompileExpresion(s, Error);
end;

procedure TfxyParser.AddVar(v: TVarDef);
begin
  VariableList.Add(v);
end;

procedure TfxyParser.ConstructLists;
var
  v: TVarDef;
begin
  with FunctionList do
  begin
    Add(TAbs.Create);
    Add(TInt.Create);
    Add(TRound.Create);
    Add(TSqr.Create);
    Add(TSqrt.Create);
    Add(TSin.Create);
    Add(TCos.Create);
    Add(TTan.Create);
    Add(TCsc.Create);
    Add(TSec.Create);
    Add(TCot.Create);

    Add(TArcSin.Create);
    Add(TArcCos.Create);
    Add(TArcTan.Create);
    Add(TArcCsc.Create);
    Add(TArcSec.Create);
    Add(TArcCot.Create);

    Add(TLn.Create);
    Add(TExp.Create);
    Add(TExp1.Create);
    Add(TLog10.Create);
    Add(TLog2.Create);

    Add(TSinh.Create);
    Add(TCosh.Create);
    Add(TTanh.Create);
    Add(TCsch.Create);
    Add(TSech.Create);
    Add(TCoth.Create);

    Add(TArcSinh.Create);
    Add(TArcCosh.Create);
    Add(TArcTanh.Create);
    Add(TArcCsch.Create);
    Add(TArcSech.Create);
    Add(TArcCoth.Create);
  end;

  v := TVarDef.Create;
  v.VarName := 'pi';
  v.Value := Pi;
  VariableList.Add(v);
  v := TVarDef.Create;
  v.VarName := '2pi';
  v.Value := twoPi;
  VariableList.Add(v);
end;

procedure TfxyParser.ClearLists;
var
  i: integer;

begin
  for i := 0 to FunctionList.Count -1 do TFuncDef(FunctionList[i]).Free;
  FunctionList.Free;
  for i := 0 to VariableList.Count -1 do TVarDef(VariableList[i]).Free;
  VariableList.Free;
end;
{ TfxyParser }

{ TOperator Class }
function TMinus.Eval: extended;
begin
  Result := e1.Eval - e2.Eval;
end;

function TSum.Eval: extended;
begin
  Result := e1.Eval + e2.Eval;
end;

function TProduct.Eval: extended;
begin
  Result := e1.Eval * e2.Eval;
end;

function TDivision.Eval: extended;
begin
  if IsInfinite(e2.Eval) then Result := NaN else Result := e1.Eval/e2.Eval;
end;

function TPower.Eval: extended;
{ For fractional exponents or exponents greater than MaxInt,
  base must be greater than 0. }
begin
{ e1.Eval base/mantissa e2.Eval exponent }
  if e1.Eval = 0 then Result := 0 else Result := Power(e1.Eval, e2.Eval)
end;

function TFactorial.Eval: extended;
var
  i, j: integer;

begin
  j := round(e1.Eval);
  if (j < 0) or (j > 1754) then Result := 0.0
  else
  begin
    Result := 1.0;
    for i := 2 to j do Result := i*Result;
  end;
end;

function TDegToRad.Eval: extended;
begin
  Result := e1.Eval*PiOn180;
end;

function TAbs.DefName: string;
begin
  Result := 'abs';
end;

function TAbs.Eval(x: extended): extended;
begin
  Result := Abs(x);
end;

function TInt.DefName: string;
begin
  Result := 'int';
end;

function TInt.Eval(x: extended): extended;
begin
  Result := Int(x);
end;

function TRound.DefName: string;
begin
  Result := 'round';
end;

function TRound.Eval(x: extended): extended;
begin
  Result := round(x);
end;

function TSqr.DefName: string;
begin
  Result := 'sqr';
end;

function TSqr.Eval(x: extended): extended;
begin
  Result := Sqr(x);
end;

function TSqrt.DefName: string;
begin
  Result := 'sqrt';
end;

function TSqrt.Eval(x: extended): extended;
begin
  Result := Sqrt(x);
end;

function TSin.DefName: string;
begin
  Result := 'sin';
end;

function TSin.Eval(x: extended): extended;
begin
  Result := Sin(x);
end;

function TCos.DefName: string;
begin
  Result := 'cos';
end;

function TCos.Eval(x: extended): extended;
begin
  Result := Cos(x);
end;

function TTan.DefName: string;
begin
  Result := 'tan';
end;

function TTan.Eval(x: extended): extended;
begin
  Result := Tan(x);
end;

function TCsc.DefName: string;
begin
  Result := 'csc';
end;

function TCsc.Eval(x: extended): extended;
begin
  Result := Csc(x);
end;

function TSec.DefName: string;
begin
  Result := 'sec';
end;

function TSec.Eval(x: extended): extended;
begin
  Result := Sec(x);
end;

function TCot.DefName: string;
begin
  Result := 'cot';
end;

function TCot.Eval(x: extended): extended;
begin
  Result := Cot(x);
end;

function TArcSin.DefName: string;
begin
  Result := 'arcsin';
end;

function TArcSin.Eval(x: extended): extended;
begin
  Result := ArcSin(x);
end;

function TArcCos.DefName: string;
begin
  Result := 'arccos';
end;

function TArcCos.Eval(x: extended): extended;
begin
  Result := ArcCos(x);
end;

function TArcTan.DefName: string;
begin
  Result := 'arctan';
end;

function TArcTan.Eval(x: extended): extended;
begin
  Result := ArcTan(x);
end;

function TArcCsc.DefName: string;
begin
  Result := 'arccsc';
end;

function TArcCsc.Eval(x: extended): extended;
begin
  Result := ArcCsc(x);
end;

function TArcSec.DefName: string;
begin
  Result := 'arcsec';
end;

function TArcSec.Eval(x: extended): extended;
begin
  Result := ArcSec(x);
end;

function TArcCot.DefName: string;
begin
  Result := 'arccot';
end;

function TArcCot.Eval(x: extended): extended;
begin
  Result := ArcCot(x);
  if (Result > Pion2) or (Result < -Pion2)
  then Result := NaN;
end;

function TLn.DefName: string;
begin
  Result := 'ln';
end;

function TLn.Eval(x: extended): extended;
begin
  Result := Ln(x);
  if isNaN(Result) then
  begin
    case Sign(Result) of
   -1:Result := NegInfinity;
    0:Result := 0;
    1:Result := Infinity;
    end;
  end;
end;

function TExp.DefName: string;
begin
  Result := 'exp';
end;

function TExp.Eval(x: extended): extended;
begin
  Result := Exp(x);
end;

function TExp1.DefName: string;
begin
  Result := 'e^';
end;

function TExp1.Eval(x: extended): extended;
begin
  Result := Exp(x);
end;

function TLog10.DefName: string;
begin
  Result := 'log';
end;

function TLog10.Eval(x: extended): extended;
begin
  Result := Log10(x);
  if isNaN(Result) then
  begin
    case Sign(Result) of
   -1:Result := NegInfinity;
    0:Result := 0;
    1:Result := Infinity;
    end;
  end;
end;

function TLog2.DefName: string;
begin
  Result := 'log2';
end;

function TLog2.Eval(x: extended): extended;
begin
  Result := Log2(x);
  if isNaN(Result) then
  begin
    case Sign(Result) of
   -1:Result := NegInfinity;
    0:Result := 0;
    1:Result := Infinity;
    end;
  end;
end;

function TSinh.DefName: string;
begin
  Result := 'sinh';
end;

function TSinh.Eval(x: extended): extended;
begin
  Result := Sinh(x);
end;

function TCosh.DefName: string;
begin
  Result := 'cosh';
end;

function TCosh.Eval(x: extended): extended;
begin
  Result := Cosh(x);
end;

function TTanh.DefName: string;
begin
  Result := 'tanh';
end;

function TTanh.Eval(x: extended): extended;
begin
  Result := Tanh(x);
end;

function TCsch.DefName: string;
begin
  Result := 'csch';
end;

function TCsch.Eval(x: extended): extended;
begin
  Result := Csch(x);
end;

function TSech.DefName: string;
begin
  Result := 'sech';
end;

function TSech.Eval(x: extended): extended;
begin
  Result := Sech(x);
end;

function TCoth.DefName: string;
begin
  Result := 'coth';
end;

function TCoth.Eval(x: extended): extended;
begin
  Result := Coth(x);
end;

function TArcSinh.DefName: string;
begin
  Result := 'arcsinh';
end;

function TArcSinh.Eval(x: extended): extended;
begin
  Result := ArcSinh(x);
end;

function TArcCosh.DefName: string;
begin
  Result := 'arccosh';
end;

function TArcCosh.Eval(x: extended): extended;
begin
  Result := ArcCosh(x);
end;

function TArcTanh.DefName: string;
begin
  Result := 'arctanh';
end;

function TArcTanh.Eval(x: extended): extended;
begin
  Result := ArcTanh(x)
end;

function TArcCsch.DefName: string;
begin
  Result := 'arccsch';
end;

function TArcCsch.Eval(x: extended): extended;
begin
  if x = 0 then Result := Infinity else Result := ArcCsch(x);
{ it would seem that Delphi 7 personal calculates ArcCsch incorrectly }
end;

function TArcSech.DefName: string;
begin
  Result := 'arcsech';
end;

function TArcSech.Eval(x: extended): extended;
begin
  if x <= 0 then Result := Infinity else Result := ArcSech(x);
end;

function TArcCoth.DefName: string;
begin
  Result := 'arccoth';
end;

function TArcCoth.Eval(x: extended): extended;
begin
  if (x >= -1) and (x < 0) then Result := NegInfinity else
  if (x > 0) and (x <= 1) then Result := Infinity else
  if x = 0 then Result := NaN else Result := ArcCoth(x);
end;
{ TOperator Class }

function ScanText(const s: string): string;
  function DropSpaces_Commas(const s: string): string;
  var
    i: integer;

  begin
    Result := '';
    for i := 1 to Length(s) do
    if (s[i] <> ' ') and (s[i] <> ',') then Result := Result + s[i];
  end;   { DropSpaces_Commas }

var
  i, j: integer;
  c0, c1, c2: char;
  cc, ccc, isStr: string;
  nostar: Boolean;
  isExp: Boolean;
  isLog: Boolean;
  isPwr: Boolean;
  t: string;

begin  { ScanText }
  t := DropSpaces_Commas(s);
  i := 1;
  j := 1;
  Result := t;
  while i < Length(t) do
  begin
    c0 := UpCase(t[i]);
    c1 := UpCase(t[i +1]);
    if i < Length(t) - 1 then c2 := UpCase(t[i +2]) else c2 := #0;

    cc  := c0+c1;
    ccc := c0+c1+c2;

    isExp := ccc = 'XP(';
    isStr := '';
    isLog := false;

    if (i > 3) and ((cc = '0(') or (cc = '2(')) then
    begin
      if cc = '0('
      then isStr := UpperCase(Copy(t, i -4, 3))    { Log10 }
      else isStr := UpperCase(Copy(t, i -3, 3));   { Log2 }
      isLog := isStr = 'LOG';
    end;

    isPwr := CharInSet(c0, ['+', '-', '0'..'9']) and (UpCase(c1) = 'E') and
             CharInSet(c2, ['+', '-', '0'..'9']);
    nostar := isExp or isLog or isPwr;

    if not nostar and
      CharInSet(c0, ['X', 'Y', 'I', '0'..'9', ')']) and
      CharInSet(c1, ['A'..'C', 'E', 'L', 'P', 'S', 'T', 'X', 'Y', '(']) then
    begin
      Insert('*', Result, i + j);
      Inc(j);
    end;
    Inc(i);
  end;
end;   { ScanText }

function ParseAndEvaluate(const aText: string; var e: byte): extended;
var
  aParser: TfxyParser;

begin
  aParser := TfxyParser.Create(0, 0);
  with aParser do
  begin
    Calculus.Free;
    ErrorByte := 0;
    ViewForm.StatusBar.Panels[4].Text := '';
    Calculus := Compile(AnsiLowerCase(aText), ErrorByte);
    e := ErrorByte;
    if ErrorByte > 0 then
    begin
      with ViewForm.StatusBar.Panels[4] do
      case ErrorByte of
      1:Text := 'Check Brackets for "'+ aText+'"';
      2:Text := 'Unable to Parse "'+aText+'"';
      end;
      Result := 0;
    end
    else Result := Calculus.Eval;
    Calculus.Free;
    Calculus := nil;
    Free;
  end;
end;

function ParseEvaluateFxy(const aVarX, aVarY: extended;
                          const aText: string; var e: byte): extended;
var
  aParser: TfxyParser;

begin
  aParser := TfxyParser.Create(0, 0);
  with aParser do
  begin
    Calculus.Free;
    ErrorByte := 0;
    ViewForm.StatusBar.Panels[4].Text := '';
    Calculus := Compile(AnsiLowerCase(aText), ErrorByte);
    VarX.Value := aVarX;
    VarY.Value := aVarY;
    e := ErrorByte;
    if ErrorByte > 0 then
    begin
      with ViewForm.StatusBar.Panels[4] do
      case ErrorByte of
      1:Text := 'Check Brackets for "'+ aText+'"';
      2:Text := 'Unable to Parse "'+aText+'"';
      end;
      Result := 0;
    end
    else Result := Calculus.Eval;
    Calculus.Free;
    Calculus := nil;
    Free;
  end;
end;

Initialization
{ Avoids arithmetic exceptions in the above code }
SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
                  exOverflow, exUnderflow, exPrecision]);

end.

