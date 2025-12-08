(*====< GLZStringUtils.pas >====================================================@br
  @created(2017-06-11)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(11/06/2017 : Creation  )
  )
--------------------------------------------------------------------------------@br

  @bold(Description :)@br
  Initialise un objet StrOps regroupant plusieur fonctions utiles à la manipulation de
  chaine de caractères

  ------------------------------------------------------------------------------@br

  Notes : @br
  ------------------------------------------------------------------------------@br

  Credits :

  ------------------------------------------------------------------------------@br
  LICENCE : MPL / GPL @br
  @br
 *==============================================================================*)
unit GLZStringUtils;

interface

{.$i ..\glzscene_options.inc}

uses
  Classes, SysUtils, Dialogs;

Const
  {$IFDEF MSWINDOWS}
  PathSeparator='\';
  {$ELSE}
  PathSeparator='/';
  {$ENDIF}
  WhiteSpaces = [#8, #9, #10, #13, #32];
{const
  gcsAll           = [#0..#255];
  gcsAZHi          = ['A'..'Z'];
  gcsAZLo          = ['a'..'z'];
  gcsAZAlpha       = ['A'..'Z','a'..'z'];
  gcsDigit         = ['0'..'9'];
  gcsInt           = ['0'..'9','-','+'];
  gcsNumeric       = ['0'..'9',',','.','-','+'];
  gcsFloat         = ['0'..'9',',','.','-','+','E','e'];
  gcsControl       = [#0..#31];
  gcCRSet          = [#13,#10];
  gcCR             = #13#10;
  gcBaseBinary     = '01';
  gcBaseHex        = '0123456789ABCDEF';
  gcWordDelims     = [#1..#47,#58..#64,#91..#96,#123..#126]-['`'];
  gcBooleanTrueSet   : TCharSet = ['1','T','t','Y','y','J','j']; }

Type
  TStrReplaceOptions = set of (roIgnoreCase, roWholeWords, roReplaceOnce, roPreserveCase, roRemoveDelim); //Delphi's is TReplaceFlags

Type
  TGLZStringHelper = class
  Public
    //Trim - Remove characters depending on the position in a string
    function  Trim(const s : String) : String; overload;
    function  TrimLeft(const s : String) : String;
    function  TrimRight(const s : String) : String;
    //Informational functions-----------------------------------------------------
    function  IsEmpty(const s : String) : Boolean;
    //Case------------------------------------------------------------------------
    function  UpCase(const s : String) : String; //overload;
    function  LoCase(const s : String) : String; //overload;
    //Delete----------------------------------------------------------------------
    //function  Del(const SubStr : String; const s : String): String;
    //Manipulation----------------------------------------------------------------
    //function  Cut(var s : String; Index, Count : Integer) : String;
    //function  Ins(const SubStr : String; const s : String; Index : Integer = 1) : String;
    //function  SetAt(const s : String; const c : Char; Index : Integer) : String;
    //function  EnsurePrefix(const SubStr : String; const s : String) : String;
    //function  EnsureSuffix(const SubStr : String; const s : String) : String;
	  //function  EnsureNoPrefix(const SubStr : String; const s : String) : String;
    //function  EnsureNoSuffix(const SubStr : String; const s : String) : String;
    function  PadCenter(const s: String; Len: Integer; PadChar : Char = ' '): String;
    function  PadRight(const s : String; PadLen : SmallInt; c : Char = ' ') : String;
    function  PadLeft(const s : String; PadLen : Word; c : Char = ' ') : String;
    //Replacement-----------------------------------------------------------------
    {function  ReplaceChars(const s : String; const OldChars : TCharSet; NewChar : Char) : String;
    function  Replace(OldChar, NewChar : Char; const InStr : String; const IgnoreCase : Boolean = False) : string; overload;
    function  Replace(
              const FindStr      : String;
              const ReplaceStr   : String;
              const InStr        : String;
              const IgnoreCase   : Boolean = False;
              const WholeWord    : Boolean = False;
              const PreserveCase : Boolean = False;
              const Delims       : TCharSet = gcWordDelims) : String; overload; }
    //Positional------------------------------------------------------------------
    function  Pos(const SubStr, s : String;StartPos :integer = 1) : Integer; //overload;
    function  PosBetween(const SubStr1, SubStr2 : String; const s : String; var StartPos, EndPos : Integer):Boolean;
    //function  Pos(c : Char; const s : String; StartPos : Integer = 1) : Integer; overload;
    //Return part of a string-----------------------------------------------------
    function  After(const SubStr : String; const s : String; Position : Integer = 1) : String;
    function  Before(const SubStr : String; const s : String; Position : Integer = 1) : String;
    function  Between(const SubStr1, SubStr2 : String; const s : String) : String;
    function  Mid(const SubStr1, SubStr2 : String; const s : String; Position : Integer = 1) : String;
    //Copy functions--------------------------------------------------------------
    function Copy(const aSourceString : string; aStart, aLength : Integer) : string;
    function CopyPos(const s: String; StartPos, EndPos: Integer): String;
    function LeftOf(const s : String; Position : Integer) : String;
    function RightOf(const s : String; Position : Integer) : String;
    //Numeric To String conversions-----------------------------------------------
    function ToStr(Value : Boolean) : String; overload;
    function ToStr(Value : Integer) : String; overload;
    function ToStr(Value : Extended) : String; overload;
{    function  ToStr(Value : Integer) : String; overload;

    function  ToStr(Value : Byte; PadLen : SmallInt = 0; PadChar : Char = '0') : String; overload;
    function  ToStr(Value : SmallInt; PadLen : SmallInt = 0; PadChar : Char = '0') : String; overload;
    function  ToStr(Value : Word; PadLen : SmallInt = 0; PadChar : Char = '0') : String; overload;
    function  ToStr(Value : Integer; PadLen : SmallInt; PadChar : Char = '0') : String; overload;
    function  ToStr(Value : Cardinal; PadLen : SmallInt = 0; PadChar : Char = '0') : String; overload;
    function  ToStr(Value : Int64; PadLen : SmallInt = 0; PadChar : Char = '0') : String; overload;
    function  ToStr(Value : Pointer) : String; overload;
    function  ToStr(Value : Double; PadLen : SmallInt = 0; PadChar : Char = '0') : String; overload;
    function  ToStr(Value : Extended; PadLen : SmallInt = 0; PadChar : Char = '0') : String; overload;
    function  ToStr(Value : TCurrency; PadLen : SmallInt = 0; PadChar : Char = '0') : String; overload; }
	//String to Numeric conversions-----------------------------------------------
{	function ToVal(const s : String) : Byte; overload;
	function ToVal(const s : String) : SmallInt; overload;
	function ToVal(const s : String) : Word; overload;
	function ToVal(const s : String) : Integer; overload;
	function ToVal(const s : String) : Cardinal; overload;
	function ToVal(const s : String) : Int64; overload;
	function ToVal(const s : String) : Double; overload;
	function ToVal(const s : String) : Extended; overload;
	function ToVal(const s : String) : TCurrency; overload; }
    //Miscellaneous-------------------------------------------------------------

    procedure SkipWhiteSpace(var Line: string);
    function ReadString(var Line: string): string;
    function ReadInt(var Line: string): Integer;

    function RepeatChar(c : Char; Count : Integer) : String;
    function RepeatStr(const s : String; Count : Integer) : String;
    function Surround(S: string; chs: string): string;  overload;
    function Surround(S: string; chsL, chsR: string): string; //overload;
    function Implode(lst:TStringList;sep : string =';'):string;
    function Explode(ch : string;sep: string = ';'):TStringList;
    function Find(const Substr, S: string; const Index: Integer =1): Integer;

//    function TokenAt(const S: string; Seperator: Char; At: Integer): string;
  end;

Var StrOps : TGLZStringHelper;

implementation

//Uses StrUtils;

{//////////////////////////////////////////////////////////////////////////////}
{                      GLOBAL FUNCTIONS, VARS                                  }
{//////////////////////////////////////////////////////////////////////////////}

//CaseArray's for fast conversion - see FillANSIArrays
{var
  gaANSIUpperArray   : array[Char] of Char;
  gaANSILowerArray   : array[Char] of Char;
  gaANSICharType     : array[Char] of TCharTypeFlags; }

{-------------------------------------------------------------------------------
*Copy - Same as the standard Copy function except Count is by default 2GB.
 Example: Copy('ABCDEF', 2) > 'BCDEF'
 Example: Copy('ABCDEF', 2, 3) > 'BCD'
-------------------------------------------------------------------------------}
function TGLZStringHelper.Copy(const aSourceString : string; aStart, aLength : Integer) : string;
var
  L                           : Integer;
begin
  L := Length(aSourceString);
  if L=0 then Exit;
  if (aStart < 1) or (aLength < 1) then Exit;

  if aStart + (aLength) > L then aLength := L - (aStart-2);

  if (aStart <1) then exit;

  SetLength(Result,aLength);
  //FastCharMove(aSourceString[aStart], Result[1], aLength);
  Move(aSourceString[aStart], Result[1], aLength);
end;

function TGLZStringHelper.CopyPos(const s: String; StartPos, EndPos: Integer): String;
var Len:integer;
begin

	if EndPos<StartPos then
	begin
		Len:=StartPos+EndPos;
	end
	else
	begin
		Len:=EndPos-StartPos;
	end;
	result:=Copy(s,StartPos, Len);
end;

{-------------------------------------------------------------------------------
*Trim - Strips leading/trailing spaces and control characters.
  Same as standard Trim function but with optional DoUpcase.
Example: Trim('  hello   world  ') > 'hello   world'
-------------------------------------------------------------------------------}
function TGLZStringHelper.Trim(const s : String) : String;
var i, l: Integer;
begin
  l := Length(s);
  i := 1;
  while (i <= l) and (s[i] <= ' ') do Inc(i);
  if i > l then Result := '' else
  begin
    while s[l] <= ' ' do System.Dec(l);
    Result := Copy(s, i, l - i + 1);
  end;
End;

{-------------------------------------------------------------------------------
*TrimLeft - Strips leading spaces and control characters from a string.
  Same as standard TrimLeft function but with optional DoUpcase.
Example: TrimLeft('  hello   world  ') > 'hello   world  '
-------------------------------------------------------------------------------}
function TGLZStringHelper.TrimLeft(const s : String) : String;
var i, l : Integer;
begin
  l := Length(s);
  i := 1;
  while (i <= l) and (s[i] <= ' ') do Inc(i);
  Result := Copy(s, i, Maxint);
end;

{-------------------------------------------------------------------------------
*TrimRight - Strips trailing spaces and control characters from a string.
  Same as standard TrimRight function but with optional DoUpcase.
Example: TrimRight('  hello   world  ') > '  hello   world'
-------------------------------------------------------------------------------}
function TGLZStringHelper.TrimRight(const s : String) : String;
var i : Integer;
begin
  i := Length(s);
  while (i > 0) and (S[i] <= ' ') do Dec(i);
  Result := Copy(s, 1, i);
end;

{-------------------------------------------------------------------------------
*IsEmpty - Returns true if a string has no characters above ord(' ').
 If basically means that it returns true if the string is empty or only contains
 control characters or spaces.
 Much faster than "if Trim(s)='' then..."
 Example:
   IsEmpty('   '+#9) > True
   IsEmpty(' a  ') > False
-------------------------------------------------------------------------------}
function TGLZStringHelper.IsEmpty(const s : String) : Boolean;
var i : Integer;
begin
  Result := False;
  for i := 1 to Length(s) do if s[i]>' ' then Exit;
  Result := True;
end;

{-------------------------------------------------------------------------------
*UpCase - AnsiUpperCase converts all characters in the given string to uppercase.
 The conversion uses the current Windows locale.
 Faster than SysUtils.AnsiUppercase but does not support MBCS.
 Example: UpCase('polé') > 'POLÉ'
Note - should be 3-4 times faster than SysUtils.AnsiUppercase and 20% faster than SysUtils.UpperCase
-------------------------------------------------------------------------------}
function TGLZStringHelper.UpCase(const s : String) : String;
Begin
  result:=UpperCase(S);
end;

{-------------------------------------------------------------------------------
*LoCase - AnsiLowerCase converts all characters in the given string to lowercase.
 The conversion uses the current Windows locale.
 Faster than SysUtils.AnsiLowerCase but does not support MBCS.
 Example: LoCase('POLÉ') > 'polé'
-------------------------------------------------------------------------------}          
function TGLZStringHelper.LoCase(const s : String) : String;
Begin
  result:=LowerCase(S);
end;

{-------------------------------------------------------------------------------
*Pos - Overloaded - Same as the standard Pos function except that:
  - if you use enable StrManEnableASM then it is faster - thanks to Peter Morris
     (see notes and credits at FastMemPos)
  - an optional starting position can be specified.
 Example: Pos('the','the man there') > 1
 Example: Pos('the','the man there',2) > 9
 Example: Pos('THE','the man there',2) > 0
-------------------------------------------------------------------------------}
function TGLZStringHelper.Pos(const SubStr, s : String; StartPos :integer = 1) : Integer;
var tmp:String;
    L:Integer;
begin
  if StartPos>1 then
  begin
    L:=Length(S)-StartPos;
    tmp:=Copy(S,StartPos, L);
  end
  else tmp:=S;

  Result := System.Pos(SubStr,tmp);
end;

function TGLZStringHelper.PosBetween(const SubStr1, SubStr2 : String; const s : String; var StartPos, EndPos : Integer):Boolean;
var REndPos, RStartPos :Integer;
Begin
  Result:=False;
  StartPos:=-1;
  EndPos:=-1;
  RStartPos:=Pos(Substr1,S);
  REndPos:=Pos(Substr2,S);
  if (RStartPos>0) And (REndPos>0) then
  begin
    result:=True;
    StartPos:=RStartPos;
    EndPos:=REndpos;
  end;
End;

{-------------------------------------------------------------------------------
*After
 Scans for SubStr in s - if found the characters after SubStr is returned else
 an empty string is returned.
 Examples:
   After('Funny','Those Funny People') > ' People';
   After('land','Your land is my land ok',1) > ' is my land ok'
   After('land','Your land is my land ok',7) > ' ok'
   After('not there','Your land is my land ok') > ''
-------------------------------------------------------------------------------}
function TGLZStringHelper.After(const SubStr : String; const s : String; Position : Integer = 1) : String;
var p,L,Start : Integer;
begin
  p := Pos(SubStr, s,Position);
  Result := '';
  if p>=0 then
  begin
    Start:=p+Length(SubStr);
    L:=Length(s)-(Start-1);
    Result := Copy(s, Start, L);
  end;
end;
{-------------------------------------------------------------------------------
*Before
 Scans for SubStr in s - if found the characters before SubStr is returned else
 the complete string is returned.
 Example: Before(' People','Those Funny People') > 'Those Funny';
 Example: Before('land','Your land is my land',1) > 'Your';
 Example: Before('land','Your land is my land',7) > 'Your land is my ';
 Example: Before('not there','Your land is my land') > 'Your land is my land'
-------------------------------------------------------------------------------}
function TGLZStringHelper.Before(const SubStr : String; const s : String; Position : Integer = 1) : String;
var p , L: Integer;
begin
  p := Pos(SubStr, s,Position);
  Result := '';
  if p>=0 then
  begin
    //Start:=p+Length(SubStr);
    L:=p-1;
    Result := Copy(s, position, L);
  end;
end;
{-------------------------------------------------------------------------------
*Between - Scans s for the start/end combination of SubStr1, SubStr2 and
 returns the text between them.
 If SubStr2 is empty SubStr2 will be regarded as identical to SubStr1.
 If SubStr2 is not found, then an empty string is returned (as opposed to function Mid).

Example:
  Between('<-','->','<-a->  <-b->') > 'a'
  Between('(',')','(a(b(c)))  (d(e))') > 'a(b(c))'
  Between('(',')','(a(b(c))  (d(e') > ''
  Between('\','\','c:\winnt\system\util.dll') > 'winnt'
  Between('\','\','c:\winnt') > ''
-------------------------------------------------------------------------------}
function TGLZStringHelper.Between(const SubStr1, SubStr2 : String; const s : String) : String;
var StartPos,EndPos : Integer;
begin
  StartPos:=0;
  EndPos:=0;
  if PosBetween(SubStr1, SubStr2, s, StartPos, EndPos) then
  begin
    StartPos:=StartPos+1;
    Result := Copy(s, StartPos, (EndPos-StartPos));
  end
  else Result := '';
end;
{-------------------------------------------------------------------------------
*Mid - Scans s and returns text after the first SubStr1 and before SubStr2 if found.
 If SubStr2 is empty SubStr2 will be regarded as identical to SubStr1.
 If SubStr2 is not found the complete string after SubStr1 is returned (as opposed to function Between).
 This function is the equivalent of a Before(After combination, but faster.
Example:
  Mid('(',')','(a) (b) (c)') > a
  Mid('(',')','(a(b)(c)  ((d)e)') > a(b
  Mid('(',')','(a(b(c)))') > a(b(c
  Mid('(',')','(a(b(c)') > a(b(c
  Mid('\','','c:\winnt\system\util.dll') > winnt
  Mid('\','','c:\winnt') > winnt
Note that function Between will return the first innermost text:
  Between('(',')','(a(b)(c)' > 'b'
-------------------------------------------------------------------------------}
function TGLZStringHelper.Mid(const SubStr1, SubStr2 : String; const s : String; Position : Integer = 1) : String;
var p1,p2 : Integer;
begin
  p1 := Pos(SubStr1, s, Position);
  if p1<=0
  then Result := '' else
  begin
    if SubStr2=''
    then p2 := Pos(SubStr1, s, p1+Length(SubStr1))
    else p2 := Pos(SubStr2, s, p1+Length(SubStr1));
    if p2<=0
      then Result := Copy(s, p1+Length(SubStr1), Length(s))
      else Result := CopyPos(s, p1+Length(SubStr1), p2-1);
  end;
end;
{-------------------------------------------------------------------------------
*LeftAt
 Simple function that returns all chars to the left from a specific (including) Position
 Example: LeftAt('hello',2) > 'he'
See also RightAt, Copy, First, Last
-------------------------------------------------------------------------------}
function TGLZStringHelper.LeftOf(const s : String; Position : Integer) : String;
begin
  Result := Copy(s, 1, Position);
end;

{-------------------------------------------------------------------------------
*RightAt
 Simple function that returns all chars to the right from a specific (including) Position
 Example: RightAt('hello',2) > 'ello'
See also LeftAt, Copy, First, Last
-------------------------------------------------------------------------------}
function TGLZStringHelper.RightOf(const s : String; Position : Integer) : String;
begin
  Result := Copy(s, Position, Length(s));
end;

{-------------------------------------------------------------------------------
*Pad
 Ads a character at the end of the string until the length is equal to PadLen.
 If PadLen is negative the character will be inserted to the left.
 Example: Pad('hello',7)  > 'hello  '
 Example: Pad('hello',7,' ')  > 'hello  '
 Example: Pad('hello',-7,'.') > '..hello'
 Example: Pad('hello',1,'.') > 'hello'
-------------------------------------------------------------------------------}
function TGLZStringHelper.PadRight(const s : String; PadLen : SmallInt; c : Char = ' ') : String;
var  i : Integer;
  More : Integer;
  Slen : Integer;
begin
  SLen := Length(s);
  More := Abs(PadLen) - Slen;
  if More>0 then
  begin
    if PadLen<0 then
    begin
      SetLength(Result, Abs(PadLen));
      System.Move(s[1], Result[More+1], Slen);
      for i := 1 to More do Result[i] := c;
    end else
    begin
      Result := s;
      SetLength(Result, Abs(PadLen));
      for i := SLen+1 to Slen+More do Result[i] := c;
    end;
  end else Result := s;
end;

{-------------------------------------------------------------------------------
*Center - Centers text in a string of length Len.
 Example: Center('ok',6) > '  ok  '
-------------------------------------------------------------------------------}
function TGLZStringHelper.PadCenter(const s: String; Len: Integer; PadChar : Char = ' '): String;
begin
  if Length(s) < Len then
  begin
    Result := RepeatChar(PadChar, (Len div 2) - (Length(s) div 2))+s;
    Result := Result + RepeatChar(PadChar, Len - Length(Result));
  end else Result := s;
end;
{-------------------------------------------------------------------------------
*PadLeft - Pads a character to the left while Length(s)<PadLen.
 PadLeft('hello',7)  > '  hello'
 PadLeft('hello',7,'.')  > '..hello'
Note that function Pad with a negative PadLen is functionally then same.
See also Pad
-------------------------------------------------------------------------------}
function TGLZStringHelper.PadLeft(const s : String; PadLen : Word; c : Char = ' ') : String;
begin
  Result := PadRight(s, -PadLen, c);
end;

{-------------------------------------------------------------------------------
*RepeatChar - Sets a string with a certain character and length.
 Example: RepeatChar('a',3) > 'aaa'
See also RepeatStr
-------------------------------------------------------------------------------}
function TGLZStringHelper.RepeatChar(c : Char; Count : Integer) : String;
begin
  SetLength(Result, Count);
  if Count>0
    then FillChar(Result[1], Count, c);
end;

{-------------------------------------------------------------------------------
*RepeatStr - Repeats a string count times.
 Example: RepeatStr('hello',3) > 'hellohellohello'
-------------------------------------------------------------------------------}
function TGLZStringHelper.RepeatStr(const s : String; Count : Integer) : String;
var  p : PChar;
  Slen : Integer;
begin
  SLen := Length(s);
  SetLength(Result, Count*SLen);
  p := Pointer(Result);
  while Count > 0 do
  begin
    Move(Pointer(s)^, p^, SLen);
    Inc(p, SLen);
    Dec(Count);
  end;
end;

function TGLZStringHelper.Find(const Substr, S: string; const Index: Integer=1): Integer;
var
  apos: Integer;
begin
  if (SubStr <> '') and (S <> '') then
  begin
    apos := Pos(Substr, Copy(S, Index, Length(S) - Index + 1));
    if apos = 0 then
      Result := 0
    else
      Result := Index + aPos - 1;
  end
  else
    Result := 0;
end;



{-------------------------------------------------------------------------------
*Str - Converts Boolean to a string.
 Example: Str(False) > 'False'
See also ToBool
-------------------------------------------------------------------------------}
function TGLZStringHelper.ToStr(Value : Boolean) : String;
begin
  if Value then Result := 'True' else Result := 'False';
end;
{-------------------------------------------------------------------------------
*Str - Converts an Integer to a string.
 Identical to standard function IntToStr, but slightly faster.
 Example: Str(12) > '12'
See also the overloaded Integer Str version with padding.
-------------------------------------------------------------------------------}
function TGLZStringHelper.ToStr(Value : Integer) : String;
begin
  System.Str(Value:0, Result);
end;

{-------------------------------------------------------------------------------
*Str - Converts an Integer to a string.
 Identical to standard function IntToStr, but slightly faster.
 Example: Str(12) > '12'
See also the overloaded Integer Str version with padding.
-------------------------------------------------------------------------------}
function TGLZStringHelper.ToStr(Value : Extended) : String;
begin
  result:=SysUtils.FloatToStr(value);
  (* StrToFloat
A new set of format settings which set a fixed decimal separator can be created with the following code:
// in your .lpr project file
uses
...
{$IFDEF UNIX}
clocale
{ required on Linux/Unix for formatsettings support. Should be one of the first (probably after cthreads?}
{$ENDIF}
and:
// in your code:
var
  FPointSeparator, FCommaSeparator: TFormatSettings;
begin
  // Format settings to convert a string to a float
  FPointSeparator := DefaultFormatSettings;
  FPointSeparator.DecimalSeparator := '.';
  FPointSeparator.ThousandSeparator := '#';// disable the thousand separator
  FCommaSeparator := DefaultFormatSettings;
  FCommaSeparator.DecimalSeparator := ',';
  FCommaSeparator.ThousandSeparator := '#';// disable the thousand separator
Later on you can use this format settings when calling StrToFloat, like this:
// This function works like StrToFloat, but simply tries two possible decimal separator
// This will avoid an exception when the string format doesn't match the locale
function AnSemantico.StringToFloat(AStr: string): Double;
begin
  if Pos('.', AStr) > 0 then Result := StrToFloat(AStr, FPointSeparator)
  else Result := StrToFloat(AStr, FCommaSeparator);
end; *)

end;

{-------------------------------------------------------------------------------
*Del - returns s with all occurences of SubStr removed.
 Example: Del('the','the moon is therefore the light') > ' moon is refore  light'
See also DelWord
--------------------------------------------------------------------------------}
{function TStringManager.Del(const SubStr : String; const s : String): String;
var StartPos : Integer;
begin
  Result := s;
  StartPos := 1;
  PReplace(SubStr, '', Result, StartPos, [], []);
end; }




{-------------------------------------------------------------------------------
*Ins - function version of standard insert procedure.
 Example: Ins('a','1234',2) > '1a234'
 Example: Ins('a','1234',200) > '1234a'
-------------------------------------------------------------------------------}
{function TStringManager.Ins(const SubStr : String; const s : String; Index : Integer = 1) : String;
begin
  Result := s;
  System.Insert(SubStr, Result, Index);
end; }
{-------------------------------------------------------------------------------
*SlashAdd - add's a slash (typically a file/path name) at the end if needed
 Example: SlashAdd('c:\windows') > 'c:\windows\'
 Example: SlashAdd('c:\windows\') > 'c:\windows\'
See also SlashDel
-------------------------------------------------------------------------------}
{function TStringManager.AddSlash(const s : String) : String;
begin
  if (Length(s) > 0) and (s[Length(s)] <> '\')
    then Result := s+'\'
    else Result := s;
end; }

{-------------------------------------------------------------------------------
*SlashDel- reMoves a slash (typically a file/path name) if at end.
 Example: SlashDel('c:\windows') > 'c:\windows'
 Example: SlashDel('c:\windows\') > 'c:\windows'
See also SlashAdd
-------------------------------------------------------------------------------}
{function TStringManager.RemoveSlash(const s : String) : String;
begin
  if (Length(s) > 0) and (s[Length(s)] = '\')
    then Result := System.Copy(s, 1, Pred(Length(s)))
    else Result := s;
end; }
//--------------------------------------------------------------------------------------
{function TokenAt(const S: string; Seperator: Char; At: Integer): string;
var
  J, I: Integer;
begin
  Result := '';
  J := 1;
  I := 0;
  while (I <= At) and (J <= Length(S)) do
  begin
    if (S[J] = Seperator) then
      Inc(I)
    else if (I = At) then
      Result := Result + S[J];
    Inc(J);
  end;
  Result := Trim(Result);
end; }

{Function SubString(Str: String ; Substr: String ; n: Integer): String;
var
  z: Integer;
begin
  Str := Str + Substr;
  For z := 1 to n do
  begin
    Str := copy(Str, pos(Substr, Str)+length(Substr), length(Str)-pos(Substr, Str)+Length(Substr));
  end;
  Result := Trim(Copy(Str, 1, pos(Substr, Str) -1));
end;  }

procedure TGLZStringHelper.SkipWhiteSpace(var Line: string);
begin
  while (Length(Line) > 0) and (Line[1] in WhiteSpaces) do
    Delete(Line, 1, 1);
end;

function TGLZStringHelper.ReadString(var Line: string): string;
begin
  Result := '';
  SkipWhiteSpace(Line);
  while (Length(Line) > 0) and not(Line[1] in WhiteSpaces) do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result)] := Line[1];
    Delete(Line, 1, 1);
  end;
end;

function TGLZStringHelper.ReadInt(var Line: string): Integer;
begin
  Result := StrToInt(ReadString(Line));
end;
function TGLZStringHelper.Surround(S: string; chs: string): string;
begin
  Result := chs + S + chs;
end;

function TGLZStringHelper.Surround(S: string; chsL, chsR: string): string;
begin
  Result := chsL + S + chsR;
end;


function TGLZStringHelper.Explode(ch : string;sep: string = ';'):TStringList;
var
 p : integer;
begin
  p := pos(sep,ch);
  explode := TStringList.Create;
  while p > 0 do
  begin
    explode.Add(copy(ch,1,p-1));
    //showMessage(copy(ch,1,p-1));
    if p <= length(ch) then ch := copy(ch,p+ length(sep),length(ch));
    p := pos(sep,ch);
  end;
  explode.Add(ch);
end;

function TGLZStringHelper.Implode(lst:TStringList;sep : string =';'):string;
var
  i : integer;
  s : string;
begin
 i:= 0;
 while i < lst.Count - 1 do begin
   s := s + lst[i] + sep;
   i := i + 1;
 end;
 if i < lst.Count then s := s + lst[i]; //Ne mets pas de séparateur sur le dernier élément
 result := s;
end;

(* StrToFloat
A new set of format settings which set a fixed decimal separator can be created with the following code:
// in your .lpr project file
uses
...
{$IFDEF UNIX}
clocale
{ required on Linux/Unix for formatsettings support. Should be one of the first (probably after cthreads?}
{$ENDIF}
and:
// in your code:
var
  FPointSeparator, FCommaSeparator: TFormatSettings;
begin
  // Format settings to convert a string to a float
  FPointSeparator := DefaultFormatSettings;
  FPointSeparator.DecimalSeparator := '.';
  FPointSeparator.ThousandSeparator := '#';// disable the thousand separator
  FCommaSeparator := DefaultFormatSettings;
  FCommaSeparator.DecimalSeparator := ',';
  FCommaSeparator.ThousandSeparator := '#';// disable the thousand separator
Later on you can use this format settings when calling StrToFloat, like this:
// This function works like StrToFloat, but simply tries two possible decimal separator
// This will avoid an exception when the string format doesn't match the locale
function AnSemantico.StringToFloat(AStr: string): Double;
begin
  if Pos('.', AStr) > 0 then Result := StrToFloat(AStr, FPointSeparator)
  else Result := StrToFloat(AStr, FCommaSeparator);
end; *)

//==============================================================================
{//////////////////////////////////////////////////////////////////////////////}
{                         INITIALIZATION / FINALIZATION                        }
{//////////////////////////////////////////////////////////////////////////////}


Initialization
 // FillANSIArrays;
  StrOps:=TGLZStringHelper.Create;
Finalization
  StrOps.Free;
//==============================================================================
end.
