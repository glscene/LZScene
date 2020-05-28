//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  Language created to localize your application.
  In Delphi, the text is encoded using Ansi cp1251 and can not be encoded \ decoding.
  In Lazarus has the ability to upload text from any encoding.

   History :  
   01/12/14 - PW - Fixed usage of IFDEF GLS_LOGGING in LoadLanguageFromFile
   04/11/10 - DaStr - Added Delphi5 and Delphi6 compatibility
   20/04/10 - Yar - Added to GLScene
  (Created by Rustam Asmandiarov aka Predator)
   
}
unit GLSLanguage;

interface

{$I GLScene.inc}
{$H+} // use AnsiString instead of ShortString as String-type (default in Delphi)

uses
  Classes, IniFiles, SysUtils;

type


  TConvertEncodingList = (celAutomatic, celISO_8859_1ToUTF8,
    celISO_8859_2ToUTF8, celCP1250ToUTF8, celCP1251ToUTF8, celCP1252ToUTF8,
    celCP1253ToUTF8, celCP1254ToUTF8, celCP1255ToUTF8, celCP1256ToUTF8,
    celCP1257ToUTF8, celCP1258ToUTF8, celCP437ToUTF8, celCP850ToUTF8,
    celCP866ToUTF8, celCP874ToUTF8, celKOI8ToUTF8, celUCS2LEToUTF8,celUCS2BEToUTF8);

  TGLLanguageEntry = record
    ID: AnsiString; // **< identifier (ASCII)
    Text: UTF8String; // **< translation (UTF-8)
  end;

  TGLLanguageEntryArray = array of TGLLanguageEntry;

  { TGLLanguage }
  { *
    *   Class TGLLanguage used interpretation to download and translation, as in the final product is no need for text  processing.
    * }
  TGLLanguage = class
  private
    FCurrentLanguageFile: UTF8String;
    Entry: TGLLanguageEntryArray; // **< Entrys of Chosen Language

    FConvertEncodingList: TConvertEncodingList;

    function EncodeToUTF8(aValue: AnsiString): UTF8String;
  public
    function FindID(const ID: AnsiString): integer;
    function Translate(const ID: AnsiString): UTF8String;
    procedure LoadLanguageFromFile(const Language: UTF8String);
    property CurrentLanguageFile: UTF8String read FCurrentLanguageFile;

    property ConvertEncodingList: TConvertEncodingList read FConvertEncodingList
      write FConvertEncodingList default celAutomatic;

  end;

  { *
    *   Advanced class is designed for loading and processing, will be useful for the editors of language.
    * }
  TGLLanguageExt = class(TGLLanguage)
  private
    function GetEntry(Index: integer): TGLLanguageEntry;
    procedure SetEntry(Index: integer; aValue: TGLLanguageEntry);
    function GetCount: integer;
  public
    procedure AddConst(const ID: AnsiString; const Text: UTF8String);
    procedure AddConsts(aValues: TStrings);
    procedure ChangeConst(const ID: AnsiString; const Text: UTF8String);
    property Items[Index: integer]: TGLLanguageEntry read GetEntry write SetEntry;
    property Count: integer read GetCount;
    procedure SaveLanguageFromFile(const Language: UTF8String); overload;
    procedure SaveLanguageFromFile; overload;
  end;

  { TGLSLanguage }

  { : Abstract class for control Language. }
  TGLSLanguage = class(TComponent)
  private
    FLanguage: TGLLanguageExt;
    FLanguageList: TStrings;

    function GetEncodingList: TConvertEncodingList;
    procedure SetEncodingList(aList: TConvertEncodingList);

    procedure SetLanguage(aValue: TGLLanguageExt);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadLanguageFromFile(const Language: UTF8String);
    procedure SaveLanguageFromFile(const Language: UTF8String); overload;
    procedure SaveLanguageFromFile; overload;
    function Translate(const ID: AnsiString): UTF8String;
    property Language: TGLLanguageExt read FLanguage write SetLanguage;

  Published
    property ConvertEncodingList: TConvertEncodingList read GetEncodingList
      write SetEncodingList;

  end;

implementation

uses

  LazFileUtils, LConvEncoding,
  GLCrossPlatform, GLSLog;

{ TGLLanguage }

{ **
  * Load the specified LanguageFile
  * }
procedure TGLLanguage.LoadLanguageFromFile(const Language: UTF8String);
var
  IniFile: TMemIniFile;
  E: integer; // entry
  S: TStringList;
  I: integer;
begin
  If Language = '' then
    Exit;
  if not FileExists(string(Language)) then
  begin
{$IFDEF GLS_LOGGING}
    GLSLogger.LogFatalError(ExtractFileName(string(Language)) +
      ' Languagefile missing!');
{$ENDIF}
    Exit;
  end;
  SetLength(Entry, 0);
  FCurrentLanguageFile := Language;
  IniFile := TMemIniFile.Create(string(Language));
  S := TStringList.Create;

  IniFile.ReadSectionValues('Text', S);

  // Problem Solving with symbols wrap (#13#10)
  I := 0;
  for E := 0 to S.Count - 1 do
  begin
    If S.Names[E] = '' then
    begin
      S.Strings[I] := S.Strings[I] + #13#10 + GetValueFromStringsIndex(S, E);
    end
    else
      I := E;
  end;

  SetLength(Entry, S.Count);
  for E := 0 to high(Entry) do
    If S.Names[E] <> '' then
    begin
      Entry[E].ID := EncodeToUTF8(S.Names[E]);
      Entry[E].Text := EncodeToUTF8(GetValueFromStringsIndex(S, E));
    end;
  S.Free;
  IniFile.Free;
end;

{ **
  * Find the index of ID an array of language entry.
  * @returns the index on success, -1 otherwise.
  * }
function TGLLanguage.FindID(const ID: AnsiString): integer;
var
  Index: integer;
begin
  for Index := 0 to High(Entry) do
  begin
    if UpperCase(string(ID)) = UpperCase(string(Entry[Index].ID)) then
    begin
      Result := Index;
      Exit;
    end;
  end;
  Result := -1;
end;

{ **
  * Translate the Text.
  * If Text is an ID, text will be translated according to the current language
  * setting. If Text is not a known ID, it will be returned as is.
  * @param Text either an ID or an UTF-8 encoded string
  * }
function TGLLanguage.Translate(const ID: AnsiString): UTF8String;
var
  EntryIndex: integer;
begin
  // fallback result in case Text is not a known ID
  Result := ID;

  // Check if ID exists

  EntryIndex := FindID(ID);
  if (EntryIndex >= 0) then
  begin
    Result := Entry[EntryIndex].Text;
    Exit;
  end;
end;

function TGLLanguage.EncodeToUTF8(aValue: AnsiString): UTF8String;
begin
{$IFDEF FPC}
  case ConvertEncodingList of
    celAutomatic:
      Result := ConvertEncoding(aValue, GuessEncoding(aValue), EncodingUTF8);
    celISO_8859_1ToUTF8:
      Result := ISO_8859_1ToUTF8(aValue);
    celISO_8859_2ToUTF8:
      Result := ISO_8859_2ToUTF8(aValue);
    celCP1250ToUTF8:
      Result := CP1250ToUTF8(aValue);
    celCP1251ToUTF8:
      Result := CP1251ToUTF8(aValue);
    celCP1252ToUTF8:
      Result := CP1252ToUTF8(aValue);
    celCP1253ToUTF8:
      Result := CP1253ToUTF8(aValue);
    celCP1254ToUTF8:
      Result := CP1254ToUTF8(aValue);
    celCP1255ToUTF8:
      Result := CP1255ToUTF8(aValue);
    celCP1256ToUTF8:
      Result := CP1256ToUTF8(aValue);
    celCP1257ToUTF8:
      Result := CP1257ToUTF8(aValue);
    celCP1258ToUTF8:
      Result := CP1258ToUTF8(aValue);
    celCP437ToUTF8:
      Result := CP437ToUTF8(aValue);
    celCP850ToUTF8:
      Result := CP850ToUTF8(aValue);
    celCP866ToUTF8:
      Result := CP866ToUTF8(aValue);
    celCP874ToUTF8:
      Result := CP874ToUTF8(aValue);
    celKOI8ToUTF8:
      Result := KOI8ToUTF8(aValue);
    celUCS2LEToUTF8:
      Result := UCS2LEToUTF8(aValue);
    celUCS2BEToUTF8:
      Result := UCS2BEToUTF8(aValue);
  end;
{$ELSE}
  Result := aValue;
{$ENDIF}
end;

{ TGLLanguageExt }

{ *
  * Add a Constant ID that will be Translated but not Loaded from the LanguageFile
  * }
procedure TGLLanguageExt.AddConst(const ID: AnsiString; const Text: UTF8String);
begin
  SetLength(Entry, Length(Entry) + 1);
  Entry[high(Entry)].ID := ID;
  Entry[high(Entry)].Text := Text;
end;

procedure TGLLanguageExt.AddConsts(aValues: TStrings);
var
  I: integer;
begin
  if aValues <> nil then
    for I := 0 to aValues.Count - 1 do
      If aValues.Names[I] <> '' then
        AddConst(EncodeToUTF8(aValues.Names[I]),
          EncodeToUTF8(GetValueFromStringsIndex(aValues, I)));
end;

{ *
  * Change a Constant Value by ID
  * }
procedure TGLLanguageExt.ChangeConst(const ID: AnsiString;
  const Text: UTF8String);
var
  I: integer;
begin
  for I := 0 to high(Entry) do
  begin
    if Entry[I].ID = ID then
    begin
      Entry[I].Text := Text;
      Break;
    end;
  end;
end;

function TGLLanguageExt.GetEntry(Index: integer): TGLLanguageEntry;
begin
  Result := Entry[Index];
end;

procedure TGLLanguageExt.SetEntry(Index: integer; aValue: TGLLanguageEntry);
begin
  Entry[Index] := aValue;
end;

function TGLLanguageExt.GetCount: integer;
begin
  Result := high(Entry) + 1;
end;

{ *
  * Save Update Language File
  * }
procedure TGLLanguageExt.SaveLanguageFromFile(const Language: UTF8String);
var
  IniFile: TMemIniFile;
  E: integer; // entry
begin
  if Language = '' then
    Exit;

  IniFile := TMemIniFile.Create(string(Language));

  for E := 0 to Count - 1 do
  begin
    IniFile.WriteString('Text', string(Items[E].ID), string(Items[E].Text));
  end;
  IniFile.UpdateFile;
  IniFile.Free;
end;

procedure TGLLanguageExt.SaveLanguageFromFile;
begin
  SaveLanguageFromFile(CurrentLanguageFile);
end;

{ TGLSLanguage }

constructor TGLSLanguage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLanguage := TGLLanguageExt.Create;
  FLanguageList := TStringList.Create;
end;

destructor TGLSLanguage.Destroy;
begin
  FLanguage.Free;
  FLanguageList.Free;
  inherited Destroy;
end;

procedure TGLSLanguage.LoadLanguageFromFile(const Language: UTF8String);
begin
  FLanguage.LoadLanguageFromFile(Language);
end;

{$IFDEF FPC}

function TGLSLanguage.GetEncodingList: TConvertEncodingList;
begin
  Result := FLanguage.ConvertEncodingList;
end;

procedure TGLSLanguage.SetEncodingList(aList: TConvertEncodingList);
begin
  FLanguage.ConvertEncodingList := aList;
end;
{$ENDIF}

procedure TGLSLanguage.SetLanguage(aValue: TGLLanguageExt);
begin
  if aValue <> nil then
    FLanguage := aValue;
end;

procedure TGLSLanguage.SaveLanguageFromFile(const Language: UTF8String);
begin
  if Language = '' then
    Exit;

  FLanguage.SaveLanguageFromFile(Language);
end;

procedure TGLSLanguage.SaveLanguageFromFile;
begin
  FLanguage.SaveLanguageFromFile;
end;

function TGLSLanguage.Translate(const ID: AnsiString): UTF8String;
begin
  Result := FLanguage.Translate(ID);
end;

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
initialization

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------

RegisterClass(TGLSLanguage);

end.
