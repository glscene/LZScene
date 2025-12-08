unit uMainForm;

{$mode objfpc}{$H+}
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, ComCtrls, GLVectorTypes, GLVectorGeometry, GLGeomObjects,
  GLObjects, GLScene, GLLCLViewer, GLMaterial, GLCadencer, GLColor,
  GLBitmapFont, GLWindowsFont, GLHUDObjects, GLGraph, uNavCube, GLZArrayClasses,
  Types;


// Simple MOL2 Tag Format description
Const
  MOL2_MOLECULE_HEADER = '@<TRIPOS>MOLECULE';
  MOL2_ATOM_HEADER     = '@<TRIPOS>ATOM';
  MOL2_BOND_HEADER     = '@<TRIPOS>BOND';
  MOL2_SUBSTR_HEADER   = '@<TRIPOS>SUBSTRUCTURE';

Const
  cAtomBondingTypeStr : array[1..4] of String =
    ('diatomic',
     'atomic',
     'metallic',
     'covalent network');

  cAtomGroupBlockStr : array[1..11] of string =
   ('nonmetal',
    'noble gas',
    'alkali metal',
    'alkaline earth metal',
    'metalloid',
    'halogen',
    'transition metal',
    'metal',
    'lanthanoid',
    'actinoid',
    'post-transition metal');

    cAtomStandardStateStr : array[1..3] of String =
        ('Gaz',
         'Liquid',
         'Solid'
         );
type
  // https://github.com/andrejewski/periodic-table/blob/master/data.csv
  // et une plus complète https://gist.githubusercontent.com/GoodmanSciences/c2dd862cd38f21b0ad36b8f96b4bf1ee/raw/1d92663004489a5b6926e944c1b3d9ec5c40900e/Periodic%2520Table%2520of%2520Elements.csv
  TAtomData = Record    // Données dans l'ordre de taille des propriétés
    atomicNumber : Byte;
    atomicRadius : Byte;
    standardState : Byte;
    symbol : String[2];
    ionizationEnergy : Integer;
    electronAffinity : Integer;
    meltingPoint : Integer;
    boilingPoint : Integer;

    bondingType : Integer;
    electronegativity : Double; // Extend
    vanDelWaalsRadius : Double; // Extend
    density : Double; // Extend
    atomicMass : String;    // Extend
    cpkColor : String; //TColorVector;
    name : String;
    electronicConfiguration : String;
    ionRadius : String;
    oxidationStates : String;
    groupBlock : String;
    yearDiscovered  : String; //Integer;
  end;

  TMoleculeAtomData = record
    AtomNumber: integer;
    Pos: TAffineVector;
    class operator =(Constref A, B : TMoleculeAtomData):Boolean;overload;

    procedure Create(ElemNum : Byte; aPos : TAffineVector); overload;
    procedure Create(ElemSym : String; aPos : TAffineVector); overload;
  end;

  TMoleculeAtomLinkData = record
    BondingType: integer;
    IdStart, IdEnd: integer;
    class operator =(Constref A, B : TMoleculeAtomLinkData):Boolean;overload;

    procedure Create(el1,el2, elType : Integer);
  end;


  { TGLAtomDataList }
  generic TGLCustomArray<T> = class(specialize TGLZBaseArray<T>);

  TAtomDataList = class(specialize TGLCustomArray<TAtomData>);

  TMoleculeAtomDataList = class(specialize TGLCustomArray<TMoleculeAtomData>);

  TMoleculeAtomLinkDataList = class(specialize TGLCustomArray<TMoleculeAtomLinkData>); //Links = Bonds = Liaison

  //TCPKColorList = class(specialize TGLCustomArray<TColorVector>);


  { TGLMolecule }

  TGLMolecule = class(TObject)
    private
      FAtoms: TMoleculeAtomDataList;
      FLinks: TMoleculeAtomLinkDataList;
      FDisplayName: string;
      FRootObject : TGLBaseSceneObject;
      FAtomsInfos, FBondsInfos : TStringList;
      //FColorMapCPK : TCPKColorList;
      FViewMode : Byte; // 0 = Atoms+Bonds, 1= Atoms 2 = Bonds
      procedure SetDisplayName(const Value: string);
      procedure SetViewMode(AValue: Byte);
    protected
      DCAtoms, DCLinks : TGLDummyCube;
      //procedure InitCPK;virtual;
      function getAtomColor(anAtom : TAtomData):TColorVector;
      function getLinkColor(Idx:Byte):TColorVector;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;

      procedure LoadFromFile(Const FileName : String);
      procedure CreateMolecule(Const RootObj : TGLBaseSceneObject);

      function getAtomsInfos :TStringList;
      function getBondsInfos : TStringList;

      property Atoms: TMoleculeAtomDataList read FAtoms;
      property Links: TMoleculeAtomLinkDataList read FLinks;
      property DisplayName: string read FDisplayName write SetDisplayName;

      property ViewMode : Byte read FViewMode Write SetViewMode;
      //property RootObject : TGLBaseSceneObject read FRootObject;
    end;

  // function GetPeriodicTableData : TAtomDataList;

type

  { TMainForm }

  TMainForm = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    GLCadencer: TGLCadencer;
    DCWorld: TGLDummyCube;
    DCMoleculeWorld: TGLDummyCube;

    GLCamera: TGLCamera;
    DCTarget: TGLDummyCube;
    DCWorldGrid: TGLDummyCube;
    DCGrids: TGLDummyCube;
    DCGridXY: TGLDummyCube;
    DCGridXZ: TGLDummyCube;
    DCGridYZ: TGLDummyCube;
    GLPoints1: TGLPoints;
    GridYZ: TGLXYZGrid;
    GridXZ: TGLXYZGrid;
    GridXY: TGLXYZGrid;
    WorldGrid: TGLXYZGrid;
    MainLightSource1: TGLLightSource;
    DCMolInfos: TGLDummyCube;
    lblMolInfo2: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    LblMolInfo: TGLFlatText;
    MainStatusBar: TStatusBar;
    MoleculeAxis: TGLCube;
    DCMolecule: TGLDummyCube;
    GLMatLib: TGLMaterialLibrary;
    GLScene: TGLScene;
    GLViewer: TGLSceneViewer;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    lblMolName: TLabel;
    MainMenu1: TMainMenu;
    mmoMolAtoms: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mmoMolBonds: TMemo;
    OpenDlg: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    RadioGroup1: TRadioGroup;
    MainTimer: TTimer;
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure GLCadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure GLViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MainTimerTimer(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    oldPick, CurrentPick : TGLCustomSceneObject;
    oldColor : TColorVector;

    procedure ShowCameraLocation;
    procedure ShowFocalLength;
    procedure ShowLightLocation;
    procedure ShowTargetLocation;

  protected
    MousePoint: TPoint;
    md:Boolean;
    DoMouseMoveObj : Integer;

    procedure UpdateAtomInfosForm(anAtom : TAtomData);
  public

     Mol: TGLMolecule;
  end;

const
  crLightxz  = 1;
  crLightyz  = 2;
  crLightxy  = 3;
  crSlidexy  = 4;
  crSlideyz  = 5;
  crSlidexz  = 6;
  crRotate   = 7;
  crZoom     = 8;
  crHandMove = 9;
  crSlidezy  = 10;

Const
  cUnknownAtom : TAtomData = (
    atomicNumber : 0;
    atomicRadius : 0;
    standardState : 0;
    symbol : '--';
    ionizationEnergy : 0;
    electronAffinity : 0;
    meltingPoint : 0;
    boilingPoint : 0;
    bondingType : 0;
    electronegativity : 0;
    vanDelWaalsRadius : 0;
    density : 0;
    atomicMass : 'na';
    cpkColor : 'clrBlack';
    name : 'Unknown';
    electronicConfiguration : 'Unknown';
    ionRadius : 'Unknown';
    oxidationStates : 'Unknown';
    groupBlock : 'Unknown';
    yearDiscovered  : 'Unknown';
  );

var
  MainForm: TMainForm;
  NavCube: TGLNavCube;
  PeriodicTable : TAtomDataList;

implementation

{$R *.lfm}
{$R Cursors.res}

uses
  LazFileUtils,
  GLZStringUtils,
  uCPKForm, uHelpCommandsForm, uAtomInfosForm;

Const
 {_GLRatio / _FPColorRatio : rapport Byte/Float d'une couleur }
  _FPColorRatio: single = 1/255;

{%region%=====[ Functions Tools  ]==============================================}

function LoadCursorFromRes(CursorName:String):THandle;
var
   Cur: TCursorImage;
begin
   Cur := TCursorImage.Create;
   Cur.LoadFromResourceName(HInstance,CursorName);
   result := Cur.ReleaseHandle;
   Cur.Free;
end;

procedure Split(const Delimiter: Char; Input: string; const Strings: TStrings);
begin
   Assert(Assigned(Strings)) ;
   Strings.Clear;
   Strings.StrictDelimiter := true;
   Strings.Delimiter := Delimiter;
   Strings.DelimitedText := Input;
end;

function HexColorToColorVector(const aHexValue : String):TColorVector;
begin

  result.x  := (StrToInt('$'+Copy(aHexValue,1,2)))*_FPColorRatio;
  result.y  := (StrToInt('$'+Copy(aHexValue,3,2)))*_FPColorRatio;
  result.z  := (StrToInt('$'+Copy(aHexValue,5,2)))*_FPColorRatio;
  result.w := 1.0;//AlphaOpaque;
end;

const
  WhiteSpaces = [#8, #9, #13, #10, #32];

procedure SkipWhiteSpace(var Line: string);
begin
  while (Length(Line) > 0) and (Line[1] in WhiteSpaces) do
    Delete(Line, 1, 1);
end;

function ReadString(var Line: string): string;
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

function ReadInt(var Line: string): Integer;
Var
   i:Integer;
   s : string;
begin
  result := 0;
  s := ReadString(Line);
  if TryStrToInt(s,i) then result := i; //StrToInt(s);
end;

function ReadFloat(var Line: string): Double;
begin
  Result := StrToFloat(ReadString(Line));
end;

function AtomSymbolToAtomicNum(const symbol: string): byte;
var s: string;
begin
  s := LowerCase(symbol);
  if s = 'h' then Result := 1 else
  if s = 'he' then Result := 2 else
  if s = 'li' then Result := 3 else
  if s = 'be' then Result := 4 else
  if s = 'b' then Result := 5 else
  if s = 'c' then Result := 6 else
  if s = 'n' then Result := 7 else
  if s = 'o' then Result := 8 else
  if s = 'f' then Result := 9 else
  if s = 'ne' then Result := 10 else
  if s = 'na' then Result := 11 else
  if s = 'mg' then Result := 12 else
  if s = 'al' then Result := 13 else
  if s = 'si' then Result := 14 else
  if s = 'p' then Result := 15 else
  if s = 's' then Result := 16 else
  if s = 'cl' then Result := 17 else
  if s = 'ar' then Result := 18 else
  if s = 'k' then Result := 19 else
  if s = 'ca' then Result := 20 else
  if s = 'sc' then Result := 21 else
  if s = 'ti' then Result := 22 else
  if s = 'v' then Result := 23 else
  if s = 'cr' then Result := 24 else
  if s = 'mn' then Result := 25 else
  if s = 'fe' then Result := 26 else
  if s = 'co' then Result := 27 else
  if s = 'ni' then Result := 28 else
  if s = 'cu' then Result := 29 else
  if s = 'zn' then Result := 30 else
  if s = 'ga' then Result := 31 else
  if s = 'ge' then Result := 32 else
  if s = 'as' then Result := 33 else
  if s = 'se' then Result := 34 else
  if s = 'br' then Result := 35 else
  if s = 'kr' then Result := 36 else
  if s = 'rb' then Result := 37 else
  if s = 'sr' then Result := 38 else
  if s = 'y' then Result := 39 else
  if s = 'zr' then Result := 40 else
  if s = 'nb' then Result := 41 else
  if s = 'mo' then Result := 42 else
  if s = 'tc' then Result := 43 else
  if s = 'ru' then Result := 44 else
  if s = 'rh' then Result := 45 else
  if s = 'pd' then Result := 46 else
  if s = 'ag' then Result := 47 else
  if s = 'cd' then Result := 48 else
  if s = 'in' then Result := 49 else
  if s = 'sn' then Result := 50 else
  if s = 'sb' then Result := 51 else
  if s = 'te' then Result := 52 else
  if s = 'i' then Result := 53 else
  if s = 'xe' then Result := 54 else
  if s = 'cs' then Result := 55 else
  if s = 'ba' then Result := 56 else
  if s = 'la' then Result := 57 else
  if s = 'ce' then Result := 58 else
  if s = 'pr' then Result := 59 else
  if s = 'nd' then Result := 60 else
  if s = 'pm' then Result := 61 else
  if s = 'sm' then Result := 62 else
  if s = 'eu' then Result := 63 else
  if s = 'gd' then Result := 64 else
  if s = 'tb' then Result := 65 else
  if s = 'dy' then Result := 66 else
  if s = 'ho' then Result := 67 else
  if s = 'er' then Result := 68 else
  if s = 'tm' then Result := 69 else
  if s = 'yb' then Result := 70 else
  if s = 'lu' then Result := 71 else
  if s = 'hf' then Result := 72 else
  if s = 'ta' then Result := 73 else
  if s = 'w' then Result := 74 else
  if s = 're' then Result := 75 else
  if s = 'os' then Result := 76 else
  if s = 'ir' then Result := 77 else
  if s = 'pt' then Result := 78 else
  if s = 'au' then Result := 79 else
  if s = 'hg' then Result := 80 else
  if s = 'tl' then Result := 81 else
  if s = 'pb' then Result := 82 else
  if s = 'bi' then Result := 83 else
  if s = 'po' then Result := 84 else
  if s = 'at' then Result := 85 else
  if s = 'rn' then Result := 86 else
  if s = 'fr' then Result := 87 else
  if s = 'ra' then Result := 88 else
  if s = 'ac' then Result := 89 else
  if s = 'th' then Result := 90 else
  if s = 'pa' then Result := 91 else
  if s = 'u' then Result := 92 else
  if s = 'np' then Result := 93 else
  if s = 'pu' then Result := 94 else
  if s = 'am' then Result := 95 else
  if s = 'cm' then Result := 96 else
  if s = 'bk' then Result := 97 else
  if s = 'cf' then Result := 98 else
  if s = 'es' then Result := 99 else
  if s = 'fm' then Result := 100 else
  if s = 'md' then Result := 101 else
  if s = 'no' then Result := 102 else
  if s = 'lr' then Result := 103 else
  if s = 'rf' then Result := 104 else
  if s = 'db' then Result := 105 else
  if s = 'sg' then Result := 106 else
  if s = 'bh' then Result := 107 else
  if s = 'hs' then Result := 108 else
  if s = 'mt' then Result := 109 else
  if s = 'ds' then Result := 110 else
  if s = 'rg' then Result := 111 else
  if s = 'cn' then Result := 112 else
  if s = 'uut' then Result := 113 else
  if s = 'uuq' then Result := 114 else
  if s = 'uup' then Result := 115 else
  if s = 'uuh' then Result := 116 else
  if s = 'uus' then Result := 117 else
  if s = 'uuo' then Result := 118 else
    Result := 0;
end;


procedure QuaternionRotation(var Obj:TGLBaseSceneObject;Ex,Ey,eZ:Double);
var
 q : TQuaternion;
 m : TMatrix;
 vFrom, vTo : TAffineVector;
begin
 q := QuaternionFromRollPitchYaw(eX,eZ,eY);
 //QuaternionToPoints(q, vFrom, vTo);
 m := QuaternionToMatrix(QuaternionConjugate(q));
 Obj.Matrix := MatrixMultiply(Obj.Matrix,m);
 Obj.TransformationChanged;
end;

{%endregion%}

{%region%=====[ TGLMoleculeAtomDataList ]=======================================}

class operator TMoleculeAtomData.=(Constref A, B: TMoleculeAtomData): Boolean;
begin
  Result := (A.AtomNumber = B.AtomNumber) And (VectorEquals(A.Pos,B.Pos));
end;

procedure TMoleculeAtomData.Create(ElemNum : Byte; aPos : TAffineVector);
begin
  with Self do
  begin
    AtomNumber:=ElemNum; // Numero de l'element de la table periodique
    Pos  := aPos;
  end;
end;

procedure  TMoleculeAtomData.Create(ElemSym : String; aPos : TAffineVector);
begin
  with Self do
  begin
    AtomNumber:=AtomSymbolToAtomicNum(ElemSym); // Numero de l'element de la table periodique
    Pos  := aPos;
  end;
end;

class operator TMoleculeAtomLinkData.=(Constref A, B: TMoleculeAtomLinkData): Boolean;
begin
  Result := (A.BondingType = B.BondingType) And (A.IdEnd = B.IdEnd) And (A.IdStart = B.IdStart);
end;

procedure TMoleculeAtomLinkData.Create(el1,el2, elType : Integer);
begin
  with Self do
  begin
    BondingType:=elType;
    idStart:= el1;
    idEnd  := el2;
  end;
end;

{%endregion%}

{%region%=====[ TGLMolecule ]===================================================}

constructor TGLMolecule.Create;
begin
  inherited;
  FAtoms := TMoleculeAtomDataList.Create;
  FLinks := TMoleculeAtomLinkDataList.Create;
  FAtomsInfos := TStringList.Create;
  FBondsInfos := TStringList.Create;
  FViewMode := 0;
  //FColorMapCPK := TCPKColorList.Create;
  //InitCPK;
end;

destructor TGLMolecule.Destroy;
begin
  //FColorMapCPK.Free;
  FBondsInfos.Free;
  FAtomsInfos.Free;
  FLinks.Free;
  FAtoms.Free;
  inherited;
end;

procedure TGLMolecule.Clear;
begin
  FAtoms.Clear;
  FLinks.Clear;
end;

procedure TGLMolecule.LoadFromFile(const FileName: String);
var
 ext : String;
 sl : TStringList;
 s : String;
 nba,nbb, i, cnt, linepos : Integer;
 px,py,pz : Double;
 atomSym : String;
 iStart, iEnd, iType, iNum : Integer;
 MolAtomData: TMoleculeAtomData;
 MolLinkData: TMoleculeAtomLinkData;
begin

  // Prise ne charge basique des fichiers au format .MOL et .MOL2

  // mol2 file format description : https://fr.scribd.com/document/218351247/mol2
  // https://docs.chemaxon.com/display/docs/MDL+MOLfiles%2C+RGfiles%2C+SDfiles%2C+Rxnfiles%2C+RDfiles+formats
  // http://infochim.u-strasbg.fr/recherche/Download/Fragmentor/MDL_SDF.pdf

  ext := lowercase(ExtractFileExt(FileName));

  if (ext = '.mol') then
  begin
    // On efface les anciennes données
    FAtoms.Clear;
    FLinks.Clear;

    sl := TStringList.Create;
    sl.LoadFromFile(FileName);

    // Read Header block
    DisplayName := sl[0];

    // Skip comments and other infos header (2 lines)
    s:=sl[3];

    // Counts Line
    nba := readInt(s); // nb atoms
    nbb := readInt(s); // nb bonds

    // Read Atoms block
    cnt := nba-1;
    linepos := 4;
    for i:= 0 to cnt do
    begin
      s := sl[LinePos];
      px := ReadFloat(s);
      py := ReadFloat(s);
      pz := ReadFloat(s);
      atomSym := ReadString(s);

      MolAtomData.Create(AtomSym,AffineVectorMake(px,py,pz));
      FAtoms.Add(MolAtomData);
      inc(LinePos);
    end;

    // Read Bonds block
    cnt := nbb -1;
    for i:= 0 to cnt do
    begin
      s := sl[LinePos];
      iStart := ReadInt(s);
      iEnd := ReadInt(s);
      iType := ReadInt(s);
      MolLinkData.Create(iStart,iEnd,iType);
      FLinks.Add(MolLinkData);
      inc(LinePos);
    end;
    // Read properties block
    {@TODO}

    sl.Free;
  end
  else if (ext = '.mol2') then
  begin
    sl := TStringList.Create;
    sl.Free;
  end;

end;

procedure TGLMolecule.CreateMolecule(const RootObj: TGLBaseSceneObject);
var
  i,k: integer;
  sf: double;
  sph: TGLSphere;
  MolAtomData: TMoleculeAtomData;
  MolLinkData: TMoleculeAtomLinkData;
  AtomData : TAtomData;
  pStart, pEnd, pMid, pAngle: TAffineVector;
  cyl: TGLCylinder;
  Lines : TGLLines;
  //aNodeStart,
  //aNodeEnd : TGLLinesNode;

  //aZ,aX : Double;
begin
  FRootObject := RootObj;
  FRootObject.DeleteChildren;
  FAtomsInfos.Clear;
  FBondsInfos.Clear;

  DCAtoms := TGLDummyCube.CreateAsChild(FRootObject);
  DCLinks := TGLDummyCube.CreateAsChild(FRootObject);
  for i := 0 to Atoms.Count-1 do
  begin
    MolAtomData := (FAtoms.Items[i]);
    AtomData := PeriodicTable.Items[MolAtomData.AtomNumber];
    sph := TGLSphere.CreateAsChild(DCAtoms);
    //sf := AtomicNrToScale(atomData.AtomKind);
    sf:=1.0;
    With sph do
    begin
      //Material.MaterialOptions:= [moNoLighting];
      Material.FrontProperties.Diffuse.DirectColor := getAtomColor(atomData);
      Radius := 0.5 * (AtomData.atomicRadius*0.01) ;
      Position.AsAffineVector := MolAtomData.Pos;
      FAtomsInfos.Add('Atom : ' + i.ToString +' | ( '+TAtomData(PeriodicTable.Items[MolAtomData.AtomNumber]).symbol
                     +' ) [' + sph.Position.AsString + ']');
      Scale.AsAffineVector := AffineVectorMake(sf,sf,sf);
      Tag := MolAtomData.AtomNumber;
      k:=i+1;
      Hint := 'Atom : ' + k.ToString +' | ( '+TAtomData(PeriodicTable.Items[MolAtomData.AtomNumber]).symbol
             +' ) [' + sph.Position.AsString + ']';
    end;
  end;

  for i := 0 to Links.Count-1 do
  begin
    MolLinkData := FLinks.Items[i];
    Lines := TGLLines.CreateAsChild(DCLinks);
    With Lines do
    begin
      Antialiased := true;
      LineColor.DirectColor := getLinkColor(MolLinkData.BondingType);
      NodeColor.DirectColor := clrBrightGold;
      NodesAspect := lnaDodecahedron;
      NodeSize := 0.1;
      LineWidth := 2;
      Pickable := True;
      k:=i+1;
      Hint := 'Link : ' + k.ToString + ' | Type : '+ IntToStr((MolLinkData.BondingType))+' [' + MolLinkData.IdStart.ToString + '-->' + MolLinkData.IdEnd.ToString + ']';
    end;
    pStart := Atoms.Items[MolLinkData.IdStart-1].Pos;
    pEnd := Atoms.Items[MolLinkData.IdEnd-1].Pos;

    Lines.Nodes.AddNode(pStart);
    Lines.Nodes.AddNode(pEnd);


   { C'est, stupide mais je n'arrive pas  à visualiser comment calculer
   les bons angles de rotations avec des cylindres.....;(

    //if VectorMorethen(pStart,pEnd) then
    //begin
    //  pMid := pStart;
    //  pStart := pEnd;
    //  pEnd :=pMid;
    //end;
    pMid :=  AffineVectorMake((pStart.X+pEnd.X)*0.5,(pStart.Y+pEnd.Y)*0.5,(pStart.Z+pEnd.Z)*0.5);
    cyl := TGLCylinder.CreateAsChild(DCLinks);
    with cyl do
    begin
      Material.FrontProperties.Diffuse.DirectColor := getLinkColor(MolLinkData.BondingType);
      //Position.AsAffineVector := pMid;
      Height :=  VectorDistance(pStart,pEnd);
      TopRadius := 0.05;
      BottomRadius:= 0.05;
      Tag := i;
      Hint := 'Link : ' + i.ToString + '| Type : '+ IntToStr((MolLinkData.BondingType))+' [' + MolLinkData.IdStart.ToString + '-->' + MolLinkData.IdEnd.ToString + ']';
    end;

    // C'est la dedans que ça foire
     pAngle.X := 180/Pi*ArcTan2(pEnd.X-pStart.X, pEnd.Y-pStart.Y);
     pAngle.Y := 180/Pi*ArcTan2(pEnd.X-pStart.X, pEnd.Z-pStart.Z);
     pAngle.Z := 180/Pi*ArcTan2(pEnd.Z-pStart.Z, pEnd.Y-pStart.Y);

    //if pAngle.X<>0 then Cyl.Roll(pAngle.X);
    //if pAngle.Z<>0 then Cyl.Pitch(pAngle.Z);
    //if pAngle.Y<>0 then Cyl.Turn(pAngle.Y);
    //Cyl.RotateAbsolute(pAngle.X,pAngle.Y,pAngle.Z);
    QuaternionRotation(TGLBaseSceneObject(Cyl),pAngle.X,pAngle.Y,pAngle.Z);
    Cyl.Position.AsAffineVector := pMid;
    }


    FBondsInfos.Add(Lines.Hint);
  end;

end;

function TGLMolecule.getAtomsInfos: TStringList;
begin
  result:= FAtomsInfos;
end;

function TGLMolecule.getBondsInfos: TStringList;
begin
 result := FBondsInfos;
end;

procedure TGLMolecule.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TGLMolecule.SetViewMode(AValue: Byte);
var
  i: integer;
  sph: TGLSphere;
 // cyl: TGLCylinder;
  lines : TGLLines;
begin
  if FViewMode=AValue then Exit;
  FViewMode:=AValue;

  for i := 0 to DCAtoms.Count-1 do
  begin
      sph := TGLSphere(DCAtoms.Children[i]);
      sph.Visible := ((FViewMode = 0) or (FViewMode = 1));
  end;

  for i := 0 to DCLinks.Count-1 do
  begin
    Lines := TGLLines(DCLinks.Children[i]);
    Lines.Visible:= ((FViewMode = 0) or (FViewMode = 2));
    //cyl := TGLCylinder(DCLinks.Children[i]);
    //cyl.Visible := ((FViewMode = 0) or (FViewMode = 2));
  end;

end;

//procedure TGLMolecule.InitCPK;
//begin
//  FColorMapCPK.Capacity:=118;
//  FColorMapCPK.Items[0] := VectorMake(0.96,0.96,0.96); //
//  FColorMapCPK.Items[1] := VectorMake(0.80,0.50,1.00); //
//  FColorMapCPK.Items[2] := VectorMake(0.67,0.36,0.95); //
//  FColorMapCPK.Items[3] := VectorMake(0.56,0.25,0.83); //
//  FColorMapCPK.Items[4] := VectorMake(0.44,0.18,0.69); //
//  FColorMapCPK.Items[5] := VectorMake(0.34,0.09,0.56); //
//  FColorMapCPK.Items[6] := VectorMake(0.40,0.40,0.40); // C
//  FColorMapCPK.Items[7] := VectorMake(1.0,0.0,0.0);    //
//
//  FColorMapCPK.Items[8] := VectorMake(0.96,0.96,0.96); // Be
//  FColorMapCPK.Items[9] := VectorMake(0.96,0.96,0.96); // Mg
//  FColorMapCPK.Items[10] := VectorMake(0.96,0.96,0.96);// Ca
//  FColorMapCPK.Items[11] := VectorMake(0.96,0.96,0.96);// Sr
//  FColorMapCPK.Items[12] := VectorMake(0.96,0.96,0.96);// Ba
//  FColorMapCPK.Items[13] := VectorMake(0.96,0.96,0.96);// Ra
//
//  FColorMapCPK.Items[14] := VectorMake(0.96,0.96,0.96); // Sc
//  FColorMapCPK.Items[15] := VectorMake(0.96,0.96,0.96); // Y
//  FColorMapCPK.Items[16] := VectorMake(0.96,0.96,0.96); // Lu
//  FColorMapCPK.Items[17] := VectorMake(0.96,0.96,0.96); // Lr
//  FColorMapCPK.Items[18] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[19] := VectorMake(0.96,0.96,0.96);
//
//  FColorMapCPK.Items[20] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[21] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[22] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[23] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[24] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[25] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[26] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[27] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[28] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[29] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[30] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[31] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[32] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[33] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[34] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[35] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[36] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[37] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[38] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[39] := VectorMake(0.96,0.96,0.96);
//
//  FColorMapCPK.Items[40] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[41] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[42] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[43] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[44] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[45] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[46] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[47] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[48] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[49] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[50] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[51] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[52] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[53] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[54] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[55] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[56] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[57] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[58] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[59] := VectorMake(0.96,0.96,0.96);
//
//  FColorMapCPK.Items[60] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[61] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[62] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[63] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[64] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[65] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[66] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[67] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[68] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[69] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[70] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[71] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[72] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[73] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[74] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[75] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[76] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[77] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[78] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[79] := VectorMake(0.96,0.96,0.96);
//
//  FColorMapCPK.Items[80] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[81] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[82] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[83] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[84] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[85] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[86] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[87] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[88] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[89] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[90] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[91] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[92] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[93] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[94] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[95] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[96] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[97] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[98] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[99] := VectorMake(0.96,0.96,0.96);
//
//  FColorMapCPK.Items[100] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[101] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[102] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[103] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[104] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[105] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[106] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[107] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[108] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[109] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[110] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[111] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[112] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[113] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[114] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[115] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[116] := VectorMake(0.96,0.96,0.96);
//  FColorMapCPK.Items[117] := VectorMake(0.96,0.96,0.96);
//
//end;

function TGLMolecule.getAtomColor(anAtom: TAtomData): TColorVector;
begin
  Result := HexColorToColorVector(anAtom.cpkColor);
end;

function TGLMolecule.getLinkColor(Idx: Byte): TColorVector;
begin
  Case Idx of
    1 : Result := clrYellow;
    2 : Result := clrBlue;
    3 : Result := clrGreen;
    4 : Result := clrFuchsia;
    else Result := clrRed; // Liaison inconnue
  end;
end;

{%endregion%}


{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
Var
  AnAtomData : TMoleculeAtomData;
  AnAtomLinkData : TMoleculeAtomLinkData;

  procedure LoadPeriodicTable;
  var
    sl : TStringList;
    s : String;
    dpt : TStringList;
    i : Integer;
    anAtom : TAtomData;
  begin
    sl := TStringList.Create;
    dpt := TStringList.Create;
    sl.LoadFromFile('periodictable.dat');

    PeriodicTable.Add(cUnknownAtom);

    for i:=0 to sl.Count-1 do
    begin
      s := sl.Strings[i];
      dpt := StrOps.Explode(s,',');

      //showMessage('Out : '+dpt.Strings[0]+' - '+dpt.Strings[1]+' - '+dpt.Strings[2]);
      //ShowMessage('Exploded = '+ dpt.Text);
      with anAtom do
      begin
        atomicNumber := strToInt(dpt[0]);
        atomicRadius := strToInt(dpt[7]);
        standardState := strToInt(dpt[13]); //****
        symbol := dpt[1];
        ionizationEnergy := strToInt(dpt[10]);
        electronAffinity := strToInt(dpt[11]);
        meltingPoint := strToInt(dpt[15]);
        boilingPoint := strToInt(dpt[16]);
        bondingType := strToInt(dpt[14]);
        atomicMass := dpt[3]; //StrToFloat(dpt[3]);
        electronegativity := StrToFloat(dpt[6]);
        vanDelWaalsRadius := StrToFloat(dpt[9]);
        density := StrToFloat(dpt[17]);
        cpkColor := dpt[4];//clrBlack;
        name := dpt[2];
        electronicConfiguration := dpt[5];
        ionRadius := dpt[8];
        oxidationStates := dpt[12];
        groupBlock := dpt[18];
        yearDiscovered  := dpt[19]; //strToInt(dpt[19]);
      end;
      PeriodicTable.Add(anAtom);
    end;
    dpt.Free;
    sl.Free;
  end;

begin
  PeriodicTable := TAtomDataList.Create;
  LoadPeriodicTable;

  Screen.Cursors[crLightxy] := LoadCursorFromRes('LIGHTXY');
  Screen.Cursors[crLightyz] := LoadCursorFromRes('LIGHTYZ');
  Screen.Cursors[crLightxz] := LoadCursorFromRes('LIGHTXZ');
  Screen.Cursors[crSlidexy] := LoadCursorFromRes('SLIDEXY');
  Screen.Cursors[crSlidexz] := LoadCursorFromRes('SLIDEXZ');
  Screen.Cursors[crSlideyz] := LoadCursorFromRes('SLIDEYZ');
  Screen.Cursors[crRotate]  := LoadCursorFromRes('ROTATE');
  Screen.Cursors[crZoom]    := LoadCursorFromRes('ZOOM');
  Screen.Cursors[crSlidezy] := LoadCursorFromRes('SLIDEZY');

  NavCube := TGLNavCube.CreateAsChild(GLScene.Objects);
  NavCube.SceneViewer := GLViewer;
  NavCube.Camera:=GLCamera;
  NavCube.ActiveMouse:=True;
  NavCube.FPS := 30;
  DoMouseMoveObj := 0;

  Mol := TGLMolecule.Create;
  With Mol do
  begin
    DisplayName :='Benzene';
    With Atoms do
    begin
       AnAtomData.Create(6,AffineVectorMake(1.9050,-0.7932,0.0000));
       Add(AnAtomData);
       AnAtomData.Create(6,AffineVectorMake(1.9050, -2.1232,0.0000));
       Add(AnAtomData);
       AnAtomData.Create(6,AffineVectorMake(0.7531,-0.1282 ,0.0000));
       Add(AnAtomData);
       AnAtomData.Create(6,AffineVectorMake(0.7531,-2.7882,0.0000));
       Add(AnAtomData);
       AnAtomData.Create(6,AffineVectorMake(-0.3987,-0.7932,0.0000));
       Add(AnAtomData);
       AnAtomData.Create(6,AffineVectorMake(-0.3987,-2.1232,0.0000));
       Add(AnAtomData);
    end;

    With Links do
    begin
       AnAtomLinkData.Create(2,1,1);
       Add(AnAtomLinkData);
       AnAtomLinkData.Create(3,1,2);
       Add(AnAtomLinkData);
       AnAtomLinkData.Create(4,2,2);
       Add(AnAtomLinkData);
       AnAtomLinkData.Create(5,3,1);
       Add(AnAtomLinkData);
       AnAtomLinkData.Create(6,4,1);
       Add(AnAtomLinkData);
       AnAtomLinkData.Create(6,5,2);
       Add(AnAtomLinkData);
    end;
  end;
end;

procedure TMainForm.ShowCameraLocation;
begin
  with GLCamera.Position do
  MainStatusBar.Panels[0].Text := 'Camera: '+FloatToStrF(X, ffNumber, 5, 2)+', '+
  FloatToStrF(Y, ffNumber, 5, 2)+', '+FloatToStrF(Z, ffNumber, 5, 2);
end;

procedure TMainForm.ShowTargetLocation;
begin
  with DCTarget.Position do
  MainStatusBar.Panels[2].Text := 'Target: '+
  FloatToStrF(-X, ffNumber, 5, 2)+', '+FloatToStrF(-Y, ffNumber, 5, 2)+', '+
  FloatToStrF(-Z, ffNumber, 5, 2);
end;

procedure TMainForm.ShowFocalLength;
begin
  with GLCamera do
  MainStatusBar.Panels[1].Text := 'Focal: '+FloatToStrF(FocalLength, ffnumber, 5, 2);
end;

procedure TMainForm.ShowLightLocation;
begin
  with MainLightSource1.Position do
  MainStatusBar.Panels[3].Text := 'Light: '+
  FloatToStrF(X, ffNumber, 5, 2)+', '+FloatToStrF(Y, ffNumber, 5, 2)+', '+
  FloatToStrF(Z, ffNumber, 5, 2);
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = 'x' then DoMouseMoveObj := 1
  else if Key = 'y' then DoMouseMoveObj := 2
  else if Key = 'z' then DoMouseMoveObj := 3
  else DoMouseMoveObj := 0;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  DoMouseMoveObj := 0;
end;

procedure TMainForm.CheckBox1Change(Sender: TObject);
begin
  MoleculeAxis.Visible:= not(MoleculeAxis.Visible);
end;

procedure TMainForm.CheckBox2Change(Sender: TObject);
begin
  DCWorld.ShowAxes:= not(DCWorld.ShowAxes);
end;

procedure TMainForm.CheckBox3Change(Sender: TObject);
begin
 DCWorldGrid.Visible:= not(DCWorldGrid.Visible);
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  GLCadencer.Enabled:= False;
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  //if (MousePoint.X >= GLViewer.Left) and
  //   (MousePoint.X <= GLViewer.Left + GLViewer.Width) and
  //   (MousePoint.Y >= GLViewer.Top) and
  //   (MousePoint.y <= GLViewer.Top + GLViewer.Height) then
  //begin
    { a wheel step = WheelDelta/300; each step adjusts target distance by 2.5%
      another method to zoom in or out }
      GLCamera.AdjustDistanceToTarget(Power(1.025, WheelDelta / 300));
      GLCamera.DepthOfView := 2 * GLCamera.DistanceToTarget + 2 * DCMoleculeWorld.BoundingSphereRadius;
   // end;
    Handled := True;

end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Mol.CreateMolecule(DCMolecule);
  mmoMolAtoms.Clear;
  mmoMolAtoms.Lines := Mol.getAtomsInfos;
  mmoMolBonds.Clear;
  mmoMolBonds.Lines := Mol.getBondsInfos;
  lblMolName.Caption := Mol.DisplayName;

  ShowCameraLocation;
  ShowFocalLength;
  ShowTargetLocation;
  //ShowLightLocation;

  GLCadencer.Enabled:= true;

end;

procedure TMainForm.GLCadencerProgress(Sender: TObject; const deltaTime,newTime: Double);
begin
  // Fait tourner la scene sur elle même sur l'axe des Y
  if NavCube.InactiveTime > 5 then
  begin
    if NavCube.InactiveTime < 8 then
      GLCamera.TurnAngle := GLCamera.TurnAngle + (NavCube.InactiveTime - 5) * deltaTime * 2
    else
      GLCamera.TurnAngle := GLCamera.TurnAngle + deltatime * 6;
  end;
  GLViewer.Refresh;
  if Self.Focused then GLViewer.Invalidate;
end;

procedure TMainForm.GLViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  AtomData : TAtomData;
begin
  MousePoint.X := X;
  MousePoint.Y := Y;
  if ssCtrl in Shift then
  begin
    if ssRight in Shift then Screen.Cursor := crRotate;
  end
  else if ssShift in Shift then
  begin
    Screen.Cursor := crDrag;
  end
  else
  begin // no Shift, no Ctrl, no Alt
    if ssleft in Shift then
    begin
      if Assigned(CurrentPick) then
      begin
        if CurrentPick is TGLSphere then
        begin
          if not(AtomInfosForm.Visible) then AtomInfosForm.Show;
          AtomData := PeriodicTable.Items[CurrentPick.Tag];
          UpdateAtomInfosForm(AtomData);
        end;
      end;
      if DoMouseMoveObj>0 then
      begin
         NavCube.ActiveMouse := False;
         if (DoMouseMoveObj = 1) then Screen.Cursor := crSlidexy
         else if (DoMouseMoveObj = 2) then Screen.Cursor := crSlidezy
         else if (DoMouseMoveObj = 3) then Screen.Cursor := crSlideyz;
      end
      else
      begin
        Screen.Cursor := crRotate;
        NavCube.ActiveMouse := True;
      end;
    end
    else if ssRight in Shift then Screen.Cursor := crZoom;
  end;

  md:=true;

end;

procedure TMainForm.GLViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  d: double;
begin
  if md and (Shift <> []) then
  begin
    if ssLeft in Shift then
    begin
      if ssShift in Shift then
      begin
        if ssCtrl in Shift then
        begin
          d :=  0.01 * (X - MousePoint.x );
          DCTarget.Translate(d, 0, 0);
          d :=  0.01 * (Y - MousePoint.y );
          DCTarget.Translate(0, 0, d);
        end
        else
        begin
          d :=  0.01 * (X - MousePoint.x );
          DCTarget.Translate(d, 0, 0);
          d :=  0.01 * (Y - MousePoint.y );
          DCTarget.Translate(0, d, 0);
        end;
      end
      else
      begin
        if DoMouseMoveObj >0 then   // Translate Object
        begin
          d := GLCamera.DistanceToTarget * 0.01 * (X - MousePoint.x + Y - MousePoint.y);
          if (DoMouseMoveObj = 1) then DCMolecule.Translate(d, 0, 0)
          else if (DoMouseMoveObj = 2) then DCMolecule.Translate(0, d, 0)
          else if (DoMouseMoveObj = 3) then DCMolecule.Translate(0, 0, d);

          // Surely not working under *nix system
          //if IsKeyDown('x') then DCMoleculeWorld.Translate(d, 0, 0)
          //else if IsKeyDown('y') then DCMoleculeWorld.Translate(0, d, 0)
          //else if IsKeyDown('z') then DCMoleculeWorld.Translate(0, 0, d)
        end
        else
        begin // Move Around the world, like a Daft punk
          // NavCube.ActiveMouse:=False;
          GLCamera.MoveAroundTarget((MousePoint.y - Y) * 0.1, (MousePoint.x - X) * 0.1)
        end;
      end;
    end
    else if ssRight in Shift then
    begin
      if ssShift in Shift then   //Adjuste Camera Distance to LookAt Target Object
      begin
        with GLCamera do AdjustDistanceToTarget(Power(1.0125, MousePoint.y - Y));
      end
      else if(ssCtrl in Shift) then  // Rotate Target Object
      begin
        if (ssAlt in Shift) then
          // 1st Solution : Rotate object with a very little step thrue Camera
          GLCamera.RotateObject(DCMolecule, (MousePoint.y - Y) * 0.1, (MousePoint.x - X) * 0.1)
          //GLCamera.RotateObject(DCMolecule, MousePoint.y - Y, MousePoint.x - X);
        else
          begin
            // 2nd Solution : Rotate object directly by using Roll, Turn, Pitch functions
            DCMolecule.Turn(MousePoint.y - Y);
            DCMolecule.Pitch(MousePoint.x - X);

            // 3rd Solution : Rotate object thrue RotationAbsolute Functions
            // DCMolecule.RotateAbsolute(MousePoint.x - X, MousePoint.y - Y,0);

            // 4rd Solution : Rotate object thrue Rotation propertie
            // Note the all the follow lines don't work
            // It's have a bug in the notification scheme of TGLCoordinates object
            // DCMolecule.Rotation.X := MousePoint.x - X;
            // DCMolecule.Rotation.Y := MousePoint.y - Y;
            // or
            //DCMolecule.Rotation.Rotate(XVector,90);
            //DCMolecule.Rotation.Rotate(YVector,45);


          end;
      end
      else
      begin
        with GLCamera do
        begin
          // Change Focal (FOV)
          FocalLength  := FocalLength - (MousePoint.y - Y);
          if FocalLength > 3000 then FocalLength := 3000;   { max focal length }
          if FocalLength < 10 then FocalLength := 10;       { min focal length }
        end;       { display in statusbar palel }
      end;
    end
  end
  else
  begin
    // find what's under the mouse
    CurrentPick := (GLViewer.Buffer.GetPickedObject(x, y) as TGLCustomSceneObject);
    // if it has changed since last MouseMove...
    if (CurrentPick  <> oldPick) then
    begin
      // ...turn to black previous "hot" object...
      if Assigned(oldPick) then
      begin
        if (oldPick is TGLLines) then
          TGLLines(oldPick).LineColor.DirectColor := OldColor
        else
          oldPick.Material.FrontProperties.Emission.Color := clrBlack;
      end;
      // ...and heat up the new selection...
      if Assigned(CurrentPick ) then
      begin
        if CurrentPick is TGLLines then
        begin
          OldColor := TGLLines(CurrentPick).LineColor.DirectColor;
          TGLLines(CurrentPick).LineColor.DirectColor := clrIndian;
        end
        else
          CurrentPick .Material.FrontProperties.Emission.Color := clrIndian;

        LblMolInfo2.Text:= AnsiToUTF8(CurrentPick.Hint);
        LblMolInfo2.Visible:=true;
      end
      else LblMolInfo2.Visible:=False;

      // ...and don't forget it !
      oldPick := CurrentPick ;

    end;
  end;



    MousePoint.X := X;         { update mouse position }
    MousePoint.Y := Y;

end;

procedure TMainForm.GLViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
  md := False;
end;

procedure TMainForm.MainTimerTimer(Sender: TObject);
begin
  ShowCameraLocation;
  ShowFocalLength;
  ShowTargetLocation;
 // ShowLightLocation;
  MainStatusBar.Panels[4].Text := Format('%.1f  FPS', [GLViewer.FramesPerSecond]);
  GLViewer.ResetPerformanceMonitor;
end;

procedure TMainForm.MenuItem2Click(Sender: TObject);
begin
  if OpenDlg.Execute then
  begin
    Mol.LoadFromFile(OpenDlg.FileName);
    Mol.CreateMolecule(DCMolecule);
    mmoMolAtoms.Clear;
    mmoMolAtoms.Lines := Mol.getAtomsInfos;
    mmoMolBonds.Clear;
    mmoMolBonds.Lines := Mol.getBondsInfos;
    lblMolName.Caption := Mol.DisplayName;
  end;
end;

procedure TMainForm.MenuItem4Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.MenuItem6Click(Sender: TObject);
begin
  HelpCommandsForm.ShowModal;
end;

procedure TMainForm.MenuItem9Click(Sender: TObject);
begin
  CPKForm.Show;
end;

procedure TMainForm.RadioGroup1Click(Sender: TObject);
begin
  Mol.ViewMode:= RadioGroup1.ItemIndex;
  GLViewer.Invalidate;
end;

procedure TMainForm.UpdateAtomInfosForm(anAtom: TAtomData);
begin
  if AtomInfosForm.Visible then
  begin
    with AtomInfosForm do
    begin
      lblAtomName.Caption := anAtom.name;
      lblAtomicNum.Caption := IntToStr(anAtom.atomicNumber);
      lblAtomSym.Caption := anAtom.symbol;
      lblAtomYear.Caption := anAtom.yearDiscovered;
      lblAtomBondType.Caption := cAtomBondingTypeStr[anAtom.bondingType];
      lblIonizationNrj.Caption := IntToStr(anAtom.ionizationEnergy);
      lblElectronAffinity.Caption := IntToStr(anAtom.electronAffinity);
      lblElectronNeg.Caption := FloatToStr(anAtom.electronegativity);
      lblAtomelectronicCfg.Caption := anAtom.electronicConfiguration;
      lblDelWaalsRadius.Caption := FloatToStr(anAtom.vanDelWaalsRadius);
      lblAtomicMass.Caption := anAtom.atomicMass;
      lblonRadius.Caption := anAtom.ionRadius;
      lblAtomicradius.Caption := IntToStr(anAtom.atomicRadius);
      lblAtomDensity.Caption := FloatToStr(anAtom.density);
      lblAtomOxidationState.Caption := anAtom.oxidationStates;
      lblAtomBoilingPoint.Caption := IntToStr(anAtom.boilingPoint);
      lblAtomMeltingPoint.Caption := IntToStr(anAtom.meltingPoint);
      lblAtomStandardState.Caption := cAtomStandardStateStr[anAtom.standardState];
      lblAtomGroup.Caption := anAtom.groupBlock; //cAtomGroupBlockStr[
    end;
  end;
end;


end.

