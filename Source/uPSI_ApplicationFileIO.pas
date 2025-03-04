//
// The graphics engine GLXEngine. The unit of LZScene for Lazarus
//
unit uPSI_ApplicationFileIO;
{
This file has been generated by UnitParser v0.7, written by M. Knight
and updated by NP. v/d Spek and George Birbilis. 
Source Code from Carlo Kok has been used to implement various sections of
UnitParser. Components of ROPS are used in the construction of UnitParser,
code implementing the class wrapper is taken from Carlo Kok's conv utility

}
interface
 
uses
   SysUtils
  ,Classes
  ,uPSComponent
  ,uPSRuntime
  ,uPSCompiler
  ;
 
type 
(*----------------------------------------------------------------------------*)
  TPSImport_ApplicationFileIO = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;
 
 
{ compile-time registration functions }
procedure SIRegister_TGLDataFile(CL: TPSPascalCompiler);
procedure SIRegister_TApplicationFileIO(CL: TPSPascalCompiler);
procedure SIRegister_ApplicationFileIO(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_ApplicationFileIO_Routines(S: TPSExec);
procedure RIRegister_TGLDataFile(CL: TPSRuntimeClassImporter);
procedure RIRegister_TApplicationFileIO(CL: TPSRuntimeClassImporter);
procedure RIRegister_ApplicationFileIO(CL: TPSRuntimeClassImporter);

procedure Register;

implementation


uses
   GLApplicationFileIO
  ;
 
 
procedure Register;
begin
  RegisterComponents('GLS ROPS', [TPSImport_ApplicationFileIO]);
end;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_TGLDataFile(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TPersistent', 'TGLDataFile') do
  with CL.AddClassN(CL.FindClass('TPersistent'),'TGLDataFile') do
  begin
    RegisterMethod('Constructor Create( AOwner : TPersistent)');
    RegisterMethod('Function Capabilities : TGLDataFileCapabilities');
    RegisterMethod('Function CreateCopy( AOwner : TPersistent) : TGLDataFile');
    RegisterMethod('Procedure LoadFromFile( const fileName : String)');
    RegisterMethod('Procedure SaveToFile( const fileName : String)');
    RegisterMethod('Procedure LoadFromStream( stream : TStream)');
    RegisterMethod('Procedure SaveToStream( stream : TStream)');
    RegisterProperty('ResourceName', 'String', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TApplicationFileIO(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TComponent', 'TApplicationFileIO') do
  with CL.AddClassN(CL.FindClass('TComponent'),'TApplicationFileIO') do
  begin
    RegisterProperty('OnFileStream', 'TAFIOFileStreamEvent', iptrw);
    RegisterProperty('OnFileStreamExists', 'TAFIOFileStreamExistsEvent', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_ApplicationFileIO(CL: TPSPascalCompiler);
begin
  CL.AddTypeS('TAFIOFileStreamEvent', 'Function ( const fileName : String; mode'
   +' : Word) : TStream');
  CL.AddTypeS('TAFIOFileStreamExistsEvent', 'Function ( const fileName : String'
   +') : Boolean');
  SIRegister_TApplicationFileIO(CL);
  CL.AddTypeS('TGLDataFileCapability', '( dfcRead, dfcWrite )');
  CL.AddTypeS('TGLDataFileCapabilities', 'set of TGLDataFileCapability');
  SIRegister_TGLDataFile(CL);
  //CL.AddTypeS('TGLDataFileClass', 'class of TGLDataFile');
 CL.AddDelphiFunction('Function ApplicationFileIODefined : Boolean');
 CL.AddDelphiFunction('Function CreateFileStream( const fileName : String; mode : Word) : TStream');
 CL.AddDelphiFunction('Function FileStreamExists( const fileName : String) : Boolean');
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure TGLDataFileResourceName_W(Self: TGLDataFile; const T: String);
begin Self.ResourceName := T; end;

(*----------------------------------------------------------------------------*)
procedure TGLDataFileResourceName_R(Self: TGLDataFile; var T: String);
begin T := Self.ResourceName; end;

(*----------------------------------------------------------------------------*)
procedure TApplicationFileIOOnFileStreamExists_W(Self: TApplicationFileIO; const T: TAFIOFileStreamExistsEvent);
begin Self.OnFileStreamExists := T; end;

(*----------------------------------------------------------------------------*)
procedure TApplicationFileIOOnFileStreamExists_R(Self: TApplicationFileIO; var T: TAFIOFileStreamExistsEvent);
begin T := Self.OnFileStreamExists; end;

(*----------------------------------------------------------------------------*)
procedure TApplicationFileIOOnFileStream_W(Self: TApplicationFileIO; const T: TAFIOFileStreamEvent);
begin Self.OnFileStream := T; end;

(*----------------------------------------------------------------------------*)
procedure TApplicationFileIOOnFileStream_R(Self: TApplicationFileIO; var T: TAFIOFileStreamEvent);
begin T := Self.OnFileStream; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_ApplicationFileIO_Routines(S: TPSExec);
begin
 S.RegisterDelphiFunction(@ApplicationFileIODefined, 'ApplicationFileIODefined', cdRegister);
 S.RegisterDelphiFunction(@CreateFileStream, 'CreateFileStream', cdRegister);
 S.RegisterDelphiFunction(@FileStreamExists, 'FileStreamExists', cdRegister);
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TGLDataFile(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TGLDataFile) do
  begin
    RegisterVirtualConstructor(@TGLDataFile.Create, 'Create');
    RegisterVirtualMethod(@TGLDataFile.Capabilities, 'Capabilities');
    RegisterVirtualMethod(@TGLDataFile.CreateCopy, 'CreateCopy');
    RegisterVirtualMethod(@TGLDataFile.LoadFromFile, 'LoadFromFile');
    RegisterVirtualMethod(@TGLDataFile.SaveToFile, 'SaveToFile');
//    RegisterVirtualAbstractMethod(@TGLDataFile, @!.LoadFromStream, 'LoadFromStream');
    RegisterVirtualMethod(@TGLDataFile.SaveToStream, 'SaveToStream');
    RegisterPropertyHelper(@TGLDataFileResourceName_R,@TGLDataFileResourceName_W,'ResourceName');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TApplicationFileIO(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TApplicationFileIO) do
  begin
    RegisterPropertyHelper(@TApplicationFileIOOnFileStream_R,@TApplicationFileIOOnFileStream_W,'OnFileStream');
    RegisterPropertyHelper(@TApplicationFileIOOnFileStreamExists_R,@TApplicationFileIOOnFileStreamExists_W,'OnFileStreamExists');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_ApplicationFileIO(CL: TPSRuntimeClassImporter);
begin
  RIRegister_TApplicationFileIO(CL);
  RIRegister_TGLDataFile(CL);
end;

 
 
{ TPSImport_ApplicationFileIO }
(*----------------------------------------------------------------------------*)
procedure TPSImport_ApplicationFileIO.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_ApplicationFileIO(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_ApplicationFileIO.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ApplicationFileIO(ri);
  RIRegister_ApplicationFileIO_Routines(CompExec.Exec); // comment it if no routines
end;
(*----------------------------------------------------------------------------*)
 
 
end.
