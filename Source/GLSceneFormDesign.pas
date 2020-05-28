//
// This unit is part of the GLScene Project, http://glscene.org
//
{
    History :  
     23/08/10 - Yar - Creation
}

unit GLSceneFormDesign;

interface

{$I GLScene.inc}

uses
  Forms,
{$IFDEF GLS_DELPHI_OR_CPPB}
  Windows,
  Classes,
  ToolsAPI;
{$ENDIF}

  ProjectIntf,
  LazIDEIntf;

type


    //-------------------------Projects------------------------------------------
   { TGLSceneBaseFormDescriptor }

  
    TGLSceneBaseFormDescriptor = class(TProjectDescriptor)
    public
      constructor Create; override;
      function GetLocalizedName: string; override;
      function GetLocalizedDescription: string; override;
      function DoInitDescriptor: TModalResult; override;
      function InitProject(AProject: TLazProject): TModalResult; override;
      function CreateStartFiles(AProject: TLazProject): TModalResult; override;
    end;

    { TGLSceneSimpleFormDescriptor }

    TGLSceneSimpleFormDescriptor = class(TGLSceneBaseFormDescriptor)
    public
      constructor Create; override;
      function GetLocalizedName: string; override;
      function GetLocalizedDescription: string; override;
      function CreateStartFiles(AProject: TLazProject): TModalResult; override;
    end;

    { TGLSceneExtendedFormDescriptor }

    TGLSceneExtendedFormDescriptor = class(TGLSceneBaseFormDescriptor)
    public
      constructor Create; override;
      function GetLocalizedName: string; override;
      function GetLocalizedDescription: string; override;
      function CreateStartFiles(AProject: TLazProject): TModalResult; override;
    end;


    //--------------------------Units + LFM--------------------------------------

    { TGLSceneBaseFileDescriptor }
    //  询塴᪫ pas lfm, 便硹 﫮 殮ﱬ
  
    TGLSceneBaseFileDescriptor = class(TFileDescPascalUnitWithResource)
    public
      constructor Create; override;
      function CreateSource(const Filename     : string;
                            const SourceName   : string;
                            const ResourceName : string): string; override;
      function GetInterfaceUsesSection: string; override;
      function GetInterfaceSource(const Filename     : string;
                                  const SourceName   : string;
                                  const ResourceName : string): string; override;
      function GetLocalizedName: string; override;
      function GetLocalizedDescription: string; override;
      function GetImplementationSource(const Filename     : string;
                                       const SourceName   : string;
                                       const ResourceName : string): string; override;
      function GetResourceSource(const ResourceName: string): string; override;
    end;

    { TGLSceneSimpleFileDescriptor }
    // аﲲ褐౮櫲 便硹驠殮ﱬ+ 믬௭殲
    //Glscene, GLCamera GlCube GLLigthSource

    TGLSceneSimpleFileDescriptor = class(TGLSceneBaseFileDescriptor)
    public
      constructor Create; override;
      function CreateSource(const Filename     : string;
                            const SourceName   : string;
                            const ResourceName : string): string; override;
      function GetInterfaceUsesSection: string; override;
      function GetInterfaceSource(const Filename     : string;
                                  const SourceName   : string;
                                  const ResourceName : string): string; override;
      function GetLocalizedName: string; override;
      function GetLocalizedDescription: string; override;
      function GetImplementationSource(const Filename     : string;
                                       const SourceName   : string;
                                       const ResourceName : string): string; override;
      function GetResourceSource(const ResourceName: string): string; override;
    end;

    { TGLSceneExtendedFileDescriptor }
    //Ѡ殭ῠﱬࡱ便硹ῠ⡧顭᢮ 믬௭殲嬿
    //﨤ᮨ 쯦 ౮櫲

    TGLSceneExtendedFileDescriptor = class(TGLSceneBaseFileDescriptor)
    public
      constructor Create; override;
      function CreateSource(const Filename     : string;
                            const SourceName   : string;
                            const ResourceName : string): string; override;
      function GetInterfaceUsesSection: string; override;
      function GetInterfaceSource(const Filename     : string;
                                  const SourceName   : string;
                                  const ResourceName : string): string; override;
      function GetLocalizedName: string; override;
      function GetLocalizedDescription: string; override;
      function GetImplementationSource(const Filename     : string;
                                       const SourceName   : string;
                                       const ResourceName : string): string; override;
      function GetResourceSource(const ResourceName: string): string; override;
    end;


resourcestring
  //-------------------------Projects------------------------------------------
  rBaseProjectLocalizedName = 'GLScene Base Application';
  //٠⬮��LScene а鬮禭鿠 GLSceneForm
  rBaseProjectLocalizedDescription = 'Template of GLScene Base Application with TGLSceneForm';

  rSimpleProjectLocalizedName = 'GLScene Simple Application';
  //٠⬮��LScene а鬮禭鿠 GLSceneForm
  rSimpleProjectLocalizedDescription = 'Template of GLScene Simple Application with TGLSceneForm';

  rExtendedProjectLocalizedName = 'GLScene Extended Application';
  //٠⬮��LScene а鬮禭鿠 GLSceneForm
  rExtendedProjectLocalizedDescription = 'Template of GLScene Extended Application with TGLSceneForm';

  //--------------------------Units + LFM--------------------------------------
  //Base
  rBaseFormLocalizedName = 'GLSBaseForm';
  //TGLSceneForm - 顫 ﱬଠ믲ﱠ 溠沠⡱梥 㯩㠍
  //㽾㦰ࡨ  ﱬ 衯諸﬿沠౮騢泌 殤汨ﲰ楱㦭⡮뮮 ౨쯦殨
  rBaseFormLocalizedDescription = 'GLSceneForm - a special form, which combines '+
                                  'the properties of the viewer and the normal '+
                                  'form and allows you to render directly into '+
                                  'the application window';
  //Simple
  rSimpleFormLocalizedName = 'GLSSimpleForm';
  //GLSSimpleForm - শ顫 ﱬଠ믲ﱠ 溠沠⡱梥 㯩㠍
  //㽾㦰ࡨ  ﱬ 衱便缲 ⡧顭᢮ 믬௭殲嬿 﨤ᮨ ౮冷 ౨쯦殨.
  rSimpleFormLocalizedDescription = 'GLSSimpleForm - a special form, which '+
                                    'combines the properties viewer and the '+
                                    'usual form and contains the basic set of '+
                                    'components to create a simple application.';
  //Extended
  rExtendedFormLocalizedName = 'GLSExtendedForm';
  rExtendedFormLocalizedDescription = 'GLSExtendedForm - a special form, which '+
                                       'combines the properties viewer and the '+
                                       'usual form, and contains an expanded set '+
                                       'of components needed to create more '+
                                       'complex applications.';

procedure Register;

implementation


uses
  SysUtils,
  GLCrossPlatform,
  GLSceneForm,
  GLScene,
  GLCadencer,

  Controls,
  FormEditingIntf;

var
  vBaseFileDescriptor: TGLSceneBaseFileDescriptor;
  vSimpleFileDescriptor: TGLSceneSimpleFileDescriptor;
  vExtendedFileDescriptor: TGLSceneExtendedFileDescriptor;

  vBaseFormDescriptor: TGLSceneBaseFormDescriptor;
  vSimpleFormDescriptor: TGLSceneSimpleFormDescriptor;
  vExtendedFormDescriptor: TGLSceneExtendedFormDescriptor;


procedure Register;
begin

  //-------------------------Projects------------------------------------------

  vBaseFormDescriptor := TGLSceneBaseFormDescriptor.Create;
  RegisterProjectDescriptor(vBaseFormDescriptor);

  vSimpleFormDescriptor := TGLSceneSimpleFormDescriptor.Create;
  RegisterProjectDescriptor(vSimpleFormDescriptor);

  vExtendedFormDescriptor := TGLSceneExtendedFormDescriptor.Create;
  RegisterProjectDescriptor(vExtendedFormDescriptor);

  //--------------------------Units + LFM--------------------------------------
  //BASE
  vBaseFileDescriptor := TGLSceneBaseFileDescriptor.Create;
  RegisterProjectFileDescriptor(vBaseFileDescriptor);
  //Simple
  vSimpleFileDescriptor := TGLSceneSimpleFileDescriptor.Create;
  RegisterProjectFileDescriptor(vSimpleFileDescriptor);
  //Extended
  vExtendedFileDescriptor := TGLSceneExtendedFileDescriptor.Create;
  RegisterProjectFileDescriptor(vExtendedFileDescriptor);
  //Ҷ뮡 ѥ䦱鱳欠㯾 ﱬ,  IDE 㩤櫠衰椥鱮㡭 ﱬ
  //衯﫠輢᫠楠㯩㠍
  FormEditingHook.RegisterDesignerBaseClass(TGLSceneForm);

end;


//-------------------------Projects------------------------------------------
{ TGLSceneBaseFormDescriptor }

constructor TGLSceneBaseFormDescriptor.Create;
begin
  inherited Create;
  Name := rBaseProjectLocalizedName;
end;

function TGLSceneBaseFormDescriptor.GetLocalizedName: string;
begin
  Result := rBaseProjectLocalizedName;
end;

function TGLSceneBaseFormDescriptor.GetLocalizedDescription: string;
begin
  Result := rBaseProjectLocalizedDescription;
end;

function TGLSceneBaseFormDescriptor.DoInitDescriptor: TModalResult;
begin
  Result:=inherited DoInitDescriptor;
end;

function TGLSceneBaseFormDescriptor.InitProject(AProject: TLazProject
  ): TModalResult;
var
  NewSource: string;
  MainFile: TLazProjectFile;
begin
  Result:=inherited InitProject(AProject);
  AProject.ProjectInfoFile:='Project1.lpi';

  AProject.LazCompilerOptions.TargetFilename:='Project1';

  MainFile := AProject.CreateProjectFile('Project1.lpr');
 MainFile.IsPartOfProject := True;
 AProject.AddFile(MainFile, False);
 AProject.MainFileID := 0;

 NewSource := 'program Project1;' + LineEnding +
 LineEnding +
 '{$MODE Delphi}' + LineEnding +
 LineEnding+

 'uses'+ LineEnding +
 ' {$IFDEF UNIX}{$IFDEF UseCThreads}cthreads,{$ENDIF}{$ENDIF}'+ LineEnding +
 ' Forms, Interfaces,'+ LineEnding +
 ' Unit1 in ''Unit1.pas'';'+ LineEnding +
 LineEnding+
// '{$R *.res}'+ LineEnding +
 LineEnding +
 'begin'+ LineEnding +
 ' Application.Initialize;' + LineEnding +
 ' Application.CreateForm(TGLSceneForm1, GLSceneForm1);' + LineEnding +
 ' Application.Run;' + LineEnding +
 'end.' + LineEnding +
 LineEnding;

 AProject.MainFile.SetSourceText(NewSource);

 AProject.AddPackageDependency('GLScene_designtime');

 AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements,
 pfMainUnitHasTitleStatement,
 pfLRSFilesInOutputDirectory];

 AProject.LazCompilerOptions.SrcPath :=
         '$(LazarusDir)' + PathDelim + 'lcl'+
         PathDelim + ';$(LazarusDir)'+ PathDelim +'lcl'+
         PathDelim + 'interfaces' + PathDelim + '$(LCLWidgetType)'+
         PathDelim;
 AProject.LazCompilerOptions.IncludePath := '$(ProjOutDir)'+ PathDelim ;
 AProject.LazCompilerOptions.UnitOutputDirectory :=
 'lib'+PathDelim+'$(TargetCPU)-$(TargetOS)';

 {Set these compiler options per ExtPascal requirements}
 AProject.LazCompilerOptions.SyntaxMode := 'delphi';
 AProject.LazCompilerOptions.CStyleOperators := True;
 AProject.LazCompilerOptions.CPPInline := True;
 AProject.LazCompilerOptions.IncludeAssertionCode:= True;
 AProject.LazCompilerOptions.UseAnsiStrings := True;
 AProject.LazCompilerOptions.AssemblerStyle:=1; //Intel
 AProject.LazCompilerOptions.AllowLabel := false;

 {Always a good idea to start out with checks on}
 AProject.LazCompilerOptions.RangeChecks := True;
 AProject.LazCompilerOptions.OverflowChecks := True;
 AProject.LazCompilerOptions.StackChecks := True;
 AProject.LazCompilerOptions.OptimizationLevel := 1;

 // linking:
 AProject.LazCompilerOptions.Win32GraphicApp := True;
 AProject.LazCompilerOptions.GenerateDebugInfo := True;
 AProject.LazCompilerOptions.UseLineInfoUnit := True;
 AProject.LazCompilerOptions.UseExternalDbgSyms := True;
 //AProject.LazCompilerOptions.UseHeaptrc := True; {FastMM}
 AProject.LazCompilerOptions.StripSymbols := True;


 Result := mrOK;
end;

function TGLSceneBaseFormDescriptor.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
    LazarusIDE.DoNewEditorFile(vBaseFileDescriptor, '', '',
                             [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
    Result := mrOK;
end;

{ TGLSceneSimpleFormDescriptor }

constructor TGLSceneSimpleFormDescriptor.Create;
begin
  inherited Create;
  Name := rSimpleProjectLocalizedName;
end;

function TGLSceneSimpleFormDescriptor.GetLocalizedName: string;
begin
  Result := rSimpleProjectLocalizedName;
end;

function TGLSceneSimpleFormDescriptor.GetLocalizedDescription: string;
begin
  Result := rSimpleProjectLocalizedDescription;
end;

function TGLSceneSimpleFormDescriptor.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
    LazarusIDE.DoNewEditorFile(vSimpleFileDescriptor, '', '',
                             [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
    Result := mrOK;
end;

{ TGLSceneExtendedFormDescriptor }

constructor TGLSceneExtendedFormDescriptor.Create;
begin
  inherited Create;
  Name := rExtendedProjectLocalizedName;
end;

function TGLSceneExtendedFormDescriptor.GetLocalizedName: string;
begin
  Result := rExtendedProjectLocalizedName;
end;

function TGLSceneExtendedFormDescriptor.GetLocalizedDescription: string;
begin
  Result := rExtendedProjectLocalizedDescription;
end;

function TGLSceneExtendedFormDescriptor.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
    LazarusIDE.DoNewEditorFile(vExtendedFileDescriptor, '', '',
                             [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
    Result := mrOK;
end;

//--------------------------Units + LFM--------------------------------------

{ TGLSceneBaseFileDescriptor }

constructor TGLSceneBaseFileDescriptor.Create;
begin
  inherited Create;
  Name:=rBaseFormLocalizedName;
  DefaultResourceName :=  'GLSceneForm';
  RequiredPackages:= 'GLScene_designtime' ;
  ResourceClass:=TGLSceneForm;
  UseCreateFormStatements:=True;
end;

function TGLSceneBaseFileDescriptor.CreateSource(
  const Filename: string; const SourceName: string; const ResourceName: string
  ): string;
begin
  Result :=
    'unit ' + SourceName + ';' + LineEnding +
    LineEnding +
    'interface' + LineEnding +
    LineEnding +
    'uses' + LineEnding +
    ' ' + GetInterfaceUsesSection + LineEnding +
    LineEnding +
    GetInterfaceSource(Filename, SourceName, ResourceName) +
    LineEnding +
   'implementation' + LineEnding +
    LineEnding +
    GetImplementationSource(Filename, SourceName, ResourceName) +
    LineEnding +
    'end.';
end;

function TGLSceneBaseFileDescriptor.GetInterfaceUsesSection: string;
begin
   Result:=
    ' Classes, SysUtils, Forms, Controls, Graphics,  Dialogs,' + LineEnding +
    '  GLSceneForm;' ;
end;

function TGLSceneBaseFileDescriptor.GetInterfaceSource(
  const Filename: string; const SourceName: string; const ResourceName: string
  ): string;
begin
  Result :=
  'type' + LineEnding +
            '  T' + ResourceName + ' = class(' + ResourceClass.ClassName + ')' + LineEnding +
            '  private' + LineEnding +
            '     ' + LineEnding +
            '  public' + LineEnding +
            '     ' + LineEnding +
            '  end;' + LineEnding +
            LineEnding +
            'var' + LineEnding +  //Declared in thread class now, not global.
            '  ' + ResourceName + ': T' + ResourceName + ';' + LineEnding;

end;

function TGLSceneBaseFileDescriptor.GetLocalizedName: string;
begin
  Result:=rBaseFormLocalizedName;
end;

function TGLSceneBaseFileDescriptor.GetLocalizedDescription: string;
begin
  Result:=rBaseFormLocalizedDescription;
end;

function TGLSceneBaseFileDescriptor.GetImplementationSource(
  const Filename: string; const SourceName: string; const ResourceName: string
  ): string;
begin
  Result:='{$R *.lfm}' + LineEnding;
end;

function TGLSceneBaseFileDescriptor.GetResourceSource(
  const ResourceName: string): string;
begin
  Result:=
    'object '+ ResourceName+ ':' + 'T'+ ResourceName + LineEnding +
    '  Left = 759'+ LineEnding +
    '  Height = 240'+ LineEnding +
    '  Top = 102'+ LineEnding +
    '  Width = 320'+ LineEnding +
    'end';
end;

{ TGLSceneSimpleFileDescriptor }

constructor TGLSceneSimpleFileDescriptor.Create;
begin
  inherited Create;
  Name:=rSimpleFormLocalizedName;
  DefaultResourceName :=  'GLSceneForm';
  RequiredPackages:= 'GLScene_designtime' ;
  ResourceClass:=TGLSceneForm;
  UseCreateFormStatements:=True;
end;

function TGLSceneSimpleFileDescriptor.CreateSource(const Filename: string;
  const SourceName: string; const ResourceName: string): string;
begin
  Result :=
    'unit ' + SourceName + ';' + LineEnding +
    LineEnding +
    'interface' + LineEnding +
    LineEnding +
    'uses' + LineEnding +
    ' ' + GetInterfaceUsesSection + LineEnding +
    LineEnding +
    GetInterfaceSource(Filename, SourceName, ResourceName) +
    LineEnding +
   'implementation' + LineEnding +
    LineEnding +
    GetImplementationSource(Filename, SourceName, ResourceName) +
    LineEnding +
    'end.';
end;

function TGLSceneSimpleFileDescriptor.GetInterfaceUsesSection: string;
begin
   Result:=
    ' Classes, SysUtils, Forms, Controls, Graphics,  Dialogs,' + LineEnding +
    '  GLSceneForm, GLScene, GLObjects, GLCadencer;' ;
end;

function TGLSceneSimpleFileDescriptor.GetInterfaceSource(
  const Filename: string; const SourceName: string; const ResourceName: string
  ): string;
begin
  Result :=
  'type' + LineEnding +
            '  T' + ResourceName + ' = class(' + ResourceClass.ClassName + ')' + LineEnding +
            '    GLCadencer1: TGLCadencer;' + LineEnding +
            '    GLCamera1: TGLCamera;' + LineEnding +
            '    GLCube1: TGLCube;' + LineEnding +
            '    GLDummyCube1: TGLDummyCube;' + LineEnding +
            '    GLLightSource1: TGLLightSource;' + LineEnding +
            '    GLScene1: TGLScene;' + LineEnding +
            '    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);' + LineEnding +
            '  private' + LineEnding +
            '     ' + LineEnding +
            '  public' + LineEnding +
            '     ' + LineEnding +
            '  end;' + LineEnding +
            LineEnding +
            'var' + LineEnding +  //Declared in thread class now, not global.
            '  ' + ResourceName + ': T' + ResourceName + ';' + LineEnding;

end;

function TGLSceneSimpleFileDescriptor.GetLocalizedName: string;
begin
  Result:=rSimpleFormLocalizedName;
end;

function TGLSceneSimpleFileDescriptor.GetLocalizedDescription: string;
begin
  Result:=rSimpleFormLocalizedDescription;
end;

function TGLSceneSimpleFileDescriptor.GetImplementationSource(
  const Filename: string; const SourceName: string; const ResourceName: string
  ): string;
begin
  Result:='{$R *.lfm}' + LineEnding +
    LineEnding +
    'procedure T'+ResourceName+'.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);' + LineEnding   +
    'begin' + LineEnding   +
    '  Invalidate;' + LineEnding   +
    'end;' + LineEnding +
    LineEnding;
end;

function TGLSceneSimpleFileDescriptor.GetResourceSource(
  const ResourceName: string): string;
begin
  Result:=
    'object '+ ResourceName+ ':' + 'T'+ ResourceName + LineEnding +
    '  Left = 246' + LineEnding   +
    '  Height = 600' + LineEnding   +
    '  Top = 74' + LineEnding   +
    '  Width = 800' + LineEnding   +
    '  Buffer.BackgroundColor = 2064383' + LineEnding   +
    '  Camera = GLCamera1' + LineEnding   +
    '  object GLScene1: TGLScene' + LineEnding   +
    '    Left = 24' + LineEnding   +
    '    Top = 24' + LineEnding   +
    '    object GLCamera1: TGLCamera' + LineEnding   +
    '      DepthOfView = 100.000000000000000000' + LineEnding   +
    '      FocalLength = 50.000000000000000000' + LineEnding   +
    '      TargetObject = GLDummyCube1' + LineEnding   +
    '      Position.Coordinates = {0000803F00000040000040400000803F}' + LineEnding   +
    '      object GLLightSource1: TGLLightSource' + LineEnding   +
    '        ConstAttenuation = 1.000000000000000000' + LineEnding   +
    '        SpotCutOff = 180.000000000000000000' + LineEnding   +
    '      end' + LineEnding   +
    '    end' + LineEnding   +
    '    object GLDummyCube1: TGLDummyCube' + LineEnding   +
    '    end ' + LineEnding   +
    '    object GLCube1: TGLCube' + LineEnding   +
    '      TagFloat = 0' + LineEnding   +
    '      PitchAngle = 0' + LineEnding   +
    '      RollAngle = 0' + LineEnding   +
    '      TurnAngle = 0' + LineEnding   +
    '    end' + LineEnding   +
    '  end' + LineEnding   +
    '  object GLCadencer1: TGLCadencer' + LineEnding   +
    '    Scene = GLScene1' + LineEnding   +
    '    OnProgress = GLCadencer1Progress' + LineEnding   +
    '    Left = 64' + LineEnding   +
    '    Top = 24' + LineEnding   +
    '  end' + LineEnding   +
    'end';

end;

{ TGLSceneExtendedFileDescriptor }

constructor TGLSceneExtendedFileDescriptor.Create;
begin
  inherited Create;
  Name:=rExtendedFormLocalizedName;
  DefaultResourceName :=  'GLSceneForm';
  RequiredPackages:= 'GLScene_designtime' ;
  ResourceClass:=TGLSceneForm;
  UseCreateFormStatements:=True;
end;

function TGLSceneExtendedFileDescriptor.CreateSource(const Filename: string;
  const SourceName: string; const ResourceName: string): string;
begin
  Result :=
    'unit ' + SourceName + ';' + LineEnding +
    LineEnding +
    'interface' + LineEnding +
    LineEnding +
    'uses' + LineEnding +
    ' ' + GetInterfaceUsesSection + LineEnding +
    LineEnding +
    GetInterfaceSource(Filename, SourceName, ResourceName) +
    LineEnding +
   'implementation' + LineEnding +
    LineEnding +
    GetImplementationSource(Filename, SourceName, ResourceName) +
    LineEnding +
    'end.';
end;

function TGLSceneExtendedFileDescriptor.GetInterfaceUsesSection: string;
begin
   Result:=
    ' Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, GLScene,' + LineEnding +
    '  GLSceneForm, GLCadencer, GLMaterial, GLObjects, GLHUDObjects, GLWindowsFont,' + LineEnding +
    '  GLSkydome, GLGeomObjects, GLShadowPlane, GLLCLViewer, GLBaseClasses, GLFilePNG;' + LineEnding;

end;

function TGLSceneExtendedFileDescriptor.GetInterfaceSource(
  const Filename: string; const SourceName: string; const ResourceName: string
  ): string;
begin
  Result :=
  'type' + LineEnding +
            LineEnding +
            '  { T'+ResourceName+' } '+
            LineEnding +
            '  T' + ResourceName + ' = class(' + ResourceClass.ClassName + ')' + LineEnding +
            '    GLCadencer1: TGLCadencer;' + LineEnding +
            '    GLBackground: TGLDummyCube;' + LineEnding +
            '    GLCamera1: TGLCamera;' + LineEnding +
            '    GLCone1: TGLCone;' + LineEnding +
            '    ShadowObjects: TGLDummyCube;' + LineEnding +
            '    GLShadowPlane1: TGLShadowPlane;' + LineEnding +
            '    GLWorldObject1: TGLCube;' + LineEnding +
            '    GLCameraTarget: TGLDummyCube;' + LineEnding +
            '    GLWorldObject4: TGLCylinder;' + LineEnding +
            '    GLInterfaceSprite1: TGLHUDSprite;' + LineEnding +
            '    GLInterfaceText1: TGLHUDText;' + LineEnding +
            '    GLInterface: TGLDummyCube;' + LineEnding +
            '    GLLightSource1: TGLLightSource;' + LineEnding +
            '    GLSkyDome1: TGLSkyDome;' + LineEnding +
            '    GLWorldObject3: TGLSphere;' + LineEnding +
            '    GLWindowsBitmapFont1: TGLWindowsBitmapFont;' + LineEnding +
            '    GLWorld: TGLDummyCube;' + LineEnding +
            '    GLMaterialLibrary1: TGLMaterialLibrary;' + LineEnding +
            '    GLScene1: TGLScene;' + LineEnding +
            '    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,' + LineEnding +
            '      newTime: Double);' + LineEnding +
            '  private' + LineEnding +
            '     ' + LineEnding +
            '  public' + LineEnding +
            '     ' + LineEnding +
            '  end;' + LineEnding +
            LineEnding +
            'var' + LineEnding +  //Declared in thread class now, not global.
            '  ' + ResourceName + ': T' + ResourceName + ';' + LineEnding;
end;

function TGLSceneExtendedFileDescriptor.GetLocalizedName: string;
begin
  Result := rExtendedFormLocalizedName;
end;

function TGLSceneExtendedFileDescriptor.GetLocalizedDescription: string;
begin
  Result := rExtendedFormLocalizedDescription;
end;

function TGLSceneExtendedFileDescriptor.GetImplementationSource(
  const Filename: string; const SourceName: string; const ResourceName: string
  ): string;
begin
  Result:='{$R *.lfm}' + LineEnding +
    LineEnding +
    'procedure T'+ResourceName+'.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);' + LineEnding   +
    'begin' + LineEnding   +
    '  Invalidate;' + LineEnding   +
    'end;' + LineEnding +
    LineEnding;
end;

function TGLSceneExtendedFileDescriptor.GetResourceSource(
  const ResourceName: string): string;
begin
  Result:=
    'object '+ ResourceName+ ':' + 'T'+ ResourceName + LineEnding   +
    '  Left = 246' + LineEnding +
    '  Height = 600' + LineEnding +
    '  Top = 74' + LineEnding +
    '  Width = 800' + LineEnding +
    '  Caption = '''+ResourceName+'''' + LineEnding +
    '  Camera = GLCamera1' + LineEnding +
    '  Buffer.BackgroundColor = clWhite' + LineEnding +
    '  Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]' + LineEnding +
    '  Buffer.AntiAliasing = aa8x' + LineEnding +
    '  FieldOfView = 161.07536315918' + LineEnding +
    '  object GLScene1: TGLScene' + LineEnding +
    '    left = 72' + LineEnding +
    '    top = 39' + LineEnding +
    '    object GLBackground: TGLDummyCube' + LineEnding +
    '      CubeSize = 1' + LineEnding +
    '      object GLSkyDome1: TGLSkyDome' + LineEnding +
    '        Direction.Coordinates = {' + LineEnding +
    '          000000000000803F2EBD3BB300000000' + LineEnding +
    '        }' + LineEnding +
    '        PitchAngle = 90' + LineEnding +
    '        Position.Coordinates = {' + LineEnding +
    '          000000000000F0C1000000000000803F' + LineEnding +
    '        }' + LineEnding +
    '        Up.Coordinates = {' + LineEnding +
    '          000000002EBD3BB3000080BF00000000' + LineEnding +
    '        }' + LineEnding +
    '        Bands = <' + LineEnding +
    '          item' + LineEnding +
    '            StartColor.Color = {' + LineEnding +
    '              0000803F0000803F0000803F0000803F' + LineEnding +
    '            }' + LineEnding +
    '            StopAngle = 15' + LineEnding +
    '            Slices = 20' + LineEnding +
    '          end' + LineEnding +
    '          item' + LineEnding +
    '            StartAngle = 15' + LineEnding +
    '            StopAngle = 90' + LineEnding +
    '            StopColor.Color = {' + LineEnding +
    '              938C0C3E938C0C3E938E0E3F0000803F' + LineEnding +
    '            }' + LineEnding +
    '            Slices = 20' + LineEnding +
    '            Stacks = 4' + LineEnding +
    '          end>' + LineEnding +
    '        Stars = <>' + LineEnding +
    '      end' + LineEnding +
    '    end' + LineEnding +
    '    object GLWorld: TGLDummyCube' + LineEnding +
    '      CubeSize = 1' + LineEnding +
    '      object GLLightSource1: TGLLightSource' + LineEnding +
    '        ConstAttenuation = 1' + LineEnding +
    '        Diffuse.Color = {' + LineEnding +
    '          DBDA5A3FDBDA5A3FDBDA5A3F0000803F' + LineEnding +
    '        }' + LineEnding +
    '        Position.Coordinates = {' + LineEnding +
    '          000000400000C040000000400000803F' + LineEnding +
    '        }' + LineEnding +
    '        SpotCutOff = 180' + LineEnding +
    '        SpotExponent = 0' + LineEnding +
    '      end' + LineEnding +
    '      object GLCamera1: TGLCamera' + LineEnding +
    '        DepthOfView = 100' + LineEnding +
    '        FocalLength = 50' + LineEnding +
    '        TargetObject = GLCameraTarget' + LineEnding +
    '        Position.Coordinates = {' + LineEnding +
    '          0000C0400000C0400000C0400000803F' + LineEnding +
    '        }' + LineEnding +
    '      end' + LineEnding +
    '      object GLCameraTarget: TGLDummyCube' + LineEnding +
    '      end' + LineEnding +
    '      object ShadowObjects: TGLDummyCube' + LineEnding +
    '        object GLWorldObject4: TGLCylinder' + LineEnding +
    '          Material.FrontProperties.Diffuse.Color = {' + LineEnding +
    '            CDCC4C3F8584043FC1C0403D0000803F' + LineEnding +
    '          }' + LineEnding +
    '          Position.Coordinates = {' + LineEnding +
    '            0000000000000000000000C00000803F' + LineEnding +
    '          }' + LineEnding +
    '          BottomRadius = 1' + LineEnding +
    '          Height = 2' + LineEnding +
    '          TopRadius = 1' + LineEnding +
    '        end' + LineEnding +
    '        object GLWorldObject3: TGLSphere' + LineEnding +
    '          Material.FrontProperties.Diffuse.Color = {' + LineEnding +
    '            8180803CCDCC4C3FCDCC4C3F0000803F' + LineEnding +
    '          }' + LineEnding +
    '          Position.Coordinates = {' + LineEnding +
    '            000080BF00000040000000000000803F' + LineEnding +
    '          }' + LineEnding +
    '          Radius = 1' + LineEnding +
    '        end' + LineEnding +
    '        object GLWorldObject1: TGLCube' + LineEnding +
    '          TagFloat = 0' + LineEnding +
    '          Material.FrontProperties.Ambient.Color = {' + LineEnding +
    '            0000803F00000000000000000000803F' + LineEnding +
    '          }' + LineEnding +
    '          Material.FrontProperties.Diffuse.Color = {' + LineEnding +
    '            0000803F00000000000000000000803F' + LineEnding +
    '          }' + LineEnding +
    '          Position.Coordinates = {' + LineEnding +
    '            0000000000000000000040400000803F' + LineEnding +
    '          }' + LineEnding +
    '        end' + LineEnding +
    '        object GLCone1: TGLCone' + LineEnding +
    '          Direction.Coordinates = {' + LineEnding +
    '            00000000000080BF2EBD3BB300000000' + LineEnding +
    '          }' + LineEnding +
    '          PitchAngle = -90' + LineEnding +
    '          Position.Coordinates = {' + LineEnding +
    '            000000400000803F000080BF0000803F' + LineEnding +
    '          }' + LineEnding +
    '          Up.Coordinates = {' + LineEnding +
    '            000000002EBD3BB30000803F00000000' + LineEnding +
    '          }' + LineEnding +
    '          BottomRadius = 0.5' + LineEnding +
    '          Height = 1' + LineEnding +
    '        end' + LineEnding +
    '      end' + LineEnding +
    '      object GLShadowPlane1: TGLShadowPlane' + LineEnding +
    '        Direction.Coordinates = {' + LineEnding +
    '          000000000000803F2EBD3BB300000000' + LineEnding +
    '        }' + LineEnding +
    '        PitchAngle = 90' + LineEnding +
    '        Position.Coordinates = {' + LineEnding +
    '          00000000000000C0000000000000803F' + LineEnding +
    '        }' + LineEnding +
    '        Up.Coordinates = {' + LineEnding +
    '          000000002EBD3BB3000080BF00000000' + LineEnding +
    '        }' + LineEnding +
    '        Height = 10' + LineEnding +
    '        Width = 10' + LineEnding +
    '        ShadowingObject = ShadowObjects' + LineEnding +
    '        ShadowedLight = GLLightSource1' + LineEnding +
    '        ShadowColor.Color = {' + LineEnding +
    '          9A99993E9A99993E9A99993E0000803F' + LineEnding +
    '        }' + LineEnding +
    '      end' + LineEnding +
    '    end' + LineEnding +
    '    object GLInterface: TGLDummyCube' + LineEnding +
    '      object GLInterfaceText1: TGLHUDText' + LineEnding +
    '        Position.Coordinates = {' + LineEnding +
    '          0000F0410000F041000000000000803F' + LineEnding +
    '        }' + LineEnding +
    '        BitmapFont = GLWindowsBitmapFont1' + LineEnding +
    '        Text = ''GLScene Project''' + LineEnding +
    '        Rotation = 0' + LineEnding +
    '        ModulateColor.Color = {' + LineEnding +
    '          D0CF4F3FDBDA5A3FF6F5753F0000803F' + LineEnding +
    '        }' + LineEnding +
    '      end' + LineEnding +
    '      object GLInterfaceSprite1: TGLHUDSprite' + LineEnding +
    '        TagFloat = 0' + LineEnding +
    '        Material.MaterialLibrary = GLMaterialLibrary1' + LineEnding +
    '        Material.LibMaterialName = ''LibMaterial''' + LineEnding +
    '        PitchAngle = 0' + LineEnding +
    '        Position.Coordinates = {' + LineEnding +
    '          008027440000FA43000000000000803F' + LineEnding +
    '        }' + LineEnding +
    '        RollAngle = 0' + LineEnding +
    '        TurnAngle = 0' + LineEnding +
    '        Width = 200' + LineEnding +
    '        Height = 100' + LineEnding +
    '        Rotation = 0' + LineEnding +
    '      end' + LineEnding +
    '    end' + LineEnding +
    '  end' + LineEnding +
    '  object GLCadencer1: TGLCadencer' + LineEnding +
    '    Scene = GLScene1' + LineEnding +
    '    FixedDeltaTime = 0' + LineEnding +
    '    OnProgress = GLCadencer1Progress' + LineEnding +
    '    left = 112' + LineEnding +
    '    top = 39' + LineEnding +
    '  end' + LineEnding +
    '  object GLMaterialLibrary1: TGLMaterialLibrary' + LineEnding +
    '    Materials = <' + LineEnding +
    '      item' + LineEnding +
    '        Name = ''LibMaterial''' + LineEnding +
    '        Material.BlendingMode = bmModulate' + LineEnding +
    '        Material.Texture.Image.Picture.Data = {' + LineEnding +
    '          1754506F727461626C654E6574776F726B47726170686963ED1B000089504E47' + LineEnding +
    '          0D0A1A0A0000000D49484452000000A60000004E080200000025FDFACF000000' + LineEnding +
    '          097048597300000EC300000EC301C76FA86400001B9F49444154785EEDDD7594' + LineEnding +
    '          24459705F00FF70FF7C5DD1D1677585C17777758DCDDDDDDDDDDE5E08BBBBBBB' + LineEnding +
    '          BBC3FE0E778993A7AA7BBABAA7AD7A26FF9853951D191919F7BDFBEE7B115933' + LineEnding +
    '          C85F7FFDF5AF5E79BCFDF6DB1F7DF491A14D3EF9E4A38E3A6AAF1C63730E0AE4' + LineEnding +
    '          BDE4F8FEFBEF3FF9E493DF7EFBED8B2FBE78E18517FEF79FE3E38F3FFEF3CF3F' + LineEnding +
    '          7BC920FBC030FED51B9E01A26FBDF516881F7AE8A18274F583F34F3EF9E48B2F' + LineEnding +
    '          BEF8F9E79FF7860137F5187A05E46FBCF146007EECB1C760FFD5575FFDFAEBAF' + LineEnding +
    '          BEBEFEFAEB3FFEF8E3679F7DF6CA2BAF140B78FEF9E79D6CEA49EFD9C1F73CE4' + LineEnding +
    '          1C179C8F3EFA2802FFE38F3FCA7404F2F2F5A79F7E7AEDB5D7C2043EF7ECAC35' + LineEnding +
    '          F5DDBB1B728A2C7E6CD644EEF0B9A3EAB8C2F9975F7EE9A488FECD37DF54EDE0' + LineEnding +
    '          BDF7DE73FEE9A79F46034D3DEF3D38F88E40FECB2FBF9C72CA29471D75D4A187' + LineEnding +
    '          1EFA3FFF1CFFFDCF31DF7CF3FDE7DFC774D34DF71F7F1F638C314691B6D34C33' + LineEnding +
    '          CD4C33CDB4EBAEBB8ADF4F3CF14489DF71E81F7EF8E1E5975FAE89E88F3CF2C8' + LineEnding +
    '          3BEFBCF3FBEFBF73EEC71F7F3C7F1502BEFEFAEB1E9CB8E6BD75BB21E77F0B2D' + LineEnding +
    '          B4D0924B2EB9C4124B74204719649041AEBCF24AE1D994FDFCF3CF6FBEF9A67F' + LineEnding +
    '          E10DC5E79E7BAE80CDBF3FFDF4533E1DB370906FC19B9EFFE0830F622B447EF3' + LineEnding +
    '          4E7D4F8DBC7D909351134F3C31A425CA1DCE95A79A6AAA134E38E1A69B6EE2A9' + LineEnding +
    '          EFBEFB2ECEF0F02FBDF41214393423A85167A200AD1EE0C5FBCC14FBF0F59967' + LineEnding +
    '          9EE9A9896BDEFBB60372533CCA28A374C0B35BBB64F0C1079F7DF6D90F3BEC30' + LineEnding +
    '          BA8CAFD371A2786B5349048409D280AFFBFAD4534FC562061E8DCF40A3905F72' + LineEnding +
    '          C925C30E3B6C27E25DED4ABCDF73CF3DD13BE05B1BBA584ED5835964D186A643' + LineEnding +
    '          0958A7F1471DD83233D010E47BEDB55717815DED76DC71C7DD679F7DA088D845' + LineEnding +
    '          EB638E39668F3DF6B8E1861B0A54441CC80BCC6201231858986BAF29B70139A6' + LineEnding +
    '          DD64934DBA01EF728BF1C61B6FD34D371D7EF8E1CB190697A7C2012027E8F235' + LineEnding +
    '          1630B01ED799904B99965D76D9EEC4BBC57BCD30C30CE5A97836C17FF9E59797' + LineEnding +
    '          625C8BDC2E0A6863FCED9D8E01A17DBFBC5C1A26D9EE66C8975A6AA96DB7DD76' + LineEnding +
    '          EBADB796F06FB1C516238D34D22EBBEC5290C0F3C633D75C7369B3D8628B5964' + LineEnding +
    '          1B618411CA08690255816DB6D9E6C0030F7472D555571D10206CEF33B60A392F' + LineEnding +
    '          1972C821D75D77DD6E86BC7ABB11471CF1A28B2E2A325EB676E9A597363E9EB5' + LineEnding +
    '          D65AABBDD33120B46F1972994FF2EF9E3DD4EFAAE42C3B3FE79C7332247F5A73' + LineEnding +
    '          CD3559E4061B6C406D6CB6D9669B6FBEB97FE98015575C31350384312040D8DE' + LineEnding +
    '          676C1972A5929E05DBDDFFFDEF7F2BBE96E7516C07EAD8638FBDC20A2BACBCF2' + LineEnding +
    '          CA534F3DB5429E66F3CC330FE6DF7DF7DDB7DB6EBB5476150F80CD1A66996516' + LineEnding +
    '          755FB95C7B27A56FB76F19729AE8F8E38FDF6AABAD165F7CF10926982093DB0D' + LineEnding +
    '          C730C30C33E1841346AE9F7BEEB955D5266C2FB0C002C01E6AA8A1AA23D977DF' + LineEnding +
    '          7DD30CFF1B6AF54FB3CD36DB965B6EC9E385F6BE8D62BB9EAED5586E06C5CEF7' + LineEnding +
    '          DF7F1FFC0F3EF8E0A9A79E4A4F1174934E3AE960830DD6E9F05361071D74D00E' + LineEnding +
    '          3BECB0FAEAAB9366F03EFCF0C3D56794DBAEBBEEBAB1C61A4B60867AF5BE4EFA' + LineEnding +
    '          CA2ECB03D37A0C6288218628CDD89006D34F3F3DF2F738BA5D7BEDB5B38E37C0' + LineEnding +
    '          1E0D9562CC8EB4E7DB6FBF55E2560397199F7FFEF9D265F0A8988E36DA68FD6F' + LineEnding +
    '          01702AEE4B362AF60551C770C30DB7C61A6B944C5D9956EA6800181B751F7BEC' + LineEnding +
    '          B1018FFE3098F5D65BCF028C95BA71C619A78C4A7B4A7ED04107750663954BB2' + LineEnding +
    '          E9CA13D9A321CBB7D8D38FF25F9F31914621AF79E07BEEB987DF3CF0C003A181' + LineEnding +
    '          BBEEBACB5A2AF5847B0B541DB603FA0B6C5C5C0F1865E38D37E6ACE90D755F78' + LineEnding +
    '          E18592F2871F7E185A1F7EF861594C53826591A9CC584DA703104319037E9A71' + LineEnding +
    '          C6197D9D6492495CA88D0BADBB97FC3E1F9CE9F3D97C4720B71569C71D773CE9' + LineEnding +
    '          A493AEBAEAAA620A3C86C252287DF5D557EFBBEFBE134F3C11A32EB2C82240A2' + LineEnding +
    '          AAA8EBC62D803D412EAA7BB5D556C3220925238F3CB2F537C0B84B23C05850DF' + LineEnding +
    '          69A79DCA7D1140968558A7A15A85D395DA8E669CDB9A5E5989C7677DC6A7EB1F' + LineEnding +
    '          A4DD909B1A11B71F4B5EB9079FFBEEBBEFB2EFE5B8E38EBBF9E69B456BC1583D' + LineEnding +
    '          B51A6BAB76B0E0820B52DD915A1B6EB8A13FF14BFB2908F58B2FBE3849E31147' + LineEnding +
    '          1CA1C3C6F120EF5D45DE9F7CF2C9A38F3EFA461B6D14D3B16E0B6F645EBA7AF6' + LineEnding +
    '          D9675912D2729E4DF7E15D37ED839CF3ADB3CE3AEDDD6DC869C29916491DDC4B' + LineEnding +
    '          8165E79D775E6EB9E56C92A1B9945C2CA1CE3BEFBCF0D87EFBED358E5B438895' + LineEnding +
    '          C4F9EEBDF75EE21C305445E3905F73CD350889993241CE6DA97ED65967D5B360' + LineEnding +
    '          91E5764F64B38D3DF38C203BAEE06DB5A60F6FB96907E490161DB376D99F87E5' + LineEnding +
    '          2FCC6CDF0B975560911194BAE99D77DE497CC5C5E5DC471F7D3460ACAB025E63' + LineEnding +
    '          D537791A2D463C2EB3CC327230CC9CC1DC71C71DE18CC9269BAC0C8FB3961080' + LineEnding +
    '          7504F28418F97D5E8D201A12C5C11C63CAC2BCB1F5E733F6DACB1B851C93AB7B' + LineEnding +
    '          A3BEAE78127A2D684142FF73CF3DB7CF5235BECED1B32D0E128B2EBAA8F333CF' + LineEnding +
    '          3C33A180A8C50B5FC9FBD34F3F5D83EBAFBFDE57FC2F40B43848BEAE678612F1' + LineEnding +
    '          8F3622F48A82C3EA6C513072C6BF5DF1A4BDA1CF862067FEBBEDB65BD991D2E9' + LineEnding +
    '          E34E0905DEE89D2F0AF6C05606881D48AED4D762070E7F823D98A798628AD2C0' + LineEnding +
    '          F29AF43D5F55606A4628FB924F46431083028A0FFBEFBF7F9A0916589D617940' + LineEnding +
    '          DBEB7CEDDB6BB20D416E272B82EDA20A067BE2A9423B99C677C9757808BA7654' + LineEnding +
    '          06426E4DC4E53307852E8FA70F2487EBAFBF7ECE5B4373BE7CAE875C72EF2E69' + LineEnding +
    '          A00EEF5F76238A3B503AF2175FC494ABAFBEFAF6DB6F670134BC93B7DD765BA7' + LineEnding +
    '          DB778F77D836E44A1C38F6BCF3CEB3DFB42B862B6A52E9EAAC60B8F6DA6B6D97' + LineEnding +
    '          F661A595568A4407154256330F5A78BE2ADF0C4978A601493368B50639BA5642' + LineEnding +
    '          B0009306D9F48124D4941CFA54B7B120EB2EC8C0F2BCAF6A0C46525DAAEF8A67' + LineEnding +
    '          EF913EDB865CF26DBEBA6E7074B2E81B3028350B243E14569F76DA69DD9AD3A7' + LineEnding +
    '          C1ADB7DE5A331298C9B53865217616D0E268D58CD3493AC7F0871C7288844D0A' + LineEnding +
    '          97743F0741A72EABAEAC8D5599AE7BF09EEAB96DC8E533C430306A5E20EAAC11' + LineEnding +
    '          53CE41C2C1B1965E7A691FDC346794F3DC882B2BA4D813A1329AFB96ED31D4BE' + LineEnding +
    '          A57D079CB2FC23EAB738B6C2FC09072442DA2BEE6272A623967B462CA2A8C708' + LineEnding +
    '          D4827A1C72AAB9CD12487B81681B72604B8DC863FE679B8A9A9AD459E85510DD' + LineEnding +
    '          6FBFFDD446CC35E7B3BF989E6FA48281667364AC32F5A02BFB1A7FFCF1F339E1' + LineEnding +
    '          B6F8D9FCF3CF2F8B43F28C2F577150A24F44A000A2E4C97E286A869C5B9C05C5' + LineEnding +
    '          80F429F52846E6831ABBE2ABC85D9D5CCC810CBA87D86BF67295B4D35374C566' + LineEnding +
    '          CEB621AF4E9F4941A144B532B53406C684AEE50DFB93F6DE7B6F551ABE68A6C8' + LineEnding +
    '          7B5F0F38E000C0B009E6C20BC1462B71538FA400CEB1C8250573D6C3DBD89345' + LineEnding +
    '          118B699AA9A873C4D4D5B99A01D8DBA4486E25D7F25AC6636D570DC73E2D3158' + LineEnding +
    '          FAAE25558F1210B5B5961621B77D36486B59859C66AC6FEF49EFBEFB6E1B31DA' + LineEnding +
    '          EB438DB72792948952F02F87E2876270E39D74A065FB206FF106C433A455D0CA' + LineEnding +
    '          3CD2D50AAB5CC4AB4C84187D8412C45AFB94E79C734E28D62CC0F3D42C8C2255' + LineEnding +
    '          E5307600F812BF154F40CB02A467CAF529B8AAE4DF78E38DBA4DEAA54F544C91' + LineEnding +
    '          F172DE5F3F4EAB67583A2354EA11EF7DA0E3A4E9430F3D34E3ABBF84A3572BB2' + LineEnding +
    '          D5061E593A27F0CBE83A4CBCC207B9AADBFBEFBF9F23F95787ECC00A4507806C' + LineEnding +
    '          FC924E809C5B5BDC2C7954D581AA9F5BDB67E17CF1BFBC7F2407B325063F97CB' + LineEnding +
    '          D90D1D0772C6247DC719BC01A994957BB5582655F6C968A02C4F199C71C619FE' + LineEnding +
    '          ADEEC3B7442B36A5F60E6F26E8837E9C911AB439714052FB73893578DBEC1118' + LineEnding +
    '          421286148258B61847EAE7854C8186C597B733196EDECE74D28C592E5A7EF9E5' + LineEnding +
    '          DD54038F26F5504964978CC922A1F1A33D7FD27958D38774F55F7F1FF9EC12BD' + LineEnding +
    '          A5DBB20DA4CDF5DFFE859C6D661DBAC307270B33CBADF37A220DCF80CC9D9378' + LineEnding +
    '          1B668CC08D24E8BC597AEDBD99B3CF3EBBDCD16C22C3145C4D0D4D60018D95F8' + LineEnding +
    '          8A032800F25B0246993B63767C600A4240E9A1ACF4B0B3D6966D9425CA9B9730' + LineEnding +
    '          36247578D14D2496C122394840CE5282157DE628BE90A2029F035BE003CD9C11' + LineEnding +
    '          8F98A352D21C73CCE1BECC91A54A0B719BA827F6E916875118949371AA093A6F' + LineEnding +
    '          12345620619DEC3BDDE6556D6667BA8CCDD4D12B99C37E1CFD053957EBAC5D91' + LineEnding +
    '          7C57E0173E33569F616622A8C248189C8F4B2979F1D5C43108B6023F6BB80987' + LineEnding +
    '          24A4E9E0B5DCCB421CDD3EE6986332230BA6D670294D73674D450C5284A727E2' + LineEnding +
    '          DFF50793E23D35A565EC1DE77630A01A32274DC4086E6718D0D503DA63A03953' + LineEnding +
    '          0EE3A47BC41D4C630C980C8A144FC8490FB929BD4CB2786A6CC4DD7339FEF074' + LineEnding +
    '          4292C68A10A54FEEE1B9708CC7B1FCE1BEF9931BB5465AFD05B99A65879DBBE6' + LineEnding +
    '          4292AA54CB3371BC81BFF2A73C835936B3EA36D082BDB791F99366E535444181' + LineEnding +
    '          C5A89C4BB768431620423B54D3B882C0E92ABEC8200E3EF8603EB4CA2AABF463' + LineEnding +
    '          F038A0BAF98E0996C6B4648D0F19C315575C916DF3263A9C019BFA9640658E67' + LineEnding +
    '          9E7926BA0E8D95C31D3199418AE5A9FF831C1F14C8D35268CB9A42F53027B42D' + LineEnding +
    '          6E2089B28C84EDB45463A869E96BC721C72A38B3B320B752AE2BEE9521B27159' + LineEnding +
    '          38C8313983C56CBC96823323F5CFD08F33CAE92E345424E1839540639616A2E5' + LineEnding +
    '          D34E3B0D25F6FB5D5A111793A1746BEDE5496D01AABF2313C4C381BC5476D17B' + LineEnding +
    '          69C93AE52302ADCAAEC6EACA2291A7AB6EE84300F0F657576909725B8DE95929' + LineEnding +
    '          52A93A1806922FDD0A07C693EC890E10BFF0A59882EAF32A7FA7416E06B165FF' + LineEnding +
    '          E34D0720407C958A7AA9A258E7F6270448CC7B3D316B5F534E3965BBF6472812' + LineEnding +
    '          A4B00366D85B6EC95E2861CF9F4C2527B07783F09968A2895A7B16B04934F257' + LineEnding +
    '          053B45A116D71A4C7AB664813C9E60C6ABC40EA7B290031BEDC52C4312D1536C' + LineEnding +
    '          CEC1028C0AC68C12E4975D7699CF66BB1415B429C4EE3CAEAABA014BC2EDC6AC' + LineEnding +
    '          5661471AE2F1B0B4CB2DB7DC52B0EFA097234C60F4CFEB4B142F721384842EAC' + LineEnding +
    '          4BB48BCDDCCE7470AC92F209E7D54D542E693C293217E5770F147928E432B3D9' + LineEnding +
    '          F168CA1C485E444033027F8BC0575FD9617C2D920AA78C54165905E9F4639B25' + LineEnding +
    '          0B00210BF619AFE45A8180C111FF94900727F5AB9B865980A84409921D968905' + LineEnding +
    '          299708EA25E5A14B742B0A3023465C855C387709C84D94FB7A646EE35F1546FA' + LineEnding +
    '          3177EF20E4625B839BDB9328E319E9447E2586D78A4622AEA72A1B6CB099A298' + LineEnding +
    '          3F99089204CC26228BAA68A02CAB8409F2901EFBACB3CE2A3A8B377059AEE34F' + LineEnding +
    '          3E53C28560019F2D500EDC6E9A6A1682B5CF963D7BBCAABF6C934B22F5B3E73A' + LineEnding +
    '          2BFAF54731C412CBE97623F77A0D3B56985271A2D87321603CA3D1A674016399' + LineEnding +
    '          42F5655B162FA61074D605382E1341EC190CE3E0D92C80AD90EE26AA1FC18E61' + LineEnding +
    '          49F984C85C6B301D871C61BAB74088E83C584A2E96AAD4624950A5318FA736E2' + LineEnding +
    '          7E9C58F4123E7132E3656E4A7526B7BE94282C1916759349C193E828BB6510A0' + LineEnding +
    '          29A866836A3518CC9FCA3C02DB57EA8C20A8266014BEB98BA43259BC90797194' + LineEnding +
    '          2AD705891407AD2189CA5583F680AEC50189E824643F7EBA2290BBDCFC180C9B' + LineEnding +
    '          36EFEC527E25E3CA8D282C946066842A5B04A45B064FA557B5110BC0A388814A' + LineEnding +
    '          45456829B0B12497981CC998E509D35285DCC014133D2FE528C6D37486012C73' + LineEnding +
    '          CBCB33E71DF47213074ED4C15D24A6B891CBD2CCF210CCEC795ADBA1668913F3' + LineEnding +
    '          B4583AB664E791705DF92B5760CE2179CC2CDBAEE75E823CF368E23229E5F0B4' + LineEnding +
    '          BC47BD2567F8B7316317736439988D1A3098CBD6AE6C887606003C432C04920B' + LineEnding +
    '          496BD6568EF27B3535BE6E4EF510C8B3CB03B5A48DD9606735FAD9D4612C7F22' + LineEnding +
    '          2C8467440DAAEABB38066075C36C2021FB158C848075E0B674EBBC3EABE31105' + LineEnding +
    '          58BCFB525A62221317D4957A542ACB683B08793DB33572C653A1B816F1E63A94' + LineEnding +
    '          8170AE268FF14A6FA6035B0469068EC77049F565091B1CD2D87457C1F6A8C273' + LineEnding +
    '          514660AB6E5957DC1644C000FE2C05558B56A6D2B5B2446CAC4FBA77E185172E' + LineEnding +
    '          9D63DAFA87659D6C0BDE85D845F46AE9945E2BEB2511A120CF4A1D9775F02267' + LineEnding +
    '          609FB81E4ABBE0820BD884A8514A4F3685569761D8596A2F4425C58757CC9578' + LineEnding +
    '          81DBA2D85DEE71AA02A8FB205754B2ECD1E23642F20D4189DC342AAD91425239' + LineEnding +
    '          CCA607733EB3C0E93928FEE7C1E4AEAB1003A12BCA9284E69D59E0FC2C783BA0' + LineEnding +
    '          2ED501AD348959E437C444657F1286383482ADB1425FCB763C03C35BBC849DB9' + LineEnding +
    '          172AD67F3DE40053134418204FE2679C545B69594C8A6810869DE7A0358BB39E' + LineEnding +
    '          14EAEC0C6C1E8482295996589E18C765AB6C51BA15F589067F653D6A91284D27' + LineEnding +
    '          2DEE5DEB3EC84D3A13AE792B8087893D20B40CC32DD8AF5CDC83157959A68CC5' + LineEnding +
    '          607E18176F437130661FE8CBE103EBAEFEC20037E5ACA805E7577F225228CD0E' + LineEnding +
    '          197EA001AA2F35BEDC4E148CF4438F49BD988B1063AE15FB585EBD62A2EA8D47' + LineEnding +
    '          4B01CE0739A7359E9AEA5B3A17D40379F1F2AA01199B677708E18CD54D336324' + LineEnding +
    '          0B0A4CF5A65A7DCBB55C59B997282EAFD4BBBCB515B9EE831C47A5C8500E6048' + LineEnding +
    '          2770660218DDC7663533A7D61BEA3D89F371506FA3A5266F665DC2ED3834A7B7' + LineEnding +
    '          C8216A24F3E66716D9A45EC8BCFA43B0E9939F8184B3322FF4AE700148BECE41' + LineEnding +
    '          B32D47CF7AA08751621906ECD90D59C450FC1518D12BCC1412C840DCF1D53678' + LineEnding +
    '          CC6C21C473D5434EBA628B40AEC454BF33D3F96C8BC87B36C23643471E548559' + LineEnding +
    '          227EDDA50672924D3434694845C86FF3C773BA0F7222B96C2967C5A8955F968C' + LineEnding +
    '          48E84DA9DCE89350D514B98A1E34EF29D5F1697E20B5C3CF5C07ADA9993073DE' + LineEnding +
    '          C0479DC93B0F20AFFE082475E624FBA07AE4115C9CA2548D51BD67010E3A5957' + LineEnding +
    '          8ABB1A64450B7E406243F0F614CAA5EE8878F32B97326030B366B2540421C13C' + LineEnding +
    '          0BF5E74F06A3FCE25A19A3375E2D114953455926C2583D26EA26BFC9F29A1480' + LineEnding +
    '          E9231EA8ABBC1A8994188DE933DD1268FA74181882C47CBA45781ABBB0CD1FC2' + LineEnding +
    '          EB3EC82926B3466B986E09898855EA779CA66C77F19CBC506016A18B50E2DFF6' + LineEnding +
    '          C72938133EE6C20C8A949E53655EBD82D5D3ED02842936052C49EAC8D59021DB' + LineEnding +
    '          0AF07C91F97362A2DDD76A7CC90F45660F5DD6C72493121B5A816862102CC980' + LineEnding +
    '          659E36BCAA1DE5872A5094C11065517F0C9122913539598EEA57032887C05FDA' + LineEnding +
    '          E0AD167F9654B706A3679657EDD3E7D2AD94B2F4D9F82B06DD07797646CB8E0C' + LineEnding +
    '          1484A5B6C05F7995128D27745E4E5CD61B90557E0382E566F37939AC6BE18954' + LineEnding +
    '          A1752521C17BB81418A83ECD609F8C510E19A4CB51DDFB6006B3D901CD005B16' + LineEnding +
    '          2EC5E2C18E6A7924B11F2DE103F6E74FE02FB4CF50D4C20CA6F1B77924268C15' + LineEnding +
    '          91B0C5EACFDB999FECE62B1BFD6A621CB2EC9F1F49E83EC8156404D7C2CF9E93' + LineEnding +
    '          C4C5C0FCD267A640F8F06F111447657E796AF68DF021CE2DE2A2744BA2A48A66' + LineEnding +
    '          20C78A9C5B334E6F8E383185A565764B4AE50BCB89CA55C8ABF25008F7272564' + LineEnding +
    '          184707F857C88CDDE0619CA17E676D2A270D4CA9C7671184AF070F8C259C3B4A' + LineEnding +
    '          78C6670964F9E5C234637FF92C46A031C8F10477A7AE0BAE78DED2AA78E1BC93' + LineEnding +
    '          AC3666E4DAA4E0BA2DBF85A7B73CA3666DAE94E716DD07798DA9FA8AE181C714' + LineEnding +
    '          7C9656A268CB006C3F356D2E8B480B661E4905CA0626B196EB9B11011527F336' + LineEnding +
    '          005000AA10AA5788C15244AA3A5CBF1474CBEF8116E0056037C52BEC4C00869F' + LineEnding +
    '          036D1007643F932209758B3979B3C6B26A4E4921CA00A162E46E917D7690C031' + LineEnding +
    '          86EAF041666524126840BA90A2C40AE2AE60C1808C96CE4224CC2575D37AC859' + LineEnding +
    '          8321B160B750AE71098F775252E74257E9D91818A262B0718A807A36986A5A58' + LineEnding +
    '          3FE13D0F7908ADA44F0C168A28575845CE9C49DA46E0903900A67E4DA2896316' + LineEnding +
    '          822B305C4B4CC1898EE3A956C1E397783E2B28BC56AC8D67E89CCE472A4C84BF' + LineEnding +
    '          62946C5E3347F64D2009DA4AF22DB12182947758836BABF5194623064BFCF4AC' + LineEnding +
    '          2E96DFA6D26120676A8CD2E18381E17FE7291223078C60615905E4E448C497A4' + LineEnding +
    '          40330300673DE446EE24E7760BC11B9CAC4D9F4CCA9902B9BC06D890761746A0' + LineEnding +
    '          9909690DE972BE27BDBCB5C1714D1E208962C262336E27C4503AC5E7B37425B9' + LineEnding +
    '          99F945A4D93FC917E9AC14C6F32FB697E1701413A125FD5574800F74387D6482' + LineEnding +
    '          B8352DED2B611C21D60FC52BD94D27C83F1B264B32494B27FCC3155AFE84B4E0' + LineEnding +
    '          8AA8008F87118F3F410B3C20F7AF4EC0CF64B5D7924D675306F7859FAF1AEB87' + LineEnding +
    '          3503D5836B9F059202B936AEF55719B958401D37B29FA037429E79A7B9AC70C8' + LineEnding +
    '          88A4284A4B8437494C99DBD628F3C91614338EF7B0A8D0C829C545FEA44646DC' + LineEnding +
    '          11DBF83F4BDD5CD9228A8418EBA241F3CBB7A40618981949F4D16F6B9B59AB76' + LineEnding +
    '          A974083C1006788C5D927E0326FDE41D3E80411B778CFE127D840671414E98F2' + LineEnding +
    '          5CC2B9E1398F54E80C5039B275C95DF25533FFEA50ECD3866A31660F2B663365' + LineEnding +
    '          CDF4EF7C22A39BEAADB5FA7FF5297A29E41922CE2755A2BA73862AF6D83C8640' + LineEnding +
    '          F39597B07D1BE001C075D884FC983443861E5E5587D6538BE59D8285F75A30B6' + LineEnding +
    '          69CD0F4449BA04E6EC3D6AFCA7FCC14034E06451A0B5FD1ADAD4EF556A936FDB' + LineEnding +
    '          6C40EBE186FEFF19BB5E0D79FD2C08D8689C39237C25AABC73A42A92FA283E4C' + LineEnding +
    '          E07790D0C22A8F47EF916CA81BFFAB69C809FD150138C9085849E3DB2E322421' + LineEnding +
    '          A05D3F66D1269CDDD9A0C920CFB6433C4F8BC9C7B835D9255F5293A2CFAB4BAB' + LineEnding +
    '          8AF6ECC01992306FC6E070799DEA4D9A890E9C9B110C68FF414B93419E6D1404' + LineEnding +
    '          0B4E56D7C4FCEAB2D9584DB7E3008ACC7A060547AF616C411AAB6B2C2114E915' + LineEnding +
    '          D4AC404775D3FC6A2FEDFDDD9BEE74C72EBA5793418E84B3844A9D2267A15DB0' + LineEnding +
    '          A7806099B5191B57783C87A67B295BBB044574DCCEA7093A4A878968A613F2B0' + LineEnding +
    '          8BE6B49777DB64904B972DBA270793B128A316792F1DE2CD322EBB7F3834B54F' + LineEnding +
    '          AFB1069F295B7A3ED957DE7B92B8F77260BA6E784D06B96219662E1BD3D455E4' + LineEnding +
    '          A6C41AD5262FB2B0A69446482BD40315CC0E8D4BF69C5FF0C70403F2FFC3D364' + LineEnding +
    '          90B37DAB2F653B5BB5BA920A898C0EAEEA1BBEFA10499F9DC859AB96B639D915' + LineEnding +
    '          EF6D779D5F766ECFCD0739B45419CBAFBA5651579125E2728646931C67ABABA2' + LineEnding +
    '          BD0C5EC286F3154A1BF9DD83CE9DE55ED55BF3416EFA54C2F362A988DEDAAF04' + LineEnding +
    '          675F6C5E55A4DE156A5455ADD9B4B983A057C1D31583694AC84D84426CBC5982' + LineEnding +
    '          9E7DE65986C9495549056D9579491D6D7FE49147AAB9663DBE2B26B1B9FA6C56' + LineEnding +
    '          C869F5A00B69F0E7E7626878304BCDF3F3BF90B6598CA0537C1DE8DCC52E9B15' + LineEnding +
    '          F2BC9BA2BE468E2998ABAB676BACBD500A2C30A6D5657403FF73FB7A066A56C8' + LineEnding +
    '          1558E2E56A2CD69430B6324B76BEF6C99F64ECC4D8D1AC9097DFFA29BBFF2D8D' + LineEnding +
    '          E4071E5BDC10DD8953D6EC5D3525E4F93F38A464364AD86D02032F31A9B0DAB5' + LineEnding +
    '          A292AAE4DEECA874E9F89B0F726BA02A6EDE79B0F5C0121975A6941692CF5B45' + LineEnding +
    '          ADFD387797CE631375DE7C904BCAF3EBF9E2B79534870D27E555720BA9D5B72C' + LineEnding +
    '          9B08896E1B6AF3415E3F3594797EFCD576391B6606E663FDB69EBE00B927847A' + LineEnding +
    '          363F35EF6695815EFEFF339097F40702D98906F17F6AC5D4AA57B406FD000000' + LineEnding +
    '          0049454E44AE426082' + LineEnding +
    '        }' + LineEnding +
    '        Material.Texture.Disabled = False' + LineEnding +
    '      end>' + LineEnding +
    '    left = 74' + LineEnding +
    '    top = 88' + LineEnding +
    '  end' + LineEnding +
    '  object GLWindowsBitmapFont1: TGLWindowsBitmapFont' + LineEnding +
    '    Font.CharSet = DEFAULT_CHARSET' + LineEnding +
    '    Font.Color = clWhite' + LineEnding +
    '    Font.Height = -32' + LineEnding +
    '    Font.Name = ''Verdana''' + LineEnding +
    '    Font.Pitch = fpVariable' + LineEnding +
    '    Font.Quality = fqDraft' + LineEnding +
    '    left = 112' + LineEnding +
    '    top = 88' + LineEnding +
    '  end' + LineEnding +
    'end' + LineEnding;

end;

initialization

finalization


end.

