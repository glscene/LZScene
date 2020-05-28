{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLScene_DesignTime;

{$warn 5023 off : no warning about unused units}
interface

uses
  FXCollectionEditor, GLSceneRegister, GLObjectManager, FLibMaterialPicker, 
  FMaterialEditorForm, FRColorEditor, FRFaceEditor, FRMaterialPreview, 
  FRTextureEdit, FRTrackBarEdit, FVectorEditor, FInfo, FGUILayoutEditor, 
  FGLSceneEdit, FShaderUniformEditor, FGUISkinEditor, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GLSceneRegister', @GLSceneRegister.Register);
end;

initialization
  RegisterPackage('GLScene_DesignTime', @Register);
end.
