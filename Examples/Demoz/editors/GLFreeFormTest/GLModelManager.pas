unit GLModelManager;

(*
 * Author : Blaise Bernier
 * Date   : 08/12/2003
 *
 ******************************************************************************
 * Description :
 *    The GLModelManager is a class that provides an easy way to manage
 *    models and master objects of proxies at runtime.  You just need to
 *    have the model filename and it will load it into the list or return
 *    the already existing model.
 *
 *
 *)

interface

uses GLScene, GLObjects, GLVectorFileObjects, SysUtils, Classes,
     GLFile3DS, GLFileOBJ;

type

   (****************************************
      TGLModelManager - class definition
   ****************************************)
   TGLModelManager = class
   private
      { Private fields }
      FMasterObject : TGLDummyCube;
      FModelList    : TStringList;
      FModelPath    : string;

      { Get/Set methods }
      procedure SetModelPath(const Value: String);
   public
      { Constructor / Destructor }
      constructor Create(AMaster : TGLDummyCube; APath : string); virtual;
      destructor Destroy; override;

      { Methods }
      function LoadModel(AFilename : string): TGLFreeForm;

      { Properties }
      property MasterObject : TGLDummyCube read FMasterObject;
      property ModelList : TStringList read FModelList;
      property Path : string read FModelPath write SetModelPath;
   end;


implementation

(****************************************
         { TGLModelManager }
****************************************)

(**********************************
   Name        : Create
   Description :
      This is the constructor.  It will create the list, assign the path to the
      models and the master object where the new models will be loaded.
*)
constructor TGLModelManager.Create(AMaster: TGLDummyCube; APath : string);
begin
   // Set the master object
   FMasterObject := AMaster;
   // Create the model list
   FModelList := TStringList.Create;
   FModelList.CaseSensitive := false;
   FModelList.Sorted := true;
   // Set the path to the models
   SetModelPath(APath);
end;

(**********************************
   Name        : Destroy
   Description :
      This is the destructor.  It will free all the loaded models.
*)
destructor TGLModelManager.Destroy;
var
   i : integer;
begin
   // Destroy every models
   for i:=0 to Pred(FModelList.Count) do
      FModelList.Objects[i].Destroy;
   // Destroy the list
   FModelList.Destroy;
   inherited;
end;

(**********************************
   Name        : LoadModel
   Description :
      It will load a new model if it's not in the list and then return the new
      freeform.  If it,s already in the list, it will return the existing
      freeform.
*)
function TGLModelManager.LoadModel(AFilename: string): TGLFreeForm;
var
   I           : integer;
   NewFreeForm : TGLFreeForm;
begin

   with FModelList do
   begin
      if Find(AFilename, I) then
         Result := TGLFreeForm(Objects[I])
      else
      begin
         NewFreeForm := TGLFreeForm(FMasterOBject.AddNewChild(TGLFreeForm));
         NewFreeForm.LoadFromFile(FModelPath + AFilename);
         FModelList.AddObject(AFilename,NewFreeForm);
         Result := NewFreeForm;
      end;
   end;

end;

(**********************************
   Name        : SetModelPath
   Description :
      It will change the path to the models and refresh the
      already existing freeforms
*)
procedure TGLModelManager.SetModelPath(const Value: String);
var
   Len : integer;
   i   : integer;
begin
   // Set the path
   FModelPath := Value;

   Len := Length(Value);

   // Correct it if there is no '\'
   if (Len > 0) then
      if (Value[Len - 1] <> '\') then
         FModelPath := Value + '\';

   // Reload the models
   for i := 0 to Pred(FModelList.Count) do
      TGLFreeForm(FModelList.Objects[i]).LoadFromFile(FModelPath + FModelList.Strings[i]);
end;

end.
