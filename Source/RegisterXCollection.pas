//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Register TGLXCollection property editor

	 History :  
       20/05/10 - Yar - Fixes for Linux x64
       11/11/09 - DaStr - Improved FPC compatibility
                             (thanks Predator) (BugtrackerID = 2893580)
       03/07/04 - LR - Removed ..\ from the GLScene.inc
       16/04/00 - Egg - Creation
	 
}
unit RegisterXCollection;

interface

{$I GLScene.inc}

uses
  Classes, GLXCollection,
     componenteditors, propedits;

type

	// TGLXCollectionProperty
	//
	TGLXCollectionProperty = class(TClassProperty)
		public
			 
			function GetAttributes: TPropertyAttributes; override;
			procedure Edit; override;
	end;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
uses FXCollectionEditor;


procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TGLXCollection), nil, '', TGLXCollectionProperty);
end;

//----------------- TGLXCollectionProperty ------------------------------------

// GetAttributes
//
function TGLXCollectionProperty.GetAttributes: TPropertyAttributes;
begin
	Result:=[paDialog];
end;

// Edit
//
procedure TGLXCollectionProperty.Edit;
begin
   with XCollectionEditor do begin
      SetXCollection(TGLXCollection(GetObjectValue));
      Show;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   
end.
