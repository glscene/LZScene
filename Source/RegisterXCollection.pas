//
// The graphics engine GLXEngine. The unit of LZScene for Lazarus
//
{
   Register TGLXCollection property editor
}
unit RegisterXCollection;

interface

{$I GLScene.inc}

uses
  Classes, 
  
  GLXCollection,
  componenteditors, 
  propedits;

type

	TGLXCollectionProperty = class(TClassProperty)
		public
			 
			function GetAttributes: TPropertyAttributes; override;
			procedure Edit; override;
	end;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

uses 
  FXCollectionEditor;


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
