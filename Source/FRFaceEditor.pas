//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Editor fram for a TGLFaceProperties.
}
unit FRFaceEditor;

interface

{$I GLScene.inc}

uses
  lresources,
  Forms, 
  ComCtrls, 
  StdCtrls, 
  Controls,  
  Classes,
  Graphics,
  GLTexture, 
  FRTrackBarEdit,
  FRColorEditor,
  GLMaterial;

type
  TRFaceEditor = class(TFrame)
    PageControl: TPageControl;
    TSAmbient: TTabSheet;
    TSDiffuse: TTabSheet;
    TSEmission: TTabSheet;
    TSSpecular: TTabSheet;
    CEAmbiant: TRColorEditor;
    Label1: TLabel;
    TBEShininess: TRTrackBarEdit;
    ImageList: TImageList;
    CEDiffuse: TRColorEditor;
    CEEmission: TRColorEditor;
    CESpecular: TRColorEditor;
    procedure TBEShininessTrackBarChange(Sender: TObject);

  private
    
    FOnChange : TNotifyEvent;
    updating : Boolean;
    FFaceProperties : TGLFaceProperties;
    procedure SetGLFaceProperties(const val : TGLFaceProperties);
    procedure OnColorChange(Sender : TObject);

  public
    
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property OnChange : TNotifyEvent read FOnChange write FOnChange;
	 property FaceProperties : TGLFaceProperties read FFaceProperties write SetGLFaceProperties;
  end;

//=======================================================
implementation
//=======================================================


constructor TRFaceEditor.Create(AOwner : TComponent);
begin
   inherited;
   FFaceProperties:=TGLFaceProperties.Create(nil);
   CEAmbiant.OnChange:=OnColorChange;
   CEDiffuse.OnChange:=OnColorChange;
   CEEmission.OnChange:=OnColorChange;
   CESpecular.OnChange:=OnColorChange;
   PageControl.DoubleBuffered:=True;
end;

destructor TRFaceEditor.Destroy;
begin
   FFaceProperties.Free;
   inherited;
end;

procedure TRFaceEditor.OnColorChange(Sender : TObject);
var
   bmp : TBitmap;
   bmpRect : TRect;

   procedure AddBitmapFor(ce : TRColorEditor);
   begin
      with bmp.Canvas do begin
         Brush.Color:=ce.PAPreview.Color;
         FillRect(bmpRect);
      end;
      ImageList.Add(bmp, nil);
   end;

begin
   if not updating then begin
      // Update imageList
      bmp:=TBitmap.Create;
      try
         bmp.Width:=16;
         bmp.Height:=16;
         bmpRect:=Rect(0, 0, 16, 16);
         ImageList.Clear;
         AddBitmapFor(CEAmbiant);
         FFaceProperties.Ambient.Color:=CEAmbiant.EditedColor;
         AddBitmapFor(CEDiffuse);
         FFaceProperties.Diffuse.Color:=CEDiffuse.EditedColor;
         AddBitmapFor(CEEmission);
         FFaceProperties.Emission.Color:=CEEmission.EditedColor;
         AddBitmapFor(CESpecular);
         FFaceProperties.Specular.Color:=CESpecular.EditedColor;
      finally
         bmp.Free;
      end;
      // Trigger onChange
      if Assigned(FOnChange) then FOnChange(Self);
   end;
end;

procedure TRFaceEditor.TBEShininessTrackBarChange(Sender: TObject);
begin
   if not updating then begin
      TBEShininess.TrackBarChange(Sender);
      FFaceProperties.Shininess:=TBEShininess.Value;
      if Assigned(FOnChange) then FOnChange(Self);
   end;
end;

// SetGLFaceProperties
//
procedure TRFaceEditor.SetGLFaceProperties(const val : TGLFaceProperties);
begin
   updating:=True;
   try
      CEAmbiant.EditedColor:=val.Ambient.Color;
      CEDiffuse.EditedColor:=val.Diffuse.Color;
      CEEmission.EditedColor:=val.Emission.Color;
      CESpecular.EditedColor:=val.Specular.Color;
      TBEShininess.Value:=val.Shininess;
   finally
      updating:=False;
   end;
   OnColorChange(Self);
   TBEShininessTrackBarChange(Self);
end;


initialization

  {$I FRFaceEditor.lrs}


end.



