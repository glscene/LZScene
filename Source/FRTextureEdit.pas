//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Basic editing frame for TGLTexture
}
{ TODO : Replace STImageClass with a dropdown (polymorphism) }

unit FRTextureEdit;

interface

{$I GLScene.inc}

uses
  lresources, 
  SysUtils,
  Forms, 
  StdCtrls, 
  Buttons, 
  Controls, 
  Classes, 
  TypInfo,
  GLTexture, 
  GLGraphics;

type

  TRTextureEdit = class(TFrame)
    CBFilteringQuality: TComboBox;
    Label2: TLabel;
    Label7: TLabel;
    SBEditImage: TSpeedButton;
    CBMagFilter: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    CBMinFilter: TComboBox;
    CBTextureMode: TComboBox;
    Label1: TLabel;
    Label5: TLabel;
    CBTextureWrap: TComboBox;
    CBDisabled: TCheckBox;
    CBImageClass: TComboBox;
    CBImageAlpha: TComboBox;
    Label6: TLabel;
    procedure CBFilteringQualityChange(Sender: TObject);
    procedure CBMagFilterChange(Sender: TObject);
    procedure CBMinFilterChange(Sender: TObject);
    procedure CBTextureModeChange(Sender: TObject);
    procedure CBTextureWrapChange(Sender: TObject);
    procedure CBDisabledClick(Sender: TObject);
    procedure SBEditImageClick(Sender: TObject);
    procedure CBImageClassChange(Sender: TObject);
    procedure CBImageAlphaChange(Sender: TObject);
  private
    FTexture: TGLTexture;
    FOnChange: TNotifyEvent;
    changeing: boolean;
  protected
    procedure SetTexture(const val: TGLTexture);
    procedure DoOnChange; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Texture: TGLTexture read FTexture write SetTexture;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

  end;

//======================================================
implementation
//======================================================


uses
  GLTextureImageEditors, GLTextureFormat;

constructor TRTextureEdit.Create(AOwner: TComponent);
var
  I: integer;
begin
  inherited;
  FTexture := TGLTexture.Create(Self);
  SetTexture(FTexture);
  SetGLTextureImageClassesToStrings(CBImageClass.Items);

  for i := 0 to integer(High(TGLTextureImageAlpha)) do
    CBImageAlpha.Items.Add(GetEnumName(TypeInfo(TGLTextureImageAlpha), i));
  for i := 0 to integer(High(TGLMagFilter)) do
    CBMagFilter.Items.Add(GetEnumName(TypeInfo(TGLMagFilter), i));
  for i := 0 to integer(High(TGLMinFilter)) do
    CBMinFilter.Items.Add(GetEnumName(TypeInfo(TGLMinFilter), i));
  for i := 0 to Integer(High(TGLTextureFilteringQuality)) do
    CBFilteringQuality.Items.Add(GetEnumName(TypeInfo(TGLTextureFilteringQuality), i));
  for i := 0 to integer(High(TGLTextureMode)) do
    CBTextureMode.Items.Add(GetEnumName(TypeInfo(TGLTextureMode), i));
  for i := 0 to integer(High(TGLTextureWrap)) do
    CBTextureWrap.Items.Add(GetEnumName(TypeInfo(TGLTextureWrap), i));
end;

// Destroy

destructor TRTextureEdit.Destroy;
begin
  FTexture.Free;
  inherited;
end;

// SetTexture

procedure TRTextureEdit.SetTexture(const val: TGLTexture);
begin
  FTexture.Assign(val);
  changeing := True;
  try
    with CBImageClass do
      ItemIndex := Items.IndexOfObject(Pointer(FTexture.Image.ClassType));
    CBImageAlpha.ItemIndex := integer(FTexture.ImageAlpha);
    CBMagFilter.ItemIndex := integer(FTexture.MagFilter);
    CBMinFilter.ItemIndex := integer(FTexture.MinFilter);
    CBFilteringQuality.ItemIndex:=Integer(FTexture.FilteringQuality);
    CBTextureMode.ItemIndex := integer(FTexture.TextureMode);
    CBTextureWrap.ItemIndex := integer(FTexture.TextureWrap);
    CBDisabled.Checked := FTexture.Disabled;
  finally
    changeing := False;
    DoOnChange;
  end;
end;

// DoOnChange

procedure TRTextureEdit.DoOnChange;
begin
  if (not changeing) and Assigned(FOnChange) then
    OnChange(Self);
end;

// CBImageClassChange

procedure TRTextureEdit.CBImageClassChange(Sender: TObject);
var
  tic: TGLTextureImageClass;
  ti: TGLTextureImage;
begin
  if not changeing then
  begin
    with CBImageClass do
      tic := TGLTextureImageClass(Items.Objects[ItemIndex]);
    if FTexture.Image.ClassType <> tic then
    begin
      ti := TGLTextureImageClass(tic).Create(FTexture);
      FTexture.Image := ti;
      ti.Free;
    end;
    DoOnChange;
  end;
end;

// CBImageAlphaChange

procedure TRTextureEdit.CBImageAlphaChange(Sender: TObject);
begin
  FTexture.ImageAlpha := TGLTextureImageAlpha(CBImageAlpha.ItemIndex);
  DoOnChange;
end;

// CBMagFilterChange

procedure TRTextureEdit.CBMagFilterChange(Sender: TObject);
begin
  FTexture.MagFilter := TGLMagFilter(CBMagFilter.ItemIndex);
  DoOnChange;
end;

procedure TRTextureEdit.CBFilteringQualityChange(Sender: TObject);
begin
  FTexture.FilteringQuality:=TGLTextureFilteringQuality(CBFilteringQuality.ItemIndex);
  DoOnChange;
end;

// CBMinFilterChange

procedure TRTextureEdit.CBMinFilterChange(Sender: TObject);
begin
  FTexture.MinFilter := TGLMinFilter(CBMinFilter.ItemIndex);
  DoOnChange;
end;

// CBTextureModeChange

procedure TRTextureEdit.CBTextureModeChange(Sender: TObject);
begin
  FTexture.TextureMode := TGLTextureMode(CBTextureMode.ItemIndex);
  DoOnChange;
end;

// CBTextureWrapChange

procedure TRTextureEdit.CBTextureWrapChange(Sender: TObject);
begin
  FTexture.TextureWrap := TGLTextureWrap(CBTextureWrap.ItemIndex);
  DoOnChange;
end;

// CBDisabledClick

procedure TRTextureEdit.CBDisabledClick(Sender: TObject);
begin
  FTexture.Disabled := CBDisabled.Checked;
  DoOnChange;
end;

// SBEditImageClick

procedure TRTextureEdit.SBEditImageClick(Sender: TObject);
begin
  EditGLTextureImage(FTexture.Image);
  DoOnChange;
end;


initialization

  {$I FRTextureEdit.lrs}

end.

