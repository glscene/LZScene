{: This sample mixes two textures by using multitexturing.<p>

   Multitexturing requires at least two materials in the material library:<ul>
   <li>a base material: determines the basic properties, incl. blending, colors
      and lighting-related properties
   <li>a second material: determines the second texture (color and other
      properties are ignored)
   </ul><p>

   This structure allows reuse of second textures among a variety of materials,
   this is particularly usefull for details maps, which are usually just "noise"
   to be applied on different base textures. You can also use it to reuse a basic
   standard lighting map throughout many objects, thus reducing texture memory
   needs (many shadows can be derived from a few deformed basic maps).<p>

   The texture matrix (scale, offset) are adjusted independantly for the two
   textures, in this sample, the TrackBar adjusts an isotropic scaling.<p>

   When multi-texturing, never forget that both texture modes (decal, modulate etc.)
   are honoured. For instance, if you "Decal" a non-transparent second texture,
   the base texture will be completely replaced!
}
unit Main;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLObjects, GLTexture, ComCtrls, StdCtrls, ExtCtrls,
  ExtDlgs, GLLCLViewer, GLMaterial, GLCoordinates, GLCrossPlatform,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    Plane1: TGLPlane;
    GLCamera1: TGLCamera;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    TrackBar1: TTrackBar;
    Label4: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    CBClampTex2: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure CBClampTex2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  GLUtils;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  // prepare images to merge in the multitexture
  with GLMaterialLibrary1 do
  begin
    Image1.Picture.LoadFromFile(MediaPath+'ashwood.jpg');
    Materials[0].Material.Texture.Image.Assign(Image1.Picture);
    Image2.Picture.LoadFromFile(MediaPath+'Flare1.bmp');
    Materials[1].Material.Texture.Image.Assign(Image2.Picture);
  end;
end;

procedure TForm1.Image1Click(Sender: TObject);
begin
  // load a new Image1
  if OpenPictureDialog1.Execute then
  begin
    Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    GLMaterialLibrary1.Materials[0].Material.Texture.Image.Assign(Image1.Picture);
  end;
end;

procedure TForm1.Image2Click(Sender: TObject);
begin
  // load a new Image2
  if OpenPictureDialog1.Execute then
  begin
    Image2.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    GLMaterialLibrary1.Materials[1].Material.Texture.Image.Assign(Image2.Picture);
  end;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  // adjust scale
  with GLMaterialLibrary1.Materials[1].TextureScale do
  begin
    X := TrackBar1.Position / 10;
    Y := TrackBar1.Position / 10;
  end;
end;

procedure TForm1.CBClampTex2Click(Sender: TObject);
begin
  with GLMaterialLibrary1.Materials[1].Material.Texture do
    if CBClampTex2.Checked then
      TextureWrap := twNone
    else
      TextureWrap := twBoth;
end;

end.

