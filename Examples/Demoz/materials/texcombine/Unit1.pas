{: TexCombineShader demo / mini-lab.<p>

   This is an advanced demo, which showcases use and setup of extra texture
   units, along with texture combination possibilities.<p>

   The texture combiner allows to declare how each texture unit should
   be used, and how each should be combined with the others. Basicly,
   a texture combiner "code" defines how each texture should be combined,
   knowing that the result of the last texture unit (the one with the higher
   index) defines the final output.<br>
   Note that if the code allows you to declare the combiners in any order,
   the hardware will evaluate them in their index order, and will only accept
   one combiner assignement for each texture unit.
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLTexture, GLObjects, StdCtrls, ExtCtrls,
  GLLCLViewer, GLTexCombineShader, GLHUDObjects, GLMaterial,
  GLCoordinates, GLCrossPlatform, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene: TGLScene;
    SceneViewer: TGLSceneViewer;
    Image1: TImage;
    Image2: TImage;
    BUApply: TButton;
    GLCamera: TGLCamera;
    GLDummyCube: TGLDummyCube;
    GLMaterialLibrary: TGLMaterialLibrary;
    Image3: TImage;
    Label1: TLabel;
    Image4: TImage;
    GLTexCombineShader: TGLTexCombineShader;
    GLHUDSprite: TGLHUDSprite;
    PATex1: TPanel;
    PATex2: TPanel;
    PATex3: TPanel;
    CBTex0: TCheckBox;
    CBTex1: TCheckBox;
    CBTex2: TCheckBox;
    CBTex3: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    MECombiner: TMemo;
    Label2: TLabel;
    ColorDialog: TColorDialog;
    PAPrimary: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure BUApplyClick(Sender: TObject);
    procedure SceneViewerPostRender(Sender: TObject);
    procedure CBTex0Click(Sender: TObject);
    procedure PAPrimaryClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses GLUtils;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  with GLMaterialLibrary.Materials do
  begin
    Image1.Picture.LoadFromFile(MediaPath+'beigemarble.jpg');
    Items[0].Material.Texture.Image.Assign(Image1.Picture);
    Image2.Picture.LoadFromFile(MediaPath+'Flare1.bmp');
    Items[1].Material.Texture.Image.Assign(Image2.Picture);
    Image3.Picture.LoadFromFile(MediaPath+'clover.jpg');
    Items[2].Material.Texture.Image.Assign(Image3.Picture);
    Image4.Picture.LoadFromFile(MediaPath+'cm_front.jpg');
    Items[3].Material.Texture.Image.Assign(Image4.Picture);
  end;
  GLTexCombineShader.Combiners.Assign(MECombiner.Lines);
  Application.HintHidePause := 30000;
end;

procedure TForm1.BUApplyClick(Sender: TObject);
begin
  // Apply new combiner code
  // Depending on shader and hardware, errors may be triggered during render
  GLTexCombineShader.Combiners.Assign(MECombiner.Lines);
end;

procedure TForm1.SceneViewerPostRender(Sender: TObject);
var
  n: integer;
begin
  // disable whatever texture units are not supported by the local hardware
  n := SceneViewer.Buffer.LimitOf[limNbTextureUnits];
  PATex1.Visible := (n < 2);
  CBTex1.Enabled := (n >= 2);
  PATex2.Visible := (n < 3);
  CBTex2.Enabled := (n >= 3);
  PATex3.Visible := (n < 4);
  CBTex3.Enabled := (n >= 4);
  CBTex1.Checked := CBTex1.Checked and CBTex1.Enabled;
end;

procedure TForm1.CBTex0Click(Sender: TObject);
var
  libMat: TGLLibMaterial;
begin
  // This event is used for all 4 checkboxes of the 4 texture units
  libMat := GLMaterialLibrary.Materials.GetLibMaterialByName(
    (Sender as TCheckBox).Caption);
  if Assigned(libMat) then
    libMat.Material.Texture.Enabled := TCheckBox(Sender).Checked;
end;

procedure TForm1.PAPrimaryClick(Sender: TObject);
begin
  // Allow choosing the primary color
  ColorDialog.Color := PAPrimary.Color;
  if ColorDialog.Execute then
  begin
    PAPrimary.Color := ColorDialog.Color;
    with GLMaterialLibrary.Materials[0].Material.FrontProperties do
      Diffuse.AsWinColor := ColorDialog.Color;
    SceneViewer.Invalidate;
  end;
end;

end.

