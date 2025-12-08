{: Procedural Texture Demo / Tobias Peirick }
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, GLScene, GLObjects, GLTexture, GLHUDObjects,
  GLCadencer, GLLCLViewer, GLProcTextures, Spin, GLCoordinates,
  GLCrossPlatform, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    Panel1: TPanel;
    Label1: TLabel;
    CBFormat: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    CBCompression: TComboBox;
    Label5: TLabel;
    RBDefault: TRadioButton;
    RBDouble: TRadioButton;
    LAUsedMemory: TLabel;
    RBQuad: TRadioButton;
    LARGB32: TLabel;
    LACompression: TLabel;
    GLCadencer1: TGLCadencer;
    CheckBox1: TCheckBox;
    Label4: TLabel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Label6: TLabel;
    CheckBox2: TCheckBox;
    GLPlane1: TGLPlane;
    procedure GLSceneViewer1AfterRender(Sender: TObject);
    procedure CBFormatChange(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    newSelection: boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses GLTextureFormat;

procedure TForm1.GLSceneViewer1AfterRender(Sender: TObject);
var
  rgb: integer;
begin
  // update compression stats, only the 1st time after a new selection
  if newSelection then
    with GLPlane1.Material.Texture do
    begin
      rgb := Image.Width * Image.Height * 4;
      LARGB32.Caption := Format('RGBA 32bits would require %d kB', [rgb div 1024]);
      LAUsedMemory.Caption := Format('Required memory : %d kB',
        [TextureImageRequiredMemory div 1024]);
      LACompression.Caption := Format('Compression ratio : %d %%',
        [100 - 100 * TextureImageRequiredMemory div rgb]);
      newSelection := False;
    end;
end;

procedure TForm1.CBFormatChange(Sender: TObject);
begin
  // adjust settings from selection and reload the texture map
  with GLPlane1.Material.Texture do
  begin
    TextureFormat := TGLTextureFormat(integer(tfRGB) + CBFormat.ItemIndex);
    Compression := TGLTextureCompression(integer(tcNone) + CBCompression.ItemIndex);
    TGLProcTextureNoise(Image).MinCut := SpinEdit1.Value;
    TGLProcTextureNoise(Image).NoiseSharpness := SpinEdit2.Value / 100;
    TGLProcTextureNoise(Image).Seamless := CheckBox2.Checked;
    if RBDefault.Checked then
    begin
      GLPlane1.Width := 50;
      GLPlane1.Height := 50;
    end
    else if RBDouble.Checked then
    begin
      GLPlane1.Width := 100;
      GLPlane1.Height := 100;
    end
    else
    begin
      GLPlane1.Width := 400;
      GLPlane1.Height := 400;
    end;
  end;
  newSelection := True;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  if CheckBox1.Checked then
    TGLProcTextureNoise(GLPlane1.Material.Texture.Image).NoiseAnimate(deltaTime);
end;

end.

