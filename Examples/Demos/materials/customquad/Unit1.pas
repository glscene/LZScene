{: Using materials in a TGLDirectOpenGL OnRender.<p>

   This demo shows how to dynamically create materials in a material library
   and use them in a TGLDirectOpenGL to render your own stuff.<br>
   The render is quite simple: two quads, each with its own texture. The
   TGLDirectOpenGL is placed in a small hierarchy with a torus and dummy cube,
   and the rotation animation are handled by those two object to show that
   the OnRender code uses the hierarchy.

  <b>History : </b><font size=-1><ul>
      <li>21/04/10 - Yar - Removed direct state changing
    </ul></font>
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLCadencer, GLScene, GLObjects, GLTexture, GLBehaviours,
  GLLCLViewer, GLGeomObjects, GLColor, GLCrossPlatform, GLMaterial,
  GLCoordinates, GLBaseClasses, GLRenderContextInfo, GLSimpleNavigation;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    GLSimpleNavigation1: TGLSimpleNavigation;
    Torus1: TGLTorus;
    DirectOpenGL1: TGLDirectOpenGL;
    GLLightSource1: TGLLightSource;
    GLCadencer1: TGLCadencer;
    procedure DirectOpenGL1Render(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses OpenGLTokens, GLContext, GLUtils;

procedure TForm1.FormCreate(Sender: TObject);
begin
   SetGLSceneMediaDir();
  // dynamically create 2 materials and load 2 textures
  with GLMaterialLibrary do
  begin
    with AddTextureMaterial('wood', MediaPath+'ashwood.jpg') do
    begin
      Material.FrontProperties.Emission.Color := clrGray50;
      Material.FaceCulling := fcNoCull;
    end;
    with AddTextureMaterial('stone', 'walkway.jpg') do
    begin
      Material.FrontProperties.Emission.Color := clrGray50;
      Material.FaceCulling := fcNoCull;
    end;
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.DirectOpenGL1Render(Sender: TObject; var rci: TGLRenderContextInfo);
var
  material: TGLLibMaterial;
begin
  // 1st quad, textured with 'wood', using standard method
  GLMaterialLibrary.ApplyMaterial('wood', rci);
  GL.Begin_(GL_QUADS);
  GL.TexCoord2f(0, 1);
  GL.Vertex3f(0.5, 0.5, -0.5);
  GL.TexCoord2f(0, 0);
  GL.Vertex3f(-0.5, 0.5, -0.5);
  GL.TexCoord2f(1, 0);
  GL.Vertex3f(-0.5, 0, 0.5);
  GL.TexCoord2f(1, 1);
  GL.Vertex3f(0.5, 0, 0.5);
  GL.End_;
  GLMaterialLibrary.UnApplyMaterial(rci);
  // 2nd quad, textured with 'stone'
  // we "manually" apply the material, this can be usefull if you want to have
  // some dynamic material control
  material := GLMaterialLibrary.Materials.GetLibMaterialByName('stone');
  material.Material.Apply(rci);
  GL.Begin_(GL_QUADS);
  GL.TexCoord2f(0, 1);
  GL.Vertex3f(0.5, -0.5, -0.5);
  GL.TexCoord2f(0, 0);
  GL.Vertex3f(0.5, 0, 0.5);
  GL.TexCoord2f(1, 0);
  GL.Vertex3f(-0.5, 0, 0.5);
  GL.TexCoord2f(1, 1);
  GL.Vertex3f(-0.5, -0.5, -0.5);
  GL.End_;
  material.Material.UnApply(rci);
end;

end.

