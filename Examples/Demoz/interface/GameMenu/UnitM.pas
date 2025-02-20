{
  A simple demo that shows how to use the TGLGameMenu component

  Version history:
    20/02/07 - DaStr - Initial version
    03/07/07 - DaStr - Keyboard renamed to GLKeyboard (BugTracker ID = 1678646)

}
unit UnitM;

{$MODE Delphi}

interface

uses
  // VCL
  SysUtils, Graphics, Controls, Forms, ExtCtrls, Classes,
  Dialogs, StdCtrls,

  // GLScene
  GLScene, GLObjects, GLLCLViewer, GLGeomObjects,
  GLBitmapFont, GLWindowsFont, GLGameMenu, GLCadencer, GLTexture,
  GLKeyboard, GLCrossPlatform, GLMaterial, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCube1: TGLCube;
    MainPanel: TPanel;
    ShoTitleCheckbox: TCheckBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ShoTitleCheckboxClick(Sender: TObject);
    procedure MainPanelResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    GameMenu: TGLGameMenu;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses GLUtils, LCLType;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();

  GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile(MediaPath+'clouds.jpg');

  GameMenu := TGLGameMenu(GLScene1.Objects.AddNewChild(TGLGameMenu));
  GameMenu.MaterialLibrary := GLMaterialLibrary1;
  GameMenu.TitleMaterialName := 'LibMaterial';
  GameMenu.TitleHeight := 80;
  GameMenu.TitleWidth := 200;
  GameMenu.Font := GLWindowsBitmapFont1;
  GameMenu.Items.Add('test line 1');
  GameMenu.Items.Add('test line 2');
  GameMenu.Items.Add('test line 3');
  GameMenu.Items.Add('test line 4');
  GameMenu.Items.Add('test line 5');
  GameMenu.Items.Add('test line 6');
  GameMenu.Spacing := 1;
  GameMenu.Selected := 0;
  GameMenu.Position.Y := 200;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  if IsKeyDown('W') then
    GameMenu.SelectPrev;
  if IsKeyDown('S') then
    GameMenu.SelectNext;
  if IsKeyDown(VK_RETURN) then
  begin
    if GameMenu.Selected <> -1 then
      ShowMessage('You have selected option: ' + GameMenu.SelectedText);
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  GameMenu.MouseMenuSelect(X, Y);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  GameMenu.MouseMenuSelect(X, Y);
  if GameMenu.Selected <> -1 then
    ShowMessage('You have selected option: ' + GameMenu.SelectedText);
end;

procedure TForm1.ShoTitleCheckboxClick(Sender: TObject);
begin
  if GameMenu.TitleHeight = 0 then
    GameMenu.TitleHeight := 80
  else
    GameMenu.TitleHeight := 0;
end;

procedure TForm1.MainPanelResize(Sender: TObject);
begin
  GameMenu.Position.X := MainPanel.Width div 2;
end;

end.

