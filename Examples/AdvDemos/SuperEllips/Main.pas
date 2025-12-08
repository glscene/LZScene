unit Main;

{$mode objfpc}{$H+}

// Made to understand some properties of TGLSuperellipsoid
// Code below was based on a code by Eric Hardinge with some some modifications
// made by Sergio Feitoza

interface

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}

  SysUtils, Math,
  Classes, Graphics, Forms,
  Controls, Menus, StdCtrls, Dialogs, Buttons,
  ExtCtrls, ComCtrls, StdActns, ActnList, ToolWin,
   ImgList,
  //GLS
  GLBitmapFont, GLWindowsFont, GLHUDObjects,GLLCLViewer,
  GLObjects, GLScene, GLGraph, GLGeomObjects, GLCoordinates, GLCrossPlatform,
  GLBaseClasses, GLColor,  GLmaterial,   GLTexture,   GLContext,
  GLVectorGeometry,  GLVectorLists, OpenGLTokens;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileNewItem: TMenuItem;
    FileOpenItem: TMenuItem;
    FileCloseItem: TMenuItem;
    Window1: TMenuItem;
    Help1: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    WindowCascadeItem: TMenuItem;
    WindowTileItem: TMenuItem;

    HelpAboutItem: TMenuItem;
    OpenDialog: TOpenDialog;
    FileSaveItem: TMenuItem;
    FileSaveAsItem: TMenuItem;
    Edit1: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    WindowMinimizeItem: TMenuItem;
    ActionList1: TActionList;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    FileNew1: TAction;
    FileSave1: TAction;
    FileExit1: TAction;
    FileOpen1: TAction;
    FileSaveAs1: TAction;

    HelpAbout1: TAction;

    WindowTileItem2: TMenuItem;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton9: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ImageList1: TImageList;
    GLScene1: TGLScene;
    CameraCube: TGLDummyCube;
    Camera: TGLCamera;
    GLLightSource1: TGLLightSource;
    ObjectsCube: TGLDummyCube;
    ArrowZ: TGLArrowLine;
    ArrowY: TGLArrowLine;
    ArrowX: TGLArrowLine;
    GLXYZGridXZ: TGLXYZGrid;
    CubeL0L1L2: TGLCube;
    GLRenderPoint1: TGLRenderPoint;
    GLSuperellipsoid1: TGLSuperellipsoid;
    GLHUDText1: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    Label14: TLabel;
    Label13: TLabel;
    Label11: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label18: TLabel;
    Label22: TLabel;
    xRadiusTrackBar: TTrackBar;
    yRadiusTrackBar: TTrackBar;
    zRadiusTrackBar: TTrackBar;
    VCurveTrackBar: TTrackBar;
    HCurveTrackBar: TTrackBar;
    SlicesTrackBar: TTrackBar;
    StacksTrackBar: TTrackBar;
    xPositionTrackBar: TTrackBar;
    yPositionTrackBar: TTrackBar;
    zPositionTrackBar: TTrackBar;
    xdirectiontrackbar: TTrackBar;
    ydirectiontrackbar: TTrackBar;
    zdirectiontrackbar: TTrackBar;
    L0trackbar: TTrackBar;
    L1trackbar: TTrackBar;
    L2trackbar: TTrackBar;
    R1trackbar: TTrackBar;
    Reset: TButton;
    GLSceneViewer1: TGLSceneViewer;
    Label7: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    BottomTrackBar: TTrackBar;
    TopTrackBar: TTrackBar;
    StartTrackBar: TTrackBar;
    StopTrackBar: TTrackBar;
    ArrowsCheckBox: TCheckBox;
    GridCheckBox: TCheckBox;
    BottomCapRadioGroup: TRadioGroup;
    TopCapRadioGroup: TRadioGroup;
    Cube_Map: TButton;
    HCheckBox: TCheckBox;
    VCheckBox: TCheckBox;
    StatusBar: TStatusBar;
    Test: TButton;
    RGdimensionsMultiplier: TRadioGroup;
    GLSuperellipsoid2: TGLSuperellipsoid;
    procedure FileNew1Execute(Sender: TObject);
    procedure FileOpen1Execute(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure ArrowsCheckBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ShowCameraLocation;
    procedure ShowFocalLength;
    procedure ShowDisplacement;
    procedure ShowSuperellipsoid;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridCheckBoxClick(Sender: TObject);
    procedure xdirectiontrackbarChange(Sender: TObject);
    procedure xRadiusTrackBarChange(Sender: TObject);
    procedure ydirectiontrackbarChange(Sender: TObject);
    procedure yPositionTrackBarChange(Sender: TObject);
    procedure yRadiusTrackBarChange(Sender: TObject);
    procedure zdirectiontrackbarChange(Sender: TObject);
    procedure zPositionTrackBarChange(Sender: TObject);
    procedure zRadiusTrackBarChange(Sender: TObject);
    procedure L0trackbarChange(Sender: TObject);
    procedure L1trackbarChange(Sender: TObject);
    procedure L2trackbarChange(Sender: TObject);
    procedure SlicesTrackBarChange(Sender: TObject);
    procedure StacksTrackBarChange(Sender: TObject);
    procedure StartTrackBarChange(Sender: TObject);
    procedure TopTrackBarChange(Sender: TObject);
    procedure TopCapRadioGroupClick(Sender: TObject);
    procedure StopTrackBarChange(Sender: TObject);
    procedure BottomCapRadioGroupClick(Sender: TObject);
    procedure BottomTrackBarChange(Sender: TObject);
    procedure TestClick(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure R1trackbarChange(Sender: TObject);
    procedure VCurveTrackBarChange(Sender: TObject);
    procedure HCurveTrackBarChange(Sender: TObject);

  private
     
    procedure CreateMDIChild(const aName: string);
  public
     
  end;

var
  MainForm: TMainForm;


implementation

{$R *.lfm}

uses
  ChildWin, About;

const
  crLightxz  = 1;        crLightyz  = 2;          crLightxy  = 3;
  crSlidexy  = 4;         crSlideyz  = 5;         crSlidexz  = 6;
  crRotate   = 7;         crZoom     = 8;        crHandMove = 9;
  spheredensity =1000;    Surf_Bounce=1.5;
  NXmax= 16;    NYmax =  11 ;  NZmax = 11 ;

var
  L0,L1, L2,R1: double;
  EllipsDir: Tvector;
  MousePoint: TPoint;

procedure TMainForm.CreateMDIChild(const aName: string);
var
  Child: TMDIChild;
begin
  { create a new MDI child window }
  Child := TMDIChild.Create(Application);
  Child.Caption := Name;
  if FileExists(Name) then Child.Memo1.Lines.LoadFromFile(Name);
end;

procedure TMainForm.FileNew1Execute(Sender: TObject);
begin
  CreateMDIChild('NONAME' + IntToStr(MDIChildCount + 1));
end;

procedure TMainForm.FileOpen1Execute(Sender: TObject);
begin
  if OpenDialog.Execute then
    CreateMDIChild(OpenDialog.FileName);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
Screen.Cursors[crSlidexy] := LoadCursor(HInstance, 'SLIDEXY');
  Screen.Cursors[crRotate]  := LoadCursor(HInstance, 'ROTATE');
  Screen.Cursors[crZoom]    := LoadCursor(HInstance, 'ZOOM');

  Randomize;
    {
  Superellipsoid := TGLSuperellipsoid(GLScene1.Objects.AddNewChild(TGLSuperellipsoid));
  Superellipsoid.name:='SuperEllis';
  Superellipsoid.Direction.SetVector(0, 0, 1);
  Superellipsoid.Up.SetVector(0, 1, 0);
  Superellipsoid.Position.SetPoint(0, 1, 0);
  Superellipsoid.Material.FrontProperties.Emission.Color:=clrYellow   ;
  Superellipsoid.Material.FrontProperties.diffuse.alpha:=0.4 ;
  Superellipsoid.Material.PolygonMode:= pmlines; //pmFill;   pmlines  pmpoints
    }

end;

procedure TMainForm.FormShow(Sender: TObject);
var    I,J,K,SphereNumber:integer;
       deltaX,deltaY,deltaZ:real;
        ptPos:Tvector;

begin
  TestClick(self);
  ShowCameraLocation;
{ focallength: right mouse drag up/down }
  ShowFocalLength;
{ displace origin: x axis: ctrl/left mouse drag left/right
                   y axis: ctrl/left mouse drag up/down }
  ShowDisplacement;
{ move light: x axis: ctrl right mouse drag left/right
              y axis: ctrl right mouse drag up/down
              z axis: shift right mouse drag up/down }

  ShowSuperellipsoid;

end;

procedure TMainForm.HelpAbout1Execute(Sender: TObject);
begin
  AboutBox.ShowModal;
end;


procedure TMainForm.FileExit1Execute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ArrowsCheckBoxClick(Sender: TObject);
begin
  ArrowX.Visible := not ArrowsCheckBox.Checked;
  ArrowY.Visible := ArrowX.Visible;
  ArrowZ.Visible := ArrowX.Visible;
end;

procedure TMainForm.GLSceneViewer1MouseDown(Sender: TObject;
          Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  MousePoint.X := X;
  MousePoint.Y := Y;
  if ssShift in Shift then
  begin
    if ssLeft in Shift then Screen.Cursor := crZoom
    else
    if ssRight in Shift then Screen.Cursor := crLightxz;
  end
  else if ssCtrl in Shift then
  begin
    if ssLeft in Shift then Screen.Cursor := crSlidexy
    else
    if ssRight in Shift then Screen.Cursor := crLightxy;
  end
  else { no shift or ctrl key }
  begin
    if Shift = [ssLeft] then Screen.Cursor := crRotate
    else
    if Shift = [ssRight] then Screen.Cursor := crZoom;
  end;
end;

procedure TMainForm.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  dx, dy: integer;
  nx, nz, d: TGLFloat;

begin  { refer GLScene\Demos\interface\camera\Camera.dpr }
  dx := MousePoint.X - X;
  dy := MousePoint.Y - Y;
  if ssShift in Shift then  { shift key down }
  begin
    if ssLeft in Shift then  { left mouse button }
    begin
  { dy = a step which adjusts target distance by 1.25%; zoom in or out }
      with Camera do	AdjustDistanceToTarget(Power(1.0125, dy));
      ShowCameraLocation;
    end
  end
  else if ssCtrl in Shift then  { Ctrl key down }
  begin
    if ssLeft in Shift then  { left mouse button }
    begin
      nz := Camera.Position.Z*dy;
      nx := Camera.Position.Z*dx;
      d := 5*Camera.FocalLength;
      with CameraCube.Position do
      begin
        Z := Z - nz/d;
        X := X - nx/d;
      end;
      ShowDisplacement;
    end
  end
  else  { no shift key }
  begin
    if Shift = [ssLeft] then
  { Left mouse button changes camera angle by moving around target }
    begin
      Camera.MoveAroundTarget(dy, dx);
      ShowCameraLocation;
    end;
    if Shift = [ssRight] then
    begin
  { Right mouse button alters the camera's focal length;
    zoom out or in by moving cursor up or down }
      with Camera do
      begin
        FocalLength  := FocalLength - dy;
        if FocalLength > 1000 then FocalLength := 1000;   { max focal length }
        if FocalLength < 20 then FocalLength := 20;       { min focal length }
      end;
      ShowFocalLength;  { display in statusbar palel }
    end;
  end;
  MousePoint.X := X;  { update mouse position }
  MousePoint.Y := Y;
end;

procedure TMainForm.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

procedure TMainForm.GridCheckBoxClick(Sender: TObject);
begin
  GlXYZGridXZ.Visible := not GridCheckBox.Checked;
end;

procedure TMainForm.xdirectiontrackbarChange(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.xRadiusTrackBarChange(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.ydirectiontrackbarChange(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.yPositionTrackBarChange(Sender: TObject);
begin
   ShowSuperellipsoid;
end;

procedure TMainForm.yRadiusTrackBarChange(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.zdirectiontrackbarChange(Sender: TObject);
begin
   ShowSuperellipsoid;
end;

procedure TMainForm.zPositionTrackBarChange(Sender: TObject);
begin
   ShowSuperellipsoid;
end;
procedure TMainForm.zRadiusTrackBarChange(Sender: TObject);
begin
   ShowSuperellipsoid;
end;

procedure TMainForm.L0trackbarChange(Sender: TObject);
begin
    ShowSuperellipsoid;
end;

procedure TMainForm.L1trackbarChange(Sender: TObject);
begin
    ShowSuperellipsoid;
end;


procedure TMainForm.L2trackbarChange(Sender: TObject);
begin
   ShowSuperellipsoid;
end;

procedure TMainForm.ShowCameraLocation;
begin
  with Camera.Position do
  StatusBar.Panels[0].Text := 'Camera: '+FloatToStrF(X, ffNumber, 5, 2)+', '+
  FloatToStrF(Y, ffNumber, 5, 2)+', '+FloatToStrF(Z, ffNumber, 5, 2);
end;

procedure TMainForm.ShowFocalLength;
begin
  with Camera do
  StatusBar.Panels[1].Text := 'f = '+FloatToStrF(FocalLength, ffnumber, 5, 2);
end;

procedure TMainForm.ShowDisplacement;
begin
  with CameraCube.Position do
  StatusBar.Panels[2].Text := 'Displaced: '+
  FloatToStrF(-X, ffNumber, 5, 2)+', '+FloatToStrF(-Y, ffNumber, 5, 2);
end;

procedure TMainForm.ShowSuperellipsoid;

var     multip: double;

begin
 case RGdimensionsMultiplier.ItemIndex of
    0: multip:=0.1;
    1: multip:=1;
    2: multip:=10;
  end;
//Superellipsoid.NormalDirection := ndInside;
//Superellipsoid.Normals :=
{ Determines how and if normals are smoothed.<p>
    - nsFlat : facetted look<br>
    - nsSmooth : smooth look<br>
    - nsNone : unlighted rendering, usefull for decla texturing }
  GLSuperellipsoid1.Scale.SetVector(multip*(xRadiusTrackBar.Position-1),
                                 multip*(yRadiusTrackBar.Position-1),
                                 multip*(zRadiusTrackBar.Position-1));

  GLSuperellipsoid1.Position.SetPoint(multip*(xPositionTrackBar.Position-1),
                                    multip*(yPositionTrackBar.Position-1),
                                    multip*(zPositionTrackBar.Position-1));

  EllipsDir:= VectorMake(multip*(xDirectionTrackBar.Position-1),
                                     multip*(yDirectionTrackBar.Position-1),
                                     multip*(zDirectionTrackBar.Position-1));
  GLSuperellipsoid1.Direction.SetVector(multip*(xDirectionTrackBar.Position-1),
                                     multip*(yDirectionTrackBar.Position-1),
                                     multip*(zDirectionTrackBar.Position-1));

  L0:=   multip*(L0TrackBar.Position-1)  ;
  L1:=   multip*(L1TrackBar.Position-1)  ;
  L2:=   multip*(L2TrackBar.Position-1)  ;
  R1:=   multip*(R1TrackBar.Position-1)  ;

  GLSuperellipsoid1.Slices := SlicesTrackBar.Position;
  GLSuperellipsoid1.Stacks := StacksTrackBar.Position;
  GLSuperellipsoid1.Top := TopTrackBar.Position;

  case TopCapRadioGroup.ItemIndex of
  0:GLSuperellipsoid1.TopCap := ctNone;
  1:GLSuperellipsoid1.TopCap := ctCenter;
  2:GLSuperellipsoid1.TopCap := ctFlat;
  end;

//  GLSuperellipsoid1.Bottom := -BottomTrackBar.Position;

  case BottomCapRadioGroup.ItemIndex of
  0:GLSuperellipsoid1.BottomCap := ctNone;
  1:GLSuperellipsoid1.BottomCap := ctCenter;
  2:GLSuperellipsoid1.BottomCap := ctFlat;
  end;

  if (StartTrackBar.Position <= StopTrackBar.Position) and
     (StartTrackBar.Position < 360) then
  begin
    GLSuperellipsoid1.Start := StartTrackBar.Position;
    GLSuperellipsoid1.Stop := StopTrackBar.Position;
  end;
 // Superellipsoid.VCheck := VCheckBox.Checked;
//  Superellipsoid.HCheck := HCheckBox.Checked;
//Superellipsoid.Normals := nsNone;
  GLHudText1.Text := 'Axis dimensions x_A  y_B  z_C :'+

               FloatToStrF(multip*(xRadiusTrackBar.Position-1), ffNumber, 6, 2)+', '+
               FloatToStrF(multip*(yRadiusTrackBar.Position-1), ffNumber, 6, 2)+', '+
               FloatToStrF(multip*(ZRadiusTrackBar.Position-1), ffNumber, 6, 2)+', '+
               #13#10'Position:'+
               FloatToStrF(multip*(xPositionTrackBar.Position-1), ffNumber, 6, 2)+', '+
               FloatToStrF(multip*(yPositionTrackBar.Position-1), ffNumber, 6, 2)+', '+
               FloatToStrF(multip*(zPositionTrackBar.Position-1), ffNumber, 6, 2)+
               #13#10'Direction:'+
               FloatToStrF(multip*(xDirectionTrackBar.Position-1), ffNumber, 6, 2)+', '+
               FloatToStrF(multip*(yDirectionTrackBar.Position-1), ffNumber, 6, 2)+', '+
               FloatToStrF(multip*(zDirectionTrackBar.Position-1), ffNumber, 6, 2)+
               #13#10'Box x_L0  y_L1   z_L2   R1 :'+
               FloatToStrF(L0, ffNumber, 6, 2)+', '+
               FloatToStrF(L1, ffNumber, 6, 2)+', '+
               FloatToStrF(L2, ffNumber, 6, 2)+', '+
               FloatToStrF(R1, ffNumber, 6, 2)+
               #13#10'VCurve:'+
                  FloatToStrF(VCurveTrackBar.Position/10, ffNumber, 6, 2)+
               #13#10'HCurve:'+
                  FloatToStrF(HCurveTrackBar.Position/10, ffNumber, 6, 2)+
               #13#10'Slices:'+
                  IntToStr(SlicesTrackBar.Position)+
               #13#10'Stacks:'+
                  IntToStr(StacksTrackBar.Position)+
               #13#10'Top:'+
                  IntToStr(TopTrackBar.Position)+'°'+
     //          #13#10'Bottom:'+
     //             IntToStr(BottomTrackBar.Position)+'°'+
               #13#10'Start:'+
                  IntToStr(StartTrackBar.Position)+'°'+
               #13#10'Stop:'+
                  IntToStr(StopTrackBar.Position)+'°'   ;
     GLSuperellipsoid1.StructureChanged ;
end;

procedure TMainForm.SlicesTrackBarChange(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.StacksTrackBarChange(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.StartTrackBarChange(Sender: TObject);
begin
  if (StartTrackBar.Position >= StopTrackBar.Position)
  then StartTrackBar.Position := StopTrackBar.Position;
  ShowSuperellipsoid;
end;


procedure TMainForm.TopCapRadioGroupClick(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.TopTrackBarChange(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.StopTrackBarChange(Sender: TObject);
begin
  if (StopTrackBar.Position <= StartTrackBar.Position)
  then StopTrackBar.Position := StartTrackBar.Position;
  ShowSuperellipsoid;
end;

procedure TMainForm.BottomCapRadioGroupClick(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.BottomTrackBarChange(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.TestClick(Sender: TObject);
begin
//  SetCurrentDir('C:\GLScene\Samples\Media\');

  with GLSuperellipsoid1.Material.Texture do
  begin
    // We need a CubeMapImage, which unlike the "regular Images" stores
    // multiple images.
    ImageClassName := TGLCubeMapImage.ClassName;
    with Image as TGLCubeMapImage do
    begin
      // Load all 6 texture map components of the cube map
      // The 'PX', 'NX', etc. refer to 'positive X', 'negative X', etc.
      // and follow the RenderMan specs/conventions
      Picture[cmtPX].LoadFromFile('cm_left.png');
      Picture[cmtNX].LoadFromFile('cm_right.png');
      Picture[cmtPY].LoadFromFile('cm_top.png');
      Picture[cmtNY].LoadFromFile('cm_bottom.png');
      Picture[cmtPZ].LoadFromFile('cm_back.png');
      Picture[cmtNZ].LoadFromFile('cm_front.png');
    end;
    // Select reflection cube map environment mapping
    // This is the mode you'll most commonly use with cube maps, normal cube
    // map generation is also supported (used for diffuse environment lighting)
    MappingMode := tmmCubeMapReflection;
    // That's all folks, let us see the thing!
    Disabled := False;
  end;
  Cube_Map.Visible := False;
end;
  {
procedure TMainForm.checkclick(Sender: TObject);
begin
  ShowSuperellipsoid;
end;

procedure TMainForm.RadiusTrackBarChange(Sender: TObject);
begin
  ShowSuperellipsoid;
end;
    }

procedure TMainForm.ResetClick(Sender: TObject);
var I,J,K:integer;
begin
   GLSuperellipsoid1.free;

  GLsceneViewer1.ResetPerformanceMonitor;
  MainForm.Close;
   FormCreate(self) ;
   FormShow (self);
  GLSuperellipsoid1.StructureChanged ;
 GLsceneViewer1.Update;

end;

procedure TMainForm.R1trackbarChange(Sender: TObject);
begin
   ShowSuperellipsoid;
end;

procedure TMainForm.VCurveTrackBarChange(Sender: TObject);
var
  n: TGLFloat;
begin
  n := VCurveTrackBar.Position/10;
  GLSuperellipsoid1.XYCurve := n;      // ex Vcurve
  ShowSuperellipsoid;
end;


procedure TMainForm.HCurveTrackBarChange(Sender: TObject);
var
  n: TGLFloat;

begin
  n := HCurveTrackBar.Position/10;
  GLSuperellipsoid1.ZCurve := n;             // ex H curve
  ShowSuperellipsoid;
end;


end.
