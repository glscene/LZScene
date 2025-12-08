unit UMainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,{$ifdef Windwos} Windows,{$endif} FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, ActnList, Types,

  GLLCLViewer, GLScene, GLState, GLMaterial, GLCadencer, GLVectorTypes,
  GLVectorGeometry, GLGraph, GLHiddenLineShader, GLOutlineShader,
  GLTextureSharingShader, GLAsyncTimer, GLObjects, GLVectorFileObjects,

  uNavCube, GLBaseClasses;

type

  { TMainForm }

  TMainForm = class(TForm)
    acSmoothShading: TAction;
    acFlatShading: TAction;
    acPointShading: TAction;
    acTextureShading: TAction;
    acSceneLighting: TAction;
    acShaderHiddenLines: TAction;
    acShaderOutLines: TAction;
    acShaderNone: TAction;
    actInvertNormals: TAction;
    actOpenModel: TAction;
    acWireFrameShading: TAction;
    DCCamera: TGLDummyCube;
    DCTarget: TGLDummyCube;
    DCGrids: TGLDummyCube;
    DCGridXY: TGLDummyCube;
    DCGridXZ: TGLDummyCube;
    DCGridYZ: TGLDummyCube;
    DCLights: TGLDummyCube;
    DCRoot: TGLDummyCube;
    DCWorld: TGLDummyCube;
    Camera1: TGLCamera;
    DCStaticLights: TGLDummyCube;
    DCDynamicLights: TGLDummyCube;
    DCWorldAxis: TGLDummyCube;
    DCWorldGrid: TGLDummyCube;
    ffObject: TGLFreeForm;
    BBox: TGLCube;
    DCAxis: TGLDummyCube;
    FrontAmbientLight: TGLLightSource;
    backAmbientLight: TGLLightSource;
    KeyAmbientLigth: TGLLightSource;
    HiddenLineShader: TGLHiddenLineShader;
    MenuItem20: TMenuItem;
    MenuItem23: TMenuItem;
    OutlineShader: TGLOutlineShader;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    WorldGrid: TGLXYZGrid;
    MainLightSource1: TGLLightSource;
    MainStatusBar: TStatusBar;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    XZGrid: TGLXYZGrid;
    YZGrid: TGLXYZGrid;
    XYGrid: TGLXYZGrid;
    MainActionList: TActionList;
    ASyncTimer: TGLAsyncTimer;
    Cadencer: TGLCadencer;
    LightMapLib: TGLMaterialLibrary;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    TextureMatLib: TGLMaterialLibrary;
    GLScene: TGLScene;
    GLSViewer: TGLSceneViewer;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    MainTimer: TTimer;
    ToolBar1: TToolBar;
    procedure acFlatShadingExecute(Sender: TObject);
    procedure acPointShadingExecute(Sender: TObject);
    procedure acSceneLightingExecute(Sender: TObject);
    procedure acShaderHiddenLinesExecute(Sender: TObject);
    procedure acShaderNoneExecute(Sender: TObject);
    procedure acShaderOutLinesExecute(Sender: TObject);
    procedure acSmoothShadingExecute(Sender: TObject);
    procedure acTextureShadingExecute(Sender: TObject);
    procedure actInvertNormalsExecute(Sender: TObject);
    procedure actOpenModelExecute(Sender: TObject);
    procedure acWireFrameShadingExecute(Sender: TObject);
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure GLSViewerAfterRender(Sender: TObject);
    procedure GLSViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLSViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MainTimerTimer(Sender: TObject);
  private
    { private declarations }
    procedure ShowCameraLocation;
    procedure ShowFocalLength;
    procedure ShowLightLocation;
    procedure ShowTargetLocation;

    procedure DoResetCamera;
    procedure SetupFreeFormShading;
    procedure ApplyShadeModeToMaterial(aMaterial: TGLMaterial);
    procedure ApplyObjectShadeMode;
//    procedure ApplyFSAA;
//    procedure ApplyObjectFaceCull;
    procedure ApplyObjectTexturing;
    procedure DoOpen(const FileName: String);
  protected
    MousePoint: TPoint;
    md:Boolean;
  public
    { public declarations }
    lastFileName: String;
    lastLoadWithTextures: Boolean;
  end;

const
  crLightxz  = 1;
  crLightyz  = 2;
  crLightxy  = 3;
  crSlidexy  = 4;
  crSlideyz  = 5;
  crSlidexz  = 6;
  crRotate   = 7;
  crZoom     = 8;
  crHandMove = 9;
  crSlidezy  = 10;

var
  MainForm: TMainForm;
  NavCube: TGLNavCube;

implementation

{$R *.lfm}
{$R Cursors.res}

uses
  Math,
  // GLFileQ3BSP
  // GLFileDXF
  // GLFileOCT
  // GLFileGRD
  GLFileOBJ, GLFileSTL, GLFileLWO, GLFileMS3D,
  GLFileNMF, GLFileMD3, GLFile3DS, GLFileMD2, GLFileSMD, GLFilePLY, GLFileGTS,
  GLFileVRML, GLFileMD5, GLFileTIN ;

function LoadCursorFromRes(CursorName:String):THandle;
var
   Cur: TCursorImage;
begin
   Cur := TCursorImage.Create;
   Cur.LoadFromResourceName(HInstance,CursorName);
   result := Cur.ReleaseHandle;
   Cur.Free;
end;

procedure TMainForm.GLSViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
  md := False;
end;

procedure TMainForm.MainTimerTimer(Sender: TObject);
begin
  ShowCameraLocation;
  ShowFocalLength;
  ShowTargetLocation;
  ShowLightLocation;
  MainStatusBar.Panels[4].Text := Format('%.1f  FPS', [GLSViewer.FramesPerSecond]);
  GLSViewer.ResetPerformanceMonitor;
end;

procedure TMainForm.GLSViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if md and (Shift <> []) then
  begin
    if ssLeft in Shift then
    begin
      if ssShift in Shift then
      begin
        //Showmessage('on the rock');
        //NavCube.ActiveMouse:=True;
      end
      else
      begin
       // NavCube.ActiveMouse:=False;
        Camera1.MoveAroundTarget((MousePoint.y - Y) * 0.1, (MousePoint.x - X) * 0.1)
      end;
    end
    else if ssRight in Shift then
    begin
      if ssShift in Shift then   { shift key down }
      begin
        with Camera1 do AdjustDistanceToTarget(Power(1.0125, MousePoint.y - Y));
      end
      else
      begin
        with Camera1 do
        begin
          FocalLength  := FocalLength - (MousePoint.y - Y);
          if FocalLength > 3000 then FocalLength := 3000;   { max focal length }
          if FocalLength < 10 then FocalLength := 10;       { min focal length }
        end;       { display in statusbar palel }
      end;
    (*  d := Camera.DistanceToTarget * 0.01 * (X - MousePoint.x + Y - MousePoint.y);
      if IsKeyDown('x') then ffObject.Translate(d, 0, 0)
      else if IsKeyDown('y') then ffObject.Translate(0, d, 0)
      else if IsKeyDown('z') then ffObject.Translate(0, 0, d)
      else
      begin
        if ssShift in Shift then
          Camera.RotateObject(ffObject, (MousePoint.y - Y) * 0.1, (MousePoint.x - X) * 0.1)
        else
          Camera.RotateObject(ffObject, MousePoint.y - Y, MousePoint.x - X);
      end; *)
    end;
    MousePoint.X := X;         { update mouse position }
    MousePoint.Y := Y;
  end;

end;

procedure TMainForm.GLSViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MousePoint.X := X;
  MousePoint.Y := Y;
  if Shift = [ssLeft] then
  begin
    Screen.Cursor := crRotate;
    NavCube.ActiveMouse := True;
  end
  else if Shift = [ssRight] then
  begin
    Screen.Cursor := crZoom;
  end;
  md:=true;
(*  if ssShift in Shift then        { Shift key down}
  begin
    if ssLeft in Shift then Screen.Cursor := crZoom;
  end
  else if ssCtrl in Shift then    { Ctrl key down }
  begin
    //if ssLeft in Shift then Screen.Cursor := crSlidexz
    //else
    // if ssRight in Shift then Screen.Cursor := crLightxz;
  end
  else if ssAlt in Shift then     { Alt key down }
  begin
    //if ssLeft in Shift then Screen.Cursor := crSlidezy
    //else
    //if ssRight in Shift then Screen.Cursor := crLightxy;
  end
  else { no shift, ctrl or alt key }
  begin
    if Shift = [ssLeft] then Screen.Cursor := crRotate
    else
      if Shift = [ssRight] then Screen.Cursor := crZoom;
  end;   *)
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if (MousePoint.X >= GLSViewer.Left) and
     (MousePoint.X <= GLSViewer.Left + GLSViewer.Width) and
     (MousePoint.Y >= GLSViewer.Top) and
     (MousePoint.y <= GLSViewer.Top + GLSViewer.Height) then
  begin
{ a wheel step = WheelDelta/300; each step adjusts target distance by 2.5%
  another method to zoom in or out }
    //GLSViewer.SetFocus;
    if ffObject.MeshObjects.Count > 0 then
    begin
      Camera1.AdjustDistanceToTarget(Power(1.025, WheelDelta / 300));
      Camera1.DepthOfView := 2 * Camera1.DistanceToTarget + 2 * ffObject.BoundingSphereRadius;
    end;
    Handled := True;

    //Camera1.AdjustDistanceToTarget(Power(1.025, WheelDelta/300));
    //ShowCameraLocation;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  GetCurrentDir;
  NavCube := TGLNavCube.CreateAsChild(GLScene.Objects);
  NavCube.SceneViewer := GLSViewer;
  NavCube.Camera:=Camera1;
  NavCube.ActiveMouse:=True;
 // NavCube.AutoRotate:=False;
  NavCube.FPS := 30;

 // instantiate our specific hidden-lines shader
 // hlShader := THiddenLineShader.Create(Self);
  ffObject.IgnoreMissingTextures := True;

  Screen.Cursors[crLightxy] := LoadCursorFromRes('LIGHTXY'); //LoadCursor(HInstance, 'LIGHTXY');
  Screen.Cursors[crLightyz] := LoadCursorFromRes('LIGHTYZ'); //LoadCursor(HInstance, 'LIGHTYZ');
  Screen.Cursors[crLightxz] := LoadCursorFromRes('LIGHTXZ'); //LoadCursor(HInstance, 'LIGHTXZ');
  Screen.Cursors[crSlidexy] := LoadCursorFromRes('SLIDEXY'); //LoadCursor(HInstance, 'SLIDEXY');
  Screen.Cursors[crSlidexz] := LoadCursorFromRes('SLIDEXZ'); //LoadCursor(HInstance, 'SLIDEXZ');
  Screen.Cursors[crSlideyz] := LoadCursorFromRes('SLIDEYZ'); //LoadCursor(HInstance, 'SLIDEYZ');
  Screen.Cursors[crRotate]  := LoadCursorFromRes('ROTATE'); //LoadCursor(HInstance, 'ROTATE');
  Screen.Cursors[crZoom]    := LoadCursorFromRes('ZOOM'); //LoadCursor(HInstance, 'ZOOM');
  Screen.Cursors[crSlidezy] := LoadCursorFromRes('SLIDEZY'); //LoadCursor(HInstance, 'SLIDEZY');
end;

procedure TMainForm.CadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if NavCube.InactiveTime > 5 then
  begin
    if NavCube.InactiveTime < 8 then
      Camera1.TurnAngle := Camera1.TurnAngle + (NavCube.InactiveTime - 5) * deltaTime * 2
    else
      Camera1.TurnAngle := Camera1.TurnAngle + deltatime * 6;
  end;
  GLSViewer.Refresh;
  if Self.Focused then
    GLSViewer.Invalidate;
end;

procedure TMainForm.acPointShadingExecute(Sender: TObject);
begin
  ApplyObjectShadeMode();
end;

procedure TMainForm.acSceneLightingExecute(Sender: TObject);
begin
  //acSceneLight.Checked := not(acSceneLight.Checked);
  ApplyObjectShadeMode;
end;

procedure TMainForm.acShaderHiddenLinesExecute(Sender: TObject);
begin
 ApplyObjectShadeMode;
end;

procedure TMainForm.acShaderNoneExecute(Sender: TObject);
begin
  ApplyObjectShadeMode;
end;

procedure TMainForm.acShaderOutLinesExecute(Sender: TObject);
begin
 ApplyObjectShadeMode;
end;

procedure TMainForm.acSmoothShadingExecute(Sender: TObject);
begin
    ApplyObjectShadeMode();
end;

procedure TMainForm.acTextureShadingExecute(Sender: TObject);
begin
    ApplyObjectTexturing();
end;

procedure TMainForm.actInvertNormalsExecute(Sender: TObject);
begin
  if actInvertNormals.Checked then
     ffObject.NormalsOrientation:=mnoInvert
   else
     ffObject.NormalsOrientation:=mnoDefault;
end;

procedure TMainForm.actOpenModelExecute(Sender: TObject);
begin
  NavCube.ActiveMouse := False;
  if OpenDialog.Execute then DoOpen(OpenDialog.fileName);
end;

procedure TMainForm.acFlatShadingExecute(Sender: TObject);
begin
    ApplyObjectShadeMode();
end;

procedure TMainForm.acWireFrameShadingExecute(Sender: TObject);
begin
    ApplyObjectShadeMode();
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ShowCameraLocation;
  ShowFocalLength;
  ShowTargetLocation;
  ShowLightLocation;

  OpenDialog.Filter := VectorFileFormatsFilter;
  SaveDialog.Filter := VectorFileFormatsSaveFilter;

  ASyncTimer.Enabled:=true;
  //ApplyFSAA;
  //ApplyFaceCull;
  //ApplyFPS;
end;

procedure TMainForm.GLSViewerAfterRender(Sender: TObject);
begin
  //ApplyFSAA;
  //Screen.Cursor := crDefault;
end;

procedure TMainForm.ShowCameraLocation;
begin
  with Camera1.Position do
  MainStatusBar.Panels[0].Text := 'Camera: '+FloatToStrF(X, ffNumber, 5, 2)+', '+
  FloatToStrF(Y, ffNumber, 5, 2)+', '+FloatToStrF(Z, ffNumber, 5, 2);
end;

procedure TMainForm.ShowTargetLocation;
begin
  with DCTarget.Position do
  MainStatusBar.Panels[2].Text := 'Target: '+
  FloatToStrF(-X, ffNumber, 5, 2)+', '+FloatToStrF(-Y, ffNumber, 5, 2)+', '+
  FloatToStrF(-Z, ffNumber, 5, 2);
end;

procedure TMainForm.ShowFocalLength;
begin
  with Camera1 do
  MainStatusBar.Panels[1].Text := 'Focal: '+FloatToStrF(FocalLength, ffnumber, 5, 2);
end;

procedure TMainForm.ShowLightLocation;
begin
  with MainLightSource1.Position do
  MainStatusBar.Panels[3].Text := 'Light: '+
  FloatToStrF(X, ffNumber, 5, 2)+', '+FloatToStrF(Y, ffNumber, 5, 2)+', '+
  FloatToStrF(Z, ffNumber, 5, 2);
end;

procedure TMainForm.ApplyObjectTexturing;
var
  i: Integer;
begin
  with TextureMatLib.Materials do
    for i := 0 to Count - 1 do
    begin
      with Items[i].Material.Texture do
      begin
        if Enabled then
          Items[i].Tag := Integer(True);
        Enabled := Boolean(Items[i].Tag) and acTextureShading.Checked;
      end;
    end;
  ffObject.StructureChanged;
end;

procedure TMainForm.ApplyShadeModeToMaterial(aMaterial: TGLMaterial);
begin
  if acPointShading.Checked then
  begin
    GLSViewer.Buffer.Lighting := True;
    GLSViewer.Buffer.ShadeModel := smSmooth;
    aMaterial.PolygonMode := pmPoints;
  end
  else
  if acSmoothShading.Checked then
  begin
    GLSViewer.Buffer.Lighting := True;
    GLSViewer.Buffer.ShadeModel := smSmooth;
    aMaterial.PolygonMode := pmFill;
  end
  else if acFlatShading.Checked then
  begin
    GLSViewer.Buffer.Lighting := True;
    GLSViewer.Buffer.ShadeModel := smFlat;
    aMaterial.PolygonMode := pmFill;
  end
  else if acWireframeShading.Checked then
  begin
    GLSViewer.Buffer.Lighting := False;
    GLSViewer.Buffer.ShadeModel := smSmooth;
    aMaterial.PolygonMode := pmLines;
  end;
end;

procedure TMainForm.ApplyObjectShadeMode;
var
  i: Integer;
begin
  with TextureMatLib.Materials do
    for i := 0 to Count - 1 do
    begin
      ApplyShadeModeToMaterial(Items[i].Material);
      if (acShaderHiddenLines.Checked) then
        Items[i].Shader := HiddenLineShader
      else if (acShaderOutLines.Checked) then
        Items[i].Shader := OutLineShader
      else if (acShaderNone.Checked) then
        Items[i].Shader := nil;
    end;
  GLSViewer.Buffer.Lighting := acSceneLighting.Checked;
  ffObject.StructureChanged;
end;

procedure TMainForm.SetupFreeFormShading;
var
  i: Integer;
  LibMat: TGLLibMaterial;
begin
  if TextureMatLib.Materials.Count = 0 then
  begin
    ffObject.Material.MaterialLibrary := TextureMatLib;
    LibMat := TextureMatLib.Materials.Add;
    ffObject.Material.LibMaterialName := LibMat.Name;
    libMat.Material.FrontProperties.Diffuse.Red := 0;
  end;
  for i := 0 to TextureMatLib.Materials.Count - 1 do
    with TextureMatLib.Materials[i].Material do
      BackProperties.Assign(FrontProperties);
  ApplyObjectShadeMode;
  ApplyObjectTexturing;
end;

procedure TMainForm.DoResetCamera;
var
  objSize: Single;
begin
  dcTarget.Position.AsVector := NullHmgPoint;
  Camera1.Position.SetPoint(50, 40, 50);
  ffObject.Position.AsVector := NullHmgPoint;
  ffObject.Up.Assign(DCWorldAxis.Up);
  ffObject.Direction.Assign(DCWorldAxis.Direction);

  objSize := ffObject.BoundingSphereRadius;
  if objSize > 0 then
  begin
    if objSize < 1 then
    begin
      Camera1.SceneScale := 1 / objSize;
      objSize := 1;
    end
    else
      Camera1.SceneScale := 1;
      Camera1.AdjustDistanceToTarget(objSize * 0.27);
      Camera1.DepthOfView := 1.5 * Camera1.DistanceToTarget + 2 * objSize;
  end;
end;

procedure TMainForm.DoOpen(const FileName: String);
var
  min, max: TAffineVector;
  GridStep, ObjSize : Single;
begin
  if not FileExists(fileName) then Exit;
  Screen.Cursor := crHourGlass;
  Caption := 'Scene Master - ' + FileName;
  TextureMatLib.Materials.Clear;
  ffObject.MeshObjects.Clear;
  ffObject.LoadFromFile(FileName);
  SetupFreeFormShading;
 // acFileSaveTextures.Enabled := (MaterialLib.Materials.Count > 0);
//  acFileOpenTexLib.Enabled := (MaterialLib.Materials.Count > 0);
  lastFileName := FileName;
  lastLoadWithTextures := acTextureShading.Enabled;
  ffObject.GetExtents(min, max);
  BBox.CubeWidth := (max.X - min.X)/2;
  BBox.CubeHeight := (max.Y - min.Y)/2;
  BBox.CubeDepth := (max.Z - min.Z/2);
//  ffObject.Position.AsAffineVector := VectorLerp(min/2, max/2, 0.5);
  //BBox.Position.AsAffineVector := VectorLerp(min, max, 0.5);
 // ffObject.Translate(0,(BBox.Position.Y*2),0);

  // GridStep := 1.0;


  With WorldGrid do
  begin
    With XSamplingScale do
    begin
      min:=-BBox.CubeWidth+1;
      max:=BBox.CubeWidth+1;
      GridStep :=1.0+ (10*BBox.CubeWidth)/BBox.CubeWidth;
      step:=GridStep;
    end;
    With ZSamplingScale do
    begin
      min:=-BBox.CubeDepth+1;
      max:=BBox.CubeDepth+1;
      GridStep :=1.0+ (10*BBox.CubeDepth)/BBox.CubeDepth;
      step:=GridStep;
    end;
  end;

  With XZGrid do
  begin
    With XSamplingScale do
    begin
      min:=-BBox.CubeWidth+1;
      max:=BBox.CubeWidth+1;
      GridStep :=1.0+ (10*BBox.CubeWidth)/BBox.CubeWidth;
      step:=GridStep;
      step:=GridStep;
    end;
    With ZSamplingScale do
    begin
      min:=-BBox.CubeDepth+1;
      max:=BBox.CubeDepth+1;
      GridStep :=1.0+ (10*BBox.CubeDepth)/BBox.CubeDepth;
      step:=GridStep;
    end;
  end;


  With YZGrid do
  begin
    With YSamplingScale do
    begin
      min:=-BBox.CubeHeight+1;
      max:=BBox.CubeHeight+1;
      GridStep :=1.0+ (10*BBox.CubeHeight)/BBox.CubeHeight;
      step:=GridStep;
    end;
    With ZSamplingScale do
    begin
      min:=-BBox.CubeDepth+1;
      max:=BBox.CubeDepth+1;
      GridStep :=1.0+ (10*BBox.CubeDepth)/BBox.CubeDepth;
      step:=GridStep;
    end;
  end;


  With XYGrid do
  begin
    With YSamplingScale do
    begin
      min:=-BBox.CubeHeight+1;
      max:=BBox.CubeHeight+1;
      step:=GridStep;
    end;
    With XSamplingScale do
    begin
      min:=-BBox.CubeWidth+1;
      max:=BBox.CubeWidth+1;
      step:=GridStep;
    end;
  end;

  With DCGridYZ do
  begin
    position.X:=-(BBox.CubeWidth);
    position.Y:=-1;
    position.Z:=0;
  end;
    With DCGridXZ do
  begin
    position.X:=0;
    position.Y:=-(BBox.CubeHeight);
    position.Z:=0;
  end;

  With DCGridXY do
  begin
    position.X:=0;
    position.Y:=-1;
    position.Z:=-(BBox.CubeDepth);
  end;

(*  StatusBar.Panels[0].Text := 'X: ' + ' ';
  StatusBar.Panels[1].Text := 'Y: ' + ' ';
  StatusBar.Panels[2].Text := 'Z: ' + ' '; *)


  DoResetCamera;
  Screen.Cursor := crDefault;
end;

end.

