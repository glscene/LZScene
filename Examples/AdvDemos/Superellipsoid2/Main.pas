unit Main;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  SysUtils, Classes,Math,
  Graphics, Controls, Forms, Dialogs, ComCtrls,
  ExtCtrls,  StdCtrls,

  //GLS
  GLBaseClasses, GLScene, GLObjects, GLCoordinates,
  GLCrossPlatform, OpenGLTokens, GLState, GLGraph, GLGeomObjects, GLMesh,
  GLHUDObjects, GLBitmapFont, GLWindowsFont, GLVectorGeometry, GLColor,
  GLTexture, GLLCLViewer;

type
  TMainForm = class(TForm)
    StatusBar: TStatusBar;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLDummyCube1: TGLDummyCube;
    GLCamera1: TGLCamera;
    ObjectsCube: TGLDummyCube;
    ArrowZ: TGLArrowLine;
    ArrowY: TGLArrowLine;
    ArrowX: TGLArrowLine;
    GLLightSource1: TGLLightSource;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label12: TLabel;

    xRadiusTrackBar: TTrackBar;
    yRadiusTrackBar: TTrackBar;
    zRadiusTrackBar: TTrackBar;

    xyCurveTrackBar: TTrackBar;
    zCurveTrackBar: TTrackBar;
    GridCheckBox: TCheckBox;
    ArrowsCheckBox: TCheckBox;
    SlicesTrackBar: TTrackBar;
    StacksTrackBar: TTrackBar;

    TopCapRadioGroup: TRadioGroup;
    BottomTrackBar: TTrackBar;
    TopTrackBar: TTrackBar;

    StartTrackBar: TTrackBar;
    StopTrackBar: TTrackBar;
    BottomCapRadioGroup: TRadioGroup;
    Button1: TButton;
    GLXYZGridXZ: TGLXYZGrid;
    GLMesh1: TGLMesh;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    GLHUDText1: TGLHUDText;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AnyChange(Sender: TObject);
    procedure GridCheckBoxClick(Sender: TObject);
    procedure ArrowsCheckBoxClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure StartTrackBarChange(Sender: TObject);
    procedure StopTrackBarChange(Sender: TObject);
  private
     
    MousePoint: TPoint;
    procedure ShowCameraLocation;
    procedure ShowFocalLength;
    procedure ShowMesh;
  public
     
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}
{$R CURSORS.RES}

const
  crLightxz = 1;
  crLightyz = 2;
  crLightxy = 3;
  crSlidexy = 4;
  crSlideyz = 5;
  crSlidexz = 6;
  crRotate = 7;
  crZoom = 8;
  crHandMove = 9;

procedure TMainForm.AnyChange(Sender: TObject);
begin
  ShowMesh;
end;

procedure TMainForm.ArrowsCheckBoxClick(Sender: TObject);
begin
  ArrowX.Visible := not ArrowsCheckBox.Checked;
  ArrowY.Visible := ArrowX.Visible;
  ArrowZ.Visible := ArrowX.Visible;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  (* vmV, vmVN, vmVNC, vmVNCT, vmVNT, vmVT *)
  GLMesh1.VertexMode := vmVN;
  with GLMesh1.Material.Texture do
  begin
    // We need a CubeMapImage, which unlike the "regular Images" stores
    // multiple images.
    ImageClassName := TGLCubeMapImage.ClassName;
    with Image as TGLCubeMapImage do
    begin
      // Load all 6 texture map components of the cube map
      // The 'PX', 'NX', etc. refer to 'positive X', 'negative X', etc.
      // and follow the RenderMan specs/conventions
      Picture[cmtPX].LoadFromFile('cm_left.jpg');
      Picture[cmtNX].LoadFromFile('cm_right.jpg');
      Picture[cmtPY].LoadFromFile('cm_top.jpg');
      Picture[cmtNY].LoadFromFile('cm_bottom.jpg');
      Picture[cmtPZ].LoadFromFile('cm_back.jpg');
      Picture[cmtNZ].LoadFromFile('cm_front.jpg');
    end;
    // Select reflection cube map environment mapping
    // This is the mode you'll most commonly use with cube maps, normal cube
    // map generation is also supported (used for diffuse environment lighting)
    MappingMode := tmmCubeMapReflection;
    // That's all folks, let us see the thing!
    Disabled := False;
  end;
  Button1.Visible := False;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Screen.Cursors[crRotate] := LoadCursor(HInstance, 'ROTATE');
  Screen.Cursors[crZoom] := LoadCursor(HInstance, 'ZOOM');
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ShowCameraLocation;
  { focallength: right mouse drag up/down }
  ShowFocalLength;
  ShowMesh;
end;

procedure TMainForm.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MousePoint.X := X;
  MousePoint.Y := Y;
  if ssShift in Shift then
  begin
    if ssLeft in Shift then
      Screen.Cursor := crZoom
    else if ssRight in Shift then
      Screen.Cursor := crLightxz;
  end
  else if ssCtrl in Shift then
  begin
    if ssLeft in Shift then
      Screen.Cursor := crSlidexy
    else if ssRight in Shift then
      Screen.Cursor := crLightxy;
  end
  else { no shift or ctrl key }
  begin
    if Shift = [ssLeft] then
      Screen.Cursor := crRotate
    else if Shift = [ssRight] then
      Screen.Cursor := crZoom;
  end;
end;

procedure TMainForm.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  dx, dy: Integer;
  nx, nz, d: TGLFloat;

begin { refer GLScene\Demos\interface\camera\Camera.dpr }
  dx := MousePoint.X - X;
  dy := MousePoint.Y - Y;
  if ssShift in Shift then { shift key down }
  begin
    if ssLeft in Shift then { left mouse button }
    begin
      { dy = a step which adjusts target distance by 1.25%; zoom in or out }
      with GLCamera1 do
        AdjustDistanceToTarget(Power(1.0125, dy));
      ShowCameraLocation;
    end
  end
  else { no shift key }
  begin
    if Shift = [ssLeft] then
    { Left mouse button changes camera angle by moving around target }
    begin
      GLCamera1.MoveAroundTarget(dy, dx);
      ShowCameraLocation;
    end;
    if Shift = [ssRight] then
    begin
      { Right mouse button alters the camera's focal length;
        zoom out or in by moving cursor up or down }
      with GLCamera1 do
      begin
        FocalLength := FocalLength - dy;
        if FocalLength > 2000 then
          FocalLength := 2000; { max focal length }
        if FocalLength < 20 then
          FocalLength := 20; { min focal length }
      end;
      ShowFocalLength; { display in statusbar palel }
    end;
  end;
  MousePoint.X := X; { update mouse position }
  MousePoint.Y := Y;
end;

procedure TMainForm.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

procedure TMainForm.GridCheckBoxClick(Sender: TObject);
begin
  GLXYZGridXZ.Visible := not GridCheckBox.Checked;
end;

procedure TMainForm.ShowCameraLocation;
begin
  with GLCamera1.Position do
    StatusBar.Panels[0].Text := 'Camera: ' + FloatToStrF(X, ffNumber, 5, 2) +
      ', ' + FloatToStrF(Y, ffNumber, 5, 2) + ', ' +
      FloatToStrF(Z, ffNumber, 5, 2);
end;

procedure TMainForm.ShowFocalLength;
begin
  with GLCamera1 do
    StatusBar.Panels[1].Text := 'f = ' + FloatToStrF(FocalLength,
      ffNumber, 5, 2);
end;

procedure TMainForm.ShowMesh;
type
  TCapType = (ctNone, ctCenter, ctFlat);

  procedure AddTriangle(const p1, p2, p3: TAffineVector;
    const color: TColorVector);
  begin
    with GLMesh1.Vertices do
    begin
      AddVertex(p1, NullVector, color);
      AddVertex(p2, NullVector, color);
      AddVertex(p3, NullVector, color);
    end;
  end;

var
  i, j, tc1, tc2: Integer;
  CosPc1, SinPc1, CosTc2, SinTc2, CosNPc1, SinNPc1, CosNTc2, SinNTc2: Double;

  xyCurve: extended;
  zCurve: extended;

  Stacks: GLInt;
  Slices: GLInt;

  aTop: TAngleLimit1; { i.e. -90 .. 90 }
  aBottom: TAngleLimit1;
  Start: TAngleLimit2; { i.e.  0 .. 360 }
  Stop: TAngleLimit2;
  TopCap, BottomCap: TCapType;

  v1, v2, v3, v4: TAffineVector;
  AngTop, AngBottom, AngStart, AngStop, StepV, StepH, Phi, NextPhi, Theta,
    NextTheta, SinP, CosP, SinNP, CosNP, SinT, CosT, SinNT, CosNT: Double;

begin
  xyCurve := xyCurveTrackBar.Position / 10;
  zCurve := zCurveTrackBar.Position / 10;

  Slices := SlicesTrackBar.Position;
  Stacks := StacksTrackBar.Position;

  aTop := TopTrackBar.Position;
  aBottom := -BottomTrackBar.Position;

  Start := StartTrackBar.Position;
  Stop := StopTrackBar.Position;

  case TopCapRadioGroup.ItemIndex of
    0: TopCap := ctNone;
    1: TopCap := ctCenter;
    2: TopCap := ctFlat;
  end;

  case BottomCapRadioGroup.ItemIndex of
    0: BottomCap := ctNone;
    1: BottomCap := ctCenter;
    2: BottomCap := ctFlat;
  end;

  AngTop := DegToRad(1.0 * aTop);
  AngBottom := DegToRad(1.0 * aBottom);
  AngStart := DegToRad(1.0 * Start);
  AngStop := DegToRad(1.0 * Stop);
  StepH := (AngStop - AngStart) / Slices;
  StepV := (AngTop - AngBottom) / Stacks;

  with GLMesh1 do
  begin
    Mode := mmTriangles;
    Vertices.Clear;
    Scale.SetVector(xRadiusTrackBar.Position / 10, yRadiusTrackBar.Position /
      10, zRadiusTrackBar.Position / 10);
  end;
  { Even integer used with the Power function, only produce positive points }
  tc1 := trunc(xyCurve);
  tc2 := trunc(zCurve);
  if tc1 mod 2 = 0 then
    xyCurve := xyCurve + 1E-6;
  if tc2 mod 2 = 0 then
    zCurve := zCurve - 1E-6;

  // bottom cap
  if (aBottom > -90) and (BottomCap in [ctCenter, ctFlat]) then
  begin
    SinCos(AngBottom, SinP, CosP);

    if (Sign(SinP) = 1) or (tc1 = xyCurve) then
      SinPc1 := Power(SinP, xyCurve)
    else
      SinPc1 := -Power(-SinP, xyCurve);

    if (Sign(CosP) = 1) or (tc1 = xyCurve) then
      CosPc1 := Power(CosP, xyCurve)
    else
      CosPc1 := -Power(-CosP, xyCurve);

    if BottomCap = ctCenter then
    begin
      v1 := NullVector;
      v3.Y := SinPc1;
      v4.Y := SinPc1;
    end
    else { BottomCap = ctFlat }
    begin
      v1.X := 0;
      v1.Y := SinPc1;
      v1.Z := 0;
      v3.Y := v1.Y;
      v4.Y := v1.Y;
    end;

    Theta := AngStart;
    NextTheta := Theta + StepH;

    for i := 0 to Slices - 1 do
    begin
      SinCos(Theta, SinT, CosT);
      if (Sign(SinT) = 1) or (tc2 = zCurve) then
        SinTc2 := Power(SinT, zCurve)
      else
        SinTc2 := -Power(-SinT, zCurve);

      SinCos(NextTheta, SinNT, CosNT);
      if (Sign(SinNT) = 1) or (tc2 = zCurve) then
        SinNTc2 := Power(SinNT, zCurve)
      else
        SinNTc2 := -Power(-SinNT, zCurve);

      if (Sign(CosT) = 1) or (tc2 = zCurve) then
        CosTc2 := Power(CosT, zCurve)
      else
        CosTc2 := -Power(-CosT, zCurve);

      if (Sign(CosNT) = 1) or (tc2 = zCurve) then
        CosNTc2 := Power(CosNT, zCurve)
      else
        CosNTc2 := -Power(-CosNT, zCurve);

      v3.X := CosPc1 * SinTc2;
      v3.Z := CosPc1 * CosTc2;
      v4.X := CosPc1 * SinNTc2;
      v4.Z := CosPc1 * CosNTc2;

      AddTriangle(v1, v4, v3, clrTurquoise);
      Theta := NextTheta;
      NextTheta := Theta + StepH;
    end;
  end;
  // main body
  Phi := AngBottom;
  NextPhi := Phi + StepV; { positive StepV; from bottom to top }

  for j := 0 to Stacks - 1 do
  begin
    Theta := AngStart;
    NextTheta := Theta + StepH; { positive StepH; from start to stop }

    SinCos(Phi, SinP, CosP);
    if (Sign(SinP) = 1) or (tc1 = xyCurve) then
      SinPc1 := Power(SinP, xyCurve)
    else
      SinPc1 := -Power(-SinP, xyCurve);

    v1.Y := SinPc1;
    v2.Y := SinPc1;

    SinCos(NextPhi, SinNP, CosNP);
    if (Sign(SinNP) = 1) or (tc1 = xyCurve) then
      SinNPc1 := Power(SinNP, xyCurve)
    else
      SinNPc1 := -Power(-SinNP, xyCurve);

    v3.Y := SinNPc1;
    v4.Y := SinNPc1;

    { define CopsPvc & CosNPc1 for Slices }
    if (Sign(CosP) = 1) or (tc1 = xyCurve) then
      CosPc1 := Power(CosP, xyCurve)
    else
      CosPc1 := -Power(-CosP, xyCurve);

    if (Sign(CosNP) = 1) or (tc1 = xyCurve) then
      CosNPc1 := Power(CosNP, xyCurve)
    else
      CosNPc1 := -Power(-CosNP, xyCurve);

    for i := 0 to Slices - 1 do
    begin
      SinCos(Theta, SinT, CosT);
      if (Sign(SinT) = 1) or (tc2 = zCurve) then
        SinTc2 := Power(SinT, zCurve)
      else
        SinTc2 := -Power(-SinT, zCurve);

      if (Sign(CosT) = 1) or (tc2 = zCurve) then
        CosTc2 := Power(CosT, zCurve)
      else
        CosTc2 := -Power(-CosT, zCurve);

      SinCos(NextTheta, SinNT, CosNT);
      if (Sign(SinNT) = 1) or (tc2 = zCurve) then
        SinNTc2 := Power(SinNT, zCurve)
      else
        SinNTc2 := -Power(-SinNT, zCurve);

      if (Sign(CosNT) = 1) or (tc2 = zCurve) then
        CosNTc2 := Power(CosNT, zCurve)
      else
        CosNTc2 := -Power(-CosNT, zCurve);

      v1.X := CosPc1 * SinTc2;
      v2.X := CosPc1 * SinNTc2;
      v3.X := CosNPc1 * SinTc2;
      v4.X := CosNPc1 * SinNTc2;

      v1.Z := CosPc1 * CosTc2;
      v2.Z := CosPc1 * CosNTc2;
      v3.Z := CosNPc1 * CosTc2;
      v4.Z := CosNPc1 * CosNTc2;

      AddTriangle(v1, v2, v3, clrTurquoise);
      AddTriangle(v2, v4, v3, clrTurquoise);

      Theta := NextTheta;
      NextTheta := Theta + StepH;
    end;
    Phi := NextPhi;
    NextPhi := Phi + StepV;
  end;

  // top cap
  if (Top < 90) and (TopCap in [ctCenter, ctFlat]) then
  begin
    SinCos(AngTop, SinP, CosP);

    if (Sign(SinP) = 1) or (tc1 = xyCurve) then
      SinPc1 := Power(SinP, xyCurve)
    else
      SinPc1 := -Power(-SinP, xyCurve);

    if (Sign(CosP) = 1) or (tc1 = xyCurve) then
      CosPc1 := Power(CosP, xyCurve)
    else
      CosPc1 := -Power(-CosP, xyCurve);

    if TopCap = ctCenter then
    begin
      v1 := NullVector;
      v3.Y := SinPc1;
      v4.Y := SinPc1;
    end
    else { FTopCap = ctFlat }
    begin
      v1.X := 0;
      v1.Y := SinPc1;
      v1.Z := 0;
      v3.Y := v1.Y;
      v4.Y := v1.Y;
    end;

    Theta := AngStart;
    NextTheta := Theta + StepH;

    for i := 0 to Slices - 1 do
    begin
      SinCos(Theta, SinT, CosT);
      if (Sign(SinT) = 1) or (tc2 = zCurve) then
        SinTc2 := Power(SinT, zCurve)
      else
        SinTc2 := -Power(-SinT, zCurve);

      SinCos(NextTheta, SinNT, CosNT);
      if (Sign(SinNT) = 1) or (tc2 = zCurve) then
        SinNTc2 := Power(SinNT, zCurve)
      else
        SinNTc2 := -Power(-SinNT, zCurve);

      if (Sign(CosT) = 1) or (tc2 = zCurve) then
        CosTc2 := Power(CosT, zCurve)
      else
        CosTc2 := -Power(-CosT, zCurve);

      if (Sign(CosNT) = 1) or (tc2 = zCurve) then
        CosNTc2 := Power(CosNT, zCurve)
      else
        CosNTc2 := -Power(-CosNT, zCurve);

      v3.X := CosPc1 * SinTc2;
      v3.Z := CosPc1 * CosTc2;
      v4.X := CosPc1 * SinNTc2;
      v4.Z := CosPc1 * CosNTc2;

      AddTriangle(v1, v3, v4, clrTurquoise);

      Theta := NextTheta;
      NextTheta := Theta + StepH;
    end;
  end; { Top Cap }

  GLMesh1.CalcNormals(fwCounterClockWise);
  GLHUDText1.Text := 'Scale:' + FloatToStrF(xRadiusTrackBar.Position / 10,
    ffNumber, 6, 2) + ', ' + FloatToStrF(yRadiusTrackBar.Position / 10,
    ffNumber, 6, 2) + ', ' + FloatToStrF(zRadiusTrackBar.Position / 10,
    ffNumber, 6, 2) + #13#10'xyCurve:' +
    FloatToStrF(xyCurveTrackBar.Position / 10, ffNumber, 6, 2) + #13#10'zCurve:'
    + FloatToStrF(zCurveTrackBar.Position / 10, ffNumber, 6, 2) +
    #13#10'Slices:' + IntToStr(SlicesTrackBar.Position) + #13#10'Stacks:' +
    IntToStr(StacksTrackBar.Position) + #13#10'Top:' +
    IntToStr(TopTrackBar.Position) + '°' + #13#10'Bottom:' +
    IntToStr(BottomTrackBar.Position) + '°' + #13#10'Start:' +
    IntToStr(StartTrackBar.Position) + '°' + #13#10'Stop:' +
    IntToStr(StopTrackBar.Position) + '°';
end;

procedure TMainForm.StartTrackBarChange(Sender: TObject);
begin
  if (StartTrackBar.Position >= StopTrackBar.Position) then
    StartTrackBar.Position := StopTrackBar.Position;
  ShowMesh;
end;

procedure TMainForm.StopTrackBarChange(Sender: TObject);
begin
  if (StopTrackBar.Position <= StartTrackBar.Position) then
    StopTrackBar.Position := StartTrackBar.Position;
  ShowMesh;
end;

end.
