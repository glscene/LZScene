unit Unit1;

{$MODE Delphi}

interface

uses
  LCLType, SysUtils, Classes, Math,
  Graphics, Controls, Forms, Dialogs,
  GLCadencer, GLScene, GLObjects, GLAsyncTimer,
  GLGeomObjects, GLTexture, GLHUDObjects,
  GLSpaceText, GLVectorGeometry,
  GLParticleFX, GLPerlinPFX, GLKeyboard, GLBitmapFont, GLWindowsFont,
  GLCoordinates, GLCrossPlatform, GLLCLViewer;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLParticleFXRenderer1: TGLParticleFXRenderer;
    AsyncTimer1: TGLAsyncTimer;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    txt_gl: TGLSpaceText;
    txt_scene: TGLSpaceText;
    GLPointLightPFXManager1: TGLPointLightPFXManager;
    GLPointLightPFXManager2: TGLPointLightPFXManager;
    DC_emitter: TGLDummyCube;
    GLPerlinPFXManager1: TGLPerlinPFXManager;
    GLHUDText1: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure vpMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure vpMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure CharType(ntype: integer);
  end;

var
  Form1: TForm1;
  _type: integer = 0;      // effect type - rain/snow/fog
  _shift: boolean = False; // indicator of any mouse button pressing
  _mx: integer;          // prev mouse button
  _zoom: single = 0;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  glHUDtext1.Text := '1 rain'#13 + '2 snow'#13 + '3 fog';
  CharType(0);
end;

procedure TForm1.CharType(ntype: integer);
var
  a: integer;
begin
  _type := ntype; // remember selected effect
  for a := 0 to 2 do
    // switch off unnecessary effects and switch on selected one
    with TGLSourcePFXEffect(GLDummyCube1.Effects[a]) do
      if _type = a then
        Enabled := True
      else
        Enabled := False;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  _zoom := WheelDelta / 120; // remember position of mouse wheel
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  if _shift then
    GLDummyCube1.Turn(_mx - mouse.CursorPos.x) // rotating camera by mouse
  else
    GLDummyCube1.Turn(deltatime * 10);       // othrwise auto rotation

  _mx := Mouse.CursorPos.x; // çàïîìèíàåì êîîðäèíàòó

  GLCamera1.AdjustDistanceToTarget(Power(1.1, _zoom)); // ïðèáëèæàåìñÿ-óäàëÿåìñÿ
  _zoom := 0;

  if IsKeyDown('1') then
    CharType(0); // rain
  if IsKeyDown('2') then
    CharType(1); // snow
  if IsKeyDown('3') then
    CharType(2); // fog
  if IsKeyDown(VK_ESCAPE) then
    Close;

  case _type of
    // ïîäñòðîéêè "äîæäèêà" ê âðàùåíèþ êàìåðû
    0: with TGLSourcePFXEffect(GLDummyCube1.Effects[0]) do
      begin
        // êîîðäèíàòû ýìèòòåðà
        InitialPosition.AsVector := dc_emitter.AbsolutePosition;
        // íàïðàâëåíèå ê öåíòðó
        InitialVelocity.AsVector :=
          VectorScale(VectorNormalize(VectorNegate(dc_emitter.AbsolutePosition)), 10);
        // ïîâîðà÷èâàåì ÷àñòèöû ê öåíòðó
        GLPointLightPFXManager1.Rotation := -arctan(InitialVelocity.X / InitialVelocity.Y);
      end;
    // ïîäñòðîéêè "ñíåæêà" îòíîñèòåëüíî âðàùåíèÿ êàìåðû
    1: with TGLSourcePFXEffect(GLDummyCube1.Effects[1]) do
      begin
        // êîîðäèíàòû ýìèòòåðà
        InitialPosition.AsVector := dc_emitter.AbsolutePosition;
        // íàïðàâëÿåì ê öåíòðó
        InitialVelocity.AsVector :=
          VectorScale(VectorNormalize(VectorNegate(dc_emitter.AbsolutePosition)), 4);
      end;
  end;
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  Form1.Caption := vp.FramesPerSecondText(2); // âûâîäèì êîëè÷åñòâî êàäðîâ â ñåêóíäó
  vp.ResetPerformanceMonitor;         // è îáíóëÿåì ñ÷¸ò÷èê
end;

procedure TForm1.vpMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  _shift := True; // pressed button
end;

procedure TForm1.vpMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  _shift := False; // êíîïêà îòæàòà
end;

end.
