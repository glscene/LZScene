////////////////////////////////////////////////////////////////////////
//                                                                    //
//  GLScene Fire Demo v1.0                                            //
//                                        - äëÿ íà÷èíàþùèõ è íà÷àâøèõ //
//====================================================================//
//                                                                    //
// äåìîíñòðàöèÿ ïîëó÷åíèÿ "ïðîñòîãî" îãíÿ                             //
//--------------------------------------------------------------------//
// • FireFX [TGLFireFXManager]                                        //
// • PointLightPFX [TGLPointLightPFXManager]                          //
// • PolygonPFX [TGLPolygonPFXManager]                                //
// • PerlinPFX [TGLPerlinPFXManager]                                  //
// • AnimatedSprite [TGLAnimatedSprite]                               //
//--------------------------------------------------------------------//
//                                                                    //
//  Ïðè èïîëüçîâàíèè ÷àñòèö îñíîâíàÿ ñëîæíîñòü - êó÷à íàñòðîåê,       //
//  à äëÿ íà÷èíàþùèõ åù¸ è ãîëîâíàÿ áîëü ñ ïîíÿòèåì "Effect"          //
//                                                                    //
//  ÷àñòèöû - ñïðàéòû, îáû÷íî ìàëåíüêèå è ìíîãî, êîòîðûå äâèãàþòñÿ ïî //
//  ïî îïðåäåëåííîìó çàêîíó, çàäàþùåìó òðàåêòîðèþ äëÿ êàæäîé ÷àñòèöû  //
//                                                                    //
// FireFX - êîìïîíåíò, çàäàþùèé çàêîí ðàñïðåäåëåíèÿ ïîäîáíûé ïëàìåíè  //
//  äëÿ åãî ðåàëèçàöèè íàäî ñîçäàòü TGLFireFXManager èç "GLScene PFX" //
//  è ïîäêëþ÷èòü ê íåìó GLCadencer, êîòîðûé áóäåò "òèêàòü"            //
//  òåïåðü íóæíî âûáðàòü "ãîðÿùèé" îáúåêò è â åãî ïàðàìåòðàõ âûáèðàåì //
//  Effects->Add->FireFX, äàëåå â Manager âûáèðàåì íàø...             //
//  ...âñ¸, îãîíü ñîçäàí è äîëæåí "ãîðåòü", îñòàëèñü íàñòðîéêè...     //
//                                                                    //
// PFX - ýôôåêòû ÷àñòèö, ò.å. ýòî íå îãîíü, à ïðîñòî òîëïà ÷àñòèö, íî //
//  ñ áîëåå øèðîêèì íàáîðîì íàñòðîåê è óëó÷øåííîé îïòèìèçàöèåé, ÷òî   //
//  ïîçâîëÿåò ñîçäàòü òîò æå ýôôåêò îãíÿ, íî áîëåå äèíàìè÷íûé         //
//  åñòü íåñêîëüêî ðåàëèçàöèé, íî ÿ èñïîëüçîâàë òðè îñíîâíûå:         //
//  PointLightPFX, PerlinPFX, PolygonPFX                              //
//  ïåðâûå äâå îäèíàêîâûå ñ òîé ëèøü ðàçíèöåé, ÷òî âî âòîðîì èñïîëü-  //
//  çóåòñÿ ãåíåðèðóåìûé Perlin-øóì êàê òåêñòóðà...                    //
//  òðåòèé - PolygonPFX - âèçóàëüíî óãëîâàòûé ìíîãîãðàííèê            //
//  äëÿ èõ èñïîëüçîâàíèÿ íóæíî ñäåëàòü ñëåäóþùåå:                     //
//  • Scene objects->Add object->Particle Systems->PFX Renderer       //
//  • ñîçäàåì íóæíûé Manager èç âêëàäêè "GLScene PFX"                 //
//  • ïîäêëþ÷àåì ê íåìó "PFX Renderer" è "òèêàþùèé" GLCadencer        //
//  • â îáúåêòå-"èñòî÷íèêå" Effects->Add->PFX Source                  //
//  âñ¸, "èñòî÷íèê" ñîçäàí, òåïåðü íàäî âûñòàâèòü ïàðàìåòðû =)        //
//                                                                    //
// AnimatedSprite - ýòî ñïðàéò, ó êîòîðîãî ìåíÿþòñÿ òåêñòóðíûå êîîð-  //
//  äèíàòû âî âðåìåíè ñ ïîâòîðåíèåì, ñîçäàâàÿ àíèìàöèþ/ìóëüòèê...     //
//  ýòî îãîíü, êîòîðûé ìîæíî íàðèñîâàòü èëè âûðåçàòü èç âèäåî ôàéëà   //
//                                                                    //
//--------------------------------------------------------------------//
//                                                                    //
// áûñòðåå âñåõ, åñòåñòâåííî, AnimatedSprite, íî òðåáóåò ïàìÿòü ïîä   //
// òåêñòóðó îãíÿ è åñòü ñëîæíîñòè â ðåàëèçàöèè àíèìàöèè "ãîðåíèÿ"     //
// FireFX óäîáíûé, íî òîðìîçíîé è âñå ÷àñòèöû - åäèíîå öåëîå          //
// PFX íå î÷åíü óäîáíûé äëÿ òåõ, êòî íå ëþáèò ëèøíèå íàñòðîéêè, íî    //
// äàåò øèðîêèå âîçìîæíîñòè ñ õîðîøåé îïòèìèçàöèåé                   //
//                                                                    //
//====================================================================//
//                                                                    //
// Óñïåõîâ â èçó÷åíèè!                                     GLScene.ru //
////////////////////////////////////////////////////////////////////////

unit Unit1;

{$MODE Delphi}

interface

uses
  Windows, Messages,
  SysUtils, Classes, Math,
  Graphics, Controls, Forms, Dialogs,
  //GLS
  GLCadencer, GLScene, GLObjects, GLAsyncTimer, GLGeomObjects, GLHUDObjects,
  GLTexture, GLVectorTypes, GLSpaceText, GLBitmapFont, GLWindowsFont,
  GLVectorGeometry, GLFireFX, GLParticleFX, GLPerlinPFX, GLAnimatedSprite,
  GLMaterial, GLCoordinates, GLCrossPlatform, GLLCLViewer, GLParticles,
  GLProjectedTextures, GLBaseClasses;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLCadencer2: TGLCadencer;
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLPointLightPFXManager1: TGLPointLightPFXManager;
    GLParticleFXRenderer1: TGLParticleFXRenderer;
    GLPerlinPFXManager1: TGLPerlinPFXManager;
    dc_plight: TGLDummyCube;
    dc_perlin: TGLDummyCube;
    dc_poly: TGLDummyCube;
    GLPolygonPFXManager1: TGLPolygonPFXManager;
    dc_fire: TGLDummyCube;
    GLDummyCube5: TGLDummyCube;
    GLFireFXManager1: TGLFireFXManager;
    AsyncTimer1: TGLAsyncTimer;
    asprite: TGLAnimatedSprite;
    matlib: TGLMaterialLibrary;
    txt_fire: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    txt_plight: TGLHUDText;
    txt_perlin: TGLHUDText;
    txt_poly: TGLHUDText;
    txt_asprite: TGLHUDText;
    dc_asprite: TGLDummyCube;
    GLLines1: TGLLines;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    txt_gl: TGLSpaceText;
    txt_scene: TGLSpaceText;
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure GLCadencer2Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure vpMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vpMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
  public
  end;

var
  Form1:TForm1;
  _shift:boolean=false; // èíäèêàòîð íàæàòèÿ ëþáîé êíîïêè "ìûøà"
  _mx,_my:Integer;      // ïðåäûäóùèå êîîðäèíàòû "ìûøà"
  _zoom:single=0;       //

implementation

{$R *.lfm}

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  _zoom:=WheelDelta/120; // çàïîìèíàåì ïîëîæåíèå êîë¸ñèêà "ìûøà"
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var v:TVector4f;
begin
  // ÿðëû÷îê äëÿ FireFX
  v:=vp.Buffer.WorldToScreen(dc_fire.AbsolutePosition);
    txt_fire.AbsolutePosition:=VectorMake(v.X,height-v.Y,0);
  // ÿðëû÷îê äëÿ PointLightPFX
  v:=vp.Buffer.WorldToScreen(dc_plight.AbsolutePosition);
    txt_plight.AbsolutePosition:=VectorMake(v.X,height-v.Y,0);
  // ÿðëû÷îê äëÿ PolygonPFX
  v:=vp.Buffer.WorldToScreen(dc_poly.AbsolutePosition);
    txt_poly.AbsolutePosition:=VectorMake(v.X,height-v.Y,0);
  // ÿðëû÷îê äëÿ PerlinPFX
  v:=vp.Buffer.WorldToScreen(dc_perlin.AbsolutePosition);
    txt_perlin.AbsolutePosition:=VectorMake(v.X,height-v.Y,0);
  // ÿðëû÷îê äëÿ AnimatedSprite
  v:=vp.Buffer.WorldToScreen(dc_asprite.AbsolutePosition);
    txt_asprite.AbsolutePosition:=VectorMake(v.X,height-v.Y,0);

  if _shift then begin
    gldummycube1.Pitch(_my-mouse.CursorPos.y); // åñëè íàæàòà êíîïêà, òî
    gldummycube5.Turn(_mx-mouse.CursorPos.x);  // âðàùåíèå êàìåðû îò "ìûøà"
    end
  else gldummycube5.Turn(deltatime*50); // èíà÷å àâòîìàòè÷åñêàÿ ðîòàöèÿ

    _my:=mouse.CursorPos.y; // ñîõðàíÿåì êîîðäèíàòû "ìûøà"
    _mx:=mouse.CursorPos.x; //

  GLCamera1.AdjustDistanceToTarget(Power(1.1, _zoom));

    _zoom:=0; // îáíóëÿåì

end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  caption:=vp.FramesPerSecondText(2); // âûâîäèì êîëè÷åñòâî êàäðîâ â ñåêóíäó
  vp.ResetPerformanceMonitor;         // è îáíóëÿåì ñ÷¸ò÷èê
end;

procedure TForm1.GLCadencer2Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin

end;

procedure TForm1.vpMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  _shift:=true; // êíîïêà íàæàòà
end;

procedure TForm1.vpMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  _shift:=false; // êíîïêà îòæàòà
end;

end.

