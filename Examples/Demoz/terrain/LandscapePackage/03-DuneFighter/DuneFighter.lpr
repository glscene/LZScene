{
  This demo show how a cyclic fractal HDS can be used with an actor. The code
  is based on the demo ActorTwoCams by Carlos Artaega Rivero.

  Alexandre Hirzel, July 2003

  Actor movement with two cameras (first-person and third-person)<p>
  The movement control is a little "doom-like" and keyboard only.<br>
  This demos mainly answers to "doom-like" movement questions and keyboard
  handling in GLScene.<p>
  The basic principle is to check which key are pressed, and for each movement
  key, multiply the movement by the deltaTime and use this value as delta
  position or angle.<p>
  The frame rate may not be that good on non-T&L accelerated board, mainly due
  to the mushrooms that are light on fillrate needs, but heavy on the polygons.<br>
  This demonstrates how badly viewport object-level clipping is needed in
  GLScene :), a fair share of rendering power is lost in projecting
  objects that are out of the viewing frustum.<p>

  TODO : 3rd person view with quaternion interpolation (smoother mvt)
  More mvt options (duck, jump...)
  Smooth animation transition for TGLActor
  HUD in 1st person view

  Carlos Arteaga Rivero <carteaga@superele.gov.bo>
}
program DuneFighter;

{$MODE Delphi}

uses
  Forms, Interfaces,
  fDuneFighterU in 'fDuneFighterU.pas' {fDuneFighter};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfDuneFighter, fDuneFighter);
  Application.Run;
end.
