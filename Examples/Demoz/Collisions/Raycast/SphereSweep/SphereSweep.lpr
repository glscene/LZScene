//******************************************************************************
//  SphereSweepAndSlide - by Dan Bartlett
//  Shows how to use OctreeSphereSweepIntersect to perform collisions between
//  a moving sphere and a freeform
//----------------------------------------
//  STILL A WORK IN PROGRESS (WIP)
//  known problems:
//    1)  Gravity is not implemented (constant fall speed)
//    2)  Going to "the gap" and pressing space can give problems
//    3)  Still need to implement options
//----------------------------------------
//  Controls:
//    W,A,S,D: Movement
//    Mouse: Movement
//    F2, F3: First person, Third person
//    F5: Toggle wireframe
//    Space: Move upwards
//    Esc: Quit
//******************************************************************************

program SphereSweep;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Unit1 in 'Unit1.pas' {Form1};

{.$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
