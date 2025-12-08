{Converted into GlScene demo
 by Ivan Lee Herring,Jan 2003  ilh2o@ezl.com from many demos: }
{Column.dpr: runtime object creation and framerate independant motion.}
{Caterpillar.dpr: use of TGLSprite for "caterpillar" effect}
{Lines from boxedin.dpr: Robert Hayes, March 2002.}
{Explosion FX Demo (Matheus, matheus@tilt.net)<p>
 This project demonstrates the use of TGLBExplosionFx. Nothing out
 of ordinary as one can see. Load the mesh, load the default settings,
 click "e" to initiate the demo, counter used to auto reset :)<p>

 The information of the mesh is cached on the cache variable, that is
 restored every time the demo is reset. The MaxSteps property defines
 the max number of frames the explosion will be rendered. Speed is the
 scalar speed each face is issued in the rendering}
(******************************************************************
Author: Charles Boyd (cboyd@mailandnews.com) - Texas Tech University
File: atomizer.cpp
Project: Atomizer
Date: 12-07-2000
- note -
Anaglyph Code adapted from Chris Stanley
(cstanley@ttacs.ttu.edu) - Texas Tech Univeristy
Project: Real-Time Shared Environment
- Description -
This program displays the shell structure of all known elements
found in the periodic table.
*****************************************************************)

program Atomics;

{$MODE objfpc}{$H+}

uses
  Forms, Interfaces,
  AtomsFrm in 'AtomsFrm.pas' {AAtomForm},
  AboutFrm in 'AboutFrm.pas' {AboutBox},
  AtomicRotationFrm in 'AtomicRotationFrm.pas' {AtomicRotationForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Atomics';
  Application.CreateForm(TAAtomForm, AAtomForm);
  Application.CreateForm(TAtomicRotationForm, AtomicRotationForm);
  Application.Run;
end.
