////////////////////////////////////////////////////////////////////////
//  GLScene Rain Demo v1.0                                            //
//====================================================================//
//  äåìîíñòðàöèÿ ïîëó÷åíèÿ "ïðîñòîãî" äîæäÿ, ñíåãà è òóìàíà           //
//--------------------------------------------------------------------//
//                                                                    //
//  Ïðè èïîëüçîâàíèè ÷àñòèö îñíîâíàÿ ñëîæíîñòü - êó÷à íàñòðîåê,       //
//  ãäå íà÷èíàþùèì âñ¸ êàæåòñÿ ëèøíèì è íåïîíÿòíûì...                 //
//                                                                    //
// «Äîæäü»                                                            //
//  èñïîëüçóÿ PointLightPFX c AspectRatio â 0,05 ïîëó÷àåì ëèíèþ, ïîõî //
//  æóþ íà êàïëþ â äâèæåíèè, åñëè çàäàòü íóæíûé óãîë (Rotation), ñêî  //
//  ðîñòü (PFX Source->InitialVelocity) è ðàçáðîñ (PFX Source->Positi //
//  onDispersion), òî ïîëó÷àåòñÿ íåïëîõîé ýôôåêò äîæäÿ =)             //
//  äëÿ ñîçäàíèÿ ýôôåêòà ÷àñòèö íàäî:                                 //
//  • Scene objects->Add object->Particle Systems->PFX Renderer       //
//  • ñîçäàåì PointLightPFXManager èç âêëàäêè "GLScene PFX"           //
//  • ïîäêëþ÷àåì ê íåìó "PFX Renderer" è ðàáîòàþùèé GLCadencer        //
//  • â îáúåêòå-"èñòî÷íèêå" äîáàâëÿåì Effects->Add->PFX Source        //
//  âñ¸, "èñòî÷íèê" ñîçäàí, òåïåðü íàäî âûñòàâèòü ïàðàìåòðû:          //
//  • PointLightPFXManager->AspectRatio=0.05 òîíêèå ÷àñòèöû           //
//  • PointLightPFXManager->ParticleSize=0.1 äåëàåì èõ ïîìåëü÷å       //
//  • PointLightPFXManager->ColorInner=[1,1,1,0.5] ïîëóïðîçðà÷íûå     //
//  • PFX Source->ParticleInterval=0.001 ÷åì ìåíüøå, òåì áîëüøå       //
//  • PFX Source->PositionDispersionRange=[1,0,1] ðàçáðîñ ïî îñÿì     //
//  • PFX Source->PositionDispersion=5 ìíîæèòåëü ðàçáðîñà             //
//  Ïàðàìåòðû âûñòàâëåíû, òåïåðü ÷åðåç GLCadencer áóäåì óïðàâëÿòü     //
//  PFX Source->InitialPosition è PFX Source->InitialVelocity,        //
//  çàäàâàÿ íóæíûé óãîë ÷àñòèö ÷åðåç PointLightPFXManager->Rotation   //
//                                                                    //
// «Ñíåã»                                                             //
//  ñîçäàíèå ñíåãà àíàëîãè÷íî ñîçäàíèþ "Äîæäÿ":                       //
//  • ñîçäàåì PointLightPFXManager èç âêëàäêè "GLScene PFX"           //
//  • ïîäêëþ÷àåì ê íåìó "PFX Renderer" è ðàáîòàþùèé GLCadencer        //
//  • â îáúåêòå-"èñòî÷íèêå" äîáàâëÿåì Effects->Add->PFX Source        //
//  òåïåðü âûñòàâëÿåì ïàðàìåòðû:                                      //
//  • PointLightPFXManager->AspectRatio=0.5 ëåãêîå èñêàæåíèå ôîðìû    //
//  • PointLightPFXManager->ParticleSize=0.1 äåëàåì ïîìåëü÷å          //
//  • PointLightPFXManager->ColorInner=clrWhite áåëûå ïóøèíêè         //
//  • PFX Source->ParticleInterval=0.001 ÷åì ìåíüøå, òåì áîëüøå       //
//  • PFX Source->PositionDispersionRange=[1,0,1] ðàçáðîñ ïî îñÿì     //
//  • PFX Source->PositionDispersion=5 ìíîæèòåëü ðàçáðîñà             //
//  • PFX Source->VelocityDispersion=1 ðàçë¸ò ÷àñòèö                  //
//  ïàðàìåòðû âûñòàâëåíû, à ÷åðåç GLCadencer òàêæå áóäåì óïðàâëÿòü    //
//  PFX Source->InitialPosition è PFX Source->InitialVelocity         //
//                                                                    //
// «Òóìàí»                                                            //
//  êëóáû ïàðà - ñîçäàíû èñïîëüçóÿ PerlinPFX äëÿ ïðèäàíèÿ äåòàëèçàöèè //
//  ñîçäàíèå ïîäîáíî ïåðäûäóùèì:                                      //
//  • ñîçäàåì PerlinPFXManager èç âêëàäêè "GLScene PFX"               //
//  • ïîäêëþ÷àåì ê íåìó "PFX Renderer" è ðàáîòàþùèé GLCadencer        //
//  • â îáúåêòå-"èñòî÷íèêå" äîáàâëÿåì Effects->Add->PFX Source        //
//  âûñòàâëÿåì ïàðàìåòðû:                                             //
//  • PointLightPFXManager->Brightness=0.2 ïðèãëóøàåì ÿðêîñòü         //
//  • PointLightPFXManager->ColorMode=scmFade áîëåå ìÿãêàÿ òåêñòóðêà  //
//  • PointLightPFXManager->ColorInner=[1,1,1,0.5] ÷óòü ïðîçðà÷íîñòè  //
//  • PointLightPFXManager->ParticleSize=1.5 êðóïíûå êëóáû ïàðà       //
//  • PFX Source->InitialPosition=[0,-3,0] ýìèòòåð ñîçäàåì âíèçó      //
//  • PFX Source->InitialVelocity=[0,0.5,0] è íàïðàâëÿåì ââåðõ        //
//  • PFX Source->ParticleInterval=0.005 ÷åì ìåíüøå, òåì õóæå         //
//  • PFX Source->PositionDispersionRange=[4,0,1] ðàçáðîñ âäîëü ëèíèè //
//  • PFX Source->RotationDispersion=1 íåìíîãî âðàùåíèÿ               //
//  • PFX Source->VelocityDispersion=1 ðàçë¸ò ÷àñòèö                  //
//                                                                    //
//====================================================================//
//  GLScene.ru                                                        //
////////////////////////////////////////////////////////////////////////

program WeatherEffects;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
