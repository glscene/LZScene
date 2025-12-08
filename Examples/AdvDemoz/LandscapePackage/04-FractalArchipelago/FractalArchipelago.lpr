{
  This demo illustrate the use of the tFractalArchipelago, an infinite random
  landscape made of fractal islands. The crucial procedures are :
  - FormCreate: Landscape instanciation, hooking to TerrainRenderer and texture loading
  - FormActivate: Setting up landscape parameters and initialisation
  - AsyncTimer1Timer: Updating
  - OnDrawTexture: Land-cover drawing. This is where you define which texture is
  used depending on elevation, slope, aspect and position.

  Please consider testing the FractalLandscape first to understand how fractal
  landscapes are built and what the various parameters are controlling.

  Alexandre Hirzel, (c) June 2003

}
program FractalArchipelago;

{$MODE Delphi}

uses
  Forms, Interfaces,
  dlgFracArchipU in 'dlgFracArchipU.pas' {dlgFracArchip};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdlgFracArchip, dlgFracArchip);
  Application.Run;
end.
