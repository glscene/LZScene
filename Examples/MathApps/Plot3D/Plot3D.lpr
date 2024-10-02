//
// Initial code by Eric Hardinge
//
program Plot3D;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Main in 'Main.pas' {ViewForm},
  Functions in 'Functions.pas' {FunctionsForm},
  GridOptions in 'GridOptions.pas' {GridOptionsForm},
  Evaluate in 'Evaluate.pas' {EvaluateForm},
  CoordOptions in 'CoordOptions.pas' {CoordsForm},
  DerivativeOptions in 'DerivativeOptions.pas' {DerivativesForm},
  GridColors in 'GridColors.pas' {GridColorsForm},
  PlotColors in 'PlotColors.pas' {PlotColorsForm},
  AddPlotColors in 'AddPlotColors.pas' {AddPlotColorsForm},
  uParser in 'uParser.pas',
  uGlobal in 'uGlobal.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TViewForm, ViewForm);
  Application.CreateForm(TFunctionsForm, FunctionsForm);
  Application.CreateForm(TGridOptionsForm, GridOptionsForm);
  Application.CreateForm(TEvaluateForm, EvaluateForm);
  Application.CreateForm(TCoordsForm, CoordsForm);
  Application.CreateForm(TDerivativesForm, DerivativesForm);
  Application.CreateForm(TGridColorsForm, GridColorsForm);
  Application.CreateForm(TPlotColorsForm, PlotColorsForm);
  Application.CreateForm(TAddPlotColorsForm, AddPlotColorsForm);
  Application.Run;
end.
