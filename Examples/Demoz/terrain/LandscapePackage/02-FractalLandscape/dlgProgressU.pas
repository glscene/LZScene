unit dlgProgressU;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls;

type

  { TdlgProgress }

  TdlgProgress = class(TForm)
    lblTask: TLabel;
    ggTaskProgress: TProgressBar;
    timTask: TTimer;
    procedure timTaskTimer(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
     
  public
    procedure Execute;
  end;

var
  dlgProgress: TdlgProgress;

implementation

uses dlgFracLanU;

{$R *.lfm}

procedure TdlgProgress.Execute;
begin
  Show;
  timTask.Enabled:=True;
end;

procedure TdlgProgress.timTaskTimer(Sender: TObject);
begin
  lblTask.Caption:=dlgFracLan.FractalHDS.Task;
  ggTaskProgress.Position:=dlgFracLan.FractalHDS.TaskProgress;
end;

procedure TdlgProgress.FormHide(Sender: TObject);
begin
  timTask.Enabled:=False;
  lblTask.Caption:='';
  ggTaskProgress.Position:=0;
end;

end.
