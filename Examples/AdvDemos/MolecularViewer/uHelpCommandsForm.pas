unit uHelpCommandsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { THelpCommandsForm }

  THelpCommandsForm = class(TForm)
    btnClose: TButton;
    Memo1: TMemo;
    procedure btnCloseClick(Sender: TObject);
  private

  public

  end;

var
  HelpCommandsForm: THelpCommandsForm;

implementation

{$R *.lfm}

{ THelpCommandsForm }

procedure THelpCommandsForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.

