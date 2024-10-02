unit FolderDialog;

{$MODE Delphi}

interface

uses {$IFDEF MSWINDOWS}Windows,{$ENDIF} SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TFDialog = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    Edit1: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FDialog: TFDialog;

implementation

{$R *.lfm}

end.