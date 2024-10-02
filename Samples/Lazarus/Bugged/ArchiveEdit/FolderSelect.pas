unit FolderSelect;

interface

uses {$IFDEF MSWINDOWS}Windows,{$ENDIF} SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, ShellCtrls;

type
  TFolderSel = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    ShellView: TShellTreeView;
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FolderSel: TFolderSel;

implementation

{$R *.lfm}

end.