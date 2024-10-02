unit about;

interface

uses Windows, Classes, Graphics, Forms, Controls,
  StdCtrls, Buttons, ExtCtrls;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    OKButton: TButton;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    Comments: TLabel;
  private
     
  public
     
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.dfm}

end.
 
