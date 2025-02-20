unit AboutFrm;

{$MODE objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Classes,{mail}
  Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ComCtrls;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    OKButton: TButton;
    RichEdit1: TMemo;
    CBLabel: TLabel;
    procedure OKButtonClick(Sender: TObject);
    procedure PrintBtnClick(Sender: TObject);
    procedure CBLabelClick(Sender: TObject);
    procedure GlsceneImageClick(Sender: TObject);
    procedure OpenglImageClick(Sender: TObject);
    procedure CopyrightClick(Sender: TObject);
  private
     
  public
     
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.lfm}

procedure TAboutBox.OKButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutBox.PrintBtnClick(Sender: TObject);
begin
 // RichEdit1.Print('Atomics Help');
end;

procedure TAboutBox.CBLabelClick(Sender: TObject);
begin
   OpenDocument('mailto:cboyd@mailandnews.com'); { *Converti depuis ShellExecute* }
end;

procedure TAboutBox.GlsceneImageClick(Sender: TObject);
begin
  OpenURL('http://www.glscene.org/'); { *Converti depuis ShellExecute* }
end;

procedure TAboutBox.OpenglImageClick(Sender: TObject);
begin
  OpenURL('http://www.opengl.org/'); { *Converti depuis ShellExecute* }
end;

procedure TAboutBox.CopyrightClick(Sender: TObject);
begin
   OpenDocument('mailto:ilh2o@ezl.com'); { *Converti depuis ShellExecute* }
end;

end.

