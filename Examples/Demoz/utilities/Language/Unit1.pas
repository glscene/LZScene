unit Unit1; 

{$mode Delphi}{$H+}
{$HINTS OFF}

interface

uses LResources,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  Menus, StdCtrls, ExtCtrls, GLScene, GLObjects, GLSLanguage, GLLCLViewer;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button: TButton;
    GLSLanguage1: TGLSLanguage;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    AboutScene: TMemo;
    mDeutsch: TMenuItem;
    mLanguage: TMenuItem;
    mEnglish: TMenuItem;
    mRussian: TMenuItem;
    mOption: TMenuItem;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure mDeutschClick(Sender: TObject);
    procedure mEnglishClick(Sender: TObject);
    procedure mRussianClick(Sender: TObject);
  private
    { private declarations }
    procedure SetLanguage(const AFile: string);
  public
    { public declarations }
  end;


var
  Form1 :TForm1;

implementation

{$R *.lfm}

{ TForm1 }
procedure TForm1.SetLanguage(const AFile: string);
begin
  with GLSLanguage1 do
  begin
    LoadLanguageFromFile(AFile);
    mOption.Caption := Translate('mOption');
    mLanguage.Caption := Translate('mLanguage');
    mEnglish.Caption := Translate('mEnglish');
    mRussian.Caption := Translate('mRussian');
    GroupBox1.Caption := Translate('GroupBox1');
    Button.Caption := Translate('Button');
    label1.Caption := Translate('label');
    Panel1.Caption := Translate('Panel1');
    AboutScene.Clear;
    AboutScene.Lines.Add(Translate('AboutScene'));
  end;
end;

procedure TForm1.mRussianClick(Sender: TObject);
begin
  SetLanguage('Russian.ini');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetLanguage('English.ini');
end;

procedure TForm1.mEnglishClick(Sender: TObject);
begin
  SetLanguage('English.ini');
end;

procedure TForm1.mDeutschClick(Sender: TObject);
begin
  SetLanguage('Deutsch.ini');
end;

initialization
  {$I Unit1.lrs}

end.

