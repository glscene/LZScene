program dyntexdemo;

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, GLScene_RunTime, Unit1;

begin
  Application.Title:='DynamicTextureDemo';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
