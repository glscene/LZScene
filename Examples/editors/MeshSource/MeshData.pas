unit MeshData;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmMeshData = class(TForm)
    rdMeshData: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowMeshData(const aList : TStringList);

implementation

{$IFnDEF FPC}
  {$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure ShowMeshData(const aList : TStringList);
var
  lfrm : TfrmMeshData;
begin
  if aList = nil then
    Exit;
    
  lfrm := TfrmMeshData.Create(nil);
  try
    lfrm.rdMeshData.Lines.Assign(aList);
    lfrm.ShowModal;
  finally
    FreeAndNil(lfrm);
  end;
end;

end.
