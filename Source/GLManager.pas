//
// The graphics engine GLXEngine. The unit of LZScene for Lazarus
//
{
   Managers are used to manage many different kinds of clients in GLScene.
   They are registered so that when clients are loaded, the client can
   look up the manager + register themselves with it.
}
unit GLManager;

interface

{$I GLScene.inc}

uses
  Classes, 
  Types;

procedure RegisterManager(aManager : TComponent);
procedure DeRegisterManager(aManager : TComponent);
function FindManager(classType : TComponentClass; const managerName : String) : TComponent;

implementation

var
   vManagers : TList;

procedure RegisterManager(aManager : TComponent);
begin
   if not Assigned(vManagers) then
      vManagers:=TList.Create;
   if vManagers.IndexOf(aManager)<0 then
      vManagers.Add(aManager);
end;

// DeRegisterManager
//
procedure DeRegisterManager(aManager : TComponent);
begin
   if Assigned(vManagers) then
      vManagers.Remove(aManager);
end;

// FindManager
//
function FindManager(classType : TComponentClass; const managerName : String) : TComponent;
var
   i : Integer;
begin
   Result:=nil;
   if Assigned(vManagers) then
      for i:=0 to vManagers.Count-1 do with TComponent(vManagers[i]) do
         if InheritsFrom(classType) and (Name=managerName) then begin
            Result:=TComponent(vManagers[i]);
            Break;
         end;
end;

initialization

finalization

   vManagers.Free;
   vManagers:=nil;

end.
