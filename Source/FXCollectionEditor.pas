//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  Edits a TGLXCollection
   History :  
     06/04/00 - Egg - Creation

}
unit FXCollectionEditor;

interface

{$I GLScene.inc}

uses
  lresources, 
  SysUtils,
  Forms, 
  ComCtrls,
  Controls, 
  Classes, 
  ActnList,
  Menus, 
  Dialogs, 
  PropEdits,
  GLBehaviours, 
  GLScene, 
  GLXCollection; 

type

  TGLXCollectionEditor = class(TForm)
    ListView: TListView;
    PMListView: TPopupMenu;
    ActionList: TActionList;
    ACRemove: TAction;
    ACMoveUp: TAction;
    ACMoveDown: TAction;
    ImageList: TImageList;
    MIAdd: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Moveup1: TMenuItem;
    Movedown1: TMenuItem;
    ToolBar1: TToolBar;
    TBAdd: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    PMToolBar: TPopupMenu;
    procedure ListViewClick(Sender: TObject);
    procedure TBAddClick(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure ACRemoveExecute(Sender: TObject);
    procedure ACMoveUpExecute(Sender: TObject);
    procedure ACMoveDownExecute(Sender: TObject);
    procedure PMToolBarPopup(Sender: TObject);
    procedure PMListViewPopup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
     
    FXCollection: TGLXCollection;
    updatingListView: boolean;
    procedure PrepareListView;
    procedure PrepareXCollectionItemPopup(AParent: TMenuItem);
    procedure OnAddXCollectionItemClick(Sender: TObject);
    procedure OnNameChanged(Sender: TObject);
    procedure OnXCollectionDestroyed(Sender: TObject);
  public
     
    procedure SetXCollection(aXCollection: TGLXCollection);
  end;

function XCollectionEditor: TGLXCollectionEditor;
procedure ReleaseXCollectionEditor;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

resourcestring
  cXCollectionEditor = 'XCollection editor';

var
  vXCollectionEditor: TGLXCollectionEditor;

function XCollectionEditor: TGLXCollectionEditor;
begin
  if not Assigned(vXCollectionEditor) then
    vXCollectionEditor := TGLXCollectionEditor.Create(nil);
  Result := vXCollectionEditor;
end;

procedure ReleaseXCollectionEditor;
begin
  if Assigned(vXCollectionEditor) then
  begin
    vXCollectionEditor.Release;
    vXCollectionEditor := nil;
  end;
end;

// FormCreate
procedure TGLXCollectionEditor.FormCreate(Sender: TObject);
begin
  RegisterGLBehaviourNameChangeEvent(OnNameChanged);
  RegisterXCollectionDestroyEvent(OnXCollectionDestroyed);
end;

// FormDestroy

procedure TGLXCollectionEditor.FormDestroy(Sender: TObject);
begin
  DeRegisterGLBehaviourNameChangeEvent(OnNameChanged);
  DeRegisterXCollectionDestroyEvent(OnXCollectionDestroyed);
end;

// FormHide

procedure TGLXCollectionEditor.FormHide(Sender: TObject);
begin
  SetXCollection(nil);
  ReleaseXCollectionEditor;
end;

// SetXCollection

procedure TGLXCollectionEditor.SetXCollection(aXCollection: TGLXCollection);
begin
  FXCollection := aXCollection;
  if Assigned(FXCollection) then
  begin
    Caption := FXCollection.GetNamePath;
  end
  else
  begin
    Caption := cXCollectionEditor;
  end;
  PrepareListView;
end;

// TBAddClick

procedure TGLXCollectionEditor.TBAddClick(Sender: TObject);
begin
  TBAdd.CheckMenuDropdown;
end;

procedure TGLXCollectionEditor.ListViewClick(Sender: TObject);
var
  sel: boolean;
begin
  if Assigned(GlobalDesignHook) and (not updatingListView) then
  begin
    // setup enablings
    sel := (ListView.Selected <> nil);
    TBAdd.Enabled := Assigned(GlobalDesignHook);
    ACRemove.Enabled := sel;
    ACMoveUp.Enabled := sel and (ListView.Selected.Index > 0);
    ACMoveDown.Enabled := sel and (ListView.Selected.Index < ListView.Items.Count - 1);
    if Assigned(GlobalDesignHook) then
      if sel then
        GlobalDesignHook.SelectOnlyThis(TGLXCollectionItem(ListView.Selected.Data))
      else
        GlobalDesignHook.SelectOnlyThis(nil);
  end;
end;

// ListViewChange

procedure TGLXCollectionEditor.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  sel: boolean;
begin
  if (Change = ctState) and Assigned(GlobalDesignHook) and (not updatingListView) then
  begin
    // setup enablings
    sel := (ListView.Selected <> nil);
    TBAdd.Enabled := Assigned(GlobalDesignHook);
    ACRemove.Enabled := sel;
    ACMoveUp.Enabled := sel and (ListView.Selected.Index > 0);
    ACMoveDown.Enabled := sel and (ListView.Selected.Index < ListView.Items.Count - 1);
    if Assigned(GlobalDesignHook) then
      if sel then
        GlobalDesignHook.SelectOnlyThis(TGLXCollectionItem(ListView.Selected.Data))
      else
        GlobalDesignHook.SelectOnlyThis(nil);
  end;
end;

// PrepareListView

procedure TGLXCollectionEditor.PrepareListView;
var
  i: integer;
  prevSelData: Pointer;
  XCollectionItem: TGLXCollectionItem;
begin
  Assert(Assigned(ListView));
  updatingListView := True;
  try
    if ListView.Selected <> nil then
      prevSelData := ListView.Selected.Data
    else
      prevSelData := nil;
    with ListView, ListView.Items do
    begin
      BeginUpdate;
      Clear;
      if Assigned(FXCollection) then
      begin
        for i := 0 to FXCollection.Count - 1 do
          with Add do
          begin
            XCollectionItem := FXCollection[i];
            Caption := Format('%d - %s', [i, XCollectionItem.Name]);
            SubItems.Add(XCollectionItem.FriendlyName);
            Data := XCollectionItem;
          end;
        if prevSelData <> nil then
          ListView.Selected := ListView.Items.FindData(prevSelData);
      end;
      EndUpdate;
    end;
  finally
    updatingListView := False;
  end;
  ListViewChange(Self, nil, ctState);
end;

// PrepareXCollectionItemPopup

procedure TGLXCollectionEditor.PrepareXCollectionItemPopup(AParent: TMenuItem);
var
  i: integer;
  list: TList;
  XCollectionItemClass: TGLXCollectionItemClass;
  mi: TMenuItem;
begin
  list := GetXCollectionItemClassesList(FXCollection.ItemsClass);
  try
    AParent.Clear;
    for i := 0 to list.Count - 1 do
    begin
      XCollectionItemClass := TGLXCollectionItemClass(list[i]);
      mi := TMenuItem.Create(owner);
      mi.Caption := XCollectionItemClass.FriendlyName;
      mi.OnClick := OnAddXCollectionItemClick;
      mi.Tag := integer(XCollectionItemClass);
      mi.Enabled := Assigned(FXCollection) and FXCollection.CanAdd(XCollectionItemClass);
      AParent.Add(mi);
    end;
  finally
    list.Free;
  end;
end;

// OnNameChanged

procedure TGLXCollectionEditor.OnNameChanged(Sender: TObject);
begin
  if TGLXCollectionItem(Sender).Owner = FXCollection then
    PrepareListView;
end;

// OnXCollectionDestroyed

procedure TGLXCollectionEditor.OnXCollectionDestroyed(Sender: TObject);
begin
  if TGLXCollection(Sender) = FXCollection then
    Close;
end;

// OnAddXCollectionItemClick

procedure TGLXCollectionEditor.OnAddXCollectionItemClick(Sender: TObject);
var
  XCollectionItemClass: TGLXCollectionItemClass;
  XCollectionItem: TGLXCollectionItem;
begin
  XCollectionItemClass := TGLXCollectionItemClass(PtrUInt((Sender as TMenuItem).Tag));
  XCollectionItem := XCollectionItemClass.Create(FXCollection);
  PrepareListView;
  ListView.Selected := ListView.Items.FindData(XCollectionItem);
  GlobalDesignHook.Modified(Sender);
end;

// ACRemoveExecute

procedure TGLXCollectionEditor.ACRemoveExecute(Sender: TObject);
begin
  if ListView.Selected <> nil then
  begin
    GlobalDesignHook.Modified(Sender);
    GlobalDesignHook.SelectOnlyThis(nil);
    TGLXCollectionItem(ListView.Selected.Data).Free;
    ListView.Selected.Free;
    ListViewChange(Self, nil, ctState);
  end;
end;

// ACMoveUpExecute

procedure TGLXCollectionEditor.ACMoveUpExecute(Sender: TObject);
begin
  if ListView.Selected <> nil then
  begin
    TGLXCollectionItem(ListView.Selected.Data).MoveUp;
    PrepareListView;
    GlobalDesignHook.Modified(Sender);
  end;
end;

// ACMoveDownExecute

procedure TGLXCollectionEditor.ACMoveDownExecute(Sender: TObject);
begin
  if ListView.Selected <> nil then
  begin
    TGLXCollectionItem(ListView.Selected.Data).MoveDown;
    PrepareListView;
    GlobalDesignHook.Modified(Sender);
  end;
end;

// PMToolBarPopup

procedure TGLXCollectionEditor.PMToolBarPopup(Sender: TObject);
begin
  PrepareXCollectionItemPopup(PMToolBar.Items);
end;

// PMListViewPopup

procedure TGLXCollectionEditor.PMListViewPopup(Sender: TObject);
begin
  PrepareXCollectionItemPopup(MIAdd);
end;

initialization

  {$i FXCollectionEditor.lrs}

finalization
  ReleaseXCollectionEditor;

end.
