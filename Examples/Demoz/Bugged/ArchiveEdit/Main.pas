{
  <b>History : </b><font size=-1><ul>
  <li>01/02/11 - Yar - Added file preview (thaks Dev)
  <li>04/06/10 - Predator - Created
  </ul>
}

unit Main;

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}LCLType, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, Menus, ImgList, ExtCtrls, ComCtrls,
  GLSArchiveManager, GLFilePAK, GLFileZLIB, GLLCLViewer, GLCrossPlatform,
  GLBaseClasses, GLScene, GLSimpleNavigation, GLMaterial, GLVectorFileObjects,
  GLObjects, GLCoordinates,GLGraphics, GLState, GLCompositeImage,
  //FileFormats 3D
  GLFileMS3D, GLFile3DS, GLFileMD2, GLFileMD3, GLFileLMTS, GLFileOBJ, GLFileSMD,
  //FileFormats 3D
  GLFileJPEG, TGA, GLFilePNG,GLFileDDS;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    ShowPreView: TMenuItem;
    View: TMenuItem;
    TreeView: TTreeView;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ImageList1: TImageList;
    ListView: TListView;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Bevel1: TBevel;
    Edit1: TMenuItem;
    Createfolder1: TMenuItem;
    Addfiles1: TMenuItem;
    Deleteselectedfile1: TMenuItem;
    Deleteselectedfolder1: TMenuItem;
    N2: TMenuItem;
    Extractselectedfiles1: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    Compression1: TMenuItem;
    Max1: TMenuItem;
    None1: TMenuItem;
    Fast1: TMenuItem;
    Default1: TMenuItem;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLSprite1: TGLSprite;
    GLFreeForm1: TGLFreeForm;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLSArchiveManager1: TGLSArchiveManager;
    GLCube1: TGLCube;
    procedure FormCreate(Sender: TObject);
    procedure ShowPreViewClick(Sender: TObject);
    procedure TreeViewRefresh;
    procedure FileListRefresh;
    procedure AddNode(text: string; node: TTreeNode);
    function TreeIndexOf(s: string; node: TTreeNode): integer;
    function ListIndexOf(s: string): integer;
    procedure TreeViewCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure ListViewClick(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Createfolder1Click(Sender: TObject);
    procedure Addfiles1Click(Sender: TObject);
    procedure Deleteselectedfile1Click(Sender: TObject);
    procedure Deleteselectedfolder1Click(Sender: TObject);
    procedure TreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Extractselectedfiles1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure None1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  CurPath,
  Selection: string;
  ArchiveManager: TGLSArchiveManager;
  Archive: TLibArchive;
  vMenu: TMenuItem;

implementation

uses FolderDialog, FolderSelect,FileUtil;

{$R *.lfm}
{.$R icons.res}


procedure TForm1.AddNode(text: string; node: TTreeNode);
var
   s: string;
   c: integer;
begin
   c:=Pos('/',Text);
   if c=0 then exit;
   s:=Copy(text,1,c-1);
   Delete(text,1,c);
   if TreeIndexOf(s,node)=-1 then
   AddNode(text, TreeView.Items.AddChild(node,s)) else
   AddNode(text, node[TreeIndexOf(s,node)]);
end;

function TForm1.TreeIndexOf(s: string; node: TTreeNode): integer;
var
   i: integer;
begin
   Result:=-1;
      for i:=0 to node.Count-1 do
         if node[i].Text=s then Result:=i;
end;

function TForm1.ListIndexOf(s: string): integer;
var
   i: integer;
begin
   Result:=-1;
      for i:=0 to ListView.Items.Count-1 do
         if ListView.Items[i].Caption=s then Result:=i;
end;

procedure TForm1.TreeViewRefresh;
var
   i: integer;
begin
   with TreeView do begin
      Items.Clear;  
      Items.AddChild(Items.GetFirstNode,ExtractFileName(Archive.FileName));
      for i:=0 to Archive.ContentList.Count-1 do
         AddNode(Archive.ContentList[i],Items.GetFirstNode);
      Items[0].Expanded:=True;
      Items[0].SelectedIndex:=1;
      Items[0].ImageIndex:=1;
   end;
   TreeView.Selected:=TreeView.Items.GetFirstNode;
end;

function MakeMemSize(size: integer): string;
const
   kb=1024;
   mb=kb*kb;
   gb=mb*kb;
begin
   case size of
      0..kb-1:Result:=IntToStr(size)+' B';
      kb..mb-1:Result:=Format('%.2f KB',[size/kb]);
      mb..gb-1:Result:=Format('%.2f MB',[size/mb]);
   else
      Result:=Format('%.2f GB',[size/gb]);
   end;
end;

procedure TForm1.FileListRefresh;
var
   i, j: integer;
   s, name: string;
   n: TTreeNode;
   dir: boolean;
begin
   n:=TreeView.Selected;
   s:=n.Text;
   if s=ExtractFileName(Archive.FileName) then s:='';
   while Assigned(n.parent) do begin
      n:=n.Parent;
      if n.AbsoluteIndex<>0 then
      s:=n.Text+'/'+s;
   end;
   if s<>'' then s:=s+'/';
   CurPath:=s;

   ListView.Clear;
   for i:=0 to Archive.ContentList.Count-1 do
      if Copy(Archive.ContentList[i],1,Length(s))=s then begin
         name:=RightStr(Archive.ContentList[i],Length(Archive.ContentList[i])-Length(s));
         j:=Pos('/',name);
         if j=0 then
            Dir:=False else
            Dir:=True;

         if Dir=True then
            name:=Copy(name,1,j-1);
         if ListIndexOf(name)=-1 then
         with ListView.Items.Add do begin
            Caption:=Name;
            if Dir=True then begin
                  SubItems.Add('-');
                  SubItems.Add('-')
               end else begin
                  SubItems.Add(MakeMemSize(Archive.GetContentSize(i)));
                  SubItems.Add(MakeMemSize(Archive.GetContentSize(i)))
               end;

            if Dir=True then
               ImageIndex:=0 else
               ImageIndex:=2;
         end;
      end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
   Bmp: TBitmap;
begin
   Bmp:=TBitmap.Create;
  // Bmp.LoadFromResourceName(HInstance, 'icons');
   Bmp.LoadFromFile('icons.bmp');
   ImageList1.AddMasked(Bmp,clWhite);
   Bmp.Free;  
   ArchiveManager:= TGLSArchiveManager.Create(Self);
   Archive := ArchiveManager.Archives.Add;
   vMenu := None1;
end;

procedure TForm1.ShowPreViewClick(Sender: TObject);
begin
  if not ShowPreView.Checked then
  begin
    GLCube1.Visible:=false;
    GLFreeForm1.Visible:=false;
  end;
end;

procedure TForm1.TreeViewCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
   if Node.AbsoluteIndex=0 then
   AllowCollapse:=False else
   AllowCollapse:=True;
end;

procedure TForm1.ListViewClick(Sender: TObject);
var
   s: string;
   len,x:Byte;
   strm: TStream;
   img: TGLCompositeImage;
   objSize:Single;
begin
   if not Assigned(ListView.Selected) then Exit;
   s:=ListView.Selected.Caption;
   Selection:=CurPath+s;

   if ShowPreView.Checked then
   if ListView.Selected.ImageIndex=2 then
     begin
       len:=Length(s);
       s:=LowerCase(s);
       if (Copy(s,len-3,5)='ms3d') or (Copy(s,len-2,5)='3ds') or (Copy(s,len-2,5)='md2') or (Copy(s,len-2,5)='md3') or (Copy(s,len-2,5)='obj') or (Copy(s,len-3,5)='lmts') or (Copy(s,len-2,5)='smd') then
         begin
           GLFreeForm1.LoadFromStream(Selection,Archive.GetContent(Selection));
           GLCube1.Visible:=false;
           GLFreeForm1.Visible:=True;
           GLCamera1.Position.SetPoint(30,40,50);

           objSize:=GLFreeForm1.BoundingSphereRadius;
           if objSize>0 then
             begin
               if objSize<1 then
                 begin
                   GLCamera1.SceneScale:=1/objSize;
                   objSize:=1;
                 end
                 else GLCamera1.SceneScale:=1;
               GLCamera1.AdjustDistanceToTarget(objSize*0.12);
               GLCamera1.DepthOfView:=1.5*GLCamera1.DistanceToTarget+1*objSize;
             end;

         end;
       if (Copy(s,len-2,5)='jpg') or (Copy(s,len-2,5)='dds') {or (Copy(s,len-2,5)='tga')} or (Copy(s,len-2,5)='png') then
         begin
           strm := Archive.GetContent(Selection);
           img := GLMaterialLibrary1.TextureByName('image').Image as TGLCompositeImage;
           img.LoadFromStream(strm);

           GLCube1.Material.LibMaterialName:='image';
           GLCube1.Visible:=True;
           GLFreeForm1.Visible:=false;
           GLCamera1.Position.SetPoint(3,4,5);
         end;
     end;
end;

procedure TForm1.ListViewDblClick(Sender: TObject);
var
   s: string;
   n: TTreeNode;
begin
   if not Assigned(ListView.Selected) then Exit;
   if ListView.Selected.ImageIndex=2 then Exit;
   s:=ListView.Selected.Caption;
   n:=TreeView.Selected;
   TreeView.Selected:=n[TreeIndexOf(s,n)];
end;

procedure TForm1.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
   FileListRefresh;
end;

procedure TForm1.ListViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if ListView.Enabled=False then Exit;
   ListViewClick(Sender);
   if Key=VK_RETURN then ListViewDblClick(Sender);
   if Key=VK_DELETE then Deleteselectedfile1Click(Self);
end;

procedure TForm1.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
   ListViewClick(Sender);
end;

procedure TForm1.New1Click(Sender: TObject);
begin
   if SaveDialog1.Execute then begin
      Archive.Clear;
      Archive.CreateArchive(SaveDialog1.FileName,True);
      Archive.LoadFromFile(SaveDialog1.FileName);
      TreeViewRefresh;
      ListView.Enabled:=True;
   end;
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
   OpenDialog1.DefaultExt:=SaveDialog1.DefaultExt;
   OpenDialog1.Filter:=SaveDialog1.Filter;
   OpenDialog1.Options:=OpenDialog1.Options-[ofAllowMultiSelect];
   if OpenDialog1.Execute then begin
      Archive.Clear;
      Archive.LoadFromFile(OpenDialog1.FileName);
      TreeViewRefresh;
      ListView.Enabled:=True;
   end;
end;

procedure TForm1.Createfolder1Click(Sender: TObject);
var
   F: TMemoryStream;
begin
   if not Assigned(Archive) then Exit;
   if FDialog.ShowModal=mrOk then begin
      F := TMemoryStream.Create;
      try
        Archive.AddFromStream('temp.tmp', CurPath+FDialog.Edit1.Text+'/', F);
       finally
        F.Free;
       end;
      TreeViewRefresh;
   end;
end;

procedure TForm1.Addfiles1Click(Sender: TObject);
var
   i: integer;
begin
   if not Assigned(Archive) then Exit;
   OpenDialog1.DefaultExt:='';
   OpenDialog1.Filter:='All Files|*.*';
   OpenDialog1.Options:=OpenDialog1.Options+[ofAllowMultiSelect];
   if OpenDialog1.Execute then begin
      for i:=0 to OpenDialog1.Files.Count-1 do
         if ExtractFileName(OpenDialog1.Files[i])<>'' then
            Archive.AddFromFile(OpenDialog1.Files[i],CurPath);
      TreeViewRefresh;
   end;
end;

procedure TForm1.Deleteselectedfile1Click(Sender: TObject);
var
   i: integer;
   S: TStrings;
begin
   if ListView.SelCount=0 then Exit;
   S:=TStringList.Create;
   for i:=0 to ListView.Items.Count-1 do
      if ListView.Items[i].Selected then S.Add(CurPath+ListView.Items[i].Caption);
   for i:=0 to S.Count-1 do
      Archive.RemoveContent(S[i]);

   S.Free;
   FileListRefresh;
   if ListView.Items.Count=0 then TreeViewRefresh;
end;

procedure TForm1.Deleteselectedfolder1Click(Sender: TObject);
var
   i, l: integer;
   S: TStrings;
begin                       
   s:=TStringList.Create;
   s.AddStrings(Archive.ContentList);
   l:=Length(CurPath);
   for i:=0 to S.Count-1 do
      if Copy(S[i],1,l)=CurPath then
         Archive.RemoveContent(S[i]);
   s.Free;
   TreeViewRefresh;
end;

procedure TForm1.TreeViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if ListView.Enabled=False then Exit;
   if Key=VK_DELETE then Deleteselectedfolder1Click(Self);
end;

procedure TForm1.Extractselectedfiles1Click(Sender: TObject);
var
   i: integer;
begin
   if ListView.SelCount=0 then Exit;
   if FolderSel.ShowModal=mrOK then begin
      for i:=0 to ListView.Items.Count-1 do
         if ListView.Items[i].Selected then
            Archive.Extract(CurPath+ListView.Items[i].Caption,
             FolderSel.ShellView.{$IFNDEF FPC}Path{$ELSE}GetBasePath{$ENDIF}+'\'+ListView.Items[i].Caption);
   end;
end;


procedure TForm1.Exit1Click(Sender: TObject);
begin
   Application.Terminate;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ArchiveManager.Free;
end;

procedure TForm1.None1Click(Sender: TObject);
begin
  vMenu.Checked := false;
  vMenu := (Sender As TMenuItem);
  case vMenu.Tag of
    0: Archive.CompressionLevel := clNone;
    1: Archive.CompressionLevel := clFastest;
    2: Archive.CompressionLevel := clDefault;
    3: Archive.CompressionLevel := clMax;
  end;
  vMenu.Checked := true;
end;

end.
