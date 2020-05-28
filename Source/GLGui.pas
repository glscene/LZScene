//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  In GL windows management classes and structures

  History :  
       15/04/11 - Yar - Added TGLGuiLayout.Assign
       16/03/11 - Yar - Fixes after emergence of GLMaterialEx
       23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
       11/06/10 - YP - Link GUI elements to their parent
       06/06/10 - Yar - Fixed warnings
       30/03/07 - DaStr - Added $I GLScene.inc, cosmetic changes
       17/02/07 - DaStr - TGLGuiElement.Create - vectors creation fixed
                          Changed some types from TGLCoordinates to TGLCoordinates2
                          Removed some empty lines
       16/12/05 - DK - Removed GuiSkinEditorFormUnit dependancy
       30/11/04 - DB - Fixed memory leaks (thanks dikoe Kenguru)
       16/07/03 - EG - TGLBaseGuiObject moved in along with RecursiveVisible mechanism
       25/11/02 - EG - TGLGuiLayout.Clear fix (Sternas Stefanos)
       06/09/02 - JAJ - Updated and added to CVS..
       01/06/02 - JAJ - Base Unit built..
  
}
unit GLGui;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,

  GLScene, GLBitmapFont, GLMaterial, GLCrossPlatform, OpenGLTokens, GLContext,
  GLPersistentClasses, GLVectorGeometry, GLCoordinates, GLBaseClasses;

type

  TGLBaseGuiObject = class(TGLBaseSceneObject)
  private
    FRecursiveVisible: Boolean;
    FWidth: Single;
    FHeight: Single;

  protected
    // self notification on hide. Also notifies children.
    procedure NotifyHide; dynamic;
    // child notification on show. Also notifies children.
    procedure NotifyShow; dynamic;

    procedure SetLeft(const Value: TGLFloat);
    function GetLeft: TGLFloat;
    procedure SetTop(const Value: TGLFloat);
    function GetTop: TGLFloat;
    procedure SetWidth(const val: Single);
    procedure SetHeight(const val: Single);
    procedure SetVisible(aValue: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;

    procedure AddChild(AChild: TGLBaseSceneObject); override;
    procedure Insert(aIndex: Integer; aChild: TGLBaseSceneObject); override;

    { GuiComponent Width in 3D world units. }
    property Width: Single read FWidth write SetWidth;
    { GuiComponent Height in 3D world units. }
    property Height: Single read FHeight write SetHeight;
    { GuiComponent Left in 3D world units. }
    property Left: TGLFloat read GetLeft write SetLeft;
    { GuiComponent Top in 3D world units. }
    property Top: TGLFloat read GetTop write SetTop;

    property RecursiveVisible: Boolean read FRecursiveVisible;
  end;

  TGUIAlignments = (GLAlTopLeft, GLAlTop, GLAlTopRight, GLAlLeft, GLAlCenter,
    GLAlRight, GLAlBottomLeft, GLAlBottom, GLAlBottomRight, GLAlBorder);
  TGUIRect = record
    X1: TGLFloat;
    Y1: TGLFloat;
    X2: TGLFloat;
    Y2: TGLFloat;
    XTiles: TGLFloat;
    YTiles: TGLFloat;
  end;
  TGUIDrawResult = array[TGUIAlignments] of TGUIRect;

  TGLGuiElementName = string;
  TGLGuiElement = class(TCollectionItem)
  private
    FTopLeft: TGLCoordinates2;
    FBottomRight: TGLCoordinates2;
    FScale: TGLCoordinates2;
    FAlign: TGUIAlignments;
    FName: TGLGuiElementName;
  protected
    function GetDisplayName: string; override;
    procedure SetName(const val: TGLGuiElementName);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property TopLeft: TGLCoordinates2 read FTopLeft write FTopLeft;
    property BottomRight: TGLCoordinates2 read FBottomRight write FBottomRight;
    property Scale: TGLCoordinates2 read FScale write FScale;
    property Align: TGUIAlignments read FAlign write FAlign;
    property Name: TGLGuiElementName read FName write SetName;
  end;

  TGLGuiComponent = class;

  TGLGuiElementList = class(TOwnedCollection)
  private
    FGuiComponent: TGLGuiComponent;
  protected
    procedure SetItems(index: Integer; const val: TGLGuiElement);
    function GetItems(index: Integer): TGLGuiElement;
  public
    constructor Create(AOwner: TGLGuiComponent);
    procedure AssignTo(Dest: TPersistent); override;

    function GetOwner: TPersistent; override;
    function IndexOf(const Item: TGLGuiElement): Integer;
    property Items[index: Integer]: TGLGuiElement read GetItems write SetItems;
      default;
  end;

  TGLGuiComponentName = string;

  TGLGuiComponentList = class;
  TGLGuiComponent = class(TCollectionItem)
  private
    FElements: TGLGuiElementList;
    FName: TGLGuiComponentName;
  protected
    function GetDisplayName: string; override;
    procedure SetName(const val: TGLGuiComponentName);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure RenderToArea(X1, Y1, X2, Y2: TGLFloat; var Res: TGUIDrawResult;
      Refresh: Boolean = True; Scale: TGLFloat = 1);
    function GetOwnerList: TGLGuiComponentList;
    property Owner: TGLGuiComponentList read GetOwnerList;
  published
    property Elements: TGLGuiElementList read FElements write FElements;
    property Name: TGLGuiComponentName read FName write SetName;
  end;

  TGLGuiLayout = class;
  TGLGuiComponentList = class(TOwnedCollection)
  private
    FLayout: TGLGuiLayout;
  protected
    procedure SetItems(index: Integer; const val: TGLGuiComponent);
    function GetItems(index: Integer): TGLGuiComponent;
  public
    constructor Create(AOwner: TGLGuiLayout);

    function GetOwner: TPersistent; override;
    function FindItem(name: TGLGuiComponentName): TGLGuiComponent;
    property Items[index: Integer]: TGLGuiComponent read GetItems write
      SetItems; default;
  end;

  TGLGuiLayout = class(TGLUpdateableComponent)
  private
    FBitmapFont: TGLCustomBitmapFont;
    FMaterial: TGLMaterial;
    FGuiComponents: TGLGuiComponentList;
    FFileName: string;
    FGuiComponentList: TList;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    procedure SetFileName(newName: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(FN: string);

    procedure Clear;

    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(FN: string);
    procedure AddGuiComponent(Component: TGLUpdateableComponent);
    procedure RemoveGuiComponent(Component: TGLUpdateableComponent);

    procedure NotifyChange(Sender: TObject); override;
  published
    property BitmapFont: TGLCustomBitmapFont read FBitmapFont write FBitmapFont;
    property Material: TGLMaterial read FMaterial write FMaterial;
    property GuiComponents: TGLGuiComponentList read FGuiComponents write
      FGuiComponents;
    property FileName: string read FFileName write SetFileName;
  end;

const
  GuiNullRect: TGUIRect = (X1: 0.0; Y1: 0.0; X2: 0.0; Y2: 0.0; XTiles: 0.0;
    YTiles: 0.0);

function IsInRect(const R: TGUIRect; X, Y: Single): Boolean;

implementation

function IsInRect(const R: TGUIRect; X, Y: Single): Boolean;

begin
  Result := (R.X1 <= X) and (R.X2 >= X) and (R.Y1 <= Y) and (R.Y2 >= Y);
end;

// ------------------
// ------------------ TGLBaseGuiObject ------------------
// ------------------

// Create
//

constructor TGLBaseGuiObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecursiveVisible := Visible;
end;

// SetLeft
//

procedure TGLBaseGuiObject.SetLeft(const Value: TGLFloat);
var
  NewPosX: TGLFloat;
  i: integer;
  Diff: TGLFloat;
begin
  if Assigned(Parent) and (Parent is TGLBaseGuiObject) then
    NewPosX := (Parent as TGLBaseGuiObject).Position.X + Value
  else
    NewPosX := Value;

  if Position.X <> NewPosX then
  begin
    Diff := NewPosX - Position.X;
    Position.X := NewPosX;

    for i := 0 to Count - 1 do
      if Children[i] is TGLBaseGuiObject then
      begin
        (Children[i] as TGLBaseGuiObject).Left := (Children[i] as
          TGLBaseGuiObject).Left + Diff;
      end;
  end;
end;

// GetLeft
//

function TGLBaseGuiObject.GetLeft: TGLFloat;
begin
  if Assigned(Parent) and (Parent is TGLBaseGuiObject) then
    Result := Position.X - (Parent as TGLBaseGuiObject).Position.X
  else
    Result := Position.X;
end;

// SetTop
//

procedure TGLBaseGuiObject.SetTop(const Value: TGLFloat);
var
  NewPosY: TGLFloat;
  i: integer;
  Diff: TGLFloat;
begin
  if Assigned(Parent) and (Parent is TGLBaseGuiObject) then
    NewPosY := (Parent as TGLBaseGuiObject).Position.Y + Value
  else
    NewPosY := Value;

  if Position.Y <> NewPosY then
  begin
    Diff := NewPosY - Position.Y;
    Position.Y := NewPosY;

    for i := 0 to Count - 1 do
      if Children[i] is TGLBaseGuiObject then
      begin
        (Children[i] as TGLBaseGuiObject).Top := (Children[i] as
          TGLBaseGuiObject).Top + Diff;
      end;
  end;
end;

// GetTop
//

function TGLBaseGuiObject.GetTop: TGLFloat;
begin
  if Assigned(Parent) and (Parent is TGLBaseGuiObject) then
    Result := Position.Y - (Parent as TGLBaseGuiObject).Position.Y
  else
    Result := Position.Y;
end;

// SetWidth
//

procedure TGLBaseGuiObject.SetWidth(const val: TGLFloat);
begin
  if FWidth <> val then
  begin
    FWidth := val;
    NotifyChange(Self);
  end;
end;

// SetHeight
//

procedure TGLBaseGuiObject.SetHeight(const val: TGLFloat);
begin
  if FHeight <> val then
  begin
    FHeight := val;
    NotifyChange(Self);
  end;
end;

// NotifyHide
//

procedure TGLBaseGuiObject.NotifyHide;
var
  child: TGLBaseSceneObject;
  xc: Integer;
begin
  if RecursiveVisible then
  begin
    FRecursiveVisible := False;
    for xc := 0 to Count - 1 do
    begin
      child := Children[xc];
      if child is TGLBaseGuiObject then
        TGLBaseGuiObject(child).NotifyHide;
    end;
  end;
end;

// NotifyShow
//

procedure TGLBaseGuiObject.NotifyShow;
var
  child: TGLBaseSceneObject;
  xc: Integer;
begin
  if not RecursiveVisible then
  begin
    FRecursiveVisible := True;
    for xc := 0 to Count - 1 do
    begin
      child := Children[xc];
      if child is TGLBaseGuiObject then
        TGLBaseGuiObject(child).NotifyShow;
    end;
  end;
end;

// AddChild
//

procedure TGLBaseGuiObject.AddChild(aChild: TGLBaseSceneObject);
begin
  inherited;
  if AChild is TGLBaseGuiObject then
  begin
    if RecursiveVisible then
      TGLBaseGuiObject(AChild).NotifyShow
    else
      TGLBaseGuiObject(AChild).NotifyHide;
  end;
end;

// Insert
//

procedure TGLBaseGuiObject.Insert(aIndex: Integer; aChild: TGLBaseSceneObject);
begin
  inherited;
  if AChild is TGLBaseGuiObject then
  begin
    if RecursiveVisible then
      TGLBaseGuiObject(AChild).NotifyShow
    else
      TGLBaseGuiObject(AChild).NotifyHide;
  end;
end;

// SetVisible
//

procedure TGLBaseGuiObject.SetVisible(aValue: Boolean);
begin
  if Visible <> aValue then
  begin
    inherited SetVisible(aValue);
    if aValue then
    begin
      if Parent <> nil then
        if Parent is TGLBaseGuiObject then
        begin
          if TGLBaseGuiObject(Parent).RecursiveVisible then
            NotifyShow;
        end
        else
        begin
          if Parent.Visible then
            NotifyShow;
        end;
    end
    else
    begin
      if RecursiveVisible then
        NotifyHide;
    end;
  end;
end;

constructor TGLGuiLayout.Create(AOwner: TComponent);
begin
  FGuiComponentList := TList.Create;
  inherited;
  FGuiComponents := TGLGuiComponentList.Create(Self);
  FMaterial := TGLMaterial.Create(Self);
end;

destructor TGLGuiLayout.Destroy;
begin
  Clear;
  FMaterial.Free;
  FGuiComponents.Free;
  inherited;
  FGuiComponentList.Free;
end;

procedure TGLGuiLayout.SetFileName(newName: string);
begin
  if newName <> FFileName then
  begin
    FFileName := newName;
    if FileExists(FFileName) then
    begin
      Clear;
      loadFromFile(FFileName);
    end;
  end;
end;

procedure TGLGuiLayout.LoadFromFile(FN: string);
var
  Stream: TMemoryStream;

begin
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(FN);
    LoadFromStream(stream);
    FFileName := FN;
  finally
    stream.Free;
  end;
end;

procedure TGLGuiLayout.SaveToFile(FN: string);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    SaveToStream(Stream);
    Stream.SaveToFile(FN);
    FFileName := FN;
  finally
    Stream.Free;
  end;
end;

procedure TGLGuiLayout.AddGuiComponent(Component: TGLUpdateableComponent);
begin
  if FGuiComponentList.IndexOf(Component) < 0 then
  begin
    FreeNotification(Component);
    FGuiComponentList.Add(Component);
  end;
end;

procedure TGLGuiLayout.RemoveGuiComponent(Component: TGLUpdateableComponent);
begin
  FGuiComponentList.Remove(Component);
  RemoveFreeNotification(Component);
end;

procedure TGLGuiLayout.Assign(Source: TPersistent);
var
  LLayout: TGLGuiLayout;
  LComponent: TGLGuiComponent;
  I: Integer;
begin
  if Source is TGLGuiLayout then
  begin
    LLayout := TGLGuiLayout(Source);
    FBitmapFont := LLayout.FBitmapFont;
    FMaterial.Assign(LLayout.Material);
    FFileName := LLayout.FFileName;
    Clear;
    for I := 0 to LLayout.FGuiComponents.Count - 1 do
    begin
      LComponent := TGLGuiComponent(FGuiComponents.Add);
      LLayout.FGuiComponents[I].AssignTo(LComponent);
      LComponent.Name := LLayout.FGuiComponents[I].Name;
    end;
    for I := 0 to FGuiComponentList.Count - 1 do
      TGLUpdateAbleComponent(FGuiComponentList[I]).RemoveFreeNotification(Self);
    FGuiComponentList.Assign(LLayout.FGuiComponentList);
    for I := 0 to FGuiComponentList.Count - 1 do
      TGLUpdateAbleComponent(FGuiComponentList[I]).FreeNotification(Self);
  end
  else
    inherited; // Assigned Error
end;

procedure TGLGuiLayout.Clear;
var
  XC: Integer;

begin
  for XC := FGuiComponents.Count - 1 downto 0 do
  begin
    FGuiComponents.Delete(XC);
  end;
  NotifyChange(Self);
end;

procedure TGLGuiLayout.NotifyChange(Sender: TObject);
var
  XC: Integer;
begin
  inherited;

  for XC := FGuiComponentList.Count - 1 downto 0 do
    TGLUpdateAbleComponent(FGuiComponentList[XC]).NotifyChange(Self);
end;

procedure TGLGuiLayout.LoadFromStream(Stream: TStream);

var
  TmpComponent: TGLGuiComponent;
  XC, YC, ZC: Integer;
  TmpElement: TGLGuiElement;
  TmpAlignment: TGUIAlignments;
  Version: Integer;
  Data: TBinaryReader;

begin
  Data := TBinaryReader.Create(Stream);
  try

    Version := Data.ReadInteger;
    if Version <> 1 then
      Exit;

    for XC := 0 to Data.ReadInteger - 1 do
    begin
      TmpComponent := FGuiComponents.Add as TGLGuiComponent;
      TmpComponent.FName := Data.ReadString;
      for YC := 0 to Data.ReadInteger - 1 do
      begin
        TmpElement := TmpComponent.FElements.add as TGLGuiElement;

        TmpElement.FName := Data.ReadString;

        TmpElement.FTopLeft.X := Data.ReadFloat;
        TmpElement.FTopLeft.Y := Data.ReadFloat;
        TmpElement.FTopLeft.Z := Data.ReadFloat;

        TmpElement.FBottomRight.X := Data.ReadFloat;
        TmpElement.FBottomRight.Y := Data.ReadFloat;
        TmpElement.FBottomRight.Z := Data.ReadFloat;

        TmpElement.FScale.X := Data.ReadFloat;
        TmpElement.FScale.Y := Data.ReadFloat;
        TmpElement.FScale.Z := Data.ReadFloat;

        for ZC := 0 to Data.ReadInteger - 1 do
        begin
          TmpAlignment := TGUIAlignments(Data.ReadInteger);
          TmpElement.FAlign := TmpAlignment;
        end;
      end;
    end;
  finally
    Data.Free;
  end;
  NotifyChange(Self);
end;

procedure TGLGuiLayout.SaveToStream(stream: TStream);
var
  TmpComponent: TGLGuiComponent;
  Alignments, XC, YC: Integer;
  TmpElement: TGLGuiElement;
  TmpAlignment: TGUIAlignments;
  Data: TBinaryWriter;

begin
  Data := TBinaryWriter.Create(Stream);
  try
    Data.WriteInteger(1);
    Data.WriteInteger(FGuiComponents.Count);
    for XC := 0 to FGuiComponents.Count - 1 do
    begin
      TmpComponent := FGuiComponents.Items[XC];
      Data.WriteString(TmpComponent.FName);

      Data.WriteInteger(TmpComponent.FElements.Count);

      for YC := 0 to TmpComponent.FElements.Count - 1 do
      begin
        TmpElement := TmpComponent.FElements.Items[YC];

        Data.WriteString(TmpElement.FName);

        Data.WriteFloat(TmpElement.FTopLeft.X);
        Data.WriteFloat(TmpElement.FTopLeft.Y);
        Data.WriteFloat(TmpElement.FTopLeft.Z);

        Data.WriteFloat(TmpElement.FBottomRight.X);
        Data.WriteFloat(TmpElement.FBottomRight.Y);
        Data.WriteFloat(TmpElement.FBottomRight.Z);

        Data.WriteFloat(TmpElement.FScale.X);
        Data.WriteFloat(TmpElement.FScale.Y);
        Data.WriteFloat(TmpElement.FScale.Z);

        Alignments := 0;
        for TmpAlignMent := GLAlTopLeft to GLAlBorder do
        begin
          if TmpAlignMent = TmpElement.FAlign then
            inc(Alignments);
        end;

        Data.WriteInteger(Alignments);

        for TmpAlignMent := GLAlTopLeft to GLAlBorder do
        begin
          if TmpAlignMent = TmpElement.FAlign then
            Data.WriteInteger(Integer(TmpAlignMent));
        end;
      end;
    end;
  finally
    Data.Free;
  end;
end;

constructor TGLGuiComponentList.Create(AOwner: TGLGuiLayout);
begin
  inherited Create(AOwner, TGLGuiComponent);
  FLayout := AOwner;
end;

function TGLGuiComponentList.GetOwner: TPersistent;
begin
  Result := FLayout;
end;

procedure TGLGuiComponentList.SetItems(index: Integer; const val:
  TGLGuiComponent);
begin
  inherited Items[index] := val;
end;

function TGLGuiComponentList.FindItem(name: TGLGuiComponentName):
  TGLGuiComponent;
var
  XC: Integer;
  gc: TGLGuiComponent;
begin
  Result := nil;
  if Name = '' then
    Exit;
  for XC := 0 to Count - 1 do
  begin
    gc := Items[xc];
    if gc.FName = Name then
    begin
      Result := gc;
      Break;
    end;
  end;
end;

function TGLGuiComponentList.GetItems(index: Integer): TGLGuiComponent;
begin
  Result := TGLGuiComponent(inherited Items[index]);
end;

procedure TGLGuiComponent.RenderToArea(X1, Y1, X2, Y2: TGLFloat; var Res:
  TGUIDrawResult; Refresh: Boolean = True; Scale: TGLFloat = 1);
var
  XC: Integer;
  ThisElement: TGLGuiElement;
  W, H: TGLFloat;
  Len1, Len2: TGLFloat;
  Layout: TGLGuiLayout;
  LibMaterial: TGLLibMaterial;
  Material: TGLMaterial;
  TexWidth,
    TexHeight: TGLFloat;
  AlignCount: TGUIAlignments;

  procedure Prepare;
  begin
    Len1 := (ThisElement.FTopLeft.x - ThisElement.FBottomRight.x) *
      ThisElement.Scale.X * Scale;
    Len2 := (ThisElement.FTopLeft.y - ThisElement.FBottomRight.y) *
      ThisElement.Scale.Y * Scale;
    if Len1 < 0 then
    begin
      if Len2 < 0 then
      begin
        W := -Len1;
        H := -Len2;
      end
      else
      begin
        W := -Len1;
        H := Len2;
      end;
    end
    else
    begin
      if Len2 < 0 then
      begin
        W := Len1;
        H := -Len2;
      end
      else
      begin
        W := Len1;
        H := Len2;
      end;
    end;
  end;

  procedure RenderIt(var ARect: TGuiRect; AElement: TGLGuiElement);
  var
    XC: TGLFloat;
    YC: TGLFloat;
    XPos, X2Pos: TGLFloat;
    YPos, y2Pos: TGLFloat;
    tx1, ty1, tx2, ty2: TGLFloat;
    XTileSize, YTileSize: TGLFloat;
    tx3, ty3: TGLFloat;
    tx, ty: TGLFloat;

  begin
    if (ARect.XTiles = 1) and (ARect.YTiles = 1) then
    begin
      GL.TexCoord2f(AElement.TopLeft.X / TexWidth, -AElement.TopLeft.Y /
        TexHeight);
      GL.Vertex2f(ARect.X1, -ARect.Y1);

      GL.TexCoord2f(AElement.TopLeft.X / TexWidth, -AElement.BottomRight.Y /
        TexHeight);
      GL.Vertex2f(ARect.X1, -ARect.Y2);

      GL.TexCoord2f(AElement.BottomRight.X / TexWidth, -AElement.BottomRight.Y /
        TexHeight);
      GL.Vertex2f(ARect.X2, -ARect.Y2);

      GL.TexCoord2f(AElement.BottomRight.X / TexWidth, -AElement.TopLeft.Y /
        TexHeight);
      GL.Vertex2f(ARect.X2, -ARect.Y1);
    end
    else
    begin
      XTileSize := (ARect.X2 - ARect.X1) / ARect.XTiles;
      YTileSize := (ARect.Y2 - ARect.Y1) / ARect.YTiles;
      tx1 := AElement.TopLeft.X / TexWidth;
      ty1 := -AElement.TopLeft.Y / TexHeight;
      tx2 := AElement.BottomRight.X / TexWidth;
      ty2 := -AElement.BottomRight.Y / TexHeight;
      tx3 := (AElement.TopLeft.X + (AElement.BottomRight.X - AElement.TopLeft.X)
        * Frac(ARect.XTiles)) / TexWidth;
      ty3 := -(AElement.TopLeft.y + (AElement.BottomRight.y - AElement.TopLeft.y)
        * Frac(ARect.yTiles)) / TexHeight;

      XC := ARect.XTiles;
      XPos := ARect.X1;
      tx := tx2;
      while XC > 0 do
      begin
        YC := ARect.YTiles;
        YPos := ARect.Y1;
        ty := ty2;

        if XC >= 1 then
          X2Pos := XPos + XTileSize
        else
        begin
          X2Pos := ARect.X2;
          tx := tx3;
        end;

        while YC > 0 do
        begin
          if YC >= 1 then
            Y2Pos := YPos + YTileSize
          else
          begin
            Y2Pos := ARect.Y2;
            ty := ty3;
          end;

          GL.TexCoord2f(tx1, ty1);
          GL.Vertex2f(XPos, -YPos);

          GL.TexCoord2f(tx1, ty);
          GL.Vertex2f(XPos, -Y2Pos);

          GL.TexCoord2f(tx, ty);
          GL.Vertex2f(X2Pos, -Y2Pos);

          GL.TexCoord2f(tx, ty1);
          GL.Vertex2f(X2Pos, -YPos);
          yc := yc - 1.0;
          ypos := Y2Pos;
        end;
        xc := xc - 1.0;
        xpos := X2Pos;
      end;
    end;
  end;

  procedure RenderBorder(AElement: TGLGuiElement);
  var
    TmpElement: TGLGuiElement;

  begin
    TmpElement := TGLGuiElement.Create(nil);
    TmpElement.FTopLeft.X := ThisElement.FTopLeft.X;
    TmpElement.FTopLeft.Y := ThisElement.FTopLeft.Y;
    TmpElement.FBottomRight.X := ThisElement.FTopLeft.X + ThisElement.Scale.X;
    TmpElement.FBottomRight.Y := ThisElement.FTopLeft.Y + ThisElement.Scale.Y;
    TmpElement.Scale.SetPoint2D(1, 1);
    RenderIt(Res[GLALTopLeft], TmpElement);

    TmpElement.FTopLeft.X := ThisElement.FTopLeft.X + ThisElement.Scale.X;
    TmpElement.FBottomRight.X := ThisElement.FBottomRight.X -
      ThisElement.Scale.X;
    RenderIt(Res[GLALTop], TmpElement);

    TmpElement.FTopLeft.X := ThisElement.FBottomRight.X - ThisElement.Scale.X;
    TmpElement.FBottomRight.X := ThisElement.FBottomRight.X;
    RenderIt(Res[GLALTopRight], TmpElement);

    TmpElement.FTopLeft.Y := ThisElement.FTopLeft.Y + ThisElement.Scale.Y;
    TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y -
      ThisElement.Scale.Y;
    RenderIt(Res[GLALRight], TmpElement);

    TmpElement.FTopLeft.X := ThisElement.FBottomRight.X - ThisElement.Scale.X;
    TmpElement.FTopLeft.Y := ThisElement.FBottomRight.Y - ThisElement.Scale.Y;
    TmpElement.FBottomRight.X := ThisElement.FBottomRight.X;
    TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y;
    RenderIt(Res[GLALBottomRight], TmpElement);

    TmpElement.FTopLeft.X := ThisElement.FTopLeft.X + ThisElement.Scale.X;
    TmpElement.FTopLeft.Y := ThisElement.FBottomRight.Y - ThisElement.Scale.Y;
    TmpElement.FBottomRight.X := ThisElement.FBottomRight.X -
      ThisElement.Scale.X;
    TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y;
    RenderIt(Res[GLALBottom], TmpElement);

    TmpElement.FTopLeft.X := ThisElement.FTopLeft.X;
    TmpElement.FTopLeft.Y := ThisElement.FBottomRight.Y - ThisElement.Scale.Y;
    TmpElement.FBottomRight.X := ThisElement.FTopLeft.X + ThisElement.Scale.X;
    TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y;
    RenderIt(Res[GLALBottomLeft], TmpElement);

    TmpElement.FTopLeft.X := ThisElement.FTopLeft.X;
    TmpElement.FTopLeft.Y := ThisElement.FTopLeft.Y + ThisElement.Scale.Y;
    TmpElement.FBottomRight.X := ThisElement.FTopLeft.X + ThisElement.Scale.X;
    TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y -
      ThisElement.Scale.Y;
    RenderIt(Res[GLALLeft], TmpElement);

    TmpElement.FTopLeft.X := ThisElement.FTopLeft.X + ThisElement.Scale.X;
    TmpElement.FTopLeft.Y := ThisElement.FTopLeft.Y + ThisElement.Scale.Y;
    TmpElement.FBottomRight.X := ThisElement.FBottomRight.X -
      ThisElement.Scale.X;
    TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y -
      ThisElement.Scale.Y;
    RenderIt(Res[GLALCenter], TmpElement);
  end;

begin
  Layout := ((GetOwner as TGLGuiComponentList).GetOwner as TGLGuiLayout);
  Material := nil;
  if Assigned(Layout.Material.MaterialLibrary)
    and (Layout.Material.MaterialLibrary is TGLMaterialLibrary)
    and (Layout.Material.LibMaterialName <> '') then
  begin
    LibMaterial :=
      TGLMaterialLibrary(Layout.Material.MaterialLibrary).Materials.GetLibMaterialByName(Layout.Material.LibMaterialName);
    if Assigned(LibMaterial) then
      Material := LibMaterial.Material;
  end;
  if not Assigned(Material) then
  begin
    Material := Layout.Material;
  end;

  if Refresh then
  begin
    Res[GLALtopLeft].X1 := X1;
    Res[GLALtopLeft].Y1 := Y1;
    Res[GLALtopLeft].X2 := X1;
    Res[GLALtopLeft].Y2 := Y1;

    Res[GLALtopRight].X1 := X2;
    Res[GLALtopRight].Y1 := Y1;
    Res[GLALtopRight].X2 := X2;
    Res[GLALtopRight].Y2 := Y1;

    Res[GLALBottomLeft].X1 := X1;
    Res[GLALBottomLeft].Y1 := Y2;
    Res[GLALBottomLeft].X2 := X1;
    Res[GLALBottomLeft].Y2 := Y2;

    Res[GLALBottomRight].X1 := X2;
    Res[GLALBottomRight].Y1 := Y2;
    Res[GLALBottomRight].X2 := X2;
    Res[GLALBottomRight].Y2 := Y2;

    for XC := 0 to FElements.Count - 1 do
    begin
      ThisElement := FElements[XC];
      if GLAlBorder = ThisElement.Align then
      begin
        Res[GLALtopLeft].X1 := X1;
        Res[GLALtopLeft].Y1 := Y1;
        Res[GLALtopLeft].X2 := X1 + ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALtopLeft].Y2 := Y1 + ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;

        Res[GLALtop].X1 := X1 + ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALtop].Y1 := Y1;
        Res[GLALtop].X2 := X2 - ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALtop].Y2 := Y1 + ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;

        Res[GLALtopRight].X1 := X2 - ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALtopRight].Y1 := Y1;
        Res[GLALtopRight].X2 := X2;
        Res[GLALtopRight].Y2 := Y1 + ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;

        Res[GLALRight].X1 := X2 - ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALRight].Y1 := Y1 + ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;
        Res[GLALRight].X2 := X2;
        Res[GLALRight].Y2 := Y2 - ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;

        Res[GLALBottomRight].X1 := X2 - ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALBottomRight].Y1 := Y2 - ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;
        Res[GLALBottomRight].X2 := X2;
        Res[GLALBottomRight].Y2 := Y2;

        Res[GLALBottom].X1 := X1 + ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALBottom].Y1 := Y2 - ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;
        Res[GLALBottom].X2 := X2 - ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALBottom].Y2 := Y2;

        Res[GLALBottomLeft].X1 := X1;
        Res[GLALBottomLeft].Y1 := Y2 - ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;
        Res[GLALBottomLeft].X2 := X1 + ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALBottomLeft].Y2 := Y2;

        Res[GLALLeft].X1 := X1;
        Res[GLALLeft].Y1 := Y1 + ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;
        Res[GLALLeft].X2 := X1 + ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALLeft].Y2 := Y2 - ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;

        Res[GLALCenter].X1 := X1 + ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALCenter].Y1 := Y1 + ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;
        Res[GLALCenter].X2 := X2 - ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALCenter].Y2 := Y2 - ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;
      end;

      if GLALtopLeft = ThisElement.Align then
      begin
        Prepare;
        Res[GLALtopLeft].X1 := X1;
        Res[GLALtopLeft].Y1 := Y1;
        Res[GLALtopLeft].X2 := X1 + W;
        Res[GLALtopLeft].Y2 := Y1 + H;
      end;
      if GLALtopRight = ThisElement.Align then
      begin
        Prepare;
        Res[GLALtopRight].X1 := X2 - W;
        Res[GLALtopRight].Y1 := Y1;
        Res[GLALtopRight].X2 := X2;
        Res[GLALtopRight].Y2 := Y1 + H;
      end;
      if GLALBottomLeft = ThisElement.Align then
      begin
        Prepare;
        Res[GLALBottomLeft].X1 := X1;
        Res[GLALBottomLeft].Y1 := Y2 - H;
        Res[GLALBottomLeft].X2 := X1 + W;
        Res[GLALBottomLeft].Y2 := Y2;
      end;
      if GLALBottomRight = ThisElement.Align then
      begin
        Prepare;
        Res[GLALBottomRight].X1 := X2 - W;
        Res[GLALBottomRight].Y1 := Y2 - H;
        Res[GLALBottomRight].X2 := X2;
        Res[GLALBottomRight].Y2 := Y2;
      end;
    end;

    Res[GLALtop].X1 := Res[GLALtopLeft].X2;
    Res[GLALtop].Y1 := Res[GLALtopRight].Y1;
    Res[GLALtop].X2 := Res[GLALtopRight].X1;
    Res[GLALtop].Y2 := Res[GLALtopLeft].Y2;

    Res[GLALBottom].X1 := Res[GLALBottomLeft].X2;
    Res[GLALBottom].Y1 := Res[GLALBottomLeft].Y1;
    Res[GLALBottom].X2 := Res[GLALBottomRight].X1;
    Res[GLALBottom].Y2 := Res[GLALBottomRight].Y2;

    Res[GLALLeft].X1 := Res[GLALtopLeft].X1;
    Res[GLALLeft].Y1 := Res[GLALtopLeft].Y2;
    Res[GLALLeft].X2 := Res[GLALBottomLeft].X2;
    Res[GLALLeft].Y2 := Res[GLALBottomLeft].Y1;

    Res[GLALRight].X1 := Res[GLALtopRight].X1;
    Res[GLALRight].Y1 := Res[GLALtopRight].Y2;
    Res[GLALRight].X2 := Res[GLALBottomRight].X2;
    Res[GLALRight].Y2 := Res[GLALBottomRight].Y1;

    for XC := 0 to FElements.Count - 1 do
    begin
      ThisElement := FElements[XC];
      if GLALtop = ThisElement.Align then
      begin
        Prepare;
        Res[GLALtop].Y1 := Y1;
        Res[GLALtop].Y2 := Y1 + H;
      end;
      if GLALBottom = ThisElement.Align then
      begin
        Prepare;
        Res[GLALBottom].Y1 := Y2 - H;
        Res[GLALBottom].Y2 := Y2;
      end;
      if GLALLeft = ThisElement.Align then
      begin
        Prepare;
        Res[GLALLeft].X1 := X1;
        Res[GLALLeft].X2 := X1 + W;
      end;
      if GLALRight = ThisElement.Align then
      begin
        Prepare;
        Res[GLALRight].X1 := X2 - W;
        Res[GLALRight].X2 := X2;
      end;
    end;

    Res[GLALCenter].X1 := Res[GLALLeft].X2;
    Res[GLALCenter].Y1 := Res[GLALtop].Y2;
    Res[GLALCenter].X2 := Res[GLALRight].X1;
    Res[GLALCenter].Y2 := Res[GLALBottom].Y1;
  end;

  TexWidth := Material.Texture.TexWidth;
  if TexWidth = 0 then
    TexWidth := Material.Texture.Image.Width;

  TexHeight := Material.Texture.TexHeight;
  if TexHeight = 0 then
    TexHeight := Material.Texture.Image.Height;

  GL.Begin_(GL_QUADS);

  for XC := 0 to FElements.Count - 1 do
  begin
    ThisElement := FElements[XC];
    for AlignCount := GLAlTopLeft to GLAlBottomRight do
      if (AlignCount = ThisElement.Align) then
      begin
        if Refresh then
        begin
          Res[AlignCount].XTiles := ((Res[AlignCount].X2 - Res[AlignCount].X1) /
            (ThisElement.FBottomRight.X - ThisElement.FTopLeft.X)) /
            ThisElement.Scale.X;
          Res[AlignCount].YTiles := ((Res[AlignCount].Y2 - Res[AlignCount].Y1) /
            (ThisElement.FBottomRight.Y - ThisElement.FTopLeft.Y)) /
            ThisElement.Scale.Y;
        end;
        RenderIt(Res[AlignCount], ThisElement);
      end;
    if (GLALBorder = ThisElement.Align) then
    begin
      RenderBorder(ThisElement);
    end;

  end;
  GL.End_;
end;

function TGLGuiComponent.GetOwnerList: TGLGuiComponentList;
begin
  Result := GetOwner as TGLGuiComponentList;
end;

function TGLGuiComponent.GetDisplayName: string;
begin
  Result := FName;
end;

procedure TGLGuiComponent.SetName(const val: TGLGuiComponentName);
begin
  FName := Val;
end;

constructor TGLGuiComponent.Create(Collection: TCollection);
begin
  inherited;
  FElements := TGLGuiElementList.Create(Self);
end;

destructor TGLGuiComponent.Destroy;
begin
  FElements.Free;
  inherited;
end;

constructor TGLGuiElementList.Create(AOwner: TGLGuiComponent);
begin
  inherited Create(AOwner, TGLGuiElement);
  FGuiComponent := AOwner;
end;

function TGLGuiElementList.GetOwner: TPersistent;
begin
  Result := FGuiComponent;
end;

procedure TGLGuiElementList.SetItems(index: Integer; const val: TGLGuiElement);
begin
  inherited Items[index] := val;
end;

function TGLGuiElementList.IndexOf(const Item: TGLGuiElement): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if GetItems(I) = Item then
      begin
        Result := I;
        Exit;
      end;
end;

function TGLGuiElementList.GetItems(index: Integer): TGLGuiElement;
begin
  Result := TGLGuiElement(inherited Items[index]);
end;

function TGLGuiElement.GetDisplayName: string;
begin
  Result := FName;
end;

procedure TGLGuiElement.SetName(const val: TGLGuiElementName);
begin
  FName := Val;
end;

constructor TGLGuiElement.Create(Collection: TCollection);
begin
  inherited;
  FTopLeft := TGLCoordinates2.CreateInitialized(Self, NullHmgVector, csPoint2D);
  FBottomRight := TGLCoordinates2.CreateInitialized(Self, NullHmgVector,
    csPoint2D);
  FScale := TGLCoordinates2.CreateInitialized(Self, XYHmgVector, csPoint2D);
end;

destructor TGLGuiElement.Destroy;
begin
  FTopLeft.Free;
  FBottomRight.Free;
  FScale.Free;
  inherited;
end;

procedure TGLGuiLayout.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FBitmapFont then
      BitmapFont := nil
    else
      FGuiComponentList.Remove(AComponent);
  end;
  NotifyChange(Self); // EG : looks suspicious...
  inherited;
end;

procedure TGLGuiComponent.AssignTo(Dest: TPersistent);
begin
  if Dest is TGLGuiComponent then
  begin
    TGLGuiComponent(Dest).Elements.Assign(Elements);
  end
  else
    inherited;
end;

procedure TGLGuiElementList.AssignTo(Dest: TPersistent);
var
  i: Integer;
begin
  if Dest is TGLGuiElementList then
  begin
    for i := 0 to Count - 1 do
    begin
      TGLGuiElementList(Dest).Add.Assign(Items[i]);
    end;
  end
  else
    inherited;
end;

procedure TGLGuiElement.AssignTo(Dest: TPersistent);
var
  element: TGLGuiElement;
begin
  if Dest is TGLGuiElement then
  begin
    element := TGLGuiElement(Dest);

    element.TopLeft.Assign(TopLeft);
    element.BottomRight.Assign(BottomRight);
    element.Scale.Assign(Scale);
    element.Align := Align;
    element.Name := Name;
  end
  else
    inherited;
end;

end.

