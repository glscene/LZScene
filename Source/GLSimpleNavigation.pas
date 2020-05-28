//
// This unit is part of the GLScene Project, http://glscene.org
//
{
    A simple component written by request from someone at the www.glscene.ru forums.
    Allows to view the FPS and do the usual Zoom and MoveAroundTarget stuff 
    that all demos usually have in themselves. All that is just by dropping 
    this component on the form.

    History :  
       14/12/10 - DaStr - Fixed compiler hint
       12/12/10 - Yar   - Adapted to using with TGLSceneForm
       01/07/10 - Yar   - Fixed zooming for FPC (by Rustam Asmandiarov aka Predator)
       17/06/10 - YP    - Fixed Zoom in/out inconsistence (mousewheel up/down inverted)
       11/06/10 - YP    - Fixed wheeldata can be equal to 0 in FormMouseWheel (div by 0 exception)
       21/01/10 - Yar   - Bugfixed zooming in design time (BugtrackerID = 2936266)
       25/12/09 - DaStr - Added OnMouseMove event (thanks YarUnderoaker)
       18/10/09 - DaStr - Added snoShowFPS option (thanks YarUnderoaker)
                             Fixed a small bug with FPS string
       29/09/07 - DaStr - Component now automaticly detects Form Caption
       24/03/07 - DaStr - Replaced GLWin32Viewer with GLViewer
                             (thanks Burkhard Carstens) (Bugtracker ID = 1684432)
                             Got rid of Types dependancy
       20/03/07 - DaStr - Improved SceneViewer detection
       02/03/07 - DaStr - Added default values to all properties
                             Added TGLSimpleNavigationOptions
                             Added TGLSimpleNavigationKeyCombination
                             Some renamings
                             Added TGLSimpleNavigation.Assign
                             MouseWheel is now handled by default
       06/02/07 - DaStr - Creation (donated to GLScene)
  

   Previous version history:
           v1.0   08 May        '2006  Creation
           v1.1   04 September  '2006  FreeNotification fix
                                       Automatic Form detection fix
           v1.2   11 September  '2006  Automatic SceneViewer detection
                                       FormCaption added
           v1.3   06 February   '2007  FPS is only updated in Run-Time now
                                       Donated to GLScene
}

unit GLSimpleNavigation;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils, TypInfo, Forms, Controls, ExtCtrls,
   
  GLSceneForm, GLVectorGeometry, GLScene,
  GLViewer, GLStrings, GLCrossPlatform;

type

  TPoint = GLCrossPlatform.TGLPoint; // for Mouse Wheel

  TGLSimpleNavigationOption = (
    snoInvertMoveAroundX, snoInvertMoveAroundY, // MoveAroundTarget.
    snoInvertZoom, snoInvertMouseWheel, // Zoom.
    snoInvertRotateX, snoInvertRotateY, // RotateTarget.
    snoMouseWheelHandled, // MouseWheel.
    snoShowFPS // Show FPS
    );

  TGLSimpleNavigationOptions = set of TGLSimpleNavigationOption;

  TGLSimpleNavigationAction = (snaNone, snaMoveAroundTarget, snaZoom, snaRotateTarget, snaCustom);

  TGLSimpleNavigationKeyCombination = class;
  TSimpleNavigationCustomActionEvent =
    procedure(Sender: TGLSimpleNavigationKeyCombination; Shift: TShiftState; X, Y: Integer) of object;

  TGLSimpleNavigationKeyCombination = class(TCollectionItem)
  private
    FExitOnMatch: Boolean;
    FAction: TGLSimpleNavigationAction;
    FOnCustomAction: TSimpleNavigationCustomActionEvent;
    FShiftState: TShiftState;
  protected
    function GetDisplayName: string; override;
    procedure DoOnCustomAction(Shift: TShiftState; X, Y: Integer); virtual;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ShiftState: TShiftState read FShiftState write FShiftState default [];
    property ExitOnMatch: Boolean read FExitOnMatch write FExitOnMatch default True;
    property Action: TGLSimpleNavigationAction read FAction write FAction default snaNone;
    property OnCustomAction: TSimpleNavigationCustomActionEvent read FOnCustomAction write FOnCustomAction;
  end;

  TGLSimpleNavigationKeyCombinations = class(TOwnedCollection)
  private
    function GetItems(Index: Integer): TGLSimpleNavigationKeyCombination;
    procedure SetItems(Index: Integer; const Value: TGLSimpleNavigationKeyCombination);
  public
    function Add: TGLSimpleNavigationKeyCombination; overload;
    function Add(const AShiftState: TShiftState; const AAction: TGLSimpleNavigationAction; const AExitOnMatch: Boolean = True): TGLSimpleNavigationKeyCombination; overload;
    property Items[Index: Integer]: TGLSimpleNavigationKeyCombination read GetItems write SetItems; default;
  end;

  TGLSimpleNavigation = class(TComponent)
  private
    FTimer: TTimer;
    FForm: TCustomForm;
    FGLSceneViewer: TGLSceneViewer;

    FOldX, FOldY: Integer;
    FFormCaption: string;
    FMoveAroundTargetSpeed: Single;
    FZoomSpeed: Single;
    FOptions: TGLSimpleNavigationOptions;
    FKeyCombinations: TGLSimpleNavigationKeyCombinations;
    FRotateTargetSpeed: Single;
    FOnMouseMove: TMouseMoveEvent;
    FSceneForm: Boolean;
    procedure ShowFPS(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TGLPoint; var Handled: Boolean);

    procedure SetGLSceneViewer(const Value: TGLSceneViewer);
    procedure SetForm(const Value: TCustomForm);
    function StoreFormCaption: Boolean;
    function StoreMoveAroundTargetSpeed: Boolean;
    function StoreZoomSpeed: Boolean;
    procedure SetKeyCombinations(const Value: TGLSimpleNavigationKeyCombinations);
    function StoreRotateTargetSpeed: Boolean;
    procedure SetOptions(const Value: TGLSimpleNavigationOptions);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Form: TCustomForm read FForm write SetForm;
    property GLSceneViewer: TGLSceneViewer read FGLSceneViewer write SetGLSceneViewer;

    property ZoomSpeed: Single read FZoomSpeed write FZoomSpeed stored StoreZoomSpeed;
    property MoveAroundTargetSpeed: Single read FMoveAroundTargetSpeed write FMoveAroundTargetSpeed stored StoreMoveAroundTargetSpeed;
    property RotateTargetSpeed: Single read FRotateTargetSpeed write FRotateTargetSpeed stored StoreRotateTargetSpeed;

    property FormCaption: string read FFormCaption write FFormCaption stored StoreFormCaption;
    property Options: TGLSimpleNavigationOptions read FOptions write SetOptions default [snoMouseWheelHandled, snoShowFPS];
    property KeyCombinations: TGLSimpleNavigationKeyCombinations read FKeyCombinations write SetKeyCombinations;

    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
  end;

implementation

const
  vFPSString = '%FPS';
  EPS = 0.001;

  { TGLSimpleNavigation }

procedure TGLSimpleNavigation.Assign(Source: TPersistent);
begin
  if Source is TGLSimpleNavigation then
  begin
    { Don't do that, because that might overide the original component's event handlers
    SetForm(TGLSimpleNavigation(Source).FForm);
    SetGLSceneViewer(TGLSimpleNavigation(Source).FGLSceneViewer);
    }
    FZoomSpeed := TGLSimpleNavigation(Source).FZoomSpeed;
    FMoveAroundTargetSpeed := TGLSimpleNavigation(Source).FMoveAroundTargetSpeed;
    FRotateTargetSpeed := TGLSimpleNavigation(Source).FRotateTargetSpeed;

    FFormCaption := TGLSimpleNavigation(Source).FFormCaption;
    FOptions := TGLSimpleNavigation(Source).FOptions;
    FKeyCombinations.Assign(TGLSimpleNavigation(Source).FKeyCombinations);
  end
  else
    inherited; // Die!
end;

constructor TGLSimpleNavigation.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  FKeyCombinations := TGLSimpleNavigationKeyCombinations.Create(Self, TGLSimpleNavigationKeyCombination);
  FKeyCombinations.Add([ssLeft, ssRight], snaZoom, True);
  FKeyCombinations.Add([ssLeft], snaMoveAroundTarget, True);
  FKeyCombinations.Add([ssRight], snaMoveAroundTarget, True);

  FMoveAroundTargetSpeed := 1;
  FRotateTargetSpeed := 1;
  FZoomSpeed := 1.5;
  FOptions := [snoMouseWheelHandled, snoShowFPS];
  FFormCaption := vFPSString;

  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := ShowFPS;

  FOnMouseMove := nil;
  //Detect form
  if AOwner is TCustomForm then
    SetForm(TCustomForm(AOwner));

  //Detect SceneViewer
  if FForm <> nil then
  begin
    if FForm.ComponentCount <> 0 then
      for I := 0 to FForm.ComponentCount - 1 do
        if FForm.Components[I] is TGLSceneViewer then
        begin
          SetGLSceneViewer(TGLSceneViewer(FForm.Components[I]));
          Exit;
        end;
  end;
end;

destructor TGLSimpleNavigation.Destroy;
begin
  FTimer.Free;
  FKeyCombinations.Free;

  if FForm <> nil then
    TForm(FForm).OnMouseWheel := nil;

  if FGLSceneViewer <> nil then
    FGLSceneViewer.OnMouseMove := nil;

  inherited;
end;

procedure TGLSimpleNavigation.ViewerMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TGLPoint;
  var Handled: Boolean);
var
  Sign: SmallInt;
  lCamera: TGLCamera;
begin
  if (csDesigning in ComponentState) or (WheelDelta = 0) then
    Exit;

  if snoInvertMouseWheel in FOptions then
    Sign := 1
  else
    Sign := -1;

  if FGLSceneViewer <> nil then
    lCamera := FGLSceneViewer.Camera
  else if FSceneForm then
    lCamera := TGLSceneForm(FForm).Camera
  else
    lCamera := nil;

  if Assigned(lCamera) then
  begin
    if lCamera.CameraStyle = csOrthogonal then
      lCamera.FocalLength := FGLSceneViewer.Camera.FocalLength
        / Power(FZoomSpeed, Sign * WheelDelta div Abs(WheelDelta))
    else
      lCamera.AdjustDistanceToTarget(
        Power(FZoomSpeed, Sign * WheelDelta div Abs(WheelDelta)));
  end;

  Handled := snoMouseWheelHandled in FOptions;
end;

procedure TGLSimpleNavigation.ViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);

var
  lCamera: TGLCamera;

  procedure DoZoom;
  var
    Sign: SmallInt;
  begin
    if snoInvertZoom in FOptions then
      Sign := -1
    else
      Sign := 1;
    lCamera.AdjustDistanceToTarget(
      Power(FZoomSpeed, Sign * (Y - FOldY) / 20));
  end;

  procedure DoMoveAroundTarget;
  var
    SignX: SmallInt;
    SignY: SmallInt;
  begin
    if snoInvertMoveAroundX in FOptions then
      SignX := -1
    else
      SignX := 1;

    if snoInvertMoveAroundY in FOptions then
      SignY := -1
    else
      SignY := 1;

    lCamera.MoveAroundTarget(SignX * FMoveAroundTargetSpeed * (FOldY - Y),
      SignY * FMoveAroundTargetSpeed * (FOldX - X));
  end;

  procedure DoRotateTarget;
  var
    SignX: SmallInt;
    SignY: SmallInt;
  begin
    if snoInvertRotateX in FOptions then
      SignX := -1
    else
      SignX := 1;

    if snoInvertRotateY in FOptions then
      SignY := -1
    else
      SignY := 1;

    lCamera.RotateTarget(SignY * FRotateTargetSpeed * (FOldY - Y),
      SignX * FRotateTargetSpeed * (FOldX - X));
  end;

var
  I: Integer;

begin
  if csDesigning in ComponentState then
    exit;

  if FGLSceneViewer <> nil then
    lCamera := FGLSceneViewer.Camera
  else if FSceneForm then
    lCamera := TGLSceneForm(FForm).Camera;

  if Assigned(lCamera) then
  begin
    if FKeyCombinations.Count <> 0 then
      for I := 0 to FKeyCombinations.Count - 1 do
        if FKeyCombinations[I].FShiftState <= Shift then
        begin
          case FKeyCombinations[I].FAction of
            snaNone: ; //Ignore.
            snaMoveAroundTarget: DoMoveAroundTarget;
            snaZoom: DoZoom;
            snaRotateTarget: DoRotateTarget;
            snaCustom: FKeyCombinations[I].DoOnCustomAction(Shift, X, Y);
          else
            Assert(False, glsErrorEx + glsUnknownType);
          end;

          if FKeyCombinations[I].FExitOnMatch then
            Break;
        end;
  end;

  FOldX := X;
  FOldY := Y;

  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TGLSimpleNavigation.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FGLSceneViewer) and (Operation = opRemove) then
    FGLSceneViewer := nil;
  if (AComponent = FForm) and (Operation = opRemove) then
    FForm := nil;
end;

procedure TGLSimpleNavigation.SetKeyCombinations(
  const Value: TGLSimpleNavigationKeyCombinations);
begin
  FKeyCombinations.Assign(Value);
end;

procedure TGLSimpleNavigation.SetForm(const Value: TCustomForm);
begin
  if FForm <> nil then
  begin
    FForm.RemoveFreeNotification(Self);
    TForm(FForm).OnMouseWheel := nil;

    if FSceneForm then TForm(FForm).OnMouseMove := nil;
    FSceneForm := False;
  end;

  FForm := Value;

  if FForm <> nil then
  begin
    if FFormCaption = vFPSString then
      FFormCaption := FForm.Caption + ' - ' + vFPSString;
    TForm(FForm).OnMouseWheel := ViewerMouseWheel;
    FForm.FreeNotification(Self);
{$IFDEF GLS_MULTITHREAD}
    if FForm is TGLSceneForm then
    begin
      FSceneForm := True;
      TForm(FForm).OnMouseMove := ViewerMouseMove;
    end;
{$ENDIF}
  end;
end;

procedure TGLSimpleNavigation.SetGLSceneViewer(
  const Value: TGLSceneViewer);
begin
  if FGLSceneViewer <> nil then
  begin
    FGLSceneViewer.RemoveFreeNotification(Self);
    FGLSceneViewer.OnMouseMove := nil;

    FGLSceneViewer.OnMouseWheel := nil;

  end;

  FGLSceneViewer := Value;

  if FGLSceneViewer <> nil then
  begin
    FGLSceneViewer.OnMouseWheel := ViewerMouseWheel;
    FGLSceneViewer.OnMouseMove := ViewerMouseMove;
    FGLSceneViewer.FreeNotification(Self);
  end;
end;

procedure TGLSimpleNavigation.ShowFPS(Sender: TObject);
var
  Index: Integer;
  Temp: string;
begin
  if (FForm <> nil) and
    not (csDesigning in ComponentState) and
    (snoShowFPS in FOptions) then
  begin
    Temp := FFormCaption;
    Index := Pos(vFPSString, Temp);
    if FForm is TGLSceneForm then
    begin
      if Index <> 0 then
      begin
        Delete(Temp, Index, Length(vFPSString));
        Insert(Format('%.*f FPS', [1, TGLSceneForm(FForm).Buffer.FramesPerSecond]), Temp, Index);
      end;
      TGLSceneForm(FForm).Buffer.ResetPerformanceMonitor;
    end
    else if Assigned(FGLSceneViewer) then
    begin
      if Index <> 0 then
      begin
        Delete(Temp, Index, Length(vFPSString));
        Insert(Format('%.*f FPS', [1, FGLSceneViewer.Buffer.FramesPerSecond]), Temp, Index);
      end;
      FGLSceneViewer.ResetPerformanceMonitor;
    end;
    FForm.Caption := Temp;
  end;
end;

function TGLSimpleNavigation.StoreFormCaption: Boolean;
begin
  Result := (FFormCaption <> vFPSString);
end;

function TGLSimpleNavigation.StoreMoveAroundTargetSpeed: Boolean;
begin
  Result := Abs(FMoveAroundTargetSpeed - 1) > EPS;
end;

function TGLSimpleNavigation.StoreZoomSpeed: Boolean;
begin
  Result := Abs(FZoomSpeed - 1.5) > EPS;
end;

function TGLSimpleNavigation.StoreRotateTargetSpeed: Boolean;
begin
  Result := Abs(FRotateTargetSpeed - 1) > EPS;
end;

procedure TGLSimpleNavigation.SetOptions(
  const Value: TGLSimpleNavigationOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;

  end;
end;

{ TGLSimpleNavigationKeyCombination }

procedure TGLSimpleNavigationKeyCombination.Assign(Source: TPersistent);
begin
  if Source is TGLSimpleNavigationKeyCombination then
  begin
    FExitOnMatch := TGLSimpleNavigationKeyCombination(Source).FExitOnMatch;
    FAction := TGLSimpleNavigationKeyCombination(Source).FAction;
    FOnCustomAction := TGLSimpleNavigationKeyCombination(Source).FOnCustomAction;
    FShiftState := TGLSimpleNavigationKeyCombination(Source).FShiftState;
  end
  else
    inherited; // Die!
end;

constructor TGLSimpleNavigationKeyCombination.Create(Collection: TCollection);
begin
  inherited;
  FAction := snaNone;
  FExitOnMatch := True;
end;

procedure TGLSimpleNavigationKeyCombination.DoOnCustomAction(
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnCustomAction) then
    FOnCustomAction(Self, Shift, X, Y);
end;

function TGLSimpleNavigationKeyCombination.GetDisplayName: string;
begin
  Result := GetSetProp(Self, 'ShiftState', True) + '  -  ' +
    GetEnumName(TypeInfo(TGLSimpleNavigationAction), Integer(FAction));
end;

{ TGLSimpleNavigationKeyCombinations }

function TGLSimpleNavigationKeyCombinations.Add: TGLSimpleNavigationKeyCombination;
begin
  Result := TGLSimpleNavigationKeyCombination(inherited Add);
end;

function TGLSimpleNavigationKeyCombinations.Add(
  const AShiftState: TShiftState; const AAction: TGLSimpleNavigationAction;
  const AExitOnMatch: Boolean): TGLSimpleNavigationKeyCombination;
begin
  Result := Add;
  with Result do
  begin
    FShiftState := AShiftState;
    FAction := AAction;
    FExitOnMatch := AExitOnMatch;
  end;
end;

function TGLSimpleNavigationKeyCombinations.GetItems(
  Index: Integer): TGLSimpleNavigationKeyCombination;
begin
  Result := TGLSimpleNavigationKeyCombination(inherited GetItem(Index));
end;

procedure TGLSimpleNavigationKeyCombinations.SetItems(Index: Integer;
  const Value: TGLSimpleNavigationKeyCombination);
begin
  inherited SetItem(Index, Value);
end;

end.

