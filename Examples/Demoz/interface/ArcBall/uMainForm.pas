
{
    Quaternion Rotation Demo by Jerome.D (BeanzMaster)

    Include : A Simple Quaternion ArcBall Gizmo Component

    History :
       24/02/18 - JD - Creation

}
unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, OpenGL1x, GLState, GLRenderContextInfo, GLVectorTypes,
  GLVectorGeometry, GLCoordinates, GLScene, GLLCLViewer, GLCadencer, GLObjects,
  GLGeomObjects;

Type

  { TGLGizmoArcBall }

  TGLGizmoArcBall = class(TGLCoordinatesUpdateAbleComponent)
  private
    FUIBaseGizmo : TGLBaseSceneObject;
    FSelectedObj: TGLBaseSceneObject;
    FViewer : TGLSceneViewer;
    FDrawArcRender: TGLDirectOpenGL;

    FRadius:Single;
    FqNow:TQuaternion;
    FqDown:TQuaternion;
    FqDrag:TQuaternion;

    FvNow:TAffineVector;
    FvDown:TAffineVector;
    FvFrom:TAffineVector;
    FvTo:TAffineVector;
    FvrFrom:TAffineVector;
    FvrTo:TAffineVector;

    FmNow:TMatrix;
    FmDown:TMatrix;

    FDragging:Boolean;
    FEnabled: Boolean;
    FEulerAngle : TGLCoordinates3;
    FEulerOrder : TGLEulerOrder;

    procedure SetViewer(const Value: TGLSceneViewer);
    procedure SetSelectedObj(const Value: TGLBaseSceneObject);
    function GetSelectedObj: TGLBaseSceneObject;

    procedure SetEulerAngle(aRotation: TGLCoordinates3);
  protected
    procedure DoDrawArcRender(Sender: TObject; var rci: TGLRenderContextInfo);
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CoordinateChanged(Sender: TGLCustomCoordinates);override;

    Procedure Reset;

    Procedure PointRotateArcBall(Var aPoint3D:TVector);

    Procedure BeginDrag;
    Procedure EndDrag;

    Procedure Update;

    procedure ViewerMouseMove(const X, Y: Integer);
    procedure ViewerMouseDown(const X, Y: Integer);
    procedure ViewerMouseUp(const X, Y: Integer);

    Property Dragging:Boolean Read FDragging;
    Property Matrix:TMatrix Read FmNow;
  published
    property Viewer: TGLSceneViewer read FViewer write SetViewer;
    property SelectedObj: TGLBaseSceneObject read GetSelectedObj write SetSelectedObj;
    Property Radius : Single Read FRadius Write FRadius;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property EulerAngles : TGLCoordinates3 Read FEulerAngle Write SetEulerAngle;
  end;

type

  { TMainForm }

  TMainForm = class(TForm)
    edtPitch: TFloatSpinEdit;
    edtRoll: TFloatSpinEdit;
    edtYaw: TFloatSpinEdit;
    GLCadencer1: TGLCadencer;
    DCWorld: TGLDummyCube;
    GLCamera1: TGLCamera;
    GLCube1: TGLCube;
    GLLightSource1: TGLLightSource;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLSceneViewer2: TGLSceneViewer;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;

    procedure edtRollEditingDone(Sender: TObject);
    procedure edtPitchEditingDone(Sender: TObject);
    procedure edtYawEditingDone(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  private
     FArcBall : TGLGizmoArcBall;

  protected
    mx, my : integer;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  OpenGLTokens, GLContext;

// Halve arc between unit vectors v0 and v1.
// Réduit de moitié l'arc entre les vecteurs unitaires v0 et v1
Function VectorBisect(Const A,B:TAffineVector):TAffineVector;
Var
  Len:Single;
Begin
  Result:=VectorAdd(A,B);

  Len:=VectorLength(Result);
  If (Len<1.0e-5) Then
    Result:= NullVector
  Else
    Result:=VectorScale(Result,1/Len);
End;

//Compute the position of the mouse on sphere
//The mouse click defines X and Y, so
//this routine calculates the Z [Depth] of the click
Function MouseOnSphere(Pos:TAffineVector):TAffineVector;
Var
  Mag,Scale:Single;
  Ballmouse:TAffineVector;
Begin
  Ballmouse.X := Pos.X;
  Ballmouse.Y := Pos.Y;
  Mag:=Sqr(Ballmouse.X)+ Sqr(Ballmouse.Y);
  If Mag>1 Then
  Begin
    Scale:=1.0/Sqrt(Mag);
    Ballmouse.X := Scale * Ballmouse.X;
    Ballmouse.Y := Scale * Ballmouse.Y;
    Ballmouse.Z := 0;
  End Else
    Ballmouse.Z:=Sqrt(1-Mag);

  Result:=Ballmouse;
End;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin

end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  FArcBall := TGLGizmoArcBall.Create(Self);
  FArcBall.SelectedObj := GLCube1;
  FArcBall.Viewer := GLSceneViewer1;
end;

procedure TMainForm.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TMainForm.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
  FArcBall.ViewerMouseDown(X,Y);
end;

procedure TMainForm.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (shift = [ssLeft]) then FArcBall.ViewerMouseMove(X,Y);
  mx := X;
  my := Y;
end;

procedure TMainForm.GLSceneViewer1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FArcBall.ViewerMouseUp(X,Y);
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FArcBall.Free;
end;

procedure TMainForm.edtPitchEditingDone(Sender: TObject);
begin
  if Assigned(FArcBall) then FArcBall.EulerAngles.Z := edtPitch.Value;
end;

procedure TMainForm.edtRollEditingDone(Sender: TObject);
begin
  if Assigned(FArcBall) then  FArcBall.EulerAngles.X := edtRoll.Value;
end;

procedure TMainForm.edtYawEditingDone(Sender: TObject);
begin
  if Assigned(FArcBall) then  FArcBall.EulerAngles.Y := edtYaw.Value;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
end;



{%region%=====[ TGLGizmoArcBall ]===============================================}

constructor TGLGizmoArcBall.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  FRadius:=512;
  FUIBaseGizmo := TGLDummyCube.Create(Self);
  FDrawArcRender := TGLDirectOpenGL(FUIBaseGizmo.AddNewChild(TGLDirectOpenGL));
  FDrawArcRender.OnRender := @DoDrawArcRender;
  FEulerAngle := TGLCoordinates3.Create(Self);

  Enabled := True;
  Reset;
End;

destructor TGLGizmoArcBall.Destroy;
begin
  FUIBaseGizmo.DeleteChildren;
  FUIBaseGizmo.Free;
  //if Assigned(FDrawArcRender) then FDrawArcRender.Free;
  inherited;
end;

procedure TGLGizmoArcBall.CoordinateChanged(Sender: TGLCustomCoordinates);
begin
  if Assigned(FSelectedObj) then
    begin
      FqNow := QuaternionFromRollPitchYaw(FEulerAngle.Z,FEulerAngle.X,FEulerAngle.Y);
      QuaternionToPoints(FqDown, FvrFrom, FvrTo);
      FmNow := QuaternionToMatrix(QuaternionConjugate(FqNow));
      FSelectedObj.Matrix := FmNow;
      FSelectedObj.TransformationChanged;
    end;
end;

procedure TGLGizmoArcBall.SetViewer(const Value: TGLSceneViewer);
begin
  if FViewer <> Value then
  begin
    if FViewer <> nil then
      FViewer.RemoveFreeNotification(Self);
    FViewer := Value;
    if FViewer <> nil then
      FViewer.FreeNotification(Self);
  end;
end;

procedure TGLGizmoArcBall.SetSelectedObj(const Value: TGLBaseSceneObject);
begin
  if FSelectedObj <> Value then
  begin
    if FSelectedObj <> nil then
      FSelectedObj.RemoveFreeNotification(Self);
    FSelectedObj := Value;
    if FSelectedObj <> nil then
      FSelectedObj.FreeNotification(Self);
    FUIBaseGizmo.MoveTo(Value);
  end;
end;

function TGLGizmoArcBall.GetSelectedObj: TGLBaseSceneObject;
begin
  result := FSelectedObj;
end;

procedure TGLGizmoArcBall.SetEulerAngle(aRotation: TGLCoordinates3);
//var
//  r, p, t : Double;
begin
  FEulerAngle.Assign(aRotation);
  FEulerAngle.NotifyChange(Self);
end;

procedure TGLGizmoArcBall.DoDrawArcRender(Sender: TObject; var rci: TGLRenderContextInfo);
Const
  S=64;
Var
  I:Integer;

  Procedure DrawArc(Const vFrom,vTo:TAffineVector);
  Const
    LG_NSEGS = 4;
    NSEGS = 1 shl LG_NSEGS;
  Var
    I,J:Integer;
    Pts:Array[0..NSEGS] Of TAffineVector;
    Dot:Single;
  Begin
    Pts[0] := vFrom;
    Pts[NSEGS] := vTo;
    Pts[1] := vTo;

    For I:=0 To Pred(LG_NSEGS) Do
      pts[1]:=VectorBisect(pts[0], pts[1]);
    Dot:=2*(pts[0].X*pts[1].X + pts[0].Y*pts[1].Y + pts[0].Z*pts[1].Z);

    For I:=2 To Pred(NSEGS) Do
      For j:=0 To 2 Do
        pts[i].V[j] := pts[i-1].V[j]*dot - pts[i-2].V[j];

    gl.Begin_(GL_LINE_STRIP);
      For I:= 0 To NSEGS Do
        gl.Vertex3fv(@pts[i]);
    gl.End_;
  End;


Begin

  if Not(Assigned(FSelectedObj)) then exit;
  If Not Dragging then Exit;

  rci.GLStates.Disable(stLighting);
  gl.Color3ub(255,255,255);

  gl.PushMatrix;
  gl.LoadIdentity;
  gl.MatrixMode(GL_PROJECTION);
  gl.PushMatrix;
  gl.LoadIdentity;
  gl.MatrixMode(GL_MODELVIEW);

  gl.Begin_(GL_LINE_LOOP);
  For I:=0 To Pred(S) Do
    gl.Vertex3f(cos(c2PI*i/S), sin(c2PI*i/S), 0);
  gl.End_;

  gl.Begin_(GL_POINTS);
  gl.Vertex3fv(@FvFrom);
  gl.Vertex3fv(@FvTo);
  gl.End_;

  DrawArc(FvFrom, FvTo);

  gl.MatrixMode(GL_PROJECTION);
  gl.PopMatrix;

  gl.MatrixMode(GL_MODELVIEW);
  gl.PopMatrix;

  rci.GLStates.Enable(stLighting);
End;

procedure TGLGizmoArcBall.Reset;
Begin
  FvDown := NullVector;
  FvNow  := NullVector;
  FqDown := IdentityQuaternion;
  FqNow  := IdentityQuaternion;

  FmNow  := IdentityHmgMatrix;
  FmDown := IdentityHmgMatrix;
End;

//Defines the X,Y,Z rotation for a point
procedure TGLGizmoArcBall.PointRotateArcBall(Var aPoint3D: TVector);
Var
  V:TVector;
Begin
  V := aPoint3D;

  aPoint3D.X := round(V.X * FmNow.V[0].X + V.Y * FmNow.V[1].X + V.Z * FmNow.V[2].X {+ V.W * M[W, X]});
  aPoint3D.Y := round(V.X * FmNow.V[0].Y + V.Y * FmNow.V[1].Y + V.Z * FmNow.V[2].Y {+ V.W * gBall.VNow[W, Y]});
  aPoint3D.Z := round(V.X * FmNow.V[0].Z + V.Y * FmNow.V[1].Z + V.Z * FmNow.V[2].Z {+ V.W * gBall.VNow[W, Z]});
  aPoint3D.W := 1.0;
End;

//sets the boolean 'dragging' to true. Mouse movements will now be automatically updated
procedure TGLGizmoArcBall.BeginDrag;
Begin
  FDragging := True;
  FvDown := FvNow;
End;

//updates the matrix mDown with the current position
procedure TGLGizmoArcBall.EndDrag;
Begin
  If Not FDragging Then Exit;
  FvDown := FvNow;
  FmDown := FmNow;
  FDragging := False;
End;

procedure TGLGizmoArcBall.Update;
Begin
  FvFrom := MouseOnSphere(FvDown);
  FvTo := MouseOnSphere(FvNow);

  If FDragging then
  Begin
    FqDrag := QuaternionFromPoints(FvFrom, FvTo);
    FqNow := QuaternionMultiply(FqDrag, FqDown);
  End;

  QuaternionToPoints(FqDown, FvrFrom, FvrTo);
  FmNow := QuaternionToMatrix(QuaternionConjugate(FqNow));

  //Note: new position is composite of starting position and
  //new rotation, so we multiply matrixes.
  FmNow := MatrixMultiply(FmDown, FmNow);

  FSelectedObj.Matrix := FmNow;

End;

procedure TGLGizmoArcBall.ViewerMouseMove(const X, Y: Integer);
Begin
  FvNow.X := (X-FRadius)/FRadius;
  FvNow.Y := -(Y-FRadius)/FRadius;
  FvNow.Z := 0;

  If FDragging Then Update;


End;

procedure TGLGizmoArcBall.ViewerMouseDown(const X, Y: Integer);
begin
  BeginDrag;
end;

procedure TGLGizmoArcBall.ViewerMouseUp(const X, Y: Integer);
begin
  EndDrag;
end;


{%endregion%}

end.

