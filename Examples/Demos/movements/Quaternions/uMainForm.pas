unit uMainForm;

interface

uses
  LCLType, LCLIntf, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin, //UIMU,
  GLVectorGeometry, GLObjects,
  GLCadencer, GLScene, GLLCLViewer,  OpenGL1x, GLState, GLRenderContextInfo, GLVectorTypes,
  GLCoordinates, GLGeomObjects, GLBaseClasses;

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
    FEulerOrder : TEulerOrder;

    procedure SetEulerOrder(AValue: TEulerOrder);
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
    property EulerOrder : TEulerOrder Read FEulerOrder Write SetEulerOrder;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    DCWorld: TGLDummyCube;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLCube1: TGLCube;
    GLLightSource1: TGLLightSource;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblRealPitch: TLabel;
    lblrealRoll: TLabel;
    lblRealYaw: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    ScrollBar3: TScrollBar;
    edtPitch: TSpinEdit;
    edtRoll: TSpinEdit;
    edtYaw: TSpinEdit;
    //ComLed2: TComLed;
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure edtPitchEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure ScrollBar1Change(Sender: TObject);
    procedure ScrollBar2Change(Sender: TObject);
    procedure ScrollBar3Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    //procedure ComComboBox2Change(Sender: TObject);
    //procedure ComComboBox1Change(Sender: TObject);
  private
    { Private declarations }
    FArcBall : TGLGizmoArcBall;

    procedure UpdateQ();
  protected
      mx, my : integer;
  public

    FromEulerOrder, ToEulerOrder : TEulerOrder;
    { Public declarations }
  end;

var
  MainForm: TMainForm;

  VQuaternion: TQuaternion;

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

{%region%=====[ TGLGizmoArcBall ]===============================================}

constructor TGLGizmoArcBall.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  FRadius:=512;
  FUIBaseGizmo := TGLDummyCube.Create(Self);
  FDrawArcRender := TGLDirectOpenGL(FUIBaseGizmo.AddNewChild(TGLDirectOpenGL));
  FDrawArcRender.OnRender := @DoDrawArcRender;
  FEulerAngle := TGLCoordinates3.Create(Self);
  FEulerOrder := eulYZX;
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
      FqNow := QuaternionFromEuler(FEulerAngle.X,FEulerAngle.Y,FEulerAngle.Z, FEulerOrder);  // QuaternionFromRollPitchYaw(FEulerAngle.Z,FEulerAngle.X,FEulerAngle.Y);
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
    if FViewer <> nil then FViewer.RemoveFreeNotification(Self);
    FViewer := Value;
    if FViewer <> nil then FViewer.FreeNotification(Self);
  end;
end;

procedure TGLGizmoArcBall.SetEulerOrder(AValue: TEulerOrder);
begin
  if FEulerOrder=AValue then Exit;
  FEulerOrder:=AValue;
  CoordinateChanged(FEulerAngle);

  //FEulerAngle.NotifyChange(Self);
end;

procedure TGLGizmoArcBall.SetSelectedObj(const Value: TGLBaseSceneObject);
begin
  if FSelectedObj <> Value then
  begin
    if FSelectedObj <> nil then FSelectedObj.RemoveFreeNotification(Self);
    FSelectedObj := Value;
    if FSelectedObj <> nil then FSelectedObj.FreeNotification(Self);
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

procedure TMainForm.Button1Click(Sender: TObject);
begin
  //if not ComPort1.Connected
  //then begin
  //  //ComPort1.Open;
  //  Button1.Caption := 'Close';
  //end
  //else begin
  //  //ComPort1.Close;
  //  Button1.Caption := 'Open';
  //end;

end;

//procedure TMainForm.ComComboBox1Change(Sender: TObject);
//begin
//   // ShowMessage(ComComboBox1.Items[ComComboBox1.ItemIndex]);
//   //ComPort1.Port := ComComboBox1.Items[ComComboBox1.ItemIndex];
//end;
//
//procedure TMainForm.ComComboBox2Change(Sender: TObject);
//begin
//  // ComPort1.BaudRate := TBaudRate(ComComboBox2.ItemIndex);
//end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
   EdtRoll.value := 0;
   EdtPitch.Value := 0;
   edtYaw.Value := 0;
   FromEulerOrder := eulZXY;
   ToEulerOrder := eulZXY;
   UpdateQ();
   //ComPort1.BaudRate := br115200;
   //ComComboBox2.ItemIndex := Ord(br115200);
end;

procedure TMainForm.ScrollBar1Change(Sender: TObject);
begin
   edtRoll.Value := ScrollBar1.Position-180;
   UpdateQ;

end;

procedure TMainForm.ComboBox1Change(Sender: TObject);
begin
  FromEulerOrder := TEulerOrder(Combobox1.ItemIndex);
  Combobox2.ItemIndex := Combobox1.ItemIndex;
  ToEulerOrder := FromEulerOrder;
  FArcBall.EulerOrder:= FromEulerOrder;
  FArcBall.CoordinateChanged(nil);
  UpdateQ();
end;

procedure TMainForm.ComboBox2Change(Sender: TObject);
begin
  ToEulerOrder := TEulerOrder(Combobox2.ItemIndex);
  UpdateQ();
end;

procedure TMainForm.edtPitchEditingDone(Sender: TObject);
begin
  ScrollBar1.Position := edtRoll.Value + 180;
  ScrollBar2.Position := edtPitch.Value + 180;
  ScrollBar3.Position := edtYaw.Value + 180;

  UpdateQ;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FArcBall.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  FArcBall := TGLGizmoArcBall.Create(Self);
  FArcBall.SelectedObj := GLCube1;
  FArcBall.Viewer := GLSceneViewer1;
  FArcBall.EulerOrder:= eulZXY;
end;

procedure TMainForm.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TMainForm.ScrollBar2Change(Sender: TObject);
begin
   edtPitch.Value := ScrollBar2.Position-180;
   UpdateQ;
end;

procedure TMainForm.ScrollBar3Change(Sender: TObject);
begin
   edtYaw.Value := ScrollBar3.Position-180;
   UpdateQ;
end;

//function QuatAxesAng(qt: TQuaternion): TQuatAxesAng;
//var
//  AxesAng: TQuatAxesAng;
//  qw2: Double;
//  den: Double;
//begin
//  qw2 := qt[0]*qt[0];
//  AxesAng.Angle := 2*ArcCos(qt[0])*180/PI;
//  den := Sqrt(1-qw2);
//  if den <> 0
//  then begin
//         AxesAng.Vector.x := qt[1]/den;
//         AxesAng.Vector.y := qt[2]/den;
//         AxesAng.Vector.z := qt[3]/den;
//       end
//  else begin
//         AxesAng.Vector.x := 0;
//         AxesAng.Vector.y := 0;
//         AxesAng.Vector.z := 0;
//       end;
//  Result := AxesAng;
//end;



procedure TMainForm.UpdateQ();
var
  // VQuaternion: Quaternion;
   vq : TQuaternion;
  // VQuatAxesAng: TQuatAxesAng;
  Angle : Single;
  Axis : TAffineVector;
   Roll, Pitch, Yaw: Double;
  // Ang: Angles;
begin
   Roll  := edtRoll.Value;
   Pitch := edtPitch.Value;
   Yaw   := edtYaw.Value;
   //VQuaternion:=EulerToQuat(Roll, Pitch, Yaw); //
   // Roll, Yaw, Pitch = X, Y ,Z Axis
   vq := QuaternionFromEuler(Roll, Yaw, Pitch, FromEulerOrder);
   // QuaternionFromRollPitchYaw(Roll, Pitch, Yaw); --> Default EULER ORDER = YZX like the nasa

   Edit4.Text := FloatToStr(Vq.ImagPart.X);
   Edit5.Text := FloatToStr(Vq.ImagPart.Y);
   Edit6.Text := FloatToStr(Vq.ImagPart.Z);
   Edit7.Text := FloatToStr(Vq.RealPart);

   //VQuaternion[1] := VQ.ImagPart.V[0];
   //VQuaternion[2] := VQ.ImagPart.V[1];
   //VQuaternion[3] := VQ.ImagPart.V[2];
   //VQuaternion[0] := VQ.RealPart;
   //
   //
   //Edit4.Text :=  FloatToStr(VQuaternion[1]);
   //Edit5.Text := FloatToStr(VQuaternion[2]);
   //Edit6.Text := FloatToStr(VQuaternion[3]);
   //Edit7.Text := FloatToStr(VQuaternion[0]);

  //   VQuatAxesAng:= QuatAxesAng(VQuaternion);
   QuaternionToAngleAxis(Vq, Angle,Axis);
   Edit8.Text := FloatToStr(Angle);
   Edit9.Text := FloatToStr(Axis.x);
   Edit10.Text := FloatToStr(Axis.y);
   Edit11.Text := FloatToStr(Axis.z);
  // Ang := QuatToEuler(VQuaternion);

   QuaternionToEuler(vq,ToEulerOrder,Roll,Pitch,Yaw);

   LblRealRoll.Caption := FloatToStr(Roll);
   LblRealYaw.Caption := FloatToStr(Yaw);
   LblRealPitch.Caption := FloatToStr(Pitch);

   Edit12.Text := FloatToStr(Round(Roll));
   Edit13.Text := FloatToStr(Round(Yaw));
   Edit14.Text := FloatToStr(Round(Pitch));

  if Assigned(FArcBall) then FArcBall.EulerAngles.X := edtRoll.Value;
  if Assigned(FArcBall) then FArcBall.EulerAngles.Y := edtYaw.Value;
  if Assigned(FArcBall) then FArcBall.EulerAngles.Z := edtPitch.Value;
end;

end.
