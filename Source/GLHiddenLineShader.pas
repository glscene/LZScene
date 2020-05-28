//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   A shader that renders hidden (back-faced) lines differently from visible
   (front) lines. Polygon offset is used to displace fragments depths a little
   so that there is no z-fighting in rendering the same geometry multiple times.

    History :  
       23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
       22/04/10 - Yar - Fixes after GLState revision
       05/03/10 - DanB - More state added to TGLStateCache
       06/06/07 - DaStr - Added $I GLScene.inc
                             Added GLColor to uses (BugtrackerID = 1732211)
       25/02/07 - DaStr - Moved registration to GLSceneRegister.pas
       25/09/04 - NelC - Fixed bug of disabled blend (thx Carlos)
       05/02/04 - NelC - Fixed memory leak in TGLHiddenLineShader.Destroy (thx Achim Hammes)
       13/12/03 - NelC - Added SurfaceLit, ShadeModel
       05/12/03 - NelC - Added ForceMaterial
       03/12/03 - NelC - Creation. Modified from the HiddenLineShader in
                            the multipass demo.
    
}
unit GLHiddenLineShader;

interface

{$I GLScene.inc}

uses
  Classes,
  GLMaterial, OpenGLTokens, GLCrossPlatform, GLScene, GLColor,
  GLBaseClasses, GLRenderContextInfo, GLState, GLContext;

type
  TGLLineSettings = class(TGLUpdateAbleObject)
  private
     
    FColor: TGLColor;
    FWidth: Single;
    FPattern: TGLushort;

    FForceMaterial: Boolean;

    procedure SetPattern(const value: TGLushort);
    procedure SetColor(const v: TGLColor);
    procedure SetWidth(const Value: Single);
    procedure SetForceMaterial(v: boolean);

  public
     
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Apply(var rci: TGLRenderContextInfo);
    procedure UnApply(var rci: TGLRenderContextInfo);

  published
     
    property Width: Single read FWidth write SetWidth;
    property Color: TGLColor read FColor write SetColor;
    property Pattern: TGLushort read FPattern write SetPattern default $FFFF;
    { Set ForceMaterial to true to enforce the application of the line settings
       for objects that sets their own color, line width and pattern. }
    property ForceMaterial: Boolean read FForceMaterial write SetForceMaterial
      default false;
  end;

  TGLHiddenLineShader = class(TGLShader)
  private
    FPassCount: integer;

    FLineSmooth: Boolean;
    FSolid: Boolean;

    FBackGroundColor: TGLColor;

    FFrontLine: TGLLineSettings;
    FBackLine: TGLLineSettings;

    FLighting: Boolean;
    FShadeModel: TGLShadeModel;

    procedure SetlineSmooth(v: boolean);
    procedure SetSolid(v: boolean);
    procedure SetBackgroundColor(AColor: TGLColor);
    procedure SetLighting(v: boolean);
    procedure SetShadeModel(const val: TGLShadeModel);

  protected
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;

  public
     
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
     
    property FrontLine: TGLLineSettings read FFrontLine write FFrontLine;
    property BackLine: TGLLineSettings read FBackLine write FBackLine;
    { Line smoothing control }
    property LineSmooth: Boolean read FlineSmooth write SetlineSmooth default
      false;
    { Solid controls if you can see through the front-line wireframe. }
    property Solid: Boolean read FSolid write SetSolid default false;
    { Color used for solid fill. }
    property BackgroundColor: TGLColor read FBackgroundColor write
      SetBackgroundColor;
    { When Solid is True, determines if lighting or background color is used. }
    property SurfaceLit: Boolean read FLighting write SetLighting default true;
    { Shade model.
       Default is "Smooth". }
    property ShadeModel: TGLShadeModel read FShadeModel write SetShadeModel
      default smDefault;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------
// ------------------ TGLLineSettings ------------------
// ------------------

// Create
//

constructor TGLLineSettings.Create(AOwner: TPersistent);
begin
  inherited;
  FColor := TGLColor.Create(Self);
  FColor.Initialize(clrGray20);
  FWidth := 2;
  Pattern := $FFFF;
  ForceMaterial := false;
end;

// Destroy
//

destructor TGLLineSettings.Destroy;
begin
  FColor.Free;
  inherited;
end;

// SetPattern
//

procedure TGLLineSettings.SetPattern(const value: TGLushort);
begin
  if FPattern <> value then
  begin
    FPattern := Value;
    NotifyChange(self);
  end;
end;

// SetColor
//

procedure TGLLineSettings.SetColor(const v: TGLColor);
begin
  FColor.Color := v.Color;
  NotifyChange(Self);
end;

// SetWidth
//

procedure TGLLineSettings.SetWidth(const Value: Single);
begin
  FWidth := Value;
  NotifyChange(Self);
end;

var
  IgnoreMatSave: boolean;

  // Apply
  //

procedure TGLLineSettings.Apply(var rci: TGLRenderContextInfo);
begin
  rci.GLStates.LineWidth := Width;
  GL.Color4fv(Color.AsAddress);
  if Pattern <> $FFFF then
  begin
    rci.GLStates.Enable(stLineStipple);
    rci.GLStates.LineStippleFactor := 1;
    rci.GLStates.LineStipplePattern := Pattern;
  end
  else
    rci.GLStates.Disable(stLineStipple);

  if ForceMaterial then
  begin
    IgnoreMatSave := rci.ignoreMaterials;
    rci.ignoreMaterials := true;
  end;
end;

// UnApply
//

procedure TGLLineSettings.UnApply(var rci: TGLRenderContextInfo);
begin
  if ForceMaterial then
    rci.ignoreMaterials := IgnoreMatSave;
end;

// SetForceMaterial
//

procedure TGLLineSettings.SetForceMaterial(v: boolean);
begin
  if FForceMaterial <> v then
  begin
    FForceMaterial := v;
    NotifyChange(self);
  end;
end;

// ------------------
// ------------------ TGLHiddenLineShader ------------------
// ------------------

// Create
//

constructor TGLHiddenLineShader.Create(AOwner: TComponent);
begin
  inherited;
  FFrontLine := TGLLineSettings.Create(self);
  FBackLine := TGLLineSettings.Create(self);
  FSolid := false;

  FBackgroundColor := TGLColor.Create(Self);
  FBackgroundColor.Initialize(clrBtnFace);

  FLineSmooth := False;
  FLighting := true;
  FShadeModel := smDefault;
end;

// Destroy
//

destructor TGLHiddenLineShader.Destroy;
begin
  FFrontLine.Free;
  FBackLine.Free;
  FBackgroundColor.Free;
  inherited;
end;

// DoApply
//

procedure TGLHiddenLineShader.DoApply(var rci: TGLRenderContextInfo; Sender:
  TObject);
begin
  FPassCount := 1;

  if solid then
    with rci.GLStates do
    begin
      // draw filled front faces in first pass
      PolygonMode := pmFill;
      CullFaceMode := cmBack;

      if FLighting then
      begin
        case ShadeModel of
          smDefault, smSmooth: GL.ShadeModel(GL_SMOOTH);
          smFlat: GL.ShadeModel(GL_FLAT);
        end
      end
      else
      begin
        Disable(stLighting);
        GL.Color4fv(FBackgroundColor.AsAddress); // use background color
      end;
      // enable and adjust polygon offset
      Enable(stPolygonOffsetFill);
    end
  else
    with rci.GLStates do
    begin
      Disable(stLighting);
      // draw back lines in first pass
      FBackLine.Apply(rci);
      CullFaceMode := cmFront;
      PolygonMode := pmLines;
      // enable and adjust polygon offset
      Enable(stPolygonOffsetLine);
    end;

  rci.GLStates.SetPolygonOffset(1, 2);
end;

// DoUnApply
//

function TGLHiddenLineShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;

  procedure SetLineSmoothBlend;
  begin
    with rci.GLStates do
    begin
      LineStippleFactor := 1;
      LineStipplePattern := $FFFF;
      if LineSmooth then
      begin
        LineSmoothHint := hintNicest;
        Enable(stLineSmooth);
      end
      else
        Disable(stLineSmooth);

      if LineSmooth or (FBackLine.FColor.Alpha < 1)
        or (FFrontLine.FColor.Alpha < 1) then
      begin
        Enable(stBlend);
        SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end
      else
        Disable(stBlend);
    end;
  end;

begin
  case FPassCount of
    1:
      with rci.GLStates do begin
        // draw front line in 2nd pass
        FPassCount := 2;

        FBackLine.UnApply(rci);
        FFrontLine.Apply(rci);

        SetLineSmoothBlend;

        if solid and FLighting then
          Disable(stLighting);

        PolygonMode := pmLines;
        CullFaceMode := cmBack;

        if solid then
          rci.GLStates.Disable(stPolygonOffsetFill)
        else
          rci.GLStates.Disable(stPolygonOffsetLine);

        Result := True;
      end;
    2:
      begin
        FFrontLine.UnApply(rci);
        rci.GLStates.PolygonMode := pmFill;
        Result := false;
      end;
  else
    Assert(False);
    Result := False;
  end;
end;

// SetBackgroundColor
//

procedure TGLHiddenLineShader.SetBackgroundColor(AColor: TGLColor);
begin
  FBackgroundColor.Color := AColor.Color;
  NotifyChange(Self);
end;

// SetlineSmooth
//

procedure TGLHiddenLineShader.SetlineSmooth(v: boolean);
begin
  if FlineSmooth <> v then
  begin
    FlineSmooth := v;
    NotifyChange(self);
  end;
end;

// SetLighting
//

procedure TGLHiddenLineShader.SetLighting(v: boolean);
begin
  if FLighting <> v then
  begin
    FLighting := v;
    NotifyChange(self);
  end;
end;

// SetSolid
//

procedure TGLHiddenLineShader.SetSolid(v: boolean);
begin
  if FSolid <> v then
  begin
    FSolid := v;
    NotifyChange(self);
  end;
end;

// SetShadeModel
//

procedure TGLHiddenLineShader.SetShadeModel(const val: TGLShadeModel);
begin
  if FShadeModel <> val then
  begin
    FShadeModel := val;
    NotifyChange(Self);
  end;
end;

end.

