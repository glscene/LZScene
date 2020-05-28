//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   A shader that applies a render pass for each material in
   its assigned MaterialLibrary.

    History :  
       22/04/10 - Yar - Fixes after GLState revision
       05/03/10 - DanB - Added more state to TGLStateCache
       03/07/09 - DanB - bug fix to allow multi-pass materials to be used by TGLMultiMaterialShader 
       20/01/09 - Mrqzzz - Published property "Shaderstyle"
                             (allows f.ex to have multiple textures using lightmaps)
       25/10/07 - Mrqzzz - commented "glPushAttrib(GL_ALL_ATTRIB_BITS);" in DoApply
                              and "glPopAttrib;" in DoUnapply, which seems to fix
                              issues with other objects and materials in the scene.

       25/02/07 - DaStr - Moved registration to GLSceneRegister.pas
       24/05/04 - Mrqzzz - Re-added design-time rendering option
                          (seems stable now)
       29/07/03 - SG - Removed design-time rendering option
                          (shader unstable at design-time)
       29/07/03 - SG - Creation
    
}
unit GLMultiMaterialShader;

interface

uses
   Classes, GLMaterial, GLRenderContextInfo, GLState;

type
   TGLMultiMaterialShader = class(TGLShader)
      private
         FPass : Integer;
         FMaterialLibrary : TGLMaterialLibrary;
         FVisibleAtDesignTime: boolean;
         FShaderActiveAtDesignTime : boolean;
    FShaderStyle: TGLShaderStyle;
    procedure SetVisibleAtDesignTime(const Value: boolean);
    procedure SetShaderStyle(const Value: TGLShaderStyle);
      protected
         procedure SetMaterialLibrary(const val : TGLMaterialLibrary);
         procedure DoApply(var rci : TGLRenderContextInfo; Sender : TObject); override;
         function DoUnApply(var rci : TGLRenderContextInfo) : Boolean; override;
      public
         constructor Create(aOwner : TComponent); override;
      published
         property MaterialLibrary : TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
         property VisibleAtDesignTime : boolean read FVisibleAtDesignTime write SetVisibleAtDesignTime;
         property ShaderStyle:TGLShaderStyle read FShaderStyle write SetShaderStyle;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLMultiMaterialShader ------------------
// ------------------

// Create
//
constructor TGLMultiMaterialShader.Create(aOwner : TComponent);
begin
   inherited;
   FShaderStyle:=ssReplace;
   FVisibleAtDesignTime := False;
end;

// DoApply
//
procedure TGLMultiMaterialShader.DoApply(var rci: TGLRenderContextInfo; Sender : TObject);
begin
   if not Assigned(FMaterialLibrary) then exit;

   FShaderActiveAtDesignTime := FVisibleAtDesignTime;

   FPass:=1;
   if (not (csDesigning in ComponentState)) or FShaderActiveAtDesignTime then begin
      rci.ignoreDepthRequests := True;
      rci.GLStates.Enable(stDepthTest);
      rci.GLStates.DepthFunc := cfLEqual;
      if FMaterialLibrary.Materials.Count>0 then
         FMaterialLibrary.Materials[0].Apply(rci);
      rci.ignoreDepthRequests := False;
  end;
end;

// DoUnApply
//
function TGLMultiMaterialShader.DoUnApply(
   var rci: TGLRenderContextInfo): Boolean;
begin
   Result:=False;
   if not Assigned(FMaterialLibrary) then exit;
   if (not (csDesigning in ComponentState)) or FShaderActiveAtDesignTime then begin
      if FMaterialLibrary.Materials.Count>0 then
         // handle multi-pass materials
         if FMaterialLibrary.Materials[FPass-1].UnApply(rci) then
         begin
           Result:=true;
           Exit;
         end;
      if (FPass >= FMaterialLibrary.Materials.Count) then begin
         rci.GLStates.DepthFunc := cfLess;
         exit;
      end;
      FMaterialLibrary.Materials[FPass].Apply(rci);
      Result:=True;
      Inc(FPass);
   end;
end;

// SetMaterialLibrary
//
procedure TGLMultiMaterialShader.SetMaterialLibrary(
   const val: TGLMaterialLibrary);
begin
   if val<>FMaterialLibrary then begin
      FMaterialLibrary:=val;
      NotifyChange(Self);
   end;
end;

procedure TGLMultiMaterialShader.SetShaderStyle(const Value: TGLShaderStyle);
begin
  FShaderStyle := Value;
  inherited ShaderStyle :=FShaderStyle;
end;

procedure TGLMultiMaterialShader.SetVisibleAtDesignTime(
  const Value: boolean);
begin
  FVisibleAtDesignTime := Value;
  if csDesigning in ComponentState then
     NotifyChange(Self);
end;

end.
