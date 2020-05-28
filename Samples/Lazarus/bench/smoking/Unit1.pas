{: Benchmark and stress test for PFX.<p>

  Fires are made of additively blended particles, smoke of transparently
  blended ones. Smokes of distinct fires should hide each other, and smoke
  in a particular fire should hide its top flames a bit.<p>

  02/03/2005 - GF3 / AXP 2 GHz - 53 FPS
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLCadencer, GLParticleFX, GLPerlinPFX, GLScene, GLObjects,
  GLLCLViewer, ExtCtrls, GLCrossPlatform, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLSceneViewer: TGLSceneViewer;
    GLScene: TGLScene;
    GLCamera: TGLCamera;
    DCFire1: TGLDummyCube;
    ParticleFXRenderer: TGLParticleFXRenderer;
    SmokePFX: TGLPerlinPFXManager;
    FlamePFX: TGLCustomSpritePFXManager;
    GLCadencer: TGLCadencer;
    DCTarget: TGLDummyCube;
    Timer: TTimer;
    procedure GLCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.GLCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   SmokePFX.Rotation:=newTime;
   GLSceneViewer.Invalidate;
end;

procedure TForm1.TimerTimer(Sender: TObject);
begin
   Caption:= GLSceneViewer.FramesPerSecondText
            +Format(' - %d Particles - %.3f ms Sort',
                    [SmokePFX.ParticleCount+FlamePFX.ParticleCount,
                     ParticleFXRenderer.LastSortTime]);
   GLSceneViewer.ResetPerformanceMonitor;
end;

end.