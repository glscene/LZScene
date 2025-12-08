{ This program shows how simple it is to set up a fractal landscape. I've commented
  the objects created at design time.
  Basically, you need a SceneViewer, a Scene, a Camera, a MaterialLibrary and
  a TerrainRenderer; make the latter child of a DummyCube if you want to be able
  to rescale it (to create wider perspective). All these objects must be linked
  properly as shown in other tutorials.

  This code just build and display a landscape. You can't navigate it nor rotate the angle
  of view.

  Alexandre Hirzel, July 2003
}
program ShortestLandscapeApp;

{$MODE Delphi}

uses
  Forms, Interfaces,
  ShortestU in 'ShortestU.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
