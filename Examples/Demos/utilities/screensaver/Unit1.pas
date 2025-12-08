{: Sample screen-saver for GLScene using TScreenSaver.<p>

	I've kept the OpenGL stuff to a minimum here, the saver is just animating
	a cube on your screen, with 3 colored dancing light source. All movements
	are handled through Inertia behaviours and the Cadencer, this is why you
	won't find any	animation code here :).<p>

	This saver has two forms : this one is the main saver screen, Form2 is the
	properties screen (adjust torus tesselation).
	Apart from dropping a TScreenSaver on Form1 and handling OnpropertiesRequested
	to display Form2, I did these things :<ul>
	<li>changed the extension to "scr" in Project/Options/Application
	<li>removed Form2 from the autocreate list (I moved the code from project1.dpr
		to the OnPropertiesRequest event code
	</ul>
	In most cases, these are the only steps required for making a screen-saver.<p>

	NB : Password stuff does not work under NT.
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, ScreenSaver, GLObjects, GLBehaviours, GLCadencer,
  GLWin32Viewer, GLGeomObjects, GLCrossPlatform, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    ScreenSaver1: TScreenSaver;
	 GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    DummyCube2: TGLDummyCube;
    DummyCube3: TGLDummyCube;
    DummyCube4: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLLightSource2: TGLLightSource;
    GLLightSource3: TGLLightSource;
    Torus1: TGLTorus;
    GLCadencer1: TGLCadencer;
    procedure ScreenSaver1PropertiesRequested(Sender: TObject);
	 procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
	 procedure FormCreate(Sender: TObject);
  private
	 { Déclarations privées }
  public
	 { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses Unit2;

procedure TForm1.FormCreate(Sender: TObject);
begin
	// read our option
	case GetMeshResolutions of
		0 : begin // Low Res, this is one ugly torus
			Torus1.Rings:=8;
			Torus1.Sides:=6;
		end;
		1 : begin // High Res, should still look smooth at high resolutions
			Torus1.Rings:=64;
			Torus1.Sides:=32;
		end;
	end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
	// let the show begin :)
	GLCadencer1.Enabled:=True;
end;

procedure TForm1.ScreenSaver1PropertiesRequested(Sender: TObject);
begin
	// we create the dialog dans display it
	// we do not need to free it (TApplication will take care of this)
	Application.CreateForm(TForm2, Form2);
	Form2.ShowModal;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
	// "Rescale" when form size is changed so our saver always looks the same
	GLCamera1.FocalLength:=50*Width/400;
end;

end.
