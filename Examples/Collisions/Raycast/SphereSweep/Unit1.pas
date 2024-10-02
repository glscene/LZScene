unit Unit1;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,
  //GLS
  GLVectorFileObjects, GLScene, GLTexture, GLCadencer,  GLState,
  GLLCLViewer, GLObjects, GLCollision,
  GLNavigator, GLVectorLists, GLOctree, GLFile3DS, GLVectorGeometry,
  GLGeomObjects, GLMaterial, GLCoordinates, GLCrossPlatform, GLBaseClasses,
  GLRenderContextInfo, OpenGLTokens, GLContext;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    FirstPersonCamera: TGLCamera;
    Map: TGLFreeForm;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLLight: TGLLightSource;
    CollisionManager1: TGLCollisionManager;
    GLNavigator1: TGLNavigator;
    World: TGLDummyCube;
    GLArrowLine2: TGLArrowLine;
    ThirdPersonCamera: TGLCamera;
    PlayerSphere: TGLSphere;
    GLArrowLine1: TGLArrowLine;
    GLArrowLine3: TGLArrowLine;
    GLLightSource1: TGLLightSource;
    GLArrowLine4: TGLArrowLine;
    GLArrowLine5: TGLArrowLine;
    GLArrowLine6: TGLArrowLine;
    PlayerCentre: TGLSphere;
    GLDirectOpenGL1: TGLDirectOpenGL;
    Player: TGLDummyCube;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure DrawArrows(intPoint,intNormal,Ray:TVector; Arrow1, Arrow2:TGLArrowLine);
    // Basic idea is to OctreeSphereSweepIntersect to plane, update position then change
    //  velocity to slide along the plane
    //  Camera can collide with multiple planes (e.g. floor + multiple walls + ceiling)
    // limit iterations to 4 or 5 for now, may need to be higher for more complex maps or fast motion
    procedure SphereSweepAndSlide(freeform:TGLFreeform;SphereStart:TVector;var Velocity, newPosition:TVector;SphereRadius:Single);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GLDirectOpenGL1Render(Sender : TObject; var rci : TGLRenderContextInfo);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TContactPoint = record
    intPoint,intNormal:TVector;
  end;

  TCollisionState=class
    Position:TVector;
    Contact:TContactPoint;
    Time:Int64;
  end;

  TCollisionStates=class(TList)
  end;

var
  Form1: TForm1;
  CollisionStates:TCollisionStates;

implementation
uses GLKeyboard;
var yangle:double=90;
    xangle:double=0;
    Velocity:TVector=(X:0;Y:0;Z:0;W:0);
    Gravity:TVector=(X:0;Y:-9.81*20;Z:0;W:0);
    Wireframe:Boolean;
    MovementScale:Single=4;
    DisplayTime:Integer=2000;
    TickCount:Int64;    
{$R *.lfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  Map.LoadFromFile('room2.3ds');
  Map.BuildOctree();
  Map.Up.SetVector(0,1,0);
  CollisionStates:=TCollisionStates.Create;

  showCursor(false);
  setcursorpos(screen.width div 2,screen.Height div 2);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   caption:=Format('%.1f FPS',[GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
//   caption:=GLCamera1.Position.AsString;
end;

procedure TForm1.DrawArrows(intPoint,intNormal,Ray:TVector; Arrow1, Arrow2:TGLArrowLine);
begin
        Arrow1.Position.AsVector:=intPoint;
        Arrow1.Direction.AsVector:=intNormal;
        Arrow1.Scale.z:=VectorLength(intNormal);
        Arrow1.Move(Arrow1.Scale.z/2);
        Arrow1.Visible:=True;

        Arrow2.Position.AsVector:=intPoint;
        Arrow2.Direction.AsVector:=Ray;
        Arrow2.Visible:=True;
end;

procedure TForm1.SphereSweepAndSlide(freeform:TGLFreeform;SphereStart:TVector;var Velocity,newPosition:TVector;SphereRadius:Single);
var
  oldPosition, ray:TVector;
  vel,slidedistance:Single;
  intPoint,intNormal:TVector;
  newDirection, newRay,collisionPosition, pointOnSphere,point2OnSphere:TVector;
  i:integer;
  CollisionState:TCollisionState;
  NegNormalizedVelocity:TVector;
begin
  oldPosition:=SphereStart;

  //Direction sphere is moving in
  ray:=VectorSubtract(newPosition,oldPosition);
//  ray:=Velocity;
//  newPosition:=VectorAdd(newPosition,ray);
  //Speed of sphere
  vel:=VectorLength(ray);

  //if the Sphere is not moving, nothing is required
  // else do up to 7 loops

  if vel>0 then
  for i:=0 to 6 do
  begin
    //if an intersection occurs, will need to do further calculations
    if (freeform.OctreeSphereSweepIntersect(oldPosition,ray,vel,SphereRadius,@intPoint,@intNormal)) then
    begin
      if VectorDistance2(oldPosition,intPoint)<=sqr(SphereRadius) then
      begin
        //sphere is intersecting triangle
        intNormal:=VectorScale(VectorSubtract(oldPosition,intPoint),1.0001);
      end
      else
      begin
        //sphere is not intersecting triangle
        //intNormal:=VectorSubtract(oldPosition,intPoint);  //not correct but works okay at small time steps
        //intNormal:=VectorScale(VectorNormalize(intNormal),SphereRadius+0.0001);
        if RayCastSphereIntersect(intPoint,VectorNormalize(VectorNegate(ray)),oldPosition,SphereRadius,PointOnSphere,Point2OnSphere)>0 then
          intNormal:=VectorScale(VectorSubtract(oldPosition,PointOnSphere),1.0001)
          //intNormal:=VectorScale(VectorNormalize(VectorSubtract(oldPosition,PointOnSphere)),SphereRadius+0.001)//VectorDistance(oldPosition,PointOnSphere));
        else
        begin
//          Assert(False);  //this shouldn't happen (this is here for debugging)
          intNormal:=VectorScale(VectorSubtract(oldPosition,intPoint),1.0001);
        end;
        
      end;

      //calculate position of centre of sphere when collision occurs
      collisionPosition:=VectorAdd(intPoint,intNormal);
      oldPosition:=collisionPosition;

      //calculate distance that wasn't travelled, due to obstacle
      newRay:=VectorSubtract(newPosition,collisionPosition);

      //calculate new direction when a wall is hit (could add bouncing to this)
      newDirection:=VectorCrossProduct(intNormal,VectorCrossProduct(newRay,intNormal));
      if VectorNorm(NewDirection)>0 then
        NormalizeVector(newDirection);

      //calculate distance that it should slide (depends on angle between plane & ray)
      SlideDistance:=vectorDotProduct(newRay,newDirection);
      //still need to implement friction properly
//      if abs(SlideDistance)<10*deltaTime then SlideDistance:=0;
      ScaleVector(newDirection,SlideDistance);

      //calculate new position sphere is heading towards
      newPosition:=VectorAdd(collisionPosition,newDirection);
      ray:=newDirection;
      vel:=VectorLength(ray);
      
      //display arrows for collision normals & slide direction
      if i=0 then
        DrawArrows(intPoint,intNormal,Ray, GLArrowLine1, GLArrowLine4)
      else if i=1 then
        DrawArrows(intPoint,intNormal,Ray, GLArrowLine2, GLArrowLine5)
      else if i=2 then
        DrawArrows(intPoint,intNormal,Ray, GLArrowLine3, GLArrowLine6)
      else if i=6 then
      begin
//        caption:=FloatToStr(vectordistance(newPosition,oldPosition));
        newPosition:=oldPosition;
        break;
      end;

      //check if very small motion (e.g. when stuck in a corner)
      if vel<1E-10 then//deltaTime then
      begin
        newPosition:=oldPosition;
        break;
      end;

      
      CollisionState:=TCollisionState.Create();
      CollisionState.Position:=oldPosition;
      CollisionState.Contact.intNormal:=intNormal;
      CollisionState.Contact.intPoint:=intPoint;      
      CollisionState.Time:=GetTickCount64();

      CollisionStates.Add(CollisionState);

    end
    else //no collision occured, so quit loop
    begin
      Break;
    end;
  end; //end i loop
  Velocity:=Ray;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
  startPosition,newPosition:TVector;
  CollisionState:TCollisionState;
begin
  GLSceneViewer1.SetFocus;
  GLScene1.BeginUpdate;
  //get starting position
  StartPosition:=glnavigator1.MovingObject.Position.AsVector;
//  GLNavigator1.AutoUpdateObject:=true;
 // GLScene1.BeginUpdate;
  //then update position according to keys being pressed
  if iskeydown('W') then glnavigator1.MoveForward(MovementScale*deltaTime);
  if iskeydown('S') then glnavigator1.MoveForward(-MovementScale*deltaTime);
  if iskeydown('A') then glnavigator1.StrafeHorizontal(-MovementScale*deltaTime);
  if iskeydown('D') then glnavigator1.StrafeHorizontal(MovementScale*deltaTime);

  //move up/down (for debugging)
  if iskeydown(VK_PRIOR)or iskeydown(VK_SPACE) then glnavigator1.StrafeVertical(MovementScale*deltaTime);
  if iskeydown(VK_NEXT) then glnavigator1.StrafeVertical(-MovementScale*deltaTime);
  
  //this is the position we are trying to move to with controls
  newPosition:=glnavigator1.MovingObject.Position.AsVector;

  //Change in velocity = acceleration * time taken
//  Velocity:=VectorAdd(Velocity,VectorScale(Gravity,deltaTime));

  //Change in position = velocity * time taken
//  newPosition:=VectorAdd(newPosition,VectorScale(Velocity,deltaTime));
  newPosition.Y:=  newPosition.Y-MovementScale*0.5*deltaTime;

  CollisionState:=TCollisionState.Create();
  CollisionState.Position:=StartPosition;
  CollisionStates.Add(CollisionState);
   
  //make arrowlines invisible (they are made visible in SphereSweepAndSlide)
  GLArrowLine1.Visible:=False;
  GLArrowLine2.Visible:=False;
  GLArrowLine3.Visible:=False;
  GLArrowLine4.Visible:=False;
  GLArrowLine5.Visible:=False;
  GLArrowLine6.Visible:=False;

  //do some magic!!!  and store new position in newPosition
  SphereSweepAndSlide(Map,StartPosition,Velocity,newPosition,PlayerSphere.Radius);

  //update velocity
 { Velocity:=VectorSubtract(newPosition,StartPosition);
  if DeltaTime<>0 then
    VectorScale(Velocity,1/DeltaTime);
    }
//  caption:=Format('%.4f',[VectorLength(Velocity)]);
  glnavigator1.MovingObject.Position.AsVector:=newPosition;

  //update mouse view
  xangle:=mouse.CursorPos.X-screen.Width/2;
  yangle:=mouse.CursorPos.Y-screen.Height/2;
  setcursorpos(screen.width div 2,screen.Height div 2);
  glnavigator1.TurnHorizontal(xangle*40*deltaTime);
  glnavigator1.TurnVertical(-yangle*20*deltaTime);

  GLScene1.EndUpdate;

  if CollisionStates.Count>0 then
  begin
    CollisionState:=TCollisionState(CollisionStates.First);
    TickCount:=GetTickCount64;
    //remove all old states
    while (CollisionState<>nil)and(CollisionState.Time<TickCount-DisplayTime) do
    begin
      CollisionStates.Remove(CollisionState);
      CollisionState.Free;
      if CollisionStates.Count=0 then Exit;
      CollisionState:=TCollisionState(CollisionStates.First);
    end;
  end;


  //GLPhysicsManager1.CalculateNextState(deltaTime);
 // CollisionManager1.CheckCollisions;
//    GLScene1.EndUpdate;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
//  if Key=Ord('R') then resetScene;
  if Key=VK_ESCAPE then Halt;
  //pause / unpause
  if Key=VK_PAUSE then GLCadencer1.Enabled:=not GLCadencer1.Enabled;
  //first person  
  if Key=VK_F2 then GLSceneViewer1.Camera:=FirstPersonCamera;
  //third person
  if Key=VK_F3 then GLSceneViewer1.Camera:=ThirdPersonCamera;
  // solid / wireframe
  if iskeydown(VK_F5) then
  begin
    WireFrame:=not WireFrame;
    if WireFrame then
    begin
      Map.UseMeshMaterials:=false;
      Map.Material.PolygonMode:=pmLines;
    end
    else
    begin
      Map.UseMeshMaterials:=true;
      Map.Material.PolygonMode:=pmFill;
    end;
  end;
end;


procedure TForm1.GLDirectOpenGL1Render(Sender : TObject; var rci : TGLRenderContextInfo);
var
  x,y,z,t:Single;
  i:integer;
  CollisionState:TCollisionState;
begin
//  caption:= IntToStr(CollisionStates.Count);
  gl.Color3f(1,1,1);
  gl.PushAttrib(GL_LIGHTING_BIT);
  gl.Disable(GL_LIGHTING);
  //draw position trail
  gl.Begin_(GL_LINE_STRIP);
  for i:=0 to CollisionStates.Count-1 do
  begin
    CollisionState:=TCollisionState(CollisionStates.Items[i]);
    x:=CollisionState.Position.X;
    y:=CollisionState.Position.Y;
    z:=CollisionState.Position.Z;
    gl.Vertex3f(x,y,z);
  end;
  gl.End_();
  //draw normals trail
  gl.Begin_(GL_LINES);
  for i:=0 to CollisionStates.Count-1 do
  begin
    CollisionState:=TCollisionState(CollisionStates.Items[i]);
    t:=(DisplayTime-(TickCount-CollisionState.Time))/DisplayTime;
    gl.Color3f(t,t,t);
      gl.vertex3f(CollisionState.Contact.intPoint.X,CollisionState.Contact.intPoint.Y,CollisionState.Contact.intPoint.Y);
      gl.vertex3f(CollisionState.Contact.intPoint.X+CollisionState.Contact.intNormal.X,//GLSphere4.Radius,
      CollisionState.Contact.intPoint.X+CollisionState.Contact.intNormal.Y,//GLSphere4.Radius,
      CollisionState.Contact.intPoint.Z+CollisionState.Contact.intNormal.Z);//GLSphere4.Radius);
  end;
  gl.End_();
  gl.PopAttrib;
end;

end.




