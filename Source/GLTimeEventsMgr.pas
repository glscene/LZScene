//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Time based events mannager using the Cadencer
   can be useful to make animations with GlScene

	 History :  
       07/01/10 - DaStr - Added TGLTimeEventsMGR.Reset()
                             Fixed code formating
       25/11/09 - DanB - Changed TTimeEvent.Name from ShortString to String
       11/10/07 - DaStr - TTimeEvent.SetEnabled now updates StartTime to
                             Cadencers's current time.
                             (Thanks Lukasz Sokol) (BugTracker ID = 1811141)
       28/03/07 - DaStr - Cosmetic fix for FPC compatibility
       29/01/07 - DaStr - Moved registration to GLSceneRegister.pas
       07/02/02 - EG - Added Notification, DoEvent, ElapsedTime and changed Event type
}

unit GLTimeEventsMgr;

interface

uses
  Classes, SysUtils,
  GLCadencer,  GLBaseClasses;

type

    TTimeEvent = class;
    TTimeEvents = class;

	// TGLTimeEventsMGR
	//
   TGLTimeEventsMGR = class(TGLUpdateAbleComponent)
   private
       
      FCadencer : TGLCadencer;
      FEnabled : boolean;
      FFreeEventOnEnd : boolean;
      FEvents : TTimeEvents;

   protected
      { Déclarations protégées }
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;

      procedure SetCadencer(const val : TGLCadencer);
      procedure SetEvents(const val : TTimeEvents);

   public
       
      constructor Create(aOwner : TComponent); override;
      destructor Destroy; override;

      procedure DoProgress(const progressTime : TProgressTimes); override;
      procedure Reset();

   published
      { Déclarations publiées }
      property Cadencer : TGLCadencer read FCadencer write SetCadencer;
      property Enabled : boolean read FEnabled write FEnabled default True;
      property FreeEventOnEnd : boolean read FFreeEventOnEnd write FFreeEventOnEnd default False;
      property Events : TTimeEvents read FEvents write SetEvents;
   end;

	// TTimeEvents
	//
	TTimeEvents = class (TCollection)
   protected
       
      Owner : TComponent;
      function GetOwner: TPersistent; override;
      procedure SetItems(index : Integer; const val : TTimeEvent);
      function GetItems(index : Integer) : TTimeEvent;

   public
       
      constructor Create(AOwner : TComponent);

      function Add: TTimeEvent;
      function FindItemID(ID: Integer): TTimeEvent;
      function EventByName(name:String): TTimeEvent;

      property Items[index : Integer] : TTimeEvent read GetItems write SetItems; default;
   end;

   TTimeEventType = (etOneShot, etContinuous, etPeriodic);
   TTimeEventProc = procedure (event : TTimeEvent) of object;

   // TTimeEvent
   //
   TTimeEvent = class (TCollectionItem)
      private
          
         FName: String;
         FStartTime, FEndTime, FElapsedTime : Double;
         FPeriod : Double;
         FEventType: TTimeEventType;
         FOnEvent:TTimeEventProc;
         FEnabled: boolean;

         FTickCount : Cardinal;
         procedure SetEnabled(const Value: Boolean);

      protected
          
         function GetDisplayName : String; override;
         procedure SetName(val : String);

         procedure DoEvent(const curTime : Double);

      public
          
         constructor Create(Collection : TCollection); override;
         destructor Destroy; override;

         // Number of times the event was triggered since activation
         property TickCount : Cardinal read FTickCount;
         // Elapsed time since the event was activated
         property ElapsedTime : Double read FElapsedTime; 

      published
          
         property Name : String read FName write SetName;
         property StartTime : Double read FStartTime write FStartTime;
         property EndTime : Double read FEndTime write FEndTime;
         property Period : Double read  FPeriod write FPeriod;
         property EventType : TTimeEventType read FEventType write FEventType default etOneShot;
         property OnEvent : TTimeEventProc read FOnEvent write FOnEvent;
         property Enabled : Boolean read FEnabled write SetEnabled  default True;

    end;

implementation

// ------------------
// ------------------ TGLTimeEventsMGR ------------------
// ------------------

// Create
//
constructor TGLTimeEventsMGR.Create(aOwner : TComponent);
begin
    inherited;
    FEnabled:=True;
    FFreeEventOnEnd:=False;
    FEvents:=TTimeEvents.Create(self);
end;

// Destroy
//
destructor TGLTimeEventsMGR.Destroy;
begin
    Cadencer:=nil;
    FEvents.Free;
    inherited Destroy;
end;

// Notification
//
procedure TGLTimeEventsMGR.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation=opRemove) and (AComponent=Cadencer) then
      FCadencer:=nil;
   inherited;
end;

// SetCadencer
//
procedure TGLTimeEventsMGR.SetCadencer(const val : TGLCadencer);
begin
   if FCadencer<>val then begin
      if Assigned(FCadencer) then
         FCadencer.UnSubscribe(Self);
      FCadencer:=val;
      if Assigned(FCadencer) then
         FCadencer.Subscribe(Self);
   end;
end;


// SetEvents
//
procedure TGLTimeEventsMGR.SetEvents(const val : TTimeEvents);
begin
   FEvents.Assign(val);
end;

// DoProgress
//
procedure TGLTimeEventsMGR.DoProgress(const progressTime : TProgressTimes);
var
   i : Integer;
begin
   if not Enabled then Exit;

   i:=0;
   with progressTime do while i<=Events.Count-1 do with Events.Items[i] do begin
      if Enabled and Assigned(FOnEvent) then begin
         case EventType of
            etOneShot :
               if (newTime>=StartTime) and (TickCount=0) then
                  DoEvent(newTime);
            etContinuous :
               if (newTime>=StartTime) and ((newTime<=EndTime) or (EndTime<=0)) then
                  DoEvent(newTime);
            etPeriodic :
               if (newTime>=StartTime+TickCount*Period) and ((newTime<=EndTime) or (EndTime<=0)) then
                  DoEvent(newTime);
         else
            Assert(False);
         end;
      end;
      if FreeEventOnEnd and
           ( ((EventType<>etOneShot) and (newTime>EndTime) and (EndTime>=0)) or
             ((EventType=etOneShot) and (TickCount>0)) ) then
         Events[i].Free
      else begin
         // if we delete current event, the next will have same index
         // so increment only if we don't delete
         Inc(i);
      end;
   end;
end;

// Reset
//
procedure TGLTimeEventsMGR.Reset;
var
  I: Integer;
begin
  if FEvents.Count <> 0 then
    for I := 0 to FEvents.Count - 1 do
      FEvents[I].FTickCount := 0;
end;


// ------------------
// ------------------ TTimeEvents ------------------
// ------------------

// Create
//
constructor TTimeEvents.Create(AOwner : TComponent);
begin
	Owner:=AOwner;
	inherited Create(TTimeEvent);
end;

// GetOwner
//
function TTimeEvents.GetOwner: TPersistent;
begin
	Result:=Owner;
end;

// Setitems
//
procedure TTimeEvents.SetItems(index : Integer; const val : TTimeEvent);
begin
	inherited Items[index]:=val;
end;

// GetItems
//
function TTimeEvents.GetItems(index : Integer) : TTimeEvent;
begin
	Result:=TTimeEvent(inherited Items[index]);
end;

// Add
//
function TTimeEvents.Add : TTimeEvent;
begin
	Result:=(inherited Add) as TTimeEvent;
end;

// FindItemID
//
function TTimeEvents.FindItemID(ID: Integer): TTimeEvent;
begin
	Result:=(inherited FindItemID(ID)) as TTimeEvent;
end;

// EventByName
//
function TTimeEvents.EventByName(name:String): TTimeEvent;
var i:integer;
begin
    i:=0;
    while (i<Count) and (Items[i].FName<>name) do inc(i);

    if i=Count then result:=nil else result:=Items[i];
end;




// ------------------
// ------------------ TTimeEvent ------------------
// ------------------

// Create
//
constructor TTimeEvent.Create(Collection : TCollection);
begin
   inherited Create(Collection);
   FEventType:=etOneShot;
   FName:=Format('Event%d', [index]); // give a default name different for each event
   FEnabled:=True;
end;

// Destroy
//
destructor TTimeEvent.Destroy;
begin
    inherited Destroy;
end;

// GetDisplayName
//
function TTimeEvent.GetDisplayName : String;
begin
    case EventType of
        etOneShot:
            Result:=Name+Format(' (OneShot ST=%g)',[StartTime]);
        etContinuous:
            Result:=Name+Format(' (Continuous ST=%g ET=%g)',[StartTime,EndTime]);
        etPeriodic:
            Result:=Name+Format(' (Periodic ST=%g ET=%g P=%g)',[StartTime,EndTime,Period]);
    end;
end;

// SetName
//
procedure TTimeEvent.SetName(val : String);
var
   i : Integer;
   ok : Boolean;
begin
   ok := True;
   with self.Collection as TTimeEvents do // we mustn't have 2 events with the same name (for EventByName)
       for i:=0 to Count-1 do
           if Items[i].FName = val then Ok := False;

   if Ok and (val<>'') then FName:=val;
end;

// DoEvent
//
procedure TTimeEvent.DoEvent(const curTime : Double);
begin
   if Assigned(FOnEvent) then begin
      FElapsedTime:=curTime-StartTime;
      FOnEvent(Self);
   end;
   Inc(FTickCount);
end;

// SetEnabled
//
procedure TTimeEvent.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  FStartTime := ((GetOwner as TTimeEvents).Owner as TGLTimeEventsMGR).Cadencer.CurrentTime;
end;

end.
