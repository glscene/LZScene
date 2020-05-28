//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Implements a HDS Filter that generates HeightData tiles in a seperate thread.

   This component is a TGLHeightDataSourceFilter, which uses a TGLHeightDataSourceThread,
   to asyncronously search the HeightData cache for any queued tiles.
   When found, it then prepares the queued tile in its own TGLHeightDataThread.

   This allows the GUI to remain responsive, and prevents freezes when new tiles are
   being prepared.  Although this keeps the framerate up, it may cause holes in the
   terrain to show, if the HeightDataThreads cant keep up with the TerrainRenderer's
   requests for new tiles.
   

	 History :  
       22/04/10 - Yar - Fixes after GLState revision
       11/10/07 - DaStr - Added $I GLScene.inc, removed unused dependancy
       25/03/07 - DaStr - Replaced Dialogs with GLCrossPlatform for Delphi5 compatibility
       22/03/07 - LIN - Added UseDirtyTiles property - Specifies how dirty tiles are replaced.
       22/03/07 - LIN - Data is now prepared in 3 stages:
                            BeforePreparingData : (Main Thread)
                            PreparingData       : (Sub-Thread)   (Main Thread if MaxThreads=0)
                            AfterPreparingData  : (Main Thread)
       05/03/07 - LIN - Added ThreadCount and WaitFor
       12/02/07 - LIN - Creation
	 
}

unit GLAsyncHDS;

interface

{$I GLScene.inc}

uses Classes, GLHeightData, GLCrossPlatform;

type
  TGLAsyncHDS = class;
  TIdleEvent = procedure(Sender:TGLAsyncHDS;TilesUpdated:boolean) of object;
  TNewTilePreparedEvent = procedure (Sender : TGLAsyncHDS; heightData : TGLHeightData) of object; //a tile was updated (called INSIDE the sub-thread?)

  // TUseDirtyTiles
  //
  {  TUseDirtyTiles determines if/how dirty tiles are displayed and when they are released.
      
      TUseDirtyTiles
      
       When a tile is maked as dirty, a replacement is queued immediately.
       However, the replacement cant be used until the HDThread has finished preparing it.
       Dirty tiles can be deleted as soon as they are no longer used/displayed.

     Possible states for a TUseDirtyTiles.
       hdsNever :            Dirty tiles get released immediately, leaving a hole in the terrain, until the replacement is hdsReady.
       hdsUntilReplaced :    Dirty tiles are used, until the HDThread has finished preparing the queued replacement.
       hdsUntilAllReplaced : Waits until the HDSThread has finished preparing ALL queued tiles,
                             before allowing the renderer to switch over to the new set of tiles.
                             (This prevents a fading checkerbox effect.)
        }
  TUseDirtyTiles=(dtNever,dtUntilReplaced,dtUntilAllReplaced);


	 TGLAsyncHDS = class (TGLHeightDataSourceFilter)
	   private
	       
       FOnIdleEvent :TIdleEvent;
       FOnNewTilePrepared : TNewTilePreparedEvent;
       FUseDirtyTiles:TUseDirtyTiles;
       FTilesUpdated:boolean;
	   protected
	       
    public
      //TilesUpdated:boolean;
	       
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure BeforePreparingData(heightData : TGLHeightData); override;
      procedure StartPreparingData(heightData : TGLHeightData); override;
      procedure ThreadIsIdle; override;
      procedure NewTilePrepared(heightData:TGLHeightData);
      function  ThreadCount:integer;
      procedure WaitFor(TimeOut:integer=2000);
      //procedure NotifyChange(Sender : TObject); override;
      function  TilesUpdated:boolean;        //Returns true if tiles have been updated since the flag was last reset
      procedure TilesUpdatedFlagReset;       //sets the TilesUpdatedFlag to false; (is ThreadSafe)
	   published
	       
      property OnIdle : TIdleEvent read FOnIdleEvent write FOnIdleEvent;
      property OnNewTilePrepared : TNewTilePreparedEvent read FOnNewTilePrepared write FOnNewTilePrepared;
      property UseDirtyTiles :TUseDirtyTiles read FUseDirtyTiles write FUseDirtyTiles;
      property MaxThreads;         //sets the maximum number of simultaineous threads that will prepare tiles.(>1 is rarely needed)
      property Active;             //set to false, to ignore new queued tiles.(Partially processed tiles will still be completed)
  end;

  TGLAsyncHDThread = class(TGLHeightDataThread)
    public
      Owner : TGLAsyncHDS;
      HDS   : TGLHeightDataSource;
      Procedure Execute; override;
      Procedure Sync;
  end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils;

// ------------------
// ------------------ TGLAsyncHDS ------------------
// ------------------

// Create
//
constructor TGLAsyncHDS.Create(AOwner: TComponent);
begin
	 inherited Create(AOwner);
  MaxThreads:=1;
  FUseDirtyTiles:=dtNever;
  FTilesUpdated:=true;
end;

// Destroy
//
destructor TGLAsyncHDS.Destroy;
begin
	inherited Destroy;
end;

// BeforePreparingData
//
procedure TGLAsyncHDS.BeforePreparingData(heightData : TGLHeightData);
begin
  if FUseDirtyTiles=dtNever then begin
    if heightData.OldVersion<>nil then begin
      heightData.OldVersion.DontUse:=true;
      heightData.DontUse:=false;
    end;
  end;
  if assigned(HeightDataSource) then HeightDataSource.BeforePreparingData(heightData);
end;

// StartPreparingData
//
procedure TGLAsyncHDS.StartPreparingData(heightData : TGLHeightData);
var HDThread : TGLAsyncHDThread;
    HDS:TGLHeightDataSource;
begin
  HDS:=HeightDataSource;
  //---if there is no linked HDS then return an empty tile--
  if not Assigned(HDS) then begin
    heightData.DataState:=hdsNone;
    exit;
  end;
  if (Active=false) then exit;

  //---If not using threads then prepare the HD tile directly---  (everything else freezes until done)
  if MaxThreads=0 then begin
    HDS.StartPreparingData(HeightData);
    if heightData.DataState=hdsPreparing
      then heightData.DataState:=hdsReady
      else heightData.DataState:=hdsNone;
  end else begin //--MaxThreads>0 : start the thread and go back to start the next one--
    heightData.DataState:=hdsPreparing; //prevent other threads from preparing this HD.
    HDThread:=TGLAsyncHDThread.Create(true);
    HDThread.Owner:=self;
    HDThread.HDS:=self.HeightDataSource;
    HDThread.HeightData:=HeightData;
    heightData.Thread:=HDThread;
    HDThread.FreeOnTerminate:=false;
{$IFDEF GLS_DELPHI_2009_DOWN}
    HDThread.Resume;
{$ELSE}
    HDThread.Start;
{$ENDIF}
  end;
end;


//OnIdle event
//
procedure TGLAsyncHDS.ThreadIsIdle;
var i:integer;
    lst:TList;
    HD:TGLHeightData;
begin
  //----------- dtUntilAllReplaced -------------
  //Switch to the new version of ALL dirty tiles
    lst:=self.Data.LockList;
    try
      if FUseDirtyTiles=dtUntilAllReplaced then begin
        i:=lst.Count;
        while(i>0) do begin
          dec(i);
          HD:=TGLHeightData(lst.Items[i]);
          if (HD.DataState in [hdsReady,hdsNone])
            and(Hd.DontUse)and(HD.OldVersion<>nil) then begin
            HD.DontUse:=false;
            HD.OldVersion.DontUse:=true;
            FTilesUpdated:=true;
          end;
        end;
      end;//Until All Replaced
      if Assigned(FOnIdleEvent) then FOnIdleEvent(Self,FTilesUpdated);
    finally
      self.Data.UnlockList;
    end;
  //--------------------------------------------
end;

//OnNewTilePrepared event
//
procedure TGLAsyncHDS.NewTilePrepared(heightData:TGLHeightData);
var HD:TGLHeightData;
begin
  if assigned(HeightDataSource) then HeightDataSource.AfterPreparingData(HeightData);
  with self.Data.LockList do begin
    try
      HD:=heightdata;
      //--------------- dtUntilReplaced -------------
      //Tell terrain renderer to display the new tile
      if (FUseDirtyTiles=dtUntilReplaced)and(HD.DontUse)and(HD.OldVersion<>nil) then begin
        HD.DontUse:=false;            //No longer ignore the new tile
        HD.OldVersion.DontUse:=true;  //Start ignoring the old tile
      end;
      //---------------------------------------------
      if HD.DontUse=false then FTilesUpdated:=true;
      if Assigned(FOnNewTilePrepared) then FOnNewTilePrepared(Self,HeightData);           //OnNewTilePrepared Event
    finally
      self.Data.UnlockList;
    end;
  end;
end;

//ThreadCount
//  Count the active threads
//
function TGLAsyncHDS.ThreadCount:integer;
var lst: Tlist;
    i,TdCtr:integer;
    HD:TGLHeightData;
begin
  lst:=self.Data.LockList;
  i:=0;TdCtr:=0;
  while(i<lst.Count)and(TdCtr<self.MaxThreads) do begin
    HD:=TGLHeightData(lst.Items[i]);
    if HD.Thread<>nil then Inc(TdCtr);
    inc(i);
  end;
  self.Data.UnlockList;
  result:=TdCtr;
end;

//WaitFor
//  Wait for all running threads to finish.
//  Should only be called after setting Active to false,
//  to prevent new threads from starting.
procedure TGLAsyncHDS.WaitFor(TimeOut:Integer=2000);
var OutTime:TDateTime;
begin
  Assert(self.active=false);
  OutTime:=now+TimeOut;
  While ((now<OutTime)and(ThreadCount>0)) do begin
    sleep(0);
  end;
  Assert(ThreadCount=0);
end;

{
procedure TGLAsyncHDS.NotifyChange(Sender : TObject);
begin
  TilesChanged:=true;
end;
}

// This function prevents the user from trying to write directly to this variable.
// FTilesUpdated if NOT threadsafe and should only be reset with TilesUpdatedFlagReset.
function TGLAsyncHDS.TilesUpdated:boolean;
begin
  result:=FTilesUpdated;
end;

// Set the TilesUpdatedFlag to false. (is Threadsafe)
procedure TGLAsyncHDS.TilesUpdatedFlagReset;
begin
  if not assigned(self) then exit; //prevents AV on Application termination.
  with Data.LockList do try
    FTilesUpdated:=False;
  finally Data.UnlockList; end;
end;

//-------------------HD Thread----------------
Procedure TGLAsyncHDThread.Execute;
Begin
  HDS.StartPreparingData(HeightData);
  HeightData.Thread:=nil;
  Synchronize(sync);
end;

Procedure TGLAsyncHDThread.Sync;
begin
  Owner.NewTilePrepared(heightData);
  if heightData.DataState=hdsPreparing then heightData.DataState:=hdsReady;
end;

//--------------------------------------------

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClass(TGLAsyncHDS);

end.
