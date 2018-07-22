unit ui.ignition;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls;

type

  TIgnitionState = (
    isStopped,
    isStarting,
    isStarted,
    isStopping
  );

  { TIgnition }

  TIgnition = class(TFrame)
    btn_stop: TButton;
    btn_start: TButton;
    lbl_status: TLabel;
  private
    FOnRequestStart: TNotifyEvent;
    FOnRequestStop: TNotifyEvent;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FState: TIgnitionState;
    procedure StartClick(Sender:TObject);
    procedure StopClick(Sender:TObject);
    procedure DefaultStart(Sender:TObject);
    procedure DefaultStop(Sender:TObject);
  protected
    procedure Loaded; override;
  public
    property OnRequestStart : TNotifyEvent read FOnRequestStart
      write FOnRequestStart;
    property OnStart : TNotifyEvent read FOnStart write FOnStart;
    property OnRequestStop : TNotifyEvent read FOnRequestStop
      write FOnRequestStop;
    property OnStop : TNotifyEvent read FOnStop write FOnStop;
    property State : TIgnitionState read FState;
  end;

implementation
uses
  Dialogs;

{$R *.lfm}

{ TIgnition }

procedure TIgnition.StartClick(Sender: TObject);
begin
  if FState in [isStopped] then
  begin
    //this event controls what happens when start is requested
    if Assigned(FOnRequestStart) then
    begin
      //update state to starting
      FState:=isStarting;
      try
        //attempt to start
        FOnRequestStart(Self);
        //no exception raised, so start assumed
        FState:=isStarted;
        //if there is an onstart event, trigger it
        if Assigned(FOnStart) then
          FOnStart(Self);
      except on E:Exception do
      begin
        //reset the state and throw the event
        if FState<>isStarted then
          FState:=isStopped;
        raise E;
      end
      end;
    end;
  end;
end;

procedure TIgnition.StopClick(Sender: TObject);
begin
  //make sure we are started
  if FState in [isStarted] then
  begin
    try
      //update state to stopping
      FState:=isStopping;
      //attempt to stop if assigned
      if Assigned(FOnRequestStop) then
        FOnRequestStop(Self);
      FState:=isStopped;
      //notify listeners that we have stopped
      if Assigned(FOnStop) then
        FOnStop(Self);
    except on E:Exception do
    begin
      if FState<>isStopped then
        FState:=isStarted;
      raise E;
    end
    end;
  end;
end;

procedure TIgnition.DefaultStart(Sender: TObject);
begin
  lbl_status.Visible:=True;
  lbl_status.Caption:='-Started-';
  ShowMessage('Default OnRequestStart Event is being used.');
end;

procedure TIgnition.DefaultStop(Sender: TObject);
begin
  lbl_status.Visible:=True;
  lbl_status.Caption:='-Stopped-';
  ShowMessage('Default OnRequestStop Event is being used.');
end;

procedure TIgnition.Loaded;
begin
  inherited Loaded;
  FState:=isStopped;
  lbl_status.Visible:=False;
  btn_start.OnClick:=StartClick;
  btn_stop.OnClick:=StopClick;
  FOnRequestStart:=DefaultStart;
  FOnRequestStop:=DefaultStop;
end;

end.

