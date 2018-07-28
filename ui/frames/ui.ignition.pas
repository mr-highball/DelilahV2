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

  TRequestEvent = procedure (Sender:TObject;Var Continue:Boolean) of object;

  { TIgnition }

  TIgnition = class(TFrame)
    btn_stop: TButton;
    btn_start: TButton;
    lbl_status: TLabel;
  private
    FOnRequestStart: TRequestEvent;
    FOnRequestStop: TRequestEvent;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FState: TIgnitionState;
    function GetStatus: String;
    procedure SetStatus(AValue: String);
    procedure StartClick(Sender:TObject);
    procedure StopClick(Sender:TObject);
    procedure DefaultStart(Sender:TObject;Var Continue:Boolean);
    procedure DefaultStop(Sender:TObject;Var Continue:Boolean);
  protected
    procedure Loaded; override;
  public
    property OnRequestStart : TRequestEvent read FOnRequestStart
      write FOnRequestStart;
    property OnStart : TNotifyEvent read FOnStart write FOnStart;
    property OnRequestStop : TRequestEvent read FOnRequestStop
      write FOnRequestStop;
    property OnStop : TNotifyEvent read FOnStop write FOnStop;
    property State : TIgnitionState read FState;
    property Status : String read GetStatus write SetStatus;
  end;

implementation
uses
  Dialogs;

{$R *.lfm}

{ TIgnition }

procedure TIgnition.StartClick(Sender: TObject);
var
  LContinue:Boolean;
begin
  if FState in [isStopped] then
  begin
    //this event controls what happens when start is requested
    if Assigned(FOnRequestStart) then
    begin
      LContinue:=True;
      //update state to starting
      FState:=isStarting;
      try
        //attempt to start
        FOnRequestStart(Self,LContinue);
        if not LContinue then
        begin
          FState:=isStopped;
          Exit;
        end;
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

function TIgnition.GetStatus: String;
begin
  Result:=lbl_status.Caption;
end;

procedure TIgnition.SetStatus(AValue: String);
begin
  if not lbl_status.Visible then
    lbl_status.Visible:=True;
  lbl_status.Caption:=AValue;
end;

procedure TIgnition.StopClick(Sender: TObject);
var
  LContinue:Boolean;
begin
  //make sure we are started
  if FState in [isStarted] then
  begin
    try
      LContinue:=True;
      //update state to stopping
      FState:=isStopping;
      //attempt to stop if assigned
      if Assigned(FOnRequestStop) then
        FOnRequestStop(Self,LContinue);
      if not LContinue then
      begin
        FState:=isStarted;
        Exit;
      end;
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

procedure TIgnition.DefaultStart(Sender: TObject; var Continue: Boolean);
begin
  lbl_status.Visible:=True;
  lbl_status.Caption:='-Started-';
  ShowMessage('Default OnRequestStart Event is being used.');
end;

procedure TIgnition.DefaultStop(Sender: TObject; var Continue: Boolean);
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

