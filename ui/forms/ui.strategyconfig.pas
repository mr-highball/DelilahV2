unit ui.strategyconfig;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs, ExtCtrls,
  ui.strategy;

type

  { TStrategyHolder }

  TStrategyHolder = class(TForm)
    pnl_config: TPanel;
  private
    FConfig : TConfigureStrategy;

    function GetConfig: TConfigureStrategy;
    procedure SetConfig(AValue: TConfigureStrategy);
  public
    (*
      config used for... configging. also this will be owned by this form
      also will use the onfinish event
    *)
    property Config : TConfigureStrategy read GetConfig write SetConfig;

    destructor Destroy; override;
  end;

var
  StrategyHolder: TStrategyHolder;

implementation

{$R *.lfm}

{ TStrategyHolder }

function TStrategyHolder.GetConfig: TConfigureStrategy;
begin
  Result := FConfig;
end;

procedure TStrategyHolder.SetConfig(AValue: TConfigureStrategy);
begin
  if Assigned(FConfig) then
  begin
    FConfig.Parent := nil;
    FConfig.Free;
    FConfig := nil
  end;

  FConfig := AValue;
  FConfig.Align := alClient;
  FConfig.Parent := pnl_config;
end;

destructor TStrategyHolder.Destroy;
begin
  if Assigned(FConfig) then
    FConfig.Free;

  inherited Destroy;
end;

end.

