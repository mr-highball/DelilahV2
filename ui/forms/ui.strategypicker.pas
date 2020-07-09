unit ui.strategypicker;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ui.strategyconfig, ui.strategy;

type

  { TStrategyPicker }
  (*
    very simple picker for all available strategies using configure meta-class
    associated with a radio button
  *)
  TStrategyPicker = class(TForm)
    btn_ok: TButton;
    pnl_ctrls: TPanel;
    radio_options: TRadioGroup;
  private
    FConfig : TConfigureStrategyClass;
    function GetConfig: TConfigureStrategyClass;

  public
    property Config : TConfigureStrategyClass read GetConfig;
  end;

var
  StrategyPicker: TStrategyPicker;

implementation
uses
  ui.strategy.tiers,
  ui.strategy.acceleration.gdax,
  ui.strategy.bobber;

{$R *.lfm}

{ TStrategyPicker }

function TStrategyPicker.GetConfig: TConfigureStrategyClass;
begin
  if radio_options.ItemIndex <= 0 then
    Result := TConfigureTiers
  else if radio_options.ItemIndex = 1 then
    Result := TConfigureAccelerationGDAX
  else
    Result := TConfigureBobber;
end;

end.

