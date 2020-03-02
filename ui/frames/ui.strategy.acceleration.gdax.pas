unit ui.strategy.acceleration.gdax;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, 
  ui.strategy.acceleration;

type

  { TConfigureAccelerationGDAX }

  TConfigureAccelerationGDAX = class(TConfigureAcceleration)
  private

  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation
uses
  delilah.strategy.acceleration.gdax;

{$R *.lfm}

{ TConfigureAccelerationGDAX }

constructor TConfigureAccelerationGDAX.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Strategy := TGDAXAccelerationStrategyImpl.Create;
end;

end.

