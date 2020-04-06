unit ui.gunslinger.gdax.order;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls;

type

  { TGDAXSlingerOrder }

  TGDAXSlingerOrder = class(TFrame)
    pnl_type: TPanel;
    pnl_quick_amt: TPanel;
    pnl_amt: TPanel;
    radio_type_limit: TRadioButton;
    radio_type_market: TRadioButton;
    radio_grp_type: TRadioGroup;
  private

  public

  end;

implementation

{$R *.lfm}

end.

