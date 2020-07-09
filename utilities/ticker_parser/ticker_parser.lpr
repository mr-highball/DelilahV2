program ticker_parser;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, utilities.tickerparser.main, delilah.manager.gdax, delilah.manager,
  delilah.order.gdax, delilah.order, delilah,
  delilah.strategy.acceleration.gdax, delilah.strategy.acceleration,
  delilah.strategy.gdax.sample, delilah.strategy.gdax.tiers, delilah.strategy,
  delilah.strategy.utils, delilah.strategy.window, delilah.ticker.gdax,
  delilah.ticker, delilah.types
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TTickerParser, TickerParser);
  Application.Run;
end.

