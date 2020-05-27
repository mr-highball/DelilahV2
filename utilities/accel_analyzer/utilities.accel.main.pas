unit utilities.accel.main;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs, ComCtrls, ExtCtrls, TAGraph,
  delilah.types,
  delilah.strategy.acceleration,
  utilities.mock.manager, utilities.filebrowser;

type

  { TAccelAnalyzer }

  TAccelAnalyzer = class(TForm)
    chart: TChart;
    directory: TDirectoryBrowser;
    page_ctrl_main: TPageControl;
    scroll_analyze: TScrollBox;
    scroll_accel: TScrollBox;
    split_analyze: TSplitter;
    ts_analyze: TTabSheet;
    ts_load: TTabSheet;
    ts_accel: TTabSheet;
  private

  public

  end;

var
  AccelAnalyzer: TAccelAnalyzer;

implementation

{$R *.lfm}

end.

