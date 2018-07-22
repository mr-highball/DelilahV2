unit ui.main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  JSONPropStorage, ExtCtrls, ComCtrls, ui.ignition, ui.authenticator,
  ui.usercontrol.multiline, ui.usercontrol.boolean, ui.usercontrol.singleline;

type

  { TMain }

  TMain = class(TForm)
    auth: TAuthenticator;
    ignition_main: TIgnition;
    json_main: TJSONPropStorage;
    multi_log: TMultiLine;
    pctrl_main: TPageControl;
    scroll_strategy: TScrollBox;
    ts_log: TTabSheet;
    ts_strategy: TTabSheet;
    ts_auth: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure json_mainRestoringProperties(Sender: TObject);
    procedure json_mainSavingProperties(Sender: TObject);
  private
    procedure InitControls;
  public

  end;

var
  Main: TMain;

implementation

{$R *.lfm}

{ TMain }

procedure TMain.json_mainRestoringProperties(Sender: TObject);
begin
  //todo
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  InitControls;
end;


procedure TMain.json_mainSavingProperties(Sender: TObject);
begin
  //todo
end;

procedure TMain.InitControls;
begin
  //logger
  multi_log.Title:='Strategy Logger';
  multi_log.Author:='Mr. Highball';
  multi_log.Description:='logging for the strategy';
end;


end.

