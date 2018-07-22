unit ui.main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  JSONPropStorage, ui.ignition, ui.authenticator, ui.usercontrol.multiline,
  ui.usercontrol.boolean, ui.usercontrol.singleline;

type

  { TMain }

  TMain = class(TForm)
    json_main: TJSONPropStorage;
    UserControl1_1: TUserControl1;
    procedure json_mainRestoringProperties(Sender: TObject);
    procedure json_mainSavingProperties(Sender: TObject);
  private

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


procedure TMain.json_mainSavingProperties(Sender: TObject);
begin
  //todo
end;


end.

