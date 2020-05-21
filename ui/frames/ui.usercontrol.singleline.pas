unit ui.usercontrol.singleline;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, 
  ui.usercontrol;

type

  { TSingleLine }

  TSingleLine = class(TUserControl)
  private
    function GetText: String;
    procedure SetText(AValue: String);
  protected
    procedure InitControls;
  public
    property Text : String read GetText write SetText;
    constructor Create(TheOwner: TComponent); override;
  end;

implementation
uses
  StdCtrls;

{$R *.lfm}

{ TSingleLine }

function TSingleLine.GetText: String;
begin
  Result:=TEdit(Control).Text;
end;

procedure TSingleLine.SetText(AValue: String);
begin
  TEdit(Control).Text := AValue;
end;

procedure TSingleLine.InitControls;
begin
  inherited;
  Text := '';
end;

constructor TSingleLine.Create(TheOwner: TComponent);
begin
  Control:=TEdit.Create(nil);
  inherited Create(TheOwner);
end;

end.

