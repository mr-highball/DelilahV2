unit ui.authenticator;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type

  { TAuthenticator }

  TAuthenticator = class(TFrame)
    chk_local_time: TCheckBox;
    chk_mode: TCheckBox;
    edit_key: TEdit;
    edit_secret: TEdit;
    edit_pass: TEdit;
    lbl_key: TLabel;
    lbl_secret: TLabel;
    lbl_pass: TLabel;
    pnl_switches: TPanel;
    pnl_edits: TPanel;
  private
    function GetIsSandbox: Boolean;
    function GetKey: String;
    function GetPass: String;
    function GetSecret: String;
    function GetUseLocalTime: Boolean;
    procedure SetIsSandbox(AValue: Boolean);
    procedure SetKey(AValue: String);
    procedure SetPass(AValue: String);
    procedure SetSecret(AValue: String);
    procedure SetUseLocalTime(AValue: Boolean);

  public
    property Key : String read GetKey write SetKey;
    property Passphrase : String read GetPass write SetPass;
    property Secret : String read GetSecret write SetSecret;
    property IsSanboxMode : Boolean read GetIsSandbox write SetIsSandbox;
    property UseLocalTime : Boolean read GetUseLocalTime write SetUseLocalTime;
  end;

implementation

{$R *.lfm}

{ TAuthenticator }

function TAuthenticator.GetIsSandbox: Boolean;
begin
  Result:=chk_mode.Checked;
end;

function TAuthenticator.GetKey: String;
begin
  Result:=edit_key.Text;
end;

function TAuthenticator.GetPass: String;
begin
  Result:=edit_pass.Text;
end;

function TAuthenticator.GetSecret: String;
begin
  Result:=edit_secret.Text;
end;

function TAuthenticator.GetUseLocalTime: Boolean;
begin
  Result:=chk_local_time.Checked;
end;

procedure TAuthenticator.SetIsSandbox(AValue: Boolean);
begin
  chk_mode.Checked:=AValue;
end;

procedure TAuthenticator.SetKey(AValue: String);
begin
  edit_key.Text:=AValue;
end;

procedure TAuthenticator.SetPass(AValue: String);
begin
  edit_pass.Text:=AValue;
end;

procedure TAuthenticator.SetSecret(AValue: String);
begin
  edit_secret.Text:=AValue;
end;

procedure TAuthenticator.SetUseLocalTime(AValue: Boolean);
begin
  chk_local_time.Checked:=AValue;
end;

end.

