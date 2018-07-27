unit ui.authenticator;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  gdax.api.types;

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
    FAuth : IGDAXAuthenticator;
    function GetAuth: IGDAXAuthenticator;
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
    property Authenticator : IGDAXAuthenticator read GetAuth;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation
uses
  gdax.api.authenticator, gdax.api.consts;
{$R *.lfm}

{ TAuthenticator }

function TAuthenticator.GetIsSandbox: Boolean;
begin
  Result:=chk_mode.Checked;
end;

function TAuthenticator.GetAuth: IGDAXAuthenticator;
begin
  Result:=FAuth;
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
  if AValue then
    FAuth.Mode:=gdSand
  else
    FAuth.Mode:=gdProd;
end;

procedure TAuthenticator.SetKey(AValue: String);
begin
  edit_key.Text:=AValue;
  FAuth.Key:=AValue;
end;

procedure TAuthenticator.SetPass(AValue: String);
begin
  edit_pass.Text:=AValue;
  FAuth.Passphrase:=AValue;
end;

procedure TAuthenticator.SetSecret(AValue: String);
begin
  edit_secret.Text:=AValue;
  FAuth.Secret:=AValue;
end;

procedure TAuthenticator.SetUseLocalTime(AValue: Boolean);
begin
  chk_local_time.Checked:=AValue;
  FAuth.UseLocalTime:=AValue;
end;

constructor TAuthenticator.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FAuth:=TGDAXAuthenticatorImpl.Create;
end;

destructor TAuthenticator.Destroy;
begin
  FAuth:=nil;
  inherited Destroy;
end;

end.

