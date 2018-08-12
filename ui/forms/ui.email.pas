unit ui.email;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  JSONPropStorage, ExtCtrls, StdCtrls, ui.email.frame;

type

  TOnSave = TNotifyEvent;
  TOnCancel = TNotifyEvent;
  { TEmailSetup }

  TEmailSetup = class(TForm)
    btn_save: TButton;
    btn_cancel: TButton;
    json_email: TJSONPropStorage;
    pnl_btns: TPanel;
    pnl_ctrls: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FEmail:TEmailSetupFrame;
    FOnCancel: TOnCancel;
    FOnSave: TOnSave;
  protected
    procedure DoOnSave(Sender:TObject);
    procedure DoOnCancel(Sender:TObject);
  public
    property OnSave : TOnSave read FOnSave write FOnSave;
    property OnCancel : TOnCancel read FOnCancel write FOnCancel;
    constructor Create(TheOwner: TComponent); override;overload;
    constructor Create(Const AOnSave:TOnSave;Const AOnCancel:TOnCancel); overload;
  end;

var
  EmailSetup: TEmailSetup;

implementation
uses
  smtpsend;

{$R *.lfm}

{ TEmailSetup }

procedure TEmailSetup.FormCreate(Sender: TObject);
begin
  FEmail:=TEmailSetupFrame.Create(Self);
  FEmail.Align:=alClient;
  FEmail.BorderSpacing.Around:=5;
  FEmail.Parent:=pnl_ctrls;
end;

procedure TEmailSetup.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  DoOnCancel(Self);
end;

procedure TEmailSetup.DoOnSave(Sender: TObject);
var
  LEmail:TStringList;
  LSMTP:TSMTPSend;
begin
  //todo - just testing code need to remove
  LSMTP:=TSMTPSend.Create;
  LSMTP.AutoTLS:=True;
  LEmail:=TStringList.Create;
  LEmail.Add('test mssaage');
  LEmail.Add('-from bot man');
  LSMTP.UserName:=FEmail.Email;
  LSMTP.Password:=FEmail.Password;
  LSMTP.TargetHost:=FEmail.SMTPAddress;
  LSMTP.TargetPort:=IntToStr(FEmail.Port);
  if not LSMTP.Login then
    ShowMessage('Unable to login');
  if not LSMTP.MailData(LEmail) then
    ShowMessage('unable to send email:' + LSMTP.ResultString);
  LSMTP.Logout;
  LEmail.Free;
  LSMTP.Free;

  //actual code...
  if Assigned(FOnSave) then
    FOnSave(Sender)
  else
    ShowMessage('Default OnSave used');
end;

procedure TEmailSetup.DoOnCancel(Sender: TObject);
begin
  if Assigned(FOnCancel) then
    FOnCancel(Sender)
  else
    ShowMessage('Default OnCancel used');
end;

constructor TEmailSetup.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  btn_save.OnClick:=DoOnSave;
  btn_cancel.OnClick:=DoOnCancel;
end;

constructor TEmailSetup.Create(const AOnSave: TOnSave;
  const AOnCancel: TOnCancel);
begin
  inherited Create(nil);
  FOnSave:=AOnSave;
  FOnCancel:=AOnCancel;
end;

end.

