unit ui.email.frame;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ui.usercontrol,
  ui.usercontrol.singleline;

type

  { TEmailSetupFrame }

  TEmailSetupFrame = class(TFrame)
  private
    FEmail,
    FPass,
    FAddr,
    FPort:TSingleLine;
    function GetAddr: String;
    function GetEmail: String;
    function GetPass: String;
    function GetPort: Cardinal;
    procedure SetAddr(AValue: String);
    procedure SetEmail(AValue: String);
    procedure SetPass(AValue: String);
    procedure SetPort(AValue: Cardinal);
  protected
    procedure Loaded; override;
  public
    property Email : String read GetEmail write SetEmail;
    property Password : String read GetPass write SetPass;
    property SMTPAddress : String read GetAddr write SetAddr;
    property Port : Cardinal read GetPort write SetPort;
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TEmailSetupFrame }

function TEmailSetupFrame.GetAddr: String;
begin
  Result:=FAddr.Text;
end;

function TEmailSetupFrame.GetEmail: String;
begin
  Result:=Trim(FEmail.Text);
end;

function TEmailSetupFrame.GetPass: String;
begin
  Result:=FPass.Text;
end;

function TEmailSetupFrame.GetPort: Cardinal;
begin
  Result:=StrToIntDef(FPort.Text,25);
end;

procedure TEmailSetupFrame.SetAddr(AValue: String);
begin
  FAddr.Text:=AValue;
end;

procedure TEmailSetupFrame.SetEmail(AValue: String);
begin
  FEmail.Text:=Trim(AValue);
end;

procedure TEmailSetupFrame.SetPass(AValue: String);
begin
  FPass.Text:=AValue;
end;

procedure TEmailSetupFrame.SetPort(AValue: Cardinal);
begin
  FPort.Text:=IntToStr(AValue);
end;

procedure TEmailSetupFrame.Loaded;
begin
  inherited Loaded;
  //init controls
  FEmail.Options:=[ucControl,ucDescr,ucTitle];
  FEmail.Control.Width:=150;
  FEmail.Title:='Email Address';
  FEmail.Description:='the email address that notifications will be sent from';
  FEmail.Height:=75;
  FEmail.Parent:=Self;

  FPass.Options:=FEmail.Options;
  FPass.Control.Width:=150;
  FPass.Title:='Email Password';
  FPass.Description:='password for the "From" email address above';
  FPass.Height:=FEmail.Height;
  FPass.Parent:=Self;

  FAddr.Options:=FEmail.Options;
  FAddr.Control.Width:=150;
  FAddr.Title:='SMTP Address';
  FAddr.Description:='SMTP server address';
  FAddr.Height:=FEmail.Height;
  FAddr.Parent:=Self;

  FPort.Options:=FEmail.Options;
  FPort.Control.Width:=50;
  FPort.Title:='SMTP Port';
  FPort.Description:='outgoing port for your SMTP server';
  FPort.Height:=FEmail.Height;
  FPort.Parent:=Self;

  FEmail.Align:=alTop;
  FPass.Align:=alTop;
  FAddr.Align:=alTop;
  FPort.Align:=alTop;

  FEmail.Top:=0;
  FPass.Top:=1;
  FAddr.Top:=2;
  FPort.Top:=3;
end;

constructor TEmailSetupFrame.Create(TheOwner: TComponent);
begin
  FEmail:=TSingleLine.Create(Self);
  FEmail.Name:='email';
  FPass:=TSingleLine.Create(Self);
  FPass.Name:='pass';
  FPort:=TSingleLine.Create(Self);
  FPort.Name:='port';
  FAddr:=TSingleLine.Create(Self);
  FAddr.Name:='addr';

  inherited Create(TheOwner);
end;

end.

