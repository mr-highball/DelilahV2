program SimpleBot;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, tachartlazaruspkg,
  ui.main;

{$R *.res}

begin
  Application.Title:='SimpleBot';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.

