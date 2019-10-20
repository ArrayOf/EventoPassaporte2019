program RedisCommands;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  Rules in 'Rules.pas' {DataModule1: TDataModule},
  LoadEPTCData in 'LoadEPTCData.pas',
  QueryEPTC in 'QueryEPTC.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
