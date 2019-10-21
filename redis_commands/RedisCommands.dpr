program RedisCommands;

uses
  Vcl.Forms,
  Main in 'Main.pas' {fMain},
  Rules in 'Rules.pas' {dmRules: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdmRules, dmRules);
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
