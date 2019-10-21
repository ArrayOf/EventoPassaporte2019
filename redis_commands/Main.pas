unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Threading,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  System.Actions,
  Vcl.ActnList;

type
  TfMain = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Image1: TImage;
    Panel3: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel4: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ImageMapa: TImage;
    Memo1: TMemo;
    Timer1: TTimer;
    Button5: TButton;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    Button3: TButton;
    Button4: TButton;
    Button6: TButton;
    Button7: TButton;
    ActionList1: TActionList;
    ActionMapaMundi: TAction;
    ActionOndeEstamos: TAction;
    ActionInserirEstabelecimentos: TAction;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure LabeledEdit1Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure LabeledEdit2Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ActionMapaMundiExecute(Sender: TObject);
    procedure ActionOndeEstamosExecute(Sender: TObject);
    procedure ActionInserirEstabelecimentosExecute(Sender: TObject);
  private
    { Private declarations }
    FLoadTask: ITask;
    procedure ShowMap(AContent: TStringStream);
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

uses
  Rules,
  Vcl.Imaging.jpeg;

procedure TfMain.ActionInserirEstabelecimentosExecute(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    dmRules.LoadDataSet;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfMain.ActionMapaMundiExecute(Sender: TObject);
var
  oResponse: TStringStream;
begin
  Screen.Cursor := crHourGlass;
  oResponse     := nil;

  Self.PageControl1.ActivePageIndex := 0;

  try
    oResponse := dmRules.GetMap;
    Self.ShowMap(oResponse);
  finally
    oResponse.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfMain.ActionOndeEstamosExecute(Sender: TObject);
var
  oResponse: TStringStream;
begin
  Screen.Cursor := crHourGlass;
  oResponse     := nil;

  Self.PageControl1.ActivePageIndex := 0;

  try
    oResponse := dmRules.WhereAreWe;
    Self.ShowMap(oResponse);
  finally
    oResponse.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfMain.Button3Click(Sender: TObject);
var
  bRet: Boolean;
begin
  Screen.Cursor := crHourGlass;

  bRet := dmRules.TestRedis;
  if bRet then
  begin
    MessageBox(Self.Handle, 'O Redis está acessível!', 'Atenção!', MB_ICONINFORMATION + MB_OK);
  end else begin
    MessageBox(Self.Handle, 'O Redis *NÃO* está acessível!', 'Atenção!', MB_ICONERROR + MB_OK);
  end;

  Screen.Cursor := crDefault;
end;

procedure TfMain.Button4Click(Sender: TObject);
begin
  dmRules.ActiveServer;
  Self.Button4.Enabled := False;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown     := True;
  FormatSettings.DecimalSeparator := '.';

  Self.FLoadTask := nil;

  Self.LabeledEdit1.Text := dmRules.GoogleAPIKey;
  Self.LabeledEdit2.Text := dmRules.RedisHostPort;

  Self.PageControl1.ActivePageIndex := 0;
end;

procedure TfMain.LabeledEdit1Change(Sender: TObject);
begin
  dmRules.GoogleAPIKey := Self.LabeledEdit1.Text;
end;

procedure TfMain.LabeledEdit2Change(Sender: TObject);
begin
  dmRules.RedisHostPort := Self.LabeledEdit2.Text;
end;

procedure TfMain.ShowMap(AContent: TStringStream);
var
  oJPG: TJPEGImage;
  oBMP: TBitmap;
begin
  oJPG := TJPEGImage.Create;
  oBMP := TBitmap.Create;

  oJPG.LoadFromStream(AContent);
  oBMP.Assign(oJPG);

  Self.ImageMapa.Picture.Bitmap.Assign(oBMP);

  oJPG.Free;
  oBMP.Free;
end;

procedure TfMain.Timer1Timer(Sender: TObject);
begin
  Self.Timer1.Enabled := False;

  if Assigned(Self.FLoadTask) then
  begin
    case Self.FLoadTask.Status of
      TTaskStatus.Created:
        ;
      TTaskStatus.WaitingToRun:
        ;
      TTaskStatus.Running:
        begin
          Self.Memo1.Lines.Add('Tarefa em andamento!');
        end;
      TTaskStatus.Completed:
        begin
          Self.Memo1.Lines.Add('Tarefa completa!');
          Self.FLoadTask := nil;
        end;
      TTaskStatus.WaitingForChildren:
        ;
      TTaskStatus.Canceled:
        ;
      TTaskStatus.Exception:
        begin
          Self.Memo1.Lines.Add('Deu exception!');
          Self.FLoadTask := nil;
        end;
    end;

    if Assigned(Self.FLoadTask) then
    begin
      Self.Timer1.Enabled := True;
    end else begin
      Self.Button5.Enabled := True;
      Application.ProcessMessages;
    end;
  end;

end;

end.
