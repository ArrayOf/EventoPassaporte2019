unit LoadEPTCData;

interface

uses
  System.Classes,
  System.Generics.Collections,

  Redis.Client,
  Redis.Commons,
  Redis.NetLib.INDY

    ;

type

  TParada = record
    IDParada: string;
    Latidude: Extended;
    Longitude: Extended;
  end;

  TProcessLine = reference to procedure(const ALine: string);

  TLoadEPTC = class(TObject)
  private
    FLog    : TProcessLine;
    procedure ScanCSVFile(const AFileName: string; AProcessLine: TProcessLine);
    function Load_paradas: TArray<TParada>;
    function Load_paradalinhas: TDictionary<string, TStringList>;
    function Load_linhas: TDictionary<string, string>;
  public
    procedure Work(const AHost: string; const APort: Integer);
    constructor Create(ALog: TProcessLine); reintroduce;
  end;

implementation

uses
  System.SysUtils,
  System.Diagnostics,
  System.Threading;

{ TLoadEPTC }

constructor TLoadEPTC.Create(ALog: TProcessLine);
begin
  inherited Create;
  Self.FLog     := ALog;
end;

function TLoadEPTC.Load_linhas: TDictionary<string, string>;
var
  sFileName: string;
  maProcess: TProcessLine;
  slFields : TStringList;
  dBuffer  : TDictionary<string, string>;
begin
  Self.FLog('Load_linhas');

  sFileName := '.\linhas.csv';

  slFields                 := TStringList.Create;
  slFields.StrictDelimiter := True;
  slFields.Delimiter       := ';';
  slFields.QuoteChar       := '"';

  dBuffer := TDictionary<string, string>.Create();

  maProcess := procedure(const ALine: string)
    begin
      slFields.DelimitedText := ALine;
      dBuffer.Add(slFields[0], slFields[2]);
    end;

  try
    Self.ScanCSVFile(sFileName, maProcess);
  finally
    slFields.Free;
  end;

  Result := dBuffer;
end;

function TLoadEPTC.Load_paradalinhas: TDictionary<string, TStringList>;
var
  sFileName: string;
  maProcess: TProcessLine;
  dBuffer  : TDictionary<string, TStringList>;
  slFields : TStringList;
begin
  Self.FLog('Load_paradalinhas');

  sFileName := '.\paradalinha.csv';

  slFields                 := TStringList.Create;
  slFields.StrictDelimiter := True;
  slFields.Delimiter       := ';';
  slFields.QuoteChar       := '"';

  dBuffer := TDictionary<string, TStringList>.Create();

  maProcess := procedure(const ALine: string)
    var
      slLinhas: TStringList;
    begin
      slFields.DelimitedText := ALine;

      if not dBuffer.TryGetValue(slFields[1], slLinhas) then
      begin
        slLinhas := TStringList.Create;
        dBuffer.Add(slFields[1], slLinhas);
      end;

      slLinhas.Add(slFields[0]);
    end;

  try
    Self.ScanCSVFile(sFileName, maProcess);
  finally
    slFields.Free;
  end;

  Result := dBuffer;
end;

function TLoadEPTC.Load_paradas: TArray<TParada>;
var
  sFileName: string;
  maProcess: TProcessLine;
  aBuffer  : TArray<TParada>;
  slFields : TStringList;
begin
  Self.FLog('Load_paradas');

  sFileName := '.\paradas.csv';

  slFields                 := TStringList.Create;
  slFields.StrictDelimiter := True;
  slFields.Delimiter       := ';';
  slFields.QuoteChar       := '"';

  maProcess := procedure(const ALine: string)
    var
      iIndex: Integer;
    begin
      iIndex := Length(aBuffer);
      SetLength(aBuffer, Succ(iIndex));

      slFields.DelimitedText := ALine;

      with aBuffer[iIndex] do
      begin
        IDParada  := slFields[0];
        Latidude  := StrToFloat(slFields[3].Replace(',', '.'));
        Longitude := StrToFloat(slFields[2].Replace(',', '.'));
      end;
    end;

  try
    try
      Self.ScanCSVFile(sFileName, maProcess);
    except
      on E: Exception do
      begin
        Self.FLog(Format('Load_paradas - (%s) - %s', [E.ClassName, E.Message]));
      end;
    end;
  finally
    slFields.Free;
  end;

  Result := aBuffer;
end;

procedure TLoadEPTC.ScanCSVFile(const AFileName: string; AProcessLine: TProcessLine);
var
  hFile: TextFile;
  sLine: string;
begin
  AssignFile(hFile, AFileName);
  try
    try
      Reset(hFile);
      Readln(hFile, sLine);
      while not Eof(hFile) do
      begin
        Readln(hFile, sLine);
        if sLine = EmptyStr then
        begin
          Break;
        end;

        AProcessLine(sLine);
      end;
    except
      on E: Exception do
      begin
        Self.FLog(Format('(%s) - %s', [E.ClassName, E.Message]));
      end;
    end;
  finally
    CloseFile(hFile);
  end;
end;

procedure TLoadEPTC.Work(const AHost: string; const APort: Integer);
var
  oTask1: IFuture<TArray<TParada>>;
  oTask2: IFuture<TDictionary<string, TStringList>>;
  oTask3: IFuture<TDictionary<string, string>>;

  aParadas    : TArray<TParada>;
  dParadaLinha: TDictionary<string, TStringList>;
  dLinhas     : TDictionary<string, string>;

  rStopWatch    : TStopWatch;
  rParada       : TParada;
  iCount        : Integer;
  sKeyName      : string;
  sCodigoLinha  : string;
  sIDLinha      : string;
  slParadaLinhas: TStringList;
  oRedis        : IRedisClient;
begin
  {
    Arquivos envolvidos:
    - paradas.csv
    - paradalinha.csv
    - linhas.csv
  }
  rStopWatch := TStopWatch.Create;
  rStopWatch.Start;

  dParadaLinha := nil;
  dLinhas      := nil;

  try
    try

      Self.FLog('Iniciando o processamento ...');

      oTask1 := TTask.Future < TArray < TParada >> (Self.Load_paradas);
      oTask2 := TTask.Future < TDictionary < string, TStringList >> (Self.Load_paradalinhas);
      oTask3 := TTask.Future < TDictionary < string, string >> (Self.Load_linhas);

      TTask.WaitForAll([oTask1, oTask2, oTask3]);
      Self.FLog('Arquivos CSVs carregados com sucesso!');

      aParadas     := oTask1.Value;
      dParadaLinha := oTask2.Value;
      dLinhas      := oTask3.Value;

      Self.FLog('Carregando o REDIS ...');
      oRedis := TRedisClient.Create(AHost, APort);
      oRedis.Connect;

      oRedis.FLUSHDB;

      iCount := 0;
      for rParada in aParadas do
      begin
        if dParadaLinha.TryGetValue(rParada.IDParada, slParadaLinhas) then
        begin
          for sIDLinha in slParadaLinhas do
          begin
            if dLinhas.TryGetValue(sIDLinha, sCodigoLinha) then
            begin
              Inc(iCount);

              sKeyName := Format('DELPHISQUAD:2019:POA:LINHA:%s#', [sCodigoLinha]);
              oRedis.GEOADD(
                sKeyName,          // Nome da chave
                rParada.Latidude,  // Latitude
                rParada.Longitude, // Longitude
                rParada.IDParada   // Informação geolacalizada
              );
            end;
          end;
        end;
      end;

      oRedis.GEOADD('DELPHISQUAD:2019:POA:LINHA:244-1#', -23.50115373, -46.84117788, 'TESTE INTERNO');

      Self.FLog(Format('Processado! %d itens', [iCount]));
    except
      on E: Exception do
      begin
        Self.FLog(Format('(%s) - %s', [E.ClassName, E.Message]));
      end;
    end;
  finally
    if Assigned(dParadaLinha) then
    begin
      for slParadaLinhas in dParadaLinha.Values do
      begin
        slParadaLinhas.Free;
      end;
      dParadaLinha.Free;
    end;

    if Assigned(dLinhas) then
    begin
      dLinhas.Free;
    end;

    rStopWatch.Stop;
    Self.FLog(Format('Tempo decorrido: %d segundos', [rStopWatch.Elapsed.Seconds]));
  end;
end;

end.
