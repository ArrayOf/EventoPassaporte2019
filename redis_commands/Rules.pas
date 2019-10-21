unit Rules;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Threading,
  System.Types,
  System.JSON,
  Data.DbxHTTPLayer,
  Data.Bind.Components,
  Data.Bind.ObjectScope,
  REST.Types,
  REST.Client,
  IPPeerServer,
  Datasnap.DSCommonServer,
  Datasnap.DSHTTP,
  Datasnap.DSServer,

  Redis.Client,
  Redis.Commons,
  Redis.NetLib.INDY

    ;

type

{$METHODINFO ON}
  TEstabelecimentos = class(TComponent)
  public
    function OlaMundo: string;
    function OndeVamos(const ALatitude: string; const ALongitude: string): TJSONArray;
  end;
{$METHODINFO OFF}

  TdmRules = class(TDataModule)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    RESTRequest2: TRESTRequest;
    RESTResponse2: TRESTResponse;
    DSServer1: TDSServer;
    DSHTTPService1: TDSHTTPService;
    DSServerClass1: TDSServerClass;
    procedure DSServerClass1GetClass(DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private const
    FEventoPassaporteLat = -23.5708384;
    FEventoPassaporteLng = -46.6576912;
  private
    FRedisHost    : string;
    FRedisPort    : Integer;
    FRedisHostPort: string;
    function GetGoogleAPIKey: string;
    procedure SetGoogleAPIKey(const Value: string);
    procedure SetRedisHostPort(AValue: string);
    procedure LoadCFGFromINI;
    procedure SaveCFGToINI;
  public
    function TestRedis: Boolean;
    function GetMap: TStringStream;
    function WhereAreWe: TStringStream;
    procedure ActiveServer;
    procedure LoadDataSet;
  published
    property GoogleAPIKey : string read GetGoogleAPIKey write SetGoogleAPIKey;
    property RedisHostPort: string read FRedisHostPort write SetRedisHostPort;
  end;

var
  dmRules: TdmRules;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses
  System.IniFiles,
  Main,
  Redis.Values;

{$R *.dfm}
{ TDataModule1 }

procedure TdmRules.ActiveServer;
begin
  Self.DSServer1.Start;
end;

procedure TdmRules.DataModuleCreate(Sender: TObject);
begin
  Self.LoadCFGFromINI;
end;

procedure TdmRules.DataModuleDestroy(Sender: TObject);
begin
  Self.SaveCFGToINI;
end;

procedure TdmRules.DSServerClass1GetClass(DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := TEstabelecimentos;
end;

function TdmRules.GetGoogleAPIKey: string;
begin
  Result := Self.RESTClient1.Params.ParameterByName('key').Value;
end;

function TdmRules.GetMap: TStringStream;
begin
  Self.RESTRequest1.Execute;

  Result := TStringStream.Create;
  Result.WriteData(Self.RESTResponse1.RawBytes, Self.RESTResponse1.ContentLength);
  Result.Seek(0, 0);
end;

procedure TdmRules.LoadCFGFromINI;
var
  oINI: TIniFile;
begin
  oINI := TIniFile.Create('.\conf.ini');
  try
    Self.GoogleAPIKey  := oINI.ReadString('GENERAL', 'GOOGLE_KEY', '- CRIE A SUA PRÓPRIA API KEY -');
    Self.RedisHostPort := oINI.ReadString('GENERAL', 'REDIS', 'localhost:6379');
  finally
    oINI.Free;
  end;
end;

procedure TdmRules.LoadDataSet;
type
  TData = record
    UUID: string[36];
    Estabelecimento: string[255];
    Latitude: Extended;
    Longitude: Extended;
    Endereco: string[255];
  end;
var
  oRedis   : IRedisClient;
  hFile    : TextFile;
  slFields : TStringList;
  sFileName: string;
  sLine    : string;
  rData    : TData;
begin
  sFileName := '.\dados.csv';

  slFields                 := TStringList.Create;
  slFields.StrictDelimiter := True;
  slFields.Delimiter       := ';';
  slFields.QuoteChar       := '"';

  try
    try
      oRedis := TRedisClient.Create(Self.FRedisHost, Self.FRedisPort);
      oRedis.Connect;
      oRedis.FLUSHDB;

      AssignFile(hFile, sFileName);
      Reset(hFile);
      while not Eof(hFile) do
      begin
        Readln(hFile, sLine);
        if sLine = EmptyStr then
        begin
          Break;
        end;

        slFields.DelimitedText := sLine;

        rData.UUID            := ShortString(slFields[0]);
        rData.Estabelecimento := ShortString(slFields[1]);
        rData.Endereco        := ShortString(slFields[2]);
        rData.Latitude        := StrToFloat(slFields[3].Replace(',', '.'));
        rData.Longitude       := StrToFloat(slFields[4].Replace(',', '.'));

        oRedis.GEOADD('EVENTO:PASSAPORTE:2019#', rData.Latitude, rData.Longitude, string(rData.UUID))
      end;
    except
      on E: Exception do
      begin
        raise;
      end;
    end;
  finally
    CloseFile(hFile);
    slFields.Free;
  end;
end;

procedure TdmRules.SaveCFGToINI;
var
  oIniFile: TIniFile;
begin
  oIniFile := TIniFile.Create('.\conf.ini');
  try
    oIniFile.WriteString('GENERAL', 'GOOGLE_KEY', Self.GoogleAPIKey);
    oIniFile.WriteString('GENERAL', 'REDIS', Self.RedisHostPort);
  finally
    oIniFile.Free;
  end;
end;

procedure TdmRules.SetGoogleAPIKey(const Value: string);
begin
  Self.RESTClient1.Params.ParameterByName('key').Value := Value;
end;

procedure TdmRules.SetRedisHostPort(AValue: string);
var
  slParts: TStringList;
  sHost  : string;
  sPort  : string;
begin
  if AValue = EmptyStr then
  begin
    AValue := 'localhost';
  end;

  slParts := TStringList.Create;
  try
    slParts.Delimiter       := ':';
    slParts.StrictDelimiter := True;
    slParts.DelimitedText   := AValue;

    sHost := slParts[0];
    if slParts.Count > 1 then
    begin
      sPort := slParts[1];
    end else begin
      sPort := '6379';
    end;
  finally
    slParts.Free;
  end;

  Self.FRedisHostPort := AValue;
  Self.FRedisHost     := sHost;
  Self.FRedisPort     := StrToInt(sPort);
end;

function TdmRules.TestRedis: Boolean;
var
  oRedis: IRedisClient;
begin
  try
    oRedis := TRedisClient.Create(Self.FRedisHost, Self.FRedisPort);
    oRedis.Connect;

    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
    end;
  end;
end;

function TdmRules.WhereAreWe: TStringStream;
var
  sCoordinates: string;
begin
  sCoordinates := Format('%g,%g', [Self.FEventoPassaporteLat, Self.FEventoPassaporteLng]);

  Self.RESTRequest2.Params.ParameterByName('markers').Value := Format('color:red|%s', [sCoordinates]);

  Self.RESTRequest2.Execute;

  Result := TStringStream.Create;
  Result.WriteData(Self.RESTResponse2.RawBytes, Self.RESTResponse2.ContentLength);
  Result.Seek(0, 0);
end;

{ TEstabelecimentos }

function TEstabelecimentos.OlaMundo: string;
begin
  Result := 'Olá Mundo!';
end;

function TEstabelecimentos.OndeVamos(const ALatitude, ALongitude: string): TJSONArray;
const
  KEY_NAME = 'EVENTO:PASSAPORTE:2019#';
var
  oRedis      : IRedisClient;
  aResultItems: TRedisArray;
  aResultCoord: TRedisMatrix;
  oItem       : TJSONObject;
  sUUID       : string;
begin
  oRedis := TRedisClient.Create(dmRules.FRedisHost, dmRules.FRedisPort);
  oRedis.Connect;

  aResultItems := oRedis.GEORADIUS(KEY_NAME, StrToFloat(ALongitude), StrToFloat(ALatitude), 3000, TRedisGeoUnit.Meters, TRedisSorting.Asc, -1);

  Result := TJSONArray.Create;
  if not aResultItems.IsNull then
  begin
    for sUUID in aResultItems.Value do
    begin
      aResultCoord := oRedis.GEOPOS(KEY_NAME, [sUUID]);

      oItem := TJSONObject.Create;
      oItem.AddPair('lat', TJSONNumber.Create(aResultCoord.Value[0].Value[0]));
      oItem.AddPair('lng', TJSONNumber.Create(aResultCoord.Value[0].Value[1]));
      oItem.AddPair('name', sUUID);

      Result.Add(oItem);
    end;
  end;
end;

end.
