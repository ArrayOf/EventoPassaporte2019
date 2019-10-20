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

  LoadEPTCData,

  Redis.Client,
  Redis.Commons,
  Redis.NetLib.INDY

    ;

type

{$METHODINFO ON}
  TStopBus = class(TComponent)
  public
    function HelloWorld: string;
    function BusHunter(const ALine: string; const ALatitude: string; const ALongitude: string): TJSONObject;
    function BusList: TJSONArray;
  end;
{$METHODINFO OFF}

  TDataModule1 = class(TDataModule)
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
    FDelphiSquadLat = -30.0428357;
    FDelphiSquadLng = -51.2188929;
  private
    FRedisHost    : string;
    FRedisPort    : Integer;
    FRedisHostPort: string;
    function GetGoogleAPIKey: string;
    procedure SetGoogleAPIKey(const Value: string);
    procedure SetRedisHostPort(const Value: string);
    procedure LoadCFGFromINI;
    procedure SaveCFGToINI;
  public
    function TestRedis: Boolean;
    function GetMap: TStringStream;
    function WhereAreWe: TStringStream;
    function LoadEPTC(ALog: TProcessLine): ITask;
    function GetNearbyBusStop(const ALine: string): TStringStream;
    function BusList: TArray<string>;
    procedure ActiveServer;
  published
    property GoogleAPIKey : string read GetGoogleAPIKey write SetGoogleAPIKey;
    property RedisHostPort: string read FRedisHostPort write SetRedisHostPort;
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses
  System.IniFiles,
  Main,
  QueryEPTC;

{$R *.dfm}
{ TDataModule1 }

procedure TDataModule1.ActiveServer;
begin
  Self.DSServer1.Start;
end;

function TDataModule1.BusList: TArray<string>;
var
  oQuery: TQueryEPTC;
begin
  oQuery := TQueryEPTC.Create(Self.FRedisHost, Self.FRedisPort);
  try
    Result := oQuery.BusList;
  finally
    oQuery.Free;
  end;
end;

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
  Self.LoadCFGFromINI;
end;

procedure TDataModule1.DataModuleDestroy(Sender: TObject);
begin
  Self.SaveCFGToINI;
end;

procedure TDataModule1.DSServerClass1GetClass(DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := TStopBus;
end;

function TDataModule1.GetGoogleAPIKey: string;
begin
  Result := Self.RESTClient1.Params.ParameterByName('key').Value;
end;

function TDataModule1.GetMap: TStringStream;
begin
  Self.RESTRequest1.Execute;

  Result := TStringStream.Create;
  Result.WriteData(Self.RESTResponse1.RawBytes, Self.RESTResponse1.ContentLength);
  Result.Seek(0, 0);
end;

function TDataModule1.GetNearbyBusStop(const ALine: string): TStringStream;
const
  MARKERS = 'color:green|%s,%s;color:red|%g,%g';
var
  oQuery      : TQueryEPTC;
  aCoordinates: TArray<string>;
  sCoordinates: string;
begin
  oQuery := TQueryEPTC.Create(Self.FRedisHost, Self.FRedisPort);
  try
    aCoordinates := oQuery.NearbyBusStop(ALine, Self.FDelphiSquadLat, Self.FDelphiSquadLng);
    sCoordinates := Format(MARKERS, [aCoordinates[0], aCoordinates[1], Self.FDelphiSquadLat, Self.FDelphiSquadLng]);

    Self.RESTRequest2.Params.ParameterByName('markers').Value := sCoordinates;
    Self.RESTRequest2.Execute;

    Result := TStringStream.Create;
    Result.WriteData(Self.RESTResponse2.RawBytes, Self.RESTResponse2.ContentLength);
    Result.Seek(0, 0);
  finally
    oQuery.Free;
  end;
end;

function TDataModule1.LoadEPTC(ALog: TProcessLine): ITask;
var
  maEPTCLoad: TProc;
begin
  maEPTCLoad := procedure
    var
      oLoader: TLoadEPTC;
    begin
      oLoader := TLoadEPTC.Create(ALog);
      try
        oLoader.Work(Self.FRedisHost, Self.FRedisPort);
      finally
        oLoader.Free;
      end;
    end;

  Result := TTask.Run(maEPTCLoad);
end;

procedure TDataModule1.LoadCFGFromINI;
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

procedure TDataModule1.SaveCFGToINI;
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

procedure TDataModule1.SetGoogleAPIKey(const Value: string);
begin
  Self.RESTClient1.Params.ParameterByName('key').Value := Value;
end;

procedure TDataModule1.SetRedisHostPort(const Value: string);
var
  slParts: TStringList;
  sHost  : string;
  sPort  : string;
begin
  slParts := TStringList.Create;
  try
    slParts.Delimiter       := ':';
    slParts.StrictDelimiter := True;
    slParts.DelimitedText   := Value;

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

  Self.FRedisHostPort := Value;
  Self.FRedisHost     := sHost;
  Self.FRedisPort     := StrToInt(sPort);
end;

function TDataModule1.TestRedis: Boolean;
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

function TDataModule1.WhereAreWe: TStringStream;
var
  sCoordinates: string;
begin
  sCoordinates := Format('%g,%g', [Self.FDelphiSquadLat, Self.FDelphiSquadLng]);

  Self.RESTRequest2.Params.ParameterByName('markers').Value := Format('color:red|%s', [sCoordinates]);

  Self.RESTRequest2.Execute;

  Result := TStringStream.Create;
  Result.WriteData(Self.RESTResponse2.RawBytes, Self.RESTResponse2.ContentLength);
  Result.Seek(0, 0);
end;

{ TStopBus }

function TStopBus.BusHunter(const ALine: string; const ALatitude, ALongitude: string): TJSONObject;
var
  oQuery : TQueryEPTC;
  aBuffer: TArray<string>;
begin
  oQuery := TQueryEPTC.Create(DataModule1.FRedisHost, DataModule1.FRedisPort);
  try
    aBuffer := oQuery.NearbyBusStop(ALine, StrToFloat(ALatitude), StrToFloat(ALongitude));

    Result := TJSONObject.Create;
    Result.AddPair('lat', aBuffer[0]);
    Result.AddPair('lng', aBuffer[1]);
  finally
    oQuery.Free;
  end;
end;

function TStopBus.BusList: TJSONArray;
var
  oQuery : TQueryEPTC;
  aBuffer: TArray<string>;
  sItem  : string;
begin
  oQuery := TQueryEPTC.Create(DataModule1.FRedisHost, DataModule1.FRedisPort);
  try
    aBuffer := oQuery.BusList;

    Result := TJSONArray.Create;
    for sItem in aBuffer do
    begin
      Result.Add(sItem);
    end;
  finally
    oQuery.Free;
  end;
end;

function TStopBus.HelloWorld: string;
begin
  Result := 'Olá mundo!';
end;

end.
