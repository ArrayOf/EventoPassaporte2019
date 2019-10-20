unit QueryEPTC;

interface

uses
  System.Types,
  System.Generics.Collections,

  Redis.Client,
  Redis.Values,
  Redis.Commons,
  Redis.NetLib.INDY

    ;

type

  TQueryEPTC = class(TObject)
  private
    FRedis: IRedisClient;
  public
    constructor Create(const ARedisHost: string; const ARedisPort: Integer); reintroduce;
    function NearbyBusStop(const ACodeLine: string; const ALatitude: Extended; const ALongitude: Extended): TArray<string>;
    function BusList: TArray<string>;
  end;

implementation

uses
  System.SysUtils,
  System.Classes;

{ TQueryEPTC }

function TQueryEPTC.BusList: TArray<string>;
var
  slLuaVai: TStringList;
  oCMD    : IRedisCommand;
  aBuffer : TRedisArray;
  i: Integer;
begin
  slLuaVai := TStringList.Create;
  try
    with slLuaVai do
    begin
      Add('local ret = {}');
      Add('local keys = redis.call("KEYS", "DELPHISQUAD:2019:POA:LINHA:*#")');
      Add('');
      Add('if (keys) then');
      Add('  for i, item in ipairs(keys) do');
      Add('    local code = string.match(item, "DELPHISQUAD:2019:POA:LINHA:([^#]+)#")');
      Add('    table.insert(ret, code)');
      Add('  end');
      Add('end');
      Add('');
      Add('return ret');
    end;

    oCMD := NewRedisCommand('EVAL');
    oCMD.Add(slLuaVai.Text).Add(0);

    aBuffer := Self.FRedis.ExecuteAndGetArray(oCMD);

    if aBuffer.HasValue then
    begin
      SetLength(Result, Length(aBuffer.Value));
      for i := 0 to Pred(Length(Result)) do
      begin
        Result[i] := aBuffer.Value[i].Value
      end;
      TArray.Sort<string>(Result);
    end;
  finally
    slLuaVai.Free;
  end;
end;

constructor TQueryEPTC.Create(const ARedisHost: string; const ARedisPort: Integer);
begin
  inherited Create;

  Self.FRedis := TRedisClient.Create(ARedisHost, ARedisPort);
  Self.FRedis.Connect;
end;

function TQueryEPTC.NearbyBusStop(const ACodeLine: string; const ALatitude: Extended; const ALongitude: Extended): TArray<string>;
var
  sKeyName    : string;
  aResult     : TRedisArray;
  aCoordinates: TRedisMatrix;
begin
  sKeyName := Format('DELPHISQUAD:2019:POA:LINHA:%s#', [ACodeLine]);
  aResult  := Self.FRedis.GEORADIUS(
    sKeyName,    // Nome da Chave
    ALongitude,  // Latitude de refeência
    ALatitude,   // Longitude de referência
    10000000,    // Distância desejada
    TRedisGeoUnit.Meters,  // Unidade de medida da distância
    TRedisSorting.Asc,     // Ordenação do resultado
    1                      // Quantidade desejada
  );

  if not aResult.IsNull then
  begin
    aCoordinates := Self.FRedis.GEOPOS(sKeyName, [aResult.Value[0].Value]);

    SetLength(Result, 2);
    Result[0] := aCoordinates.Value[0].Value[0].Value;
    Result[1] := aCoordinates.Value[0].Value[1].Value;
  end;
end;

end.
