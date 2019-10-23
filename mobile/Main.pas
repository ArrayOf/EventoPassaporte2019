unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Maps, FMX.Controls.Presentation, System.Sensors, FMX.Objects, FMX.Edit,
  System.Sensors.Components, System.Permissions, FMX.Layouts, FMX.DialogService,
  REST.Types,
  REST.Client,
  Web.HTTPApp,
  System.JSON,
  System.Threading,

{$IFDEF ANDROID}
  Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers,
  Androidapi.JNI.Telephony, Androidapi.JNI.Provider, Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Os,
  Androidapi.JNI.App, Data.Bind.Components, Data.Bind.ObjectScope,
  System.ImageList, FMX.ImgList
{$ENDIF};

type
  TMainForm = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    MapView: TMapView;
    FooterLabel: TLabel;
    LocationSensor: TLocationSensor;
    RESTClient: TRESTClient;
    RESTRequest: TRESTRequest;
    ImageList1: TImageList;
    procedure SearchStores();
    procedure LocationSensorLocationChanged(Sender: TObject;
      const OldLocation, NewLocation: TLocationCoord2D);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fCoordinate: TMapCoordinate;
    fUserPin: TMapMarker;
    oPins: array of TMapMarker;
    fCircle: TMapCircle;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MapView.MapType := TMapType.Normal;

  PermissionsService.RequestPermissions
    ([JStringToString(TJManifest_permission.JavaClass.ACCESS_FINE_LOCATION)],
    procedure(const APermissions: TArray<string>;
      const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) = 1) and
        (AGrantResults[0] = TPermissionStatus.Granted) then
      begin
        LocationSensor.Active := True;
        FooterLabel.Text := 'Show! GPS habilitado';
      end
      else
      begin
        LocationSensor.Active := False;
        FooterLabel.Text := 'GPS não habilitado';
      end;
    end)
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  LocationSensor.Active := False;
end;

procedure TMainForm.LocationSensorLocationChanged(Sender: TObject;
const OldLocation, NewLocation: TLocationCoord2D);
var
  oMarker: TMapMarkerDescriptor;
  oCircle: TMapCircleDescriptor;
  aTask: ITask;
begin
  fCoordinate := TMapCoordinate.Create(NewLocation.Latitude,
    NewLocation.Longitude);
  MapView.Location := fCoordinate;

  // Pin
  if Assigned(fUserPin) then
    fUserPin.Remove;

  oMarker := TMapMarkerDescriptor.Create(fCoordinate, 'Sua Localização');
  oMarker.Icon := ImageList1.Source.Items[0].MultiResBitmap.Items[0].Bitmap;
  fUserPin := MapView.AddMarker(oMarker);
  FooterLabel.Text := Format('Latitude: %s | Longitude: %s',
    [fCoordinate.Latitude.ToString, fCoordinate.Longitude.ToString]);

  // Zoom map
  MapView.Zoom := 15;

  if fCircle <> nil then
    fCircle.Remove;

  oCircle := TMapCircleDescriptor.Create(fCoordinate, 3000);
  oCircle.StrokeWidth := 1;
  oCircle.StrokeColor := $B3FEFBEE;
  oCircle.FillColor := $B3FEFBEE;
  fCircle := MapView.AddCircle(oCircle);

  aTask := TTask.Create(
    procedure()
    begin
      SearchStores();
    end);
  aTask.Start;

end;

// Busca estabelecimento
procedure TMainForm.SearchStores();
var
  I: Integer;
  oResponse: TJSONObject;
  oPoints: TJSONArray;
  oPoint: TJSONObject;
  oMarker: TMapMarkerDescriptor;
  oECPin: TMapMarker;
  fLat: Double;
  fLng: Double;
begin

  try
    TThread.Synchronize(TThread.CurrentThread,
      procedure()
      begin
        FooterLabel.Text := 'Buscando Estabelecimentos...';
      end);

    RESTRequest.Method := TRESTRequestMethod.rmGET;
    RESTRequest.Params.ParameterByName('lat').Value :=
    fCoordinate.Latitude.ToString.Replace(',', '.');
     // '-23.5708384';
    RESTRequest.Params.ParameterByName('lng').Value :=
    fCoordinate.Longitude.ToString.Replace(',', '.');
    //  '-46.6576912';

    RESTRequest.Execute;
    oResponse := RESTRequest.Response.JSONValue as TJSONObject;
    oPoints := oResponse.GetValue('result') as TJSONArray;
    oPoints := oPoints.Items[0] as TJSONArray;

    // Limpando ECS anteriores
    for I := 0 to High(oPins) do
    begin
       TThread.Synchronize(TThread.CurrentThread,procedure()
        begin
      oPins[I].Remove;
        end);
    end;
    SetLength(oPins, 0);

    if oPoints.Count > 1 then
    begin
      SetLength(oPins, oPoints.Count);
      TThread.Synchronize(TThread.CurrentThread,
        procedure()
        begin
          FooterLabel.Text := 'Estabelecimentos localizados...';
        end);

      for I := 0 to oPoints.Count - 1 do
      begin
        oPoint := oPoints.Items[I] as TJSONObject;

        // Replace existe por conta do Locale
        fLat := StrToFloat(oPoint.GetValue('lat').Value.Replace('.', ','));
        fLng := StrToFloat(oPoint.GetValue('lng').Value.Replace('.', ','));

        oMarker := TMapMarkerDescriptor.Create(TMapCoordinate.Create(fLat,
          fLng), oPoint.GetValue('name').Value);
        oMarker.Icon := ImageList1.Source.Items[1].MultiResBitmap.Items
          [0].Bitmap;

        TThread.Synchronize(TThread.CurrentThread,
          procedure()
          begin
            oECPin := MapView.AddMarker(oMarker);
            oPins[I] := oECPin;
          end);
      end;
      TThread.Synchronize(TThread.CurrentThread,
        procedure()
        begin
          FooterLabel.Text := 'Estabelecimentos marcado';
        end);
    end
    else
    begin
      TThread.Synchronize(TThread.CurrentThread,
        procedure()
        begin
          FooterLabel.Text := 'Nenhum estabelecimento encontrado';
        end);
    end;
    MapView.Zoom := 25;

  finally
    oResponse.Free;
    oPoints.Free;
    oPoint.Free;
  end

end;

end.
