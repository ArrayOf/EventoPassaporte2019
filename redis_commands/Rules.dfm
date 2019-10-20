object DataModule1: TDataModule1
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 677
  Width = 810
  object RESTClient1: TRESTClient
    Accept = 'application/json, text/plain; q=0.9, text/html;q=0.8,'
    AcceptCharset = 'utf-8, *;q=0.8'
    BaseURL = 'https://maps.googleapis.com/maps/api/staticmap'
    Params = <
      item
        Kind = pkQUERY
        Name = 'key'
      end
      item
        Kind = pkQUERY
        Name = 'size'
        Value = '500x400'
      end
      item
        Name = 'format'
        Value = 'jpg'
      end
      item
        Name = 'scale'
        Value = '2'
      end>
    Left = 80
    Top = 32
  end
  object RESTRequest1: TRESTRequest
    Client = RESTClient1
    Params = <>
    Response = RESTResponse1
    SynchronizedEvents = False
    Left = 80
    Top = 104
  end
  object RESTResponse1: TRESTResponse
    Left = 80
    Top = 184
  end
  object RESTRequest2: TRESTRequest
    Client = RESTClient1
    Params = <
      item
        Kind = pkQUERY
        Name = 'markers'
        Options = [poFlatArray]
        Value = 'color:red|-30.0428357,-51.2188929'
      end>
    Response = RESTResponse2
    SynchronizedEvents = False
    Left = 232
    Top = 104
  end
  object RESTResponse2: TRESTResponse
    ContentType = 'text/plain'
    Left = 232
    Top = 184
  end
  object DSServer1: TDSServer
    AutoStart = False
    Left = 456
    Top = 32
  end
  object DSHTTPService1: TDSHTTPService
    Server = DSServer1
    Filters = <>
    Left = 456
    Top = 104
  end
  object DSServerClass1: TDSServerClass
    OnGetClass = DSServerClass1GetClass
    Server = DSServer1
    LifeCycle = 'Invocation'
    Left = 456
    Top = 176
  end
end
