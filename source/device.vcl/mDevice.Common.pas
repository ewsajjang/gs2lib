unit mDevice.Common;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo;

type
  TDeviceCaterize = (dcGOD, dcGDH, dcHbA1c, dcBG);
  TDeviceCaterizeHelper = record helper for TDeviceCaterize
    function Str: String;
  end;

  TDeviceCaterizeSet = Set of TDeviceCaterize;
  TDeviceCaterizeSetHelper = record Helper for TDeviceCaterizeSet
    function Contain(const AValue: TDeviceCaterize): Boolean;
  end;

  TDeviceCommunicationType = (dctCP210x, dctDragon);
  TDeviceCommunicationTypeHelper = record helper for TDeviceCommunicationType
    function Str: String;
  end;
  TDeviceCommunicationTypeSet = set of TDeviceCommunicationType;
  TDeviceCommunicationTypeSetHelper = record Helper for TDeviceCommunicationTypeSet
    function Contain(const AValue: TDeviceCommunicationType): Boolean;
  end;

  EDeviceModel = class(Exception);
  ECaterizeNotMatch = class(EDeviceModel);
  ECommunicationTypeNotMatch = class(EDeviceModel);
  TDeviceModel = record
    Caterize: TDeviceCaterizeSet;
    CommunicationType: TDeviceCommunicationTypeSet;

    constructor Create(const AModel: Integer);
  end;
  TDeviceModelHelper = record helper for TDeviceModel
    function Contain(const AValue: TDeviceCaterize): Boolean; overload;
    function Contain(const AValue: TDeviceCommunicationType): Boolean; overload;
    function Str: String;
  end;

implementation

uses
  mSocket.Common;

{ TDeviceCaterizeHelper }

function TDeviceCaterizeHelper.Str: String;
begin
  Result := GetEnumName(TypeInfo(TDeviceCaterize), Integer(Self));
end;

{ TDeviceCommunicationTypeHelper }

function TDeviceCommunicationTypeHelper.Str: String;
begin
  Result := GetEnumName(TypeInfo(TDeviceCommunicationType), Integer(Self));
end;

{ TDeviceCaterizeSetHelper }

function TDeviceCaterizeSetHelper.Contain(
  const AValue: TDeviceCaterize): Boolean;
begin
  Result := AValue in Self;
end;

{ TDeviceCommunicationTypeSetHelper }

function TDeviceCommunicationTypeSetHelper.Contain(
  const AValue: TDeviceCommunicationType): Boolean;
begin
  Result := AValue in Self;
end;

{ TDeviceMode }

constructor TDeviceModel.Create(const AModel: Integer);
var
  LModel: TSDModel;
begin
  LModel := TSDModel.Create(AModel);
  if LModel in TSDModels.CaterizeBGUnidentified then
    Caterize := [dcBG]
  else if LModel in TSDModels.CaterizeBGEnzymeGOD then
    Caterize := [dcGOD]
  else if LModel in TSDModels.CaterizeBGEnzymeGDH then
    Caterize := [dcGDH]
  else if LModel in TSDModels.CaterizeHbA1c then
    Caterize := [dcHbA1c]
  else
    raise ECaterizeNotMatch.CreateFmt('Model[%x] can not matches Caterize', [LModel.Str]);

  CommunicationType := [];
  if LModel in TSDModels.SocketCP210x then
    CommunicationType := [dctCP210x];
  if LModel in TSDModels.SocketDragonBG then
    CommunicationType := CommunicationType + [dctCP210x];

  if CommunicationType = [] then
    raise ECommunicationTypeNotMatch.CreateFmt('Model[%x] can not matches CommunicationType', [AModel]);
end;

{ TDeviceModelHelper }

function TDeviceModelHelper.Contain(const AValue: TDeviceCaterize): Boolean;
begin
  Result := Caterize.Contain(AValue)
end;

function TDeviceModelHelper.Contain(
  const AValue: TDeviceCommunicationType): Boolean;
begin
  Result := CommunicationType.Contain(AValue)
end;

function TDeviceModelHelper.Str: String;
var
  LDC: TDeviceCaterize;
//  LDCT: TDeviceCommunicationType;
  LSL: TStringList;
begin
  LSL := TStringList.Create;
  try
    for LDC in Caterize do
      LSL.Add(LDC.Str.Substring(2));
    Result := Format('%s', [LSL.CommaText]);

//    LSL.Clear;
//    for LDCT in CommunicationType do
//      LSL.Add(LDCT.Str.Substring(3));
//    Result := Format('%s / %s', [Result, LSL.CommaText]);
  finally
    FreeAndNil(LSL);
  end;
end;

end.
