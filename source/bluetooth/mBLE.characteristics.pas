unit mBLE.characteristics;

interface

uses
  System.Bluetooth, System.BluetoothConsts,

  System.Classes, System.SysUtils,
  System.Generics.Collections
  ;

type
  TGlucoseMeasurementCharacteristic = class(TBluetoothGattCharacteristic)
  public const
    IDX_TIME_OFFSET_PRESENT                            = 1;
    IDX_GLUCOSE_CONCERTRATION_TYPE_AND_SAMPLE_LOCATION = 2;
    IDX_GLUCOSE_CONCERTRATIONUNITS                     = 4;
    IDX_SENSOR_STATUS_ANNUNCIATION_PRESENT             = 8;
    IDX_CONTEXT_INFORMATIONFOLLOWS                     = 16;
  public type
    TFlag = (
      fTimeOffsetPresent                         ,
      fGlucoseConcertrationTypeAndSampleLocation ,
      fGlucoseConcertrationUnits_kg_L            ,
      fGlucoseConcertrationUnits_mol_L           ,
      fSensorStatusAnnunciationPresent           ,
      fContextInformationFollows
    );
    TFlags = set of TFlag;
    TType = (
      tNone = 0,
      tCapillaryWholeBlood,
      tCapillaryPlasma,
      tVenousWholeBlood,
      tVenousPlasma,
      tArterialWholeBlood,
      tArterialPlasma,
      tUndeterminedWholeBlood,
      tUndeterminedPlasma,
      tInterstitialFluid,
      tControlSolution
    );
    TSampleLocation = (
      slNone = 0,
      slFinger,
      slAlternateSiteTest,
      slEarlobe,
      slControlSolution,
      slSampleLocationValueNotAvailable
    );
    TSensorStatusAnnunciation = (
      ssaSensorStatusAnnunciation,
      ssaSensormalfunctionOrFaultingAtTimeOfMeasurement,
      ssaSampleSizeForBloodOrControlSolutionInsufficientAtTimeOfMeasurement,
      ssaStripInsertionError,
      ssaStripTypeIncorrectForDevice,
      ssaSensorResultHigherThanTheDeviceCanProcess,
      ssaSensorResultLowerThanTheDeviceCanProcess,
      ssaSensorTemperatureTooHighForValidTestResultAtTimeOfMeasurement,
      ssaSensorTemperatureTooLowForValidTestResultAtTimeOfMeasurement,
      ssaSensorReadInterruptedBecauseStripWasPulledTooSoonAtTimeOfMeasurement,
      ssaGeneralDeviceFaultHasOccurredInTheSensor,
      ssaTimeFaultHasOccurredInTheSensorAndTimeMayBeInaccurate
    );
    TSensorStatusAnnunciations = set of TSensorStatusAnnunciation;
  private
    function GetBaseTime: TDateTime;
    function GetGlucoseConcentration: Single;
    function GetSampleLocation: TSampleLocation;
    function GetSensorStatusAnnunciations: TSensorStatusAnnunciations;
    function GetSequenceNumber: Word;
    function GetTimeOffset: SmallInt;
    function GetType: TType;
  public
    property SequenceNumber: Word read GetSequenceNumber;
    property BaseTime: TDateTime read GetBaseTime;
    property TimeOffset: SmallInt read GetTimeOffset;
    property GlucoseConcentration: Single read GetGlucoseConcentration;
    property &Type: TType read GetType;
    property SampleLocation: TSampleLocation read GetSampleLocation;
    property SensorStatusAnnunciations: TSensorStatusAnnunciations read GetSensorStatusAnnunciations;
  end;

  TGlucoseMeasurementContext = class(TBluetoothGattCharacteristic)
  public const
    IDX_CARBOHYDRATE_ID_AND_CARBOHYDRATE_PRESENT         = 1;
    IDX_MEAL_PRESENT                                     = 2;
    IDX_TESTER_HEALTH_PRESENT                            = 4;
    IDX_EXERCISE_DURATION_AND_EXERCISE_INTENSITY_PRESENT = 8;
    IDX_MEDICATION_ID_AND_MEDICATION_PRESENT             = 16;
    IDX_MEDICATION_VALUE_UNITS                           = 32;
    IDX_HBA1C_PRESENT                                    = 64;
  public type
    TFlag = (
      fCarbohydrateIDAndCarbohydratePresent,
      fMealPresent,
      fTesterHealthPresent,
      fExerciseDurationAndExerciseIntensityPresent,
      fMedicationIDAndMedicationPresent,
      fMedicationValueUnits,
      fHbA1cPresent
    );
    TFlags = set of TFlag;
    TCarbohydrateID = (
      ciNone = 0,
      ciBreakfast,
      ciLunch,
      ciDinner,
      ciSnack,
      ciDrink,
      ciSupper,
      ciBrunch
    );
    TInformationMeal = (
      imNone = 0,
      imPreparandial,
      imPostprandial,
      imFasting,
      imCasual,
      imBedTime
    );
    TInformationTester = (
      itNone = 0,
      itSelf,
      itHealthCareProfessional,
      itLabTest,
      itTesterValueNotAvaliable = 15
    );
    TInformationHealth = (
      ihNone = 0,
      ihMinorHealthIssues,
      ihMajorHealthIssues,
      ihDuringMenses,
      ihUnderStress,
      ihNoHealthIssues,
      ihHealthValueNotAvailable = 15
    );
    TMedicationID = (
      miNone = 0,
      miRapidActingInsulin,
      miShortActingInsulin,
      miIntermediateActingInsulin,
      miLongActingInsulin,
      miPreMixedInsulin
    );
  private
    function GetCarbohydrate: Single;
    function GetCarbohydrateID: TCarbohydrateID;
    function GetExerciseDuration: Word;
    function GetExerciseIntensity: Byte;
    function GetExtenedFlasg: Byte;
    function GetFlags: TFlags;
    function GetHbA1c: Single;
    function GetHealth: TInformationHealth;
    function GetMeal: TInformationMeal;
    function GetMedication: Single;
    function GetMedicationID: TMedicationID;
    function GetSequenceNumber: Word;
    function GetTester: TInformationTester;
  public
    property Flags: TFlags read GetFlags;
    property SequenceNumber: Word read GetSequenceNumber;
    property ExtenedFlasg: Byte read GetExtenedFlasg;
    property CarbohydrateID: TCarbohydrateID read GetCarbohydrateID;
    property Carbohydrate: Single read GetCarbohydrate;
    property Meal: TInformationMeal read GetMeal;
    property Tester: TInformationTester read GetTester;
    property Health: TInformationHealth read GetHealth;
    property ExerciseDuration: Word read GetExerciseDuration;
    property ExerciseIntensity: Byte read GetExerciseIntensity;
    property MedicationID: TMedicationID read GetMedicationID;
    property Medication: Single read GetMedication;
    property HbA1c: Single read GetHbA1c;
  end;

  TGlucoseFeatureCharacteristic = class(TBluetoothGattCharacteristic)
  public const
    IDX_LOW_BATTERY_DETECTION_DURING_MEASUREMENT_SUPPORTED = 1    ;
    IDX_SENSOR_MALFUNCTION_DETECTION_SUPPORTED             = 2    ;
    IDX_SENSOR_SAMPLE_SIZE_SUPPORTED                       = 4    ;
    IDX_SENSOR_STRIP_INSERTION_ERROR_DETECTION_SUPPORTED   = 8    ;
    IDX_SENSOR_STRIP_TYPE_ERROR_DETECTION_SUPPORTED        = 16   ;
    IDX_SENSOR_RESULT_HIGH_LOW_DETECTION_SUPPORTED         = 32   ;
    IDX_SENSOR_TEMPERATURE_HIGHLOW_DETECTION_SUPPORTED     = 64   ;
    IDX_SENSOR_READINTERRUPT_DETECTION_SUPPORTED           = 128  ;
    IDX_GENERAL_DEVICE_FAULT_SUPPORTED                     = 256  ;
    IDX_TIME_FAULT_SUPPORTED                               = 512  ;
    IDX_MULTIPLE_BOND_SUPPORTED                            = 1024 ;
  public type
    TGlucoseFeature = (
      gfLowBatteryDetectionDuringMeasurementSupported,
      gfSensorMalfunctionDetectionSupported,
      gfSensorSampleSizeSupported,
      gfSensorStripInsertionErrorDetectionSupported,
      gfSensorStripTypeErrorDetectionSupported,
      gfSensorResultHighLowDetectionSupported,
      gfSensorTemperatureHighLowDetectionSupported,
      gfSensorReadInterruptDetectionSupported,
      gfGeneralDeviceFaultSupported,
      gfTimeFaultSupported,
      gfMultipleBondSupported
    );
    TGlucoseFeatureSet = set of TGlucoseFeature;
  private
    function GetAsGlucoseFeatureSet: TGlucoseFeature;
  public
    property AsGlucoseFeatureSet: TGlucoseFeature Read GetAsGlucoseFeatureSet;
  end;

  TRecordAccessControlPointCharacteristic = class(TBluetoothGattCharacteristic)
  public type
    TOpCode = (
      ocReportStoredRecords = 1,
      ocDeleteStoredRecords,
      ocAbortOperation,
      ocReportNumberOfStoredRecords,
      ocNumberOfSotredRecordsResponse,
      ocResponseCode
    );
    TOperator = (
      oNull = 0,
      oAllRecords,
      oLessThanOrEqualTo,
      oGreaterThanOrEqualTo,
      oWithinRangeOf,
      oFirstRecord,
      oLastRecord
    );
    TOperand = (
      oNotAssigned = 0,
      oFilterParameters1,
      oFilterParameters2,
      oNotInclude,
      oFilterParameters3,
      oNumberOfRecords,
      oRequestOpCodeResponseCodeValue
    );
    TOperandResponse = (
      orSuccess = 1,
      orOpCodeNotSupported,
      orInvalidOperator,
      orOperatorNotSupported,
      orInvalidOperand,
      orNoRecordsFound,
      orAbortUnsuccessful,
      orProcedureNotComplted,
      OperandNotSupported
    );
  private
    FOpCode: TOpCode;
    function GetAsOpCode: TOpCode;
    function GetAsOperator: TOperator;
    procedure SetAsOpCode(const Value: TOpCode);
    procedure SetAsOperator(const Value: TOperator);
    function GetOperand: TOperand;
    procedure SetOperand(const Value: TOperand);
    function GetAsOperandResonse: TOperandResponse;
    procedure SetAsOperandResonse(const Value: TOperandResponse);
  public
    OpCode: Byte;
    &Operator: Byte;
    Operand: Byte;

    property AsOpCode: TOpCode read GetAsOpCode Write SetAsOpCode;
    property AsOperator: TOperator read GetAsOperator Write SetAsOperator;
    property AsOperand: TOperand Read GetOperand Write SetOperand;
    property AsOperandResonse: TOperandResponse read GetAsOperandResonse write SetAsOperandResonse;
  end;

  TSystemIDCharacteristic = class(TBluetoothGattCharacteristic)
  private
    function GetAsManufactureIdentifier: Integer;
    function GetAsOrganizationallyUniqueIdentifier: Integer;
  public
    property AsManufactureIdentifier: Integer read GetAsManufactureIdentifier;
    property AsOrganizationallyUniqueIdentifier: Integer read GetAsOrganizationallyUniqueIdentifier;
  end;

  TIEEE11073_20601RegultoryCerificationDataList = class(TBluetoothGattCharacteristic)
  private
  public
    RegulatoryCertificationDataListCount: Byte;
    RegulatoryCertificationDataListLength: Byte;
    AuthorizationBody: Byte;
    AuthorizationBodyStructureType: Byte;
    AuthorizationBodyStructureLength: Byte;
    AuthorizationBodyData: TBytes;
    AuthorizationBodyDataMajorIGVersion: Byte;
    AuthorizationBodyDataMinorIGVersion: Byte;
    CertifiredDeviceClassListCount: Byte;
    CertifiredDeviceClassListLength: Byte;
    ContinuaRegulatoryStructure: Byte;
    ContinuaRegulatoryStructureLength: Byte;
    ContinuaRegulatoryStructureRegulationBitFieldType: Byte;
  end;

  TUTF8StringCharacteristic = class(TBluetoothGattCharacteristic)
  private
    function GetAsString: String;
  public
    property AsString: String read GetAsString;
  end;

  TManufactureNameStringCharacteristic = TUTF8StringCharacteristic;
  TModelNumberStringCharacteristic = TUTF8StringCharacteristic;
  TSerialNumberStringCharacteristic = TUTF8StringCharacteristic;
  THardwareRevisionStringCharacteristic = TUTF8StringCharacteristic;
  TFirmwareRevisionStringCharacteristic = TUTF8StringCharacteristic;
  TSoftwareRevisionStringCharacteristic = TUTF8StringCharacteristic;

  TPnpID = class(TBluetoothGattCharacteristic)
  public type
    TVenderIDSource = (
      BluetoothSIGAssignedCompanyIdentifierValueFromTheAssignedNumbersDocument = 1,
      USBImplementersForumAssignedVendorIDValue
    );
  private
    function GetProductID: Word;
    function GetProductVersion: Word;
    function GetVenderID: Word;
    function GetVenderIDSource: TVenderIDSource;
  public
    property VenderIDSource: TVenderIDSource read GetVenderIDSource;
    property VenderID: Word read GetVenderID;
    property ProductID: Word read GetProductID;
    property ProductVersion: Word read GetProductVersion;
  end;

implementation

{ TRecordAccessControlPointCharacteristic }

function TRecordAccessControlPointCharacteristic.GetAsOpCode: TOpCode;
begin

end;

function TRecordAccessControlPointCharacteristic.GetAsOperandResonse: TOperandResponse;
begin

end;

function TRecordAccessControlPointCharacteristic.GetAsOperator: TOperator;
begin

end;

function TRecordAccessControlPointCharacteristic.GetOperand: TOperand;
begin

end;

procedure TRecordAccessControlPointCharacteristic.SetAsOpCode(
  const Value: TOpCode);
begin

end;

procedure TRecordAccessControlPointCharacteristic.SetAsOperandResonse(
  const Value: TOperandResponse);
begin

end;

procedure TRecordAccessControlPointCharacteristic.SetAsOperator(
  const Value: TOperator);
begin

end;

procedure TRecordAccessControlPointCharacteristic.SetOperand(
  const Value: TOperand);
begin

end;

{ TGlucoseMeasurementContextCharacteristic }

function TGlucoseFeatureCharacteristic.GetAsGlucoseFeatureSet: TGlucoseFeature;
begin

end;

{ TPnpID }

function TPnpID.GetProductID: Word;
begin

end;

function TPnpID.GetProductVersion: Word;
begin

end;

function TPnpID.GetVenderID: Word;
begin

end;

function TPnpID.GetVenderIDSource: TVenderIDSource;
begin

end;

{ TSystemIDCharacteristic }

function TSystemIDCharacteristic.GetAsManufactureIdentifier: Integer;
begin

end;

function TSystemIDCharacteristic.GetAsOrganizationallyUniqueIdentifier: Integer;
begin

end;

{ TUTF8StringCharacteristic }

function TUTF8StringCharacteristic.GetAsString: String;
begin

end;

{ TGlucoseMeasurementCharacteristic }

function TGlucoseMeasurementCharacteristic.GetBaseTime: TDateTime;
begin

end;

function TGlucoseMeasurementCharacteristic.GetGlucoseConcentration: Single;
begin

end;

function TGlucoseMeasurementCharacteristic.GetSampleLocation: TSampleLocation;
begin

end;

function TGlucoseMeasurementCharacteristic.GetSensorStatusAnnunciations: TSensorStatusAnnunciations;
begin

end;

function TGlucoseMeasurementCharacteristic.GetSequenceNumber: Word;
begin

end;

function TGlucoseMeasurementCharacteristic.GetTimeOffset: SmallInt;
begin

end;

function TGlucoseMeasurementCharacteristic.GetType: TType;
begin

end;

{ TGlucoseMeasurementContext }

function TGlucoseMeasurementContext.GetCarbohydrate: Single;
begin

end;

function TGlucoseMeasurementContext.GetCarbohydrateID: TCarbohydrateID;
begin

end;

function TGlucoseMeasurementContext.GetExerciseDuration: Word;
begin

end;

function TGlucoseMeasurementContext.GetExerciseIntensity: Byte;
begin

end;

function TGlucoseMeasurementContext.GetExtenedFlasg: Byte;
begin

end;

function TGlucoseMeasurementContext.GetFlags: TFlags;
begin

end;

function TGlucoseMeasurementContext.GetHbA1c: Single;
begin

end;

function TGlucoseMeasurementContext.GetHealth: TInformationHealth;
begin

end;

function TGlucoseMeasurementContext.GetMeal: TInformationMeal;
begin

end;

function TGlucoseMeasurementContext.GetMedication: Single;
begin

end;

function TGlucoseMeasurementContext.GetMedicationID: TMedicationID;
begin

end;

function TGlucoseMeasurementContext.GetSequenceNumber: Word;
begin

end;

function TGlucoseMeasurementContext.GetTester: TInformationTester;
begin

end;

end.
