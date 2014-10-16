unit mWin.AutoComplete;

interface

uses
	mWin.EnumString,
  System.SysUtils, System.Classes,
	Winapi.Windows, Winapi.Messages, Winapi.ActiveX, Winapi.ShlObj,
  System.Win.ComObj,
  Vcl.Controls;

type
	TAutoCompleteOption = (acAutoAppend, acAutoSuggest, acUseArrowKey);
	TAutoCompleteOptions = set of TAutoCompleteOption;
  TAutoCompleteSource = (acsList, acsHistory, acsMRU, acsShell);

  TAutoComplete = class(TWinControl)
  private
  	FEnabled: Boolean;
		FEnumStrs: TEnumString;
  	FAutoComplete: IAutoComplete2;
    FReadyToUse: Boolean;
    FSource: TAutoCompleteSource;
    FOptions: TAutoCompleteOptions;
    procedure SetItems(const Value: TStringList);
    procedure SetOptions(const Value: TAutoCompleteOptions);
    procedure SetSource(const Value: TAutoCompleteSource);
    function GetItems: TStringList;
  protected
  	function GetEnabled: Boolean; override;
    procedure SetEnabled(Value: Boolean); override;
  public
  	constructor Create; reintroduce;
    destructor Destroy; override;

    procedure WinCtrlAssign(const ACtrl: TWinControl);

    property ReadyToUse: Boolean read FReadyToUse;

		property Options: TAutoCompleteOptions read FOptions write SetOptions;
		property Source: TAutoCompleteSource read FSource write SetSource;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Items: TStringList read GetItems write SetItems;
  end;

implementation

{ TAutoComplete }

procedure TAutoComplete.WinCtrlAssign(const ACtrl: TWinControl);
var
	LEnumStrs: IEnumString;
begin
  case FSource of
    acsHistory: LEnumStrs := CreateComObject(CLSID_ACLHistory) as IEnumString;
    acsMRU: LEnumStrs := CreateComObject(CLSID_ACLMRU) as IEnumString;
    acsShell: LEnumStrs := CreateComObject(CLSID_ACListISF) as IEnumString;
    else LEnumStrs := FEnumStrs as IEnumString;
  end;
  if FAutoComplete.Init(ACtrl.Handle, LEnumStrs, nil, nil) = S_OK then
  begin
    Enabled := FEnabled;
    Options := FOptions;
  end;
end;

constructor TAutoComplete.Create;
var
	LAutoComplete: IUnknown;
begin
	inherited Create(nil);

  FReadyToUse := False;
  	try
      LAutoComplete := CreateComObject(CLSID_AutoComplete);
      if Assigned(LAutoComplete) then
        FReadyToUse := LAutoComplete.QueryInterface(IID_IAutoComplete, FAutoComplete) = S_OK;
    except on E: Exception do
    end;
  if FReadyToUse then
  	FEnumStrs := TEnumString.Create;
end;

destructor TAutoComplete.Destroy;
begin
	if Assigned(FEnumStrs) then
  	FEnumStrs := nil;
	if Assigned(FAutoComplete) and ReadyToUse then
	begin
  	Enabled := False;
		FAutoComplete := nil;
	end;

  inherited;
end;

function TAutoComplete.GetEnabled: Boolean;
begin
	Result := FEnabled;
end;

function TAutoComplete.GetItems: TStringList;
begin
	Result := FEnumStrs.Items
end;

procedure TAutoComplete.SetEnabled(Value: Boolean);
begin
	if FEnabled <> Value then
  begin
  	FEnabled := Value;
	  FAutoComplete.Enable(FEnabled);
  end;
end;

procedure TAutoComplete.SetItems(const Value: TStringList);
begin
	FEnumStrs.Items.Assign(Value);
end;

procedure TAutoComplete.SetOptions(const Value: TAutoCompleteOptions);
const
	IAUTOCOMPLETE_SUPPORT_OPT: array [TAutoCompleteOption] of Cardinal = (
  	ACO_AUTOAPPEND,
    ACO_AUTOSUGGEST,
		ACO_UPDOWNKEYDROPSLIST
  );
var
	LOption: TAutoCompleteOption;
	LFlag: Cardinal;
	LAC2: IAutoComplete2;
begin
	if Assigned(FAutoComplete) then
		if FAutoComplete.QueryInterface(IID_IAutoComplete2, LAC2) = S_OK then
		begin
			LFlag := ACO_NONE;
			for LOption := Low(IAUTOCOMPLETE_SUPPORT_OPT) to High(IAUTOCOMPLETE_SUPPORT_OPT) do
				if LOption in FOptions then
					LFlag := LFlag or IAUTOCOMPLETE_SUPPORT_OPT[LOption];
			LAC2.SetOptions(LFlag);
		end;
	if FOptions <> Value then
  	FOptions := Value;
end;

procedure TAutoComplete.SetSource(const Value: TAutoCompleteSource);
begin
	if FSource <> Value then
  	FSource := Value;
end;

end.
