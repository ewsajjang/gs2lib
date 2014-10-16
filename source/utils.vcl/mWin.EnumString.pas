unit mWin.EnumString;

interface

uses
	Winapi.ActiveX,
	System.SysUtils, System.Classes
  ;

type
	TEnumString = class(TInterfacedObject, IEnumString)
	private type
		TPointerArray = array [0 .. 0] of Pointer;
	private
		FList: TStringList;
		FIdx: Integer;
	public
		// IEnumString
		function Next(celt: Longint; out elt; pceltFetched: PLongint): HResult; stdcall;
		function Skip(celt: Longint): HResult; stdcall;
		function Reset: HResult; stdcall;
		function Clone(out enm: IEnumString): HResult; stdcall;

		constructor Create;
		destructor Destroy; override;

    property Items: TStringList read FList write FList;
	end;

implementation

uses
	System.Math
  ;

{ TEnumString }

function TEnumString.Clone(out enm: IEnumString): HResult;
begin
	Result := E_NOTIMPL;
	Pointer(enm) := nil;
end;

constructor TEnumString.Create;
begin
	inherited Create;

	FList := TStringList.Create;
	FIdx := 0;
end;

destructor TEnumString.Destroy;
begin
	FreeAndNil(FList);

	inherited;
end;

function TEnumString.Next(celt: Integer; out elt;
	pceltFetched: PLongint): HResult;
var
	i: Integer;
	LItem: String;
  LLength: Integer;
begin
	i := 0;
	while (i < celt) and (FIdx < FList.Count) do
	begin
		LItem := FList[FIdx];
    LLength := 2 * (LItem.Length + 1);
		TPointerArray(elt)[i] := CoTaskMemAlloc(LLength);
		StringToWideChar(LItem, TPointerArray(elt)[i], LLength);
		Inc(i);
		Inc(FIdx);
	end;
	if pceltFetched <> nil then
		pceltFetched^ := i;
	if i = celt then
		Result := S_OK
	else
		Result := S_FALSE;
end;

function TEnumString.Reset: HResult;
begin
	FIdx := 0;
	Result := S_OK;
end;

function TEnumString.Skip(celt: Integer): HResult;
begin
	if InRange(FIdx + celt, 0, FList.Count) then
	begin
		Inc(FIdx, celt);
		Result := S_OK;
	end
	else
	begin
		FIdx := FList.Count;
		Result := S_FALSE;
	end;
end;

end.
