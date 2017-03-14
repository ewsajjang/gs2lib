unit mExceptions;

interface

uses
  System.Classes, System.SysUtils
  ;

type
  EAbstractException = class(Exception)
    constructor Create(const Msg: String); overload; virtual;
    constructor CreateFmt(const Msg: String; Args: array of const); overload; virtual;
  end;
  ELogical = class(EAbstractException);
  ELogicalError = ELogical;

implementation

{ EAbstractException }

constructor EAbstractException.Create(const Msg: String);
begin
  inherited Create(Msg);
end;

constructor EAbstractException.CreateFmt(const Msg: String; Args: array of const);
begin
  inherited CreateFmt(Msg, Args);
end;

end.
