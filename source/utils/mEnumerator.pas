unit mEnumerator;

interface

type
  IEnumerator<T> = interface
    function GetCurrent: T;
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

  TEnumerator<T> = class(TInterfacedObject, IEnumerator<T>)
    function GetCurrent: T; virtual; abstract;
    function MoveNext: Boolean; virtual; abstract;
    property Current: T read GetCurrent;
  end;

  IEnumerable<T> = interface
    function GetEnumerator: IEnumerator<T>;
  end;

  TEnumerable<T> = class(TInterfacedObject, IEnumerable<T>)
    function GetEnumerator: IEnumerator<T>; virtual; abstract;
  end;

implementation

end.
