{
  see - http://delphihaven.wordpress.com/2011/01/22/tip-detecting-graphic-formats/
}
unit mBitmapHelper;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Graphics, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, Vcl.Imaging.GIFImg;

type
  EBitmapHelper = class(Exception);
  EFileNotExists = class(EBitmapHelper);
  EStreamSizeIsNotEnough = class(EBitmapHelper);

  TBitmapHelper = class helper for TBitmap
  private
  public
    function IsGraphicStreamEnough(const AStream: TStream): Boolean;
    function IsGraphicStream(const AStream: TStream): Boolean;
    function LoadFromUnknowFile(const AFileName: String): Boolean;
    function LoadFromUnknownStream(AStream: TStream): Boolean;
    function TextWidth(const AValue: String): Integer;
    function TextHeight(const AValue: String): Integer;
  end;

function FindGraphicClass(const ABuffer; const ABufferSize: Int64;
  out AGraphicClass: TGraphicClass): Boolean; overload;
function FindGraphicClass(Stream: TStream;
  out GraphicClass: TGraphicClass): Boolean; overload;

implementation

uses
  System.IOUtils;

const
    MIN_GRAPHIC_SIZE = 44;

function FindGraphicClass(const ABuffer; const ABufferSize: Int64;
  out AGraphicClass: TGraphicClass): Boolean;
var
  LLongWords: array[Byte] of LongWord absolute ABuffer;
  LWords: array[Byte] of word absolute ABuffer;
begin
  Result := False;
  AGraphicClass := nil;
  if ABufferSize < MIN_GRAPHIC_SIZE then
    Exit;

  case LWords[0] of
    $4D42: AGraphicClass := TBitmap;

    $D8FF: AGraphicClass := TJPEGImage;

    $4949: if LWords[1] = $002A then AGraphicClass := TWicImage; //i.e., TIFF

    $4D4D: if LWords[1] = $2A00 then AGraphicClass := TWicImage; //i.e., TIFF

    else if Int64(ABuffer) = $A1A0A0D474E5089 then
        AGraphicClass := TPNGImage
    else if LLongWords[0] = $9AC6CDD7 then
      AGraphicClass := TMetafile
    else if (LLongWords[0] = 1) and (LLongWords[10] = $464D4520) then
      AGraphicClass := TMetafile
    else if StrLComp(PAnsiChar(@ABuffer), 'GIF', 3) = 0 then
      AGraphicClass := TGIFImage
    else if LWords[1] = 1 then
      AGraphicClass := TIcon;
  end;
  Result := AGraphicClass <> nil;
end;

function FindGraphicClass(Stream: TStream;
  out GraphicClass: TGraphicClass): Boolean;
var
  LBuffer: PByte;
  LCurPos: Int64;
  LBytesRead: Integer;
begin
  if Stream is TCustomMemoryStream then
  begin
    LBuffer := TCustomMemoryStream(Stream).Memory;
    LCurPos := Stream.Position;
    Inc(LBuffer, LCurPos);
    Result := FindGraphicClass(LBuffer^, Stream.Size - LCurPos, GraphicClass);
    Exit;
  end;
  GetMem(LBuffer, MIN_GRAPHIC_SIZE);
  try
    LBytesRead := Stream.Read(LBuffer^, MIN_GRAPHIC_SIZE);
    Stream.Seek(-LBytesRead, soCurrent);
    Result := FindGraphicClass(LBuffer^, LBytesRead, GraphicClass);
  finally
    FreeMem(LBuffer);
  end;
end;

{ TBitmapHelper }

function TBitmapHelper.IsGraphicStream(const AStream: TStream): Boolean;
var
  LClass: TGraphicClass;
begin
  if not IsGraphicStreamEnough(AStream) then
    Exit(False);

  Result := FindGraphicClass(AStream, AStream.Size, LClass);
end;

function TBitmapHelper.LoadFromUnknowFile(const AFileName: String): Boolean;
var
  LStream: TMemoryStream;
begin
  if not TFile.Exists(AFileName) then
    raise EFileNotExists.CreateFmt('%s is not exists.', [AFileName]);

  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(AFileName);
    LStream.Position := 0;
    Result := LoadFromUnknownStream(LStream);
  finally
    FreeAndNil(LStream);
  end;
end;

function TBitmapHelper.LoadFromUnknownStream(AStream: TStream): Boolean;
var
  LStream: TMemoryStream;
  LGraphic: TGraphic;
  LClass: TGraphicClass;
begin
  if not IsGraphicStreamEnough(AStream) then
    raise EStreamSizeIsNotEnough.Create('AStream size is not enough');

  LStream := TMemoryStream.Create;
  try
    AStream.Position := 0;
    LStream.CopyFrom(AStream, AStream.Size);
    LStream.Position := 0;
    Result := FindGraphicClass(LStream.Memory^, LStream.Size, LClass);
    if Result then
    begin
      LGraphic := LClass.Create;
      try
        LStream.Position := 0;
        LGraphic.LoadFromStream(LStream);
        Assign(LGraphic);
      finally
        FreeAndNil(LGraphic);
      end;
    end;
  finally
    FreeAndNil(LStream);
  end;
end;

function TBitmapHelper.IsGraphicStreamEnough(const AStream: TStream): Boolean;
begin
  Result := (AStream.Size > MIN_GRAPHIC_SIZE) or Assigned(AStream)
end;

function TBitmapHelper.TextHeight(const AValue: String): Integer;
begin
  Result := Canvas.TextHeight(AValue);
end;

function TBitmapHelper.TextWidth(const AValue: String): Integer;
begin
  Result := Canvas.TextWidth(AValue);
end;

end.
