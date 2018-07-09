{***************************************************************************}
{                                                                           }
{           DateTimeHelper                                                  }
{                                                                           }
{           Copyright (C) Colin Johnsun                                     }
{                                                                           }
{           https://github.com/colinj                                       }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{ This unit is a fork of "https://github.com/colinj/TDateTimeHelper"        }
{  Modify by gomsun2@gmail.com                                              }
{                                                                           }
{***************************************************************************}

unit mDateTimeHelper;

interface

uses
  System.SysUtils, System.Types, System.DateUtils;

type
  TDateTimeHelper = record helper for TDateTime
  private
    function GetDay: Word; inline;
    function GetDate: TDateTime; inline;
    function GetDayOfWeek: Word; inline;
    function GetDayOfYear: Word; inline;
    function GetHour: Word; inline;
    function GetMillisecond: Word; inline;
    function GetMinute: Word; inline;
    function GetMonth: Word; inline;
    function GetSecond: Word; inline;
    function GetTime: TDateTime; inline;
    function GetYear: Word ; inline;
    class function GetNow: TDateTime; static; inline;
    class function GetToday: TDateTime; static; inline;
    class function GetTomorrow: TDateTime; static; inline;
    class function GetYesterDay: TDateTime; static; inline;
  public
    class function Create(const aYear, aMonth, aDay: Word): TDateTime; overload; static; inline;
    class function Create(const aYear, aMonth, aDay, aHour, aMinute, aSecond, aMillisecond: Word): TDateTime; overload; static; inline;
    class function Create(const ADate, ATime: TDateTime): TDateTime; overload; static; inline;

    class function FmtStr(const ADateStr, AFmt: String): TDateTime; static; inline;
    class function FmtSettingsStr(const ADateStr: String; AFmtSettings: TFormatSettings): TDateTime; static; inline;
    class function FromDateStr(const ADateStr, AFmt: String): TDateTime; static;

    class function FmtYMD(const YYYYMMDD: String): TDateTime; overload; static; inline;
    class function FmtYMDHN(const YYYYMMDDHHNN: String): TDateTime; overload; static; inline;
    class function FmtYMDHNS(const YYYYMMDDHHNNSS: String): TDateTime; static; inline;
    class function FmtISO8601(const AValue: String): TDateTime; overload; static; inline;

    class property Now: TDateTime read GetNow;
    class property Today: TDateTime read GetToday;
    class property Yesterday: TDateTime read GetYesterDay;
    class property Tomorrow: TDateTime read GetTomorrow;

    property Date: TDateTime read GetDate;
    property Time: TDateTime read GetTime;

    property DayOfWeek: Word read GetDayOfWeek;
    property DayOfYear: Word read GetDayOfYear;

    property Year: Word read GetYear;
    property Month: Word read GetMonth;
    property Day: Word read GetDay;
    property Hour: Word read GetHour;
    property Minute: Word read GetMinute;
    property Second: Word read GetSecond;
    property Millisecond: Word read GetMillisecond;

    function ToString(const aFormatStr: string = ''): string; overload; inline;
    function ToShotString: String;
    function ToString(const AFmt: TFormatSettings; const aFormatStr: string = ''): string; overload; inline;
    function ToISO8601Str: String;

    function StartOfYear: TDateTime; inline;
    function EndOfYear: TDateTime; inline;
    function StartOfMonth: TDateTime; inline;
    function EndOfMonth: TDateTime; inline;
    function StartOfWeek: TDateTime; inline;
    function EndOfWeek: TDateTime; inline;
    function StartOfDay: TDateTime; inline;
    function EndOfDay: TDateTime; inline;

    function AddYears(const aNumberOfYears: Integer = 1): TDateTime; inline;
    function AddMonths(const aNumberOfMonths: Integer = 1): TDateTime; inline;
    function AddDays(const aNumberOfDays: Integer = 1): TDateTime; inline;
    function AddHours(const aNumberOfHours: Int64 = 1): TDateTime; inline;
    function AddMinutes(const aNumberOfMinutes: Int64 = 1): TDateTime; inline;
    function AddSeconds(const aNumberOfSeconds: Int64 = 1): TDateTime; inline;
    function AddMilliseconds(const aNumberOfMilliseconds: Int64 = 1): TDateTime; inline;

    function CompareTo(const aDateTime: TDateTime): TValueRelationship; inline;
    function Equals(const aDateTime: TDateTime): Boolean; inline;
    function IsSameDay(const aDateTime: TDateTime): Boolean; inline;
    function InRange(const aStartDateTime, aEndDateTime: TDateTime; const aInclusive: Boolean = True): Boolean; inline;
    function IsInLeapYear: Boolean; inline;
    function IsToday: Boolean; inline;
    function IsAM: Boolean; inline;
    function IsPM: Boolean; inline;

    function YearsBetween(const aDateTime: TDateTime): Integer; inline;
    function MonthsBetween(const aDateTime: TDateTime): Integer; inline;
    function WeeksBetween(const aDateTime: TDateTime): Integer; inline;
    function DaysBetween(const aDateTime: TDateTime): Integer; inline;
    function HoursBetween(const aDateTime: TDateTime): Int64; inline;
    function MinutesBetween(const aDateTime: TDateTime): Int64; inline;
    function SecondsBetween(const aDateTime: TDateTime): Int64; inline;
    function MilliSecondsBetween(const aDateTime: TDateTime): Int64; inline;

    function WithinYears(const aDateTime: TDateTime; const aYears: Integer): Boolean; inline;
    function WithinMonths(const aDateTime: TDateTime; const aMonths: Integer): Boolean; inline;
    function WithinWeeks(const aDateTime: TDateTime; const aWeeks: Integer): Boolean; inline;
    function WithinDays(const aDateTime: TDateTime; const aDays: Integer): Boolean; inline;
    function WithinHours(const aDateTime: TDateTime; const aHours: Int64): Boolean; inline;
    function WithinMinutes(const aDateTime: TDateTime; const aMinutes: Int64): Boolean; inline;
    function WithinSeconds(const aDateTime: TDateTime; const aSeconds: Int64): Boolean; inline;
    function WithinMilliseconds(const aDateTime: TDateTime; const AMilliseconds: Int64): Boolean; inline;

    function WeekOfTheYear: Word;
    function WeekOfTheMonth: Word;
  end;

implementation

const
  FMT_DATE_TIME_ISO_8601 = 'YYYY-MM-DD HH:NN:SS.ZZZ';
  LEN_DATE_TIME_ISO_8601 = Length(FMT_DATE_TIME_ISO_8601);

{ TDateTimeHelper }

function TDateTimeHelper.AddDays(const aNumberOfDays: Integer): TDateTime;
begin
  Result := IncDay(Self, aNumberOfDays);
end;

function TDateTimeHelper.AddHours(const aNumberOfHours: Int64): TDateTime;
begin
  Result := IncHour(Self, aNumberOfHours);
end;

function TDateTimeHelper.AddMilliseconds(const aNumberOfMilliseconds: Int64): TDateTime;
begin
  Result := IncMilliSecond(Self, aNumberOfMilliseconds);
end;

function TDateTimeHelper.AddMinutes(const aNumberOfMinutes: Int64): TDateTime;
begin
  Result := IncMinute(Self, aNumberOfMinutes);
end;

function TDateTimeHelper.AddMonths(const aNumberOfMonths: Integer): TDateTime;
begin
  Result := IncMonth(Self, aNumberOfMonths);
end;

function TDateTimeHelper.AddSeconds(const aNumberOfSeconds: Int64): TDateTime;
begin
  Result := IncSecond(Self, aNumberOfSeconds);
end;

function TDateTimeHelper.AddYears(const aNumberOfYears: Integer): TDateTime;
begin
  Result := IncYear(Self, aNumberOfYears);
end;

function TDateTimeHelper.CompareTo(const aDateTime: TDateTime): TValueRelationship;
begin
  Result := CompareDateTime(Self, aDateTime);
end;

class function TDateTimeHelper.Create(const aYear, aMonth,
  aDay: Word): TDateTime;
begin
  Result := EncodeDate(aYear, aMonth, aDay);
end;

class function TDateTimeHelper.Create(const aYear, aMonth, aDay, aHour, aMinute,
  aSecond, aMillisecond: Word): TDateTime;
begin
  Result := EncodeDateTime(aYear, aMonth, aDay, aHour, aMinute, aSecond, aMillisecond);
end;

function TDateTimeHelper.DaysBetween(const aDateTime: TDateTime): Integer;
begin
  Result := System.DateUtils.DaysBetween(Self, aDateTime);
end;

function TDateTimeHelper.EndOfDay: TDateTime;
begin
  Result := EndOfTheDay(Self);
end;

function TDateTimeHelper.EndOfMonth: TDateTime;
begin
  Result := EndOfTheMonth(Self);
end;

function TDateTimeHelper.EndOfWeek: TDateTime;
begin
  Result := EndOfTheWeek(Self);
end;

function TDateTimeHelper.EndOfYear: TDateTime;
begin
  Result := EndOfTheYear(Self);
end;

function TDateTimeHelper.Equals(const aDateTime: TDateTime): Boolean;
begin
  Result := SameDateTime(Self, aDateTime);
end;

function TDateTimeHelper.ToISO8601Str: String;
begin
  Result := FormatDateTime(FMT_DATE_TIME_ISO_8601, Self);
end;

function TDateTimeHelper.ToShotString: String;
begin
  Result := FormatDateTime('c', Self);
end;

function TDateTimeHelper.ToString(const AFmt: TFormatSettings;
  const aFormatStr: string): string;
begin
  if aFormatStr = '' then
    Result := DateTimeToStr(Self, AFmt)
  else
    Result := FormatDateTime(aFormatStr, Self, AFmt);
end;

function TDateTimeHelper.GetDate: TDateTime;
begin
  Result := DateOf(Self);
end;

function TDateTimeHelper.GetDay: Word;
begin
  Result := DayOf(Self);
end;

function TDateTimeHelper.GetDayOfWeek: Word;
begin
  Result := DayOfTheWeek(Self);
end;

function TDateTimeHelper.GetDayOfYear: Word;
begin
  Result := DayOfTheYear(Self);
end;

function TDateTimeHelper.GetHour: Word;
begin
  Result := HourOf(Self);
end;

function TDateTimeHelper.GetMillisecond: Word;
begin
  Result := MilliSecondOf(Self);
end;

function TDateTimeHelper.GetMinute: Word;
begin
  Result := MinuteOf(Self);
end;

function TDateTimeHelper.GetMonth: Word;
begin
  Result := MonthOf(Self);
end;

class function TDateTimeHelper.GetNow: TDateTime;
begin
  Result := System.SysUtils.Now;
end;

function TDateTimeHelper.GetSecond: Word;
begin
  Result := SecondOf(Self);
end;

function TDateTimeHelper.GetTime: TDateTime;
begin
  Result := TimeOf(Self);
end;

class function TDateTimeHelper.GetToday: TDateTime;
begin
  Result := System.SysUtils.Date;
end;

class function TDateTimeHelper.GetTomorrow: TDateTime;
begin
  Result := System.SysUtils.Date + 1;
end;

function TDateTimeHelper.GetYear: Word;
begin
  Result := YearOf(Self);
end;

class function TDateTimeHelper.GetYesterDay: TDateTime;
begin
  Result := System.SysUtils.Date - 1;
end;

function TDateTimeHelper.HoursBetween(const aDateTime: TDateTime): Int64;
begin
  Result := System.DateUtils.HoursBetween(Self, aDateTime);
end;

function TDateTimeHelper.InRange(const aStartDateTime, aEndDateTime: TDateTime; const aInclusive: Boolean): Boolean;
begin
  Result := DateTimeInRange(Self, aStartDateTime, aEndDateTime, aInclusive);
end;

function TDateTimeHelper.IsAM: Boolean;
begin
  Result := System.DateUtils.IsAM(Self);
end;

function TDateTimeHelper.IsInLeapYear: Boolean;
begin
  Result := System.DateUtils.IsInLeapYear(Self);
end;

function TDateTimeHelper.IsPM: Boolean;
begin
  Result := System.DateUtils.IsPM(Self);
end;

function TDateTimeHelper.IsSameDay(const aDateTime: TDateTime): Boolean;
begin
  Result := System.DateUtils.IsSameDay(Self, aDateTime);
end;

function TDateTimeHelper.IsToday: Boolean;
begin
  Result := System.DateUtils.IsToday(Self);
end;

function TDateTimeHelper.MilliSecondsBetween(const aDateTime: TDateTime): Int64;
begin
  Result := System.DateUtils.MilliSecondsBetween(Self, aDateTime);
end;

function TDateTimeHelper.MinutesBetween(const aDateTime: TDateTime): Int64;
begin
  Result := System.DateUtils.MinutesBetween(Self, aDateTime);
end;

function TDateTimeHelper.MonthsBetween(const aDateTime: TDateTime): Integer;
begin
  Result := System.DateUtils.MonthsBetween(Self, aDateTime);
end;

function TDateTimeHelper.SecondsBetween(const aDateTime: TDateTime): Int64;
begin
  Result := System.DateUtils.SecondsBetween(Self, aDateTime);
end;

function TDateTimeHelper.StartOfDay: TDateTime;
begin
  Result := StartOfTheDay(Self);
end;

function TDateTimeHelper.StartOfMonth: TDateTime;
begin
  Result := StartOfTheMonth(Self);
end;

function TDateTimeHelper.StartOfWeek: TDateTime;
begin
  Result := StartOfTheWeek(Self);
end;

function TDateTimeHelper.StartOfYear: TDateTime;
begin
  Result := StartOfTheYear(Self);
end;

function TDateTimeHelper.ToString(const aFormatStr: string): string;
begin
  if aFormatStr = '' then
    Result := DateTimeToStr(Self)
  else
    Result := FormatDateTime(aFormatStr, Self);
end;

function TDateTimeHelper.WeekOfTheMonth: Word;
var
  y, m: word;
begin
  y := Year;
  m := Month;
  Result := System.DateUtils.WeekOfTheMonth(Self, y, m);
end;

function TDateTimeHelper.WeekOfTheYear: Word;
var
  y: word;
begin
  y := Year;
  Result := System.DateUtils.WeekOfTheYear(Self, y);
end;

function TDateTimeHelper.WeeksBetween(const aDateTime: TDateTime): Integer;
begin
  Result := System.DateUtils.WeeksBetween(Self, aDateTime);
end;

function TDateTimeHelper.WithinDays(const aDateTime: TDateTime;
  const aDays: Integer): Boolean;
begin
  Result := System.DateUtils.WithinPastDays(Self, aDateTime, aDays);
end;

function TDateTimeHelper.WithinHours(const aDateTime: TDateTime;
  const aHours: Int64): Boolean;
begin
  Result := System.DateUtils.WithinPastHours(Self, aDateTime, aHours);
end;

function TDateTimeHelper.WithinMilliseconds(const aDateTime: TDateTime;
  const AMilliseconds: Int64): Boolean;
begin
  Result := System.DateUtils.WithinPastMilliSeconds(Self, aDateTime, AMilliseconds);
end;

function TDateTimeHelper.WithinMinutes(const aDateTime: TDateTime;
  const aMinutes: Int64): Boolean;
begin
  Result := System.DateUtils.WithinPastMinutes(Self, aDateTime, aMinutes);
end;

function TDateTimeHelper.WithinMonths(const aDateTime: TDateTime;
  const aMonths: Integer): Boolean;
begin
  Result := System.DateUtils.WithinPastMonths(Self, aDateTime, aMonths);
end;

function TDateTimeHelper.WithinSeconds(const aDateTime: TDateTime;
  const aSeconds: Int64): Boolean;
begin
  Result := System.DateUtils.WithinPastSeconds(Self, aDateTime, aSeconds);
end;

function TDateTimeHelper.WithinWeeks(const aDateTime: TDateTime;
  const aWeeks: Integer): Boolean;
begin
  Result := System.DateUtils.WithinPastWeeks(Self, aDateTime, aWeeks);
end;

function TDateTimeHelper.WithinYears(const aDateTime: TDateTime;
  const aYears: Integer): Boolean;
begin
  Result := System.DateUtils.WithinPastYears(Self, aDateTime, aYears);
end;

function TDateTimeHelper.YearsBetween(const aDateTime: TDateTime): Integer;
begin
  Result := System.DateUtils.YearsBetween(Self, aDateTime);
end;

class function TDateTimeHelper.Create(const ADate,
  ATime: TDateTime): TDateTime;
var
  LDT: String;
begin
  LDT := ADate.ToString('YYYYMMDD') + ATime.ToString('HHNN');
  Result := FmtYMDHN(LDT);
end;

class function TDateTimeHelper.FmtYMD(const YYYYMMDD: String): TDateTime;
var
  Y, M, D: Word;
begin
  Y := StrToInt(Copy(YYYYMMDD, 1, 4));
  M := StrToInt(Copy(YYYYMMDD, 5, 2));
  D := StrToInt(Copy(YYYYMMDD, 7, 2));

  Result := EncodeDate(Y, M, D);
end;

class function TDateTimeHelper.FmtYMDHN(const YYYYMMDDHHNN: String): TDateTime;
var
  Y, M, D, H, N: Word;
begin
  Y := StrToInt(Copy(YYYYMMDDHHNN, 1, 4));
  M := StrToInt(Copy(YYYYMMDDHHNN, 5, 2));
  D := StrToInt(Copy(YYYYMMDDHHNN, 7, 2));
  H := StrToInt(Copy(YYYYMMDDHHNN, 9, 2));
  N := StrToInt(Copy(YYYYMMDDHHNN, 11, 2));

  Result :=   EncodeDateTime(Y, M, D, H, N, 0, 0);
end;

class function TDateTimeHelper.FmtYMDHNS(
  const YYYYMMDDHHNNSS: String): TDateTime;
var
  Y, M, D, H, N, S: Word;
begin
  Y := StrToInt(Copy(YYYYMMDDHHNNSS, 1, 4));
  M := StrToInt(Copy(YYYYMMDDHHNNSS, 5, 2));
  D := StrToInt(Copy(YYYYMMDDHHNNSS, 7, 2));
  H := StrToInt(Copy(YYYYMMDDHHNNSS, 9, 2));
  N := StrToIntDef(Copy(YYYYMMDDHHNNSS, 11, 2), 0);
  S := StrToIntDef(Copy(YYYYMMDDHHNNSS, 13, 2), 0);

  Result :=   EncodeDateTime(Y, M, D, H, N, S, 0);
end;

class function TDateTimeHelper.FromDateStr(const ADateStr,
  AFmt: String): TDateTime;
const
  YYYY = 'yyyy'; MM = 'MM'; DD = 'dd';
var
  LDate: String;
  Y, M, D: Integer;
begin
  Y := AFmt.IndexOf(YYYY);
  M := AFmt.IndexOf(MM);
  D := AFmt.IndexOf(DD);
  LDate := Format('%s%s%s0000', [ADateStr.Substring(Y, YYYY.Length)
                                 , ADateStr.Substring(M, MM.Length)
                                 , ADateStr.Substring(D, DD.Length)]);
  Result := TDateTime.FmtYMDHN(LDate);
end;

class function TDateTimeHelper.FmtISO8601(const AValue: String): TDateTime;
var
  y, m, d, h, n, s, z: word;
begin
  //12345678901234567890123
  //YYYY-MM-DD HH:NN:SS.ZZZ
  try
    Y := StrToInt(Copy(AValue, 1, 4));
    M := StrToInt(Copy(AValue, 6, 2));
    D := StrToInt(Copy(AValue, 9, 2));
    H := StrToInt(Copy(AValue, 12, 2));
    N := StrToInt(Copy(AValue, 15, 2));
    S := StrToInt(Copy(AValue, 18, 2));
    Z := StrToInt(Copy(AValue, 21, 3));
    Result :=   EncodeDateTime(Y, M, D, H, N, S, Z);
  except on E: Exception do
    Result := Now;
  end;
end;

class function TDateTimeHelper.FmtSettingsStr(const ADateStr: String;
  AFmtSettings: TFormatSettings): TDateTime;
begin
  Result := StrToDateTime(ADateStr, AFmtSettings);
end;

class function TDateTimeHelper.FmtStr(const ADateStr, AFmt: String): TDateTime;
const
  YYYY = 'yyyy'; MM = 'MM'; DD = 'dd';
var
  LDate: String;
  Y, M, D: Integer;
begin
  Y := AFmt.IndexOf(YYYY);
  M := AFmt.IndexOf(MM);
  D := AFmt.IndexOf(DD);
  LDate := Format('%s%s%s0000', [ADateStr.Substring(Y, YYYY.Length)
                                 , ADateStr.Substring(M, MM.Length)
                                 , ADateStr.Substring(D, DD.Length)]);
  Result := TDateTime.FmtYMDHN(LDate);
end;

end.