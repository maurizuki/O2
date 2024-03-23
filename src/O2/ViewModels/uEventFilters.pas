{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2024 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uEventFilters;

interface

type
  TEventFilter = class
  private
    FAll: Boolean;
    FUseParams: Boolean;
    FUseParamsForNextEvent: Boolean;
    FDate1: TDateTime;
    FDate2: TDateTime;
  public
    constructor Create; virtual;
    property All: Boolean read FAll;
    property UseParams: Boolean read FUseParams;
    property UseParamsForNextEvent: Boolean read FUseParamsForNextEvent;
    property Date1: TDateTime read FDate1;
    property Date2: TDateTime read FDate2;
  end;

  TEventFilterClass = class of TEventFilter;

  TEventFilterAllEvents = class(TEventFilter)
    constructor Create; override;
  end;

  TEventFilterCustom = class(TEventFilter)
    constructor Create; override;
  end;

  TEventFilterToday = class(TEventFilter)
    constructor Create; override;
  end;

  TEventFilterTomorrow = class(TEventFilter)
    constructor Create; override;
  end;

  TEventFilterThisWeek = class(TEventFilter)
    constructor Create; override;
  end;

  TEventFilterThisMonth = class(TEventFilter)
    constructor Create; override;
  end;

  TEventFilterThisYear = class(TEventFilter)
    constructor Create; override;
  end;

  TEventFilterNext7days = class(TEventFilter)
    constructor Create; override;
  end;

  TEventFilterNext15days = class(TEventFilter)
    constructor Create; override;
  end;

  TEventFilterNext30days = class(TEventFilter)
    constructor Create; override;
  end;

  TEventFilterNext60days = class(TEventFilter)
    constructor Create; override;
  end;

  TEventFilterNext90days = class(TEventFilter)
    constructor Create; override;
  end;

  TEventFilterNext180days = class(TEventFilter)
    constructor Create; override;
  end;

  TEventFilterNext365days = class(TEventFilter)
    constructor Create; override;
  end;

implementation

uses
  SysUtils, DateUtils;

{ TEventFilter }

constructor TEventFilter.Create;
begin
  FAll := True;
  FUseParamsForNextEvent := True;
end;

{ TEventFilterAllEvents }

constructor TEventFilterAllEvents.Create;
begin
  FUseParamsForNextEvent := True;
  FDate1 := EncodeDate(1, 1, 1);
  FDate2 := EncodeDate(9999, 12, 31);
end;

{ TEventFilterCustom }

constructor TEventFilterCustom.Create;
begin
  FUseParams := True;
  FUseParamsForNextEvent := True;
end;

{ TEventFilterToday }

constructor TEventFilterToday.Create;
begin
  FDate1 := Date;
  FDate2 := FDate1;
end;

{ TEventFilterTomorrow }

constructor TEventFilterTomorrow.Create;
begin
  FDate1 := Date + 1;
  FDate2 := FDate1;
end;

{ TEventFilterThisWeek }

constructor TEventFilterThisWeek.Create;
begin
  FDate1 := StartOfTheWeek(Date);
  FDate2 := StartOfTheDay(EndOfTheWeek(Date));
end;

{ TEventFilterThisMonth }

constructor TEventFilterThisMonth.Create;
begin
  FDate1 := StartOfTheMonth(Date);
  FDate2 := StartOfTheDay(EndOfTheMonth(Date));
end;

{ TEventFilterThisYear }

constructor TEventFilterThisYear.Create;
begin
  FDate1 := StartOfTheYear(Date);
  FDate2 := StartOfTheDay(EndOfTheYear(Date));
end;

{ TEventFilterNext7days }

constructor TEventFilterNext7days.Create;
begin
  FDate1 := Date;
  FDate2 := FDate1 + 7;
end;

{ TEventFilterNext15days }

constructor TEventFilterNext15days.Create;
begin
  FDate1 := Date;
  FDate2 := FDate1 + 15;
end;

{ TEventFilterNext30days }

constructor TEventFilterNext30days.Create;
begin
  FDate1 := Date;
  FDate2 := FDate1 + 30;
end;

{ TEventFilterNext60days }

constructor TEventFilterNext60days.Create;
begin
  FDate1 := Date;
  FDate2 := FDate1 + 60;
end;

{ TEventFilterNext90days }

constructor TEventFilterNext90days.Create;
begin
  FDate1 := Date;
  FDate2 := FDate1 + 90;
end;

{ TEventFilterNext180days }

constructor TEventFilterNext180days.Create;
begin
  FDate1 := Date;
  FDate2 := FDate1 + 180;
end;

{ TEventFilterNext365days }

constructor TEventFilterNext365days.Create;
begin
  FDate1 := Date;
  FDate2 := FDate1 + 365;
end;

end.
