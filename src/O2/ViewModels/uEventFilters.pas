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
    FStartDate: TDateTime;
    FEndDate: TDateTime;
  public
    constructor Create; virtual;
    property All: Boolean read FAll;
    property UseParams: Boolean read FUseParams;
    property UseParamsForNextEvent: Boolean read FUseParamsForNextEvent;
    property StartDate: TDateTime read FStartDate;
    property EndDate: TDateTime read FEndDate;
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
  FStartDate := EncodeDate(1, 1, 1);
  FEndDate := EncodeDate(9999, 12, 31);
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
  FStartDate := Date;
  FEndDate := FStartDate;
end;

{ TEventFilterTomorrow }

constructor TEventFilterTomorrow.Create;
begin
  FStartDate := Date + 1;
  FEndDate := FStartDate;
end;

{ TEventFilterThisWeek }

constructor TEventFilterThisWeek.Create;
begin
  FStartDate := StartOfTheWeek(Date);
  FEndDate := StartOfTheDay(EndOfTheWeek(Date));
end;

{ TEventFilterThisMonth }

constructor TEventFilterThisMonth.Create;
begin
  FStartDate := StartOfTheMonth(Date);
  FEndDate := StartOfTheDay(EndOfTheMonth(Date));
end;

{ TEventFilterThisYear }

constructor TEventFilterThisYear.Create;
begin
  FStartDate := StartOfTheYear(Date);
  FEndDate := StartOfTheDay(EndOfTheYear(Date));
end;

{ TEventFilterNext7days }

constructor TEventFilterNext7days.Create;
begin
  FStartDate := Date;
  FEndDate := FStartDate + 7;
end;

{ TEventFilterNext15days }

constructor TEventFilterNext15days.Create;
begin
  FStartDate := Date;
  FEndDate := FStartDate + 15;
end;

{ TEventFilterNext30days }

constructor TEventFilterNext30days.Create;
begin
  FStartDate := Date;
  FEndDate := FStartDate + 30;
end;

{ TEventFilterNext60days }

constructor TEventFilterNext60days.Create;
begin
  FStartDate := Date;
  FEndDate := FStartDate + 60;
end;

{ TEventFilterNext90days }

constructor TEventFilterNext90days.Create;
begin
  FStartDate := Date;
  FEndDate := FStartDate + 90;
end;

{ TEventFilterNext180days }

constructor TEventFilterNext180days.Create;
begin
  FStartDate := Date;
  FEndDate := FStartDate + 180;
end;

{ TEventFilterNext365days }

constructor TEventFilterNext365days.Create;
begin
  FStartDate := Date;
  FEndDate := FStartDate + 365;
end;

end.
