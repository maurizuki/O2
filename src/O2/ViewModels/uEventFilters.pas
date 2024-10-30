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

uses
  uServices;

type
  TEventFilter = class
  private
    FAll: Boolean;
    FUseParams: Boolean;
    FUseParamsForNextEvent: Boolean;
    FStartDate: TDateTime;
    FEndDate: TDateTime;
  public
    constructor Create(DateProvider: IDateProvider); virtual;
    property All: Boolean read FAll;
    property UseParams: Boolean read FUseParams;
    property UseParamsForNextEvent: Boolean read FUseParamsForNextEvent;
    property StartDate: TDateTime read FStartDate;
    property EndDate: TDateTime read FEndDate;
  end;

  TEventFilterClass = class of TEventFilter;

  TEventFilterAllEvents = class(TEventFilter)
    constructor Create(DateProvider: IDateProvider); override;
  end;

  TEventFilterCustom = class(TEventFilter)
    constructor Create(DateProvider: IDateProvider); override;
  end;

  TEventFilterToday = class(TEventFilter)
    constructor Create(DateProvider: IDateProvider); override;
  end;

  TEventFilterTomorrow = class(TEventFilter)
    constructor Create(DateProvider: IDateProvider); override;
  end;

  TEventFilterThisWeek = class(TEventFilter)
    constructor Create(DateProvider: IDateProvider); override;
  end;

  TEventFilterThisMonth = class(TEventFilter)
    constructor Create(DateProvider: IDateProvider); override;
  end;

  TEventFilterThisYear = class(TEventFilter)
    constructor Create(DateProvider: IDateProvider); override;
  end;

  TEventFilterNext7days = class(TEventFilter)
    constructor Create(DateProvider: IDateProvider); override;
  end;

  TEventFilterNext15days = class(TEventFilter)
    constructor Create(DateProvider: IDateProvider); override;
  end;

  TEventFilterNext30days = class(TEventFilter)
    constructor Create(DateProvider: IDateProvider); override;
  end;

  TEventFilterNext60days = class(TEventFilter)
    constructor Create(DateProvider: IDateProvider); override;
  end;

  TEventFilterNext90days = class(TEventFilter)
    constructor Create(DateProvider: IDateProvider); override;
  end;

  TEventFilterNext180days = class(TEventFilter)
    constructor Create(DateProvider: IDateProvider); override;
  end;

  TEventFilterNext365days = class(TEventFilter)
    constructor Create(DateProvider: IDateProvider); override;
  end;

implementation

uses
  SysUtils, DateUtils;

{ TEventFilter }

constructor TEventFilter.Create(DateProvider: IDateProvider);
begin
  FAll := True;
  FUseParamsForNextEvent := True;
end;

{ TEventFilterAllEvents }

constructor TEventFilterAllEvents.Create(DateProvider: IDateProvider);
begin
  FUseParamsForNextEvent := True;
  FStartDate := EncodeDate(1, 1, 1);
  FEndDate := EncodeDate(9999, 12, 31);
end;

{ TEventFilterCustom }

constructor TEventFilterCustom.Create(DateProvider: IDateProvider);
begin
  FUseParams := True;
  FUseParamsForNextEvent := True;
end;

{ TEventFilterToday }

constructor TEventFilterToday.Create(DateProvider: IDateProvider);
begin
  FStartDate := DateProvider.GetDate;
  FEndDate := FStartDate;
end;

{ TEventFilterTomorrow }

constructor TEventFilterTomorrow.Create(DateProvider: IDateProvider);
begin
  FStartDate := DateProvider.GetDate + 1;
  FEndDate := FStartDate;
end;

{ TEventFilterThisWeek }

constructor TEventFilterThisWeek.Create(DateProvider: IDateProvider);
begin
  FStartDate := StartOfTheWeek(DateProvider.GetDate);
  FEndDate := StartOfTheDay(EndOfTheWeek(DateProvider.GetDate));
end;

{ TEventFilterThisMonth }

constructor TEventFilterThisMonth.Create(DateProvider: IDateProvider);
begin
  FStartDate := StartOfTheMonth(DateProvider.GetDate);
  FEndDate := StartOfTheDay(EndOfTheMonth(DateProvider.GetDate));
end;

{ TEventFilterThisYear }

constructor TEventFilterThisYear.Create(DateProvider: IDateProvider);
begin
  FStartDate := StartOfTheYear(DateProvider.GetDate);
  FEndDate := StartOfTheDay(EndOfTheYear(DateProvider.GetDate));
end;

{ TEventFilterNext7days }

constructor TEventFilterNext7days.Create(DateProvider: IDateProvider);
begin
  FStartDate := DateProvider.GetDate;
  FEndDate := FStartDate + 7;
end;

{ TEventFilterNext15days }

constructor TEventFilterNext15days.Create(DateProvider: IDateProvider);
begin
  FStartDate := DateProvider.GetDate;
  FEndDate := FStartDate + 15;
end;

{ TEventFilterNext30days }

constructor TEventFilterNext30days.Create(DateProvider: IDateProvider);
begin
  FStartDate := DateProvider.GetDate;
  FEndDate := FStartDate + 30;
end;

{ TEventFilterNext60days }

constructor TEventFilterNext60days.Create(DateProvider: IDateProvider);
begin
  FStartDate := DateProvider.GetDate;
  FEndDate := FStartDate + 60;
end;

{ TEventFilterNext90days }

constructor TEventFilterNext90days.Create(DateProvider: IDateProvider);
begin
  FStartDate := DateProvider.GetDate;
  FEndDate := FStartDate + 90;
end;

{ TEventFilterNext180days }

constructor TEventFilterNext180days.Create(DateProvider: IDateProvider);
begin
  FStartDate := DateProvider.GetDate;
  FEndDate := FStartDate + 180;
end;

{ TEventFilterNext365days }

constructor TEventFilterNext365days.Create(DateProvider: IDateProvider);
begin
  FStartDate := DateProvider.GetDate;
  FEndDate := FStartDate + 365;
end;

end.
