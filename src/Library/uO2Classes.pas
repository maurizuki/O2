{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2015 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uO2Classes;

interface

uses
  Classes;

type
  TO2Notification = (
    onAdded,
    onExtracting,
    onDeleting,
    onIndexChanged,
    onPropertyChanged
    );

  TO2CollectionItem = class(TCollectionItem)
  protected
    procedure SetIndex(Value: Integer); override;
    procedure NotifyChanges(Item: TO2CollectionItem;
      Action: TO2Notification); virtual;
  end;

  TO2Collection = class(TOwnedCollection)
  private
    Destroying: Boolean;
  protected
    procedure Notify(Item: TCollectionItem;
      Action: TCollectionNotification); override;
    procedure NotifyChanges(Item: TO2CollectionItem;
      Action: TO2Notification); virtual;
  public
    destructor Destroy; override;
  end;

  TO2Persistent = class(TPersistent)
  protected
    procedure NotifyChanges(Item: TO2CollectionItem;
      Action: TO2Notification); virtual; abstract;
  end;

implementation

{ TO2CollectionItem }

procedure TO2CollectionItem.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := Index;
  inherited SetIndex(Value);
  if Value <> CurIndex then
    NotifyChanges(Self, onIndexChanged);
end;

procedure TO2CollectionItem.NotifyChanges(Item: TO2CollectionItem;
  Action: TO2Notification);
begin
  if Assigned(Collection) and (Collection is TO2Collection) then
    TO2Collection(Collection).NotifyChanges(Item, Action);
end;

{ TO2Collection }

destructor TO2Collection.Destroy;
begin
  Destroying := True;
  inherited Destroy;
end;

procedure TO2Collection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
var
  O2Action: TO2Notification;
begin
  inherited Notify(Item, Action);
  if Item is TO2CollectionItem then
  begin
    case Action of
      cnAdded:
        O2Action := onAdded;
      cnExtracting:
        O2Action := onExtracting;
      cnDeleting:
        O2Action := onDeleting;
      else
        O2Action := onPropertyChanged;
    end;
    NotifyChanges(TO2CollectionItem(Item), O2Action);
  end;
end;

procedure TO2Collection.NotifyChanges(Item: TO2CollectionItem;
  Action: TO2Notification);
begin
  if Destroying or (Owner = nil) then Exit;
  if Owner is TO2Persistent then
    TO2Persistent(Owner).NotifyChanges(Item, Action)
  else if Owner is TO2CollectionItem then
    TO2CollectionItem(Owner).NotifyChanges(Item, Action);
end;

end.
