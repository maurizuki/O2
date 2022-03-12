unit uCtrlHelpers;

interface

uses
  Controls, Menus;

type
  TControlHelper = class helper for TControl
  public
    procedure DropDown(APopupMenu: TPopupMenu);
  end;

implementation

uses
  Types;

{ TControlHelper }

procedure TControlHelper.DropDown(APopupMenu: TPopupMenu);
var
  P: TPoint;
begin
  P := Self.Parent.ClientToScreen(Point(Self.Left, Self.Top + Self.Height));
  APopupMenu.Popup(P.X, P.Y);
end;

end.
