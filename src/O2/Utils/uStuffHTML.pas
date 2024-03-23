unit uStuffHTML;

interface

uses
  Windows, Variants, SysUtils, ActiveX, ComObj, SHDocVw, Classes, MSHTML;

{!~This method writes HTML code directly into the webbrowser control }
procedure StuffHTML(const Browser: IWebBrowser; const HTML: string);

implementation

type
  { TStuffHTML is used to perform the StuffHTML operation. It also implements
  the IDispatch interface to catch some WebBrowser events } 
  TStuffHTML = class(TInterfacedObject, IDispatch)
  private
    FConnection: Longint;
    FConnectionPoint: IConnectionPoint;
    FBrowser: IWebBrowser;
    FHTML: string;
    procedure DisconnectBrowserEvents;
  protected

    //IDispatch - for event catching
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount: Integer; LocaleID: Integer; DispIDs: Pointer): HRESULT;
      stdcall;
    function GetTypeInfo(Index: Integer; LocaleID: Integer;
      out TypeInfo): HRESULT; stdcall;
    function GetTypeInfoCount(out Count: Integer): HRESULT; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult: Pointer; ExcepInfo: Pointer;
      ArgErr: Pointer): HRESULT; stdcall;
  public
    constructor Create(const Browser: IWebBrowser; const HTML: string);
      reintroduce;
  end;

procedure StuffHTML(const Browser: IWebBrowser; const HTML: string);
var
  Unk: IUnknown;
begin

  { Assign an interface reference to guarantee that the object is referenced
    outside the constructor. Otherwise the whole process might occur inside
    the constructor, in which case the object won't be freed in the refcount
    reaches zero }
  Unk := TStuffHTML.Create(Browser, HTML);
end;

{ TStuffHTML }

constructor TStuffHTML.Create(const Browser: IWebBrowser;
  const HTML: string);
var
  CPC: IConnectionPointContainer;
begin
  inherited Create;

  //Save arguments for later
  FBrowser := Browser;
  FHTML := HTML;

  //Stop any possible browser activity
  FBrowser.Stop;

  { The process of loading HTML text consists of two steps:

    - First, we need the Document property of the browser to contain something.
      To ensure this, we'll navigate to "about:blank"
    - Then we use the Write method of the Document property to write the HTML
      code.

    Because navigation is an asynchronous process, we can't just perform the
    second step directly after the first, but we'll have to wait until the blank
    page has been properly loaded. We'll use the OnDocumentComplete event for
    this. Fortunately the connection point for the appropriate events interface
    supports multiple connections, so this event connection will not interfere
    with any event handlers connected by the client application }
  FConnection := 0; //Init, as long as it's zero, we have no connection

  //See MSDN documentation for the concept of connection points
  OleCheck(FBrowser.QueryInterface(IConnectionPointContainer, CPC));
{$WARN SYMBOL_PLATFORM OFF}
  OleCheck(CPC.FindConnectionPoint(DWebBrowserEvents2, FConnectionPoint));
{$WARN SYMBOL_PLATFORM ON}

  { The Advise will refcount this object. The Unadvise called in the event
    handler will unreference the object }
  OleCheck(FConnectionPoint.Advise(Self, FConnection));

  //Now start the navigation. Upon completion, we'll write in the HTML code
  FBrowser.Navigate('about:blank', EmptyParam, EmptyParam,
    EmptyParam, EmptyParam);
end;

function TStuffHTML.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HRESULT;
begin
  Result := E_NOTIMPL; 
end;

function TStuffHTML.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TStuffHTML.GetTypeInfoCount(out Count: Integer): HRESULT;
begin
  Result := E_NOTIMPL;
  Count := 0;
end;

procedure TStuffHTML.DisconnectBrowserEvents;
begin
  OleCheck(FConnectionPoint.Unadvise(FConnection));
end;

function TStuffHTML.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HRESULT;
var
  URL: WideString;
begin

  //Event handling
  case DispID of
    259: //Document Complete
      begin

        //See which URL completed
        URL := Variant(TDispParams(Params).rgvarg^[0]);
        if WideSameText(URL, 'about:blank') then
        begin

          { Write the HTML text into the browser. The Write method wants a
            safe array of HTML strings. For convenience, the 'late binding' is
            used here to call write on a string, because then the safe array
            will be built automatically }
          Variant(FBrowser.Document).Write(FHTML);
          DisconnectBrowserEvents;
        end;
      end;
    253: //Quit browser
      DisconnectBrowserEvents;
  end;
  Result := S_OK;
end;

end.
