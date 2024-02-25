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

unit uMain;

interface

{$WARN UNIT_PLATFORM OFF}

{$R Dictionaries.res}
{$R Icons.res}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, ImgList, ActnList, Menus, XPMan, AppEvnts,
  StdCtrls, ExtCtrls, FileCtrl, Types, System.ImageList, System.Actions,
  REST.Client, Data.Bind.Components, Data.Bind.ObjectScope, OleCtrls, SHDocVw,
  JvComponentBase, JvDragDrop,
  uO2File, uO2Objects, uO2Relations, uO2Rules, uGlobal, uMRUlist, uServices,
  uUtils;

type
  TCmdLineAction = (caNone, caOpenFile);

  TNotifyChange = (ncObjects, ncObjProps, ncRelations, ncRules,
    ncTagList, ncRuleList);
  TNotifyChanges = set of TNotifyChange;

  TObjectViewColumn = (ocName, ocTags, ocNextEvent);

  TMainForm = class(TForm, IPasswordProvider)
    ToolBar: TToolBar;
    StatusBar: TStatusBar;
    ActionList: TActionList;
    ToolBarImages: TImageList;
    NewFile: TControlAction;
    OpenFile: TControlAction;
    SaveFile: TControlAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolBarImagesH: TImageList;
    ToolBarImagesD: TImageList;
    View: TControlAction;
    ToolButton4: TToolButton;
    ViewMenu: TPopupMenu;
    ViewLargeIcons: TAction;
    ViewSmallIcons: TAction;
    ViewList: TAction;
    Icons1: TMenuItem;
    Smallicons1: TMenuItem;
    List1: TMenuItem;
    SaveFileAs: TAction;
    SaveMenu: TPopupMenu;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    ObjectsViewLargeImages: TImageList;
    ObjectsViewSmallImages: TImageList;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    About: TAction;
    ToolButton5: TToolButton;
    NewObject: TAction;
    DeleteObject: TAction;
    RenameObject: TAction;
    ObjectProps: TAction;
    ObjectMenu: TPopupMenu;
    Newitem1: TMenuItem;
    Deleteitem1: TMenuItem;
    Renameitem1: TMenuItem;
    Itemproperties1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    SelectAll: TAction;
    Selectall1: TMenuItem;
    N3: TMenuItem;
    XPManifest1: TXPManifest;
    ApplicationEvents: TApplicationEvents;
    InvertSelection: TAction;
    Invertselection1: TMenuItem;
    Timer: TTimer;
    OpenMenu: TPopupMenu;
    Open1: TMenuItem;
    N5: TMenuItem;
    MRU1: TMenuItem;
    MRU2: TMenuItem;
    MRU3: TMenuItem;
    MRU4: TMenuItem;
    MRU5: TMenuItem;
    MRU6: TMenuItem;
    MRU7: TMenuItem;
    MRU8: TMenuItem;
    MRU9: TMenuItem;
    PrintFile: TAction;
    ToolButton9: TToolButton;
    Tools: TControlAction;
    ToolButton10: TToolButton;
    ToolsMenu: TPopupMenu;
    PrintDialog: TPrintDialog;
    FieldMenu: TPopupMenu;
    CopyValue: TAction;
    CopyNameValueAsColumn: TAction;
    CopyNameValueAsRow: TAction;
    CopyAsRows: TAction;
    CopyAsColumns: TAction;
    Copyvaluetoclipboard1: TMenuItem;
    CopykeyandvaluetoclipboardWYSIWYG1: TMenuItem;
    Copykeyandvaluetoclipboardtable1: TMenuItem;
    CopytoclipboardWYSIWYG1: TMenuItem;
    Copytoclipboardtable1: TMenuItem;
    N4: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    ViewReport: TAction;
    Report1: TMenuItem;
    Import: TAction;
    Export: TAction;
    ImportDialog: TOpenDialog;
    ExportDialog: TSaveDialog;
    Import2: TMenuItem;
    Export2: TMenuItem;
    N8: TMenuItem;
    DuplicateObject: TAction;
    Duplicate1: TMenuItem;
    OpenLink: TAction;
    SendEmail: TAction;
    ShowPasswords: TAction;
    N9: TMenuItem;
    Browse1: TMenuItem;
    Sendemail1: TMenuItem;
    RefreshViews: TAction;
    RuleMenu: TPopupMenu;
    NewRule: TAction;
    DuplicateRule: TAction;
    DeleteRule: TAction;
    Newrule1: TMenuItem;
    N10: TMenuItem;
    Duplicate2: TMenuItem;
    Delete1: TMenuItem;
    RuleProps: TAction;
    N11: TMenuItem;
    Properties1: TMenuItem;
    DragDrop: TJvDragDrop;
    AddToIEFavorites: TAction;
    AddtoIEfavorites1: TMenuItem;
    SaveFileAsCopy: TAction;
    Saveascopy1: TMenuItem;
    ReopenFile: TAction;
    Reopen1: TMenuItem;
    Deselect: TAction;
    Deselect1: TMenuItem;
    FileProps: TAction;
    Fileproperties1: TMenuItem;
    EnableRules: TAction;
    DisableRules: TAction;
    Enableall1: TMenuItem;
    Disableall1: TMenuItem;
    N12: TMenuItem;
    ViewStayOnTop: TAction;
    N13: TMenuItem;
    Stayontop1: TMenuItem;
    ImportSettings: TAction;
    ExportSettings: TAction;
    N14: TMenuItem;
    Importsettings1: TMenuItem;
    Exportsettings1: TMenuItem;
    ImportSettingsDlg: TOpenDialog;
    ExportSettingsDlg: TSaveDialog;
    Transparency1: TMenuItem;
    Transparency0: TAction;
    Transparency10: TAction;
    N01: TMenuItem;
    N101: TMenuItem;
    Transparency20: TAction;
    Transparency30: TAction;
    N201: TMenuItem;
    N301: TMenuItem;
    Transparency40: TAction;
    Transparency50: TAction;
    N401: TMenuItem;
    N501: TMenuItem;
    Transparency60: TAction;
    Transparency70: TAction;
    Transparency80: TAction;
    Transparency90: TAction;
    N601: TMenuItem;
    N701: TMenuItem;
    N801: TMenuItem;
    N901: TMenuItem;
    CopyValuesAsRows: TAction;
    CopyValuesAsColumns: TAction;
    Copyfieldvaluesasrows1: TMenuItem;
    Copyfieldvaluesascolumns1: TMenuItem;
    N15: TMenuItem;
    InstallOnRemovableMedia: TAction;
    N16: TMenuItem;
    Installonremovablemedia1: TMenuItem;
    MoveUpRule: TAction;
    MoveDownRule: TAction;
    N17: TMenuItem;
    Moveup1: TMenuItem;
    Movedown1: TMenuItem;
    SortByName: TAction;
    SortByTags: TAction;
    Sortbyname1: TMenuItem;
    Sortbytags1: TMenuItem;
    CheckForUpdatesNow: TAction;
    Checkforupdatesnow1: TMenuItem;
    CheckForUpdatesPeriodically: TAction;
    Checkforupdatesperiodically1: TMenuItem;
    N20: TMenuItem;
    ReplaceFieldName: TAction;
    Renamefield1: TMenuItem;
    Renametag1: TMenuItem;
    ReplaceTag: TAction;
    N22: TMenuItem;
    OpenFolder: TAction;
    Openfolder1: TMenuItem;
    NewMenu: TPopupMenu;
    New1: TMenuItem;
    Newobject1: TMenuItem;
    Newrule2: TMenuItem;
    N23: TMenuItem;
    NewWindow: TAction;
    N18: TMenuItem;
    Newwindow1: TMenuItem;
    SortByNextEvent: TAction;
    Sortbynextevent1: TMenuItem;
    NewRelation: TAction;
    NewRelation1: TMenuItem;
    NewRelation2: TMenuItem;
    RelationMenu: TPopupMenu;
    DeleteRelation: TAction;
    RelationProps: TAction;
    Delete2: TMenuItem;
    N19: TMenuItem;
    Properties2: TMenuItem;
    RelationGoToObj: TAction;
    N24: TMenuItem;
    Gotoobject1: TMenuItem;
    LanguageMenu: TMenuItem;
    N25: TMenuItem;
    DefaultLanguage: TAction;
    Systemdefault1: TMenuItem;
    SelectAllRules: TAction;
    DeselectRules: TAction;
    InvertRulesSelection: TAction;
    Selectall2: TMenuItem;
    Deselect2: TMenuItem;
    Invertselection2: TMenuItem;
    N26: TMenuItem;
    ReplaceFieldValue: TAction;
    Replacefieldvalue1: TMenuItem;
    HelpMenu: TPopupMenu;
    Help: TControlAction;
    WebSite: TAction;
    WebProject: TAction;
    WebSupportRequests: TAction;
    WebFeatureRequests: TAction;
    WebBugTracker: TAction;
    About1: TMenuItem;
    Projectwebpage1: TMenuItem;
    Projectinfo1: TMenuItem;
    Supportrequests1: TMenuItem;
    Featurerequests1: TMenuItem;
    Bugtracker1: TMenuItem;
    N21: TMenuItem;
    N27: TMenuItem;
    ExportToHTML: TAction;
    ExporttoHTML1: TMenuItem;
    TransparencyOnlyIfDeactivated: TAction;
    N28: TMenuItem;
    Onlyifdeactivated1: TMenuItem;
    ClearMRUList: TAction;
    Clearrecentfileslist1: TMenuItem;
    ObjectTags: TAction;
    Edittags1: TMenuItem;
    N29: TMenuItem;
    AddTag: TMenuItem;
    DeleteTag: TMenuItem;
    pnClient: TPanel;
    ObjectsView: TListView;
    SearchBox: TScrollBox;
    VSplitter: TSplitter;
    lbFindByName: TLabel;
    FindByName: TEdit;
    lbFindByEvent: TLabel;
    FindByEvent: TComboBox;
    lbFindByTag: TLabel;
    FindByTag: TListBox;
    ToolButton6: TToolButton;
    Find: TControlAction;
    PageControl: TPageControl;
    tsFields: TTabSheet;
    tsNotes: TTabSheet;
    tsRelations: TTabSheet;
    tsRules: TTabSheet;
    FieldsView: TListView;
    NotesView: TWebBrowser;
    RelationsView: TListView;
    RulesView: TListView;
    HSplitter: TSplitter;
    SearchMenu: TPopupMenu;
    Find1: TMenuItem;
    ClearSearch: TAction;
    Clearsearch1: TMenuItem;
    lbFindByRule: TLabel;
    FindByRule: TListBox;
    FindByTagSelectAll: TAction;
    FindByTagDeselect: TAction;
    FindByTagInvertSelection: TAction;
    FindByTagMenu: TPopupMenu;
    Selectall3: TMenuItem;
    Deselect3: TMenuItem;
    Invertselection3: TMenuItem;
    FindByRuleSelectAll: TAction;
    FindByRuleDeselect: TAction;
    FindByRuleInvertSelection: TAction;
    FindByRuleMenu: TPopupMenu;
    Selectall4: TMenuItem;
    Deselect4: TMenuItem;
    Invertselection4: TMenuItem;
    ReplaceRole: TAction;
    Replacerole1: TMenuItem;
    N30: TMenuItem;
    Showpasswords1: TMenuItem;
    CheckForUpdatesRESTClient: TRESTClient;
    CheckForUpdatesRequest: TRESTRequest;
    CheckForUpdatesResponse: TRESTResponse;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ApplicationEventsActivate(Sender: TObject);
    procedure ApplicationEventsDeactivate(Sender: TObject);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure ApplicationEventsMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure ObjectsViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ObjectsViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ObjectsViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ObjectsViewDblClick(Sender: TObject);
    procedure ObjectsViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ObjectsViewEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure ObjectsViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ObjectsViewResize(Sender: TObject);
    procedure FindByNameChange(Sender: TObject);
    procedure FindByTagSelectAllExecute(Sender: TObject);
    procedure FindByTagDeselectExecute(Sender: TObject);
    procedure FindByTagInvertSelectionExecute(Sender: TObject);
    procedure FindByRuleSelectAllExecute(Sender: TObject);
    procedure FindByRuleDeselectExecute(Sender: TObject);
    procedure FindByRuleInvertSelectionExecute(Sender: TObject);
    procedure FilterChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure MRUItemClick(Sender: TObject);
    procedure LanguageClick(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    procedure NewFileExecute(Sender: TObject);
    procedure NewWindowExecute(Sender: TObject);
    procedure OpenFileExecute(Sender: TObject);
    procedure ReopenFileExecute(Sender: TObject);
    procedure ReopenFileUpdate(Sender: TObject);
    procedure OpenFolderExecute(Sender: TObject);
    procedure OpenFolderUpdate(Sender: TObject);
    procedure ClearMRUListExecute(Sender: TObject);
    procedure ClearMRUListUpdate(Sender: TObject);
    procedure ImportExecute(Sender: TObject);
    procedure SaveFileExecute(Sender: TObject);
    procedure SaveFileAsExecute(Sender: TObject);
    procedure SaveFileAsCopyExecute(Sender: TObject);
    procedure ExportExecute(Sender: TObject);
    procedure ExportToHTMLExecute(Sender: TObject);
    procedure PrintFileExecute(Sender: TObject);
    procedure PrintFileUpdate(Sender: TObject);
    procedure FilePropsExecute(Sender: TObject);
    procedure ImportSettingsExecute(Sender: TObject);
    procedure ExportSettingsExecute(Sender: TObject);
    procedure DefaultLanguageExecute(Sender: TObject);
    procedure InstallOnRemovableMediaExecute(Sender: TObject);
    procedure CheckForUpdatesNowExecute(Sender: TObject);
    procedure CheckForUpdatesPeriodicallyExecute(Sender: TObject);
    procedure CheckForUpdatesPeriodicallyUpdate(Sender: TObject);
    procedure RefreshViewsExecute(Sender: TObject);
    procedure ViewLargeIconsExecute(Sender: TObject);
    procedure ViewLargeIconsUpdate(Sender: TObject);
    procedure ViewSmallIconsExecute(Sender: TObject);
    procedure ViewSmallIconsUpdate(Sender: TObject);
    procedure ViewListExecute(Sender: TObject);
    procedure ViewListUpdate(Sender: TObject);
    procedure ViewReportExecute(Sender: TObject);
    procedure ViewReportUpdate(Sender: TObject);
    procedure SortByNameExecute(Sender: TObject);
    procedure SortByNameUpdate(Sender: TObject);
    procedure SortByTagsExecute(Sender: TObject);
    procedure SortByTagsUpdate(Sender: TObject);
    procedure SortByNextEventExecute(Sender: TObject);
    procedure SortByNextEventUpdate(Sender: TObject);
    procedure TransparencyExecute(Sender: TObject);
    procedure TransparencyUpdate(Sender: TObject);
    procedure TransparencyOnlyIfDeactivatedExecute(Sender: TObject);
    procedure TransparencyOnlyIfDeactivatedUpdate(Sender: TObject);
    procedure ViewStayOnTopExecute(Sender: TObject);
    procedure ViewStayOnTopUpdate(Sender: TObject);
    procedure FindExecute(Sender: TObject);
    procedure FindUpdate(Sender: TObject);
    procedure ClearSearchExecute(Sender: TObject);
    procedure AboutExecute(Sender: TObject);
    procedure WebExecute(Sender: TObject);
    procedure ObjectMenuPopup(Sender: TObject);
    procedure NewObjectExecute(Sender: TObject);
    procedure DuplicateObjectExecute(Sender: TObject);
    procedure DeleteObjectExecute(Sender: TObject);
    procedure RenameObjectExecute(Sender: TObject);
    procedure SelectAllExecute(Sender: TObject);
    procedure DeselectExecute(Sender: TObject);
    procedure InvertSelectionExecute(Sender: TObject);
    procedure ObjectTagsExecute(Sender: TObject);
    procedure AddTagClick(Sender: TObject);
    procedure DeleteTagClick(Sender: TObject);
    procedure ReplaceTagExecute(Sender: TObject);
    procedure ReplaceFieldNameExecute(Sender: TObject);
    procedure ReplaceFieldValueExecute(Sender: TObject);
    procedure ReplaceRoleExecute(Sender: TObject);
    procedure ObjectPropsExecute(Sender: TObject);
    procedure ObjectActionUpdate(Sender: TObject);
    procedure FieldsViewDblClick(Sender: TObject);
    procedure FieldsViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FieldsViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FieldsViewCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure FieldsViewCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure FieldsViewResize(Sender: TObject);
    procedure FieldMenuPopup(Sender: TObject);
    procedure CopyValueExecute(Sender: TObject);
    procedure CopyNameValueAsRowExecute(Sender: TObject);
    procedure CopyNameValueAsColumnExecute(Sender: TObject);
    procedure CopyNameValueUpdate(Sender: TObject);
    procedure CopyValuesAsRowsExecute(Sender: TObject);
    procedure CopyValuesAsColumnsExecute(Sender: TObject);
    procedure CopyAsRowsExecute(Sender: TObject);
    procedure CopyAsColumnsExecute(Sender: TObject);
    procedure CopyUpdate(Sender: TObject);
    procedure AddToIEFavoritesExecute(Sender: TObject);
    procedure OpenLinkExecute(Sender: TObject);
    procedure OpenLinkUpdate(Sender: TObject);
    procedure SendEmailExecute(Sender: TObject);
    procedure SendEmailUpdate(Sender: TObject);
    procedure ShowPasswordsExecute(Sender: TObject);
    procedure ShowPasswordsUpdate(Sender: TObject);
    procedure RelationsViewDblClick(Sender: TObject);
    procedure RelationsViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RelationsViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RelationsViewResize(Sender: TObject);
    procedure NewRelationExecute(Sender: TObject);
    procedure NewRelationUpdate(Sender: TObject);
    procedure DeleteRelationExecute(Sender: TObject);
    procedure RelationGoToObjExecute(Sender: TObject);
    procedure RelationPropsExecute(Sender: TObject);
    procedure RelationActionUpdate(Sender: TObject);
    procedure RulesViewDblClick(Sender: TObject);
    procedure RulesViewKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RulesViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RulesViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RulesViewResize(Sender: TObject);
    procedure NewRuleExecute(Sender: TObject);
    procedure DuplicateRuleExecute(Sender: TObject);
    procedure DeleteRuleExecute(Sender: TObject);
    procedure MoveUpRuleExecute(Sender: TObject);
    procedure MoveUpRuleUpdate(Sender: TObject);
    procedure MoveDownRuleExecute(Sender: TObject);
    procedure MoveDownRuleUpdate(Sender: TObject);
    procedure EnableRulesExecute(Sender: TObject);
    procedure DisableRulesExecute(Sender: TObject);
    procedure SelectAllRulesExecute(Sender: TObject);
    procedure DeselectRulesExecute(Sender: TObject);
    procedure InvertRulesSelectionExecute(Sender: TObject);
    procedure RulePropsExecute(Sender: TObject);
    procedure RuleActionUpdate(Sender: TObject);
    procedure SaveDialogCanClose(Sender: TObject; var CanClose: Boolean);
    procedure ExportDialogCanClose(Sender: TObject; var CanClose: Boolean);
    procedure ExportSettingsDlgCanClose(Sender: TObject;
      var CanClose: Boolean);
    procedure DragDropDrop(Sender: TObject; Pos: TPoint;
      Value: TStrings);
  private
    FFile: TO2File;
    FFileName: string;
    FSelectedObjects: IEnumerable<TO2Object>;
    FAppVersionInfo: TAppVersionInfo;
    FBusy: Boolean;
    FStayOnTop: Boolean;
    FTransparency: Integer;
    FTransparencyOnlyIfDeactivated: Boolean;
    FShowPasswords: Boolean;
    FPasswordScoreCache: IPasswordScoreCache;

    MRUMenuItems: TList;
    MRUList: TMRUList;

    CmdLineAction: TCmdLineAction;
    CmdLineFileName: string;

    BatchOperationCount: Integer;

    PendingChanges: TNotifyChanges;
    ApplyingChanges: Boolean;

    SortColumn: TObjectViewColumn;
    SortSign: Integer;

    AutoCheckForUpdates: Boolean;
    LastCheckForUpdates: TDateTime;
    CheckForUpdatesSilent: Boolean;

    function GetFile: TO2File;
    function GetHasSelectedObject: Boolean;
    function GetSelectedObject: TO2Object;
    function GetHasSelectedField: Boolean;
    function GetSelectedField: TO2Field;
    function GetEventFilter: TEventFilter;
    procedure SetFileName(const Value: string);
    procedure SetBusy(const Value: Boolean);
    procedure SetStayOnTop(const Value: Boolean);
    procedure SetTransparency(const Value: Integer);

    function CanCloseFile: Boolean;
    function TryGetPassword(var Password: string): Boolean;
    procedure Initialize;
    procedure InitializeSearch;
    procedure LoadLanguageList;
    procedure DecodeCommandLine(out ACmdLineAction: TCmdLineAction;
      out ACmdLineFileName, APortablePath: string);
    procedure OpenNewInstance(const FileName: string = '');
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string; Copy: Boolean = False);
    procedure LoadMRUList;
    procedure SaveMRUList;
    procedure ConvertSettings;
    procedure LoadSettings(const FileName: string);
    procedure SaveSettings(const FileName: string);

    procedure GetStartDate(out StartDate: TDateTime; out UseParams: Boolean);
    function ObjToListItem(const AObject: TO2Object;
      const Item: TListItem): TListItem; overload;
    function ObjToListItem(ObjectIndex: Integer;
      const Item: TListItem): TListItem; overload;
    function FieldToListItem(const AField: TO2Field;
      const Item: TListItem): TListItem;
    function RelationToListItem(const ARelation: TO2Relation;
      const AObject: TO2Object; const Item: TListItem): TListItem; overload;
    function RelationToListItem(const AObjRelation: TO2ObjRelation;
      const Item: TListItem): TListItem; overload;
    function RuleToListItem(const ARule: TO2Rule;
      const Item: TListItem): TListItem; overload;
    function RuleToListItem(RuleIndex: Integer;
      const Item: TListItem): TListItem; overload;
    procedure EnableSelectedRules(Enable: Boolean);
    procedure UpdateRulesStatus;
    procedure SortObjectsView(Column: TObjectViewColumn);
    function CompareObjectsByName(const Obj1, Obj2: TO2Object): Integer;
    function CompareObjectsByTags(const Obj1, Obj2: TO2Object): Integer;
    function CompareObjectsByNextEvent(const Obj1, Obj2: TO2Object): Integer;
    procedure UpdateAllActions;
    procedure NotifyChanges(Changes: TNotifyChanges);
    procedure ResizeObjectsViewColumns;
    procedure ResizeFieldsViewColumns;
    procedure ResizeRelationsViewColumns;
    procedure ResizeRulesViewColumns;
    procedure UpdateObjectsView;
    procedure UpdateFieldsView;
    procedure UpdateNotesView;
    procedure UpdateRelationsView;
    procedure UpdateRulesView;
    procedure UpdateTagList;
    procedure UpdateRuleList;
    procedure UpdateMRUList(const FileName: string = '');
  protected
    property EventFilter: TEventFilter read GetEventFilter;
    property StayOnTop: Boolean read FStayOnTop write SetStayOnTop;
    property Transparency: Integer read FTransparency write SetTransparency;
  public
    procedure BeginBatchOperation;
    procedure EndBatchOperation;
    procedure FillObjList(const Objects: TO2ObjectList);

    property O2File: TO2File read GetFile;
    property O2FileName: string read FFileName write SetFileName;
    property HasSelectedObject: Boolean read GetHasSelectedObject;
    property SelectedObject: TO2Object read GetSelectedObject;
    property HasSelectedField: Boolean read GetHasSelectedField;
    property SelectedField: TO2Field read GetSelectedField;
    property Busy: Boolean read FBusy;
  end;

var
  MainForm: TMainForm;

implementation

uses
  TypInfo, StrUtils, DateUtils, Contnrs, ShellApi, Clipbrd, XMLDoc, XMLIntf,
  xmldom, msxmldom, System.JSON, JclFileUtils,
  uAppFiles, uShellUtils, uAbout, uGetPassword, uSetPassword,
  uFilePropsDlg, uObjPropsDlg, uRelationPropsDlg, uRulePropsDlg,
  uReplaceDlg, uPrintModel, uPrintPreview, uHTMLExportModel, uHTMLExport,
  uXmlStorage, uO2Xml, uO2Defs, uBrowserEmulation, uCtrlHelpers, uFileOperation,
  uO2ImportExport, uXmlImportExport, uiCalendarExport, uStuffHTML, uHTMLHelper,
  uO2ObjectsUtils, uPasswordScoreCache;

{$R *.dfm}

const
  ViewStyles: array[0..3] of TIdentMapEntry = (
    (Value: Integer(vsIcon);      Name: 'Icons'),
    (Value: Integer(vsSmallIcon); Name: 'SmallIcons'),
    (Value: Integer(vsList);      Name: 'List'),
    (Value: Integer(vsReport);    Name: 'Report'));

  SortColumns: array[0..2] of TIdentMapEntry = (
    (Value: Integer(ocName);      Name: 'Name'),
    (Value: Integer(ocTags);      Name: 'Tags'),
    (Value: Integer(ocNextEvent); Name: 'NextEvent'));

procedure SetHighlightColors(const Canvas: TCanvas; Highlight: THighlight);
begin
  case Highlight.Highlight of
    htCustom:
    begin
      Canvas.Brush.Color := Highlight.Color;
      Canvas.Font.Color := Highlight.TextColor;
    end;
    htPasswordScore:
    begin
      Canvas.Brush.Color := PasswordScoreColors[Highlight.PasswordScore];
      Canvas.Font.Color := clBlack;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  AppPath, SettingsPath, LauncherPath, PortablePath, AppInfo: string;
  ExeVersionInfo: TJclFileVersionInfo;
  AppInfoBuilder: TStringBuilder;
begin
  FBusy := False;
  BatchOperationCount := 0;

  FStayOnTop := False;
  FTransparency := 0;

  PendingChanges := [];
  ApplyingChanges := False;

  Application.HintHidePause := 4500;

  DecodeCommandLine(CmdLineAction, CmdLineFileName, PortablePath);

  ExeVersionInfo := TJclFileVersionInfo.Create(Application.ExeName);
  try
    FAppVersionInfo.AppName := ExeVersionInfo.ProductName;
    FAppVersionInfo.DisplayVersion := ExeVersionInfo.BinFileVersion;
    VersionExtractFileInfo(ExeVersionInfo.FixedInfo,
      FAppVersionInfo.Version.MajorVersion,
      FAppVersionInfo.Version.MinorVersion,
      FAppVersionInfo.Version.Release,
      FAppVersionInfo.Version.Build);

    AppInfoBuilder := TStringBuilder.Create;
    try
      AppInfo := AppInfoBuilder
        .AppendLine('[' + PAF_FormatSection + ']')
        .AppendLine(PAF_FormatTypeId + '=' + PAF_FormatType)
        .AppendLine(PAF_FormatVersionId + '=' + PAF_FormatVersion)
        .AppendLine
        .AppendLine('[' + PAF_DetailsSection + ']')
        .AppendLine(PAF_AppNameId + '='
          + ExeVersionInfo.ProductName + ' Portable')
        .AppendLine(PAF_AppIDId + '=' + ExeVersionInfo.ProductName)
        .AppendLine(PAF_PublisherId + '=' + ExeVersionInfo.CompanyName)
        .AppendLine(PAF_HomepageId + '='
          + ExeVersionInfo.GetCustomFieldValue('Homepage'))
        .AppendLine(PAF_CategoryId + '=' + PAF_CategorySecurity)
        .AppendLine(PAF_DescriptionId + '=' + ExeVersionInfo.Comments)
        .AppendLine(PAF_LanguageId + '=' + PAF_LanguageMultilingual)
        .AppendLine
        .AppendLine('[' + PAF_LicenseSection + ']')
        .AppendLine(PAF_ShareableId + '=' + BoolToStr(True, True))
        .AppendLine(PAF_OpenSourceId + '=' + BoolToStr(True, True))
        .AppendLine(PAF_FreewareId + '=' + BoolToStr(True, True))
        .AppendLine(PAF_CommercialUseId + '=' + BoolToStr(True, True))
        .AppendLine
        .AppendLine('[' + PAF_VersionSection + ']')
        .AppendLine(PAF_PackageVersionId + '=' + ExeVersionInfo.BinFileVersion)
        .AppendLine(PAF_DisplayVersionId + '=' + ExeVersionInfo.BinFileVersion)
        .AppendLine
        .AppendLine('[' + PAF_ControlSection + ']')
        .AppendLine(PAF_IconsId + '=1')
        .AppendLine(PAF_StartId + '=' + LauncherFile)
        .AppendLine
        .AppendLine('[' + PAF_AssociationsSection + ']')
        .AppendLine(PAF_FileTypesId + '=' + DefaultFileExt)
        .AppendLine
        .AppendLine('[' + PAF_FileTypeIconsSection + ']')
        .AppendLine(DefaultFileExt + '=' + PAF_FileTypeIconCustom)
        .ToString;
    finally
      AppInfoBuilder.Free;
    end;
  finally
    ExeVersionInfo.Free;
  end;

  if PortablePath <> '' then
  begin
    AppPath :=
      IncludeTrailingPathDelimiter(PortablePath) + PortableAppPath;
    if not GetSettingsOverride(Application.ExeName, SettingsPath) then
      SettingsPath :=
        IncludeTrailingPathDelimiter(PortablePath) + PortableSettingsPath;
    LauncherPath :=
      IncludeTrailingPathDelimiter(PortablePath) + PortableLauncherPath;
  end
  else
  begin
    AppPath := ExtractFilePath(Application.ExeName);
    if not GetSettingsOverride(Application.ExeName, SettingsPath) then
      SettingsPath :=
        IncludeTrailingPathDelimiter(TShellFolders.AppData) + LocalSettingsDir;
    LauncherPath := AppPath;
  end;

  AppFiles
    .Add(IdAppExe, ExtractFileName(Application.ExeName), AppPath,
      PortableAppPath)
    .AddInMemory(IdAppInfo, AppInfoFile, AppPath, PortableAppInfoPath, AppInfo)
    .Add(IdAppIcon, AppIconFile, AppPath, PortableAppInfoPath)
    .Add(IdAppIcon16, AppIcon16File, AppPath, PortableAppInfoPath)
    .Add(IdAppIcon32, AppIcon32File, AppPath, PortableAppInfoPath)
    .Add(IdFileTypeIcon, FileTypeIconFile, AppPath, PortableFileTypeIconsPath)
    .Add(IdFileTypeIcon16, FileTypeIcon16File, AppPath,
      PortableFileTypeIconsPath)
    .Add(IdFileTypeIcon32, FileTypeIcon32File, AppPath,
      PortableFileTypeIconsPath)
    .Add(IdLauncher, LauncherFile, LauncherPath, PortableLauncherPath)
    .Add(IdSettings, SettingsFile, SettingsPath, PortableSettingsPath,
      SSettingsOverwriteQuery)
    .Add(IdHelp, HTMLHelpFile, AppPath, PortableLauncherPath)
    .Add(IdLicense, LicenseFile, AppPath, PortableAppPath)
    .Add(IdReadMe, ReadMeFile, AppPath, PortableAppPath);

  LoadLanguageList;

  InstallOnRemovableMedia.Visible := AppFiles.Exists(IdLauncher);

  TEventFilterLookup.Fill(FindByEvent);

  MRUList := TMRUList.Create;
  MRUMenuItems := TList.Create;
  MRUMenuItems.Add(MRU1);
  MRUMenuItems.Add(MRU2);
  MRUMenuItems.Add(MRU3);
  MRUMenuItems.Add(MRU4);
  MRUMenuItems.Add(MRU5);
  MRUMenuItems.Add(MRU6);
  MRUMenuItems.Add(MRU7);
  MRUMenuItems.Add(MRU8);
  MRUMenuItems.Add(MRU9);

  LoadSettings(AppFiles.FullPath[IdSettings]);
  Initialize;

  FieldsView.Hint := SFieldsViewHint + #13#10 + SFieldsViewHint2;
  RelationsView.Hint := SRelationsViewHint + #13#10 + SRelationsViewHint2;
  OpenDialog.Filter := SOpenFileFilter;
  ImportDialog.Filter := SImportFileFilter;
  SaveDialog.Filter := SSaveFileFilter;
  SaveDialog.DefaultExt := DefaultFileExt;
  ExportDialog.Filter := SExportFileFilter;
  ImportSettingsDlg.Filter := SImportSettingsFileFilter;
  ExportSettingsDlg.Filter := SExportSettingsFileFilter;

  FSelectedObjects := TO2ObjectListViewEnumerable.Create(ObjectsView);
  FPasswordScoreCache := TPasswordScoreCache.Create;

  ActiveControl := ObjectsView;

  SetBrowserEmulation(ExtractFileName(Application.ExeName), IE11Default);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SaveSettings(AppFiles.FullPath[IdSettings]);
  MRUList.Free;
  MRUMenuItems.Free;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CanCloseFile;
end;

procedure TMainForm.ApplicationEventsActivate(Sender: TObject);
begin
  if FTransparencyOnlyIfDeactivated then
    AlphaBlend := False;
end;

procedure TMainForm.ApplicationEventsDeactivate(Sender: TObject);
begin
  if FTransparencyOnlyIfDeactivated then
    AlphaBlend := Transparency > 0;
end;

procedure TMainForm.ApplicationEventsIdle(Sender: TObject;
  var Done: Boolean);
begin
  if (PendingChanges <> []) and not ApplyingChanges then
  begin
    ApplyingChanges := True;
    try
      if ncObjects in PendingChanges then
      begin
        UpdateObjectsView;
        PendingChanges := PendingChanges + [ncObjProps, ncRelations];
      end;

      if ncObjProps in PendingChanges then
      begin
        UpdateFieldsView;
        UpdateNotesView;
      end;

      if ncRelations in PendingChanges then
        UpdateRelationsView;

      if ncRules in PendingChanges then
        UpdateRulesView;

      if ncTagList in PendingChanges then
        UpdateTagList;

      if ncRuleList in PendingChanges then
        UpdateRuleList;
    finally
      PendingChanges := [];
      ApplyingChanges := False;
    end;
  end;

  if CmdLineAction = caOpenFile then
  begin
    CmdLineAction := caNone;
    LoadFromFile(CmdLineFileName);
  end;

  if ObjectsView.SelCount = 0 then
  begin
    StatusBar.Panels[0].Text :=
      Format(SStatusItemsCount, [ObjectsView.Items.Count]);
    StatusBar.Panels[1].Text := O2FileName;
  end
  else
  begin
    StatusBar.Panels[0].Text :=
      Format(SStatusSelectedItemsCount, [ObjectsView.SelCount]);
    if SelectedObject.Tag <> '' then
      StatusBar.Panels[1].Text := Format(SStatusObject, [SelectedObject.Name,
        SelectedObject.Tag])
    else
      StatusBar.Panels[1].Text := Format(SStatusObject, [SelectedObject.Name,
        STagsNone]);
  end;

  if AutoCheckForUpdates and (LastCheckForUpdates < Today) then
  begin
    LastCheckForUpdates := Today;
    CheckForUpdatesSilent := True;
    CheckForUpdatesNow.Execute;
  end;
end;

procedure TMainForm.ApplicationEventsMessage(var Msg: tagMSG;
  var Handled: Boolean);
begin
  if ((Msg.Message = WM_RBUTTONDOWN) or (Msg.Message = WM_RBUTTONDBLCLK))
    and IsChild(NotesView.Handle, Msg.hwnd) then
    Handled := True;
end;

procedure TMainForm.ObjectsViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  NotifyChanges([ncObjProps, ncRelations]);
end;

procedure TMainForm.ObjectsViewColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  SortObjectsView(TObjectViewColumn(Column.Index));
end;

procedure TMainForm.ObjectsViewCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  case SortColumn of
    ocName:
      Compare := CompareObjectsByName(TO2Object(Item1.Data),
        TO2Object(Item2.Data)) * SortSign;
    ocTags:
      Compare := CompareObjectsByTags(TO2Object(Item1.Data),
        TO2Object(Item2.Data)) * SortSign;
    ocNextEvent:
      Compare := CompareObjectsByNextEvent(TO2Object(Item1.Data),
        TO2Object(Item2.Data)) * SortSign;
    else
      Compare := 0;
  end;
end;

procedure TMainForm.ObjectsViewDblClick(Sender: TObject);
begin
  ObjectProps.Execute;
end;

procedure TMainForm.ObjectsViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) then ObjectProps.Execute;
end;

procedure TMainForm.ObjectsViewResize(Sender: TObject);
begin
  ResizeObjectsViewColumns;
end;

procedure TMainForm.ObjectsViewEdited(Sender: TObject; Item: TListItem;
  var S: String);
begin
  try
    TO2Object(Item.Data).Name := S;
  except
    S := Item.Caption;
    raise;
  end;
end;

procedure TMainForm.ObjectsViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if (State * [cdsFocused, cdsHot] = []) and Assigned(Item.Data) then
  begin
    SetHighlightColors(Sender.Canvas,
      O2File.Rules.GetHighlightColors(TO2Object(Item.Data),
      FPasswordScoreCache));
  end;
end;

procedure TMainForm.FindByNameChange(Sender: TObject);
begin
  Timer.Enabled := False;
  if FindByName.Text = '' then
    NotifyChanges([ncObjects])
  else
    Timer.Enabled := True;
end;

procedure TMainForm.FindByTagSelectAllExecute(Sender: TObject);
begin
  FindByTag.SelectAll;
  NotifyChanges([ncObjects]);
end;

procedure TMainForm.FindByTagDeselectExecute(Sender: TObject);
begin
  FindByTag.ClearSelection;
  NotifyChanges([ncObjects]);
end;

procedure TMainForm.FindByTagInvertSelectionExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FindByTag.Count - 1 do
    FindByTag.Selected[I] := not FindByTag.Selected[I];
  NotifyChanges([ncObjects]);
end;

procedure TMainForm.FindByRuleSelectAllExecute(Sender: TObject);
begin
  FindByRule.SelectAll;
  NotifyChanges([ncObjects]);
end;

procedure TMainForm.FindByRuleDeselectExecute(Sender: TObject);
begin
  FindByRule.ClearSelection;
  NotifyChanges([ncObjects]);
end;

procedure TMainForm.FindByRuleInvertSelectionExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FindByRule.Count - 1 do
    FindByRule.Selected[I] := not FindByRule.Selected[I];
  NotifyChanges([ncObjects]);
end;

procedure TMainForm.FilterChange(Sender: TObject);
begin
  NotifyChanges([ncObjects]);
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;
  NotifyChanges([ncObjects]);
end;

procedure TMainForm.MRUItemClick(Sender: TObject);
var
  MRUItem: TMRUItem;
begin
  MRUItem := MRUList[TComponent(Sender).Tag];
  if CanCloseFile then
    if FileExists(MRUItem.Item) then
      LoadFromFile(MRUItem.Item)
    else
      if YesNoWarningBox(SRemoveFromMRUListQuery) then
      begin
        MRUList.Remove(MRUItem);
        UpdateMRUList;
      end;
end;

procedure TMainForm.LanguageClick(Sender: TObject);
begin
  SetLocaleOverride(AppFiles.FullPath[IdAppExe],
    Languages[TComponent(Sender).Tag].Language);
  InfoBox(SApplyAtNextStartup);
end;

procedure TMainForm.ActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Busy;
end;

procedure TMainForm.NewFileExecute(Sender: TObject);
begin
  if CanCloseFile then
  begin
    ObjectsView.Clear;
    FreeAndNil(FFile);
    Initialize;
    O2FileName := '';
  end;
end;

procedure TMainForm.NewWindowExecute(Sender: TObject);
begin
  OpenNewInstance;
end;

procedure TMainForm.OpenFileExecute(Sender: TObject);
begin
  OpenDialog.FileName := '';
  if CanCloseFile and OpenDialog.Execute then
    LoadFromFile(OpenDialog.FileName);
end;

procedure TMainForm.ReopenFileExecute(Sender: TObject);
begin
  if CanCloseFile then LoadFromFile(O2FileName);
end;

procedure TMainForm.ReopenFileUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Busy and (O2FileName <> '');
end;

procedure TMainForm.OpenFolderExecute(Sender: TObject);
begin
  ShellOpen(ExtractFileDir(O2FileName));
end;

procedure TMainForm.OpenFolderUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Busy and (O2FileName <> '');
end;

procedure TMainForm.ClearMRUListExecute(Sender: TObject);
begin
  MRUList.Clear;
  UpdateMRUList;
end;

procedure TMainForm.ClearMRUListUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := MRUList.Count > 0;
end;

procedure TMainForm.ImportExecute(Sender: TObject);
const
  idxImportFromO2File = 1;
  idxImportFromXmlFile = 2;
var
  ImportFile: IFileOperation;
begin
  ImportDialog.FileName := '';
  if ImportDialog.Execute then
  begin
    BeginBatchOperation;
    try
      case ImportDialog.FilterIndex of
        idxImportFromO2File:
          ImportFile := TO2Import.Create(O2File, Self);
        idxImportFromXmlFile:
          ImportFile := TXmlImport.Create(O2File);
        else
          Exit;
      end;

      ImportFile.Execute(ImportDialog.FileName);
    finally
      EndBatchOperation;
    end;
    Initialize;
  end;
end;

procedure TMainForm.SaveFileExecute(Sender: TObject);
begin
  if (O2FileName = '')
    or O2File.Encrypted and ((O2File.Cipher in DeprecatedCiphers)
    or (O2File.Hash in DeprecatedHashes)) then
    SaveFileAs.Execute
  else
    SaveToFile(O2FileName);
end;

procedure TMainForm.SaveFileAsExecute(Sender: TObject);
begin
  SaveDialog.FileName := '';
  if SaveDialog.Execute(Handle) then
    SaveToFile(SaveDialog.FileName);
end;

procedure TMainForm.SaveFileAsCopyExecute(Sender: TObject);
begin
  SaveDialog.FileName := '';
  if SaveDialog.Execute then
    SaveToFile(SaveDialog.FileName, True);
end;

procedure TMainForm.ExportExecute(Sender: TObject);
const
  idxExportToO2File = 1;
  idxExportToXmlFile = 2;
  idxExportToIcsFile = 3;
var
  ExportFile: IFileOperation;
begin
  ExportDialog.FileName := '';
  if ExportDialog.Execute then
  begin
    BeginBatchOperation;
    try
      case ExportDialog.FilterIndex of
        idxExportToO2File:
          ExportFile := TO2Export.Create(O2File, FSelectedObjects);
        idxExportToXmlFile:
          ExportFile := TXmlExport.Create(O2File);
        idxExportToIcsFile:
          ExportFile := TiCalendarExport.Create(O2File, FSelectedObjects);
        else
          Exit;
      end;

      ExportFile.Execute(ExportDialog.FileName);
    finally
      EndBatchOperation;
    end;
  end;
end;

procedure TMainForm.ExportToHTMLExecute(Sender: TObject);
var
  Model: THTMLExportModel;
begin
  Model := THTMLExportModel.Create(O2File, FSelectedObjects, FAppVersionInfo,
    XmlStorage);
  try
    THTMLExport.Execute(Application, Model);
  finally
    Model.Free;
  end;
end;

procedure TMainForm.PrintFileExecute(Sender: TObject);
var
  Selection: TO2ObjectList;
  Model: TPrintModel;
begin
  Selection := TO2ObjectList.Create;
  try
    FillObjList(Selection);

    Model := TPrintModel.Create(O2File, Selection, XmlStorage);
    try
      TPrintPreview.Execute(Application, Model);
    finally
      Model.Free;
    end;
  finally
    Selection.Free;
  end;
end;

procedure TMainForm.PrintFileUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Busy and (ObjectsView.Items.Count > 0);
end;

procedure TMainForm.FilePropsExecute(Sender: TObject);
begin
  TFilePropsDlg.Execute(Application, O2File);
end;

procedure TMainForm.ImportSettingsExecute(Sender: TObject);
begin
  ImportSettingsDlg.FileName := '';
  if ImportSettingsDlg.Execute then
    LoadSettings(ImportSettingsDlg.FileName);
end;

procedure TMainForm.ExportSettingsExecute(Sender: TObject);
begin
  ExportSettingsDlg.FileName := '';
  if ExportSettingsDlg.Execute then
    SaveSettings(ExportSettingsDlg.FileName);
end;

procedure TMainForm.DefaultLanguageExecute(Sender: TObject);
begin
  DeleteLocaleOverride(AppFiles.FullPath[IdAppExe]);
  InfoBox(SApplyAtNextStartup);
end;

procedure TMainForm.InstallOnRemovableMediaExecute(Sender: TObject);
var
  Dir: string;
begin
  Dir := '';
  if SelectDirectory(Format(SInstallOnRemovableMediaPrompt +
    #13#10 + SInstallOnRemovableMediaFolderPrompt,
    [AppFiles.PortableFilesTotalSize / (1024 * 1024)]),
    '', Dir, [sdNewUI, sdNewFolder]) then
  begin
    BeginBatchOperation;
    try
      AppFiles.InstallPortable(Dir);
    finally
      EndBatchOperation;
    end;
  end;
end;

procedure TMainForm.CheckForUpdatesNowExecute(Sender: TObject);
begin
  CheckForUpdatesRequest.ExecuteAsync(
    procedure
    const
      DebugOutputFmt =
        'Application Version Check: Current Version: %d.%d.%d.%d. Available Version %d.%d.%d.%d. Download URL: %s.';
    var
      AppUpdateInfo: TAppUpdateInfo;
      DebugOutput: string;
    begin
      try
        AppUpdateInfo := TAppUpdateInfo
          .Create(CheckForUpdatesResponse.JSONValue);

        DebugOutput := Format(DebugOutputFmt,
          [FAppVersionInfo.Version.MajorVersion,
          FAppVersionInfo.Version.MinorVersion,
          FAppVersionInfo.Version.Release,
          FAppVersionInfo.Version.Build,
          AppUpdateInfo.Version.MajorVersion,
          AppUpdateInfo.Version.MinorVersion,
          AppUpdateInfo.Version.Release,
          AppUpdateInfo.Version.Build,
          AppUpdateInfo.DownloadURL]);
        OutputDebugString(PChar(DebugOutput));

        if AppUpdateInfo.Version
          .Compare(FAppVersionInfo.Version) = GreaterThanValue then
        begin
          if YesNoBox(Format(SDownloadUpdatesQuery,
            [AppUpdateInfo.Version.MajorVersion,
            AppUpdateInfo.Version.MinorVersion,
            AppUpdateInfo.Version.Release])) then
            ShellOpen(AppUpdateInfo.DownloadURL);
        end
        else if not CheckForUpdatesSilent then
          InfoBox(SNoAvailableUpdates);
      except
        if not CheckForUpdatesSilent then
          ErrorBox(SCannotCheckForUpdates);
      end;

      CheckForUpdatesSilent := False;
    end,
    True,
    True,
    procedure (Obj: TObject)
    begin
      if not CheckForUpdatesSilent then
        ErrorBox(SCannotCheckForUpdates);
    end
  );
end;

procedure TMainForm.CheckForUpdatesPeriodicallyExecute(Sender: TObject);
begin
  AutoCheckForUpdates := not AutoCheckForUpdates;
end;

procedure TMainForm.CheckForUpdatesPeriodicallyUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := AutoCheckForUpdates;
end;

procedure TMainForm.RefreshViewsExecute(Sender: TObject);
begin
  NotifyChanges([ncObjects, ncRules, ncTagList, ncRuleList]);
end;

procedure TMainForm.ViewLargeIconsExecute(Sender: TObject);
begin
  ObjectsView.ViewStyle := vsIcon;
end;

procedure TMainForm.ViewLargeIconsUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := ObjectsView.ViewStyle = vsIcon;
end;

procedure TMainForm.ViewSmallIconsExecute(Sender: TObject);
begin
  ObjectsView.ViewStyle := vsSmallIcon;
end;

procedure TMainForm.ViewSmallIconsUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := ObjectsView.ViewStyle = vsSmallIcon;
end;

procedure TMainForm.ViewListExecute(Sender: TObject);
begin
  ObjectsView.ViewStyle := vsList;
end;

procedure TMainForm.ViewListUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := ObjectsView.ViewStyle = vsList;
end;

procedure TMainForm.ViewReportExecute(Sender: TObject);
begin
  ObjectsView.ViewStyle := vsReport;
end;

procedure TMainForm.ViewReportUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := ObjectsView.ViewStyle = vsReport;
end;

procedure TMainForm.SortByNameExecute(Sender: TObject);
begin
  SortObjectsView(ocName);
end;

procedure TMainForm.SortByNameUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := SortColumn = ocName;
end;

procedure TMainForm.SortByTagsExecute(Sender: TObject);
begin
  SortObjectsView(ocTags);
end;

procedure TMainForm.SortByTagsUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := SortColumn = ocTags;
end;

procedure TMainForm.SortByNextEventExecute(Sender: TObject);
begin
  SortObjectsView(ocNextEvent);
end;

procedure TMainForm.SortByNextEventUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := SortColumn = ocNextEvent;
end;

procedure TMainForm.TransparencyExecute(Sender: TObject);
begin
  Transparency := TAction(Sender).Tag;
end;

procedure TMainForm.TransparencyUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := TAction(Sender).Tag = Transparency;
end;

procedure TMainForm.TransparencyOnlyIfDeactivatedExecute(Sender: TObject);
begin
  FTransparencyOnlyIfDeactivated := not FTransparencyOnlyIfDeactivated;
  Transparency := -1;
end;

procedure TMainForm.TransparencyOnlyIfDeactivatedUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FTransparencyOnlyIfDeactivated;
  TAction(Sender).Enabled := Transparency > 0;
end;

procedure TMainForm.ViewStayOnTopExecute(Sender: TObject);
begin
  StayOnTop := not StayOnTop;
end;

procedure TMainForm.ViewStayOnTopUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := StayOnTop;
end;

procedure TMainForm.FindExecute(Sender: TObject);
begin
  if SearchBox.Visible then
  begin
    SearchBox.Visible := False;
    VSplitter.Visible := False;
  end
  else
  begin
    SearchBox.Visible := True;
    VSplitter.Visible := True;
    FindByName.SetFocus;
  end;
end;

procedure TMainForm.FindUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Busy;
  TAction(Sender).Checked := SearchBox.Visible;
end;

procedure TMainForm.ClearSearchExecute(Sender: TObject);
begin
  InitializeSearch;
  NotifyChanges([ncObjects]);
end;

procedure TMainForm.AboutExecute(Sender: TObject);
begin
  TAboutForm.Execute(Application, FAppVersionInfo, AppFiles);
end;

procedure TMainForm.WebExecute(Sender: TObject);
begin
  ShellOpen(TAction(Sender).Hint);
end;

function TMainForm.GetFile: TO2File;
begin
  if FFile = nil then
  begin
    FFile := TO2File.Create;
  end;
  Result := FFile;
end;

function TMainForm.GetHasSelectedObject: Boolean;
begin
  Result := Assigned(ObjectsView.Selected);
end;

function TMainForm.GetSelectedObject: TO2Object;
begin
  Result := TO2Object(ObjectsView.Selected.Data);
end;

function TMainForm.GetHasSelectedField: Boolean;
begin
  Result := Assigned(FieldsView.Selected);
end;

function TMainForm.GetSelectedField: TO2Field;
begin
  Result := TO2Field(FieldsView.Selected.Data);
end;

function TMainForm.GetEventFilter: TEventFilter;
begin
  Result := TEventFilter(TEventFilterLookup.SelectedValue(FindByEvent));
end;

procedure TMainForm.SetFileName(const Value: string);
begin
  FFileName := Value;
  if FFileName <> '' then
    Caption := ExtractFileName(FFileName) + ' - O2'
  else
    Caption := 'O2';
  Application.Title := Caption;
  UpdateMRUList(FFileName);
end;

procedure TMainForm.SetBusy(const Value: Boolean);
begin
  if FBusy <> Value then
  begin
    FBusy := Value;

    if FBusy then
      Screen.Cursor := crHourGlass
    else
      Screen.Cursor := crDefault;

    UpdateAllActions;

    FindByName.Enabled := not FBusy;
    FindByEvent.Enabled := not FBusy;
    FindByTag.Enabled := not FBusy;
    FindByRule.Enabled := not FBusy;

    Application.ProcessMessages;
  end;
end;

procedure TMainForm.SetStayOnTop(const Value: Boolean);
begin
  if FStayOnTop <> Value then
  begin
    FStayOnTop := Value;

    if FStayOnTop then
      FormStyle := fsStayOnTop
    else
      FormStyle := fsNormal;
  end;
end;

procedure TMainForm.SetTransparency(const Value: Integer);
begin
  if (FTransparency <> Value) and (Value in [0..100]) then
    FTransparency := Value;
  AlphaBlend := (FTransparency > 0) and not FTransparencyOnlyIfDeactivated;
  AlphaBlendValue := Trunc((100 - FTransparency) * 2.55);
end;

function TMainForm.CanCloseFile: Boolean;
begin
  Result := True;
  if O2File.Modified then
    case YesNoCancelBox(SSaveChangesQuery) of
      ID_YES:
      begin
        SaveFile.Execute;
        Result := not O2File.Modified;
      end;
      ID_CANCEL:
        Result := False;
    end;
end;

function TMainForm.TryGetPassword(var Password: string): Boolean;
begin
  Result := TGetPasswordDlg.Execute(Application, Password);
end;

procedure TMainForm.Initialize;
begin
  InitializeSearch;
  NotifyChanges([ncObjects, ncRules, ncTagList, ncRuleList]);
end;

procedure TMainForm.InitializeSearch;
begin
  FindByName.Clear;
  FindByEvent.ItemIndex := 0;
  FindByTag.ClearSelection;
  FindByRule.ClearSelection;
end;

procedure TMainForm.LoadLanguageList;
var
  LanguageModule: string;
  Item: TMenuItem;
  I: Integer;
begin
  for I := Low(Languages) to High(Languages) do
  begin
    LanguageModule := ChangeFileExt(AppFiles.FullPath[IdAppExe],
      '.' + Languages[I].Language);
    if FileExists(LanguageModule) then
    begin
      AppFiles.Add(IdResourceModule + Languages[I].Language,
        ExtractFileName(LanguageModule), ExtractFilePath(LanguageModule),
        PortableAppPath);

      Item := TMenuItem.Create(LanguageMenu);
      Item.Caption := GetLanguageName(Languages[I].LangId);
      Item.Tag := I;
      Item.OnClick := LanguageClick;
      LanguageMenu.Add(Item);
    end;
  end;
end;

procedure TMainForm.DecodeCommandLine(out ACmdLineAction: TCmdLineAction;
  out ACmdLineFileName, APortablePath: string);
var
  I: Integer;
begin
  ACmdLineAction := caNone;
  ACmdLineFileName := '';
  APortablePath := '';
  I := 1;
  while I <= ParamCount do
    if SameText(ParamStr(I), 'portable') and (ParamStr(I + 1) <> '') then
    begin
      APortablePath := ParamStr(I + 1);
      Inc(I, 2);
    end
    else
    begin
      ACmdLineAction := caOpenFile;
      ACmdLineFileName := ParamStr(I);
      Inc(I);
    end;
end;

procedure TMainForm.OpenNewInstance(const FileName: string);
var
  ACmdLineAction: TCmdLineAction;
  ACmdLineFileName, APortablePath, AppExe, Parameters: string;
begin
  DecodeCommandLine(ACmdLineAction, ACmdLineFileName, APortablePath);

  if APortablePath <> '' then
    Parameters := 'portable "' + APortablePath + '" '
  else
    Parameters := '';

  if FileName <> '' then
    Parameters := Parameters + '"' + FileName + '"';

  AppExe := AppFiles.FullPath[IdAppExe];
  ShellExecute(Application.Handle, 'open', PChar(AppExe), PChar(Parameters),
    PChar(ExtractFileDir(AppExe)), SW_SHOWNORMAL);
end;

procedure TMainForm.LoadFromFile(const FileName: string);
var
  OldFile: TO2File;
begin
  OldFile := O2File;
  try
    FFile := nil;
    O2File.FileName := FileName;

    BeginBatchOperation;
    try
      O2File.Load(Self);
      FPasswordScoreCache.Update(O2File);
    finally
      EndBatchOperation;
    end;
  except
    if Assigned(O2File) then O2File.Free;
    FFile := OldFile;
    raise;
  end;
  OldFile.Free;

  Initialize;
  O2FileName := O2File.FileName;
end;

procedure TMainForm.SaveToFile(const FileName: string; Copy: Boolean);
begin
  O2File.FileName := FileName;

  BeginBatchOperation;
  try
    O2File.Save(Copy);
  finally
    EndBatchOperation;
  end;

  if Copy then
    O2File.FileName := O2FileName
  else
    O2FileName := O2File.FileName;
end;

procedure TMainForm.GetStartDate(out StartDate: TDateTime;
  out UseParams: Boolean);
begin
  UseParams := False;
  if FindByEvent.ItemIndex <> -1 then
    case EventFilter of
      efAll,
      efAllEvents,
      efCustom:
        UseParams := True;
      efToday:
        StartDate := Date;
      efTomorrow:
        StartDate := Date + 1;
      efThisWeek:
        StartDate := StartOfTheWeek(Date);
      efThisMonth:
        StartDate := StartOfTheMonth(Date);
      efThisYear:
        StartDate := StartOfTheYear(Date);
      efNext7days,
      efNext15days,
      efNext30days,
      efNext60days,
      efNext90days,
      efNext180days,
      efNext365days:
        StartDate := Date;
    end;
end;

function TMainForm.ObjToListItem(const AObject: TO2Object;
  const Item: TListItem): TListItem;
var
  StartDate, EventDate: TDateTime;
  UseParams: Boolean;
begin
  if Assigned(Item) then
    Result := Item
  else
    Result := ObjectsView.Items.Add;
  Result.Caption := AObject.Name;
  Result.ImageIndex := 0;
  Result.SubItems.Add(AObject.Tag);
  GetStartDate(StartDate, UseParams);
  if O2File.Rules.GetNextEvent(AObject, StartDate, EventDate, UseParams) then
    Result.SubItems.Add(DateToStr(EventDate))
  else
    Result.SubItems.Add('');
  Result.Data := AObject;
end;

function TMainForm.ObjToListItem(ObjectIndex: Integer;
  const Item: TListItem): TListItem;
begin
  Result := ObjToListItem(O2File.Objects[ObjectIndex], Item);
end;

function TMainForm.FieldToListItem(const AField: TO2Field;
  const Item: TListItem): TListItem;
begin
  if Assigned(Item) then
    Result := Item
  else
    Result := FieldsView.Items.Add;
  Result.Caption := AField.FieldName;
  Result.SubItems.Clear;
  Result.SubItems.Add(O2File.Rules.GetDisplayText(AField, FShowPasswords));
  Result.Data := AField;
end;

function TMainForm.RelationToListItem(const ARelation: TO2Relation;
  const AObject: TO2Object; const Item: TListItem): TListItem;
var
  AObjRelation: TO2ObjRelation;
begin
  AObjRelation := ARelation.GetObjectRelation(AObject);
  try
    Result := RelationToListItem(AObjRelation, Item);
  finally
    AObjRelation.Free;
  end;
end;

function TMainForm.RelationToListItem(
  const AObjRelation: TO2ObjRelation; const Item: TListItem): TListItem;
begin
  if Assigned(Item) then
    Result := Item
  else
    Result := RelationsView.Items.Add;
  if Assigned(AObjRelation.Obj) then
    Result.Caption := AObjRelation.Obj.Name
  else
    Result.Caption := '';
  Result.SubItems.Clear;
  Result.SubItems.Add(AObjRelation.Role);
  Result.Data := AObjRelation.Relation;
end;

function TMainForm.RuleToListItem(const ARule: TO2Rule;
  const Item: TListItem): TListItem;
begin
  if Assigned(Item) then
    Result := Item
  else
    Result := RulesView.Items.Add;
  Result.Checked := ARule.Active;
  Result.Caption := ARule.Name;
  Result.SubItems.Clear;
  Result.SubItems.Add(TRuleTypeLookup.Lookup(Integer(ARule.RuleType)));
  Result.SubItems.Add(ARule.FieldName);
  Result.SubItems.Add(ARule.FieldValue);
  Result.Data := ARule;
end;

function TMainForm.RuleToListItem(RuleIndex: Integer;
  const Item: TListItem): TListItem;
begin
  Result := RuleToListItem(O2File.Rules[RuleIndex], Item);
end;

procedure TMainForm.EnableSelectedRules(Enable: Boolean);
var
  AItem: TListItem;
begin
  RulesView.Items.BeginUpdate;
  try
    for AItem in RulesView.Items do
      if AItem.Selected then
      begin
        TO2Rule(AItem.Data).Active := Enable;
        AItem.Checked := Enable;
      end;
  finally
    RulesView.Items.EndUpdate;
  end;
  NotifyChanges([ncObjects, ncRuleList]);
end;

procedure TMainForm.UpdateRulesStatus;
var
  AItem: TListItem;
  ARule: TO2Rule;
begin
  for AItem in RulesView.Items do
  begin
    ARule := TO2Rule(AItem.Data);
    if AItem.Checked <> ARule.Active then
    begin
      ARule.Active := AItem.Checked;
      NotifyChanges([ncObjects, ncRuleList]);
    end;
  end;
end;

procedure TMainForm.SortObjectsView(Column: TObjectViewColumn);
begin
  if SortColumn = Column then
    SortSign := -SortSign
  else
  begin
    SortColumn := Column;
    SortSign := 1;
  end;
  ObjectsView.AlphaSort;
end;

function TMainForm.CompareObjectsByName(const Obj1, Obj2: TO2Object): Integer;
begin
  Result := CompareText(Obj1.Name, Obj2.Name);
end;

function TMainForm.CompareObjectsByTags(const Obj1, Obj2: TO2Object): Integer;
var
  Tags1, Tags2: TStringList;
  I: Integer;
begin
  Tags1 := TStringList.Create;
  try
    Obj1.GetTags(Tags1);

    Tags2 := TStringList.Create;
    try
      Obj2.GetTags(Tags2);

      I := 0;
      Result := 0;
      while (I < Tags1.Count) and (I < Tags2.Count) and (Result = 0) do
      begin
        Result := CompareText(Tags1[I], Tags2[I]);
        Inc(I);
      end;
      if (Result = 0) and (Tags1.Count <> Tags2.Count) then
        if Tags1.Count < Tags2.Count then
          Result := -1
        else
          Result := 1;
    finally
      Tags2.Free;
    end;
  finally
    Tags1.Free;
  end;
end;

function TMainForm.CompareObjectsByNextEvent(const Obj1,
  Obj2: TO2Object): Integer;
var
  StartDate, Date1, Date2: TDateTime;
  UseParams: Boolean;
begin
  GetStartDate(StartDate, UseParams);
  if O2File.Rules.GetNextEvent(Obj1, StartDate, Date1, UseParams) then
  begin
    if O2File.Rules.GetNextEvent(Obj2, StartDate, Date2, UseParams) then
      Result := CompareDate(Date1, Date2)
    else
      Result := -1;
  end
  else
  begin
    if O2File.Rules.GetNextEvent(Obj2, StartDate, Date2, UseParams) then
      Result := 1
    else
      Result := 0;
  end
end;

procedure TMainForm.BeginBatchOperation;
begin
  Inc(BatchOperationCount);
  SetBusy(BatchOperationCount > 0);
end;

procedure TMainForm.EndBatchOperation;
begin
  Dec(BatchOperationCount);
  SetBusy(BatchOperationCount > 0);
end;

procedure TMainForm.UpdateAllActions;
var
  I: Integer;
begin
  for I := 0 to Pred(ActionList.ActionCount) do
    ActionList.Actions[I].Update;
end;

procedure TMainForm.NotifyChanges(Changes: TNotifyChanges);
begin
  if not ApplyingChanges then
    PendingChanges := PendingChanges + Changes;
end;

procedure TMainForm.ResizeObjectsViewColumns;
begin
  ObjectsView.ResizeColumns(1);
end;

procedure TMainForm.ResizeFieldsViewColumns;
begin
  FieldsView.ResizeColumns(1);
end;

procedure TMainForm.ResizeRelationsViewColumns;
begin
  RelationsView.ResizeColumns(0);
end;

procedure TMainForm.ResizeRulesViewColumns;
begin
  RulesView.ResizeColumns;
end;

procedure TMainForm.UpdateObjectsView;
var
  Date1, Date2: TDateTime;
  UseParams: Boolean;
  AObject: TO2Object;
  Selection: TList;

function CheckName(const Obj: TO2Object): Boolean;
begin
  Result := (FindByName.Text = '')
    or (Pos(LowerCase(FindByName.Text), LowerCase(Obj.Name)) > 0);
end;

function CheckTag(const Obj: TO2Object): Boolean;
var
  Tags: TStringList;
  I: Integer;
begin
  if (FindByTag.SelCount = 0)
    or FindByTag.Selected[0] and (Obj.Tag = '') then
    Result := True
  else
  begin
    Result := False;
    Tags := TStringList.Create;
    try
      Obj.GetTags(Tags);
      for I := 1 to FindByTag.Items.Count - 1 do
        if FindByTag.Selected[I]
          and (Tags.IndexOf(FindByTag.Items[I]) > -1) then
        begin
          Result := True;
          Break;
        end;
    finally
      Tags.Free;
    end;
  end;
end;

function CheckEvents(const Obj: TO2Object): Boolean;
begin
  Result := (FindByEvent.ItemIndex = -1) or (EventFilter = efAll)
    or O2File.Rules.CheckEvents(Obj, Date1, Date2, UseParams);
end;

function CheckRules(const Obj: TO2Object): Boolean;
var
  AField: TO2Field;
  I: Integer;
begin
  if FindByRule.SelCount = 0 then
    Result := True
  else
  begin
    Result := False;
    for AField in Obj.Fields do
      for I := 0 to FindByRule.Items.Count - 1 do
        if FindByRule.Selected[I] then
          with TO2Rule(FindByRule.Items.Objects[I]) do
            if not (RuleType in EventRules) and Matches(AField)
              or CheckEvents(AField, 0, 0, True) then
            begin
              Result := True;
              Break;
            end;
  end;
end;

begin
  Selection := ObjectsView.ListSelectedItemsData;
  try
    UseParams := False;
    if FindByEvent.ItemIndex <> -1 then
      case EventFilter of
        efAllEvents:
        begin
          Date1 := EncodeDate(1, 1, 1);
          Date2 := EncodeDate(9999, 12, 31);
        end;
        efCustom:
          UseParams := True;
        efToday:
        begin
          Date1 := Date;
          Date2 := Date;
        end;
        efTomorrow:
        begin
          Date1 := Date + 1;
          Date2 := Date + 1;
        end;
        efThisWeek:
        begin
          Date1 := StartOfTheWeek(Date);
          Date2 := StartOfTheDay(EndOfTheWeek(Date));
        end;
        efThisMonth:
        begin
          Date1 := StartOfTheMonth(Date);
          Date2 := StartOfTheDay(EndOfTheMonth(Date));
        end;
        efThisYear:
        begin
          Date1 := StartOfTheYear(Date);
          Date2 := StartOfTheDay(EndOfTheYear(Date));
        end;
        efNext7days:
        begin
          Date1 := Date;
          Date2 := Date + 7;
        end;
        efNext15days:
        begin
          Date1 := Date;
          Date2 := Date + 15;
        end;
        efNext30days:
        begin
          Date1 := Date;
          Date2 := Date + 30;
        end;
        efNext60days:
        begin
          Date1 := Date;
          Date2 := Date + 60;
        end;
        efNext90days:
        begin
          Date1 := Date;
          Date2 := Date + 90;
        end;
        efNext180days:
        begin
          Date1 := Date;
          Date2 := Date + 180;
        end;
        efNext365days:
        begin
          Date1 := Date;
          Date2 := Date + 365;
        end;
      end;

    ObjectsView.Items.BeginUpdate;
    try
      ObjectsView.Clear;
      for AObject in O2File.Objects do
        if CheckName(AObject) and CheckTag(AObject)
          and CheckEvents(AObject) and CheckRules(AObject) then
          ObjToListItem(AObject, nil);

      ObjectsView.AlphaSort;

      ResizeObjectsViewColumns;
      ObjectsView.SelectItemsByData(Selection);
    finally
      ObjectsView.Items.EndUpdate;
    end;
  finally
    Selection.Free;
  end;
end;

procedure TMainForm.UpdateFieldsView;
var
  AField: TO2Field;
  Selection: TList;
begin
  Selection := FieldsView.ListSelectedItemsData;
  try
    FieldsView.Items.BeginUpdate;
    try
      FieldsView.Clear;
      if HasSelectedObject then
        for AField in SelectedObject.Fields do
          FieldToListItem(AField, nil);
      ResizeFieldsViewColumns;
      FieldsView.SelectItemsByData(Selection);
    finally
      FieldsView.Items.EndUpdate;
    end;
  finally
    Selection.Free;
  end;
end;

procedure TMainForm.UpdateNotesView;
var
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    SB.AppendLine('<!DOCTYPE html>')
      .AppendLine('<html>')
      .Append('<body style="color: #000; background-color: #fff; font-family: sans-serif; font-size: 1rem;">');

    if HasSelectedObject then
      SB.AppendHTML(SelectedObject.Text, SelectedObject.TextType);

    SB.AppendLine('</body>').Append('</html>');

    StuffHTML(NotesView.DefaultInterface, SB.ToString);
  finally
    SB.Free;
  end;
end;

procedure TMainForm.UpdateRelationsView;
var
  AObjRelation: TO2ObjRelation;
  ObjRelations: TO2ObjRelations;
  Selection: TList;
begin
  Selection := RelationsView.ListSelectedItemsData;
  try
    RelationsView.Items.BeginUpdate;
    try
      RelationsView.Clear;
      if HasSelectedObject then
      begin
        ObjRelations := O2File.Relations.GetObjectRelations(SelectedObject);
        try
          for AObjRelation in ObjRelations do
            RelationToListItem(AObjRelation, nil);
        finally
          ObjRelations.Free;
        end;
      end;
      ResizeRelationsViewColumns;
      RelationsView.SelectItemsByData(Selection);
    finally
      RelationsView.Items.EndUpdate;
    end;
  finally
    Selection.Free;
  end;
end;

procedure TMainForm.UpdateRulesView;
var
  ARule: TO2Rule;
  Selection: TList;
begin
  Selection := RulesView.ListSelectedItemsData;
  try
    RulesView.Items.BeginUpdate;
    try
      RulesView.Clear;
      for ARule in O2File.Rules do RuleToListItem(ARule, nil);
      ResizeRulesViewColumns;
      RulesView.SelectItemsByData(Selection);
      if Assigned(RulesView.Selected) then
      begin
        RulesView.Selected.Focused := True;
        RulesView.Selected.MakeVisible(False);
      end;
    finally
      RulesView.Items.EndUpdate;
    end;
  finally
    Selection.Free;
  end;
end;

procedure TMainForm.UpdateTagList;
var
  NoneSelected: Boolean;
  Selection: TStrings;
  I: Integer;
begin
  Selection := TStringList.Create;
  try
    if FindByTag.Items.Count > 0 then
      NoneSelected := FindByTag.Selected[0]
    else
      NoneSelected := False;
    for I := 1 to FindByTag.Items.Count - 1 do
      if FindByTag.Selected[I] then
        Selection.Add(FindByTag.Items[I]);

    FindByTag.Items.BeginUpdate;
    try
      FindByTag.Items.Clear;
      FindByTag.Items.Add(STagsNone);
      AppendTagsToList(O2File.Objects.ToEnumerable, FindByTag.Items);
    finally
      FindByTag.Items.EndUpdate;
    end;

    FindByTag.Selected[0] := NoneSelected;
    for I := 1 to FindByTag.Items.Count - 1 do
      FindByTag.Selected[I] := Selection.IndexOf(FindByTag.Items[I]) > -1;
  finally
    Selection.Free;
  end;
end;

procedure TMainForm.UpdateRuleList;
var
  Selection: TObjectList;
  ARule: TO2Rule;
  I: Integer;
begin
  Selection := TObjectList.Create(False);
  try
    for I := 0 to FindByRule.Items.Count - 1 do
      if FindByRule.Selected[I] then
        Selection.Add(FindByRule.Items.Objects[I]);

    FindByRule.Items.BeginUpdate;
    try
      FindByRule.Items.Clear;
      for ARule in O2File.Rules do
        if ARule.Active then
          FindByRule.AddItem(ARule.Name, ARule);
    finally
      FindByRule.Items.EndUpdate;
    end;

    for I := 0 to FindByRule.Items.Count - 1 do
      FindByRule.Selected[I] :=
        Selection.IndexOf(FindByRule.Items.Objects[I]) > -1;
  finally
    Selection.Free;
  end;
end;

procedure TMainForm.UpdateMRUList(const FileName: string);
var
  I, J: Integer;
begin
  if FileName <> '' then MRUList.AddItem(FileName);

  J := 0;
  for I := 0 to MRUMenuItems.Count - 1 do
  begin
    while (J < MRUList.Count) and SameText(MRUList[J].Item, FileName) do Inc(J);

    with TMenuItem(MRUMenuItems[I]) do
      if J < MRUList.Count then
      begin
        Caption := IntToStr(Succ(I)) + ' '
          + MinimizeName(MRUList[J].Item, Canvas, 400);
        Visible := True;
        Tag := J;
        Inc(J);
      end
      else
        Visible := False;
  end;
end;

const
  IdMRUListItemFmt = '%s.%d';
  IdMRUListItemCntFmt = '%s.%d.Count';

procedure TMainForm.LoadMRUList;
var
  I: Integer;
begin
  MRUList.Clear;
  for I := 0 to XmlStorage.ReadInteger(IdMRUList, 0) - 1 do
  begin
    MRUList.Add(TMRUItem.Create(
      XmlStorage.ReadString(Format(IdMRUListItemFmt, [IdMRUList, I]), ''),
      XmlStorage.ReadInteger(Format(IdMRUListItemCntFmt, [IdMRUList, I]), 1)));
  end;
end;

procedure TMainForm.SaveMRUList;
var
  I: Integer;
begin
  XmlStorage.WriteInteger(IdMRUList, MRUList.Count);
  for I := 0 to MRUList.Count - 1 do
  begin
    XmlStorage.WriteString(Format(IdMRUListItemFmt,
      [IdMRUList, I]), MRUList[I].Item);
    XmlStorage.WriteInteger(Format(IdMRUListItemCntFmt,
      [IdMRUList, I]), MRUList[I].Count);
  end;
end;

procedure TMainForm.ConvertSettings;
var
  XML: IXMLDocument;
  Node: IXMLNode;
  NodeValue: Variant;
  I: Integer;
begin
  if SameText(XmlStorage.XML.DocumentElement.NodeName, 'Configuration') then
  begin
    XML := XmlStorage.XML;
    XmlStorage.XML := nil;

    Node := XML.DocumentElement.ChildNodes.FindNode('MRUList');
    if Assigned(Node) then
    begin
      NodeValue := Node.ChildValues['Count'];
      if not VarIsNull(NodeValue) then
      begin
        MRUList.Clear;
        for I := 0 to StrToIntDef(NodeValue, 0) - 1 do
        begin
          NodeValue := Node.ChildValues['Item' + IntToStr(I)];
          if not VarIsNull(NodeValue) then
            MRUList.Add(TMRUItem.Create(NodeValue));
        end;
        SaveMRUList;
      end;
    end;

    NodeValue := XML.DocumentElement.ChildValues['StayOnTop'];
    if not VarIsNull(NodeValue) then
      XmlStorage.WriteBoolean(IdStayOnTop,
        StrToBoolDef(NodeValue, False));

    NodeValue := XML.DocumentElement.ChildValues['Transparency'];
    if not VarIsNull(NodeValue) then
      XmlStorage.WriteInteger(IdTransparency,
        StrToIntDef(NodeValue, 0));

    NodeValue := XML.DocumentElement.ChildValues['AutoCheckForUpdates'];
    if not VarIsNull(NodeValue) then
      XmlStorage.WriteBoolean(IdAutoCheckForUpdates,
        StrToBoolDef(NodeValue, True));

    NodeValue := XML.DocumentElement.ChildValues['ViewStyle'];
    if not VarIsNull(NodeValue) then
      XmlStorage.WriteIntIdent(IdViewStyle, ViewStyles,
        GetEnumValue(TypeInfo(TViewStyle), NodeValue));

    NodeValue := XML.DocumentElement.ChildValues['SortColumn'];
    if not VarIsNull(NodeValue) then
      XmlStorage.WriteIntIdent(IdSortColumn, SortColumns,
        GetEnumValue(TypeInfo(TObjectViewColumn), NodeValue));

    NodeValue := XML.DocumentElement.ChildValues['SortSign'];
    if not VarIsNull(NodeValue) then
      XmlStorage.WriteBoolean(IdSortAscending,
        StrToIntDef(NodeValue, 1) > 0);

    Node := XML.DocumentElement.ChildNodes.FindNode('Print');
    if Assigned(Node) then
    begin
      NodeValue := Node.ChildValues['IncludeTags'];
      if not VarIsNull(NodeValue) then
        XmlStorage.WriteBoolean(IdPrintIncludeTags,
          StrToBoolDef(NodeValue, True));

      NodeValue := Node.ChildValues['IncludeNotes'];
      if not VarIsNull(NodeValue) then
        XmlStorage.WriteBoolean(IdPrintIncludeNotes,
          StrToBoolDef(NodeValue, True));

      NodeValue := Node.ChildValues['IncludeRelations'];
      if not VarIsNull(NodeValue) then
        XmlStorage.WriteBoolean(IdPrintIncludeRelations,
          StrToBoolDef(NodeValue, True));

      NodeValue := Node.ChildValues['IncludePasswords'];
      if not VarIsNull(NodeValue) then
        XmlStorage.WriteBoolean(IdPrintIncludePasswords,
          StrToBoolDef(NodeValue, True));
    end;
  end;
end;

procedure TMainForm.LoadSettings(const FileName: string);
const
  SortSigns: array[Boolean] of Integer = (-1, 1);
begin
  XmlStorage.DocumentElementName := 'O2';
  XmlStorage.LoadFromFile(FileName);

  ConvertSettings;

  LoadMRUList;
  UpdateMRUList;

  StayOnTop := XmlStorage.ReadBoolean(IdStayOnTop, False);
  FTransparencyOnlyIfDeactivated :=
    XmlStorage.ReadBoolean(IdTransparencyOnlyIfDeactivated, True);
  Transparency := XmlStorage.ReadInteger(IdTransparency, 0);
  AutoCheckForUpdates := XmlStorage.ReadBoolean(IdAutoCheckForUpdates, True);
  LastCheckForUpdates := XmlStorage.ReadFloat(IdLastCheckForUpdates, Yesterday);
  ObjectsView.ViewStyle := TViewStyle(XmlStorage.ReadIntIdent(IdViewStyle,
    ViewStyles, Integer(vsIcon)));
  SortColumn := TObjectViewColumn(XmlStorage.ReadIntIdent(IdSortColumn,
    SortColumns, Integer(ocName)));
  SortSign := SortSigns[XmlStorage.ReadBoolean(IdSortAscending, True)];
end;

procedure TMainForm.SaveSettings(const FileName: string);
begin
  SaveMRUList;

  XmlStorage.WriteBoolean(IdStayOnTop, StayOnTop);
  XmlStorage.WriteInteger(IdTransparency, Transparency);
  XmlStorage.WriteBoolean(IdAutoCheckForUpdates, AutoCheckForUpdates);
  XmlStorage.WriteBoolean(IdTransparencyOnlyIfDeactivated,
    FTransparencyOnlyIfDeactivated);
  XmlStorage.WriteFloat(IdLastCheckForUpdates, LastCheckForUpdates);
  XmlStorage.WriteIntIdent(IdViewStyle, ViewStyles,
    Integer(ObjectsView.ViewStyle));
  XmlStorage.WriteIntIdent(IdSortColumn, SortColumns,
    Integer(SortColumn));
  XmlStorage.WriteBoolean(IdSortAscending, SortSign > 0);

  ForceDirectories(ExtractFileDir(FileName));
  XmlStorage.SaveToFile(FileName);
end;

procedure TMainForm.ObjectMenuPopup(Sender: TObject);
var
  Selection: TO2ObjectList;
  Item: TMenuItem;
  Tags: TStrings;
  Tag: string;
begin
  AddTag.Clear;
  DeleteTag.Clear;
  if HasSelectedObject then
  begin
    Selection := TO2ObjectList.Create;
    Tags := TStringList.Create;
    try
      FillObjList(Selection);

      AppendTagsToList(O2File.Objects.ToEnumerable, Tags);
      for Tag in Tags do
      begin
        Item := TMenuItem.Create(Self);
        Item.Caption := StringReplace(Tag, '&', '&&', [rfReplaceAll]);
        Item.OnClick := AddTagClick;
        AddTag.Add(Item);
      end;
      AddTag.Enabled := AddTag.Count > 0;

      Tags.Clear;
      AppendTagsToList(Selection.ToEnumerable, Tags);
      for Tag in Tags do
      begin
        Item := TMenuItem.Create(Self);
        Item.Caption := StringReplace(Tag, '&', '&&', [rfReplaceAll]);
        Item.OnClick := DeleteTagClick;
        DeleteTag.Add(Item);
      end;
      DeleteTag.Enabled := DeleteTag.Count > 0;
    finally
      Selection.Free;
      Tags.Free;
    end;
  end
  else
  begin
    AddTag.Enabled := False;
    DeleteTag.Enabled := False;
  end;
end;

procedure TMainForm.NewObjectExecute(Sender: TObject);
var
  Item: TListItem;
  Index: Integer;
begin
  Index := -1;
  if TObjPropsDlg.Execute(Application, O2File.Objects, Index, False,
    pgGeneral) then
  begin
    FPasswordScoreCache.Update(O2File, Index);
    Item := ObjToListItem(Index, nil);
    ObjectsView.ClearSelection;
    Item.Selected := True;
    Item.Focused := True;
    NotifyChanges([ncObjProps, ncRelations, ncTagList]);
  end;
end;

procedure TMainForm.DuplicateObjectExecute(Sender: TObject);
var
  Item: TListItem;
  Index: Integer;
begin
  Index := SelectedObject.Index;
  if TObjPropsDlg.Execute(Application, O2File.Objects, Index, True,
    pgGeneral) then
  begin
    FPasswordScoreCache.Update(O2File, Index);
    Item := ObjToListItem(Index, nil);
    ObjectsView.ClearSelection;
    Item.Selected := True;
    Item.Focused := True;
    NotifyChanges([ncObjProps, ncRelations, ncTagList]);
  end;
end;

procedure TMainForm.DeleteObjectExecute(Sender: TObject);
begin
  if YesNoBox(SDeleteObjectsQuery) then
  begin
    ObjectsView.FreeSelectedItemsData;
    ObjectsView.DeleteSelected;
  end;
end;

procedure TMainForm.RenameObjectExecute(Sender: TObject);
begin
  ObjectsView.Selected.EditCaption;
end;

procedure TMainForm.SelectAllExecute(Sender: TObject);
begin
  ObjectsView.SelectAll;
end;

procedure TMainForm.DeselectExecute(Sender: TObject);
begin
  ObjectsView.ClearSelection;
end;

procedure TMainForm.InvertSelectionExecute(Sender: TObject);
begin
  ObjectsView.InvertSelection;
end;

procedure TMainForm.ObjectTagsExecute(Sender: TObject);
var
  Index: Integer;
begin
  Index := SelectedObject.Index;
  if TObjPropsDlg.Execute(Application, O2File.Objects, Index, False,
    pgGeneralTags) then
  begin
    ObjToListItem(Index, ObjectsView.Selected);
    NotifyChanges([ncObjProps, ncTagList]);
  end;
end;

procedure TMainForm.AddTagClick(Sender: TObject);
var
  Selection: TO2ObjectList;
  AObject: TO2Object;
  Tag: string;
begin
  Selection := TO2ObjectList.Create;
  try
    FillObjList(Selection);
    Tag := StringReplace(TMenuItem(Sender).Caption, '&&', '&', [rfReplaceAll]);
    for AObject in Selection do
      AObject.AddTag(Tag);
  finally
    Selection.Free;
  end;
end;

procedure TMainForm.DeleteTagClick(Sender: TObject);
var
  Selection: TO2ObjectList;
  AObject: TO2Object;
  Tag: string;
begin
  Selection := TO2ObjectList.Create;
  try
    FillObjList(Selection);
    Tag := StringReplace(TMenuItem(Sender).Caption, '&&', '&', [rfReplaceAll]);
    for AObject in Selection do
      AObject.DeleteTag(Tag);
  finally
    Selection.Free;
  end;
end;

procedure TMainForm.ReplaceTagExecute(Sender: TObject);
var
  Selection: TO2ObjectList;
  SearchTag, ReplaceTag: string;
  SearchTags, ReplaceTags: TStrings;
begin
  Selection := TO2ObjectList.Create;
  SearchTags := TStringList.Create;
  ReplaceTags := TStringList.Create;
  try
    FillObjList(Selection);
    AppendTagsToList(Selection.ToEnumerable, SearchTags);
    AppendTagsToList(O2File.Objects.ToEnumerable, ReplaceTags);
    if TReplaceDlg.Execute(Application, acReplaceTag,
      SearchTags, ReplaceTags, SearchTag, ReplaceTag) then
    begin
      ReplaceObjectsTag(Selection.ToEnumerable, SearchTag, ReplaceTag);
      NotifyChanges([ncObjects, ncTagList]);
    end;
  finally
    Selection.Free;
    SearchTags.Free;
    ReplaceTags.Free;
  end;
end;

procedure TMainForm.ReplaceFieldNameExecute(Sender: TObject);
var
  Selection: TO2ObjectList;
  SearchFieldName, ReplaceFieldName: string;
  SearchFieldNames, ReplaceFieldNames: TStrings;
begin
  Selection := TO2ObjectList.Create;
  SearchFieldNames := TStringList.Create;
  ReplaceFieldNames := TStringList.Create;
  try
    FillObjList(Selection);
    AppendFieldNamesToList(Selection.ToEnumerable, SearchFieldNames);
    AppendFieldNamesToList(O2File.Objects.ToEnumerable, ReplaceFieldNames);
    if TReplaceDlg.Execute(Application, acReplaceFieldName,
      SearchFieldNames, ReplaceFieldNames,
      SearchFieldName, ReplaceFieldName) then
    begin
      ReplaceObjectsFieldName(Selection.ToEnumerable, SearchFieldName,
        ReplaceFieldName);
      NotifyChanges([ncObjects]);
    end;
  finally
    Selection.Free;
    SearchFieldNames.Free;
    ReplaceFieldNames.Free;
  end;
end;

procedure TMainForm.ReplaceFieldValueExecute(Sender: TObject);
var
  Selection: TO2ObjectList;
  SearchFieldName, ReplaceFieldValue: string;
  SearchFieldNames, ReplaceFieldValues: TStrings;
begin
  Selection := TO2ObjectList.Create;
  SearchFieldNames := TStringList.Create;
  ReplaceFieldValues := TStringList.Create;
  try
    FillObjList(Selection);
    AppendFieldNamesToList(Selection.ToEnumerable, SearchFieldNames);
    AppendFieldValuesToList(O2File.Objects.ToEnumerable, '',
      ReplaceFieldValues);
    if TReplaceDlg.Execute(Application, acReplaceFieldValue,
      SearchFieldNames, ReplaceFieldValues,
      SearchFieldName, ReplaceFieldValue) then
    begin
      ReplaceObjectsFieldValue(Selection.ToEnumerable, SearchFieldName,
        ReplaceFieldValue);
      NotifyChanges([ncObjects]);
    end;
  finally
    Selection.Free;
    SearchFieldNames.Free;
    ReplaceFieldValues.Free;
  end;
end;

procedure TMainForm.ReplaceRoleExecute(Sender: TObject);
var
  Selection: TO2ObjectList;
  SearchRole, ReplaceRole: string;
  SearchRoles, ReplaceRoles: TStrings;
begin
  Selection := TO2ObjectList.Create;
  SearchRoles := TStringList.Create;
  ReplaceRoles := TStringList.Create;
  try
    FillObjList(Selection);
    O2File.Relations.GetRoles(Selection, SearchRoles);
    O2File.Relations.GetRoles(ReplaceRoles);
    if TReplaceDlg.Execute(Application, acReplaceRole,
      SearchRoles, ReplaceRoles, SearchRole, ReplaceRole) then
    begin
      O2File.Relations.ReplaceRole(Selection, SearchRole, ReplaceRole);
      NotifyChanges([ncRelations]);
    end;
  finally
    Selection.Free;
    SearchRoles.Free;
    ReplaceRoles.Free;
  end;
end;

procedure TMainForm.ObjectPropsExecute(Sender: TObject);
var
  Page: TObjPropsDlgPage;
  Index: Integer;
begin
  case PageControl.ActivePageIndex of
    0: Page := pgFields;
    1: Page := pgNotes;
    else Page := pgGeneral;
  end;
  Index := SelectedObject.Index;
  if TObjPropsDlg.Execute(Application, O2File.Objects, Index, False, Page) then
  begin
    FPasswordScoreCache.Update(O2File, Index);
    ObjToListItem(Index, ObjectsView.Selected);
    NotifyChanges([ncObjProps, ncTagList]);
  end;
end;

procedure TMainForm.ObjectActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Busy and HasSelectedObject;
end;

procedure TMainForm.FieldsViewDblClick(Sender: TObject);
begin
  CopyValue.Execute;
end;

procedure TMainForm.FieldsViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) then CopyValue.Execute;
end;

procedure TMainForm.FieldsViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (ssCtrl in Shift) then
  begin
    OpenLink.Execute;
    SendEmail.Execute;
  end;
end;

procedure TMainForm.FieldsViewResize(Sender: TObject);
begin
  ResizeFieldsViewColumns;
end;

procedure TMainForm.FieldsViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if (State * [cdsFocused, cdsHot] = []) and Assigned(Item.Data) then
  begin
    SetHighlightColors(Sender.Canvas,
      O2File.Rules.GetHighlightColors(TO2Field(Item.Data),
      FPasswordScoreCache));
  end;
end;

procedure TMainForm.FieldsViewCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if Assigned(Item.Data) then
  begin
    if (State * [cdsFocused, cdsHot] = []) then
    begin
      SetHighlightColors(Sender.Canvas,
        O2File.Rules.GetHighlightColors(TO2Field(Item.Data),
        FPasswordScoreCache));
    end;
    if Assigned(O2File.Rules.FindFirstRule(
      TO2Field(Item.Data), HyperlinkRules)) then
      Sender.Canvas.Font.Style := Sender.Canvas.Font.Style + [fsUnderline];
  end;
end;

procedure TMainForm.FieldMenuPopup(Sender: TObject);
begin
  UpdateAllActions;
end;

procedure TMainForm.CopyValueExecute(Sender: TObject);
begin
  Clipboard.AsText := SelectedField.FieldValue;
end;

procedure TMainForm.CopyNameValueAsRowExecute(Sender: TObject);
begin
  Clipboard.AsText := SelectedField.FieldName + #9 + SelectedField.FieldValue;
end;

procedure TMainForm.CopyNameValueAsColumnExecute(Sender: TObject);
begin
  Clipboard.AsText := SelectedField.FieldName + #13#10
    + SelectedField.FieldValue;
end;

procedure TMainForm.CopyNameValueUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Busy and HasSelectedField;
end;

procedure TMainForm.CopyValuesAsRowsExecute(Sender: TObject);
var
  AItem: TListItem;
  S: string;
begin
  S := '';
  for AItem in FieldsView.Items do
    S := S + TO2Field(AItem.Data).FieldValue + #13#10;

  Clipboard.AsText := TrimRight(S);
end;

procedure TMainForm.CopyValuesAsColumnsExecute(Sender: TObject);
var
  AItem: TListItem;
  S: string;
begin
  S := '';
  for AItem in FieldsView.Items do
    S := S + TO2Field(AItem.Data).FieldValue + #9;

  Clipboard.AsText := TrimRight(S);
end;

procedure TMainForm.CopyAsRowsExecute(Sender: TObject);
var
  AItem: TListItem;
  S: string;
begin
  S := '';
  for AItem in FieldsView.Items do
    S := S + TO2Field(AItem.Data).FieldName + #9
      + TO2Field(AItem.Data).FieldValue + #13#10;

  Clipboard.AsText := TrimRight(S);
end;

procedure TMainForm.CopyAsColumnsExecute(Sender: TObject);
var
  AItem: TListItem;
  S1, S2: string;
begin
  S1 := '';
  S2 := '';
  for AItem in FieldsView.Items do
  begin
    S1 := S1 + TO2Field(AItem.Data).FieldName + #9;
    S2 := S2 + TO2Field(AItem.Data).FieldValue + #9;
  end;

  Clipboard.AsText := TrimRight(S1) + #13#10 + TrimRight(S2);
end;

procedure TMainForm.CopyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Busy and (FieldsView.Items.Count > 0);
end;

procedure TMainForm.AddToIEFavoritesExecute(Sender: TObject);
var
  URLFile: TStrings;
begin
  URLFile := TStringList.Create;
  try
    URLFile.Add('[InternetShortcut]');
    URLFile.Add('URL=' + O2File.Rules.GetHyperLink(SelectedField));
    URLFile.SaveToFile(IncludeTrailingPathDelimiter(TShellFolders.Favorites)
      + SelectedObject.Name + '.url');
  finally
    URLFile.Free;
  end;
end;

procedure TMainForm.OpenLinkExecute(Sender: TObject);
begin
  ShellOpen(O2File.Rules.GetHyperLink(SelectedField));
end;

procedure TMainForm.OpenLinkUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := not Busy and HasSelectedField
    and Assigned(O2File.Rules.FindFirstRule(SelectedField, [rtHyperLink]));
  TAction(Sender).Enabled := TAction(Sender).Visible;
end;

procedure TMainForm.SendEmailExecute(Sender: TObject);
begin
  ShellMailTo(SelectedField.FieldValue);
end;

procedure TMainForm.SendEmailUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := not Busy and HasSelectedField
    and Assigned(O2File.Rules.FindFirstRule(SelectedField, [rtEmail]));
  TAction(Sender).Enabled := TAction(Sender).Visible;
end;

procedure TMainForm.ShowPasswordsExecute(Sender: TObject);
begin
  FShowPasswords := not FShowPasswords;
  NotifyChanges([ncObjProps]);
end;

procedure TMainForm.ShowPasswordsUpdate(Sender: TObject);
begin
  TAction(sender).Checked := FShowPasswords;
  TAction(sender).Enabled := not Busy;
end;

procedure TMainForm.RelationsViewDblClick(Sender: TObject);
begin
  RelationProps.Execute;
end;

procedure TMainForm.RelationsViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) then RelationProps.Execute;
end;

procedure TMainForm.RelationsViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (ssCtrl in Shift) then RelationGoToObj.Execute;
end;

procedure TMainForm.RelationsViewResize(Sender: TObject);
begin
  ResizeRelationsViewColumns;
end;

procedure TMainForm.NewRelationExecute(Sender: TObject);
var
  ARelation: TO2Relation;
  Selection: TO2ObjectList;
begin
  ARelation := nil;
  Selection := TO2ObjectList.Create;
  try
    FillObjList(Selection);
    if TRelationPropsDlg.Execute(Application,
      O2File, Selection[0], Selection[1], ARelation) then
      RelationToListItem(ARelation, SelectedObject, nil);
  finally
    Selection.Free;
  end;
end;

procedure TMainForm.NewRelationUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Busy and (ObjectsView.SelCount = 2);
end;

procedure TMainForm.DeleteRelationExecute(Sender: TObject);
begin
  if YesNoBox(SDeleteRelationQuery) then
  begin
    RelationsView.FreeSelectedItemsData;
    RelationsView.DeleteSelected;
  end;
end;

procedure TMainForm.RelationGoToObjExecute(Sender: TObject);
var
  AObjRelation: TO2ObjRelation;
  Item: TListItem;
begin
  AObjRelation := TO2Relation(RelationsView.Selected.Data).GetObjectRelation(
    SelectedObject);
  try
    Item := ObjectsView.FindData(0, AObjRelation.Obj, True, False);
    if Assigned(Item) then
    begin
      ObjectsView.ClearSelection;
      Item.Selected := True;
      Item.Focused := True;
    end;
  finally
    AObjRelation.Free;
  end;
end;

procedure TMainForm.RelationPropsExecute(Sender: TObject);
var
  ARelation: TO2Relation;
begin
  ARelation := TO2Relation(RelationsView.Selected.Data);
  if TRelationPropsDlg.Execute(Application, O2File, nil, nil, ARelation) then
    RelationToListItem(ARelation, SelectedObject, RelationsView.Selected);
end;

procedure TMainForm.RelationActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Busy and Assigned(RelationsView.Selected);
end;

procedure TMainForm.RulesViewDblClick(Sender: TObject);
begin
  RuleProps.Execute;
end;

procedure TMainForm.RulesViewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_SPACE) and (Shift = []) then UpdateRulesStatus;
end;

procedure TMainForm.RulesViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) then RuleProps.Execute;
end;

procedure TMainForm.RulesViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateRulesStatus;
end;

procedure TMainForm.RulesViewResize(Sender: TObject);
begin
  ResizeRulesViewColumns;
end;

procedure TMainForm.NewRuleExecute(Sender: TObject);
var
  Item: TListItem;
  Index: Integer;
begin
  Index := -1;
  if TRulePropsDlg.Execute(Application, O2File.Rules, Index, False) then
  begin
    Item := RuleToListItem(Index, nil);
    Item.Selected := True;
    Item.Focused := True;
    NotifyChanges([ncObjects, ncRuleList]);
  end;
end;

procedure TMainForm.DuplicateRuleExecute(Sender: TObject);
var
  Item: TListItem;
  Index: Integer;
begin
  Index := TO2Rule(RulesView.Selected.Data).Index;
  if TRulePropsDlg.Execute(Application, O2File.Rules, Index, True) then
  begin
    Item := RuleToListItem(Index, nil);
    Item.Selected := True;
    Item.Focused := True;
    NotifyChanges([ncObjects, ncRuleList]);
  end;
end;

procedure TMainForm.DeleteRuleExecute(Sender: TObject);
begin
  if YesNoBox(SDeleteRuleQuery) then
  begin
    RulesView.FreeSelectedItemsData;
    RulesView.DeleteSelected;
    NotifyChanges([ncObjects, ncRuleList]);
  end;
end;

procedure TMainForm.MoveUpRuleExecute(Sender: TObject);
begin
  with TO2Rule(RulesView.Selected.Data) do Index := Index - 1;
  NotifyChanges([ncObjects, ncRules]);
end;

procedure TMainForm.MoveUpRuleUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Busy and Assigned(RulesView.Selected)
    and (TO2Rule(RulesView.Selected.Data).Index > 0);
end;

procedure TMainForm.MoveDownRuleExecute(Sender: TObject);
begin
  with TO2Rule(RulesView.Selected.Data) do Index := Index + 1;
  NotifyChanges([ncObjects, ncRules]);
end;

procedure TMainForm.MoveDownRuleUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Busy and Assigned(RulesView.Selected)
    and (TO2Rule(RulesView.Selected.Data).Index < Pred(O2File.Rules.Count));
end;

procedure TMainForm.EnableRulesExecute(Sender: TObject);
begin
  EnableSelectedRules(True);
end;

procedure TMainForm.DisableRulesExecute(Sender: TObject);
begin
  EnableSelectedRules(False);
end;

procedure TMainForm.SelectAllRulesExecute(Sender: TObject);
begin
  RulesView.SelectAll;
end;

procedure TMainForm.DeselectRulesExecute(Sender: TObject);
begin
  RulesView.ClearSelection;
end;

procedure TMainForm.InvertRulesSelectionExecute(Sender: TObject);
begin
  RulesView.InvertSelection;
end;

procedure TMainForm.RulePropsExecute(Sender: TObject);
var
  Index: Integer;
begin
  Index := TO2Rule(RulesView.Selected.Data).Index;
  if TRulePropsDlg.Execute(Application, O2File.Rules, Index, False) then
  begin
    RuleToListItem(Index, RulesView.Selected);
    NotifyChanges([ncObjects, ncRuleList]);
  end;
end;

procedure TMainForm.RuleActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Busy and Assigned(RulesView.Selected);
end;

procedure TMainForm.SaveDialogCanClose(Sender: TObject;
  var CanClose: Boolean);
var
  Cipher, Hash: Byte;
  Encrypted: Boolean;
  Password: string;
begin
  if not FileExists(SaveDialog.FileName)
    or YesNoBox(SFileOverwriteQuery) then
  begin
    Encrypted := O2File.Encrypted;
    Cipher := O2File.Cipher;
    Hash := O2File.Hash;
    Password := O2File.Password;
    CanClose := TSetPasswordDlg.Execute(Application, Encrypted, Cipher, Hash,
      Password);
    if CanClose then
    begin
      O2File.Encrypted := Encrypted;
      O2File.Cipher := Cipher;
      O2File.Hash := Hash;
      O2File.Password := Password;
    end;
  end
  else
    CanClose := False;
end;

procedure TMainForm.ExportDialogCanClose(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := not FileExists(ExportDialog.FileName)
    or YesNoBox(SFileOverwriteQuery);
end;

procedure TMainForm.ExportSettingsDlgCanClose(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := not FileExists(ExportSettingsDlg.FileName)
    or YesNoBox(SFileOverwriteQuery);
end;

procedure TMainForm.DragDropDrop(Sender: TObject; Pos: TPoint;
  Value: TStrings);
begin
  if (Value.Count > 0) and CanCloseFile then LoadFromFile(Value[0]);
end;

procedure TMainForm.FillObjList(const Objects: TO2ObjectList);
var
  NoneSelected: Boolean;
  AItem: TListItem;
begin
  NoneSelected := ObjectsView.SelCount = 0;
  for AItem in ObjectsView.Items do
    if NoneSelected or AItem.Selected then Objects.Add(AItem.Data);
end;

end.

