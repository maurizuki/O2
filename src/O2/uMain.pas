{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2026 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uMain;

interface

{$WARN UNIT_PLATFORM OFF}

{$R Icons.res}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, ImgList, ActnList, Menus, XPMan, AppEvnts,
  StdCtrls, ExtCtrls, FileCtrl, Types, ImageList, Actions, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, WebView2, ActiveX, Edge,
  JvComponentBase, JvDragDrop, Spring.Container,
  uO2File, uO2Objects, uO2Relations, uO2Rules, uGlobal, uMRUlist, uServices;

type
  TNotifyChange = (
    ncObjectsView,
    ncObjPropsViews,
    ncRelationsView,
    ncRulesView,
    ncTagList,
    ncRuleList
    );

  TNotifyChanges = set of TNotifyChange;

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
    NotesView: TEdgeBrowser;
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
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ApplicationEventsActivate(Sender: TObject);
    procedure ApplicationEventsDeactivate(Sender: TObject);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
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
    procedure TimerTimer(Sender: TObject);
    procedure FindByEventChange(Sender: TObject);
    procedure FindByTagClick(Sender: TObject);
    procedure FindByTagSelectAllExecute(Sender: TObject);
    procedure FindByTagDeselectExecute(Sender: TObject);
    procedure FindByTagInvertSelectionExecute(Sender: TObject);
    procedure FindByRuleClick(Sender: TObject);
    procedure FindByRuleSelectAllExecute(Sender: TObject);
    procedure FindByRuleDeselectExecute(Sender: TObject);
    procedure FindByRuleInvertSelectionExecute(Sender: TObject);
    procedure MRUItemClick(Sender: TObject);
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
    FServiceContainer: TContainer;
    FModel: IFileManager;
    FAppFiles: IAppFiles;
    FStorage: IStorage;
    FBusy: Boolean;
    FBatchOperationCount: Integer;
    FPendingChanges: TNotifyChanges;
    FApplyingChanges: Boolean;
    FOpenFileName: string;
    FFileName: string;
    FMRUMenuItems: TList;
    FMRUList: TMRUList;
    FStayOnTop: Boolean;
    FTransparency: Integer;
    FTransparencyOnlyIfDeactivated: Boolean;
    FSortKind: TObjectSortKind;
    FSortSign: Integer;
    FAutoCheckForUpdates: Boolean;
    FLastCheckForUpdates: TDateTime;
    FCheckForUpdatesSilent: Boolean;
    FSelectedObjects: IEnumerable<TO2Object>;
    FShowPasswords: Boolean;

    function GetHasSelectedObject: Boolean;
    function GetSelectedObject: TO2Object;
    function GetHasSelectedField: Boolean;
    function GetSelectedField: TO2Field;
    function GetSelectedRelation: TO2Relation;
    function GetSelectedRule: TO2Rule;
    procedure SetServiceContainer(const Value: TContainer);
    procedure SetBusy(const Value: Boolean);
    procedure SetFileName(const Value: string);
    procedure SetStayOnTop(const Value: Boolean);
    procedure SetTransparency(const Value: Integer);

    procedure BeginBatchOperation;
    procedure EndBatchOperation;
    function CanCloseFile: Boolean;
    function TryGetPassword(var Password: string): Boolean;
    procedure Initialize;
    procedure InitializeSearch;
    procedure OpenNewInstance(const AFileName: string = '');
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string; Copy: Boolean = False);
    procedure LoadMRUList;
    procedure SaveMRUList;
    procedure LoadSettings(const AFileName: string);
    procedure SaveSettings(const AFileName: string);

    function ObjToListItem(const AObject: TO2Object;
      const Item: TListItem): TListItem;
    function FieldToListItem(const AField: TO2Field;
      const Item: TListItem): TListItem;
    function RelationToListItem(const ARelation: TO2Relation;
      const AObject: TO2Object; const Item: TListItem): TListItem; overload;
    function RelationToListItem(const AObjRelation: TO2ObjRelation;
      const Item: TListItem): TListItem; overload;
    function RuleToListItem(const ARule: TO2Rule;
      const Item: TListItem): TListItem;
    procedure EnableSelectedRules(Enable: Boolean);
    procedure UpdateRulesStatus;
    procedure SortObjectsView(SortKind: TObjectSortKind);
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
    procedure UpdateMRUList(const AFileName: string = '');

    property FileName: string read FFileName write SetFileName;
    property HasSelectedObject: Boolean read GetHasSelectedObject;
    property HasSelectedField: Boolean read GetHasSelectedField;
    property SelectedField: TO2Field read GetSelectedField;
    property StayOnTop: Boolean read FStayOnTop write SetStayOnTop;
    property Transparency: Integer read FTransparency write SetTransparency;
  public
    property ServiceContainer: TContainer read FServiceContainer
      write SetServiceContainer;
    property Model: IFileManager read FModel;
    property SelectedObjects: IEnumerable<TO2Object> read FSelectedObjects;
    property SelectedObject: TO2Object read GetSelectedObject;
    property SelectedRelation: TO2Relation read GetSelectedRelation;
    property SelectedRule: TO2Rule read GetSelectedRule;
  end;

var
  MainForm: TMainForm;

implementation

uses
  StrUtils, DateUtils, Contnrs, ShellApi, Clipbrd, JSON, UITypes,
  uShellUtils, uStorageUtils, uAbout, uGetPassword, uSetPassword, uFilePropsDlg,
  uObjPropsDlg, uRelationPropsDlg, uRulePropsDlg, uReplaceDlg, uPrintPreview,
  uHTMLExport, uO2Defs, uCtrlHelpers, uHTMLHelper, uO2ObjectsUtils, uUtils;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FBusy := False;
  FBatchOperationCount := 0;
  FPendingChanges := [];
  FApplyingChanges := False;
  FOpenFileName := OpenFileName;
  FStayOnTop := False;
  FTransparency := 0;
  FSelectedObjects := TO2ObjectListViewEnumerable.Create(ObjectsView);

  Application.HintHidePause := 4500;

  FMRUList := TMRUList.Create;
  FMRUMenuItems := TList.Create;
  FMRUMenuItems.Add(MRU1);
  FMRUMenuItems.Add(MRU2);
  FMRUMenuItems.Add(MRU3);
  FMRUMenuItems.Add(MRU4);
  FMRUMenuItems.Add(MRU5);
  FMRUMenuItems.Add(MRU6);
  FMRUMenuItems.Add(MRU7);
  FMRUMenuItems.Add(MRU8);
  FMRUMenuItems.Add(MRU9);

  FieldsView.Hint := SFieldsViewHint + #13#10 + SFieldsViewHint2;
  RelationsView.Hint := SRelationsViewHint + #13#10 + SRelationsViewHint2;
  OpenDialog.Filter := SOpenFileFilter;
  ImportDialog.Filter := SImportFileFilter;
  SaveDialog.Filter := SSaveFileFilter;
  SaveDialog.DefaultExt := DefaultFileExt;
  ExportDialog.Filter := SExportFileFilter;
  ImportSettingsDlg.Filter := SImportSettingsFileFilter;
  ExportSettingsDlg.Filter := SExportSettingsFileFilter;

  ActiveControl := ObjectsView;

  ResizeObjectsViewColumns;
  ResizeFieldsViewColumns;
  ResizeRelationsViewColumns;
  ResizeRulesViewColumns;

  NotesView.UserDataFolder := WebDataPath;
  NotesView.CreateWebView;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SaveSettings(FAppFiles.FullPaths[IdSettings]);
  FMRUList.Free;
  FMRUMenuItems.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  InstallOnRemovableMedia.Visible := FAppFiles.FileExists(IdLauncher);
  LoadSettings(FAppFiles.FullPaths[IdSettings]);
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
var
  OpenFileName: string;
begin
  if (FPendingChanges <> []) and not FApplyingChanges then
  begin
    FApplyingChanges := True;
    try
      if ncTagList in FPendingChanges then
        UpdateTagList;

      if ncRuleList in FPendingChanges then
        UpdateRuleList;

      if ncObjectsView in FPendingChanges then
      begin
        UpdateObjectsView;
        FPendingChanges := FPendingChanges + [ncObjPropsViews, ncRelationsView];
      end;

      if ncObjPropsViews in FPendingChanges then
      begin
        UpdateFieldsView;
        UpdateNotesView;
      end;

      if ncRelationsView in FPendingChanges then
        UpdateRelationsView;

      if ncRulesView in FPendingChanges then
        UpdateRulesView;
    finally
      FPendingChanges := [];
      FApplyingChanges := False;
    end;
  end;

  if FOpenFileName <> '' then
  begin
    OpenFileName := FOpenFileName;
    FOpenFileName := '';
    LoadFromFile(OpenFileName);
  end;

  if ObjectsView.SelCount = 0 then
  begin
    StatusBar.Panels[0].Text :=
      Format(SStatusItemsCount, [ObjectsView.Items.Count]);
    StatusBar.Panels[1].Text := FFileName;
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

  if FAutoCheckForUpdates and (FLastCheckForUpdates < Today) then
  begin
    FLastCheckForUpdates := Today;
    FCheckForUpdatesSilent := True;
    CheckForUpdatesNow.Execute;
  end;
end;

procedure TMainForm.ObjectsViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  NotifyChanges([ncObjPropsViews, ncRelationsView]);
end;

procedure TMainForm.ObjectsViewColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  SortObjectsView(TObjectSortKind(Column.Tag));
end;

procedure TMainForm.ObjectsViewCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  case FSortKind of
    osName:
      Compare := CompareText(TO2Object(Item1.Data).Name,
        TO2Object(Item2.Data).Name) * FSortSign;
    osTags:
      Compare := CompareObjectsByTags(Item1.Data, Item2.Data) * FSortSign;
    osNextEvent:
      Compare := CompareObjectsByNextEvent(Item1.Data, Item2.Data) * FSortSign;
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
var
  Color, TextColor: TColor;
begin
  if (State * [cdsFocused, cdsHot] = []) and Assigned(Item.Data)
    and FModel.TryGetHighlightColors(TO2Object(Item.Data), Color,
      TextColor) then
  begin
    Sender.Canvas.Brush.Color := Color;
    Sender.Canvas.Font.Color := TextColor;
  end;
end;

procedure TMainForm.FindByEventChange(Sender: TObject);
begin
  FModel.EventFilterIndex := FindByEvent.ItemIndex;
  NotifyChanges([ncObjectsView]);
end;

procedure TMainForm.FindByNameChange(Sender: TObject);
begin
  if FModel = nil then Exit;

  FModel.ObjectName := FindByName.Text;

  Timer.Enabled := False;
  if FModel.ObjectName = '' then
    NotifyChanges([ncObjectsView])
  else
    Timer.Enabled := True;
end;

procedure TMainForm.FindByTagSelectAllExecute(Sender: TObject);
begin
  FindByTag.SelectAll;
  NotifyChanges([ncObjectsView]);
end;

procedure TMainForm.FindByTagClick(Sender: TObject);
var
  I: Integer;
begin
  FModel.IncludeUntagged := FindByTag.Selected[0];

  FModel.ObjectTags.Clear;
  for I := 1 to FindByTag.Count - 1 do
    if FindByTag.Selected[I] then
      FModel.ObjectTags.Add(FindByTag.Items[I]);

  NotifyChanges([ncObjectsView]);
end;

procedure TMainForm.FindByTagDeselectExecute(Sender: TObject);
begin
  FindByTag.ClearSelection;
  NotifyChanges([ncObjectsView]);
end;

procedure TMainForm.FindByTagInvertSelectionExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FindByTag.Count - 1 do
    FindByTag.Selected[I] := not FindByTag.Selected[I];
  NotifyChanges([ncObjectsView]);
end;

procedure TMainForm.FindByRuleSelectAllExecute(Sender: TObject);
begin
  FindByRule.SelectAll;
  NotifyChanges([ncObjectsView]);
end;

procedure TMainForm.FindByRuleClick(Sender: TObject);
var
  I: Integer;
begin
  FModel.ObjectRules.Clear;
  for I := 0 to FindByRule.Count - 1 do
    if FindByRule.Selected[I] then
      FModel.ObjectRules.Add(TO2Rule(FindByRule.Items.Objects[I]));

  NotifyChanges([ncObjectsView]);
end;

procedure TMainForm.FindByRuleDeselectExecute(Sender: TObject);
begin
  FindByRule.ClearSelection;
  NotifyChanges([ncObjectsView]);
end;

procedure TMainForm.FindByRuleInvertSelectionExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FindByRule.Count - 1 do
    FindByRule.Selected[I] := not FindByRule.Selected[I];
  NotifyChanges([ncObjectsView]);
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;
  NotifyChanges([ncObjectsView]);
end;

procedure TMainForm.MRUItemClick(Sender: TObject);
var
  MRUItem: TMRUItem;
begin
  MRUItem := FMRUList[TComponent(Sender).Tag];
  if CanCloseFile then
    if FileExists(MRUItem.Item) then
      LoadFromFile(MRUItem.Item)
    else
      if YesNoWarningBox(SRemoveFromMRUListQuery) then
      begin
        FMRUList.Remove(MRUItem);
        UpdateMRUList;
      end;
end;

procedure TMainForm.ActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not FBusy;
end;

procedure TMainForm.NewFileExecute(Sender: TObject);
begin
  if CanCloseFile then
  begin
    FModel.NewFile;
    Initialize;
    FileName := '';
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
  if CanCloseFile then LoadFromFile(FFileName);
end;

procedure TMainForm.ReopenFileUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not FBusy and (FFileName <> '');
end;

procedure TMainForm.OpenFolderExecute(Sender: TObject);
begin
  ShellOpen(ExtractFileDir(FFileName));
end;

procedure TMainForm.OpenFolderUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not FBusy and (FFileName <> '');
end;

procedure TMainForm.ClearMRUListExecute(Sender: TObject);
begin
  FMRUList.Clear;
  UpdateMRUList;
end;

procedure TMainForm.ClearMRUListUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FMRUList.Count > 0;
end;

procedure TMainForm.ImportExecute(Sender: TObject);
const
  ServiceNames: array [1..2] of string = (
    ImportFromO2FileService,
    ImportFromXmlFileService);
begin
  ImportDialog.FileName := '';
  if ImportDialog.Execute then
  begin
    BeginBatchOperation;
    try
      FServiceContainer.Resolve<IFileOperation>(
        ServiceNames[ImportDialog.FilterIndex]).Execute(ImportDialog.FileName);
    finally
      EndBatchOperation;
    end;
    Initialize;
  end;
end;

procedure TMainForm.SaveFileExecute(Sender: TObject);
begin
  if (FFileName = '')
    or FModel.O2File.Encrypted and ((FModel.O2File.Cipher in DeprecatedCiphers)
    or (FModel.O2File.Hash in DeprecatedHashes)) then
    SaveFileAs.Execute
  else
    SaveToFile(FFileName);
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
  ServiceNames: array [1..3] of string = (
    ExportToO2FileService,
    ExportToXmlFileService,
    ExportToIcsFileService);
begin
  ExportDialog.FileName := '';
  if ExportDialog.Execute then
  begin
    BeginBatchOperation;
    try
      FServiceContainer.Resolve<IFileOperation>(
        ServiceNames[ExportDialog.FilterIndex]).Execute(ExportDialog.FileName);
    finally
      EndBatchOperation;
    end;
  end;
end;

procedure TMainForm.ExportToHTMLExecute(Sender: TObject);
begin
  THTMLExport.Execute(FServiceContainer.Resolve<IHTMLExport>);
end;

procedure TMainForm.PrintFileExecute(Sender: TObject);
begin
  TPrintPreview.Execute(FServiceContainer.Resolve<IPrint>);
end;

procedure TMainForm.PrintFileUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not FBusy and (ObjectsView.Items.Count > 0);
end;

procedure TMainForm.FilePropsExecute(Sender: TObject);
begin
  TFilePropsDlg.Execute(FServiceContainer.Resolve<IFileProps>);
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

procedure TMainForm.InstallOnRemovableMediaExecute(Sender: TObject);
var
  Dir, AppExe: string;
begin
  Dir := '';
  if SelectDirectory(Format(SInstallOnRemovableMediaPrompt +
    #13#10 + SInstallOnRemovableMediaFolderPrompt,
    [FAppFiles.GetTotalSize / (1024 * 1024)]), '', Dir,
    [sdNewUI, sdNewFolder]) then
  begin
    BeginBatchOperation;
    try
      FAppFiles.InstallPortable(Dir);

      AppExe := FAppFiles.GetFullPortablePath(IdAppExe, Dir);
      DeleteFile(ChangeFileExt(AppExe, '.ENU'));
      DeleteFile(ChangeFileExt(AppExe, '.ITA'));
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
      AppVersionInfo: TAppVersionInfo;
      AppUpdateInfo: TAppUpdateInfo;
      DebugOutput: string;
    begin
      AppVersionInfo := FServiceContainer.Resolve<TAppVersionInfo>;
      try
        AppUpdateInfo := TAppUpdateInfo
          .Create(CheckForUpdatesResponse.JSONValue);

        DebugOutput := Format(DebugOutputFmt,
          [AppVersionInfo.Version.MajorVersion,
          AppVersionInfo.Version.MinorVersion,
          AppVersionInfo.Version.Release,
          AppVersionInfo.Version.Build,
          AppUpdateInfo.Version.MajorVersion,
          AppUpdateInfo.Version.MinorVersion,
          AppUpdateInfo.Version.Release,
          AppUpdateInfo.Version.Build,
          AppUpdateInfo.DownloadURL]);
        OutputDebugString(PChar(DebugOutput));

        if AppUpdateInfo.Version
          .Compare(AppVersionInfo.Version) = GreaterThanValue then
        begin
          if YesNoBox(Format(SDownloadUpdatesQuery,
            [AppUpdateInfo.Version.MajorVersion,
            AppUpdateInfo.Version.MinorVersion,
            AppUpdateInfo.Version.Release])) then
            ShellOpen(AppUpdateInfo.DownloadURL);
        end
        else if not FCheckForUpdatesSilent then
          InfoBox(SNoAvailableUpdates);
      except
        if not FCheckForUpdatesSilent then
          ErrorBox(SCannotCheckForUpdates);
      end;

      FCheckForUpdatesSilent := False;
    end,
    True,
    True,
    procedure (Obj: TObject)
    begin
      if not FCheckForUpdatesSilent then
        ErrorBox(SCannotCheckForUpdates);
    end
  );
end;

procedure TMainForm.CheckForUpdatesPeriodicallyExecute(Sender: TObject);
begin
  FAutoCheckForUpdates := not FAutoCheckForUpdates;
end;

procedure TMainForm.CheckForUpdatesPeriodicallyUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FAutoCheckForUpdates;
end;

procedure TMainForm.RefreshViewsExecute(Sender: TObject);
begin
  NotifyChanges([ncObjectsView, ncRulesView, ncTagList, ncRuleList]);
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
  SortObjectsView(osName);
end;

procedure TMainForm.SortByNameUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FSortKind = osName;
end;

procedure TMainForm.SortByTagsExecute(Sender: TObject);
begin
  SortObjectsView(osTags);
end;

procedure TMainForm.SortByTagsUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FSortKind = osTags;
end;

procedure TMainForm.SortByNextEventExecute(Sender: TObject);
begin
  SortObjectsView(osNextEvent);
end;

procedure TMainForm.SortByNextEventUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FSortKind = osNextEvent;
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
  TAction(Sender).Enabled := not FBusy;
  TAction(Sender).Checked := SearchBox.Visible;
end;

procedure TMainForm.ClearSearchExecute(Sender: TObject);
begin
  InitializeSearch;
  NotifyChanges([ncObjectsView]);
end;

procedure TMainForm.AboutExecute(Sender: TObject);
begin
  TAboutForm.Execute(FServiceContainer.Resolve<TAppVersionInfo>, FAppFiles);
end;

procedure TMainForm.WebExecute(Sender: TObject);
begin
  ShellOpen(TAction(Sender).Hint);
end;

function TMainForm.GetHasSelectedObject: Boolean;
begin
  Result := Assigned(ObjectsView.Selected);
end;

function TMainForm.GetSelectedObject: TO2Object;
begin
  Result := TO2Object(ObjectsView.Selected.Data);
end;

function TMainForm.GetSelectedRelation: TO2Relation;
begin
  Result := TO2Relation(RelationsView.Selected.Data);
end;

function TMainForm.GetSelectedRule: TO2Rule;
begin
  Result := TO2Rule(RulesView.Selected.Data);
end;

function TMainForm.GetHasSelectedField: Boolean;
begin
  Result := Assigned(FieldsView.Selected);
end;

function TMainForm.GetSelectedField: TO2Field;
begin
  Result := TO2Field(FieldsView.Selected.Data);
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

procedure TMainForm.SetServiceContainer(const Value: TContainer);
begin
  if FServiceContainer <> Value then
  begin
    FServiceContainer := Value;

    FModel := FServiceContainer.Resolve<IFileManager>;
    FAppFiles := FServiceContainer.Resolve<IAppFiles>;
    FStorage := FServiceContainer.Resolve<IStorage>;

    FindByEvent.Items := FModel.EventFilters;

    Initialize;
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
  if FModel.O2File.Modified then
    case YesNoCancelBox(SSaveChangesQuery) of
      ID_YES:
      begin
        SaveFile.Execute;
        Result := not FModel.O2File.Modified;
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
  NotifyChanges([ncObjectsView, ncRulesView, ncTagList, ncRuleList]);
end;

procedure TMainForm.InitializeSearch;
begin
  FModel.ObjectName := '';
  FModel.EventFilterIndex := 0;
  FModel.IncludeUntagged := False;
  FModel.ObjectTags.Clear;
  FModel.ObjectRules.Clear;

  FindByName.Clear;
  FindByEvent.ItemIndex := 0;
  FindByTag.ClearSelection;
  FindByRule.ClearSelection;
end;

procedure TMainForm.OpenNewInstance(const AFileName: string);
var
  AppExe, Parameters: string;
begin
  if PortablePath <> '' then
    Parameters := 'portable "' + PortablePath + '" '
  else
    Parameters := '';

  if AFileName <> '' then
    Parameters := Parameters + '"' + AFileName + '"';

  AppExe := FAppFiles.FullPaths[IdAppExe];
  ShellExecute(Application.Handle, 'open', PChar(AppExe), PChar(Parameters),
    PChar(ExtractFileDir(AppExe)), SW_SHOWNORMAL);
end;

procedure TMainForm.LoadFromFile(const AFileName: string);
begin
  BeginBatchOperation;
  try
    FModel.LoadFromFile(AFileName);
  finally
    EndBatchOperation;
  end;

  Initialize;
  FileName := FModel.O2File.FileName;
end;

procedure TMainForm.SaveToFile(const AFileName: string; Copy: Boolean);
begin
  BeginBatchOperation;
  try
    FModel.SaveToFile(AFileName, Copy);
  finally
    EndBatchOperation;
  end;

  if Copy then
    FModel.O2File.FileName := FFileName
  else
    FileName := FModel.O2File.FileName;
end;

function TMainForm.ObjToListItem(const AObject: TO2Object;
  const Item: TListItem): TListItem;
var
  EventDate: TDateTime;
begin
  if Assigned(Item) then
    Result := Item
  else
    Result := ObjectsView.Items.Add;
  Result.Caption := AObject.Name;
  Result.ImageIndex := 0;
  Result.SubItems.Add(AObject.Tag);
  if FModel.TryGetNextEvent(AObject, EventDate) then
    Result.SubItems.Add(DateToStr(EventDate))
  else
    Result.SubItems.Add('');
  Result.Data := AObject;
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
  Result.SubItems.Add(FModel.GetDisplayText(AField, FShowPasswords));
  Result.Data := AField;
end;

function TMainForm.RelationToListItem(const ARelation: TO2Relation;
  const AObject: TO2Object; const Item: TListItem): TListItem;
var
  AObjRelation: TO2ObjRelation;
begin
  AObjRelation := FModel.O2File.GetObjectRelation(AObject, ARelation);
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
  Result.SubItems.Add(RuleTypes[ARule.RuleType]);
  Result.SubItems.Add(ARule.FieldName);
  Result.SubItems.Add(ARule.FieldValue);
  Result.Data := ARule;
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
  NotifyChanges([ncObjectsView, ncRuleList]);
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
      NotifyChanges([ncObjectsView, ncRuleList]);
    end;
  end;
end;

procedure TMainForm.SortObjectsView(SortKind: TObjectSortKind);
begin
  if FSortKind = SortKind then
    FSortSign := -FSortSign
  else
  begin
    FSortKind := SortKind;
    FSortSign := 1;
  end;
  ObjectsView.AlphaSort;
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
  Date1, Date2: TDateTime;
begin
  if FModel.TryGetNextEvent(Obj1, Date1) then
  begin
    if FModel.TryGetNextEvent(Obj2, Date2) then
      Result := CompareDate(Date1, Date2)
    else
      Result := -1;
  end
  else
  begin
    if FModel.TryGetNextEvent(Obj2, Date2) then
      Result := 1
    else
      Result := 0;
  end
end;

procedure TMainForm.BeginBatchOperation;
begin
  Inc(FBatchOperationCount);
  SetBusy(FBatchOperationCount > 0);
end;

procedure TMainForm.EndBatchOperation;
begin
  Dec(FBatchOperationCount);
  SetBusy(FBatchOperationCount > 0);
end;

procedure TMainForm.UpdateAllActions;
var
  I: Integer;
begin
  for I := 0 to ActionList.ActionCount - 1 do
    ActionList.Actions[I].Update;
end;

procedure TMainForm.NotifyChanges(Changes: TNotifyChanges);
begin
  if not FApplyingChanges then
    FPendingChanges := FPendingChanges + Changes;
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
  SelectedItemsData: TList;
  AObject: TO2Object;
begin
  SelectedItemsData := ObjectsView.ListSelectedItemsData;
  try
    ObjectsView.Items.BeginUpdate;
    try
      ObjectsView.Clear;
      for AObject in FModel.GetObjects do ObjToListItem(AObject, nil);

      ObjectsView.AlphaSort;

      ResizeObjectsViewColumns;
      ObjectsView.SelectItemsByData(SelectedItemsData);
    finally
      ObjectsView.Items.EndUpdate;
    end;
  finally
    SelectedItemsData.Free;
  end;
end;

procedure TMainForm.UpdateFieldsView;
var
  SelectedItemsData: TList;
  AField: TO2Field;
begin
  SelectedItemsData := FieldsView.ListSelectedItemsData;
  try
    FieldsView.Items.BeginUpdate;
    try
      FieldsView.Clear;
      if HasSelectedObject then
        for AField in SelectedObject.Fields do
          FieldToListItem(AField, nil);
      ResizeFieldsViewColumns;
      FieldsView.SelectItemsByData(SelectedItemsData);
    finally
      FieldsView.Items.EndUpdate;
    end;
  finally
    SelectedItemsData.Free;
  end;
end;

procedure TMainForm.UpdateNotesView;
var
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    SB.Append('<!DOCTYPE html><html>')
      .AppendContextMenuBlockerScript
      .Append('<body style="color: #000; background-color: #fff; font-size: 1rem; font-family: ');

    if HasSelectedObject and (SelectedObject.TextType = ttCommonMark) then
      SB.Append('sans-serif;">')
    else
      SB.Append('monospace;">');

    if HasSelectedObject then
      SB.AppendHTML(SelectedObject.Text, SelectedObject.TextType);

    SB.Append('</body></html>');

    NotesView.NavigateToString(SB.ToString);
  finally
    SB.Free;
  end;
end;

procedure TMainForm.UpdateRelationsView;
var
  AObjRelation: TO2ObjRelation;
  ObjRelations: TO2ObjRelations;
  SelectedItemsData: TList;
begin
  SelectedItemsData := RelationsView.ListSelectedItemsData;
  try
    RelationsView.Items.BeginUpdate;
    try
      RelationsView.Clear;
      if HasSelectedObject then
      begin
        ObjRelations := FModel.O2File.GetObjectRelations(SelectedObject);
        try
          for AObjRelation in ObjRelations do
            RelationToListItem(AObjRelation, nil);
        finally
          ObjRelations.Free;
        end;
      end;
      ResizeRelationsViewColumns;
      RelationsView.SelectItemsByData(SelectedItemsData);
    finally
      RelationsView.Items.EndUpdate;
    end;
  finally
    SelectedItemsData.Free;
  end;
end;

procedure TMainForm.UpdateRulesView;
var
  ARule: TO2Rule;
  SelectedItemsData: TList;
begin
  SelectedItemsData := RulesView.ListSelectedItemsData;
  try
    RulesView.Items.BeginUpdate;
    try
      RulesView.Clear;
      for ARule in FModel.O2File.Rules do RuleToListItem(ARule, nil);
      ResizeRulesViewColumns;
      RulesView.SelectItemsByData(SelectedItemsData);
      if Assigned(RulesView.Selected) then
      begin
        RulesView.Selected.Focused := True;
        RulesView.Selected.MakeVisible(False);
      end;
    finally
      RulesView.Items.EndUpdate;
    end;
  finally
    SelectedItemsData.Free;
  end;
end;

procedure TMainForm.UpdateTagList;
var
  I: Integer;
begin
  FindByTag.Items := FModel.Tags;

  I := 0;
  while I < FModel.ObjectTags.Count do
    if FModel.Tags.IndexOf(FModel.ObjectTags[I]) = -1 then
      FModel.ObjectTags.Delete(I)
    else
      Inc(I);

  for I := 1 to FindByTag.Count - 1 do
    if FModel.ObjectTags.IndexOf(FindByTag.Items[I]) <> -1 then
      FindByTag.Selected[I] := True;

  FindByTag.Selected[0] := FModel.IncludeUntagged;
end;

procedure TMainForm.UpdateRuleList;
var
  ARule: TO2Rule;
  I: Integer;
begin
  FindByRule.Items.BeginUpdate;
  try
    FindByRule.Items.Clear;
    for ARule in FModel.O2File.Rules do
      if ARule.Active then
        FindByRule.AddItem(ARule.Name, ARule);
  finally
    FindByRule.Items.EndUpdate;
  end;

  I := 0;
  while I < FModel.ObjectRules.Count do
    if not FModel.ObjectRules[I].Active
      or (FModel.O2File.Rules.IndexOf(FModel.ObjectRules[I]) = -1) then
      FModel.ObjectRules.Delete(I)
    else
      Inc(I);

  for I := 0 to FindByRule.Count - 1 do
    if FModel.ObjectRules.IndexOf(
      TO2Rule(FindByRule.Items.Objects[I])) <> -1 then
      FindByRule.Selected[I] := True;
end;

procedure TMainForm.UpdateMRUList(const AFileName: string);
var
  I, J: Integer;
begin
  if AFileName <> '' then FMRUList.AddItem(AFileName);

  J := 0;
  for I := 0 to FMRUMenuItems.Count - 1 do
  begin
    while (J < FMRUList.Count) and SameText(FMRUList[J].Item, AFileName) do
      Inc(J);

    with TMenuItem(FMRUMenuItems[I]) do
      if J < FMRUList.Count then
      begin
        Caption := IntToStr(Succ(I)) + ' '
          + MinimizeName(FMRUList[J].Item, Canvas, 400);
        Visible := True;
        Tag := J;
        Inc(J);
      end
      else
        Visible := False;
  end;
end;

procedure TMainForm.LoadMRUList;
var
  I, Count: Integer;
begin
  FMRUList.Clear;
  Count := FStorage.ReadInteger(IdMRUList, 0);
  for I := 0 to Count - 1 do
    FMRUList.Add(TMRUItem.Create(
      FStorage.ReadString(Format(IdMRUListItemFmt, [I])),
      FStorage.ReadInteger(Format(IdMRUListItemCntFmt, [I]), 1)));
end;

procedure TMainForm.SaveMRUList;
var
  I: Integer;
begin
  FStorage.WriteInteger(IdMRUList, FMRUList.Count);
  for I := 0 to FMRUList.Count - 1 do
  begin
    FStorage.WriteString(Format(IdMRUListItemFmt, [I]), FMRUList[I].Item);
    FStorage.WriteInteger(Format(IdMRUListItemCntFmt, [I]), FMRUList[I].Count);
  end;
end;

procedure TMainForm.LoadSettings(const AFileName: string);
const
  SortSigns: array[Boolean] of Integer = (-1, 1);
begin
  FStorage.LoadFromFile(AFileName);

  LoadMRUList;
  UpdateMRUList;

  StayOnTop := FStorage.ReadBoolean(IdStayOnTop, False);
  FTransparencyOnlyIfDeactivated :=
    FStorage.ReadBoolean(IdTransparencyOnlyIfDeactivated, True);
  Transparency := FStorage.ReadInteger(IdTransparency, 0);
  FAutoCheckForUpdates := FStorage.ReadBoolean(IdAutoCheckForUpdates, True);
  FLastCheckForUpdates := FStorage.ReadFloat(IdLastCheckForUpdates, Yesterday);
  ObjectsView.ViewStyle := TViewStyle(ReadIntIdent(FStorage, IdViewStyle,
    ViewStyles, Integer(vsIcon)));
  FSortKind := TObjectSortKind(ReadIntIdent(FStorage, IdSortKind,
    SortKinds, Integer(osName)));
  FSortSign := SortSigns[FStorage.ReadBoolean(IdSortAscending, True)];
end;

procedure TMainForm.SaveSettings(const AFileName: string);
begin
  SaveMRUList;

  FStorage.WriteBoolean(IdStayOnTop, StayOnTop);
  FStorage.WriteInteger(IdTransparency, Transparency);
  FStorage.WriteBoolean(IdAutoCheckForUpdates, FAutoCheckForUpdates);
  FStorage.WriteBoolean(IdTransparencyOnlyIfDeactivated,
    FTransparencyOnlyIfDeactivated);
  FStorage.WriteFloat(IdLastCheckForUpdates, FLastCheckForUpdates);
  WriteIntIdent(FStorage, IdViewStyle, ViewStyles,
    Integer(ObjectsView.ViewStyle));
  WriteIntIdent(FStorage, IdSortKind, SortKinds, Integer(FSortKind));
  FStorage.WriteBoolean(IdSortAscending, FSortSign > 0);

  SysUtils.ForceDirectories(ExtractFileDir(AFileName));
  FStorage.SaveToFile(AFileName);
end;

procedure TMainForm.ObjectMenuPopup(Sender: TObject);
var
  Item: TMenuItem;
  Tags: TStrings;
  Tag: string;
begin
  AddTag.Clear;
  DeleteTag.Clear;
  if HasSelectedObject then
  begin
    Tags := TStringList.Create;
    try
      AppendTagsToList(FModel.O2File.Objects.ToEnumerable, Tags);
      for Tag in Tags do
      begin
        Item := TMenuItem.Create(Self);
        Item.Caption := StringReplace(Tag, '&', '&&', [rfReplaceAll]);
        Item.OnClick := AddTagClick;
        AddTag.Add(Item);
      end;
      AddTag.Enabled := AddTag.Count > 0;

      Tags.Clear;
      AppendTagsToList(FSelectedObjects, Tags);
      for Tag in Tags do
      begin
        Item := TMenuItem.Create(Self);
        Item.Caption := StringReplace(Tag, '&', '&&', [rfReplaceAll]);
        Item.OnClick := DeleteTagClick;
        DeleteTag.Add(Item);
      end;
      DeleteTag.Enabled := DeleteTag.Count > 0;
    finally
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
  Model: IObjectProps;
  Item: TListItem;
begin
  Model := FServiceContainer.Resolve<IObjectProps>(NewObjectService);
  if TObjPropsDlg.Execute(Model, pgGeneral) then
  begin
    Item := ObjToListItem(Model.O2Object, nil);
    ObjectsView.ClearSelection;
    Item.Selected := True;
    Item.Focused := True;
    NotifyChanges([ncObjPropsViews, ncRelationsView, ncTagList]);
  end;
end;

procedure TMainForm.DuplicateObjectExecute(Sender: TObject);
var
  Model: IObjectProps;
  Item: TListItem;
begin
  Model := FServiceContainer.Resolve<IObjectProps>(DuplicateObjectService);
  if TObjPropsDlg.Execute(Model, pgGeneral) then
  begin
    Item := ObjToListItem(Model.O2Object, nil);
    ObjectsView.ClearSelection;
    Item.Selected := True;
    Item.Focused := True;
    NotifyChanges([ncObjPropsViews, ncRelationsView, ncTagList]);
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
  Model: IObjectProps;
begin
  Model := FServiceContainer.Resolve<IObjectProps>(EditObjectService);
  if TObjPropsDlg.Execute(Model, pgGeneralTags) then
  begin
    ObjToListItem(Model.O2Object, ObjectsView.Selected);
    NotifyChanges([ncObjPropsViews, ncTagList]);
  end;
end;

procedure TMainForm.AddTagClick(Sender: TObject);
var
  AObject: TO2Object;
  Tag: string;
begin
  Tag := StringReplace(TMenuItem(Sender).Caption, '&&', '&', [rfReplaceAll]);
  for AObject in FSelectedObjects do
    AObject.AddTag(Tag);
end;

procedure TMainForm.DeleteTagClick(Sender: TObject);
var
  AObject: TO2Object;
  Tag: string;
begin
  Tag := StringReplace(TMenuItem(Sender).Caption, '&&', '&', [rfReplaceAll]);
  for AObject in FSelectedObjects do
    AObject.DeleteTag(Tag);
  NotifyChanges([ncObjectsView, ncTagList]);
end;

procedure TMainForm.ReplaceTagExecute(Sender: TObject);
begin
  if TReplaceDlg.Execute(
    FServiceContainer.Resolve<IReplaceOperation>(ReplaceTagService)) then
    NotifyChanges([ncObjectsView, ncTagList]);
end;

procedure TMainForm.ReplaceFieldNameExecute(Sender: TObject);
begin
  if TReplaceDlg.Execute(
    FServiceContainer.Resolve<IReplaceOperation>(ReplaceFieldNameService)) then
    NotifyChanges([ncObjectsView]);
end;

procedure TMainForm.ReplaceFieldValueExecute(Sender: TObject);
begin
  if TReplaceDlg.Execute(
    FServiceContainer.Resolve<IReplaceOperation>(ReplaceFieldValueService)) then
    NotifyChanges([ncObjectsView]);
end;

procedure TMainForm.ReplaceRoleExecute(Sender: TObject);
begin
  if TReplaceDlg.Execute(
    FServiceContainer.Resolve<IReplaceOperation>(ReplaceRoleService)) then
    NotifyChanges([ncRelationsView]);
end;

procedure TMainForm.ObjectPropsExecute(Sender: TObject);
var
  Page: TObjPropsDlgPage;
  Model: IObjectProps;
begin
  case PageControl.ActivePageIndex of
    0: Page := pgFields;
    1: Page := pgNotes;
    else Page := pgGeneral;
  end;
  Model := FServiceContainer.Resolve<IObjectProps>(EditObjectService);
  if TObjPropsDlg.Execute(Model, Page) then
  begin
    ObjToListItem(Model.O2Object, ObjectsView.Selected);
    NotifyChanges([ncObjPropsViews, ncTagList]);
  end;
end;

procedure TMainForm.ObjectActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not FBusy and HasSelectedObject;
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
var
  Color, TextColor: TColor;
begin
  if (State * [cdsFocused, cdsHot] = []) and Assigned(Item.Data)
    and FModel.TryGetHighlightColors(TO2Field(Item.Data), Color,
      TextColor) then
  begin
    Sender.Canvas.Brush.Color := Color;
    Sender.Canvas.Font.Color := TextColor;
  end;
end;

procedure TMainForm.FieldsViewCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var
  Color, TextColor: TColor;
begin
  if Assigned(Item.Data) then
  begin
    if (State * [cdsFocused, cdsHot] = [])
      and FModel.TryGetHighlightColors(TO2Field(Item.Data), Color,
        TextColor) then
    begin
      Sender.Canvas.Brush.Color := Color;
      Sender.Canvas.Font.Color := TextColor;
    end;

    if FModel.IsHyperlinkOrEmail(Item.Data) then
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
  TAction(Sender).Enabled := not FBusy and HasSelectedField;
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
  TAction(Sender).Enabled := not FBusy and (FieldsView.Items.Count > 0);
end;

procedure TMainForm.OpenLinkExecute(Sender: TObject);
begin
  ShellOpen(FModel.GetHyperLink(SelectedField));
end;

procedure TMainForm.OpenLinkUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := not FBusy
    and HasSelectedField and FModel.IsHyperlink(SelectedField);
  TAction(Sender).Enabled := TAction(Sender).Visible;
end;

procedure TMainForm.SendEmailExecute(Sender: TObject);
begin
  ShellMailTo(SelectedField.FieldValue);
end;

procedure TMainForm.SendEmailUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := not FBusy
    and HasSelectedField and FModel.IsEmail(SelectedField);
  TAction(Sender).Enabled := TAction(Sender).Visible;
end;

procedure TMainForm.ShowPasswordsExecute(Sender: TObject);
begin
  FShowPasswords := not FShowPasswords;
  NotifyChanges([ncObjPropsViews]);
end;

procedure TMainForm.ShowPasswordsUpdate(Sender: TObject);
begin
  TAction(sender).Checked := FShowPasswords;
  TAction(sender).Enabled := not FBusy;
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
  Model: IRelationProps;
begin
  Model := FServiceContainer.Resolve<IRelationProps>(NewRelationService);
  if TRelationPropsDlg.Execute(Model) then
    RelationToListItem(Model.Relation, SelectedObject, nil);
end;

procedure TMainForm.NewRelationUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not FBusy and (ObjectsView.SelCount = 2);
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
  AObjRelation := FModel.O2File.GetObjectRelation(SelectedObject,
    SelectedRelation);
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
  Model: IRelationProps;
begin
  Model := FServiceContainer.Resolve<IRelationProps>(EditRelationService);
  if TRelationPropsDlg.Execute(Model) then
    RelationToListItem(Model.Relation, SelectedObject, RelationsView.Selected);
end;

procedure TMainForm.RelationActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not FBusy and Assigned(RelationsView.Selected);
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
  Model: IRuleProps;
  Item: TListItem;
begin
  Model := FServiceContainer.Resolve<IRuleProps>(NewRuleService);
  if TRulePropsDlg.Execute(Model) then
  begin
    Item := RuleToListItem(Model.Rule, nil);
    Item.Selected := True;
    Item.Focused := True;
    NotifyChanges([ncObjectsView, ncRuleList]);
  end;
end;

procedure TMainForm.DuplicateRuleExecute(Sender: TObject);
var
  Model: IRuleProps;
  Item: TListItem;
begin
  Model := FServiceContainer.Resolve<IRuleProps>(DuplicateRuleService);
  if TRulePropsDlg.Execute(Model) then
  begin
    Item := RuleToListItem(Model.Rule, nil);
    Item.Selected := True;
    Item.Focused := True;
    NotifyChanges([ncObjectsView, ncRuleList]);
  end;
end;

procedure TMainForm.DeleteRuleExecute(Sender: TObject);
begin
  if YesNoBox(SDeleteRuleQuery) then
  begin
    RulesView.FreeSelectedItemsData;
    RulesView.DeleteSelected;
    NotifyChanges([ncObjectsView, ncRuleList]);
  end;
end;

procedure TMainForm.MoveUpRuleExecute(Sender: TObject);
begin
  with TO2Rule(RulesView.Selected.Data) do Index := Index - 1;
  NotifyChanges([ncObjectsView, ncRulesView]);
end;

procedure TMainForm.MoveUpRuleUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not FBusy and Assigned(RulesView.Selected)
    and (TO2Rule(RulesView.Selected.Data).Index > 0);
end;

procedure TMainForm.MoveDownRuleExecute(Sender: TObject);
begin
  with TO2Rule(RulesView.Selected.Data) do Index := Index + 1;
  NotifyChanges([ncObjectsView, ncRulesView]);
end;

procedure TMainForm.MoveDownRuleUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not FBusy and Assigned(RulesView.Selected)
    and (TO2Rule(RulesView.Selected.Data).Index < FModel.O2File.Rules.Count - 1)
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
  Model: IRuleProps;
begin
  Model := FServiceContainer.Resolve<IRuleProps>(EditRuleService);
  if TRulePropsDlg.Execute(Model) then
  begin
    RuleToListItem(Model.Rule, RulesView.Selected);
    NotifyChanges([ncObjectsView, ncRuleList]);
  end;
end;

procedure TMainForm.RuleActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not FBusy and Assigned(RulesView.Selected);
end;

procedure TMainForm.SaveDialogCanClose(Sender: TObject;
  var CanClose: Boolean);
begin
  if not FileExists(SaveDialog.FileName) or YesNoBox(SFileOverwriteQuery) then
    CanClose := TSetPasswordDlg.Execute(
      FServiceContainer.Resolve<IEncryptionProps>)
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

end.

