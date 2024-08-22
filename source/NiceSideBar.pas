{-------------------------------------------------------------------------------

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

     The Original Code is NiceSideBar.pas released at May 26th, 2007.
     The Initial Developer of the Original Code is Priyatna.
     (Website: http://www.priyatna.org Email: me@priyatna.org)
     All Rights Reserved.

     Contributors:
     - Carl Stuffer (cstuffer@hotmail.com), fixing some mouse hover bugs
     
Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

-------------------------------------------------------------------------------}

unit NiceSideBar;

{$IFDEF FPC}
 {$MODE Delphi}
 {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLIntf, LCLType, LMessages, LazLoggerBase,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  Graphics, SysUtils, Types, Controls, Classes, ImgList, Math,
  ExtCtrls, Forms;

const
  clDefaultHover = $00AAFFFF; //clYellow;
  clDefaultSelected = clBtnHighlight;
  clDefaultHoverFont = clRed;

type
  TSideBarState = (ssNormal, ssHover, ssSelected, ssDisabled);

  TSideBarStates = set of TSideBarState;

  TSideBarAlign = (saLeft, saCenter, saRight);

  TSideBarBullet = (sbRound, sbRectangle, sbDiamond);

  TSideBarEvent = procedure (Sender: TObject; Index, SubIndex: Integer;
    Caption: string) of object;

  TSideBarCustomDrawItem = procedure (Sender: TObject; ACanvas: TCanvas;
    Rc: TRect; Str: string; States: TSideBarStates; ImageIndex: Integer) of object;

  TSideBarCustomDrawSubItem = procedure (Sender: TObject; ACanvas: TCanvas;
    Rc: TRect; Str: string; States: TSideBarStates) of object;

  TSideBarCustomDrawNonItem = procedure (Sender: TObject; ACanvas: TCanvas;
    Rc: TRect) of object;

  TSideBarCustomDrawScroller= procedure (Sender: TObject; ACanvas: TCanvas;
    Rc: TRect; Up: Boolean; Hover: Boolean) of object;

  TNiceSideBar = class;

  TSideBarItem = class(TCollectionItem)
  private
    FCaption: string;
    FImageIndex: TImageIndex;
    FItems: TStringList;
    FTag: Integer;
    FExpanded: Boolean;
    FStates: TList;
    FVisible: Boolean;
    FEnabled: Boolean;
    function GetSideBar: TNiceSideBar;
    procedure SetCaption(Value: string);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetItems(Value: TStringList);
    procedure SetExpanded(const Value: Boolean);
    procedure ItemsChange(Sender: TObject);
    function GetItemEnabled(Index: Integer): Boolean;
    function GetItemVisible(Index: Integer): Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetItemEnabled(Index: Integer; const Value: Boolean);
    procedure SetItemVisible(Index: Integer; const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDisplayName: string; override;
    procedure Expand;
    procedure Collapse;
    property ItemEnabled[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
    property ItemVisible[Index: Integer]: Boolean read GetItemVisible write SetItemVisible;
  published
    property SideBar: TNiceSideBar read GetSideBar;
    property Caption: string read FCaption write SetCaption;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Items: TStringList read FItems write SetItems;
    property Tag: Integer read FTag write FTag default 0;
    property Expanded: Boolean read FExpanded write SetExpanded default True;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TSideBarItems = class(TCollection)
  private
    FSideBar: TNiceSideBar;
    function GetItem(Index: Integer): TSideBarItem;
    procedure SetItem(Index: Integer; Value: TSideBarItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ASideBar: TNiceSideBar);
    property SideBar: TNiceSideBar read FSideBar;
    property Items[Index: Integer]: TSideBarItem read GetItem write SetItem; default;
    function Add: TSideBarItem;
    function AddItem(Item: TSideBarItem; Index: Integer): TSideBarItem;
    function Insert(Index: Integer): TSideBarItem;
  end;

  TSideBarItemStyle = class(TPersistent)
  private
    FSideBar: TNiceSideBar;
    FSelectedColor: TColor;
    FHoverColor: TColor;
    FNormalColor: TColor;
    FSelectedFont: TFont;
    FDisabledFont: TFont;
    FHoverFont: TFont;
    FNormalFont: TFont;
    FLineColor: TColor;
    procedure FontChange(Sender: TObject);
    procedure SetHoverFont(const Value: TFont);
    procedure SetNormalColor(const Value: TColor);
    procedure SetNormalFont(const Value: TFont);
    procedure SetSelectedColor(const Value: TColor);
    procedure SetSelectedFont(const Value: TFont);
    procedure SetLineColor(const Value: TColor);
    procedure Deactivate;
    procedure SetDisabledFont(const Value: TFont);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(SideBar: TNiceSideBar);
    destructor Destroy; override;
    procedure Activate;
  published
    property NormalFont: TFont read FNormalFont write SetNormalFont;
    property HoverFont: TFont read FHoverFont write SetHoverFont;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
    property DisabledFont: TFont read FDisabledFont write SetDisabledFont;
    property NormalColor: TColor read FNormalColor write SetNormalColor default clBtnFace;
    property HoverColor: TColor read FHoverColor write FHoverColor default clDefaultHover;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clDefaultSelected;
    property LineColor: TColor read FLineColor write SetLineColor default clWindowText;
  end;

  TSideBarBulletStyle = class(TPersistent)
  private
    FSideBar: TNiceSideBar;
    FHoverColor: TColor;
    FSelectedPenColor: TColor;
    FNormalColor: TColor;
    FHoverPenColor: TColor;
    FNormalPenColor: TColor;
    FSelectedColor: TColor;
    FVisible: Boolean;
    FKind: TSideBarBullet;
    FSize: Integer;
    FDisabledPenColor: TColor;
    FDisabledColor: TColor;
    procedure SetNormalColor(const Value: TColor);
    procedure SetNormalPenColor(const Value: TColor);
    procedure SetSelectedColor(const Value: TColor);
    procedure SetSelectedPenColor(const Value: TColor);
    procedure SetKind(const Value: TSideBarBullet);
    procedure SetVisible(const Value: Boolean);
    procedure SetSize(const Value: Integer);
    procedure SetDisabledColor(const Value: TColor);
    procedure SetDisabledPenColor(const Value: TColor);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(SideBar: TNiceSideBar);
  published  
    property Visible: Boolean read FVisible write SetVisible default True;
    property Size: Integer read FSize write SetSize default 5;
    property Kind: TSideBarBullet read FKind write SetKind default sbRound;
    property NormalColor: TColor read FNormalColor write SetNormalColor default clWindowText;
    property HoverColor: TColor read FHoverColor write FHoverColor default clDefaultHoverFont;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clWindowText;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clGrayText;
    property NormalPenColor: TColor read FNormalPenColor write SetNormalPenColor default clWindowText;
    property HoverPenColor: TColor read FHoverPenColor write FHoverPenColor default clDefaultHoverFont;
    property SelectedPenColor: TColor read FSelectedPenColor write SetSelectedPenColor default clWindowText;
    property DisabledPenColor: TColor read FDisabledPenColor write SetDisabledPenColor default clGrayText;
  end;

  TSideBarScrollerStyle = class(TPersistent)
  private
    FSideBar: TNiceSideBar;
    FHoverColor: TColor;
    FNormalArrowColor: TColor;
    FNormalColor: TColor;
    FHoverArrowColor: TColor;
    procedure SetNormalArrowColor(const Value: TColor);
    procedure SetNormalColor(const Value: TColor);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(SideBar: TNiceSideBar);
  published  
    property NormalColor: TColor read FNormalColor write SetNormalColor default clBlack;
    property HoverColor: TColor read FHoverColor write FHoverColor default clWhite;
    property NormalArrowColor: TColor read FNormalArrowColor write SetNormalArrowColor default clWhite;
    property HoverArrowColor: TColor read FHoverArrowColor write FHoverArrowColor default clBlack;
  end;

  TNiceSideBar = class(TCustomPanel)
  private
    FList: TList;
    FItems: TSideBarItems;
    FAlignment: TSideBarAlign;
    FHandPointCursor: Boolean;
    FItemIndex: Integer;
    FSubItemIndex: Integer;
    FItemHeight: Integer;
    FSubItemHeight: Integer;
    FImages: TImageList;
    FHoverImages: TImageList;
    FSelectedImages: TImageList;
    FDisabledImages: TImageList;
    FOnHover: TSideBarEvent;
    FOnSelect: TSideBarEvent;
    FOnCustomDrawItem: TSideBarCustomDrawItem;
    FOnCustomDrawSubItem: TSideBarCustomDrawSubItem;
    FOnCustomDrawNonItem: TSideBarCustomDrawNonItem;
    FOnCustomDrawScroller: TSideBarCustomDrawScroller;
    TopIndex, BottomIndex: Integer;
    DeltaY: Integer;
    LastIndex: Integer;
    LastSubIndex: Integer;
    LastHover, HoverIndex: Integer;
    ScTop, ScBottom: TRect;
    ScTopVisible, ScBottomVisible: Boolean;
    FMargin: Integer;
    FGroupSeparator: Integer;
    IsUpdating: Boolean;
    FIndent: Integer;
    FAlwaysExpand: Boolean;
    FItemStyle: TSideBarItemStyle;
    FSubItemStyle: TSideBarItemStyle;
    FBullets: TSideBarBulletStyle;
    FScrollers: TSideBarScrollerStyle;
    {$IFDEF FPC}
    procedure CMColorChanged(var Msg: TLMessage); message CM_COLORCHANGED;
    procedure CMMouseLeave(var Msg: TLMessage); message CM_MOUSELEAVE;
    procedure CMWantSpecialKey(var Message: TLMKey); message CM_WANTSPECIALKEY;
    procedure WMEraseBkgnd(var Msg: TLMessage); message LM_ERASEBKGND;
    procedure WMSize(var Msg: TLMSize); message LM_SIZE;
    procedure WMMouseWheel(var Msg: TLMMouseEvent); message LM_MOUSEWHEEL;
    {$ELSE}
    procedure CMColorChanged(var Msg: TMessage); message CM_COLORCHANGED;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMWantSpecialKey(var Message: TWMKey); message CM_WANTSPECIALKEY;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    {$ENDIF}
    function IsStoredItemHeight: Boolean;
    function IsStoredSubItemHeight: Boolean;
    function IsStoredMargin: Boolean;
    function IsStoredIndent: Boolean;
    function IsStoredGroupSeparator: Boolean;
    procedure SetItems(Value: TSideBarItems);
    procedure SetItemIndex(Value: Integer);
    procedure SetSubItemIndex(Value: Integer);
    procedure SetItemHeight(Value: Integer);
    procedure SetAlignment(Value: TSideBarAlign);
    procedure SetSubItemHeight(Value: Integer);
    procedure SetImages(Value: TImageList);
    procedure SetHoverImages(Value: TImageList);
    procedure SetSelectedImages(Value: TImageList);
    procedure SetDisabledImages(Value: TImageList);
    procedure SetHandPointCursor(Value: Boolean);
    procedure ClearList;
    procedure ListChange(RebuildItems: Boolean);
    procedure DoDrawItem(Index: Integer);
    function GetIndexAtPos(X, Y: Integer): Integer;
    function CreateItem: TSideBarItem;
    procedure UpdateItem(Index: Integer);
    procedure UpdateItems;
    procedure SetMargin(const Value: Integer);
    procedure SetGroupSeparator(const Value: Integer);
    procedure SetIndent(const Value: Integer);
    procedure SetAlwaysExpand(const Value: Boolean);
    procedure SetItemStyle(const Value: TSideBarItemStyle);
    procedure SetSubItemStyle(const Value: TSideBarItemStyle);
    procedure SetBullets(const Value: TSideBarBulletStyle);
    procedure SetScrollers(const Value: TSideBarScrollerStyle);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure DrawItem(ACanvas: TCanvas; Rc: TRect; Str: string; States: TSideBarStates;
      ImageIndex: Integer); virtual;
    procedure DrawSubItem(ACanvas: TCanvas; Rc: TRect; Str: string; States: TSideBarStates); virtual;
    procedure DrawNonItem(ACanvas: TCanvas; Rc: TRect); virtual;
    procedure DrawScroller(ACanvas: TCanvas; Rc: TRect; Up: Boolean; Hover: Boolean); virtual;
    procedure InvalidateItem(Index: Integer); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$IFDEF FPC}
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double); override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    {$IFDEF FPC}
    procedure FixDesignFontsPPI(const ADesignTimePPI: Integer); override;
    procedure ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double); override;
    {$ENDIF}

  published
    property ItemStyle: TSideBarItemStyle read FItemStyle write SetItemStyle;
    property SubItemStyle: TSideBarItemStyle read FSubItemStyle write SetSubItemStyle;
    property Bullets: TSideBarBulletStyle read FBullets write SetBullets;
    property Scrollers: TSideBarScrollerStyle read FScrollers write SetScrollers;
    property Items: TSideBarItems read FItems write SetItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property SubItemIndex: Integer read FSubItemIndex write SetSubItemIndex default -1;
    property ItemHeight: Integer read FItemHeight write SetItemHeight stored IsStoredItemHeight;
    property SubItemHeight: Integer read FSubItemHeight write SetSubItemHeight stored IsStoredSubItemHeight;
    property Alignment: TSideBarAlign read FAlignment write SetAlignment default saLeft;
    property Margin: Integer read FMargin write SetMargin stored IsStoredMargin;
    property GroupSeparator: Integer read FGroupSeparator write SetGroupSeparator stored IsStoredGroupSeparator;
    property Indent: Integer read FIndent write SetIndent stored IsStoredIndent;
    property AlwaysExpand: Boolean read FAlwaysExpand write SetAlwaysExpand;
    property Images: TImageList read FImages write SetImages;
    property HoverImages: TImageList read FHoverImages write SetHoverImages;
    property SelectedImages: TImageList read FSelectedImages write SetSelectedImages;
    property DisabledImages: TImageList read FDisabledImages write SetDisabledImages;
    property HandPointCursor: Boolean read FHandPointCursor write SetHandPointCursor default False;
    property OnHover: TSideBarEvent read FOnHover write FOnHover;
    property OnSelect: TSideBarEvent read FOnSelect write FOnSelect;
    property OnCustomDrawItem: TSideBarCustomDrawItem read FOnCustomDrawItem write FOnCustomDrawItem;
    property OnCustomDrawSubItem: TSideBarCustomDrawSubItem read FOnCustomDrawSubItem write FOnCustomDrawSubItem;
    property OnCustomDrawNonItem: TSideBarCustomDrawNonItem read FOnCustomDrawNonItem write FOnCustomDrawNonItem;
    property OnCustomDrawScroller: TSideBarCustomDrawScroller read FOnCustomDrawScroller write FOnCustomDrawScroller;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    {$IFDEF FPC}
    property BorderSpacing;
    {$ELSE}
    property BevelKind;
    {$ENDIF}
    property BorderStyle default bsSingle;
    property ParentBackground;
    property ParentColor;
    property Color;
    property Align default alLeft;
    property TabStop;
    property TabOrder;
  end;


implementation

const
  DEFAULT_ITEMHEIGHT = 30;         // Values at 96 ppi
  DEFAULT_SUBITEMHEIGHT = 18;
  DEFAULT_MARGIN = 8;
  DEFAULT_INDENT = 10;
  DEFAULT_GROUPSEPARATOR = 10;

type
  TSBInfo = record
    ItemIndex: Integer;
    SubIndex: Integer;
    Level: Integer;
    Rc: TRect;
    Disabled: Boolean;
    Caption: string;
  end;
  PSBInfo = ^TSBInfo;

const
  SCTOPINDEX = MaxInt;
  SCBOTTOMINDEX = MaxInt-1;

  SBITEM_STATE_DISABLED = $00000001;
  SBITEM_STATE_HIDDEN   = $00000004;

{$HINTS OFF}
procedure Unused(const A1);
begin
end;
{$HINTS ON}

{ TSideBarItem }

constructor TSideBarItem.Create(Collection: TCollection);
begin
  FItems := TStringList.Create;
  FItems.OnChange := ItemsChange;
  FStates := TList.Create;
  FImageIndex := -1;
  FExpanded := True;
  FEnabled := True;
  FVisible := True;
  FTag := 0;
  inherited Create(Collection);
end;

destructor TSideBarItem.Destroy;
begin
  inherited Destroy;
  FItems.Free;
  FStates.Free;
end;

procedure TSideBarItem.Assign(Source: TPersistent);
begin
  if (Source is TSideBarItem) then
  begin
    FCaption    := TSideBarItem(Source).Caption;
    FImageIndex := TSideBarItem(Source).ImageIndex;
    FTag        := TSideBarItem(Source).Tag;
    FExpanded   := TSideBarItem(Source).Expanded;
    FItems.Assign(TSideBarItem(Source).Items);
    FStates.Assign(TSideBarItem(Source).FStates);
    Changed(True);
  end;
end;

procedure TSideBarItem.ItemsChange(Sender: TObject);
begin
  if (FItems.Count = 0)
    then FStates.Clear;
  Changed(True);
end;

function TSideBarItem.GetSideBar: TNiceSideBar;
begin
  Result := TSideBarItems(Collection).FSideBar;
end;

function TSideBarItem.GetDisplayName: string;
begin
  if (FCaption <> '')
    then Result := FCaption
    else Result := inherited GetDisplayName;
end;

procedure TSideBarItem.SetCaption(Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Changed(True);
  end;
end;

procedure TSideBarItem.SetImageIndex(Value: TImageIndex);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

procedure TSideBarItem.SetItems(Value: TStringList);
begin
  FItems.Assign(Value);
  Changed(GetSideBar.ItemIndex = Index);
end;

procedure TSideBarItem.Collapse;
begin
  SetExpanded(False);
end;

procedure TSideBarItem.Expand;
begin
  SetExpanded(True);
end;

procedure TSideBarItem.SetExpanded(const Value: Boolean);
begin
  if (FExpanded <> Value) then
  begin
    FExpanded := Value;
    Changed(True);
  end;  
end;

function TSideBarItem.GetItemEnabled(Index: Integer): Boolean;
begin
  Result := True;
  if (FStates.Count > Index)
    then Result := (NativeUInt(FStates[Index]) and SBITEM_STATE_DISABLED) = 0;
end;

procedure TSideBarItem.SetItemEnabled(Index: Integer; const Value: Boolean);
var
  State: NativeUInt;
begin
  while (FStates.Count <= Index)
    do FStates.Add(nil);
  State := NativeUInt(FStates[Index]);
  if Value
    then State := State and not SBITEM_STATE_DISABLED
    else State := State or SBITEM_STATE_DISABLED;
  FStates[Index] := Pointer(State);
  Changed(True);
end;

function TSideBarItem.GetItemVisible(Index: Integer): Boolean;
begin
  Result := True;
  if (FStates.Count > Index)
    then Result := (NativeUInt(FStates[Index]) and SBITEM_STATE_HIDDEN) = 0;
end;

procedure TSideBarItem.SetItemVisible(Index: Integer; const Value: Boolean);
var
  State: NativeUInt;
begin
  while (FStates.Count <= Index)
    do FStates.Add(nil);
  State := NativeUInt(FStates[Index]);
  if Value
    then State := State and not SBITEM_STATE_HIDDEN
    else State := State or SBITEM_STATE_HIDDEN;
  FStates[Index] := Pointer(State);
  if (not Value) and (SideBar.FItemIndex = Self.Index) and (SideBar.FSubItemIndex = Index) then
  begin
    SideBar.FSubItemIndex := -1;
    SideBar.LastSubIndex := -1;
    SideBar.LastHover := -1;
    SideBar.HoverIndex := -1;
  end;
  Changed(True);
end;

procedure TSideBarItem.SetEnabled(const Value: Boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    Changed(True);
  end;  
end;

procedure TSideBarItem.SetVisible(const Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    if (not FVisible) and (SideBar.FItemIndex = Self.Index) then
    begin
      SideBar.LastIndex := -1;
      SideBar.LastSubIndex := -1;
      SideBar.LastHover := -1;
      SideBar.HoverIndex := -1;
      SideBar.FItemIndex := -1;
      SideBar.FSubItemIndex := -1;
    end;
    Changed(True);
  end;
end;

{ TSideBarItems }

constructor TSideBarItems.Create(ASideBar: TNiceSideBar);
begin
  inherited Create(TSideBarItem);
  FSideBar := ASideBar;
end;

function TSideBarItems.Add: TSideBarItem;
begin
  Result := TSideBarItem(inherited Add);
end;

function TSideBarItems.AddItem(Item: TSideBarItem;
  Index: Integer): TSideBarItem;
begin
  if (Item = nil)
    then Result := FSideBar.CreateItem
    else
    begin
      Result := Item;
      if Assigned(Item) then
      begin
        Result.Collection := Self;
        if (Index < 0)
          then Index := Count - 1;
        Result.Index := Index;
      end;
    end;
end;

function TSideBarItems.GetItem(Index: Integer): TSideBarItem;
begin
  Result := TSideBarItem(inherited GetItem(Index));
end;

function TSideBarItems.GetOwner: TPersistent;
begin
  Result := FSideBar;
end;

function TSideBarItems.Insert(Index: Integer): TSideBarItem;
begin
  Result := AddItem(nil, Index);
end;

procedure TSideBarItems.SetItem(Index: Integer; Value: TSideBarItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TSideBarItems.Update(Item: TCollectionItem);
begin
  if (Count = 0) then
  begin
    FSideBar.LastIndex := -1;
    FSideBar.LastSubIndex := -1;
    FSideBar.LastHover := -1;
  end;
  if (Item <> nil)
    then FSideBar.UpdateItem(Item.Index)
    else FSideBar.UpdateItems;
end;


{ TNiceSidebar }

constructor TNiceSidebar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  IsUpdating := True;
  Width := 200;
  Align := alLeft;
  Color := clBtnFace;
  BorderStyle := bsSingle;
  ParentBackground := False;
  ParentColor := False;
  ParentFont := False;
  TabStop := True;

  LastIndex := -1;
  LastSubIndex := -1;
  LastHover := -1;
  HoverIndex := -1;
  ScTop := Rect(0, 0, 0, 0);
  ScBottom := Rect(0, 0, 0, 0);
  ScTopVisible := False;
  ScBottomVisible := False;
  TopIndex := 0;
  BottomIndex := -1;
  DeltaY := 0;

  FItemIndex := -1;
  FSubItemIndex := -1;
  FItemHeight := DEFAULT_ITEMHEIGHT;
  FSubItemHeight := DEFAULT_SUBITEMHEIGHT;
  FAlignment := saLeft;
  FHandPointCursor := False;
  FMargin := DEFAULT_MARGIN;
  FGroupSeparator := DEFAULT_GROUPSEPARATOR;
  FIndent := DEFAULT_INDENT;
  FAlwaysExpand := True;

  FItemStyle := TSideBarItemStyle.Create(Self);
  FItemStyle.FNormalFont.Style := [fsBold];
  FItemStyle.FHoverFont.Style := [fsBold];
  FItemStyle.FSelectedFont.Style := [fsBold];
  FItemStyle.FDisabledFont.Style := [fsBold];
  FItemStyle.Activate;

  FSubItemStyle := TSideBarItemStyle.Create(Self);
  FSubItemStyle.Activate;

  FBullets := TSideBarBulletStyle.Create(Self);
  FScrollers := TSideBarScrollerStyle.Create(Self);

  FList := TList.Create;
  FItems := TSideBarItems.Create(Self);

  IsUpdating := False;
end;

destructor TNiceSidebar.Destroy;
begin
  FItems.Free;
  ClearList;
  FList.Free;
  FScrollers.Free;
  FBullets.Free;
  FSubItemStyle.Free;
  FItemStyle.Free;
  inherited Destroy;
end;

{$IFDEF FPC}
// Handle Lazarus' High-DPI scaling
procedure TNiceSidebar.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
    FItemHeight := round(FItemHeight * AYProportion);
    FSubItemHeight := round(FSubItemHeight * AYProportion);
    FMargin := round(FMargin * AXProportion);
    FIndent := round(FIndent * AXProportion);
    FGroupSeparator := round(FGroupSeparator * AYProportion);
    ListChange(true);
  end;
end;

procedure TNiceSidebar.FixDesignFontsPPI(const ADesignTimePPI: Integer);
begin
  inherited;

  DoFixDesignFontPPI(FItemStyle.NormalFont, ADesignTimePPI);
  DoFixDesignFontPPI(FItemStyle.HoverFont, ADesignTimePPI);
  DoFixDesignFontPPI(FItemStyle.SelectedFont, ADesignTimePPI);
  DoFixDesignFontPPI(FItemStyle.DisabledFont, ADesignTimePPI);

  DoFixDesignFontPPI(FSubItemStyle.NormalFont, ADesignTimePPI);
  DoFixDesignFontPPI(FSubItemStyle.HoverFont, ADesignTimePPI);
  DoFixDesignFontPPI(FSubItemStyle.SelectedFont, ADesignTimePPI);
  DoFixDesignFontPPI(FSubItemStyle.DisabledFont, ADesignTimePPI);
end;

procedure TNiceSidebar.ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double);
begin
  inherited;

  DoScaleFontPPI(FItemStyle.NormalFont, AToPPI, AProportion);
  DoScaleFontPPI(FItemStyle.HoverFont, AToPPI, AProportion);
  DoScaleFontPPI(FItemStyle.SelectedFont, AToPPI, AProportion);
  DoScaleFontPPI(FItemStyle.DisabledFont, AToPPI, AProportion);

  DoScaleFontPPI(FSubItemStyle.NormalFont, AToPPI, AProportion);
  DoScaleFontPPI(FSubItemStyle.HoverFont, AToPPI, AProportion);
  DoScaleFontPPI(FSubItemStyle.SelectedFont, AToPPI, AProportion);
  DoScaleFontPPI(FSubItemStyle.DisabledFont, AToPPI, AProportion);
end;
{$ENDIF}

procedure TNiceSidebar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  i: Integer;
  P: PSBInfo;
  Str: string;
  Changed: Boolean;
begin
  {$IFDEF FPC}
  LCLIntf.SetFocus(Handle);
  {$ELSE}
  Windows.SetFocus(Handle);
  {$ENDIF}

  if ScTopVisible then
  begin
    if PtInRect(ScTop, Point(X, Y)) then
    begin
      TopIndex := TopIndex - 1;
      ListChange(False);
      Invalidate;
      Exit;
    end;
  end;

  if ScBottomVisible then
  begin
    if PtInRect(ScBottom, Point(X, Y)) then
    begin
      TopIndex := TopIndex + 1;
      ListChange(False);
      Invalidate;
      Exit;
    end;
  end;

  i := GetIndexAtPos(X, Y);
  if (i = -1) then
  begin
    inherited;
    Exit;
  end;

  P := PSBInfo(FList[i]);
  if (P^.Level = 0) and FAlwaysExpand then
  begin
    inherited;
    Exit;
  end;
  if P^.Disabled then
  begin
    inherited;
    Exit;
  end;

  Changed := True;
  Str := P^.Caption;
  if (P^.ItemIndex = FItemIndex) then
  begin
    // on header
    if (P^.SubIndex = -1) and not FAlwaysExpand then
    begin
      FSubItemIndex := -1;
      FItems[P^.ItemIndex].Expanded := not FItems[P^.ItemIndex].Expanded;
      LastSubIndex := -1;
    end else
    // on sub items
    begin
      Changed := P^.SubIndex <> FSubItemIndex;
      if Changed then
      begin
        FItemIndex := P^.ItemIndex;
        FSubItemIndex := P^.SubIndex;
        InvalidateItem(LastSubIndex);
        InvalidateItem(i);
        LastSubIndex := i;
      end;
    end;
  end else
  begin
    FItemIndex := P^.ItemIndex;
    FSubItemIndex := P^.SubIndex;
    // on header
    if (FSubItemIndex = -1) then
    begin
      if FItems[FItemIndex].FExpanded then
      begin
        InvalidateItem(LastIndex);
        InvalidateItem(LastSubIndex);
        InvalidateItem(i);
        LastIndex := i;
        LastSubIndex := -1;
      end else
      begin
        LastIndex := i;
        LastSubIndex := -1;
        FItems[FItemIndex].FExpanded := True;
        ListChange(True);
        Invalidate;
      end;
    end else
    // on sub items
    begin
      InvalidateItem(LastIndex);
      InvalidateItem(LastSubIndex);
      InvalidateItem(i);
      InvalidateItem(i - FSubItemIndex - 1);
      LastSubIndex := i;
      LastIndex := i - FSubItemIndex - 1;
    end;
  end;
  if Changed then
  begin
    if Assigned(FOnSelect)
      then FOnSelect(Self, FItemIndex, FSubItemIndex, Str);
  end;
  inherited;
end;

procedure TNiceSidebar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  P: PSBInfo;
  Rc, tmpRc: TRect;
begin

  if ScTopVisible then
  begin
    if PtInRect(ScTop, Point(X, Y)) then
    begin
      if (HoverIndex <> SCTOPINDEX) then
      begin
        HoverIndex := SCTOPINDEX;
        InvalidateItem(LastHover);
        InvalidateItem(HoverIndex);
        LastHover := SCTOPINDEX;
      end;
      Exit;
    end;
  end;

  if ScBottomVisible then
  begin
    if PtInRect(ScBottom, Point(X, Y)) then
    begin
      if (HoverIndex <> SCBOTTOMINDEX) then
      begin
        HoverIndex := SCBOTTOMINDEX;
        InvalidateItem(LastHover);
        InvalidateItem(HoverIndex);
        LastHover := SCBOTTOMINDEX;
      end;  
      Exit;
    end;
  end;

  i := GetIndexAtPos(X, Y);

  if (i > -1) then
  begin
    P := PSBInfo(FList[i]);
    if (P^.Level = 0) and FAlwaysExpand then
      i := -1;
  end;

  if FHandPointCursor then
  begin
    if (i = -1)
      then Cursor := crDefault
      else Cursor := crHandPoint;
  end;

  if (i <> HoverIndex) then
  begin
    HoverIndex := i;
    if (LastHover >= 0) and (LastHover < FList.Count) then
      InvalidateItem(LastHover);
    if (HoverIndex > -1) then
    begin
      InvalidateItem(HoverIndex);
      P := PSBInfo(FList[i]);
      if Assigned(FOnHover)
        then FOnHover(Self, P^.ItemIndex, P^.SubIndex, P^.Caption);

      Rc := P^.Rc;
      OffsetRect(Rc, 0, -DeltaY);
      tmpRc := Rect(0, 0, 0, 0);  // To silence the compiler
      if IntersectRect(tmpRc, ScTop, Rc) then
        InvalidateItem(SCTOPINDEX);

      if IntersectRect(tmpRc, ScBottom, Rc) then
        InvalidateItem(SCBOTTOMINDEX);
    end;
    LastHover := HoverIndex;
  end;

  inherited;

end;

procedure TNiceSideBar.CMMouseLeave(var Msg: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
  Unused(Msg);
  if (HoverIndex <> -1) then
  begin
    HoverIndex := -1;
    if (LastHover >= 0) and (LastHover < FList.Count) then
      InvalidateItem(LastHover);
    LastHover := -1;
  end;
  if Assigned(FOnHover)
    then FOnHover(Self, -1, -1, '');
end;

procedure TNiceSideBar.ClearList;
var
  x: Integer;
begin
  for x := 0 to FList.Count-1
    do Dispose(PSBInfo(FList[x]));
  FList.Clear;
end;

procedure TNiceSideBar.ListChange(RebuildItems: Boolean);
var
  P: PSBInfo;
  x, y, v: Integer;
  Item: TSideBarItem;
  delta: Integer;
begin
  if IsUpdating
    then Exit;
  DeltaY := 0;
  BottomIndex := -1;
  ScTopVisible := False;
  ScBottomVisible := False;
  if RebuildItems then
  begin
    ClearList;
    v := 0;
    for x := 0 to FItems.Count-1 do
    begin
      Item := FItems[x];
      if not Item.FVisible
        then Continue;
      P := New(PSBInfo);
      P^.Caption := Item.FCaption;
      P^.ItemIndex := x;
      P^.SubIndex := -1;
      P^.Level := 0;
      P^.Rc := Rect(FMargin, v, Width - FMargin, v + FItemHeight + FGroupSeparator);
      P^.Disabled := not Item.FEnabled;
      FList.Add(P);
      Inc(v, FItemHeight + 1 + FGroupSeparator);
      if Item.FExpanded then
      begin
        for y := 0 to Item.FItems.Count-1 do
        begin
          if not Item.GetItemVisible(y)
            then Continue;
          P := New(PSBInfo);
          P^.Caption := Item.FItems[y];
          P^.Level := 1;
          P^.ItemIndex := x;
          P^.SubIndex := y;
          P^.Rc := Rect(FMargin, v, Width - FMargin, v + FSubItemHeight);
          P^.Disabled := not Item.GetItemEnabled(y);
          FList.Add(P);
          Inc(v, FSubItemHeight+1);
        end;
      end;
    end;
  end;
  if (FList.Count > 0) then
  begin
    ScTopVisible := TopIndex > 0;
    DeltaY := PSBInfo(FList[TopIndex])^.Rc.Top;
    for x := TopIndex to FList.Count-1 do
    begin
      P := PSBInfo(FList[x]);
      v := P^.Rc.Bottom - DeltaY;
      if (v > ClientHeight) then
      begin
        BottomIndex := x;
        ScBottomVisible := True;
        Break;
      end;
    end;
    if (BottomIndex = -1) then
    begin
      BottomIndex := FList.Count-1;
      ScBottomVisible := False;
    end;
    delta := 12;
    {$IFDEF FPC}
    delta := Scale96ToFont(delta);
    {$ENDIF}
    if (FAlignment = saRight) then
    begin
      ScTop := Rect(FMargin + delta, delta, FMargin + 2*delta + 1, 2*delta + 1);
      ScBottom := Rect(FMargin + delta, ClientHeight - 2*delta - 1, FMargin + 2*delta + 1, ClientHeight - delta);
    end else
    begin
      ScTop := Rect(ClientWidth - FMargin - 2*delta - 1, delta, ClientWidth - FMargin - delta, 2*delta+1);
      ScBottom := Rect(ClientWidth - FMargin - 2*delta - 1, ClientHeight - 2*delta-1, ClientWidth - FMargin - delta, ClientHeight - delta);
    end;
  end;
end;

procedure TNiceSideBar.InvalidateItem(Index: Integer);
var
  Rc: TRect;
  Info: PSBInfo;
begin
  if Index = -1 then
    exit;

  if Index = SCTOPINDEX then
    Rc := ScTop
  else
  if Index = SCBOTTOMINDEX then
    Rc := ScBottom
  else
  begin
    Info := PSBInfo(FList[Index]);
    Rc := Info^.Rc;
    OffsetRect(Rc, 0, -DeltaY);
  end;
  InvalidateRect(Handle, @Rc, false);
end;

procedure TNiceSideBar.DoDrawItem(Index: Integer);
var
  Info: PSBInfo;
  States: TSideBarStates;
  Rc, Tmp: TRect;
begin
  if (Index = SCTOPINDEX) then
  begin
    if ScTopVisible then
    begin
      if Assigned(FOnCustomDrawScroller)
        then FOnCustomDrawScroller(Self, Canvas, ScTop, True, HoverIndex = SCTOPINDEX)
        else DrawScroller(Canvas, ScTop, True, HoverIndex = SCTOPINDEX);
    end;
    Exit;
  end;

  if (Index = SCBOTTOMINDEX) then
  begin
    if ScBottomVisible then
    begin
      if Assigned(FOnCustomDrawScroller)
        then FOnCustomDrawScroller(Self, Canvas, ScBottom, False, HoverIndex = SCBOTTOMINDEX)
        else DrawScroller(Canvas, ScBottom, False, HoverIndex = SCBOTTOMINDEX);
    end;
    Exit;
  end;

  if (Index < 0)
    then Exit;

  Info := PSBInfo(FList[Index]);
  Rc := Info^.Rc;
  OffsetRect(Rc, 0, -DeltaY);

  if (Index = HoverIndex)
    then States := [ssHover]
    else States := [ssNormal];
  if (Info^.Level = 1) then
  begin
    if (Info^.SubIndex = FSubItemIndex) and (Info^.ItemIndex = FItemIndex)
      then Include(States, ssSelected);
  end else
  begin
    if (Info^.ItemIndex = FItemIndex)
      then Include(States, ssSelected);
  end;
  if Info^.Disabled
    then Include(States, ssDisabled);

  if (Info^.Level = 1) then
  begin
    if Assigned(FOnCustomDrawSubItem)
      then FOnCustomDrawSubItem(Self, Canvas, Rc, Info^.Caption, States)
      else DrawSubItem(Canvas, Rc, Info^.Caption, States);
  end else
  begin
    if Assigned(FOnCustomDrawItem)
      then FOnCustomDrawItem(Self, Canvas, Rc, Info^.Caption, States, FItems[Info^.ItemIndex].FImageIndex)
      else DrawItem(Canvas, Rc, Info^.Caption, States, FItems[Info^.ItemIndex].FImageIndex);
  end;

  Tmp := Rect(0, 0, 0, 0);  // to silence the compiler
  if IntersectRect(Tmp, Rc, ScTop) and ScTopVisible then
  begin
    if Assigned(FOnCustomDrawScroller)
      then FOnCustomdrawScroller(Self, Canvas, ScTop, True, HoverIndex = SCTOPINDEX)
      else DrawScroller(Canvas, ScTop, True, HoverIndex = SCTOPINDEX);
  end;

  if IntersectRect(Tmp, Rc, ScBottom) and ScBottomVisible then
  begin
    if Assigned(FOnCustomDrawScroller)
      then FOnCustomDrawScroller(Self, Canvas, ScBottom, False, HoverIndex = SCBOTTOMINDEX)
      else DrawScroller(Canvas, ScBottom, False, HoverIndex = SCBOTTOMINDEX);
  end;

end;

procedure TNiceSideBar.DrawItem(ACanvas: TCanvas; Rc: TRect; Str: string;
  States: TSideBarStates; ImageIndex: Integer);
var
  w, h, x, y: Integer;
  RcItem: TRect;
  Img: TImageList;
  ImgWidth: Integer;
  ImgHeight: Integer;
  {$IFDEF FPC}
  imgR: TScaledImageListResolution;
  ppi: Integer;
  {$ENDIF}
begin
  RcItem := Rc;
  with ACanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect(Rect(RcItem.Left, RcItem.Top, RcItem.Right, RcItem.Top + FGroupSeparator));
    RcItem.Top := RcItem.Top + FGroupSeparator;

    if (ssDisabled in States) then
    begin
      Brush.Color := FItemStyle.FNormalColor;
      Font.Assign(FItemStyle.FDisabledFont);
    end else
    if (ssNormal in States) then
    begin
      if (ssSelected in States) then
      begin
        Brush.Color := FItemStyle.FSelectedColor;
        Font.Assign(FItemStyle.FSelectedFont);
      end else
      begin
        Brush.Color := FItemStyle.FNormalColor;
        Font.Assign(FItemStyle.FNormalFont);
      end;
    end else
    if (ssHover in States) then
    begin
      if (ssSelected in States)
        then Brush.Color := FItemStyle.FSelectedColor
        else Brush.Color := FItemStyle.FHoverColor;
      Font.Assign(FItemStyle.FHoverFont);
    end;

    FillRect(RcItem);
    Brush.Style := bsClear;
    Pen.Color := FItemStyle.FLineColor;
    MoveTo(RcItem.Left, RcItem.Bottom);
    LineTo(RcItem.Right, RcItem.Bottom);
    MoveTo(RcItem.Left, RcItem.Bottom-1);
    LineTo(RcItem.Right, RcItem.Bottom-1);

    InflateRect(RcItem, -8, 0);

    Img := nil;
    if (ssDisabled in States) then
    begin
      if Assigned(FDisabledImages)
        then Img := FDisabledImages else
      if Assigned(FImages)
        then Img := FImages;  
    end else
    if (ssSelected in States) then
    begin
      if Assigned(FSelectedImages)
        then Img := FSelectedImages else
      if Assigned(FImages)
        then Img := FImages;
    end else
    if (ssNormal in States) then
    begin
      if Assigned(FImages)
        then Img := FImages;
    end else
    if (ssHover in States) then
    begin
      if Assigned(FHoverImages)
        then Img := FHoverImages else
      if Assigned(FImages)
        then Img := FImages;
    end;

    if Assigned(Img) then
    begin
      {$IFDEF FPC}
      ppi := NeedParentDesignControl(Self).PixelsPerInch;
      ImgWidth := Img.WidthForPPI[0, ppi];
      ImgHeight := Img.HeightForPPI[0, ppi];
      ImgR := Img.ResolutionForPPI[0, ppi, GetCanvasScaleFactor];
      {$ELSE}
      ImgWidth := Img.Width;
      ImgHeight := Img.Height;
      {$ENDIF}
    end else
    begin
      ImgWidth := 0;
      ImgHeight := 0;
    end;

    w := TextWidth(Str);
    h := TextHeight('Ag');
    x := 0;

    if Assigned(Img) and (ImageIndex > -1)
      then w := w + ImgWidth + FIndent;

    case FAlignment of
      saLeft:   x := RcItem.Left;
      saCenter: x := RcItem.Left + (((RcItem.Right - RcItem.Left) - w) div 2);
      saRight:  x := RcItem.Right - w;
    end;

    if Assigned(Img) then
    begin
      if (ImageIndex > -1) then
      begin
        y := RcItem.Top + ((FItemHeight - ImgHeight) div 2);
        if (FAlignment <> saRight) then
        begin
          {$IFDEF FPC}ImgR{$ELSE}Img{$ENDIF}.Draw(ACanvas, x, y, ImageIndex, dsTransparent, itImage);
          Inc(x, ImgWidth + FIndent);
        end else
          {$IFDEF FPC}ImgR{$ELSE}Img{$ENDIF}.Draw(ACanvas, RcItem.Right - ImgWidth, y, ImageIndex, dsTransparent, itImage);
      end;
    end;

    y := RcItem.Top + (((RcItem.Bottom - RcItem.Top) - h) div 2);
    TextRect(RcItem, x, y, Str);

  end;

end;

procedure TNiceSideBar.DrawSubItem(ACanvas: TCanvas; Rc: TRect;
  Str: string; States: TSideBarStates);
const
  Separator = 7;
var
  RcItem, Rc2: TRect;
  x, y, w, h, i: Integer;
  Old: TColor;
begin
  RcItem := Rc;
  Rc2 := Rc;
  inc(Rc2.Bottom);
  case FAlignment of
    saLeft:
      begin
        Rc2.Right := Rc2.Left + FIndent;
        RcItem.Left := Rc2.Right;
      end;
    saCenter: ;
    saRight:
      begin
        Rc2.Left := Rc2.Right - FIndent;
        RcItem.Right := Rc2.Left;
      end;
  end;
  with ACanvas do
  begin
    Brush.Style := bsSolid;
    if (FAlignment <> saCenter) then
    begin
      Brush.Color := Color;
      FillRect(Rc2);
    end;

    if (ssDisabled in States) then
    begin
      Brush.Color := FSubItemStyle.FNormalColor;
      Font.Assign(FSubItemStyle.FDisabledFont);
    end else
    if (ssNormal in States) then
    begin
      if (ssSelected in States) then
      begin
        Brush.Color := FSubItemStyle.FSelectedColor;
        Font.Assign(FSubItemStyle.FSelectedFont);
      end else
      begin
        Brush.Color := FSubItemStyle.FNormalColor;
        Font.Assign(FSubItemStyle.FNormalFont);
      end;
    end else
    if (ssHover in States) then
    begin
      if (ssSelected in States)
        then Brush.Color := FSubItemStyle.FSelectedColor
        else Brush.Color := FSubItemStyle.FHoverColor;
      Font.Assign(FSubItemStyle.FHoverFont);
    end;

    FillRect(RcItem);
    Brush.Style := bsClear;
    Pen.Color := FSubItemStyle.FLineColor;
    MoveTo(RcItem.Left, RcItem.Bottom);
    LineTo(RcItem.Right, RcItem.Bottom);
    InflateRect(RcItem, -8, 0);

    w := TextWidth(Str);
    h := TextHeight('Ag');
    x := 0;

    if FBullets.Visible
      then w := w + FBullets.Size + Separator;

    case FAlignment of
      saLeft:   x := RcItem.Left;
      saCenter: x := RcItem.Left + (((RcItem.Right - RcItem.Left) - w) div 2);
      saRight:  x := RcItem.Right - w;
    end;

    if FBullets.Visible then
    begin
      y := RcItem.Top + ((FSubItemHeight - FBullets.Size) div 2);
      if (FAlignment <> saRight) then
      begin
        Rc2 := Rect(x, y, x + FBullets.Size, y + FBullets.Size);
        Inc(x, FBullets.Size + Separator);
      end else
      begin
        i := RcItem.Right - FBullets.Size;
        Rc2 := Rect(i, y, i + FBullets.Size, y + FBullets.Size);
      end;

      Old := Pen.Color;
      Brush.Style := bsSolid;

      if (ssDisabled in States) then
      begin
        Brush.Color := FBullets.FDisabledColor;
        Pen.Color := FBullets.FDisabledPenColor;
      end else
      if (ssHover in States) then
      begin
        Brush.Color := FBullets.FHoverColor;
        Pen.Color := FBullets.FHoverPenColor;
      end else
      if (ssSelected in States) then
      begin
        Brush.Color := FBullets.FSelectedColor;
        Pen.Color := FBullets.FSelectedPenColor;
      end else
      if (ssNormal in States) then
      begin
        Brush.Color := FBullets.FNormalColor;
        Pen.Color := FBullets.FNormalPenColor;
      end;

      case FBullets.Kind of
        sbRound:
          Ellipse(Rc2);
        sbRectangle:  
          Rectangle(Rc2);
        sbDiamond:
          begin
            i := FBullets.Size div 2;
            Polygon([
                Point(Rc2.Left + i, Rc2.Top),
                Point(Rc2.Left, Rc2.Top + i),
                Point(Rc2.Left + i, Rc2.Top + (i * 2)),
                Point(Rc2.Left + (i * 2), Rc2.Top + i)
              ]);
          end;
      end;

      Pen.Color := Old;
      Brush.Style := bsClear;
    end;

    y := RcItem.Top + (((RcItem.Bottom - RcItem.Top) - h) div 2);
    TextRect(RcItem, x, y, Str);

  end;
end;

procedure TNiceSideBar.DrawNonItem(ACanvas: TCanvas; Rc: TRect);
begin
  with ACanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect(Rc);
  end;
end;

procedure TNiceSideBar.DrawScroller(ACanvas: TCanvas; Rc: TRect;
  Up: Boolean; Hover: Boolean);
var
  Old: TColor;
  dist: Integer;
begin
  with ACanvas do
  begin
    Old := Pen.Color;
    Brush.Style := bsSolid;
    if Hover then
    begin
      Brush.Color := FScrollers.FHoverColor;
      Pen.Color := FScrollers.FHoverColor;
    end else
    begin
      Brush.Color := FScrollers.FNormalColor;
      Pen.Color := FScrollers.FNormalColor;
    end;
    RoundRect(Rc.Left, Rc.Top, Rc.Right, Rc.Bottom, 3, 3);
    if Hover then
    begin
      Brush.Color := FScrollers.FHoverArrowColor;
      Pen.Color := FScrollers.FHoverArrowColor;
    end else
    begin
      Brush.Color := FScrollers.FNormalArrowColor;
      Pen.Color := FScrollers.FNormalArrowColor;
    end;
    {$IFDEF FPC}
    dist := Scale96ToFont(3);
    {$ELSE}
    dist := 3;
    {$ENDIF}
    if Up then
    begin
      Polygon([
        Point(Rc.Left + dist, Rc.Bottom - dist - 1),
        Point(Rc.Right - dist - 1, Rc.Bottom - dist - 1),
        Point((Rc.Left + Rc.Right) div 2, Rc.Top + dist)
      ]);
      {
      Polygon([
        Point(Rc.Left+3, Rc.Bottom-5),
        Point(Rc.Right-4, Rc.Bottom-5),
        Point(Rc.Left+5, Rc.Top+3)
      ]);
      }
    end else
    begin
      Polygon([
        Point(Rc.Left + dist, Rc.Top + dist),
        Point(Rc.Right - dist - 1, Rc.Top + dist),
        Point((Rc.Left + Rc.Right) div 2, Rc.Bottom - dist - 1)
      ]);
      {
      Polygon([
        Point(Rc.Left+3, Rc.Top+4),
        Point(Rc.Right-4, Rc.Top+4),
        Point(Rc.Left+5, Rc.Bottom-4)
      ]);
      }
    end;
    Pen.Color := Old;
  end;
end;

procedure TNiceSideBar.Paint;
var
  x, v: Integer;
  Rc: TRect;

begin

  if IsUpdating
    then Exit;

  if (FMargin > 0) then
  begin
    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := Color;
      FillRect(Rect(0, 0, FMargin, ClientHeight));
      FillRect(Rect(ClientWidth-FMargin, 0, ClientWidth, ClientHeight));
    end;
  end;

  v := 0;
  if (FList.Count > 0) then
  begin
    for x := TopIndex to BottomIndex
      do DoDrawItem(x);
    v := PSBInfo(FList[FList.Count-1])^.Rc.Bottom + 1 - DeltaY;
  end;
  if (ClientHeight > v) then
  begin
    Rc := Rect(0, v, ClientWidth, ClientHeight);
    if Assigned(FOnCustomDrawNonItem)
      then FOnCustomDrawNonItem(Self, Canvas, Rc)
      else DrawNonItem(Canvas, Rc);
  end;
  DoDrawItem(SCTOPINDEX);
  DoDrawItem(SCBOTTOMINDEX);

end;

function TNiceSideBar.GetIndexAtPos(X, Y: Integer): Integer;
var
  i: Integer;
  Pt: TPoint;
begin
  Result := -1;
  Pt := Point(X, Y + DeltaY);
  for i := 0 to FList.Count-1 do
  begin
    if PtInRect(PSBInfo(FList[i])^.Rc, Pt) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TNiceSideBar.WMEraseBkgnd(var Msg: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
  Msg.Result := 1;
end;

procedure TNiceSideBar.WMSize(var Msg: {$IFDEF FPC}TLMSize{$ELSE}TWMSize{$ENDIF});
begin
  Unused(Msg);
  TopIndex := 0;
  ListChange(False);
  Invalidate;
end;

procedure TNiceSidebar.CMColorChanged(var Msg: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
  Unused(Msg);
  Invalidate;
end;

procedure TNiceSideBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (AComponent = FImages) then
    begin
      FImages := nil;
      Invalidate;
    end;
    if (AComponent = FHoverImages) then
    begin
      FHoverImages := nil;
      Invalidate;
    end;
    if (AComponent = FSelectedImages) then
    begin
      FSelectedImages := nil;
      Invalidate;
    end;
    if (AComponent = FDisabledImages) then
    begin
      FDisabledImages := nil;
      Invalidate;
    end;
  end;
end;

function TNiceSideBar.CreateItem: TSideBarItem;
begin
  Result := TSideBarItem.Create(FItems);
end;

procedure TNiceSideBar.UpdateItem(Index: Integer);
var
  x, i: Integer;
  P: PSBInfo;
begin
  i := -1;
  for x := 0 to FList.Count-1 do
  begin
    P := PSBInfo(FList[x]);
    if (P^.ItemIndex = Index) and (P^.SubIndex = -1) then
    begin
      i := x;
      Break;
    end;
  end;
  InvalidateItem(i);
end;

procedure TNiceSideBar.UpdateItems;
begin
  ListChange(True);
  Invalidate;
end;

function TNiceSideBar.IsStoredItemHeight: Boolean;
begin
  Result := FItemHeight <> DEFAULT_ITEMHEIGHT;
end;

function TNiceSideBar.IsStoredSubItemHeight: Boolean;
begin
  Result := FSubItemHeight <> DEFAULT_SUBITEMHEIGHT;
end;

function TNiceSideBar.IsStoredMargin: Boolean;
begin
  Result := FMargin <> DEFAULT_MARGIN;
end;

function TNiceSideBar.IsStoredIndent: Boolean;
begin
  Result := FIndent <> DEFAULT_INDENT;
end;

function TNiceSideBar.IsStoredGroupSeparator: Boolean;
begin
  Result := FGroupSeparator <> DEFAULT_GROUPSEPARATOR;
end;

procedure TNiceSideBar.SetItemIndex(Value: Integer);
var
  x: Integer;
  Redraw: Boolean;
begin
  if (FItemIndex <> Value) then
  begin
    FItemIndex := Value;
    FSubItemIndex := -1;

    Redraw := True;
    if (FItemIndex <> -1) then
    begin
      if FItems[FItemIndex].FExpanded then
      begin
        InvalidateItem(LastIndex);
        InvalidateItem(LastSubIndex);
      end else
      begin
        FItems[FItemIndex].Expand;
        Redraw := False;
      end;
    end else
    begin
      InvalidateItem(LastIndex);
      InvalidateItem(LastSubIndex);
    end;

    if IsUpdating then
    begin
      IsUpdating := False;
      ListChange(False);
      IsUpdating := True;
    end;

    LastSubIndex := -1;
    LastIndex := -1;
    for x := 0 to FList.Count-1 do
    begin
      if (PSBInfo(FList[x])^.ItemIndex = FItemIndex) then
      begin
        LastIndex := x;
        Break;
      end;
    end;
    if Redraw then
      InvalidateItem(LastIndex);
  end;
end;

procedure TNiceSideBar.SetSubItemIndex(Value: Integer);
var
  x, i: Integer;
  P: PSBInfo;
begin
  if (FItemIndex = -1)
    then Exit;
  if (FSubItemIndex <> Value) then
  begin
    FSubItemIndex := Value;
    if IsUpdating then
    begin
      IsUpdating := False;
      ListChange(False);
      IsUpdating := True;
    end;
    i := -1;
    for x := 0 to FList.Count-1 do
    begin
      P := PSBInfo(FList[x]);
      if (P^.ItemIndex = FItemIndex) then
      begin
        if (P^.SubIndex = Value) then
        begin
          i := x;
          Break;
        end;
      end;
    end;
    InvalidateItem(LastSubIndex);
    LastSubIndex := i;
    if (i > -1) then
      InvalidateItem(i);
  end;
end;

procedure TNiceSideBar.SetItemHeight(Value: Integer);
begin
  if (FItemHeight <> Value) then
  begin
    FItemHeight := Value;
    ListChange(True);
    Invalidate;
  end;
end;

procedure TNiceSideBar.SetSubItemHeight(Value: Integer);
begin
  if (FSubItemHeight <> Value) then
  begin
    FSubItemHeight := Value;
    ListChange(True);
    Invalidate;
  end;
end;

procedure TNiceSideBar.SetAlignment(Value: TSideBarAlign);
begin
  if (FAlignment <> Value) then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TNiceSidebar.SetItems(Value: TSideBarItems);
begin
  FItems.Assign(Value);
end;

procedure TNiceSideBar.SetImages(Value: TImageList);
begin
  if (FImages <> Value) then
  begin
    FImages := Value;
    Invalidate;
  end;
end;

procedure TNiceSideBar.SetHoverImages(Value: TImageList);
begin
  if (FHoverImages <> Value) then
  begin
    FHoverImages := Value;
    Invalidate;
  end;
end;

procedure TNiceSideBar.SetSelectedImages(Value: TImageList);
begin
  if (FSelectedImages <> Value) then
  begin
    FSelectedImages := Value;
    Invalidate;
  end;
end;

procedure TNiceSideBar.SetDisabledImages(Value: TImageList);
begin
  if (FDisabledImages <> Value) then
  begin
    FDisabledImages := Value;
    Invalidate;
  end;
end;

procedure TNiceSideBar.SetHandPointCursor(Value: Boolean);
begin
  if (FHandPointCursor <> Value) then
  begin
    FHandPointCursor := Value;
    Cursor := crDefault;
  end;
end;

procedure TNiceSideBar.SetMargin(const Value: Integer);
begin
  if (FMargin <> Value) then
  begin
    FMargin := Value;
    ListChange(True);
    Invalidate;
  end;
end;

procedure TNiceSideBar.SetGroupSeparator(const Value: Integer);
begin
  if (FGroupSeparator <> Value) then
  begin
    FGroupSeparator := Value;
    ListChange(True);
    Invalidate;
  end;
end;

procedure TNiceSideBar.BeginUpdate;
begin
  IsUpdating := True;
end;

procedure TNiceSideBar.EndUpdate;
begin
  IsUpdating := False;
  FItemIndex := -1;
  FSubItemIndex := -1;
  LastIndex := -1;
  LastSubIndex := -1;
  LastHover := -1;
  HoverIndex := -1;
  TopIndex := 0;
  ListChange(True);
  Invalidate;
end;

procedure TNiceSideBar.SetIndent(const Value: Integer);
begin
  if (FIndent <> Value) then
  begin
    FIndent := Value;
    Invalidate;
  end;
end;

procedure TNiceSideBar.SetAlwaysExpand(const Value: Boolean);
begin
  if (FAlwaysExpand <> Value) then
  begin
    FAlwaysExpand := Value;
    if FAlwaysExpand then
    begin
      ListChange(True);
      Invalidate;
    end;
  end;
end;

procedure TNiceSideBar.SetItemStyle(const Value: TSideBarItemStyle);
begin
  FItemStyle.Assign(Value);
end;

procedure TNiceSideBar.SetSubItemStyle(const Value: TSideBarItemStyle);
begin
  FSubItemStyle.Assign(Value);
end;

procedure TNiceSideBar.SetBullets(const Value: TSideBarBulletStyle);
begin
  FBullets := Value;
end;

procedure TNiceSideBar.SetScrollers(const Value: TSideBarScrollerStyle);
begin
  FScrollers := Value;
end;

procedure TNiceSideBar.WMMouseWheel(var Msg: {$IFDEF FPC}TLMMouseEvent{$ELSE}TWMMouseWheel{$ENDIF});
begin
  if (Msg.WheelDelta > 0) and ScTopVisible then
  begin
    TopIndex := TopIndex - 1;
    ListChange(False);
    Invalidate;
    Exit;
  end else
  if (Msg.WheelDelta < 0) and ScBottomVisible then
  begin
    TopIndex := TopIndex + 1;
    ListChange(False);
    Invalidate;
  end else                                                        
    inherited;
end;

procedure TNiceSideBar.CMWantSpecialKey(var Message: {$IFDEF FPC}TLMKey{$ELSE}TWMKey{$ENDIF});
begin
  inherited;
  with Message do
  case CharCode of
    VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN:
      Result := 1;
  end;
end;

procedure TNiceSideBar.KeyDown(var Key: Word; Shift: TShiftState);
var
  Rc: TRect;
  x, y, i: Integer;
  Info: PSBInfo; 
begin
  if (Key = VK_NEXT) and ScBottomVisible then
  begin
    Info := PSBInfo(FList[BottomIndex]);
    if ((Info^.Rc.Bottom - DeltaY) <= Height)
      then TopIndex := BottomIndex + 1
      else TopIndex := BottomIndex;
    ListChange(False);
    Invalidate;
  end else
  if (Key = VK_PRIOR) and ScTopVisible then
  begin
    y := Height;
    i := TopIndex-1;
    for x := TopIndex-1 downto 0 do
    begin
      Info := PSBInfo(FList[x]);
      y := y - (Info^.Rc.Bottom - Info^.Rc.Top + 1);
      if (y <= 0)
        then Break
        else i := x;
    end;
    TopIndex := i;
    ListChange(False);
    Invalidate;
  end else
  if (Key = VK_DOWN) then
  begin
    if (HoverIndex = FList.Count-1)
      then Exit;
    if (HoverIndex < TopIndex) or (HoverIndex > BottomIndex) then
    begin
      LastHover := TopIndex;
      HoverIndex := TopIndex;
      InvalidateItem(HoverIndex);
    end else
    begin
      HoverIndex := Min(FList.Count-1, HoverIndex + 1);
      if (LastHover >= 0) and (LastHover < FList.Count) then
        InvalidateItem(LastHover);
      InvalidateItem(HoverIndex);
      LastHover := HoverIndex;
    end;
    if (HoverIndex >= BottomIndex-1) and ScBottomVisible then
    begin
      TopIndex := TopIndex + 1;
      ListChange(False);
      Invalidate;
    end;
  end else
  if (Key = VK_UP) then
  begin
    if (HoverIndex = 0)
      then Exit;
    if (HoverIndex = TopIndex) and ScTopVisible then
    begin
      TopIndex := TopIndex - 1;
      ListChange(False);
      Invalidate;
    end;
    if (HoverIndex < TopIndex) or (HoverIndex > BottomIndex) then
    begin
      LastHover := BottomIndex;
      HoverIndex := BottomIndex;
      InvalidateItem(HoverIndex);
    end else
    begin
      HoverIndex := Max(0, HoverIndex - 1);
      if (LastHover >= 0) and (LastHover < FList.Count) then
        InvalidateItem(LastHover);
      InvalidateItem(HoverIndex);
      LastHover := HoverIndex;
    end;
  end else
  if (Key = VK_RETURN) then
  begin
    if (HoverIndex < TopIndex) or (HoverIndex > BottomIndex)
        or (HoverIndex < 0) or (HoverIndex >= FList.Count)
      then Exit;
    Rc := PSBInfo(FList[HoverIndex])^.Rc;
    OffsetRect(Rc, 0, -DeltaY);
    MouseDown(mbLeft, [], Rc.Left + 1, Rc.Top + 1);
  end;
  inherited;
end;

{ TSideBarItemStyle }

constructor TSideBarItemStyle.Create(SideBar: TNiceSideBar);
begin
  inherited Create;
  FSideBar := SideBar;
  FNormalFont := TFont.Create;
  FNormalFont.Name := 'Arial';
  FHoverFont := TFont.Create;
  FHoverFont.Name := 'Arial';
  FHoverFont.Color := clDefaultHoverFont;
  FSelectedFont := TFont.Create;
  FSelectedFont.Name := 'Arial';
  FDisabledFont := TFont.Create;
  FDisabledFont.Name := 'Arial';
  FDisabledFont.Color := clGrayText;
  FNormalColor := clBtnFace;
  FHoverColor := clDefaultHover;
  FSelectedColor := clDefaultSelected;
  FLineColor := clWindowText;
end;

destructor TSideBarItemStyle.Destroy;
begin
  FNormalFont.Free;
  FHoverFont.Free;
  FSelectedFont.Free;
  FDisabledFont.Free;
  inherited Destroy;
end;

procedure TSideBarItemStyle.AssignTo(Dest: TPersistent);
begin
  if (Dest is TSideBarItemStyle) then
  begin
    with TSideBarItemStyle(Dest) do
    begin
      Deactivate;
      FNormalFont.Assign(Self.FNormalFont);
      FHoverFont.Assign(Self.FHoverFont);
      FSelectedFont.Assign(Self.FSelectedFont);
      FNormalColor := Self.FNormalColor;
      FHoverColor := Self.FHoverColor;
      FSelectedColor := Self.FSelectedColor;
      FLineColor := Self.FLineColor;
      Activate;
      FSideBar.Invalidate;
    end;
  end else
    inherited AssignTo(Dest);
end;

procedure TSideBarItemStyle.Activate;
begin
  FNormalFont.OnChange := FontChange;
  FSelectedFont.OnChange := FontChange;
  FDisabledFont.OnChange := FontChange;
end;

procedure TSideBarItemStyle.Deactivate;
begin
  FNormalFont.OnChange := nil;
  FSelectedFont.OnChange := nil;
  FDisabledFont.OnChange := nil;
end;

procedure TSideBarItemStyle.FontChange(Sender: TObject);
begin
  FSideBar.Invalidate;
end;

procedure TSideBarItemStyle.SetHoverFont(const Value: TFont);
begin
  FHoverFont.Assign(Value);
end;

procedure TSideBarItemStyle.SetLineColor(const Value: TColor);
begin
  if (FLineColor <> Value) then
  begin
    FLineColor := Value;
    FSideBar.Invalidate;
  end;
end;

procedure TSideBarItemStyle.SetNormalColor(const Value: TColor);
begin
  if (FNormalColor <> Value) then
  begin
    FNormalColor := Value;
    FSideBar.Invalidate;
  end;
end;

procedure TSideBarItemStyle.SetNormalFont(const Value: TFont);
begin
  FNormalFont.Assign(Value);
end;

procedure TSideBarItemStyle.SetSelectedColor(const Value: TColor);
begin
  if (FSelectedColor <> Value) then
  begin
    FSelectedColor := Value;
    FSideBar.Invalidate;
  end;
end;

procedure TSideBarItemStyle.SetSelectedFont(const Value: TFont);
begin
  FSelectedFont.Assign(Value);
end;

procedure TSideBarItemStyle.SetDisabledFont(const Value: TFont);
begin
  FDisabledFont.Assign(Value);
end;

{ TSideBarBulletStyle }

constructor TSideBarBulletStyle.Create(SideBar: TNiceSideBar);
begin
  inherited Create;
  FSideBar := SideBar;
  FKind := sbRound;
  FVisible := True;
  FSize := 5;
  FNormalColor := clWindowText;
  FHoverColor := clDefaultHoverFont;
  FSelectedColor := clWindowText;
  FDisabledColor := clGrayText;
  FNormalPenColor := clWindowText;
  FHoverPenColor := clDefaultHoverFont;
  FSelectedPenColor := clWindowText;
  FDisabledPenColor := clGrayText;
end;

procedure TSideBarBulletStyle.AssignTo(Dest: TPersistent);
begin
  if (Dest is TSideBarBulletStyle) then
  begin
    with TSideBarBulletStyle(Dest) do
    begin
      FKind := Self.FKind;
      FVisible := Self.FVisible;
      FSize := Self.FSize;
      FNormalColor := Self.FNormalColor;
      FHoverColor := Self.FHoverColor;
      FSelectedColor := Self.FSelectedColor;
      FDisabledColor := Self.FDisabledColor;
      FNormalPenColor := Self.FNormalPenColor;
      FHoverPenColor := Self.FHoverPenColor;
      FSelectedPenColor := Self.FSelectedPenColor;
      FDisabledPenColor := Self.FDisabledPenColor;
      FSideBar.Invalidate;
    end;
  end else
    inherited AssignTo(Dest);
end;

procedure TSideBarBulletStyle.SetKind(const Value: TSideBarBullet);
begin
  if (FKind <> Value) then
  begin
    FKind := Value;
    FSideBar.Invalidate;
  end;
end;

procedure TSideBarBulletStyle.SetNormalColor(const Value: TColor);
begin
  if (FNormalColor <> Value) then
  begin
    FNormalColor := Value;
    FSideBar.Invalidate;
  end;  
end;

procedure TSideBarBulletStyle.SetNormalPenColor(const Value: TColor);
begin
  if (FNormalPenColor <> Value) then
  begin
    FNormalPenColor := Value;
    FSideBar.Invalidate;
  end;  
end;

procedure TSideBarBulletStyle.SetSelectedColor(const Value: TColor);
begin
  if (FSelectedColor <> Value) then
  begin
    FSelectedColor := Value;
    FSideBar.Invalidate;
  end;  
end;

procedure TSideBarBulletStyle.SetSelectedPenColor(const Value: TColor);
begin
  if (FSelectedPenColor <> Value) then
  begin
    FSelectedPenColor := Value;
    FSideBar.Invalidate;
  end;  
end;

procedure TSideBarBulletStyle.SetDisabledColor(const Value: TColor);
begin
  if (FDisabledColor <> Value) then
  begin
    FDisabledColor := Value;
    FSideBar.Invalidate;
  end;  
end;

procedure TSideBarBulletStyle.SetDisabledPenColor(const Value: TColor);
begin
  if (FDisabledPenColor <> Value) then
  begin
    FDisabledPenColor := Value;
    FSideBar.Invalidate;
  end;  
end;

procedure TSideBarBulletStyle.SetSize(const Value: Integer);
begin
  if (FSize <> Value) then
  begin
    FSize := Value;
    FSideBar.Invalidate;
  end;
end;

procedure TSideBarBulletStyle.SetVisible(const Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    FSideBar.Invalidate;
  end;
end;

{ TSideBarScrollerStyle }

constructor TSideBarScrollerStyle.Create(SideBar: TNiceSideBar);
begin
  inherited Create;
  FSideBar := SideBar;
  FNormalColor := clBlack;
  FNormalArrowColor := clWhite;
  FHoverColor := clWhite;
  FHoverArrowColor := clBlack;
end;

procedure TSideBarScrollerStyle.AssignTo(Dest: TPersistent);
begin
  if (Dest is  TSideBarScrollerStyle) then
  begin
    with TSideBarScrollerStyle(Dest) do
    begin
      FNormalColor := Self.FNormalColor;
      FNormalArrowColor := Self.FNormalArrowColor;
      FHoverColor := Self.FHoverColor;
      FHoverArrowColor := Self.FHoverArrowColor;
      FSideBar.Invalidate;
    end;
  end else
    inherited AssignTo(Dest);
end;

procedure TSideBarScrollerStyle.SetNormalArrowColor(const Value: TColor);
begin
  if (FNormalArrowColor <> Value) then
  begin
    FNormalArrowColor := Value;
    FSideBar.Invalidate;
  end;
end;

procedure TSideBarScrollerStyle.SetNormalColor(const Value: TColor);
begin
  if (FNormalColor <> Value) then
  begin
    FNormalColor := Value;
    FSideBar.Invalidate;
  end;  
end;

end.

