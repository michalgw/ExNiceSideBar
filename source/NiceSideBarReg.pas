unit NiceSideBarReg;

{$IFDEF FPC}
 {$MODE Delphi}
{$ENDIF}

interface

  procedure Register;

implementation

{$IFDEF FPC}
 {$R nicesidebar_images.res}
{$ENDIF}

uses
  {$IFDEF FPC}
  PropEdits, ComponentEditors,
  {$ELSE}
  DesignIntf, DesignEditors, ColnEdit,
  {$ENDIF}
  Dialogs, Classes, NiceSideBar;

type
  TNiceSideBarEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
  end;


{ TNiceSideBarEditor }

procedure TNiceSideBarEditor.ExecuteVerb(Index: Integer);
begin
  case Index of

    0: {$IFDEF FPC}
       EditCollection(Component, TNiceSideBar(Component).Items, 'Items');
       {$ELSE}
       ShowCollectionEditorClass(Designer, TCollectionEditor, Component,
         TNiceSideBar(Component).Items, 'Items', [coAdd, coDelete, coMove]);
       {$ENDIF}

    1: ShowMessage(
         'TNiceSideBar v1.00'#13 +
         '(c) Priyatna, 2003'#13 +
         'Bandung - Indonesia'#13 +
         'http://www.priyatna.org/'#13 +
         'mailto:me@priyatna.org'
       );

  end;
end;

function TNiceSideBarEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit Items ...';
    1: Result := 'About TNiceSideBar...';
  end;
end;

function TNiceSideBarEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;


procedure Register; 
begin
  RegisterComponents('priyatna.org', [TNiceSideBar]);
  RegisterComponentEditor(TNiceSideBar, TNiceSideBarEditor);
end;

end.
