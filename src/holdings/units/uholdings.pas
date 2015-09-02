unit uHoldings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uLibrary;

type

  IHolding = interface;


  IHoldingsEnumerator = interface
    ['{382D0026-DCFB-41C5-A134-A6DCCF1CE1F8}']
    function GetCurrent: IHolding;
    function MoveNext: Boolean;
    property Current: IHolding read GetCurrent;
  end;

  TCompareHoldings = function(const Item1, Item2: IHolding): Integer;
  IHoldings = interface
    ['{DC90726B-0F70-4ECC-B9BF-D7AE341E3234}']

    function Get(Index: Integer): IHolding;

    function GetLast: IHolding;
    function GetFirst: IHolding;

    function GetEnumerator: IHoldingsEnumerator;
    procedure Sort(Compare: TCompareHoldings);
    function IndexOf(const Item: IHolding): Integer;

    function Remove(const Item: IHolding): Integer;
    function AddNew: IHolding;

    function GetCount: Integer;

    function GetCreated: TDateTime;
    function GetModuleName: string;
    function GetUid: word;
    procedure SetCreated(const aCreated: TDateTime);
    procedure SetModuleName(const aModuleName: string);
    procedure SetUid(const aUid: word);


    property First: IHolding read GetFirst;
    property Last: IHolding read GetLast;
    property Count: Integer read GetCount;
    property Holdings[Index: Integer]: IHolding read Get; default;

    property ModuleName: string read GetModuleName write SetModuleName;
    property Created: TDateTime read GetCreated write SetCreated;
    property Uid: word read GetUid write SetUid;

  end;

  { IHolding }

  IHolding = interface
    ['{67BAC76B-EF52-467D-A337-DCF4BB3B57A6}']
    function GetName: String;
    procedure SetName(const aName: String);
    function GetShortDescription: String;
    procedure SetShortDescription(const aShortDescription: String);
    function GetIndex: Word;
    procedure SetIndex(const aIndex: Word);
    function GetVarType: TVarType;
    procedure SetVarType(const aVarType: TVarType);
    function GetValue: DWord;
    procedure SetValue(const aValue: DWord);
    function GetValueStr: string;
    procedure SetValueStr(const aValueStr: string);
    function GetMultipler: DWord;
    procedure SetMultipler(const aMultipler: DWord);

    property Name: String read GetName write SetName;
    property ShortDescription: String read GetShortDescription write SetShortDescription;
    property ValueStr: string read GetValueStr write SetValueStr;
    property Index: Word read GetIndex write SetIndex;
    property VarType: TVarType read GetVarType write SetVarType;
    property Value: DWord read GetValue write SetValue;
    property Multipler: DWord read GetMultipler write SetMultipler;
  end;

  //////////////////////////////////////////////////////////////////////////////
  function CreateHoldings: IHoldings; external 'holdings.dll';
  function GetHoldings(const aFileName: string): IHoldings; external 'holdings.dll';
  procedure SaveHoldings(const aHoldings: IHoldings; const aFileName: string); external 'holdings.dll';
implementation



end.

