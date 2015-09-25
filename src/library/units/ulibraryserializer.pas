unit uLibrarySerializer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dom, XmlWrite,
  uLibrary,
  uSaxBase;

type

  { TBaseSerializer }

  TBaseSerializer = class
  protected
    fXDoc: TXmlDocument;
  protected
    function AddRootNode(const aNodeName: string): TDomNode;
    function AddNode(const aNodeName: string;
      const aParentNode: TDomNode): TDomNode;
    function AddTextNode(const aNodeName, aNodeValue: string;
      const aParentNode: TDomNode): TDomNode;
    procedure AddAttribute(const aNode: TDomNode; const aAttrName, aAttrValue: string);
  end;

  { TLibrarySerializer }

  TLibrarySerializer = class(TBaseSerializer)
  private
    procedure ModuleSerialize(const aSublibrary: ISublibrary;
      const aParentNode: TDomNode; const aFolder: string);
  public
    procedure Serialize(const aLibrary: ILibrary);
  end;

  { TModuleDefineSerializer }

  TModuleDefineSerializer = class(TBaseSerializer)
  private
    procedure GroupsSerialize(const aGroups: IGroups; const aParentNode: TDomNode);
    procedure VarSerialize(const aVar: IVarDefine; const aParentNode: TDomNode);
    procedure DescriptionSerialize(const aDescription: IDescription; const aParentNode: TDomNode);
    procedure BitsSerialize(const aBits: IBits; const aParentNode: TDomNode);
    procedure PickListSerialize(const aPickList: IPickList; const aParentNode: TDomNode);
    procedure PresetsSerialize(const aPresets: IPresets; const aParentNode: TDomNode);
    procedure RegistersSerialize(const aRegisters: IRegisters;
      const aParentNode: TDomNode);
  public
    procedure Serialize(const aModule: IModule; const aFolder: string);
  end;


implementation

uses
  uSetting;

{ TBaseSerializer }

function TBaseSerializer.AddRootNode(const aNodeName: string): TDomNode;
begin
  Result := fXDoc.CreateElement(aNodeName);
  fXDoc.AppendChild(Result);
end;

function TBaseSerializer.AddNode(const aNodeName: string;
  const aParentNode: TDomNode): TDomNode;
begin
  Result := fXDoc.CreateElement(aNodeName);
  aParentNode.AppendChild(Result);
end;

function TBaseSerializer.AddTextNode(const aNodeName, aNodeValue: string;
  const aParentNode: TDomNode): TDomNode;
var
  TextNode: TDomNode;
begin
  Result := AddNode(aNodeName, aParentNode);

  TextNode := fXDoc.CreateTextNode(Utf8Decode(aNodeValue));
  Result.AppendChild(TextNode);
end;

procedure TBaseSerializer.AddAttribute(const aNode: TDomNode;
  const aAttrName, aAttrValue: string);
begin
  TDOMElement(aNode).SetAttribute(aAttrName, aAttrValue);
end;

{ TLibrarySerializer }

procedure TLibrarySerializer.Serialize(const aLibrary: ILibrary);
var
  P: Pointer;
  Folder: string;
  RootNode: TDomNode;
begin
  for P in aLibrary do
  begin
    fXDoc := TXmlDocument.Create;
    try
      // Корневой элемент
      RootNode := AddRootNode(sLib);

      case aLibrary.ExtractKey(P) of
        slDeveloper:
        begin
          Folder := GetSetting.DeveloperLibrary + PathDelim;
          AddAttribute(RootNode, sTypeLib, 'developer');
        end;

        slUser:
        begin
          Folder := GetSetting.UserLibrary + PathDelim;
          AddAttribute(RootNode, sTypeLib, 'user');
        end;
      end;

      ModuleSerialize(aLibrary.ExtractData(P), RootNode, Folder);

      if FileExists(Folder + 'module.jlf') then
        RenameFile(Folder + 'module.jlf', Folder + 'module.bak');
      WriteXMLFile(fXDoc, utf8ToAnsi(Folder + 'module.jlf'));

    finally
      fXDoc.Free;
    end;
  end;

end;

procedure TLibrarySerializer.ModuleSerialize(const aSublibrary: ISublibrary;
  const aParentNode: TDomNode; const aFolder: string);
var
  P: Pointer;
  Uid: word;
  Module: IModule;
  Node: TDomNode;

  ModuleDefineSerializer: TModuleDefineSerializer;
begin
  for P in aSubLibrary do
  begin
    Uid := aSublibrary.ExtractKey(P);
    Module := aSublibrary.ExtractData(P);

    Node := AddTextNode(sModule, Module.Name, aParentNode);
    AddAttribute(Node, sUid, IntToStr(Uid));
    AddAttribute(Node, sTypeSignature, IntToStr(Ord(Module.TypeSignature)));
    AddAttribute(Node, sTypeBootloader, IntToStr(Ord(Module.TypeBootloader)));

    if Module.ModuleDefine <> nil then
    begin
      ModuleDefineSerializer := TModuleDefineSerializer.Create;
      try
        ModuleDefineSerializer.Serialize(Module, aFolder);
      finally
        ModuleDefineSerializer.Free;
      end;
    end;

  end;
end;

procedure TModuleDefineSerializer.GroupsSerialize(const aGroups: IGroups;
  const aParentNode: TDomNode);
var
  Groups: IGroups;
  Node, GroupNode: TDomNode;
  GroupItem: IGroupItem;
begin

  for Groups in aGroups.GroupsList do
  begin
    Node := AddNode(sGncSet, aParentNode);
    AddAttribute(Node, sName, Utf8ToAnsi(Groups.ShortDescription));
    AddAttribute(Node, sImageIndex, Utf8ToAnsi(IntToStr(Groups.ImageIndex)));
    GroupsSerialize(Groups, Node);
  end;

  if aGroups.Group.Count > 0 then
  begin
    GroupNode := AddNode(sGncVar, aParentNode);
  	for GroupItem in aGroups.Group do
      AddTextNode(sUid, GroupItem.VarDefine.Uid, GroupNode);
  end;
end;

procedure TModuleDefineSerializer.VarSerialize(const aVar: IVarDefine; const aParentNode: TDomNode);
var
  Node, BitsNode, PickListNode: TDomNode;
begin
	Node := AddNode(sVarDefine, aParentNode);

  AddAttribute(Node, sVer, aVar.Ver);
  AddAttribute(Node, sVarType, VarTypes.Keys[VarTypes.IndexOfData(aVar.VarType)]);
  AddTextNode(sIndex, IntTOStr(aVar.Index), Node);
  AddTextNode(sShortDescription, aVar.ShortDescription, Node);
  AddTextNode(sName, aVar.Name, Node);
  AddTextNode(sUid, aVar.Uid, Node);
  case aVar.ReadAlways of
  	True:  AddTextNode(sReadAlways, '1', Node);
  	False: AddTextNode(sReadAlways, '0', Node);
  end;
  AddTextNode(sMultipler, IntToStr(aVar.Multipler), Node);
  AddTextNode(sAccess, IntToStr(Ord(aVar.Access)), Node);
  AddTextNode(sSynchronization, IntToStr(Ord(aVar.Synchronization)), Node);
  case aVar.SingleRequest of
  	True:  AddTextNode(sSingleRequest, '1', Node);
  	False: AddTextNode(sSingleRequest, '0', Node);
  end;
  AddTextNode(sKind, IntToStr(Ord(aVar.Kind)), Node);
  AddTextNode(sUnit, aVar.Measure, Node);
  DescriptionSerialize(aVar.Description, Node);

  BitsNode := AddNode(sBits, Node);
  if aVar.Bits.Count > 0 then
  begin
    if aVar.Bits.Name <> '' then
    	AddAttribute(BitsNode, sName, aVar.Bits.Name)
    else
    	BitsSerialize(aVar.Bits, BitsNode);
  end;

  PickListNode := AddNode(sPickList, Node);
  if aVar.PickList.Count > 0 then
  begin
	  if aVar.PickList.GetName <> '' then
    	AddAttribute(PickListNode, sName, aVar.PickList.GetName)
    else
      PickListSerialize(aVar.Picklist, PickListNode);
  end;
end;

procedure TModuleDefineSerializer.DescriptionSerialize(
  const aDescription: IDescription; const aParentNode: TDomNode);
var
  S: String;
  Node: TDomNode;
begin
  Node := AddNode(sDescription, aParentNode);
  for S in aDescription do
  	AddTextNode(sPara, S, Node);
end;

procedure TModuleDefineSerializer.BitsSerialize(const aBits: IBits;
  const aParentNode: TDomNode);
var
  P: Pointer;
  Index: Byte;
  Bit: IBitDefine;

  Node: TDomNode;
begin
  for P in aBits do
  begin
    Index := aBits.ExtractKey(P);
    Bit := aBits.ExtractData(P);

    Node := AddNode(sBitDefine, aParentNode);
    AddAttribute(Node, sVer, Bit.Ver);

    AddTextNode(sName, Bit.Name, Node);
    AddTextNode(sIndex, IntToStr(Index), Node);
    AddTextNode(sShortDescription, Bit.ShortDescription, Node);
    DescriptionSerialize(Bit.Description, Node);
  end;
end;

procedure TModuleDefineSerializer.PickListSerialize(const aPickList: IPickList;
  const aParentNode: TDomNode);
var
  P: Pointer;
  Value: Word;
  PickItem: IPickItem;

  Node: TDomNode;
begin
  for P in aPickList do
  begin
    Value := aPickList.ExtractKey(P);
    PickItem := aPickList.ExtractData(P);

    Node := AddNode(sPickItem, aParentNode);

    if PickItem.Ver <> '' then
    	AddAttribute(Node, sVer, PickItem.Ver);

    AddTextNode(sName, PickItem.Name, Node);
    AddTextNode(sValue, IntToStr(Value), Node);
    AddTextNode(sShortDescription, PickItem.ShortDescription, Node);
    DescriptionSerialize(PickItem.Description, Node);
  end;
end;

procedure TModuleDefineSerializer.PresetsSerialize(
  const aPresets: IPresets; const aParentNode: TDomNode);
var
  P: Pointer;
  Node, BitsNode, PickListNode: TDomNode;
  Bits: IBits;
  PickList: IPickList;
begin
  Node := AddNode(sPresets, aParentNode);

  for P in aPresets.BitsSet do
  begin
    Bits := aPresets.BitsSet.ExtractData(P);

    BitsNode := AddNode(sBits, Node);
    AddAttribute(BitsNode, sName, aPresets.BitsSet.ExtractKey(P));
    AddTextNode(sShortDescription, Bits.ShortDescription, BitsNode);

    BitsSerialize(Bits, BitsNode);
  end;

  for P in aPresets.PickLists do
  begin
    PickList := aPresets.PickLists.ExtractData(P);

    PickListNode := AddNode(sPickList, Node);
    AddAttribute(PickListNode, sName, aPresets.PickLists.ExtractKey(P));
    AddTextNode(sShortDescription, PickList.ShortDescription, PickListNode);

    PickListSerialize(PickList, PickListNode);
  end;

end;

procedure TModuleDefineSerializer.RegistersSerialize(const aRegisters: IRegisters;
  const aParentNode: TDomNode);
var
  Node, VarsNode: TDomNode;
  P, PP: Pointer;
  Vars: IVars;
begin
  Node := AddNode(sRegisters, aParentNode);
  for P in aRegisters do
  begin
    VarsNode := AddNode(sVars, Node);
    Vars := aRegisters.ExtractData(P);
    case aRegisters.ExtractKey(P) of
   		trHolding:
      	AddAttribute(VarsNode, sArrayType, sHolding);
   		trInput:
    		AddAttribute(VarsNode, sArrayType, sInput);
    end;
    for PP in Vars do
      VarSerialize(Vars.ExtractData(PP), VarsNode);
  end;
end;



{ TModuleDefineSerializer }
procedure TModuleDefineSerializer.Serialize(const aModule: IModule;
  const aFolder: string);
var
	RootNode: TDomNode;
  ConfigNode: TDomNode;
//  Groups: IGroups;
begin
  fXDoc := TXmlDocument.Create;
  try
    // Корневой элемент
    RootNode := AddRootNode(sModule);

    PresetsSerialize(aModule.ModuleDefine.PreSets, RootNode);
    RegistersSerialize(aModule.ModuleDefine.Registers, RootNode);

    ConfigNode := AddNode(sConfiguration, RootNode);
    GroupsSerialize(aModule.ModuleDefine.Configuration, ConfigNode);

    if FileExists(aFolder + aModule.Name + '.jlb') then
    	RenameFile(aFolder + aModule.Name + '.jlb', aFolder + aModule.Name + '.bak');
    WriteXMLFile(fXDoc, utf8ToAnsi(aFolder + aModule.Name + '.jlb'));
  finally
    fXDoc.Free;
  end;
end;


end.
