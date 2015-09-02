unit uHoldingsSerialize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uHoldings, dom, XmlWrite, uSaxBase, uLibrary;

type

  { THoldingsDomFactory }

  THoldingsDomFactory = class
  public
    class procedure Serialize(const aHolding: IHolding;
      const aXDoc: TXmlDocument; const aParent: TDomNode);
    class procedure Serialize(const aHoldings: IHoldings; const aFileName: String);
  end;

implementation

{ THoldingsDomFactory }

class procedure THoldingsDomFactory.Serialize(const aHolding: IHolding;
  const aXDoc: TXmlDocument; const aParent: TDomNode);

  procedure AddTextNode(const aNodeName, aNodeValue: string;
    const aParent: TDomNode);
  var
    Node: TDomNode;
    TextNode: TDomNode;
  begin
    Node := aXDoc.CreateElement(aNodeName);
    TextNode := aXDoc.CreateTextNode(Utf8Decode(aNodeValue));

    Node.AppendChild(TextNode);
    aParent.AppendChild(Node);
  end;

var
  Node: TDomNode;
  Index: Integer;
begin
  if aHolding <> nil then
  begin
    Node := aXDoc.CreateElement(uSaxBase.sHolding);
    aParent.AppendChild(Node);

    TDOMElement(Node).SetAttribute(sName, aHolding.Name);
    TDOMElement(Node).SetAttribute(sIndex, IntToStr(aHolding.Index));

    Index := VarTypes.IndexOfData(aHolding.VarType);
    TDOMElement(Node).SetAttribute(sVarType, VarTypes.Keys[Index]);
    TDOMElement(Node).SetAttribute(sMultipler, IntToStr(aHolding.Multipler));

    AddTextNode(sShortDescription, aHolding.ShortDescription, Node);
    AddTextNode(sValueStr, aHolding.ValueStr, Node);
    AddTextNode(sValue, IntToStr(aHolding.Value), Node);

  end;

end;

class procedure THoldingsDomFactory.Serialize(const aHoldings: IHoldings;
  const aFileName: String);
var
  RootNode: TDomNode;
  XDoc:     TXmlDocument;
  Holdings: IHoldings;
  Holding: IHolding;
begin
  Holdings := aHoldings;

  XDoc := TXmlDocument.Create;
  try

    if Holdings <> nil then
    begin
      RootNode := XDoc.CreateElement(sHoldings);
      XDoc.AppendChild(RootNode);

      TDOMElement(RootNode).SetAttribute(sDate, DateTimeToStr(Holdings.Created));
      TDOMElement(RootNode).SetAttribute(sModule, Holdings.ModuleName);
      TDOMElement(RootNode).SetAttribute(sUid, IntToStr(Holdings.Uid));

      for Holding in Holdings do
          Serialize(Holding, XDoc, RootNode);

      WriteXMLFile(XDoc, utf8ToAnsi(aFileName));
    end;
  finally
    XDoc.Free;
  end;

end;

end.

