//
// This unit is part of the GLScene Project, http://glscene.org
//
{
    History :  
     23/08/10 - Yar - Creation
  
}

unit GLSCrossXML;

interface

uses
  Classes,
  SysUtils,

  DOM,
  XMLRead,
  XMLWrite;

type
  GLSXMLDocument = TXMLDocument;
  GLSXMLNode = TDOMNode;
  GLSDOMNode = TDOMNode;

function GLSNewXMLDocument: GLSXMLDocument;
procedure ReleaseXMLDocument(var ADoc: GLSXMLDocument);
procedure WriteXMLFile(var ADoc: GLSXMLDocument; AStream: TStream); overload;
procedure ReadXMLFile(var ADoc: GLSXMLDocument; AStream: TStream); overload;
procedure WriteXMLFile(var ADoc: GLSXMLDocument; AFileName: string); overload;
procedure ReadXMLFile(var ADoc: GLSXMLDocument; AFileName: string); overload;
function GetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; out Value: string): Boolean; overload;
function GetXMLAttribute(const XMLNode: GLSXMLNode; Idx: Integer): GLSXMLNode; overload;
procedure SetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; const Value: string); overload;

function GetXMLAttributeCount(const XMLNode: GLSXMLNode): Integer;
function FindXMLNode(const ParentNode: GLSXMLNode; const NodeName: string; out ChildNode: GLSXMLNode): Boolean;
function CreateDOMNode(const ParentNode: GLSDOMNode; const NodeName: string): GLSDOMNode;
procedure SetXMLText(const DOMNode: GLSDOMNode; const AText: string);
function GetXMLText(const XMLNode: GLSXMLNode; out AText: string): Boolean;

implementation

function GLSNewXMLDocument: GLSXMLDocument;
begin
  Result := TXMLDocument.Create;
end;

procedure ReleaseXMLDocument(var ADoc: GLSXMLDocument);
begin
  FreeAndNil(ADoc);
end;

procedure WriteXMLFile(var ADoc: GLSXMLDocument; AStream: TStream);
begin
  XMLWrite.WriteXMLFile(ADoc, AStream);
end;

procedure ReadXMLFile(var ADoc: GLSXMLDocument; AStream: TStream);
begin
  XMLRead.ReadXMLFile(ADoc, AStream);
end;

procedure WriteXMLFile(var ADoc: GLSXMLDocument; AFileName: string);
begin
  XMLWrite.WriteXMLFile(ADoc, AFileName);
end;

procedure ReadXMLFile(var ADoc: GLSXMLDocument; AFileName: string);
begin
  XMLRead.ReadXMLFile(ADoc, AFileName);
end;

function GetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; out Value: string): Boolean;
var
  E: TDOMElement;
begin
  E := XMLNode as TDOMElement;
  Value := E[AttrName];
  Result := Length(Value) > 0;
end;

procedure SetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; const Value: string);
var
  E: TDOMElement;
begin
  E := XMLNode as TDOMElement;
  E[AttrName] := Value;
end;

function FindXMLNode(const ParentNode: GLSXMLNode; const NodeName: string; out ChildNode: GLSXMLNode): Boolean;
var
  E: TDOMElement;
begin
  E := ParentNode as TDomElement;
  ChildNode := E.FindNode(NodeName);
  Result := Assigned(ChildNode);
end;

function CreateDOMNode(const ParentNode: GLSDOMNode; const NodeName: string): GLSDOMNode;
begin
  Result := ParentNode.OwnerDocument.CreateElement(NodeName);
  ParentNode.AppendChild(Result);
end;

procedure SetXMLText(const DOMNode: GLSDOMNode; const AText: string);
begin
  DOMNode.AppendChild(DOMNode.ownerDocument.createTextNode(AText));
end;

function GetXMLText(const XMLNode: GLSXMLNode; out AText: string): Boolean;
var
  E: TDOMElement;
begin
  E := XMLNode as TDOMElement;
  AText := E.TextContent;
  Result := Length(AText)>0;
end;

function GetXMLAttributeCount(const XMLNode: GLSXMLNode): Integer;
var
  E: TDOMElement;
begin
  E := XMLNode as TDOMElement;
  Result := E.Attributes.Length;
end;

function GetXMLAttribute(const XMLNode: GLSXMLNode; Idx: Integer): GLSXMLNode;
var
  E: TDOMElement;
begin
  E := XMLNode as TDOMElement;
  Result := E.Attributes[Idx];
end;

end.
