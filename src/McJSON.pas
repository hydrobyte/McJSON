(*******************************************************************************

  The MIT License (MIT)

  Copyright (c) 2021 - 2023,  HydroByte Software

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
*******************************************************************************)

unit McJSON;

interface

uses
  Classes, SysUtils;

type
  EMcJsonException = class(Exception);

  TJItemType  = (jitUnset, jitValue, jitObject, jitArray);
  TJValueType = (jvtString, jvtNumber, jvtBoolean, jvtNull);

  TMcJsonItemEnumerator = class;

  TMcJsonItem = class
  private
    fType   : TJItemType;  // item type (value/object/array)
    fValType: TJValueType; // value type (text/number/boolean)
    fKey    : string;      // item name
    fValue  : string;      // value (if item type is value)
    fChild  : TList;       // child nodes (if item type is object/array)

    // property getters
    function fGetCount: Integer;
    function fGetKey(aIdx: Integer): string;
    function fGetType: TJItemType;
    function fGetItemByKey(const aKey: string): TMcJsonItem;
    function fGetItemByIdx(aIdx: Integer): TMcJsonItem;
    function fHasChild: Boolean;
    function fIsNull  : Boolean;
    // AsSomething getters
    function fGetAsJSON   : string     ;
    function fGetAsObject : TMcJsonItem;
    function fGetAsArray  : TMcJsonItem;
    function fGetAsInteger: Integer    ;
    function fGetAsDouble : Double     ;
    function fGetAsString : string     ;
    function fGetAsBoolean: Boolean    ;
    function fGetAsNull   : string     ;
    // shortener getters
    function fGetJSON   (const aKey: string): string     ;
    function fGetObject (const aKey: string): TMcJsonItem;
    function fGetArray  (const aKey: string): TMcJsonItem;
    function fGetInteger(const aKey: string): Integer    ;
    function fGetDouble (const aKey: string): Double     ;
    function fGetString (const aKey: string): string     ;
    function fGetBoolean(const aKey: string): Boolean    ;
    function fGetNull   (const aKey: string): string     ;

    // property setters
    procedure fSetType(aType: TJItemType);
    // AsSomething setters.
    procedure fSetAsJSON   (aValue: string     );
    procedure fSetAsObject (aValue: TMcJsonItem);
    procedure fSetAsArray  (aValue: TMcJsonItem);
    procedure fSetAsInteger(aValue: Integer    );
    procedure fSetAsDouble (aValue: Double     );
    procedure fSetAsString (aValue: string     );
    procedure fSetAsBoolean(aValue: Boolean    );
    procedure fSetAsNull   (aValue: string     );
    // shortener setters
    procedure fSetJSON   (const aKey: string; aValue: string     );
    procedure fSetObject (const aKey: string; aValue: TMcJsonItem);
    procedure fSetArray  (const aKey: string; aValue: TMcJsonItem);
    procedure fSetInteger(const aKey: string; aValue: Integer    );
    procedure fSetDouble (const aKey: string; aValue: Double     );
    procedure fSetString (const aKey: string; aValue: string     );
    procedure fSetBoolean(const aKey: string; aValue: Boolean    );
    procedure fSetNull   (const aKey: string; aValue: string     );

    // string single-pass parser
    procedure parse(const aCode: string; aSpeed: Boolean); overload;
    function parse(const aCode: string; aPos, aLen: Integer; aSpeed: Boolean): Integer; overload;
    // read methods used by parse
    function readString (const aCode: string; var aStr:string; aPos, aLen: Integer): Integer;
    function readChar   (const aCode: string; aChar: Char; aPos: Integer): Integer;
    function readKeyword(const aCode, aKeyword: string; aPos, aLen: Integer): Integer;
    function readValue  (const aCode: string; aPos, aLen: Integer): Integer;
    function readObject (const aCode: string; aPos, aLen: Integer; aSpeed: Boolean): Integer;
    function readArray  (const aCode: string; aPos, aLen: Integer; aSpeed: Boolean): Integer;
    function readNumber (const aCode: string; aPos, aLen: Integer): Integer;
    function readBoolean(const aCode: string; aPos, aLen: Integer): Integer;
    function readNull   (const aCode: string; aPos, aLen: Integer): Integer;

    // aux functions used in ToString
    function sFormat(aHuman: Boolean): string;
    function sFormatItem(aStrS: TStringStream; const aIn, aNL, aSp: string): string;
    // other
    function isIndexValid(aIdx: Integer): Boolean;
    function InternalFloatToStr(aValue: Extended): string;
    function InternalStrToFloat(const aStr: string): Extended;

  public
    property Count   : Integer    read fGetCount;
    property Key     : string     read fKey     write fKey;
    property Value   : string     read fValue;
    property ItemType: TJItemType read fGetType write fSetType;

    property Keys  [aIdx      : Integer]: string      read fGetKey;
    property Items [aIdx      : Integer]: TMcJsonItem read fGetItemByIdx;
    property Values[const aKey: string ]: TMcJsonItem read fGetItemByKey; default;

    property HasChild: Boolean read fHasChild;
    property IsNull  : Boolean read fIsNull;
    // shorteners properties
    property J[const aKey: string]: string      read fGetJSON    write fSetJSON   ;
    property O[const aKey: string]: TMcJsonItem read fGetObject  write fSetObject ;
    property A[const aKey: string]: TMcJsonItem read fGetArray   write fSetArray  ;
    property I[const aKey: string]: Integer     read fGetInteger write fSetInteger;
    property D[const aKey: string]: Double      read fGetDouble  write fSetDouble ;
    property S[const aKey: string]: string      read fGetString  write fSetString ;
    property B[const aKey: string]: Boolean     read fGetBoolean write fSetBoolean;
    property N[const aKey: string]: string      read fGetNull    write fSetNull   ;
    // AsSomething properties
    property AsJSON   : string      read fGetAsJSON    write fSetAsJSON   ;
    property AsObject : TMcJsonItem read fGetAsObject  write fSetAsObject ;
    property AsArray  : TMcJsonItem read fGetAsArray   write fSetAsArray  ;
    property AsInteger: Integer     read fGetAsInteger write fSetAsInteger;
    property AsNumber : Double      read fGetAsDouble  write fSetAsDouble ;
    property AsString : string      read fGetAsString  write fSetAsString ;
    property AsBoolean: Boolean     read fGetAsBoolean write fSetAsBoolean;
    property AsNull   : string      read fGetAsNull    write fSetAsNull   ;

    constructor Create; overload; virtual; 
    constructor Create(aJItemType: TJItemType); overload; virtual;
    constructor Create(const aItem: TMcJsonItem); overload; virtual;
    constructor Create(const aCode: string); overload; virtual;
    destructor  Destroy; override;

    procedure Clear;
    function IndexOf(const aKey: string): Integer; overload;
    function Path(const aPath: string): TMcJsonItem; overload;
    function Add(const aKey: string = ''): TMcJsonItem; overload;
    function Add(const aKey: string; aItemType: TJItemType): TMcJsonItem; overload;
    function Add(aItemType: TJItemType): TMcJsonItem; overload;
    function Add(const aItem: TMcJsonItem): TMcJsonItem; overload;
    function Copy(const aItem: TMcJsonItem): TMcJsonItem; overload;
    function Clone: TMcJsonItem; overload;
    function Insert(const aKey: string; aIdx: Integer): TMcJsonItem; overload;
    function Insert(const aItem: TMcJsonItem; aIdx: Integer): TMcJsonItem; overload;
    function Delete(aIdx: Integer): Boolean; overload;
    function Delete(const aKey: string): Boolean; overload;
    function HasKey(const aKey: string): Boolean;
    function IsEqual(const aItem: TMcJsonItem): Boolean;
    function Check(const aStr: string; aSpeed: Boolean = False): Boolean;
    function CheckException(const aStr: string; aSpeed: Boolean = False): Boolean;
    function CountItems: Integer;

    // array shortener
    function At(aIdx: Integer; const aKey: string = ''): TMcJsonItem; overload;
    function At(const aKey: string; aIdx: Integer = -1): TMcJsonItem; overload;

    function ToString: string;{$IFNDEF FPC} overload;{$ELSE} override;{$ENDIF}
    function ToString(aHuman: Boolean = False): string; overload;
    function Minify(const aCode: string): string;

    procedure LoadFromStream(Stream: TStream; asUTF8: Boolean = True);
    procedure SaveToStream(Stream: TStream; asHuman: Boolean = True; asUTF8: Boolean = True);
    procedure LoadFromFile(const aFileName: string; asUTF8: Boolean = True);
    procedure SaveToFile(const aFileName: string; asHuman: Boolean = True; asUTF8: Boolean = True);

    function GetEnumerator: TMcJsonItemEnumerator;

    // helpers
    function  GetTypeStr: string;
    function  GetValueStr: string;
    function  Qot(const aMsg: string): string;
    function  QotKey(const aKey: string): string;
    procedure Error(const Msg: string; const S1: string = '';
                                       const S2: string = '';
                                       const S3: string = '');
  end;

  // TMcJsonItemEnumerator
  TMcJsonItemEnumerator = class
  private
    fItem : TMcJsonItem;
    fIndex: Integer;
  public
    constructor Create(aItem: TMcJsonItem);
    function GetCurrent: TMcJsonItem;
    function MoveNext: Boolean;
    property Current: TMcJsonItem read GetCurrent;
  end;

  // Auxiliary escape types and functions
  TJEscapeType = (jetNormal , // #8 #9 #10 #12 #13 " \
                  jetStrict , // Normal + /
                  jetUnicode, // Strict + \uXXXX
                  jetNone  ); // backwards compatibility
  function McJsonEscapeString(const aStr: string; aEsc: TJEscapeType = jetNone): string;
  function McJsonUnEscapeString(const aStr: string): string;

  // Auxiliary functions
  function GetItemTypeStr(aType: TJItemType): string;
  function GetValueTypeStr(aType: TJValueType): string;

implementation

const C_MCJSON_VERSION = '1.1.3';
const C_EMPTY_KEY      = '__a3mptyStr__';

resourcestring
  SItemNil           = 'Object reference is nil: %s';
  SItemTypeInvalid   = 'Invalid item type: expected "%s" got "%s"';
  SItemTypeConvValue = 'Can''t convert item "%s" with value "%s" to "%s"';
  SItemTypeConv      = 'Can''t convert item "%s" to "%s"';
  SParsingError      = 'Error while parsing text: "%s" at pos "%s"';
  SIndexInvalid      = 'Invalid index: %s';

const
  WHITESPACE: set of Char = [#9, #10, #13, #32]; // \t(ab), \n(LF), \r(CR), spc
  LINEBREAK:  set of Char = [#10, #13];
  ESCAPES:    set of Char = ['b', 't', 'n', 'f', 'r', '"', '/', '\', 'u' ];
  DIGITS:     set of Char = ['0'..'9'];
  SIGNS:      set of Char = ['+', '-'];
  CLOSES:     set of Char = ['}', ']'];
  HEXA:       set of Char = ['0'..'9', 'A'..'F', 'a'..'f'];
  PATHSEPS:   set of Char = ['\', '/', '.'];
  // escape chars names
  CHAR_ESCAPE    = '\';
  CHAR_BACKSPACE = 'b';
  CHAR_H_TAB     = 't';
  CHAR_NEW_LINE  = 'n';
  CHAR_FORM_FEED = 'f';
  CHAR_C_RETURN  = 'r';
  CHAR_Q_MARK    = '"';
  CHAR_SOLIDUS   = '/';
  CHAR_R_SOLIDUS = '\';
  CHAR_U_HEX     = 'u';
  // escape chars integer values
  ID_BACKSPACE =   #8;
  ID_H_TAB     =   #9;
  ID_NEW_LINE  =  #10;
  ID_FORM_FEED =  #12;
  ID_C_RETURN  =  #13;
  ID_Q_MARK    =  #34; // '"'
  ID_SOLIDUS   =  #47; // '/'
  ID_R_SOLIDUS =  #92; // '\'
  ID_U_HEX     = #117; // 'u'

{ ---------------------------------------------------------------------------- }
{ Auxiliary private functions }
{ ---------------------------------------------------------------------------- }

function escapeChar(const aStr: string; aPos, aLen: Integer; var aUnk: Boolean): Integer;
var
  n: Integer;
begin
  aUnk := False;
  n    := 1;
  if (aStr[aPos] = '\') then
  begin
    // check next char is escapable
    if (aPos < aLen) and
       (aStr[aPos+1] in ESCAPES) then
    begin
      // one char escapes
      if (aStr[aPos+1] <> 'u') then
        n := 2
      else
      //  u+(4 hexa) escape
      begin
        if (aLen-aPos-1  >  4   ) and
           (aStr[aPos+2] in HEXA) and
           (aStr[aPos+3] in HEXA) and
           (aStr[aPos+4] in HEXA) and
           (aStr[aPos+5] in HEXA)
          then n := 6        // \u1234 (6 chars)
          else aUnk := True; // bad \u escape
      end
    // if not escapable
    end
    else aUnk := True;
  end;
  // return the gap escaped
  Result := n;
end;

function escapeWS(const aStr: string; aPos, aLen: Integer): Integer;
var
  n,c: Integer;
begin
  c := aPos;
  n := 0;
  while (c <= aLen) and (aStr[c] in WHITESPACE) do
  begin
    Inc(c);
    Inc(n);
  end;
  // return the gap escaped
  Result := n;
end;

// removes all the whitespaces from the begining of the line
function trimWS(const aStr: string): string;
var
  i, j, k, n, len: Integer;
  sRes: string;
  opn, unk: Boolean;
begin
  i := 1;
  j := 1;
  len := Length(aStr);
  sRes := '';
  SetLength(sRes, len);
  opn := false;

  while ( i <= len ) do
  begin
    // check escapes
    n := escapeChar(aStr, i, len, unk);
    // control '"' for keys and string values.
    // if not escaped, toggle opn status
    if (n = 1) and (aStr[i] = '"') then
      opn := not opn;
    // ignore whitespaces chars
    if not (opn) and (aStr[i] in WHITESPACE) then
      Inc(i)
    else
    // copy n chars from aStr to sRes and move on
    begin
      for k := 1 to n do
      begin
        sRes[j] := aStr[i];
        Inc(i);
        Inc(j);
      end;
    end;
  end;
  if (j > 1) then
    SetLength(sRes, j-1);
  // result
  Result := sRes;
end;

function findUtf8BOM(const Stream: TStream): Int64;
var
  bom: array[0..2] of Byte;
begin
  Result := 0;
  if (Stream.Size > Length(bom)) then
  begin
    Stream.Read(bom, sizeof(bom));
    if ( (bom[0] = $EF) and // UTF-8 BOM
         (bom[1] = $BB) and
         (bom[2] = $BF) ) then 
      Result := 3;
  end;
end;

{ ---------------------------------------------------------------------------- }
{ TMcJsonItem }
{ ---------------------------------------------------------------------------- }

function TMcJsonItem.fGetCount: Integer;
begin
  if (Self = nil) then Error(SItemNil, 'get count');
  if (Assigned(fChild))
    then Result := fChild.Count
    else Result := 0;
end;

function TMcJsonItem.fGetKey(aIdx: Integer): string;
var
  aItem: TMcJsonItem;
begin
  if (Self = nil) then Error(SItemNil, 'get key');
  // return the key of the idx-th child
  Result := '';
  aItem := fGetItemByIdx(aIdx);
  Result := aItem.fKey;
end;

function TMcJsonItem.fGetType(): TJItemType;
begin
  if (Self = nil) then Error(SItemNil, 'get type');
  Result := fType;
end;

function TMcJsonItem.fGetItemByKey(const aKey: string): TMcJsonItem;
var
  idx: Integer;
begin
  Result := nil;
  // check
  if (Self = nil) then Error(SItemNil, 'get item by key ' + Qot(aKey));
  // find index of item with aKey
  idx := Self.IndexOf(aKey);
  if (idx >= 0)
    then Result := TMcJsonItem(fChild[idx])
    else Error(SItemNil, 'get item by key ' + Qot(aKey));
end;

function TMcJsonItem.fGetItemByIdx(aIdx: Integer): TMcJsonItem;
begin
  // check
  if (Self = nil) then Error(SItemNil, 'get item by index ' + IntToStr(aIdx));
  // type compatibility check
  if (fType <> jitObject) and
     (fType <> jitArray ) then
    Error(SItemNil, 'get item by index ' + IntToStr(aIdx));
  // range check
  if (not isIndexValid(aIdx)) then
    Error(SIndexInvalid, 'get item by index ' + IntToStr(aIdx));
  // return valid child at index aIdx
  Result := TMcJsonItem(fChild[aIdx]);
end;

function TMcJsonItem.fHasChild: Boolean;
begin
  if (Self = nil) then Error(SItemNil, 'has child');
  if (Assigned(fChild))
    then Result := ( fChild.Count > 0 )
    else Result := False;
end;

function TMcJsonItem.fIsNull: Boolean;
begin
  if (Self = nil) then Error(SItemNil, 'is null');
  Result := ( fValType = jvtNull );
end;

function TMcJsonItem.fGetAsJSON(): string;
begin
  if (Self = nil) then Error(SItemNil, 'get as JSON');
  Result := ToString(False);
end;

function TMcJsonItem.fGetAsObject: TMcJsonItem;
begin
  if      (Self = nil        ) then Error(SItemNil, 'get as object')
  else if (fType <> jitObject) then Error(SItemTypeInvalid, 'object', GetTypeStr);
  // return a compatible value type
  Result := Self;
end;

function TMcJsonItem.fGetAsArray: TMcJsonItem;
begin
  if      (Self = nil       ) then Error(SItemNil, 'get as array')
  else if (fType <> jitArray) then Error(SItemTypeInvalid, 'array', GetTypeStr);
  // return a compatible value type
  Result := Self;
end;

function TMcJsonItem.fGetAsInteger: Integer;
var
  Ans: Integer;
  Aux: Integer;
begin
  Ans := 0;
  Aux := 0;
  if      (Self = nil       ) then Error(SItemNil, 'get as integer')
  else if (fType <> jitValue) then Error(SItemTypeInvalid, 'value', GetTypeStr);
  // return a compatible value type
  // try to convert
  try
    case fValType of
      jvtNumber : Ans := StrToInt(fValue);         // expected
      jvtString : Ans := StrToInt(fValue);         // convertion
      jvtBoolean: Ans := Integer(fValue = 'true'); // convertion
      else        Aux := -1;
    end;
  except
    Error(SItemTypeConvValue, GetValueStr, fValue, 'integer');
  end;
  // can´t convert, value type does not permit it
  if (Aux = -1) then
    Error(SItemTypeConv, GetValueStr, 'integer');
  Result := Ans;
end;

function TMcJsonItem.fGetAsDouble: Double;
var
  Ans: Double;
  Aux: Integer;
begin
  Ans := 0.0;
  Aux := 0;
  if      (Self = nil       ) then Error(SItemNil, 'get as double')
  else if (fType <> jitValue) then Error(SItemTypeInvalid, 'value', GetTypeStr);
  // return a compatible value type
  // try to convert
  try
    case fValType of
      jvtNumber : Ans := InternalStrToFloat(fValue); // expected
      jvtString : Ans := InternalStrToFloat(fValue); // convertion
      jvtBoolean: Ans := Integer(fValue = 'true');   // convertion
      else        Aux := -1;
    end;
  except
    Error(SItemTypeConvValue, GetValueStr, fValue, 'double');
  end;
  // can´t convert, value type does not permit it
  if (Aux = -1) then
    Error(SItemTypeConv, GetValueStr, 'double');
  Result := Ans;
end;

function TMcJsonItem.fGetAsString: string;
begin
  if      (Self = nil       ) then Error(SItemNil, 'get as string')
  else if (fType <> jitValue) then Error(SItemTypeInvalid, 'value', GetTypeStr);
  // return fValue that is string already
  // no need to convert
  Result := fValue;
end;

function TMcJsonItem.fGetAsBoolean: Boolean;
var
  Ans: Boolean;
  Aux: Integer;
begin
  Ans := False;
  Aux := 0;
  if      (Self = nil       ) then Error(SItemNil, 'get as boolean')
  else if (fType <> jitValue) then Error(SItemTypeInvalid, 'value', GetTypeStr);
  // return a compatible value type
  // try to convert
  try
    case fValType of
      jvtBoolean: Ans := Boolean(fValue = 'true'  ); // expected
      jvtString : Ans := Boolean(StrToBool(fValue)); // convertion
      jvtNumber : Ans := Boolean(StrToInt(fValue) ); // convertion
      else        Aux := -1;
    end;
  except
    Error(SItemTypeConvValue, GetValueStr, fValue, 'boolean');
  end;
  // can´t convert, value type does not permit it
  if (Aux = -1) then
    Error(SItemTypeConv, GetValueStr, 'boolean');
  Result := Ans;
end;

function TMcJsonItem.fGetAsNull: string;
begin
  if      (Self = nil       ) then Error(SItemNil, 'get as null')
  else if (fType <> jitValue) then Error(SItemTypeInvalid, 'value', GetTypeStr);
  // return fValue that is string already
  // no need to convert (null does not convet to anything, not presume zero)
  Result := fValue;
end;

// shortener getters
function TMcJsonItem.fGetJSON(const aKey: string): string;
begin
  if ( not HasKey(aKey) )
    then Result := Self.Add(aKey, jitObject).AsJSON
    else Result := Self[aKey].AsJSON;
end;

function TMcJsonItem.fGetObject(const aKey: string): TMcJsonItem;
begin
  if ( not HasKey(aKey) )
    then Result := Self.Add(aKey, jitObject).AsObject
    else Result := Self[aKey].AsObject;
end;

function TMcJsonItem.fGetArray(const aKey: string): TMcJsonItem;
begin
  if ( not HasKey(aKey) )
    then Result := Self.Add(aKey, jitArray).AsArray
    else Result := Self[aKey].AsArray;
end;

function TMcJsonItem.fGetInteger(const aKey: string): Integer;
begin
  if ( not HasKey(aKey) )
    then Result := Self.Add(aKey, jitValue).AsInteger
    else Result := Self[aKey].AsInteger;
end;

function TMcJsonItem.fGetDouble(const aKey: string): Double;
begin
  if ( not HasKey(aKey) )
    then Result := Self.Add(aKey, jitValue).AsNumber
    else Result := Self[aKey].AsNumber;
end;

function TMcJsonItem.fGetString(const aKey: string): string;
begin
  if ( not HasKey(aKey) )
    then Result := Self.Add(aKey, jitValue).AsString
    else Result := Self[aKey].AsString;
end;

function TMcJsonItem.fGetBoolean(const aKey: string): Boolean;
begin
  if ( not HasKey(aKey) )
    then Result := Self.Add(aKey, jitValue).AsBoolean
    else Result := Self[aKey].AsBoolean;
end;

function TMcJsonItem.fGetNull(const aKey: string): string;
begin
  if ( not HasKey(aKey) )
    then Result := Self.Add(aKey, jitValue).AsNull
    else Result := Self[aKey].AsNull;
end;

procedure TMcJsonItem.fSetType(aType: TJItemType);
var
  k: Integer;
begin
  if (Self = nil) then Error(SItemNil, 'set type');
  // if an array or object is converted to a number, clear all descendants
  if (aType = jitValue) and (fType <> jitValue) then
  begin
    Clear;
    // the default value type is text
    fValType := jvtString;
  end
  // if a number is converted to an object or array, then take away the value from it
  else if (aType <> jitValue) and (fType = jitValue) then
  begin
    fValue := '';
  end
  // if the array is converted into an object, then assign keys to all its elements
  else if (aType = jitObject) and (fType = jitArray) then
  begin
    for k := 0 to (fChild.Count - 1) do
      TMcJsonItem(fChild[k]).fKey := IntToStr(k);
  end
  // if an object is converted into an array, then remove the keys from its descendants
  else if (aType = jitArray) and (fType = jitObject) then
  begin
    for k := 0 to (fChild.Count - 1) do
      TMcJsonItem(fChild[k]).fKey := '';
  end;
  // return aked type
  fType := aType;
end;

procedure TMcJsonItem.fSetAsJSON(aValue: string);
begin
  if (Self = nil) then Error(SItemNil, 'set as JSON');
  // speed up = no key duplication verification (default)
  Self.parse(aValue, True);
end;

procedure TMcJsonItem.fSetAsObject(aValue: TMcJsonItem);
begin
  if (Self  = nil) then Error(SItemNil, 'set as object');
  // if unset, set as value
  if (fType <> jitObject) then fSetType(jitObject);
  // make a copy (parsing)
  Self.AsJSON := aValue.AsJSON;
end;

procedure TMcJsonItem.fSetAsArray(aValue: TMcJsonItem);
begin
  if (Self  = nil) then Error(SItemNil, 'set as array');
  // if unset, set as value
  if (fType <> jitArray) then fSetType(jitArray);
  // make a copy (parsing)
  Self.AsJSON := aValue.AsJSON;
end;

procedure TMcJsonItem.fSetAsInteger(aValue: Integer);
var
  k: Integer;
begin
  if (Self  = nil     ) then Error(SItemNil, 'set as integer');
  // if unset, set as value
  if (fType = jitUnset) then fSetType(jitValue);
  // if container, set aValue for each child
  if (fType = jitArray) or (fType = jitObject) then
  begin
    for k := 0 to (fChild.Count - 1) do
      TMcJsonItem(fChild[k]).AsInteger := aValue;
  end
  else
  begin
    if (fValType <> jvtNumber) then fValType := jvtNumber;
    // set aValue as string
    fValue := IntToStr(aValue);
  end;
end;

procedure TMcJsonItem.fSetAsDouble(aValue: Double);
var
  k: Integer;
begin
  if (Self  = nil     ) then Error(SItemNil, 'set as double');
  // if unset, set as value
  if (fType = jitUnset) then fSetType(jitValue);
  // if container, set aValue for each child
  if (fType = jitArray) or (fType = jitObject) then
  begin
    for k := 0 to (fChild.Count - 1) do
      TMcJsonItem(fChild[k]).AsNumber := aValue;
  end
  else
  begin
    if (fValType <> jvtNumber) then fValType := jvtNumber;
    // set aValue as string
    fValue := InternalFloatToStr(aValue);
  end;
end;

procedure TMcJsonItem.fSetAsString(aValue: string);
var
  k: Integer;
begin
  if (Self  = nil     ) then Error(SItemNil, 'set as string');
  // if unset, set as value
  if (fType = jitUnset) then fSetType(jitValue);
  // if container, set aValue for each child
  if (fType = jitArray) or (fType = jitObject) then
  begin
    for k := 0 to (fChild.Count - 1) do
      TMcJsonItem(fChild[k]).AsString := aValue;
  end
  else
  begin
    if (fValType <> jvtString) then fValType := jvtString;
    // set aValue as string
    fValue := aValue;
  end;
end;

procedure TMcJsonItem.fSetAsBoolean(aValue: Boolean);
var
  k: Integer;
begin
  if (Self  = nil     ) then Error(SItemNil, 'set as boolean');
  // if unset, set as value
  if (fType = jitUnset) then fSetType(jitValue);
  // if container, set aValue for each child
  if (fType = jitArray) or (fType = jitObject) then
  begin
    for k := 0 to (fChild.Count - 1) do
      TMcJsonItem(fChild[k]).AsBoolean := aValue;
  end
  else
  begin
    if (fValType <> jvtBoolean) then fValType := jvtBoolean;
    // set aValue as string
    if aValue
      then fValue := 'true'
      else fValue := 'false';
  end;
end;

procedure TMcJsonItem.fSetAsNull(aValue: string);
var
  k: Integer;
begin
  if (Self  = nil     ) then Error(SItemNil, 'set as null');
  // if unset, set as value
  if (fType = jitUnset) then fSetType(jitValue);
  // if container, set aValue for each child
  if (fType = jitArray) or (fType = jitObject) then
  begin
    for k := 0 to (fChild.Count - 1) do
      TMcJsonItem(fChild[k]).AsNull := 'null'; // ignore aValue
  end
  else
  begin
    if (fValType <> jvtNull) then fValType := jvtNull;
    // set aValue as string
    fValue := 'null'; // ignore aValue
  end;
end;

// shortener setters
procedure TMcJsonItem.fSetJSON(const aKey: string; aValue: string);
begin
  if ( not HasKey(aKey) )
    then Self.Add(aKey).AsJSON := aValue
    else Self[aKey].AsJSON     := aValue;
end;

procedure TMcJsonItem.fSetObject(const aKey: string; aValue: TMcJsonItem);
begin
  if ( not HasKey(aKey) )
    then Self.Add(aKey).AsObject := aValue
    else Self[aKey].AsObject     := aValue;
end;

procedure TMcJsonItem.fSetArray(const aKey: string; aValue: TMcJsonItem);
begin
  if ( not HasKey(aKey) )
    then Self.Add(aKey).AsArray := aValue
    else Self[aKey].AsArray     := aValue;
end;

procedure TMcJsonItem.fSetInteger(const aKey: string; aValue: Integer);
begin
  if ( not HasKey(aKey) )
    then Self.Add(aKey).AsInteger := aValue
    else Self[aKey].AsInteger     := aValue;
end;

procedure TMcJsonItem.fSetDouble(const aKey: string; aValue: Double);
begin
  if ( not HasKey(aKey) )
    then Self.Add(aKey).AsNumber := aValue
    else Self[aKey].AsNumber     := aValue;
end;

procedure TMcJsonItem.fSetString(const aKey: string; aValue: string);
begin
  if ( not HasKey(aKey) )
    then Self.Add(aKey).AsString := aValue
    else Self[aKey].AsString     := aValue;
end;

procedure TMcJsonItem.fSetBoolean(const aKey: string; aValue: Boolean);
begin
  if ( not HasKey(aKey) )
    then Self.Add(aKey).AsBoolean := aValue
    else Self[aKey].AsBoolean     := aValue;
end;

procedure TMcJsonItem.fSetNull(const aKey: string; aValue: string);
begin
  if ( not HasKey(aKey) )
    then Self.Add(aKey).AsNull := aValue
    else Self[aKey].AsNull     := aValue;
end;

procedure TMcJsonItem.parse(const aCode: string; aSpeed: Boolean);
var
  c, len: Integer;
begin
  Clear;
  len := Length(aCode);
  c   := 1;
  try
    c := Self.parse(aCode, 1, len, aSpeed);
  except
    on EOutOfMemory do
      Error(SItemNil, 'out of memory with ' + IntToStr(CountItems) + ' items');
  end;
  // valid-JSON
  if (len = 0) then
    Error(SParsingError, 'bad json', IntToStr(len));
  if (c < len) then
    Error(SParsingError, 'bad json', IntToStr(c)  );
end;

function TMcJsonItem.parse(const aCode: string; aPos, aLen: Integer; aSpeed: Boolean): Integer;
begin
  Result := aPos;
  // check position
  if (aPos > aLen) then
    Exit;
  // escape white spaces
  Inc(aPos, escapeWS(aCode, aPos, aLen));
  // now in the first character our open parenthesis
  case aCode[aPos] of
    '{':                aPos := readObject (aCode, aPos, aLen, aSpeed); // recursive
    '[':                aPos := readArray  (aCode, aPos, aLen, aSpeed); // recursive
    '"':                aPos := readValue  (aCode, aPos, aLen);
    '0'..'9', '+', '-': aPos := readNumber (aCode, aPos, aLen);
    't', 'T', 'f', 'F': aPos := readBoolean(aCode, aPos, aLen);
    'n', 'N':           aPos := readNull   (aCode, aPos, aLen);
    else
    begin
      // valid-JSON
      Error(SParsingError, 'invalid char', IntToStr(aPos));
    end;
  end;
  // escape white spaces
  Inc(aPos, escapeWS(aCode, aPos, aLen));
  // move on
  Result := aPos;
end;

function TMcJsonItem.readObject(const aCode: string; aPos, aLen: Integer; aSpeed: Boolean): Integer;
var
  c: Integer;
  aItem: TMcJsonItem;
  sKey : string;
  first: Boolean;
begin
  // we got here because current symbol was '{'
  c  := aPos+1; 
  // escape white spaces
  Inc(c, escapeWS(aCode, c, aLen));
  // set item type
  Self.fSetType(jitObject);
  first := True;
  // reading values until we reach a '}'
  while ( (c <= aLen) and (aCode[c] <> '}') ) do
  begin
    // parse ','
    if (not first) then
      c := readChar(aCode, ',', c);
    first := False;
    // escape white spaces
    Inc(c, escapeWS(aCode, c, aLen));
    // parsing a "key", stop next to '"'
    c := readString(aCode, sKey, c, aLen);
    // check empty key like {"":"value"}
    if (sKey = '') then
      sKey := C_EMPTY_KEY;
    // create a new item with parsed key
    // check duplicate (subject to speed up flag)
    aItem := nil;
    if (aSpeed) then
      aItem := Self.Add(sKey)
    else
    begin
      // valid-JSON
      if (Self.IndexOf(sKey) < 0)
        then aItem := Self.Add(sKey)
        else Error(SParsingError, 'duplicated key ' + sKey, IntToStr(c));
    end;
    // escape white spaces
    Inc(c, escapeWS(aCode, c, aLen));
    // parse ':'
    c := readChar(aCode, ':', c);
    // escape white spaces
    Inc(c, escapeWS(aCode, c, aLen));
    // parsing a value (recursive)
    if (aItem <> nil) then
      c := aItem.parse(aCode, c, aLen, aSpeed);
    // move on
    Inc(c, escapeWS(aCode, c, aLen));
  end;
  // valid-JSON
  if (c > aLen) then
    Error(SParsingError, 'bad object', IntToStr(aLen))
  else if (aCode[c] <> '}') then
    Error(SParsingError, 'bad object', IntToStr(c)   );
  // stop next to '}'
  Result := c+1;
end;

function TMcJsonItem.readArray(const aCode: string; aPos, aLen: Integer; aSpeed: Boolean): Integer;
var
  c: Integer;
  aItem: TMcJsonItem;
  first: Boolean;
begin
  // we got here because current symbol was '['
  c := aPos+1;
  // escape white spaces
  Inc(c, escapeWS(aCode, c, aLen));
  // set item type
  Self.fSetType(jitArray);
  first := True;
  // reading values until we reach a ']'
  while ( (c <= aLen) and (aCode[c] <> ']') ) do
  begin
    // parse ','
    if (not first) then
      c := readChar(aCode, ',', c);
    first := False;
    // escape white spaces
    Inc(c, escapeWS(aCode, c, aLen));
    // Creating a new value (here explicity whith no key)
    aItem := Self.Add();
    // parsing values (recursive)
    c := aItem.parse(aCode, c, aLen, aSpeed); // 1,2,3 or {...},{...}
    if (c > aLen) then
      Error(SParsingError, 'bad array', IntToStr(aLen));
    // move on
    Inc(c, escapeWS(aCode, c, aLen));
  end;
  // valid-JSON
  if (c > aLen) then
    Error(SParsingError, 'bad object', IntToStr(aLen))
  else if (aCode[c] <> ']') then
    Error(SParsingError, 'bad array', IntToStr(c)   );
  // stop next to ']'
  Result := c+1;
end;

function TMcJsonItem.readString(const aCode: string; var aStr:string; aPos, aLen: Integer): Integer;
var
  c: Integer;
  unk: Boolean;
begin
  aStr := '';
  c    := aPos;
  if (aCode[aPos] = '"') then
  begin
    Inc(c);
    while ( (c <= aLen) and (aCode[c] <> '"') ) do
    begin
      // do escapes
      Inc(c, escapeChar(aCode, c, aLen, unk));
      // Valid-JSON: break lines
      if (c > aLen) or (aCode[c] in LINEBREAK) then
        Error(SParsingError, 'line break', IntToStr(c));
      // Valid-JSON: unknown escape
      if (unk) then
        Error(SParsingError, 'unknown escape', IntToStr(c));
    end;
    // copy between '"'
    if (aCode[c] = '"') then
      aStr := System.Copy(aCode, aPos+1, c-aPos-1); // "string" -> string
  end;
  // stop next to '"'
  if (c < aLen) then Inc(c);
  Result := c;
end;

function TMcJsonItem.readChar(const aCode: string; aChar: Char; aPos: Integer): Integer;
begin
  // Valid-JSON: unexpected char
  if ( aCode[aPos] <> aChar ) then
    Error(SParsingError, 'expected ' + aChar + ' got ' + aCode[aPos], IntToStr(aPos));
  // stop next to aChar
  Result := aPos+1;
end;

function TMcJsonItem.readKeyword(const aCode, aKeyword: string; aPos, aLen: Integer): Integer;
var
  len: Integer;
  sAux: string;
begin
  len := Length(aKeyword);
  // valid-JSON
  if (aPos+len > aLen) then
    Error(SParsingError, 'bad reserved keyword', IntToStr(aLen));
  sAux := System.Copy(aCode, aPos, len);
  // valid-JSON
  if (Lowercase(sAux) <> aKeyword) then
    Error(SParsingError, 'invalid reserved keyword ' + sAux, IntToStr(aPos));
  // stop next to keyword last char
  Result := aPos + len;
end;

function TMcJsonItem.readValue(const aCode: string; aPos, aLen: Integer): Integer;
var
  c: Integer;
  sVal: string;
begin
  // we got here because current symbol is '"'
  c := aPos;
  // parse a "value" -> value
  c := readString(aCode, sVal, c, aLen);
  // valid-JSON
  if (c > aLen) then
    Error(SParsingError, 'bad value', IntToStr(aLen));
  // set item and value types
  Self.fSetType(jitValue);
  Self.fValType := jvtString;
  Self.fValue   := sVal;
  // stop next to '"'
  Result := c;
end;

function TMcJsonItem.readNumber(const aCode: string; aPos, aLen: Integer): Integer;
var
  c, cEnd, ePos: Integer;
begin
  // we got here because current symbol was '+/-' or Digit
  c := aPos;
  // 1. sign (optional)
  if aCode[c] in SIGNS
    then Inc(c);
  // 2. some digits but not leading zeros
  while (aCode[c] in DIGITS) do
    Inc(c);
  // 3. decimal dot (optional)
  if aCode[c] = '.'
    then Inc(c);
  // 4. fractional digits (optional)
  while (aCode[c] in DIGITS) do
    Inc(c);
  // 5. scientific notation ...E-01
  if LowerCase(aCode[c]) = 'e' then
  begin
    ePos := c;
    Inc(c);
    if aCode[c] in SIGNS
      then Inc(c);
    while (aCode[c] in DIGITS) do
      Inc(c);
    // valid-JSON: bad scientific number
    if (ePos+1 = c) then
      Error(SParsingError, 'bad scientific number', IntToStr(c));
  end;
  // Result
  Self.fSetType(jitValue);
  Self.fValType := jvtNumber;
  Self.fValue   := System.Copy(aCode, aPos, c-aPos);
  // last number pos.
  cEnd := c;
  // escape white spaces
  Inc(c, escapeWS(aCode, c, aLen));
  // valid-JSON: not a number
  if not ((aCode[c] = ','    ) or
          (aCode[c] in CLOSES)) then
    Error(SParsingError, 'not a number', IntToStr(c));
  // valid-JSON: leading zero
  if (aCode[aPos]   =  '0') and (aPos < aLen) and (cEnd-aPos > 1) and
     (aCode[aPos+1] <> '.') then
    Error(SParsingError, 'bad number, leading zero', IntToStr(c));
  // stop next to number last char
  Result := c;
end;

function TMcJsonItem.readBoolean(const aCode: string; aPos, aLen: Integer): Integer;
var
  c: Integer;
begin
  // we got here because current symbol was 't/T' or 'f/F'
  c := aPos;
  // check boolean value 'true'
  if (aCode[aPos] = 't') or
     (aCode[aPos] = 'T') then
  begin
    c := readKeyword(aCode, 'true', c, aLen);
    Self.fValue := 'true';
  end
  // check boolean value 'false'
  else if (aCode[aPos] = 'f') or
          (aCode[aPos] = 'F') then
  begin
    c := readKeyword(aCode, 'false', c, aLen);
    Self.fValue := 'false';
  end;
  // set item and value types
  Self.fSetType(jitValue);
  Self.fValType := jvtBoolean;
  // stop next to keyword last char
  Result := c;
end;

function TMcJsonItem.readNull(const aCode: string; aPos, aLen: Integer): Integer;
var
  c: Integer;
begin
  // we got here because current symbol was 'n/N'
  c := aPos;
  // check if null
  if (aCode[aPos] = 'n') or
     (aCode[aPos] = 'N') then
  begin
    c := readKeyword(aCode, 'null', c, aLen);
    Self.fValue := 'null';
  end;
  // set item and value types
  Self.fSetType(jitValue);
  Self.fValType := jvtNull;
  // stop next to keyword last char
  Result := c;
end;

function TMcJsonItem.sFormat(aHuman: Boolean): string;
var
  strS: TStringStream;
  sNL, sSp: string;
begin
  strS := TStringStream.Create('');
  try
    // new line
    if aHuman
      then sNL := #13#10
      else sNL := '';
    // key value separator
    if (aHuman)
      then sSp := ': '
      else sSp := ':';
    // call format item recursively
    SFormatItem(strS, '', sNL, sSp);
    // final result;
    Result := strS.DataString;
  finally
    strS.Free;
  end;
end;

function TMcJsonItem.sFormatItem(aStrS: TStringStream; const aIn, aNL, aSp: string): string;
var
  k, len: Integer;
  sGoIn: string;
begin
  Result := '';
  sGoIn  := '';

  if (Self = nil) then
    Exit;

  case Self.fType of
    // format JSON object
    jitObject:
    begin
      if (fKey <> '') then
        aStrS.WriteString(QotKey(fKey) + aSp);
      aStrS.WriteString('{' + aNL);
      len := Self.Count - 1;
      // use aSp to define if aHuman is true.
      if (aSp <> ':') then sGoIn := aIn + '  ';
      // mount recursively
      for k := 0 to len do
      begin
        aStrS.WriteString(sGoIn);
        aStrS.WriteString(TMcJsonItem(fChild[k]).sFormatItem(aStrS, sGoIn, aNL, aSP) );
        if ( k < len ) then
          aStrS.WriteString(',' + aNL);
      end;
      aStrS.WriteString(aNL + aIn + '}');
    end;
    // format JSON array
    jitArray:
    begin
      if (fKey <> '') then
        aStrS.WriteString(QotKey(fKey) + aSp);
      aStrS.WriteString('[' + aNL);
      len := Self.Count - 1;
      // use aSp to define if aHuman is true.
      if (aSp <> ':') then sGoIn := aIn + '  ';
      // mount recursively
      for k := 0 to len do
      begin
        aStrS.WriteString(sGoIn);
        aStrS.WriteString(TMcJsonItem(fChild[k]).SFormatItem(aStrS, sGoIn, aNL, aSP) );
        if ( k < len ) then
          aStrS.WriteString(','+ aNL);
      end;
      aStrS.WriteString(aNL + aIn + ']');
    end;
    // format JSON key:value pair
    jitValue:
    begin
      if (fKey <> '') then
        aStrS.WriteString(QotKey(fKey) + aSp);
      if (fValType = jvtString)
        then aStrS.WriteString(Qot(fValue))
        else aStrS.WriteString(    fValue );
    end;
    // empty key and/or empty value
    jitUnset:
    begin
      if (fKey <> '') then
      begin
        aStrS.WriteString(QotKey(fKey) + aSp);
        if (fValType = jvtString)
          then aStrS.WriteString(Qot(fValue))
          else aStrS.WriteString(    fValue );
      end;
    end;
  end;
end;

function TMcJsonItem.isIndexValid(aIdx: Integer): Boolean;
var
  Ans: Boolean;
begin
  if (fChild.Count <= 0)
    then Ans := (AIdx  = 0)
    else Ans := (AIdx >= 0) and (AIdx < fChild.Count);
  Result := Ans;
end;

function TMcJsonItem.InternalFloatToStr(aValue: Extended): string;
var
  Fmt: TFormatSettings;
begin
  // internally, use "." as Decimal Separator.
  Fmt.DecimalSeparator := '.';
  // will raize an exception if aValue not valid.
  Result := FloatToStr(aValue, Fmt);
end;

function TMcJsonItem.InternalStrToFloat(const aStr: string): Extended;
var
  Fmt: TFormatSettings;
begin
  // internally, use "." as Decimal Separator.
  Fmt.DecimalSeparator := '.';
  // will raize an exception if aValue not valid.
  Result := StrToFloat(aStr, Fmt);
end;

{ ---------------------------------------------------------------------------- }
{ TMcJsonItem - Public methods }
{ ---------------------------------------------------------------------------- }

constructor TMcJsonItem.Create;
begin
  fChild   := nil;
  fType    := jitUnset;
end;

constructor TMcJsonItem.Create(aJItemType: TJItemType);
begin
  Create;
  Self.ItemType := aJItemType;
end;

constructor TMcJsonItem.Create(const aItem: TMcJsonItem);
begin
  Create;
  Self.AsJSON := aItem.AsJSON;
end;

constructor TMcJsonItem.Create(const aCode: string);
begin
  Create;
  try
    Self.AsJSON := aCode;
  except
    Self.AsJSON := '';
  end;
end;

destructor TMcJsonItem.Destroy;
begin
  Clear;
  fChild.Free;
  inherited Destroy;
end;

procedure TMcJsonItem.Clear;
var
  k: Integer;
begin
  if (Self   = nil) then Error(SItemNil, 'clear');
  if (Assigned(fChild)) then
  begin
    // free memory of all children (will be recursive)
    for k := 0 to (fChild.Count - 1) do
      TMcJsonItem(fChild[k]).Free;
    // clear list
    fChild.Clear;
  end;  
end;

function TMcJsonItem.IndexOf(const aKey: string): Integer;
var
  k, idx: Integer;
  item: TMcJsonItem;
begin
  idx    := -1;
  Result := idx;
  // check
  if (Self = nil) then Error(SItemNil, 'index of');
  if (not Assigned(fChild)) then Exit;
  // if self is an object
  if (Self.fType = jitObject) then
  begin
    // looking for an child element
    for k := 0 to (fChild.Count - 1) do
    begin
      if (TMcJsonItem(fChild[k]).fKey = aKey) then
      begin
        idx := k;
        Break;
      end;
    end;
  end
  else if (Self.fType = jitArray) then
  begin
    // looking for an child element: arrays items are "empty objects"
    for k := 0 to (fChild.Count - 1) do
    begin
      item := TMcJsonItem(fChild[k]).Items[0];
      if (TMcJsonItem(item).fKey = aKey) then
      begin
        idx := k;
        Break;
      end;
    end;
  end;
  // return the Result
  if (idx >= 0           ) and
     (idx <  fChild.Count) then
    Result := idx;
end;

function TMcJsonItem.Path(const aPath: string): TMcJsonItem;

  function GetKeyByPath(const aPath: string; var aPos, aLen: Integer): string;
  var
    c: Integer;
  begin
    Result := '';
    // check start with sep
    if (aPath[aPos] in PATHSEPS) then
      Inc(aPos);
    c := aPos;
    while (c <= aLen) and not (aPath[c] in PATHSEPS) do
    begin
      Inc(c);
    end;
    // copy between seps
    if (c-aPos >= 0) then
      Result := System.Copy(aPath, aPos, c-aPos);
    // move on  
    aPos := c;
  end;

var
  aItem: TMcJsonItem;
  c, len: Integer;
  sKey: string;
begin
  if (Self = nil) then Error(SItemNil, 'get by path ' + Qot(aPath));
  aItem  := Self;
  // parse path of keys using seps
  c   := 1;
  len := Length(aPath);
  while (c < len) do
  begin
    // get by key
    sKey := GetKeyByPath(aPath, c, len);
    if (sKey <> '') then
      aItem := aItem.fGetItemByKey(sKey);
  end;
  // result aItem to permit chain
  Result := aItem;
end;

function TMcJsonItem.Add(const aKey: string): TMcJsonItem;
var
  aItem: TMcJsonItem;
begin
  if (Self = nil) then Error(SItemNil, 'add using key ' + Qot(aKey));
  // check unset item
  if (fType = jitUnset) then
    fSetType(jitObject);
  // create a new item and check its parent type.
  aItem := TMcJsonItem.Create;
  // parent is array
  if (fType = jitArray) then
  begin
    // if not empty key, create object
    if (aKey <> '') then
      aItem.Add(aKey)
  end
  // parent is object or value
  else
  begin
    // check empty key {"":"value"}
    if (aKey = '')
      then aItem.fKey := C_EMPTY_KEY
      else aItem.fKey := aKey;
  end;
  // child on demand
  if (not Assigned(fChild)) then
    fChild := TList.Create;
  fChild.Add(aItem);
  // result aItem to permit chain
  Result := aItem;
end;

function TMcJsonItem.Add(const aKey: string; aItemType: TJItemType): TMcJsonItem;
var
  aItem: TMcJsonItem;
begin
  aItem := Self.Add(aKey);
  aItem.ItemType := aItemType;
  // result aItem to permit chain
  Result := aItem;
end;

function TMcJsonItem.Add(aItemType: TJItemType): TMcJsonItem;
var
  aItem: TMcJsonItem;
begin
  aItem := Self.Add();
  aItem.ItemType := aItemType;
  // result aItem to permit chain
  Result := aItem;
end;

function TMcJsonItem.Add(const aItem: TMcJsonItem): TMcJsonItem;
var
  aNewItem: TMcJsonItem;
begin
  if (Self = nil) then Error(SItemNil, 'add using item');
  // check unset item
  if (fType = jitUnset) then
    fSetType(jitObject);
  // check if self is an array
  if (fType <> jitArray) then
    Error(SItemTypeInvalid, 'array', GetTypeStr);
  // create a new item copy of aItem and add it.
  aNewItem := TMcJsonItem.Create(aItem);
  // add item.
  if (not Assigned(fChild)) then
    fChild := TList.Create;
  fChild.Add(aNewItem);
  // result aNewItem to permit chain
  Result := aNewItem;
end;

function TMcJsonItem.Copy(const aItem: TMcJsonItem): TMcJsonItem;
begin
  if (Self = nil) then Error(SItemNil, 'copy');
  // clear self and copy JSON from aItem
  Self.Clear;
  Self.AsJSON := aItem.AsJSON;
  // result self to permit chain
  Result := Self;
end;

function TMcJsonItem.Clone: TMcJsonItem;
var
  aItem: TMcJsonItem;
begin
  if (Self = nil) then Error(SItemNil, 'clone');
  // create a new item using self
  aItem := TMcJsonItem.Create(Self);
  // result aItem to permit chain
  Result := aItem;
end;

function TMcJsonItem.Insert(const aKey: string; aIdx: Integer): TMcJsonItem;
var
  aItem: TMcJsonItem;
begin
  if (Self = nil            ) then Error(SItemNil, 'insert using key ' + Qot(aKey));
  if (not isIndexValid(aIdx)) then Error(SIndexInvalid, 'insert index ' + IntToStr(aIdx));
  // check unset item
  if (fType = jitUnset) then
    fSetType(jitObject);
  // create a new item with aKey and insert it.
  aItem := TMcJsonItem.Create;
  aItem.fKey := aKey;
  if (not Assigned(fChild)) then
    fChild := TList.Create;
  fChild.Insert(aIdx, aItem);
  // result aItem to permit chain
  Result := aItem;
end;

function TMcJsonItem.Insert(const aItem: TMcJsonItem; aIdx: Integer): TMcJsonItem;
var
  aNewItem: TMcJsonItem;
begin
  if (Self = nil            ) then Error(SItemNil, 'insert using item');
  if (not isIndexValid(aIdx)) then Error(SIndexInvalid, 'insert index ' + IntToStr(aIdx));
  // check unset item
  if (fType = jitUnset) then
    fSetType(jitObject);
  // check if self is an array
  if (fType <> jitArray) then
    Error(SItemTypeInvalid, 'array', GetTypeStr);
  // create a new item copy of aItem and insert it.
  aNewItem := TMcJsonItem.Create(aItem);
  // insert item.
  if (not Assigned(fChild)) then
    fChild := TList.Create;
  fChild.Insert(aIdx, aNewItem);
  // result aNewItem to permit chain
  Result := aNewItem;
end;

function TMcJsonItem.Delete(aIdx: Integer): Boolean;
var
  Size: Integer;
  aItemDel: TMcJsonItem;
  Ans: Boolean;
begin
  Ans := False;
  if (Self = nil) then Error(SItemNil, 'delete index ' + IntToStr(aIdx));
  // check idx and size
  Size := fChild.Count;
  if (not isIndexValid(aIdx)) or (Size <= 0) then
    Ans := False
  else
  begin
    // item to delete
    aItemDel := TMcJsonItem(fChild[aIdx]);
    // delete position and free memory.
    if (aItemDel <> nil) then
    begin
      fChild.Delete(aIdx);
      aItemDel.Free;
      Ans := True;
    end;
  end;
  Result := Ans;
end;

function TMcJsonItem.Delete(const aKey: string): Boolean;
var
  Ans: Boolean;
  idx: Integer;
begin
  Ans := False;
  if (Self = nil) then Error(SItemNil, 'delete key ' + Qot(aKey));
  // find index of item with aKey
  idx := Self.IndexOf(aKey);
  if (idx >= 0) then
    Ans := Self.Delete(idx);
  Result := Ans;
end;

function TMcJsonItem.HasKey(const aKey: string): Boolean;
begin
  if (Self = nil) then Error(SItemNil, 'has key ' + Qot(aKey));
  // find index of item with aKey
  Result := ( Self.IndexOf(aKey) >= 0 );
end;

function TMcJsonItem.IsEqual(const aItem: TMcJsonItem): Boolean;
begin
  Result := False;
  if (Self  =  nil) then Error(SItemNil, 'is equal item');
  if (aItem <> nil) then
    Result := (Self.AsJSON = aItem.AsJSON);
end;

function TMcJsonItem.Check(const aStr: string; aSpeed: Boolean): Boolean;
var
  aItem: TMcJsonItem;
begin
  if (Self = nil) then Error(SItemNil, 'check');
  aItem := TMcJsonItem.Create;
  try
    try
      aItem.parse(aStr, aSpeed);
      Result := (aItem.AsJSON = trimWS(aStr));
    except
      Result := False;
    end;
  finally
    aItem.Free;
  end;
end;

function TMcJsonItem.CheckException(const aStr: string; aSpeed: Boolean): Boolean;
var
  aItem: TMcJsonItem;
begin
  if (Self = nil) then Error(SItemNil, 'check exception');
  aItem := TMcJsonItem.Create;
  try
    aItem.parse(aStr, aSpeed);
    Result := (aItem.AsJSON = trimWS(aStr));
  finally
    aItem.Free;
  end;
end;

function TMcJsonItem.CountItems: Integer;

  function CountItemsRec(const aItem: TMcJsonItem): Integer;
  var
    i, sum: Integer;
  begin
    sum := aItem.Count;
    for i := 0 to aItem.Count-1 do
      sum := sum + CountItemsRec( TMcJsonItem(aItem.fChild[i]) );
    Result := sum;
  end;

begin
  Result := CountItemsRec(Self);
end;

function TMcJsonItem.At(aIdx: Integer; const aKey: string): TMcJsonItem;
var
  aItem: TMcJsonItem;
begin
  // get by index
  aItem := fGetItemByIdx(aIdx);
  // get by key
  if ((aKey <> '') and (aItem <> nil)) then
    aItem := aItem.fGetItemByKey(aKey);
  Result := aItem;
end;

function TMcJsonItem.At(const aKey: string; aIdx: Integer): TMcJsonItem;
var
  aItem: TMcJsonItem;
begin
  // get by key
  aItem := fGetItemByKey(aKey);
  // get by index
  if ((aIdx >= 0) and (aItem <> nil)) then
    aItem := aItem.fGetItemByIdx(aIdx);
  Result := aItem;
end;

function TMcJsonItem.ToString: string;
begin
  Result := sFormat(False);
end;

function TMcJsonItem.ToString(aHuman: Boolean): string;
begin
  Result := sFormat(aHuman);
end;

function TMcJsonItem.Minify(const aCode: string): string;
begin
  Result := trimWS(aCode);
end;

procedure TMcJsonItem.LoadFromStream(Stream: TStream; asUTF8: Boolean);
var
  sCode: AnsiString;
  len  : Int64;
begin
  // check UTF-8 BOM
  Stream.Position := findUtf8BOM(Stream);
  // dimention and read stream to string
  len   := Stream.Size - Stream.Position;
  sCode := '';
  SetLength(sCode, len);
  Stream.Read(Pointer(sCode)^, len);
  // asUTF8 has difference in behavior in Delphi(true)/Lazarus(false).
  if (asUTF8)
    then Self.AsJSON := Utf8ToAnsi(sCode) // UTF-8 to ANSI
    else Self.AsJSON := sCode;            // keep as read
end;

procedure TMcJsonItem.SaveToStream(Stream: TStream; asHuman, asUTF8: Boolean);
var
  sCode: AnsiString;
  len  : Int64;
begin
  sCode := Self.ToString(asHuman);
  // asUTF8 has difference in behavior in Delphi(true)/Lazarus(false).
  if (asUTF8) then 
    sCode := AnsiToUtf8(sCode);
  len := Length(sCode);
  Stream.Write(Pointer(sCode)^, len);
end;

procedure TMcJsonItem.LoadFromFile(const aFileName: string; asUTF8: Boolean);
var
  fileStream: TFileStream;
begin
  fileStream := nil;
  try
    fileStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
    Clear;
    LoadFromStream(fileStream, asUTF8);
  finally
    fileStream.Free;
  end;
end;

procedure TMcJsonItem.SaveToFile(const aFileName: string; asHuman, asUTF8: Boolean);
var
  fileStream: TFileStream;
begin
  fileStream := nil;
  try
    fileStream := TFileStream.Create(aFileName, fmCreate or fmShareDenyWrite);
    SaveToStream(fileStream, asHuman, asUTF8);
  finally
    fileStream.Free;
  end;
end;

function TMcJsonItem.GetEnumerator: TMcJsonItemEnumerator;
var
  enum: TMcJsonItemEnumerator;
begin
  enum := TMcJsonItemEnumerator.Create(Self);
  Result := enum;
end;

function TMcJsonItem.GetTypeStr: string;
begin
  if (Self = nil) then Error(SItemNil, 'get type description');
  Result := GetItemTypeStr(Self.fType);
end;

function TMcJsonItem.GetValueStr: string;
begin
  if (Self = nil) then Error(SItemNil, 'get value type description');
  Result := GetValueTypeStr(Self.fValType);
end;

function TMcJsonItem.Qot(const aMsg: string): string;
begin
  Result := '"' + aMsg + '"';
end;

function TMcJsonItem.QotKey(const aKey: string): string;
begin
  Result := '';
  if (aKey = C_EMPTY_KEY)
    then Result := Qot('')
    else Result := Qot(aKey);
end;

procedure TMcJsonItem.Error(const Msg: string; const S1: string;
                                               const S2: string;
                                               const S3: string);
var
  aStr: string;
begin
  aStr := Format(Msg, [S1, S2, S3]);
  raise EMcJsonException.Create(aStr);
end;

{ ---------------------------------------------------------------------------- }
{ TMcJsonItemEnumerator }
{ ---------------------------------------------------------------------------- }

constructor TMcJsonItemEnumerator.Create(aItem: TMcJsonItem);
begin
  fItem  := aItem;
  FIndex := -1;
end;

function TMcJsonItemEnumerator.GetCurrent: TMcJsonItem;
begin
  if      (not Assigned(fItem.fChild) ) then Result := nil
  else if (fIndex < 0                 ) then Result := nil
  else if (fIndex < fItem.fChild.Count) then Result := TMcJsonItem(fItem.fChild[fIndex])
  else                                  Result := nil;
end;

function TMcJsonItemEnumerator.MoveNext: Boolean;
begin
  Inc(fIndex);
  if (fItem.fChild = nil)
    then Result := False
    else Result := (fIndex < fItem.fChild.Count);
end;

{ ---------------------------------------------------------------------------- }
{ Auxiliary public functions }
{ ---------------------------------------------------------------------------- }
function GetItemTypeStr(aType: TJItemType): string;
begin
  Result := 'unknown';
  case aType of
    jitValue : Result := 'value' ;
    jitObject: Result := 'object';
    jitArray : Result := 'array' ;
    jitUnset : Result := 'unset' ;
  end;
end;

function GetValueTypeStr(aType: TJValueType): string;
begin
  Result := 'unknown';
  case aType of
    jvtString : Result := 'string' ;
    jvtNumber : Result := 'number' ;
    jvtBoolean: Result := 'boolean';
    jvtNull   : Result := 'null'   ;
  end;
end;

function McJsonEscapeString(const aStr: string; aEsc: TJEscapeType): string;
var
  i, len: Integer;
  c: Char;
begin
  Result := '';
  len := Length(aStr);
  for i := 1 to len do
  begin
    c := aStr[i];
    case c of
      ID_BACKSPACE: Result := Result + CHAR_ESCAPE + CHAR_BACKSPACE;
      ID_H_TAB    : Result := Result + CHAR_ESCAPE + CHAR_H_TAB    ;
      ID_NEW_LINE : Result := Result + CHAR_ESCAPE + CHAR_NEW_LINE ;
      ID_FORM_FEED: Result := Result + CHAR_ESCAPE + CHAR_FORM_FEED;
      ID_C_RETURN : Result := Result + CHAR_ESCAPE + CHAR_C_RETURN ;
      ID_Q_MARK   : Result := Result + CHAR_ESCAPE + CHAR_Q_MARK   ;
      ID_R_SOLIDUS: Result := Result + CHAR_ESCAPE + CHAR_R_SOLIDUS;
      ID_SOLIDUS  : if (aEsc >= jetStrict)
                      then Result := Result + CHAR_ESCAPE + CHAR_SOLIDUS
                      else Result := Result + c;
      else
      begin
        if ( (aEsc >= jetUnicode) and
             ((Integer(c) <  32)  or
              (Integer(c) > 126)) ) then // \uXXXX
          Result := Result + CHAR_ESCAPE + CHAR_U_HEX + IntToHex(Integer(c), 4)
        else
          Result := Result + c;
      end;
    end;
  end;
end;

function McJsonUnEscapeString(const aStr: string): string;
var
  cs, cd, len: Integer;
  ndTrim: Boolean;
  ans: string;
begin
  cs  := 1; // char in source
  cd  := 1; // char in destiny
  ndTrim := False; // need trim
  len := Length(aStr);
  ans := '';
  SetLength(ans, len);
  while (cs <= len) do
  begin
    // no escape, copy and move on
    if (aStr[cs] <> '\') then
    begin
      ans[cd] := aStr[cs];
      Inc(cs);
      Inc(cd);
    end
    // check bad ending escaped. Example: 'a\' -> 'a'
    else if (cs = len) then
    begin
      ndTrim := True; // ignore it
      Inc(cs);
    end
    // there are escapes
    else 
    begin
      ndTrim := True;
      // move next to '\'
      Inc(cs);
      // unescape visible escaped chars
      if ((aStr[cs] = CHAR_Q_MARK   ) or
          (aStr[cs] = CHAR_SOLIDUS  ) or
          (aStr[cs] = CHAR_R_SOLIDUS)) then
      begin
        ans[cd] := aStr[cs];
        Inc(cd);
        Inc(cs);
      end
      // unescape u+(4 hexa) escaped chars
      else if (aStr[cs]  = CHAR_U_HEX) and
              (len-cs   >= 4         ) and
              (aStr[cs+1] in HEXA) and (aStr[cs+2] in HEXA) and
              (aStr[cs+3] in HEXA) and (aStr[cs+1] in HEXA) then
      begin
        try
          ans[cd] := Chr( StrToInt('$' + Copy(aStr, cs+1, 4)) );
          Inc(cd);
        except
          ; // invalid hexa, ignore and move on
        end;
        Inc(cs, 5); // uXXXX
      end
      // unescape other "invisible" escaped chars
      else 
      begin
        case aStr[cs] of
          CHAR_BACKSPACE: ans[cd] := ID_BACKSPACE;
          CHAR_H_TAB    : ans[cd] := ID_H_TAB    ;
          CHAR_NEW_LINE : ans[cd] := ID_NEW_LINE ;
          CHAR_FORM_FEED: ans[cd] := ID_FORM_FEED;
          CHAR_C_RETURN : ans[cd] := ID_C_RETURN ;
        end;
        Inc(cd);
        Inc(cs);
      end;
    end
  end;
  // trim extra size
  if (ndTrim) then
    SetLength(ans, cd-1);
  // return the string unescaped
  Result := ans;
end;

end.
