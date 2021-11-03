unit McJSON;

interface

uses
  Classes, SysUtils;

type
  EMcJsonException = class(Exception);

  TJItemType  = (jitValue, jitObject, jitArray, jitUnset);
  TJValueType = (jvtString, jvtNumber, jvtBoolean, jvtNull);

  TMcJsonItem = class
  private
    fType   : TJItemType;  // item type (value/object/array)
    fKey    : string;      // item name
    fValue  : string;      // value (if item type is value)
    fValType: TJValueType; // value type (text/number/boolean)
    fChild  : TList;       // child nodes (if item type is object/array)
    fSpeedUp: Boolean;     // flag to speed up the parse task

    // property getters
    function fGetCount: Integer;
    function fGetKey(aIdx: Integer): string;
    function fGetType: TJItemType;
    function fGetItemByKey(aKey: string): TMcJsonItem;
    function fGetItemByIdx(aIdx: Integer): TMcJsonItem;
    function fHasChild: Boolean;
    function fIsNull  : Boolean;

    function fGetAsJSON   : string;
    function fGetAsObject : TMcJsonItem;
    function fGetAsArray  : TMcJsonItem;
    function fGetAsInteger: Integer;
    function fGetAsDouble : Double;
    function fGetAsString : string;
    function fGetAsBoolean: Boolean;
    function fGetAsNull   : string;

    // property setters
    procedure fSetType(aType: TJItemType);

    procedure fSetAsJSON(aStr: string);
    procedure fSetAsObject(aValue: TMcJsonItem);
    procedure fSetAsArray(aValue: TMcJsonItem);
    procedure fSetAsInteger(aValue: Integer);
    procedure fSetAsDouble(aValue: Double);
    procedure fSetAsString(aValue: string);
    procedure fSetAsBoolean(aValue: Boolean);
    procedure fSetAsNull(aValue: string);

    // parse with single pass per string
    function parse(const aCode: string; aPos, aLen: Integer): Integer;
    // read methods used by parse
    function readString(const aCode: string; out aStr:string; aPos, aLen: Integer): Integer;
    function readChar(const aCode: string; aChar: Char; aPos, aLen: Integer): Integer;
    function readKeyword(const aCode, aKeyword: string; aPos, aLen: Integer): Integer;
    function readValue(const aCode: string; aPos, aLen: Integer): Integer;
    function readObject(const aCode: string; aPos, aLen: Integer): Integer;
    function readArray(const aCode: string; aPos, aLen: Integer): Integer;
    function readNumber(const aCode: string; aPos, aLen: Integer): Integer;
    function readBoolean(const aCode: string; aPos, aLen: Integer): Integer;
    function readNull(const aCode: string; aPos, aLen: Integer): Integer;
    // aux functions used in ToString
    function sFormat(aHuman: Boolean): string;
    function sFormatItem(aStrS: TStringStream; const aIn, aNL, aSp: string): string;

  public
    property Count   : Integer    read fGetCount;
    property Key     : string     read fKey;
    property Value   : string     read fValue;
    property ItemType: TJItemType read fGetType write fSetType;

    property Keys  [aIdx: Integer]: string      read fGetKey;
    property Values[aIdx: Integer]: TMcJsonItem read fGetItemByIdx;
    property Items [aKey: string ]: TMcJsonItem read fGetItemByKey; default;

    property HasChild: Boolean read fHasChild;
    property IsNull  : Boolean read fIsNull;
    property SpeedUp : Boolean read fSpeedUp write fSpeedUp;

    property AsJSON   : string      read fGetAsJSON    write fSetAsJSON   ;
    property AsObject : TMcJsonItem read fGetAsObject  write fSetAsObject ;
    property AsArray  : TMcJsonItem read fGetAsArray   write fSetAsArray  ;
    property AsInteger: Integer     read fGetAsInteger write fSetAsInteger;
    property AsNumber : Double      read fGetAsDouble  write fSetAsDouble ;
    property AsString : string      read fGetAsString  write fSetAsString ;
    property AsBoolean: Boolean     read fGetAsBoolean write fSetAsBoolean;
    property AsNull   : string      read fGetAsNull    write fSetAsNull   ;

    constructor Create; overload;
    constructor Create(const aItem: TMcJsonItem); overload;
    constructor Create(const aCode: string); overload;
    destructor  Destroy; override;

    procedure Clear;

    function IndexOf(const aKey: string): Integer; overload;
    function Add(const aKey: string = ''): TMcJsonItem; overload;
    function Add(const aItem: TMcJsonItem): TMcJsonItem; overload;
    function Copy(const aItem: TMcJsonItem): TMcJsonItem; overload;
    function Clone: TMcJsonItem; overload;
    function Delete(aIdx: Integer): Boolean; overload;
    function Delete(const aKey: string): Boolean; overload;
    function HasKey(const aKey: string): Boolean;
    function IsEqual(const aItem: TMcJsonItem): Boolean;
    function Check(const aStr: string; aSpeedUp: Boolean = False): Boolean;

    function ToString: string; overload;
    function ToString(aHuman: Boolean = False): string; overload;

    procedure LoadFromFile(const aFileName: string; aUTF8: Boolean = True);
    procedure SaveToFile(const aFileName: string; aHuman: Boolean = True);

    // helpers
    function  GetTypeStr: string;
    function  GetValueStr: string;
    function  Qot(const aMsg: string): string;
    procedure Error(const Msg: string; const S1: string = '';
                                       const S2: string = '';
                                       const S3: string = '');
  end;

  function GetItemTypeStr(aType: TJItemType): string;
  function GetValueTypeStr(aType: TJValueType): string;

implementation

resourcestring
  SItemNil           = 'Object reference is nil: %s';
  SItemTypeInvalid   = 'Invalid item type: expected "%s" got "%s"';
  SItemTypeConvValue = 'Can''t convert item "%s" with value "%s" to "%s"';
  SItemTypeConv      = 'Can''t convert item "%s" to "%s"';
  //SItemKeyDublicate  = 'Duplicate key "%s"';
  SParsingError      = 'Error while parsing text: read "%s" at pos "%s"';

const
  WHITESPACE: set of char = [#9, #10, #13, #32];
  ESCAPES:    set of char = ['b', 't', 'n', 'f', 'r', 'u', '"', '\'];
  DIGITS:     set of char = ['0'..'9'];
  SIGNS:      set of char = ['+', '-'];
  OPENS:      set of char = ['{', '['];
  CLOSES:     set of char = ['}', ']'];

// Auxiliary functions
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

function escapeChar(const aStr: string; aPos: Integer): Integer;
var
  c: Integer;
begin
  case aStr[aPos] of
    '\':
    begin
      if (aStr[aPos+1] in ESCAPES)
        then c := 2
        else c := 1;
    end;
    else
      c := 1;
  end;
  Result := c;
end;

// removes all the whitespaces from the begining of the line
function trimWS(const aStr: string): string;
var
  i, j, k, n, len: Integer;
  sRes: string;
  opn : Boolean;
begin
  i := 1;
  j := 1;
  n := 1;
  len := Length(aStr);
  SetLength(sRes, len);
  opn := false;

  while ( i <= len ) do
  begin
    // check escapes
    if (i < len) then
      n := escapeChar(aStr, i);
    // control '"' for keys and string values.
    // if not escaped, toggle opn status
    if (n = 1) and (aStr[i] = '"') then
    begin
      opn := not opn;
    end;
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
  if (j > 1)
    then SetLength(sRes, j-1);
  // result
  Result := sRes;
end;

{ TMcJsonItem }

function TMcJsonItem.fGetCount: Integer;
begin
  if (Self = nil) then Error(SItemNil, 'get count');
  Result := fChild.Count;
end;

function TMcJsonItem.fGetKey(aIdx: Integer): string;
var
  aItem: TMcJsonItem;
begin
  // return the key of the idx-th child
  Result := '';
  aItem := fGetItemByIdx(aIdx);
  if (aItem <> nil) then
    Result := aItem.fKey;
end;

function TMcJsonItem.fGetType(): TJItemType;
begin
  if (Self = nil) then Error(SItemNil, 'get type');
  Result := fType;
end;

function TMcJsonItem.fGetItemByKey(aKey: string): TMcJsonItem;
var
  idx: Integer;
begin
  Result := nil;
  // check
  if (Self = nil) then Error(SItemNil, 'get item by key ' + Qot(aKey));
  // find index of item with aKey
  idx := Self.IndexOf(aKey);
  if (idx >= 0)
    then Result := TMcJsonItem(fChild.Items[idx])
    else Error(SItemNil, 'get item by key ' + Qot(aKey));
end;

function TMcJsonItem.fGetItemByIdx(aIdx: Integer): TMcJsonItem;
begin
  Result := nil;
  // check
  if (Self = nil) then Error(SItemNil, 'get item by index ' + IntToStr(aIdx));
  // type compatibility check
  if (fType <> jitObject) and
     (fType <> jitArray ) then
    Exit;
  // range check
  if (aIdx < 0) then
    Error(SItemNil, 'get item by index ' + IntToStr(aIdx));
  // object cannot return an element with an index higher than the maximum
  if (aIdx < fChild.Count)
    then Result := TMcJsonItem(fChild.Items[aIdx])
    else Error(SItemNil, 'get item by index ' + IntToStr(aIdx));
end;

function TMcJsonItem.fHasChild: Boolean;
begin
  if (Self = nil) then Error(SItemNil, 'has child');
  Result := ( fChild.Count > 0 );
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
      jvtNumber : Ans := StrToFloat(fValue);       // expected
      jvtString : Ans := StrToFloat(fValue);       // convertion
      jvtBoolean: Ans := Integer(fValue = 'true'); // convertion
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
      jvtBoolean: Ans := Boolean(fValue = 'true') ; // expected
      jvtString : Ans := Boolean(StrToInt(fValue)); // convertion
      jvtNumber : Ans := Boolean(StrToInt(fValue)); // convertion
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
  // no need to convert (null does not convet to nothing, not presume zero)
  Result := fValue;
end;

procedure TMcJsonItem.fSetType(aType: TJItemType);
var
  i: Integer;
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
    for i := 0 to (fChild.Count - 1) do
      TMcJsonItem(fChild.Items[i]).fKey := IntToStr(i);
  end
  // if an object is converted into an array, then remove the keys from its descendants
  else if (aType = jitArray) and (fType = jitObject) then
  begin
    for i := 0 to (fChild.Count - 1) do
      TMcJsonItem(fChild.Items[i]).fKey := '';
  end;
  // return aked type
  fType := aType;
end;

procedure TMcJsonItem.fSetAsJSON(aStr: string);
var
  c, len: Integer;
begin
  if (Self = nil) then Error(SItemNil, 'set as JSON');
  Clear;
  aStr := trimWS(aStr);
  len  := Length(aStr);
  c := Self.parse(aStr, 1, len);
  // valid-JSON
  if (c < len) then
    Error(SParsingError, 'bad json', IntToStr(len));
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
  i: Integer;
begin
  if (Self  = nil     ) then Error(SItemNil, 'set as integer');
  // if unset, set as value
  if (fType = jitUnset) then fSetType(jitValue);
  // if container, set aValue for each child
  if (fType = jitArray) or (fType = jitObject) then
  begin
     for i := 0 to (fChild.Count - 1) do
       TMcJsonItem(fChild[i]).AsInteger := aValue;
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
  i: Integer;
begin
  if (Self  = nil     ) then Error(SItemNil, 'set as double');
  // if unset, set as value
  if (fType = jitUnset) then fSetType(jitValue);
  // if container, set aValue for each child
  if (fType = jitArray) or (fType = jitObject) then
  begin
     for i := 0 to (fChild.Count - 1) do
       TMcJsonItem(fChild[i]).AsNumber := aValue;
  end
  else
  begin
    if (fValType <> jvtNumber) then fValType := jvtNumber;
    // set aValue as string
    fValue := FloatToStr(aValue);
  end;
end;

procedure TMcJsonItem.fSetAsString(aValue: string);
var
  i: Integer;
begin
  if (Self  = nil     ) then Error(SItemNil, 'set as string');
  // if unset, set as value
  if (fType = jitUnset) then fSetType(jitValue);
  // if container, set aValue for each child
  if (fType = jitArray) or (fType = jitObject) then
  begin
     for i := 0 to (fChild.Count - 1) do
       TMcJsonItem(fChild[i]).AsString := aValue;
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
  i: Integer;
begin
  if (Self  = nil     ) then Error(SItemNil, 'set as boolean');
  // if unset, set as value
  if (fType = jitUnset) then fSetType(jitValue);
  // if container, set aValue for each child
  if (fType = jitArray) or (fType = jitObject) then
  begin
     for i := 0 to (fChild.Count - 1) do
       TMcJsonItem(fChild[i]).AsBoolean := aValue;
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
  i: Integer;
begin
  if (Self  = nil     ) then Error(SItemNil, 'set as null');
  // if unset, set as value
  if (fType = jitUnset) then fSetType(jitValue);
  // if container, set aValue for each child
  if (fType = jitArray) or (fType = jitObject) then
  begin
     for i := 0 to (fChild.Count - 1) do
       TMcJsonItem(fChild[i]).AsNull := 'null'; // ignore aValue
  end
  else
  begin
    if (fValType <> jvtNull) then fValType := jvtNull;
    // set aValue as string
    fValue := 'null'; // ignore aValue
  end;
end;

function TMcJsonItem.parse(const aCode: string; aPos, aLen: Integer): Integer;
begin
  Result := aPos;
  // check position
  if (aPos > aLen) then
    Exit;
  // now in the first character our open parenthesis
  case aCode[aPos] of
    '{':                aPos := readObject (aCode, aPos, aLen); // recursive
    '[':                aPos := readArray  (aCode, aPos, aLen); // recursive
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
  // move on
  Result := aPos;
end;

function TMcJsonItem.readObject(const aCode: string; aPos, aLen: Integer): Integer;
var
  c: Integer;
  aItem: TMcJsonItem;
  sKey : string;
  first: Boolean;
begin
  // we got here because current symbol was '{'
  c  := aPos+1; // char iterator
  // set item type
  Self.fSetType(jitObject);
  first := True;
  // reading values until we reach a '}'
  while (aCode[c] <> '}') and (c <= aLen) do
  begin
    // parse ','
    if (not first) then
    begin
      c := readChar(aCode, ',', c, aLen);
      Inc(c);
    end;
    first := False;
    // parsing a "key"
    c := readString(aCode, sKey, c, aLen);
    // create a new item with parsed key
    // check duplicate (subject to speed up flag)
    aItem := nil;
    if (fSpeedUp) then
      aItem := Self.Add(sKey)
    else
    begin
      // valid-JSON
      if (Self.IndexOf(sKey) < 0)
        then aItem := Self.Add(sKey)
        else Error(SParsingError, 'duplicated key ' + sKey, IntToStr(c));
    end;
    // parse ':'
    c := readChar(aCode, ':', c+1, aLen);
    // parsing a value (recursive)
    if (aItem <> nil) then
      c := aItem.parse(aCode, c+1, aLen);
    // move on
    Inc(c);
  end;
  // valid-JSON
  if (c > aLen) and (aCode[aLen] <> '}') then
    Error(SParsingError, 'bad object', IntToStr(aLen));
  // stop at '}'
  Result := c;
end;

function TMcJsonItem.readArray(const aCode: string; aPos, aLen: Integer): Integer;
var
  c: Integer;
  aItem: TMcJsonItem;
  first: Boolean;
begin
  // we got here because current symbol was '['
  c := aPos+1;
  // set item type
  Self.fSetType(jitArray);
  first := True;
  // reading values until we reach a ']'
  while (aCode[c] <> ']') and (c <= aLen) do
  begin
    // parse ','
    if (not first) then
    begin
      c := readChar(aCode, ',', c, aLen);
      Inc(c);
    end;
    first := False;
    // Creating a new value (here explicity whith no key)
    aItem := Self.Add();
    // parsing values (recursive)
    c := aItem.parse(aCode, c, aLen); // 1,2,3 or {...},{...}
    if (c > aLen) then
      Error(SParsingError, 'bad array', IntToStr(aLen));
    // move on
    Inc(c);
  end;
  // valid-JSON
  if (c = aLen) and (aCode[c] <> ']') then
    Error(SParsingError, 'bad object', IntToStr(aLen));
  // stop at '}'
  Result := c;
end;

function TMcJsonItem.readString(const aCode: string; out aStr:string; aPos, aLen: Integer): Integer;
var
  c: Integer;
begin
  aStr := '';
  c    := aPos;
  if (aCode[c] = '"') then
  begin
    Inc(c);
    while (aCode[c] <> '"') and (c <= aLen) do
    begin
      // do escapes
      if (c < aLen)
        then Inc(c, escapeChar(aCode, c))
        else Inc(c);
    end;
    // copy between '"'
    if (aCode[aPos] = '"') and
       (aCode[c   ] = '"') then
    begin
      aStr := System.Copy(aCode, aPos+1, c-aPos-1); // "string" -> string
    end;
  end;
  // stop at '"'
  Result := c;
end;

function TMcJsonItem.readChar(const aCode: string; aChar: Char; aPos, aLen: Integer): Integer;
begin
  if ( aCode[aPos] <> aChar ) then
    Error(SParsingError, 'expected ' + aChar, IntToStr(aPos));
  // stop at aChar
  Result := aPos;
end;

function TMcJsonItem.readKeyword(const aCode, aKeyword: string; aPos, aLen: Integer): Integer;
var
  len: Integer;
  sAux: string;
begin
  len  := Length(aKeyword);
  sAux := System.Copy(aCode, aPos, len);
  if (Lowercase(sAux) <> aKeyword) then
    Error(SParsingError, 'invalid keyword "' + sAux + '"', IntToStr(aPos));
  // stop at keyword last char
  Result := aPos + len - 1;
end;

function TMcJsonItem.readValue(const aCode: string; aPos, aLen: Integer): Integer;
var
  c: Integer;
  sVal: string;
begin
  // we got here because current symbol is '"'
  c := aPos;
  // parse a "value"
  c := readString(aCode, sVal, c, aLen);
  // valid-JSON
  if (c > aLen) then
    Error(SParsingError, 'bad value', IntToStr(aLen));
  // set item and value types
  Self.fSetType(jitValue);
  Self.fValType := jvtString;
  Self.fValue   := sVal; // "value" -> value
  // stop at '"'
  Result := c;
end;

function TMcJsonItem.readNumber(const aCode: string; aPos, aLen: Integer): Integer;
var
  c: Integer;
begin
  // we got here because current symbol was '+/-' or Digit
  c := aPos;
  // 1. sign (optional)
  if aCode[c] in SIGNS
    then c := c + 1;
  // 2. some digits
  while (aCode[c] in DIGITS) do
    c := c + 1;
  // 3. decimal dot (optional)
  if aCode[c] = '.'
    then c := c + 1;
  // 4. fractional digits (optional)
  while (aCode[c] in DIGITS) do
    c := c + 1;
  // 5. scientific notation ...E-01
  if LowerCase(aCode[c]) = 'e' then
  begin
    c := c + 1;
    if aCode[c] in SIGNS
      then c := c + 1;
    while (aCode[c] in DIGITS) do
      c := c + 1;
  end;
  // valid-JSON: not a number
  if    (aCode[c] <> ','   ) and
    not (aCode[c] in CLOSES) then
    Error(SParsingError, 'bad number', IntToStr(c));
  // Result
  Self.fSetType(jitValue);
  Self.fValType := jvtNumber;
  Self.fValue   := System.Copy(aCode, aPos, c-aPos);
  // go back one char
  if (aCode[c] = ','    ) or
     (aCode[c] in CLOSES) then
    Dec(c);
  // stop at number last char
  Result := c;
end;

function TMcJsonItem.readBoolean(const aCode: string; aPos, aLen: Integer): Integer;
var
  c: Integer;
  sAux: string;
begin
  // we got here because current symbol was 't/T' or 'f/F'
  c    := aPos;
  sAux := '';
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
  // stop at keyword last char
  Result := c;
end;

function TMcJsonItem.readNull(const aCode: string; aPos, aLen: Integer): Integer;
var
  c: Integer;
  sAux: string;
begin
  // we got here because current symbol was 'n/N'
  c    := aPos;
  sAux := '';
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
  // stop at keyword last char
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
  i, len: Integer;
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
        aStrS.WriteString(Qot(fKey) + aSp);
      aStrS.WriteString('{' + aNL);
      len := Self.Count - 1;
      // use aSp to define if aHuman is true.
      if (aSp <> ':') then sGoIn := aIn + '  ';
      // mount recursively
      for i := 0 to len do
      begin
        aStrS.WriteString(sGoIn);
        aStrS.WriteString(TMcJsonItem(fChild[i]).sFormatItem(aStrS, sGoIn, aNL, aSP) );
        if ( i < len ) then
          aStrS.WriteString(',' + aNL);
      end;
      aStrS.WriteString(aNL + aIn + '}');
    end;
    // format JSON array
    jitArray:
    begin
      if (fKey <> '') then
        aStrS.WriteString(Qot(fKey) + aSp);
      aStrS.WriteString('[' + aNL);
      len := Self.Count - 1;
      // use aSp to define if aHuman is true.
      if (aSp <> ':') then sGoIn := aIn + '  ';
      // mount recursively
      for i := 0 to len do
      begin
        aStrS.WriteString(sGoIn);
        aStrS.WriteString(TMcJsonItem(fChild[i]).SFormatItem(aStrS, sGoIn, aNL, aSP) );
        if ( i < len ) then
          aStrS.WriteString(','+ aNL);
      end;
      aStrS.WriteString(aNL + aIn + ']');
    end;
    // format JSON key:value pair
    jitValue:
    begin
      if (fKey <> '') then
        aStrS.WriteString(Qot(fKey) + aSp);
      if (fValType = jvtString)
        then aStrS.WriteString(Qot(fValue))
        else aStrS.WriteString(    fValue );
    end;
  end;
end;

{ ---------------------------------------------------------------------------- }
{ TMcJsonItem - Public methods }
{ ---------------------------------------------------------------------------- }

constructor TMcJsonItem.Create;
begin
  fChild   := TList.Create;
  fType    := jitUnset;
  fSpeedUp := True;
end;

constructor TMcJsonItem.Create(const aItem: TMcJsonItem);
begin
  inherited Create;
  Create;
  Self.AsJSON := aItem.AsJSON;
end;

constructor TMcJsonItem.Create(const aCode: string);
begin
  inherited Create;
  Create;
  try
    Self.AsJSON := aCode;
  except
    Self.AsJSON := '';
  end;
end;

destructor TMcJsonItem.Destroy;
begin
  // mem leaks prevention
  Clear;
  fChild.Free;
  inherited;
end;

procedure TMcJsonItem.Clear;
var
  i: Integer;
begin
  if (Self = nil) then Error(SItemNil, 'clear');
  // recursively removes all childs
  for i := 0 to (fChild.Count - 1) do
  begin
    TMcJsonItem(fChild.Items[i]).Free;
  end;
  fChild.Clear;
end;

function TMcJsonItem.IndexOf(const aKey: string): Integer;
var
  i, idx: Integer;
begin
  idx    := -1;
  Result := idx;
  // check
  if  (Self = nil) then Error(SItemNil, 'index of');
  // looking for an element
  for i := 0 to (fChild.Count - 1) do
  begin
    if (TMcJsonItem(fChild.Items[i]).fKey = aKey) then
    begin
      idx := i;
      Break;
    end;
  end;
  // return the Result
  if (idx >= 0           ) and
     (idx <  fChild.Count) then
    Result := idx;
end;

function TMcJsonItem.Add(const aKey: string): TMcJsonItem;
var
  aItem: TMcJsonItem;
begin
  if (Self = nil) then Error(SItemNil, 'add using key ' + Qot(aKey));
  // check unset item
  if (fType = jitUnset) then
    fSetType(jitObject);
  // create a new item with aKey and add it.
  aItem := TMcJsonItem.Create;
  aItem.fKey := aKey;
  fChild.Add(aItem);
  // result aItem to permit chain
  Result := aItem;
end;

function TMcJsonItem.Add(const aItem: TMcJsonItem): TMcJsonItem;
begin
  if (Self = nil) then Error(SItemNil, 'add using item');
  // check unset item
  if (fType = jitUnset) then
    fSetType(jitObject);
  // add item.
  fChild.Add(aItem);
  // result aItem to permit chain
  Result := aItem;
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
  if (aIdx <  0   ) or
     (aIdx >  Size) or
     (Size <= 0   ) then
    Ans := False
  else
  begin
    // item to delete
    aItemDel := TMcJsonItem(fChild.Items[aIdx]);
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
  Result := ( fGetItemByKey(aKey) <> nil );
end;

function TMcJsonItem.IsEqual(const aItem: TMcJsonItem): Boolean;
begin
  Result := False;
  if (Self  =  nil) then Error(SItemNil, 'is equal item');
  if (aItem <> nil) then
    Result := (Self.AsJSON = aItem.AsJSON);
end;

function TMcJsonItem.Check(const aStr: string; aSpeedUp: Boolean): Boolean;
var
  aItem: TMcJsonItem;
begin
  aItem := TMcJsonItem.Create;
  try
    aItem.fSpeedUp := aSpeedUp;
    aItem.AsJSON   := aStr;
    Result := True;
//    Result := (aItem.AsJSON = trimWS(aStr));
  except
    Result := False;
  end;
  aItem.Free;
end;

function TMcJsonItem.ToString: string;
begin
  Result := sFormat(False);
end;

function TMcJsonItem.ToString(aHuman: Boolean): string;
begin
  Result := sFormat(aHuman);
end;

procedure TMcJsonItem.LoadFromFile(const aFileName: string; aUTF8: Boolean);
var
  aFile: Text;
  code, line: string;
begin
  try
    Clear;
    AssignFile(aFile, aFileName);
    {$I-}
    Reset(aFile);
    if (IOResult <> 0) then
      raise EMcJsonException.CreateFmt('JSON: Failed to load %s', [aFileName]);
    {$I+}
    while (not EOF(aFile)) do
    begin
      Readln(aFile, line);
      line := Trim(line);
      code := code + line;
    end;
    if aUTF8
      then Self.AsJSON := UTF8Decode(code)
      else Self.AsJSON := code;
  finally
    CloseFile(aFile);
  end;
end;

procedure TMcJsonItem.SaveToFile(const aFileName: string; aHuman: Boolean);
var
  aFile: TextFile;
begin
  try
    AssignFile(aFile, aFileName);
    Rewrite(aFile);
    Write(aFile, ToString(aHuman));
    if (IOResult <> 0) then
      raise EMcJsonException.CreateFmt('JSON: Failed to save %s', [aFileName]);
  finally
    CloseFile(aFile);
  end;
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

procedure TMcJsonItem.Error(const Msg: string; const S1: string;
                                               const S2: string;
                                               const S3: string);
var
  aStr: string;
begin
  aStr := Format(Msg, [S1, S2, S3]);
  raise EMcJsonException.Create(aStr);
end;

end.
                    const S2: string;
                                               const S3: string);
var
  aStr: string;
begin
  aStr := Format(Msg, [S1, S2, S3]);
  raise EMcJsonException.Create(aStr);
end;

end.
