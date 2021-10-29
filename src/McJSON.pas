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
    function parse(const aCode: string; aPos: Integer = 1): Integer;
    // read methods used by parse
    function readValue(const aCode: string; aPos: Integer): Integer;
    function readObject(const aCode: string; aPos: Integer): Integer;
    function readArray(const aCode: string; aPos: Integer): Integer;
    function readNumber(const aCode: string; aPos: Integer): Integer;
    function readBoolean(const aCode: string; aPos: Integer): Integer;
    function readNull(const aCode: string; aPos: Integer): Integer;

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
    function ToString(aHuman: Boolean = False; const aIndent: string = ''): string; overload;

    procedure LoadFromFile(const aFileName: string; aUTF8: Boolean = True);
    procedure SaveToFile(const aFileName: string; aHuman: Boolean = True);

    // helpers
    function  GetTypeStr: string;
    function  GetValueStr: string;
    procedure Error(const Msg: string = '');
  end;

  function GetItemTypeStr(aType: TJItemType): string;
  function GetValueTypeStr(aType: TJValueType): string;

implementation

resourcestring
  SItemNil              = 'Invalid item object (nil)';
  SItemTypeInvalid      = 'Invalid item type';
  SItemTypeIncompatible = 'Incompatible item type';
  SItemKeyDublicate     = 'Duplicate key';
  SParsingError         = 'Error while parsing text';

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

// removes all the whitespaces from the begining of the line
function trimWS(aStr: string): string;
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
      case aStr[i] of
        '\':
        begin
          if (aStr[i+1] in ESCAPES)
            then n := 2
            else n := 1;
        end;
        else
          n := 1;
      end;
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
  if (Self = nil) then Error(SItemNil);
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
  if (Self = nil) then Error(SItemNil);
  Result := fType;
end;

function TMcJsonItem.fGetItemByKey(aKey: string): TMcJsonItem;
var
  idx: Integer;
begin
  Result := nil;
  // check
  if (Self = nil) then Error(SItemNil);
  // find index of item with aKey
  idx := Self.IndexOf(aKey);
  if (idx >= 0) then
    Result := TMcJsonItem(fChild.Items[idx]);
end;

function TMcJsonItem.fGetItemByIdx(aIdx: Integer): TMcJsonItem;
begin
  // Item type is not changed, unlike fGetItemByKey
  Result := nil;
  // check
  if (Self = nil) then Error(SItemNil);
  // type compatibility check
  if (fType <> jitObject) and
     (fType <> jitArray ) then
    Exit;
  // range check
  if (aIdx < 0) then
    Exit;
  // object cannot return an element with an index higher than the maximum
  if (aIdx < fChild.Count) then
    Result := TMcJsonItem(fChild.Items[aIdx]);
end;

function TMcJsonItem.fHasChild: Boolean;
begin
  if (Self = nil) then Error(SItemNil);
  Result := ( fChild.Count > 0 );
end;

function TMcJsonItem.fIsNull: Boolean;
begin
  if (Self = nil) then Error(SItemNil);
  Result := ( fValType = jvtNull );
end;

function TMcJsonItem.fGetAsJSON(): string;
begin
  if (Self = nil) then Error(SItemNil);
  Result := ToString(False, '');
end;

function TMcJsonItem.fGetAsObject: TMcJsonItem;
begin
  if      (Self = nil        ) then Error(SItemNil)
  else if (fType <> jitObject) then Error(SItemTypeInvalid);
  // return a compatible value type
  Result := Self;
end;

function TMcJsonItem.fGetAsArray: TMcJsonItem;
begin
  if      (Self = nil       ) then Error(SItemNil)
  else if (fType <> jitArray) then Error(SItemTypeInvalid);
  // return a compatible value type
  Result := Self;
end;

function TMcJsonItem.fGetAsInteger: Integer;
var
  Ans: Integer;
begin
  Ans := 0;
  if      (Self = nil       ) then Error(SItemNil)
  else if (fType <> jitValue) then Error(SItemTypeInvalid);
  // return a compatible value type
  // try to convert
  try
    case fValType of
      jvtNumber : Ans := StrToInt(fValue);         // expected
      jvtString : Ans := StrToInt(fValue);         // convertion
      jvtBoolean: Ans := Integer(fValue = 'true'); // convertion
      else Error(SItemTypeIncompatible);
    end;
  except
    Error(SItemTypeIncompatible);
  end;
  Result := Ans;
end;

function TMcJsonItem.fGetAsDouble: Double;
var
  Ans: Double;
begin
  Ans := 0.0;
  if      (Self = nil       ) then Error(SItemNil)
  else if (fType <> jitValue) then Error(SItemTypeInvalid);
  // return a compatible value type
  // try to convert
  try
    case fValType of
      jvtNumber : Ans := StrToFloat(fValue);       // expected
      jvtString : Ans := StrToFloat(fValue);       // convertion
      jvtBoolean: Ans := Integer(fValue = 'true'); // convertion
      else Error(SItemTypeIncompatible);
    end;
  except
    Error(SItemTypeIncompatible);
  end;
  Result := Ans;
end;

function TMcJsonItem.fGetAsString: string;
begin
  if      (Self = nil       ) then Error(SItemNil)
  else if (fType <> jitValue) then Error(SItemTypeInvalid);
  // return fValue that is string already
  // no need to convert
  Result := fValue;
end;

function TMcJsonItem.fGetAsBoolean: Boolean;
var
  Ans: Boolean;
begin
  Ans := False;
  if      (Self = nil       ) then Error(SItemNil)
  else if (fType <> jitValue) then Error(SItemTypeInvalid);
  // return a compatible value type
  // try to convert
  try
    case fValType of
      jvtBoolean: Ans := Boolean(fValue = 'true') ; // expected
      jvtString : Ans := Boolean(StrToInt(fValue)); // convertion
      jvtNumber : Ans := Boolean(StrToInt(fValue)); // convertion
      else Error(SItemTypeIncompatible);
    end;
  except
    Error(SItemTypeIncompatible);
  end;
  Result := Ans;
end;

function TMcJsonItem.fGetAsNull: string;
begin
  if      (Self = nil       ) then Error(SItemNil)
  else if (fType <> jitValue) then Error(SItemTypeInvalid);
  // return fValue that is string already
  // no need to convert
  Result := fValue;
end;

procedure TMcJsonItem.fSetType(aType: TJItemType);
var
  i: Integer;
begin
  if (Self = nil) then Error(SItemNil);
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
begin
  if (Self = nil) then Error(SItemNil);
  Clear;
  aStr := trimWS(aStr);
  Self.parse(aStr);
end;

procedure TMcJsonItem.fSetAsObject(aValue: TMcJsonItem);
begin
  if (Self  = nil) then Error(SItemNil);
  // if unset, set as value
  if (fType <> jitObject) then fSetType(jitObject);
  // make a copy (parsing)
  Self.AsJSON := aValue.AsJSON;
end;

procedure TMcJsonItem.fSetAsArray(aValue: TMcJsonItem);
begin
  if (Self  = nil) then Error(SItemNil);
  // if unset, set as value
  if (fType <> jitArray) then fSetType(jitArray);
  // make a copy (parsing)
  Self.AsJSON := aValue.AsJSON;
end;

procedure TMcJsonItem.fSetAsInteger(aValue: Integer);
begin
  if  (Self  = nil       ) then Error(SItemNil);
  if ((fType <> jitUnset ) and
      (fType <> jitValue)) then Error(SItemTypeInvalid);
  // if unset, set as value
  if (fType    =  jitUnset ) then fSetType(jitValue);
  if (fValType <> jvtNumber) then fValType := jvtNumber;
  // set aValue as string
  fValue := IntToStr(aValue);
end;

procedure TMcJsonItem.fSetAsDouble(aValue: Double);
begin
  if  (Self  = nil       ) then Error(SItemNil);
  if ((fType <> jitUnset ) and
      (fType <> jitValue)) then Error(SItemTypeInvalid);
  // if unset, set as value
  if (fType    =  jitUnset ) then fSetType(jitValue);
  if (fValType <> jvtNumber) then fValType := jvtNumber;
  // set aValue as string
  fValue := FloatToStr(aValue);
end;

procedure TMcJsonItem.fSetAsString(aValue: string);
begin
  if  (Self  = nil       ) then Error(SItemNil);
  if ((fType <> jitUnset ) and
      (fType <> jitValue)) then Error(SItemTypeInvalid);
  // if unset, set as value
  if (fType    =  jitUnset ) then fSetType(jitValue);
  if (fValType <> jvtString) then fValType := jvtString;
  // set aValue as string
  fValue := aValue;
end;

procedure TMcJsonItem.fSetAsBoolean(aValue: Boolean);
begin
  if  (Self  = nil       ) then Error(SItemNil);
  if ((fType <> jitUnset ) and
      (fType <> jitValue)) then Error(SItemTypeInvalid);
  // if unset, set as value
  if (fType    =  jitUnset  ) then fSetType(jitValue);
  if (fValType <> jvtBoolean) then fValType := jvtBoolean;
  // set aValue as string
  if aValue
    then fValue := 'true'
    else fValue := 'false';
end;

procedure TMcJsonItem.fSetAsNull(aValue: string);
begin
  if  (Self  = nil       ) then Error(SItemNil);
  if ((fType <> jitUnset ) and
      (fType <> jitValue)) then Error(SItemTypeInvalid);
  // if unset, set as value
  if (fType    =  jitUnset) then fSetType(jitValue);
  if (fValType <> jvtNull ) then fValType := jvtNull;
  // set aValue as string
  fValue := aValue;
end;

function TMcJsonItem.parse(const aCode: string; aPos: Integer): Integer;
begin
  Result := aPos;

  if (Length(aCode) < aPos) or
     (aCode[aPos]   = #0  ) then
    Exit;

  // now in the first character our open parenthesis
  case aCode[aPos] of
    '"':                Result := readValue(aCode, aPos);   // generic value (text)
    '{':                Result := readObject(aCode, aPos);  // Object
    '[':                Result := readArray(aCode, aPos);   // Array
    '0'..'9', '+', '-': Result := readNumber(aCode, aPos);  // number
    't', 'T', 'f', 'F': Result := readBoolean(aCode, aPos); // boolean
    'n', 'N':           Result := readNull(aCode, aPos);    // null
    else
    begin
      // Should raise an exception here
      raise EMcJsonException.CreateFmt('JSON parsing error at char %s', [aPos]);
      Result := aPos;
    end;
  end;
end;

function TMcJsonItem.readValue(const aCode: string; aPos: Integer): Integer;
var
  c, len: Integer;
begin
  // we get here because current symbol is '"'
  c   := aPos+1;
  len := Length(aCode);
  // parse a value"
  while (aCode[c] <> '"') and (c <= len) do
  begin
    case aCode[c] of
      '\':
      begin
        if (aCode[c+1] in ESCAPES)
          then c := c + 2
          else c := c + 1;
      end;
      else
        c := c + 1;
    end;
  end;

  // valid-JSON 
  if (c > len) then Error(SParsingError);

  Self.fSetType(jitValue);
  Self.fValType := jvtString;
  Self.fValue   := System.Copy(aCode, aPos+1, c-aPos-1); // val" -> val

  Result := c+1;
end;

function TMcJsonItem.readObject(const aCode: string; aPos: Integer): Integer;
var
  c, ct, len: Integer;
  aItem: TMcJsonItem;
  sKey : string;
begin
  // we get here because current symbol was '{'
  c   := aPos+1; // char iterator
  ct  := aPos;   // char iterator current object
  len := Length(aCode);
  // set item type
  Self.fSetType(jitObject);
  // reading values until we reach a '}'
  while (aCode[c] <> '}') and (c <= len) do
  begin
    // parsing a key
    while (aCode[c] <> ':') and (c <= len) do
    begin
      // valid-JSON
      if (aCode[c] in OPENS ) then Error(SParsingError);
      if (aCode[c] in CLOSES) then Error(SParsingError);
      c := c + 1;
    end;
    // create a new item with parsed key
    sKey := System.Copy(aCode, ct+2, c-ct-3); //{"key":val -> key
    // check duplicate (subject to speed up flag)
    aItem := nil;
    if (fSpeedUp) then
      aItem := Self.Add(sKey)
    else
      // valid-JSON
      if (fGetItemByKey(sKey) = nil)
        then aItem := Self.Add(sKey)
        else Error(SItemKeyDublicate);
    // parsing a value (recursive)
    if (aItem <> nil)
      then c := aItem.parse(aCode, c+1);
    // skipping to the next pair
    if (aCode[c] = ',') then
    begin
      ct := c;
      Inc(c); 
    end;
  end;
  Result := c+1;
end;

function TMcJsonItem.readArray(const aCode: string; aPos: Integer): Integer;
var
  c, len: Integer;
  aItem: TMcJsonItem;
begin
  // we get here because current symbol was '['
  c   := aPos+1;
  len := Length(aCode);
  // set item type
  Self.fSetType(jitArray);
  // reading values until we reach a ']'
  while (aCode[c] <> ']') and (c <= len) do
  begin
    // valid-JSON
    if (aCode[c] = '[') then Error(SParsingError);
    // Creating a new value (here explicity whith no key)
    aItem := Self.Add();
    // parsing values (recursive)
    c := aItem.parse(aCode, c); // 1,2,3
    if (aCode[c] = ',')
      then Inc(c);
  end;
  Result := c+1;
end;

function TMcJsonItem.readNumber(const aCode: string; aPos: Integer): Integer;
var
  c: Integer;
begin
  // we get here because current symbol was '+/-' or Digit
  c := aPos+1;
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
  // valid-JSON
  if    (aCode[c] <> ','   ) and
    not (aCode[c] in CLOSES) then
    Error(SParsingError);
  // Result
  Self.fSetType(jitValue);
  Self.fValType := jvtNumber;
  Self.fValue   := System.Copy(aCode, aPos, c-aPos);
  Result := c;
end;

function TMcJsonItem.readBoolean(const aCode: string; aPos: Integer): Integer;
var
  len: Integer;
  sAux: string;
begin
  // we get here because current symbol was 't/T' or 'f/F'
  len := 0;
  // check boolean value 'true'
  if (aCode[aPos] = 't') or
     (aCode[aPos] = 'T') then
  begin
    len  := 4;
    sAux := System.Copy(aCode, aPos, len);
    if LowerCase(sAux) = 'true' then
      Self.fValue := 'true'
    else
      // valid-JSON
      Error(SParsingError);
  end
  // check boolean value 'false'
  else if (aCode[aPos] = 'f') or
          (aCode[aPos] = 'F') then
  begin
    len  := 5;
    sAux := System.Copy(aCode, aPos, len);
    if LowerCase(sAux) = 'false' then
      Self.fValue := 'false'
    else
      // valid-JSON
      Error(SParsingError);
  end;
  // set item and value types
  if (len > 0) then
  begin
    Self.fSetType(jitValue);
    Self.fValType := jvtBoolean;
  end;
  Result := aPos+len;
end;

function TMcJsonItem.readNull(const aCode: string; aPos: Integer): Integer;
var
  len: Integer;
  sAux: string;
begin
  // we get here because current symbol was 'n/N'
  len := 0;
  // check if null
  if (aCode[aPos] = 'n') or
     (aCode[aPos] = 'N') then
  begin
    len  := 4;
    sAux := System.Copy(aCode, aPos, len);
    if LowerCase(sAux) = 'null' then
      Self.fValue := 'null'
    else
      // valid-JSON
      Error(SParsingError);
  end;
  // set item and value types
  if (len > 0) then
  begin
    Self.fSetType(jitValue);
    Self.fValType := jvtNull;
  end;
  Result := aPos+len;
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
  if (Self = nil) then Error(SItemNil);
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
  if  (Self = nil) then Error(SItemNil);
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
  if (Self = nil) then Error(SItemNil);
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
  if (Self = nil) then Error(SItemNil);
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
  if (Self = nil) then Error(SItemNil);
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
  if (Self = nil) then Error(SItemNil);
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
  if (Self = nil) then Error(SItemNil);
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
  if (Self = nil) then Error(SItemNil);
  // find index of item with aKey
  idx := Self.IndexOf(aKey);
  if (idx >= 0) then
    Ans := Self.Delete(idx);
  Result := Ans;
end;

function TMcJsonItem.HasKey(const aKey: string): Boolean;
begin
  if (Self = nil) then Error(SItemNil);
  Result := ( fGetItemByKey(aKey) <> nil );
end;

function TMcJsonItem.IsEqual(const aItem: TMcJsonItem): Boolean;
begin
  if (Self  = nil) then Error(SItemNil);
  if (aItem = nil) then Error(SItemNil);
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
  except
    Result := False;
  end;
  aItem.Free;
end;

function TMcJsonItem.ToString: string;
begin
  Result := ToString(False, '');
end;

function TMcJsonItem.ToString(aHuman: Boolean; const aIndent: string): string;

  function EnumItems: string;
  var
    i, len: Integer;
    aNewInd: string;
    sNL: string;
  begin
    if (Self = nil) or (Self.Count = 0) then
    begin
      Result := '';
      Exit;
    end;
    // new line
    if aHuman
      then sNL := #13#10
      else sNL := '';
    // indentation (used in recursive loop)
    if aHuman
      then aNewInd := aIndent + '  '
      else aNewInd := '';
    // init
    Result := sNL;
    len    := Self.Count - 1;
    // mount recursively
    for i := 0 to len do
    begin
      Result := Result + TMcJsonItem(fChild.Items[i]).ToString(aHuman, aNewInd);
      if (i < len)
        then Result := Result + ',' + sNL
        else Result := Result + sNL + aIndent;
    end;
  end;

  function QuoteValue(const aValue: string): string;
  begin
    Result := Self.fValue;
    if (Self.fValType = jvtString) then
      Result := '"' + Self.fValue + '"';
  end;

var
  sPrefix: string;
  sSp: string;
begin
  Result := '';
  if (Self =  nil) then Error(SItemNil);
  // key value separator
  if (aHuman)
    then sSp := ': '
    else sSp := ':';
  // key as a prefix 
  if (Self <> nil) and (fKey <> '')
    then sPrefix := '"' + fKey + '"' + sSp
    else sPrefix := '';
  // mount
  case fType of
    jitObject: Result := aIndent + sPrefix + '{' + EnumItems + '}';
    jitArray : Result := aIndent + sPrefix + '[' + EnumItems + ']';
    else if (fValue <> '') then
      Result := aIndent + sPrefix + QuoteValue(fValue);
  end;
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
    Write(aFile, ToString(aHuman, ''));
    if (IOResult <> 0) then
      raise EMcJsonException.CreateFmt('JSON: Failed to save %s', [aFileName]);
  finally
    CloseFile(aFile);
  end;
end;

function TMcJsonItem.GetTypeStr: string;
begin
  if (Self = nil) then Error(SItemNil);
  Result := GetItemTypeStr(Self.fType);
end;

function TMcJsonItem.GetValueStr: string;
begin
  if (Self = nil) then Error(SItemNil);
  Result := GetValueTypeStr(Self.fValType);
end;

procedure TMcJsonItem.Error(const Msg: string);
begin
  if (Msg = '') then
    raise EMcJsonException.Create(SParsingError)
  else
    raise EMcJsonException.Create(Msg);
end;

end.
