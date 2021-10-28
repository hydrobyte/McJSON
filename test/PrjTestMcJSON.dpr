program PrjTestMcJSON;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  McJSON in '..\McJSON.pas';

type
  TTest = function(out Msg: string): Boolean;

var
  s: string;  

procedure Check(Test: TTest; var Passed, Failed: Integer);
var
  S: string;
begin
  if Test(S) then
  begin
    Inc(Passed);
    WriteLn('[PASS] ', S);
  end
  else
  begin
    Inc(Failed);
    WriteLn('[FAIL] ', S);
  end;
end;

function Test1(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test: parse simple object';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := '{ "key": "value" }';
    s:= N.AsJSON;
    Result   :=   (N.ItemType               = jitObject)
              and (N.Count                  = 1        )
              and (N.Key                    = ''       )
              and (N['key'].Key             = 'key'    )
              and (N['key'].AsString        = 'value'  )
              and (N.Keys[0]                = 'key'    )
              and (N.Values[0].AsString     = 'value'  )
              and (N.Items['key'].AsString  = 'value'  );
  except
    Result := False;
  end;
  N.Free;
end;

function Test2(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test: parse simple array';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := '{ "array": [1, 2.0, "3"] }';
    s:= N.AsJSON;
    Result   :=   (N.ItemType                     = jitObject)
              and (N.Count                        = 1        )
              and (N['array'].ItemType            = jitArray )
              and (N['array'].Count               = 3        )
              and (N['array'].Key                 = 'array'  )
              and (N['array'].Values[1].AsString  = '2.0'    )
              and (N['array'].Values[2].AsInteger = 3        );
  except
    Result := False;
  end;
  N.Free;
end;

function Test3(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test: parse simple sub object array';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := '{ "sub": [{"key1": 1}, {"key2": 2}] }';
    Result   :=   (N.ItemType                           = jitObject)
              and (N.Count                              = 1        )
              and (N['sub'].ItemType                    = jitArray )
              and (N['sub'].Count                       = 2        )
              and (N['sub'].Key                         = 'sub'    )
              and (N['sub'].Values[1]['key2'].Key       = 'key2'   )
              and (N['sub'].Values[1]['key2'].AsInteger = 2        );
  except
    Result := False;
  end;
  N.Free;
end;

function Test4(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test: simple object value change';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := '{ "key": "value" }';
    N['key'].AsString := 'new value';
    Result := (N['key'].AsString = 'new value');
  except
    Result := False;
  end;
  N.Free;
end;

function Test5(out Msg: string): Boolean;
var
  N, M, P, Q: TMcJsonItem;
begin
  Msg := 'Test: Add, Remove functions';
  N := TMcJsonItem.Create;
  M := TMcJsonItem.Create;
  P := TMcJsonItem.Create;
  Q := TMcJsonItem.Create;
  try
    // add sub item.
    N.AsJSON := '{ "sub": [{"key1": 1}, {"key2": 2}] }';
    M.Add('key3').AsInteger := 3;
    N['sub'].Add(M);
    Result :=     (N.ItemType                           = jitObject)
              and (N.Count                              = 1        )
              and (N['sub'].ItemType                    = jitArray )
              and (N['sub'].Count                       = 3        )
              and (N['sub'].Key                         = 'sub'    )
              and (N['sub'].Values[2]['key3'].Key       = 'key3'   )
              and (N['sub'].Values[2]['key3'].AsInteger = 3        );
    // add nested object
    N.Clear;
    N.Add('k1').Add('k2').Add('k3').AsString := 'v3';
    Result := Result and (N.AsJSON = '{"k1": {"k2": {"k3": "v3"}}}');
    // add int values into array
    P.Add('a').ItemType := jitArray;
    P['a'].Add.AsInteger := 1;
    P['a'].Add.AsInteger := 2;
    P['a'].Add.AsInteger := 3;
    Result := Result and (P.AsJSON = '{"a": [1,2,3]}');
    // add obj values into array
    P.Clear;
    P.Add('a' ).ItemType  := jitArray;
    Q.Add('k1').AsInteger := 1;
    P['a'].Add.AsObject := Q;
    P['a'].Add.AsObject := Q;
    Result := Result and (P.AsJSON = '{"a": [{"k1": 1},{"k1": 1}]}')
                     and (P['a'].Count = 2);
    // remove item by index
    P['a'].Delete(1);
    Result := Result and (P.AsJSON = '{"a": [{"k1": 1}]}')
                     and (P['a'].Count = 1);
    // remove item by key
    P.Delete('a');
    Result := Result and (P.AsJSON = '{}')
                     and (P.Count  = 0   );
    // remove empty item
    P.Delete(0);
    Result := Result and (P.AsJSON = '{}')
                     and (P.Count  = 0   );
  except
    Result := False;
  end;
  N.Free;
  //M.Free // no! free inside N
  P.Free;
end;

function Test6(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test: out of index';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := '{ "array": [1, "2", 3] }';
    N['array'].Values[3].AsInteger := 4;
    //N['array'].Items[0].SetInt(4);        // will not compile in Delphi
    Result := False;
  except
    Result := True;
  end;
  N.Free;
end;

function Test7(out Msg: string): Boolean;
var
  N, M: TMcJsonItem;
  Aux: Boolean;
begin
  Msg := 'Test: getters and setters';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := '{ "i": 123, "f": 123.456, "s": "abc", "b": True, "n": Null }';
    // changes
    N['i'].AsInteger := 321;
    N['f'].AsNumber  := 456.123;
    N['s'].AsString  := 'cba';
    N['b'].AsBoolean := False;
    // get reference as object.
    M := N.AsObject;
    // aux text
    Aux := abs(N['f'].AsNumber - 456.123) < 0.001;
    // check result
    Result :=     (N['i'].AsInteger = 321  )
              and (Aux                     )
              and (N['s'].AsString  = 'cba')
              and (N['b'].AsBoolean = False)
              and (N['n'].IsNull           )
              and (M.Count          = 5    )
              and (M['i'].AsInteger = 321  )
              and (M['s'].AsString  = 'cba')
              and (M['b'].AsBoolean = False)
              and (M['n'].IsNull           );
  except
    Result := False;
  end;
  N.Free;
end;

function Test8(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test: numbers: scientific notation';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := '{ "n": -1.23456789E-10 }';
    Result   :=   (N['n'].AsString = '-1.23456789E-10')
              and (N['n'].AsNumber >  -1.23456789E-00 );
  except
    Result := False;
  end;
  N.Free;
end;

function Test9(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test: escapes';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := '{ "k": "\b\t\n\f\r\u\"\\" }';
    Result   := (N['k'].AsString = '\b\t\n\f\r\u\"\\');
  except
    Result := False;
  end;
  N.Free;
end;

function Test10(out Msg: string): Boolean;
var
  N: TMcJsonItem;
  IsValid: Boolean;
begin
  Msg := 'Test: invalid JSON';
  N := TMcJsonItem.Create;
  IsValid := False;
  try
    // value bad formats
    IsValid := IsValid or N.Check( '{ "k": value" }' );
    IsValid := IsValid or N.Check( '{ "k": "value }' );
    IsValid := IsValid or N.Check( '{ "k": 12345a }' );
    IsValid := IsValid or N.Check( '{ "k": 12"45a }' );
    // values not recognized
    IsValid := IsValid or N.Check( '{ "k": truee }' );
    IsValid := IsValid or N.Check( '{ "k": falsi }' );
    IsValid := IsValid or N.Check( '{ "k": nil   }' );
    // key bad formats
    IsValid := IsValid or N.Check( '{ "k: "value" }' );
    IsValid := IsValid or N.Check( '{ k": "value" }' );
    IsValid := IsValid or N.Check( '{ "a": 1, "b": 2, "a": 3 }' );
    // object bad formats
    IsValid := IsValid or N.Check( '{ "k": {{"key":"value"}} }' );
    IsValid := IsValid or N.Check( '{ "k":  {"key":"value"]  }' );
    // array bad format
    IsValid := IsValid or N.Check( '{ "k": [["1","2"]]       }' );
    IsValid := IsValid or N.Check( '{ "k":  ["key":"value"]  }' );
    IsValid := IsValid or N.Check( '{ "k":  ["1","2"      }  }' );
    // json inside a json
    // ... not permited
    IsValid := IsValid or     N.Check( '{ "j": "{"key": "value"}"}' );
    // ... permited (escaped)
    IsValid := IsValid or not N.Check( '{ "j": "{\"key\": \"value\"}"}' );
    // if any is valid, test fails
    Result := not IsValid;
  except
    Result := False;
  end;
  N.Free;
end;

function Test11(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test: type transformations';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := '{ "k1": ["1", "2"], "k2": {"1": "a", "2": "b"} }';
    N['k1'].ItemType := jitObject;
    N['k2'].ItemType := jitArray ;
    Result :=   (N['k1'].ItemType           = jitObject)
            and (N['k1']['0'].AsString      = '1'      )
            and (N['k1']['1'].AsString      = '2'      )
            and (N['k2'].ItemType           = jitArray )
            and (N['k2'].Values[0].AsString = 'a'      )
            and (N['k2'].Values[1].AsString = 'b'      );
  except
    Result := False;
  end;
  N.Free;
end;

function Test12(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test: load and save file';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := '{ "i": 123, "f": 123.456, "s": "abc", "b": True, "n": Null, "a": [1,2], "o": {"k": "v"} }';
    N.SaveToFile('.\test.json', True);
    N.LoadFromFile('.\test.json');
    // check result
    Result :=     (N['i'].AsInteger           = 123  )
              and (N['s'].AsString            = 'abc')
              and (N['b'].AsBoolean           = True )
              and (N['n'].IsNull                     )
              and (N['a'].Values[0].AsInteger = 1    )
              and (N['o']['k'].AsString       = 'v'  );
  except
    Result := False;
  end;
  N.Free;
end;

function Test13(out Msg: string): Boolean;
var
  N, M, P: TMcJsonItem;
begin
  Msg := 'Test: constructors';
  N := nil;
  M := nil;
  P := nil;
  try
    Result := True;
    // constructor empty
    N := TMcJsonItem.Create();
    Result := Result and (N.AsJSON = '');
    // constructor by code
    M := TMcJsonItem.Create('{"i": 123}');
    Result := Result and (M.AsJSON = '{"i": 123}');
    // constructor copy
    P := TMcJsonItem.Create(M);
    Result := Result and (M.AsJSON = '{"i": 123}')
                     and (P.AsJSON = '{"i": 123}');
  except
    Result := False;
  end;
  N.Free;
  M.Free;
  P.Free;
end;

function Test14(out Msg: string): Boolean;
var
  N, M, P, Q: TMcJsonItem;
begin
  Msg := 'Test: Copy, Clone, IsEqual, Remove functions';
  N := TMcJsonItem.Create();
  M := TMcJsonItem.Create();
  P := nil;
  Q := nil;
  try
    Result := True;
    // constructor empty
    N.AsJSON := '{"i": 123}';
    M.Copy(N);
    M.Add('k').AsString := 'v';
    P := M.Clone;
    P.Delete(0);
    Q := P.Clone;
    Result := Result and (N.AsJSON = '{"i": 123}'         )
                     and (M.AsJSON = '{"i": 123,"k": "v"}')
                     and (P.AsJSON = '{"k": "v"}'         )
                     and (Q.IsEqual(P)                    );
  except
    Result := False;
  end;
  N.Free;
  M.Free;
  P.Free;
  Q.Free;
end;

function Test99(out Msg: string): Boolean;
var
  Json: TMcJsonItem;
  i: Integer;
begin
  Msg := 'Test: Github readme.md content';
  Json := TMcJsonItem.Create();
  try
    try
      // add some pairs.
      Json.Add('key1').AsInteger := 1;
      Json.Add('key2').AsBoolean := True;
      Json.Add('key3').AsNumber  := 1.234;
      Json.Add('key4').AsString  := 'value 1';
      // add an array
      Json.Add('array').ItemType := jitArray;
      for i := 1 to 3 do
        Json['array'].Add.AsInteger := i;
      // save and load
      Json.SaveToFile('example.json');
      Json.LoadFromFile('example.json');
      // remove an item
      Json.Delete('array');
      // test final result
      Result := (Json.AsJSON = '{"key1": 1,"key2": true,"key3": 1.234,"key4": "value 1"}');
    except
      Result := False;
    end;
  finally
    Json.Free;
  end;
end;

procedure RunTests;
var
  TotalPassed, TotalFailed: Integer;
begin
  TotalPassed := 0;
  TotalFailed := 0;

  Check(Test1 , TotalPassed, TotalFailed);
  Check(Test2 , TotalPassed, TotalFailed);
  Check(Test3 , TotalPassed, TotalFailed);
  Check(Test4 , TotalPassed, TotalFailed);
  Check(Test5 , TotalPassed, TotalFailed);
  Check(Test6 , TotalPassed, TotalFailed);
  Check(Test7 , TotalPassed, TotalFailed);
  Check(Test8 , TotalPassed, TotalFailed);
  Check(Test9 , TotalPassed, TotalFailed);
  Check(Test10, TotalPassed, TotalFailed);
  Check(Test11, TotalPassed, TotalFailed);
  Check(Test12, TotalPassed, TotalFailed);
  Check(Test13, TotalPassed, TotalFailed);
  Check(Test14, TotalPassed, TotalFailed);
  Check(Test99, TotalPassed, TotalFailed);

  WriteLn;
  if TotalFailed > 0 then
    WriteLn(TotalFailed, ' tests FAILED')
  else
    WriteLn('All tests PASSED');
end;

begin
  RunTests;
  Readln;
end.