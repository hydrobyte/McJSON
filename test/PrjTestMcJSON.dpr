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

function Test01(out Msg: string): Boolean;
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
    on E: Exception do
    begin
      Msg := Msg + #13#10 + '       Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
end;

function Test02(out Msg: string): Boolean;
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
    on E: Exception do
    begin
      Msg := Msg + #13#10 + '       Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
end;

function Test03(out Msg: string): Boolean;
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
    on E: Exception do
    begin
      Msg := Msg + #13#10 + '       Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
end;

function Test04(out Msg: string): Boolean;
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
    on E: Exception do
    begin
      Msg := Msg + #13#10 + '       Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
end;

function Test05(out Msg: string): Boolean;
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
    Result := Result and (N.AsJSON = '{"k1":{"k2":{"k3":"v3"}}}');
    // add int values into array
    P.Add('a').ItemType := jitArray;
    P['a'].Add.AsInteger := 1;
    P['a'].Add.AsInteger := 2;
    P['a'].Add.AsInteger := 3;
    Result := Result and (P.AsJSON = '{"a":[1,2,3]}');
    // add obj values into array
    P.Clear;
    P.Add('a' ).ItemType  := jitArray;
    Q.Add('k1').AsInteger := 1;
    P['a'].Add.AsObject := Q;
    P['a'].Add.AsObject := Q;
    Result := Result and (P.AsJSON = '{"a":[{"k1":1},{"k1":1}]}')
                     and (P['a'].Count = 2);
    // remove item by index
    P['a'].Delete(1);
    Result := Result and (P.AsJSON = '{"a":[{"k1":1}]}')
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
    on E: Exception do
    begin
      Msg := Msg + #13#10 + '       Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
  //M.Free // no! free inside N
  P.Free;
end;

function Test06(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test: object is nil';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := '{ "array": [1, "2", 3] }';
    //N['not'].Values[3].AsInteger := 4;
    N['array'].Values[3].AsInteger := 4;
    //N['array'].Items[0].SetInt(4);        // will not compile in Delphi
    Result := False;
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + '       Error: ' + E.Message;
      Result := True;
    end;
  end;
  N.Free;
end;

function Test07(out Msg: string): Boolean;
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
    on E: Exception do
    begin
      Msg := Msg + #13#10 + '       Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
end;

function Test08(out Msg: string): Boolean;
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
    on E: Exception do
    begin
      Msg := Msg + #13#10 + '       Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
end;

function Test09(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test: escapes';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := '{ "k": "\b\t\n\f\r\u\"\\" }';
    Result   := (N['k'].AsString = '\b\t\n\f\r\u\"\\');
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + '       Error: ' + E.Message;
      Result := False;
    end;
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
    on E: Exception do
    begin
      Msg := Msg + #13#10 + '       Error: ' + E.Message;
      Result := False;
    end;
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
    Result := True;
    // object and arrays
    N.AsJSON := '{ "k1": ["1", "2"], "k2": {"1": "a", "2": "b"} }';
    N['k1'].ItemType := jitObject;
    N['k2'].ItemType := jitArray ;
    Result := Result and (N['k1'].ItemType           = jitObject)
                     and (N['k1']['0'].AsString      = '1'      )
                     and (N['k1']['1'].AsString      = '2'      )
                     and (N['k2'].ItemType           = jitArray )
                     and (N['k2'].Values[0].AsString = 'a'      )
                     and (N['k2'].Values[1].AsString = 'b'      );
    // array and value setters
    N.AsJSON := '{ "a": ["1", "2"]}';
    N['a'].AsInteger := 1;
    Result := Result and (N.AsJSON = '{"a":[1,1]}');
    N['a'].AsNumber := 1.1;
    Result := Result and (N.AsJSON = '{"a":[1.1,1.1]}');
    N['a'].AsString := 'str';
    Result := Result and (N.AsJSON = '{"a":["str","str"]}');
    N['a'].AsBoolean := True;
    Result := Result and (N.AsJSON = '{"a":[true,true]}');
    N['a'].AsNull := 'null';
    Result := Result and (N.AsJSON = '{"a":[null,null]}');
    // object and value setters
    N.AsJSON := '{ "o": {"k1":"v1", "k2":"v2"}}';
    N['o'].AsInteger := 1;
    Result := Result and (N.AsJSON = '{"o":{"k1":1,"k2":1}}');
    N['o'].AsNumber := 1.1;
    Result := Result and (N.AsJSON = '{"o":{"k1":1.1,"k2":1.1}}');
    N['o'].AsString := 'str';
    Result := Result and (N.AsJSON = '{"o":{"k1":"str","k2":"str"}}');
    N['o'].AsBoolean := True;
    Result := Result and (N.AsJSON = '{"o":{"k1":true,"k2":true}}');
    N['o'].AsNull := 'null';
    Result := Result and (N.AsJSON = '{"o":{"k1":null,"k2":null}}');
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + '       Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
end;

function Test12(out Msg: string): Boolean;
var
  N, M: TMcJsonItem;
  i, idx: Integer;
begin
  Msg := 'Test: Save and Load using files';
  N := TMcJsonItem.Create();
  M := TMcJsonItem.Create();
  try
    Result := True;
    // create a simple object.
    N.AsJSON := '{"i": 123}';
    // now add a array of objects
    N.Add('array').ItemType := jitArray;
    for i := 1 to 2 do
      N['array'].Add.AsJSON := '{"k'+IntToStr(i)+'": "v'+IntToStr(i)+'"}';
    // save to file (not Human readable)
    N.SaveToFile('test12.json', false);
    // change N using IndexOf
    idx := N.IndexOf('array');
    if (idx >= 0) then
      N.Delete(idx);
    // load from file
    M.LoadFromFile('test12.json');
    // check before and after delete
    Result := Result and (N.AsJSON = '{"i":123}'                                  )
                     and (M.AsJSON = '{"i":123,"array":[{"k1":"v1"},{"k2":"v2"}]}');
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + '       Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
  M.Free;
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
    Result := Result and (M.AsJSON = '{"i":123}');
    // constructor copy
    P := TMcJsonItem.Create(M);
    Result := Result and (M.AsJSON = '{"i":123}')
                     and (P.AsJSON = '{"i":123}');
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + '       Error: ' + E.Message;
      Result := False;
    end;
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
    Result := Result and (N.AsJSON = '{"i":123}'        )
                     and (M.AsJSON = '{"i":123,"k":"v"}')
                     and (P.AsJSON = '{"k":"v"}'        )
                     and (Q.IsEqual(P)                  );
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + '       Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
  M.Free;
  P.Free;
  Q.Free;
end;

function Test15(out Msg: string): Boolean;
var
  N: TMcJsonItem;
  i: Integer;
  anyPass: Boolean;
begin
  Msg := 'Test: exceptions';
  N := TMcJsonItem.Create();
  Result  := True;
  anyPass := False;
  for i := 1 to 7 do
  begin
    try
      N.AsJSON := '{"s": "123a"}';
      // Exception Object reference is nil
      if (i = 1) then
      begin
        N['not'].AsInteger;
        anyPass := True;
      end
      else if (i = 2) then
      begin
        N['s'].Values[1].AsInteger;
        anyPass := True;
      end
      // Exception Invalid item type
      else if (i = 3) then
      begin
        N['s'].AsObject;
        anyPass := True;
      end
      // Exception Can't convert item "%s" with value "%s" to "%s"
      else if (i = 4) then
      begin
        N['s'].AsInteger;
        anyPass := True;
      end
      // Exception Can't convert item "%s" to "%s"
      else if (i = 5) then
      begin
        N.AsJSON := '{"n": null}';
        N['n'].AsInteger;
        anyPass := True;
      end
      // Exception Duplicate key "%s"
      else if (i = 6) then
      begin
        N.SpeedUp := False;
        N.AsJSON := '{"k":"v", "k":"v"}';
        anyPass := True;
      end
      // Exception Error while parsing text: read "%s" at pos "%s"
      else if (i = 7) then
      begin
        N.AsJSON := '{"n"[:null}';
        anyPass := True;
      end;
    except
      on E: Exception do
      begin
        Msg := Msg + #13#10 + '       Error: ' + E.Message;
        Result := Result and anyPass;
      end;
    end;
  end;
  N.Free;
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
      // save a backup to file
      if (Json['array'].Count = 3) then
        Json.SaveToFile('example.json');
      // remove an item
      Json.Delete('array');
      // oops, load the backup
      if (Json.Count = 4) then
        Json.LoadFromFile('example.json');
      // test final result
      Result := (Json.AsJSON = '{"key1":1,"key2":true,"key3":1.234,"key4":"value 1","array":[1,2,3]}');
    except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + '       Error: ' + E.Message;
      Result := False;
    end;
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

  Check(Test01, TotalPassed, TotalFailed);
  Check(Test02, TotalPassed, TotalFailed);
  Check(Test03, TotalPassed, TotalFailed);
  Check(Test04, TotalPassed, TotalFailed);
  Check(Test05, TotalPassed, TotalFailed);
  Check(Test06, TotalPassed, TotalFailed);
  Check(Test07, TotalPassed, TotalFailed);
  Check(Test08, TotalPassed, TotalFailed);
  Check(Test09, TotalPassed, TotalFailed);
  Check(Test10, TotalPassed, TotalFailed);
  Check(Test11, TotalPassed, TotalFailed);
  Check(Test12, TotalPassed, TotalFailed);
  Check(Test13, TotalPassed, TotalFailed);
  Check(Test14, TotalPassed, TotalFailed);
  Check(Test15, TotalPassed, TotalFailed);
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
