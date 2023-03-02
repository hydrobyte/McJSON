program PrjTestMcJSON;

{$APPTYPE CONSOLE}

uses
  Classes,
  SysUtils,
  McJSON in '..\McJSON.pas';

type
  TTest = function(out Msg: string): Boolean;

var
  sIndent: string;

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
  Msg := 'Test 01: parse simple object';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := ' { "key" : "value" } ';
    Result   :=   (N.ItemType               = jitObject)
              and (N.Count                  = 1        )
              and (N.Key                    = ''       )
              and (N['key'].Key             = 'key'    )
              and (N.HasKey('key')          = True     )
              and (N.HasKey('not')          = False    )
              and (N.HasChild               = True     )
              and (N['key'].AsString        = 'value'  )
              and (N.Keys[0]                = 'key'    )
              and (N.Items[0].AsString      = 'value'  )
              and (N.Values['key'].AsString = 'value'  );
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
end;

function Test02(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test 02: parse simple array';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := '{ "array": [1, 2.0, "3"] }';
    Result   :=   (N.Count                        = 1        )
              and (N.ItemType                     = jitObject)
              and (N['array'].ItemType            = jitArray )
              and (N['array'].Items[0].ItemType   = jitValue )
              and (N['array'].Count               = 3        )
              and (N['array'].Key                 = 'array'  )
              and (N['array'].Items[1].AsString   = '2.0'    )
              and (N['array'].Items[2].AsInteger  = 3        );
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
end;

function Test03(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test 03: parse simple sub object array';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := '{ "sub": [{"key1": 1}, {"key2": 2}] }';
    Result   :=   (N.ItemType                           = jitObject)
              and (N.Count                              = 1        )
              and (N['sub'].ItemType                    = jitArray )
              and (N['sub'].Count                       = 2        )
              and (N.HasKey('sub')                      = True     )
              and (N.HasKey('not')                      = False    )
              and (N['sub'].Key                         = 'sub'    )
              and (N['sub'].Items[1]['key2'].Key        = 'key2'   )
              and (N['sub'].Items[1]['key2'].AsInteger  = 2        );
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
end;

function Test04(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test 04: simple object value change';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := '{ "key": "value" }';
    N['key'].AsString := 'new value';
    Result := (N['key'].AsString = 'new value');
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
end;

function Test05(out Msg: string): Boolean;
var
  N, M, P, Q: TMcJsonItem;
begin
  Msg := 'Test 05: Add, Insert, Delete functions';
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
              and (N['sub'].Items[2]['key3'].Key        = 'key3'   )
              and (N['sub'].Items[2]['key3'].AsInteger  = 3        );
    // add nested object
    N.Clear;
    N.Add('k1').Add('k2').Add('k3').AsString := 'v3';
    Result := Result and (N.AsJSON = '{"k1":{"k2":{"k3":"v3"}}}');
    // add int values into array
    P.Add('a', jitArray);
    P['a'].Add.AsInteger := 1;
    P['a'].Add.AsInteger := 2;
    P['a'].Add.AsInteger := 3;
    Result := Result and (P.AsJSON = '{"a":[1,2,3]}');
    // add item
    Q.AsJSON := '{"x":0}';
    P['a'].Add(Q);
    Result := Result and (P.AsJSON = '{"a":[1,2,3,{"x":0}]}');
    // add obj values into array
    P.Clear;
    Q.Clear;
    P.Add('a', jitArray);
    Q.Add('k1').AsInteger := 1;
    P['a'].Add.AsObject := Q;
    P['a'].Add.AsObject := Q;
    Result := Result and (P.AsJSON = '{"a":[{"k1":1},{"k1":1}]}')
                     and (P['a'].Count = 2);
    // delete item by index
    P['a'].Delete(1);
    Result := Result and (P.AsJSON = '{"a":[{"k1":1}]}')
                     and (P['a'].Count = 1);
    // remove item by key
    P.delete('a');
    Result := Result and (P.AsJSON = '{}')
                     and (P.Count  = 0   );
    // remove empty item
    P.delete(0);
    Result := Result and (P.AsJSON = '{}')
                     and (P.Count  = 0   );
    // insert item by key
    P.Insert('c', 0).AsInteger := 3;
    P.Insert('b', 0).AsInteger := 2;
    P.Insert('a', 0).AsInteger := 1;
    Result := Result and (P.AsJSON = '{"a":1,"b":2,"c":3}')
                     and (P.Count  = 3 );
    // insert item
    Q.AsJSON := '{"x":0}';
    P.ItemType := jitArray;
    P.Insert(Q, 1);
    Result := Result and (P.AsJSON = '[1,{"x":0},2,3]')
                     and (P.Count  = 4 );
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
  M.Free; // yes! Add cloned M inside N
  P.Free;
  Q.Free; // yes! Add/Insert cloned Q inside P.
end;

function Test06(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test 06: object is nil';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := '{ "array": [1, "2", 3] }';
    //N['not'].Items[3].AsInteger := 4;
    N['array'].Items[3].AsInteger := 4;
    //N['array'].Values[0].SetInt(4);        // will not compile in Delphi
    Result := False;
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
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
  Msg := 'Test 07: getters and setters';
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
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
end;

function Test08(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test 08: numbers: scientific notation';
  N := TMcJsonItem.Create;
  try
    N.AsJSON := '{ "n": -1.23456789E-10 }';
    Result   :=   (N['n'].AsString = '-1.23456789E-10')
              and (N['n'].AsNumber >  -1.23456789E-00 );
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
end;

function Test09(out Msg: string): Boolean;
var
  N: TMcJsonItem;
  S: string;
begin
  Msg := 'Test 09: escapes';
  N := TMcJsonItem.Create;
  Result := True;
  try
    N.AsJSON := '{ "k": "\b\t\n\f\r\u05d1 \" \\ \/"}';
    Result := Result and (N['k'].AsString = '\b\t\n\f\r\u05d1 \" \\ \/');
    // unescape function
    S := UnEscapeUnicode('aB\t\n\u00e7d\u00e7'); // debug sees 'ç'
    Result := Result and (S = 'aBçdç');
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
end;

function Test10(out Msg: string): Boolean;
var
  StrL: TStringList;
  i: Integer;
  N: TMcJsonItem;
  anyPass: Boolean;
  sName, sTest: string;
begin
  Msg := 'Test 10: invalid JSON';
  StrL := TStringList.Create;
  N := TMcJsonItem.Create;
  anyPass := False;
  try
    // value bad formats
    StrL.Add('bad value: not open'       +'='+ '{"k":value"}'         );
    StrL.Add('bad value: not close'      +'='+ '{"k":"value}'         );
    StrL.Add('bad value: not number 1'   +'='+ '{"k":12345a}'         );
    StrL.Add('bad value: not number 2'   +'='+ '{"k":+-1234}'         );
    StrL.Add('bad value: not number 3'   +'='+ '{"k":1234E}'          );
    StrL.Add('bad value: not number 4'   +'='+ '{"k":1234E+-1}'       );
    StrL.Add('bad value: not number 5'   +'='+ '{"k":1234E+a}'        );
    StrL.Add('bad value: not number 6'   +'='+ '{"k":1,234}'          );
    StrL.Add('bad value: leading 0s 1'   +'='+ '{"k":01234}'          );
    StrL.Add('bad value: leading 0s 2'   +'='+ '{"k":00004}'          );
    StrL.Add('bad value: invalid'        +'='+ '{"k":"v"a}'           );
    StrL.Add('bad value: line break'     +'='+ '{"k":"v'+#13+'"}'     );
    // values not recognized
    StrL.Add('bad value: not keyword'    +'='+ '{"k":truee}'          );
    StrL.Add('bad value: not keyword'    +'='+ '{"k":falsi}'          );
    StrL.Add('bad value: not keyword'    +'='+ '{"k":nil  }'          );
    // key bad formats
    StrL.Add('bad key: no key'           +'='+ '{"value"}'            );
    StrL.Add('bad key: not closed 1'     +'='+ '{"k:"value"}'         );
    StrL.Add('bad key: not closed 2'     +'='+ '{"key:"value"}'       );
    StrL.Add('bad key: not opened'       +'='+ '{k":"value"}'         );
    StrL.Add('bad key: duplicated'       +'='+ '{"k":1,"a":2,"a":3}'  );
    // object bad formats
    StrL.Add('bad object: not closed 1'  +'='+ '{'                    );
    StrL.Add('bad object: not closed 2'  +'='+ '{"k":"value"'         );
    StrL.Add('bad object: bi closed 1'   +'='+ '{"k":"value"}}'       );
    StrL.Add('bad object: bi closed 2'   +'='+ '{"k":[{"k":"v"}}]}'   );
    StrL.Add('bad object: wrong close'   +'='+ '{"k":{"key":"value"]}');
    // array bad formats
    StrL.Add('bad array: not closed 1'   +'='+ '["1","2"'             );
    StrL.Add('bad array: not closed 2'   +'='+ '{"k":["1","2"}'       );
    StrL.Add('bad array: bi closed 1'    +'='+ '{"k":["1","2"]]'      );
    StrL.Add('bad array: wrong item'     +'='+ '{"k":["key":"value"]}');
    StrL.Add('bad array: wrong close'    +'='+ '{"k":["1","2"}}'      );
    // json inside a json
    StrL.Add('bad value: json'           +'='+ '{"k":"{"key":"value"}"}');
    // unknown escape
    StrL.Add('bad value: unknown escape' +'='+ '{"k":"aa \x aa"}');
    StrL.Add('bad value: bad u escape 1' +'='+ '{"k":"\u"}'      );
    StrL.Add('bad value: bad u escape 2' +'='+ '{"k":"\u000"}'   );
    StrL.Add('bad value: bad u escape 3' +'='+ '{"k":"\u0FaX"}'  );
    // check
    for i:=0 to StrL.Count-1 do
    begin
      sName   := Trim( StrL.Names[i]      );
      sTest   := Trim( StrL.Values[sName] );
      if ( N.Check(sTest) ) then
      begin
        Msg := Msg + #13#10 + sIndent + 'Expected to fail but pass: ' + sName;
        anyPass := True;
      end;
    end;
    // if any passed, report fail
    Result := not anyPass;
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
      Result := False;
    end;
  end;
  StrL.Free;
  N.Free;
end;

function Test11(out Msg: string): Boolean;
var
  StrL: TStringList;
  i: Integer;
  N: TMcJsonItem;
  anyFail: Boolean;
  sName, sTest: string;
begin
  Msg := 'Test 11: valid or unusual JSON';
  StrL := TStringList.Create;
  N := TMcJsonItem.Create;
  anyFail := False;
  try
    // keys
    StrL.Add('key: empty'          +'='+ '{"":"value"}'               );
    StrL.Add('key: keyword'        +'='+ '{"{":"value"}'              );
    // values
    StrL.Add('value: alone'        +'='+ '"k"'                        );
    StrL.Add('value: leading zero' +'='+ '{"k": 0.1234}'              );
    // objects
    StrL.Add('object: empty'       +'='+ '{}'                         );
    StrL.Add('object: empty w key' +'='+ '{"o": { }}'                 );
    // arrays
    StrL.Add('array: empty no key' +'='+ '[]'                         );
    StrL.Add('array: empty w key'  +'='+ '{"a": [ ]}'                 );
    StrL.Add('array: empty'        +'='+ '{"k":[]}'                   );
    StrL.Add('array: no root'      +'='+ '[1,2]'                      );
    StrL.Add('array: bi openned'   +'='+ '{"k":[["1","2"]]}'          );
    // json inside a json
    StrL.Add('value: escaped json' +'='+ '{"k":"{\"key\":\"value\"}"}');
    // check
    for i:=0 to StrL.Count-1 do
    begin
      sName   := Trim( StrL.Names[i]      );
      sTest   := Trim( StrL.Values[sName] );
      if ( not N.Check(sTest) ) then
      begin
        Msg := Msg + #13#10 + sIndent + 'Expected to pass but fail: ' + sName;
        anyFail := True;
      end;
    end;
    // if any fail, report fail
    Result := not anyFail;
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
      Result := False;
    end;
  end;
  StrL.Free;
  N.Free;
end;

function Test12(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test 12: type transformations';
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
                     and (N['k2'].Items[0].AsString  = 'a'      )
                     and (N['k2'].Items[1].AsString  = 'b'      );
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
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
end;

function Test13(out Msg: string): Boolean;
var
  N, M: TMcJsonItem;
  i, idx: Integer;
begin
  Msg := 'Test 13: Save and Load using files';
  N := TMcJsonItem.Create();
  M := TMcJsonItem.Create();
  try
    Result := True;
    // create a simple object.
    N.AsJSON := '{"i": 123}';
    // now add a array of objects
    N.Add('array').ItemType := jitArray;
    for i := 1 to 2 do
      N['array'].Add.AsJSON := '{"k'+IntToStr(i)+'": "ç'+IntToStr(i)+'"}';
    // save to file (not Human readable and UTF-8)
    N.SaveToFile('test13.json', false);
    // change N using IndexOf
    idx := N.IndexOf('array');
    if (idx >= 0) then
      N.Delete(idx);
    // load from file (default UTF-8 convertion)
    M.LoadFromFile('test13.json');
    // check before and after delete
    Result := Result and (N.AsJSON = '{"i":123}'                                  )
                     and (M.AsJSON = '{"i":123,"array":[{"k1":"ç1"},{"k2":"ç2"}]}');
    // load a Ansi file (no convertion to UTF-8 is needed)
    M.LoadFromFile('test13-Ansi.json', false);
    Result := Result and (M['ansi'].AsString = 'ãçüö');
    // load a UTF-8 file (default UTF-8 convertion)
    M.LoadFromFile('test13-UTF8.json');
    Result := Result and (M['utf8'].AsString = 'ãçüö');
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
  M.Free;
end;

function Test14(out Msg: string): Boolean;
var
  N, M, P, Q: TMcJsonItem;
begin
  Msg := 'Test 14: constructors';
  N := nil;
  M := nil;
  P := nil;
  Q := nil;
  try
    Result := True;
    // constructor empty
    N := TMcJsonItem.Create();
    Result := Result and (N.AsJSON = '');
    // constructor by type
    Q := TMcJsonItem.Create(jitArray);
    Q.Add.AsInteger := 1;
    Q.Add.AsInteger := 2;
    Result := Result and (Q.AsJSON = '[1,2]');
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
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
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
  N, M, P, Q: TMcJsonItem;
begin
  Msg := 'Test 15: Copy, Clone, IsEqual, Remove functions';
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
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
  M.Free;
  P.Free;
  Q.Free;
end;

function Test16(out Msg: string): Boolean;
var
  N: TMcJsonItem;
  i: Integer;
  anyPass: Boolean;
begin
  Msg := 'Test 16: exceptions';
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
        N.Path('s/not').AsInteger;
        anyPass := True;
      end
      else if (i = 3) then
      begin
        N['s'].Items[1].AsInteger;
        anyPass := True;
      end
      // Exception Invalid item type
      else if (i = 4) then
      begin
        N['s'].AsObject;
        anyPass := True;
      end
      // Exception Can't convert item "%s" with value "%s" to "%s"
      else if (i = 5) then
      begin
        N['s'].AsInteger;
        anyPass := True;
      end
      // Exception Can't convert item "%s" to "%s"
      else if (i = 6) then
      begin
        N.AsJSON := '{"n": null}';
        N['n'].AsInteger;
        anyPass := True;
      end
      // Exception Duplicate key "%s"
      else if (i = 7) then
      begin
        N.SpeedUp := False;
        N.AsJSON := '{"k":"v", "k":"v"}';
        anyPass := True;
      end
      // Exception Error while parsing text: "%s" at pos "%s"
      else if (i = 8) then
      begin
        N.AsJSON := '{"n"[:null}';
        anyPass := True;
      end;
    except
      on E: Exception do
      begin
        Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
        Result := Result and (not anyPass);
      end;
    end;
  end;
  N.Free;
end;

function Test17(out Msg: string): Boolean;
var
  N, item: TMcJsonItem;
begin
  Msg := 'Test 17: enumerators';
  N    := TMcJsonItem.Create;
  item := nil;
  try
    N.AsJSON := '{"o": {"k1":"v1", "k2":"v2"}}';
    // use enumerator to browse values.
    for item in N['o'] do
      item.AsJSON;
    // check final value
    Result := (item.AsString = 'v2');
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
end;

function Test18(out Msg: string): Boolean;
var
  Obj, ChildObj: TMcJsonItem;
begin
  Msg := 'Test 18: example like JsonDataObjects';
  Obj := TMcJsonItem.Create;
  try
    // access (automatic creation as in JDO)
    Obj.S['foo'] := 'bar';
    Obj.S['bar'] := 'foo';
    // array creation, Obj is the owner of 'array'
    Obj.A['array'].Add.AsInteger := 10;
    Obj.A['array'].Add.AsInteger := 20;
    // object creation, 'array' is the owner of ChildObj
    ChildObj := Obj['array'].Add(jitObject);
    ChildObj.D['value'] := 12.3;
    // array creation, ChildObj is the owner of 'subarray'
    ChildObj.A['subarray'].Add.AsInteger := 100;
    ChildObj.A['subarray'].Add.AsInteger := 200;
    Result := (Obj.AsJSON = '{"foo":"bar","bar":"foo","array":[10,20,{"value":12.3,"subarray":[100,200]}]}');
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
      Result := False;
    end;
  end;
  Obj.Free;
end;

function Test19(out Msg: string): Boolean;
var
  N: TMcJsonItem;
begin
  Msg := 'Test 19: At() shortener for array item access';
  N := TMcJsonItem.Create;
  Result := True;
  try
    // how to access a at pos 1.
    N.AsJSON := '{"a": [1, 2, 3]}';
    Result := Result and (N['a'].Items[1].AsInteger = 2);
    Result := Result and (N.At('a',1).AsInteger     = 2);
    // how to access k2 at pos 1.
    N.AsJSON := '{"a": [{"k1":1,"k2":2},{"k1":10,"k2":20}]}';
    Result := Result and (N['a'].Items[1].Values['k2'].AsInteger = 20);
    Result := Result and (N['a'].Items[1]['k2'].AsInteger        = 20);
    Result := Result and (N['a'].At(1,'k2').AsInteger            = 20);
    // other uses
    N.AsJSON := '{"k1":1,"k2":2,"k3":3,"k4":4}';
    Result := Result and (N['k3'].AsInteger    = 3);
    Result := Result and (N.Items[2].AsInteger = 3);
    Result := Result and (N.At(2).AsInteger    = 3);
    Result := Result and (N.At('k3').AsInteger = 3);
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
      Result := False;
    end;
  end;
  N.Free;
end;

function Test20(out Msg: string): Boolean;
var
  N, M: TMcJsonItem;
begin
  Msg := 'Test 20: key paths';
  N   := TMcJsonItem.Create;
  Result := True;
  try
    N.AsJSON := '{"o": {"k1":"v1", "k2":"v2"}}';
    // get second object using path of keys.
    M := N.Path('o/k2');
    Result := Result and (M.AsString = 'v2');
    M := N.Path('/o/k2');
    Result := Result and (M.AsString = 'v2');
    M := N.Path('o/k2/');
    Result := Result and (M.AsString = 'v2');
    M := N.Path('/o/k2/');
    Result := Result and (M.AsString = 'v2');
    M := N.Path('\o\k2\');
    Result := Result and (M.AsString = 'v2');
    M := N.Path('o.k2');
    Result := Result and (M.AsString = 'v2');
  except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
      Result := False;
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
      Json.Add('array', jitArray);
      for i := 1 to 3 do
        Json['array'].Add.AsInteger := i;
      // save a backup to file
      if (Json['array'].Count = 3) then
        Json.SaveToFile('test99.json');
      // remove an item
      Json.Delete('array');
      // oops, load the backup
      if (Json.Count = 4) then
        Json.LoadFromFile('test99.json');
      // test final result
      Result := (Json.AsJSON = '{"key1":1,"key2":true,"key3":1.234,"key4":"value 1","array":[1,2,3]}');
    except
    on E: Exception do
    begin
      Msg := Msg + #13#10 + sIndent + 'Error: ' + E.Message;
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
  ReportMemoryLeaksOnShutdown := true;

  TotalPassed := 0;
  TotalFailed := 0;

  // [PASS] [
  sIndent := '       ';

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
  Check(Test16, TotalPassed, TotalFailed);
  Check(Test17, TotalPassed, TotalFailed);
  Check(Test18, TotalPassed, TotalFailed);
  Check(Test19, TotalPassed, TotalFailed);
  Check(Test20, TotalPassed, TotalFailed);

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
