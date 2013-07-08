Unit ICal;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, StrUtils, FGL;

Type

  TStringArray = Array of String;

  { TICalReader }

  TICalReader = class
  private
    FFilename : String;
    FText     : Text;
    FEntry    : String;    // last entry (might have benn multi-line)
    FLine     : String;    // last text line
    FDepth    : Integer;   // only 0 and 1 are implemented (otherwise FLine must be an array and a stack must be implemented)
  public
    Constructor Create(AFilename : String);
    Destructor Destroy; override;
    Function Get : String;
    Procedure Push;
    Procedure Expect(St:String);
    class Procedure Split(Entry:String;Out Key,Value:String;Delimiter:Char=':');
    class Function Split(Entry:String):TStringArray;
    class Function DateTime(Value:String):TDateTime;
    Class Function Date(Value:String):TDateTime;
    class Function DateTime(KeyParts:TStringArray;Value:String):TDateTime;
  End;

  TStringStringMap = specialize TFPGMap<String,String>;

  { TICalBlock }

  TICalBlock = class
    FVerbosity    : Integer;
    FUnknown      : TStringStringMap;
    Constructor Create;
    Destructor Destroy; override;
    Procedure Read(ABlock:String;AReader:TICalReader);
    Function ReadEntry(Entry,Key,Value:String) : Boolean; virtual;
    Function ReadBlock(Block:String;AReader:TICalReader) : Boolean; virtual;
  End;

  TICalAlarm = class(TICalBlock)
    FUID          : String;
    FAction       : String;   // TODO: implement
    FTrigger      : String;   // TODO: implement
    FDescription  : String;
  End;

Type
  TICalTransp = (itOpaque,itTransparent);
Const
  SICalTransp : Array[TICalTransp] of String = ('OPAQUE','TRANSPARENT');

Type

  { TICalEvent }

  TICalEvent = class(TICalBlock)
    FUID          : String;
    FDTStamp      : TDateTime;
    FTransp       : String; // TODO: TICalTransp;
    FDTStart      : TDateTime;
    FDTEnd        : TDateTime;
    FSummary      : String;
    FCreated      : TDateTime;
    FLastModified : TDateTime;
    FAlarm        : TICalAlarm;
    Function ReadEntry(Entry,Key,Value:String) : Boolean; override;
    Function ReadBlock(Block:String;AReader:TICalReader) : Boolean; override;
  End;
  TICalEvents = specialize TFPGList<TICalEvent>;

  { TICalCalendar }

  TICalCalendar = class(TICalBlock)
    FCalScale    : String;   // TODO: implement
    FProdID      : String;
    FVersion     : String;
    FEvents      : TICalEvents;

    Constructor Create;
    Constructor Create(AReader:TICalReader);
    Constructor Create(AFilename:String);
    Destructor Destroy; override;
    Function ReadEntry(Entry,Key,Value:String) : Boolean; override;
    Function ReadBlock(Block:String;AReader:TICalReader) : Boolean; override;
  End;

Const
  ReaderVerbosity : Integer = 0;

Function CompareDTStart(Const E1,E2:TICalEvent) : Integer;

Implementation

Function CompareDTStart(Const E1,E2:TICalEvent):Integer;
Begin
  if      E1.FDTStart < E2.FDTStart then Result := -1
  else if E1.FDTStart > E2.FDTStart then Result :=  1
  else Result := 0;
End;

{ TICalReader }

Constructor TICalReader.Create(AFilename:String);
Begin
  inherited Create;
  FFilename := AFilename;
  Assign(FText,FFilename);
  Reset(FText);
  ReadLn(FText,FLine);  // read first line
End;

Destructor TICalReader.Destroy;
Begin
  Close(FText);
  Inherited Destroy;
End;

Function TICalReader.Get:String;
Var St : String;
Begin
  if FDepth > 0 then
    Begin
      Dec(FDepth);
    End
  else
    Begin
      // use text-line which was read last time
      // the very first time this was initialized by the constructor
      // if EOF=true here, we still have one line in FLine, but then we set
      // FLine to an empty string
      FEntry := FLine;
      if EOF(FText) then
        FLine := '';
      While not EOF(FText) do
        Begin
          ReadLn(FText,FLine);
          if FLine = '' then continue;
          if FLine[1] <> ' ' then break;
          FEntry := FEntry + Copy(FLine,2,Length(FLine));
        End;
    End;
  Result := FEntry;
End;

Procedure TICalReader.Push;
Begin
  Inc(FDepth);
  if FDepth > 1 then
    raise Exception.Create('Can''t push more than once');
End;

Procedure TICalReader.Expect(St:String);
Begin
  if Get <> St then
    raise Exception.Create('Expected '''+St+''' but got '''+FEntry+'''');
End;

Class Procedure TICalReader.Split(Entry:String;Out Key,Value:String;Delimiter:Char=':');
Var I : Integer;
Begin
  I := Pos(Delimiter,Entry);
  if I = 0 then
    Begin
      Key := Entry;
      Value := '';
      Exit;
    End;
  Key   := Copy(Entry,1,I-1);
  Value := Copy(Entry,I+1,Length(Entry));
End;

Class Function TICalReader.Split(Entry:String):TStringArray;
Var P1,P2 : Integer;
Begin
  P1 := 1;
  SetLength(Result,0);
  repeat
    P2 := PosEx(';',Entry,P1);
    SetLength(Result,Length(Result)+1);
    if P2 = 0 then
      Begin
        Result[Length(Result)-1] := Copy(Entry,P1,Length(Entry));
        Exit;
      End;
    Result[Length(Result)-1] := Copy(Entry,P1,P2-P1);
    P1 := P2+1;
  Until false;
End;

Class Function TICalReader.Date(Value:String):TDateTime;
Begin
  Result := ComposeDateTime(
    EncodeDate(
      StrToInt(Copy(Value,1,4)),
      StrToInt(Copy(Value,5,2)),
      StrToInt(Copy(Value,7,2))),
    0);
End;

Class Function TICalReader.DateTime(Value:String):TDateTime;
Begin
  Result := ComposeDateTime(
    EncodeDate(
      StrToInt(Copy(Value,1,4)),
      StrToInt(Copy(Value,5,2)),
      StrToInt(Copy(Value,7,2))),
    EncodeTime(
      StrToInt(Copy(Value,10,2)),
      StrToInt(Copy(Value,12,2)),
      StrToInt(Copy(Value,14,2)),
      0));
End;

Class Function TICalReader.DateTime(KeyParts:TStringArray;Value:String):TDateTime;
Type TValue = (tvDateTime,tvDate,tvTime);
Var I : Integer;
    K,V : String;
    ValuePart : TValue;
Begin
  ValuePart := tvDateTime;
  For I := 1 to Length(KeyParts)-1 do
    Begin
      TICalReader.Split(KeyParts[I],K,V,'=');
      Case K of
        'TZID'  : ;  // ignored, TODO
        'VALUE' : Case V of
                    'DATE' : ValuePart := tvDate;
                  else
                    raise Exception.Create('Unknown VALUE for date/time entry '''+V+'''');
                  End
      else
        raise Exception.Create('Unknown key part key '''+K+''' for entry '''+KeyParts[0]+':'+Value+'''');
      End;
    End;
  Case ValuePart of
    tvDateTime : Result := TICalReader.DateTime(Value);
    tvDate     : Result := TICalReader.Date(Value);
  else
    raise Exception.Create('Unimplemented ValuePart');
  End;
End;

{ TICalBlock }

Constructor TICalBlock.Create;
Begin
  inherited Create;
  FVerbosity := ReaderVerbosity;  // default value
  FUnknown := TStringStringMap.Create;
  FUnknown.Duplicates := dupError;
End;

Destructor TICalBlock.Destroy;
Begin
  FUnknown.Free;
  Inherited Destroy;
End;

Procedure TICalBlock.Read(ABlock:String;AReader:TICalReader);
Var St        : String;
    Key,Value : String;
    B         : TICalBlock;
Begin
  // first line 'BEGIN:<block>'
  AReader.Expect('BEGIN:'+ABlock);
  // second line, parse the file
  St := AReader.Get;
  While St > '' do
    Begin
      AReader.Split(St,Key,Value);
      if Key = 'BEGIN' then
        Begin
          AReader.Push;
          if not ReadBlock(Value,AReader) then
            Begin
              if FVerbosity > 0 then
                WriteLn('Warning: unimplemented block ',Value);
              // handle unimplemented block
              B := TICalBlock.Create;
              B.Read(Value,AReader);
              B.Free;
            End;
        End
      else if (Key = 'END') and (Value = ABlock) then
        Begin
          // block closing --> done
          break;
        End
      else
        Begin
          if not ReadEntry(St,Key,Value) then
            Begin
              if FVerbosity > 0 then
                WriteLn('Warning: unimplemented entry ',St);
              FUnknown.Add(Key,Value);
            End;
        End;
      St := AReader.Get;
    End;
End;

Function TICalBlock.ReadEntry(Entry,Key,Value:String):Boolean;
Begin
  // handle an unimplemented entry
  Result := false;
End;

Function TICalBlock.ReadBlock(Block:String;AReader:TICalReader):Boolean;
Begin
  // handle an unimplemented block
  Result := false;
End;

{ TICalEvent }

Function TICalEvent.ReadEntry(Entry,Key,Value:String):Boolean;
Var KeyParts : TStringArray;
Begin
  Result := True; // entry was handled
  KeyParts := TICalReader.Split(Key);
  Case KeyParts[0] of
    'UID'           : FUID          := Value;
    'DTSTAMP'       : FDTStamp      := TICalReader.DateTime(KeyParts,Value);
    'TRANSP'        : FTransp       := Value;
    'DTSTART'       : FDTStart      := TICalReader.DateTime(KeyParts,Value);
    'DTEND'         : FDTEnd        := TICalReader.DateTime(KeyParts,Value);
    'SUMMARY'       : FSummary      := Value;
    'CREATED'       : FCreated      := TICalReader.DateTime(KeyParts,Value);
    'LAST-MODIFIED' : FLastModified := TICalReader.DateTime(KeyParts,Value);
  else
    Result := false;  // couldn't handle the entry
  End;
End;

Function TICalEvent.ReadBlock(Block:String;AReader:TICalReader):Boolean;
Begin
  Result := True; // entry was handled
  Case Block of
    'VALARM' : Begin FAlarm := TICalAlarm.Create; FAlarm.Read('VALARM',AReader); End;  // TODO: check if we got multiple alarms
  else
    Result := false;  // couldn't handle the block
  End;
End;

{ TICalCalendar }

Constructor TICalCalendar.Create;
Begin
  inherited Create;
  FEvents := TICalEvents.Create;
End;

Constructor TICalCalendar.Create(AReader:TICalReader);
Begin
  Create;
  Read('VCALENDAR',AReader);
End;

Constructor TICalCalendar.Create(AFilename:String);
Var Reader : TICalReader;
Begin
  Reader := TICalReader.Create(AFilename);
  Create(Reader);
  Reader.Free;
End;

Destructor TICalCalendar.Destroy;
Begin
  FEvents.Free;
  Inherited Destroy;
End;

Function TICalCalendar.ReadEntry(Entry,Key,Value:String):Boolean;
Begin
  Result := True; // entry was handled
  Case Key of
    'CALSCALE' : FCalScale := Value;
    'PRODID'   : FProdID   := Value;
    'VERSION'  : FVersion  := Value;
  else
    Result := false;  // couldn't handle the entry
  End;
End;

Function TICalCalendar.ReadBlock(Block:String;AReader:TICalReader):Boolean;
Var Event : TICalEvent;
Begin
  Result := True; // entry was handled
  Case Block of
    'VEVENT' : Begin Event := TICalEvent.Create; Event.Read('VEVENT',AReader); FEvents.Add(Event); End;
  else
    Result := false;  // couldn't handle the block
  End;
End;

End.

