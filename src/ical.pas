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
    class Procedure Split(Entry:String;Out Key,Value:String);
    class Function Split(Entry:String):TStringArray;
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

Implementation

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

Class Procedure TICalReader.Split(Entry:String;Out Key,Value:String);
Var I : Integer;
Begin
  I := Pos(':',Entry);
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
    'DTSTAMP'       : FDTStamp      := 0;
    'TRANSP'        : FTransp       := Value;
    'DTSTART'       : FDTStart      := 0;
    'DTEND'         : FDTEnd        := 0;
    'SUMMARY'       : FSummary      := Value;
    'CREATED'       : FCreated      := 0;
    'LAST-MODIFIED' : FLastModified := 0;
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

