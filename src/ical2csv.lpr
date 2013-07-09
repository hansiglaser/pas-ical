(**
 * Convert iCal file to CSV file
 *
 *)
Program ICal2CSV;

{$mode objfpc}{$H+}

Uses
  Classes, SysUtils, Math, ICal;

Procedure Usage;
Begin
  WriteLn('Usage: ical2csv [-s] [-o] infile.ics');
  WriteLn('  -s   sort events by starts time');
  WriteLn('  -o   merge overlapping events (implies -s)');
  WriteLn('The CSV file is printed to stdout');
  Halt;
End;

Var
  Filename : String;
  Sort     : Boolean;
  Overlaps : Boolean;
  I,J      : Integer;
  Cal      : TICalCalendar;
  Event    : TICalEvent;
  NewEvent : Boolean;
  St       : String;

Begin
  // file name is last parameter
  if ParamCount < 1 then
    Usage;
  Filename := ParamStr(ParamCount);
  // check all other parameters
  Sort     := false;
  Overlaps := false;
  For I := 1 to ParamCount-1 do
    Begin
      Case ParamStr(I) of
        '-s'             : Sort := true;
        '-o'             : Begin Sort := true; Overlaps := true; End;
        '-h', '--help'   : Usage;
      End
    End;

  // read ICal file
  try
    Cal := TICalCalendar.Create(Filename);
  except
    on E : Exception do
      Begin
        WriteLn('Error reading ICal file ''',Filename,''': ',E.Message);
        Halt(1);
      End;
  End;

  // optional sorting
  if Sort then
    Cal.FEvents.Sort(@CompareDTStart);

  // print as CSV
  I := 0;
  While I < Cal.FEvents.Count do
    Begin
      Event := Cal.FEvents[I];
      NewEvent := false;

      // find overlaps
      if Overlaps then
        Begin
          J := 1;
          while (I+J < Cal.FEvents.Count) and (Cal.FEvents[I+J].FDTStart < Event.FDTEnd) do
            Begin
              if J = 1 then
                Begin
                  // create new dummy event which summarizes all overlapping events
                  Event := TICalEvent.Create;
                  NewEvent := true;
                  With Cal.FEvents[I] do
                    Begin
                      // copy old
                      Event.FDTStart := FDTStart;
                      Event.FDTEnd   := FDTEnd;
                      Event.FSummary := '*Overlap* ' + FSummary;
                    End;
                End;
              With Cal.FEvents[I+J] do
                Begin
                  // extedn dummy event with end time and summary
                  Event.FDTEnd   := Max(Event.FDTEnd,FDTEnd);
                  Event.FSummary := Event.FSummary + '; ' + FSummary;
                End;
              Inc(J);
            End;
          I := I + J - 1;
        End;

      // print
      With Event do
        St := FormatDateTime('yyyy-mm-dd hh:nn:ss',FDTStart) + ',' +
              FormatDateTime('yyyy-mm-dd hh:nn:ss',FDTEnd)   + ',' +
              FormatDateTime('hh:nn:ss',FDTEnd - FDTStart) + ',' +
              '"' + FSummary + '"';

      WriteLn(St);

      if NewEvent then
        Event.Free;

      Inc(I);
    End;

  Cal.Free;
End.

