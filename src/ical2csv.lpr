(**
 * Convert iCal file to CSV file
 *
 *
 * TODO:
 *  - corrected for overlaps
 *)
Program ICal2CSV;

{$mode objfpc}{$H+}

Uses
  Classes, SysUtils, ICal;

Procedure Usage;
Begin
  WriteLn('Usage: ical2csv [-s] infile.ics');
  WriteLn('  -s   sort events by starts time');
  WriteLn('The CSV file is printed to stdout');
  Halt;
End;

Var
  Filename : String;
  Sort     : Boolean;
  Cal : TICalCalendar;
  I   : Integer;
  St  : String;

Begin
  // file name is last parameter
  if ParamCount < 1 then
    Usage;
  Filename := ParamStr(ParamCount);
  // check all other parameters
  Sort := false;
  For I := 1 to ParamCount-1 do
    Begin
      Case ParamStr(I) of
        '-s'             : Sort := true;
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
  For I := 0 to Cal.FEvents.Count-1 do
    Begin
      St := FormatDateTime('yyyy-mm-dd hh:nn:ss',Cal.FEvents[I].FDTStart) + ',' +
            FormatDateTime('yyyy-mm-dd hh:nn:ss',Cal.FEvents[I].FDTEnd)   + ',' +
            FormatDateTime('hh:nn:ss',Cal.FEvents[I].FDTEnd - Cal.FEvents[I].FDTStart) + ',' +
            '"' + Cal.FEvents[I].FSummary + '"';

      WriteLn(St);
    End;

  Cal.Free;
End.

