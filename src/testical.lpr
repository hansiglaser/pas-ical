(*

TODO:
 - create new program to create a CSV file with all events (sorted), start- and
   end time, duration, and corrected for overlaps
*)

Program TestICal;

{$mode objfpc}{$H+}

Uses
  Classes, SysUtils, ICal;

Var
  Cal : TICalCalendar;
  I   : Integer;

Begin
  Cal := TICalCalendar.Create('/home/hansi/.local/share/evolution/calendar/1289818695.26386.4@hansi/calendar.ics');

  Cal.FEvents.Sort(@CompareDTStart);

  For I := 0 to Cal.FEvents.Count-1 do
    WriteLn(DateTimeToStr(Cal.FEvents[I].FDTStart),': ',Cal.FEvents[I].FSummary);

  Cal.Free;
End.

