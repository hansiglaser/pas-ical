(*
*)

Program TestICal;

{$mode objfpc}{$H+}

Uses
  Classes, SysUtils, ICal;

Var
  Cal : TICalCalendar;
  I   : Integer;

Begin
  Cal := TICalCalendar.Create('calendar.ics');

  Cal.FEvents.Sort(@CompareDTStart);

  For I := 0 to Cal.FEvents.Count-1 do
    WriteLn(DateTimeToStr(Cal.FEvents[I].FDTStart),': ',Cal.FEvents[I].FSummary);

  Cal.Free;
End.

