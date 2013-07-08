Program TestICal;

{$mode objfpc}{$H+}

Uses
  Classes, ICal;

Var
  Cal : TICalCalendar;

Begin
  Cal := TICalCalendar.Create('/home/hansi/.local/share/evolution/calendar/1289818695.26386.4@hansi/calendar.ics');


  Cal.Free


End.

