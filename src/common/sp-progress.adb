with Ada.Calendar;
with Ada.Strings.Fixed;
with Progress_Indicators.Spinners;
with SP.Terminal;
with Trendy_Terminal.VT100;

package body SP.Progress is

    task body Update_Progress is
        Spinner      : PI.Spinners.Spinner := PI.Spinners.Make (PI.Spinners.Normal, 1);
        SR           : PI.Work_Trackers.Status_Report;
        Start_Time   : Ada.Calendar.Time;
        Current_Time : Ada.Calendar.Time;

        procedure Update is
            use all type Ada.Calendar.Time;
        begin
            Current_Time := Ada.Calendar.Clock;

            SP.Terminal.Beginning_Of_Line;
            SP.Terminal.Clear_Line;
            SR := Work.Report;
            PI.Spinners.Tick(Spinner);

            declare
                Seconds : constant Natural := Natural (Float'Rounding (100.0 * Float (Current_Time - Start_Time)) * 0.01);
                Elapsed : constant String := '(' & (if Seconds = 0
                    then "<1 s"
                    else Ada.Strings.Fixed.Trim (Seconds'Image, Ada.Strings.Left) & " s")
                    & ')';
            begin
                SP.Terminal.Put (
                    PI.Spinners.Value (Spinner)
                    & "  "
                    & SR.Completed'Image
                    & " done of"
                    & SR.Total'Image
                    & "   "
                    & Elapsed
                    & "   "
                    & PI.Spinners.Value (Spinner));
            end;
        end Update;
    begin
        Start_Time := Ada.Calendar.Clock;
        Trendy_Terminal.VT100.Hide_Cursor;
        loop
            select
                accept Stop;
                Trendy_Terminal.VT100.Show_Cursor;
                exit;
            or
                delay 0.2;
            end select;

            Update;
        end loop;
    end Update_Progress;

end SP.Progress;
