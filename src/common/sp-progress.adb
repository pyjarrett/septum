with Progress_Indicators.Spinners;
with SP.Terminal;

package body SP.Progress is

    task body Update_Progress is
        Spinner : PI.Spinners.Spinner := PI.Spinners.Make (PI.Spinners.Normal, 1);
        SR      : PI.Work_Trackers.Status_Report;
    begin
        loop
            select
                accept Stop;
                exit;
            or
                delay 0.2;
            end select;

            SP.Terminal.Beginning_Of_Line;
            SP.Terminal.Clear_Line;
            SR := Work.Report;
            PI.Spinners.Tick(Spinner);

            SP.Terminal.Put
                (PI.Spinners.Value (Spinner) & "  " & SR.Completed'Image &
                    " done of" & SR.Total'Image & "   " & PI.Spinners.Value (Spinner));
        end loop;
    end Update_Progress;

end SP.Progress;
