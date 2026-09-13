package body SP.Time is
   function MM_SS (Elapsed : Ada.Real_Time.Time_Span) return String is
        Total_Seconds : constant Natural :=
            Natural (Ada.Real_Time.To_Duration (Elapsed));
        Minutes : constant Natural := Total_Seconds / 60;
        Seconds : constant Natural := Total_Seconds mod 60;
    begin
        return Minutes'Image (2 .. Minutes'Image'Last) & ":"
            & (if Seconds < 10 then "0" else "")
            & Seconds'Image (2 .. Seconds'Image'Last);
    end MM_SS;
end SP.Time;
