with Progress_Indicators.Work_Trackers;

package SP.Progress is

    package PI renames Progress_Indicators;

    task type Update_Progress (Work : access PI.Work_Trackers.Work_Tracker) with CPU => 1 is
        entry Stop;
    end Update_Progress;

end SP.Progress;
