SP.Help.Header ("Commands");

SP.Help.Block (
    "Use " & Colorize_Command ("add-dirs") & " to load directories into the search pool."
);

SP.Help.Block (
    Colorize_Command ("list-dirs") & " lists currently loaded directories while " & Colorize_Command ("list-files") & " lists individual files."
);

SP.Help.Block (
    "Commands can appear mid-sentence: run " & Colorize_Command ("reload") & " after edits."
);

