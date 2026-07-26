SP.Help.Header ("Quotes");

SP.Help.Block (
    "Say ""hello"" to quoted text in help."
);

SP.Help.Block (
    "Commands may mention quotes: " & Colorize_Command ("echo ""x""") & "."
);

SP.Help.Block (
    "A bare apostrophe is fine: don't panic."
);

