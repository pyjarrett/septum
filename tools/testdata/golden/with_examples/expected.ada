SP.Help.Header ("Examples");

SP.Help.Block (
    "Use " & Colorize_Command ("add-dirs") & " then search."
);

SP.Help.Example (
    String_Vectors.Empty_Vector
    & To_Unbounded_String ("add-dirs src")
    & To_Unbounded_String ("find-text ""needle""")
    & To_Unbounded_String ("match-contexts")
);

SP.Help.Block (
    "Quotes in examples are preserved literally above."
);

