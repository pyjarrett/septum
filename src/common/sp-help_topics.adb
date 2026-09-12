with SP.Help;
package body SP.Help_Topics is
   pragma Style_Checks(Off);
   procedure About is
   begin
      SP.Help.Plain("");

      SP.Help.Block("Septum provides interactive searching of a codebase for blocks of lines which contain the terms you want, and exclude the terms you don't want.  ");

      SP.Help.Block("It's different from grep by interactively allowing filters to be added and removed, and focusing on multiple line groups. If you're doing a search looking for something on a specific line, you probably want to use grep (or ripgrep). If you're looking for a block of code with a bunch of terms in those lines and want to whittle it down since they appear in a lot of places, you probably want to use Septum. Using a context width of 0 approximates an interactive grep.  ");

      SP.Help.Block("Limiting the search into blocks around search terms allows searching for elements in arbitrary order which may span across lines, in a way which can be difficult to express in other tools. Sometimes terms appear multiple times in a project and have names which change based on context. Septum allows exclusion of these contexts.  ");

      SP.Help.Block("Filters get applied in turn with contexts being removed at every step. Similar contexts get deduplicated before presented to cut out more clutter.  ");

      SP.Help.Block("Example:  ");

      SP.Help.Plain("    > find-like String");
      SP.Help.Plain("    > match-contexts");
      SP.Help.Plain("");

      SP.Help.Plain("    D:/dev/ada/dir_iterators/src/dir_iterators-recursive.ads");
      SP.Help.Plain("        57      end record;");
      SP.Help.Plain("        58");
      SP.Help.Plain("        59      -- The list of unprocessed directories needs to be stored.");
      SP.Help.Plain("    ->     60      package String_Vectors is new Ada.Containers.Vectors");
      SP.Help.Plain("        61         (Index_Type   => Positive,");
      SP.Help.Plain("    ->     62          Element_Type => Ada.Strings.Unbounded.Unbounded_String,");
      SP.Help.Plain("    ->     63          ""=""          => Ada.Strings.Unbounded.""="");");
      SP.Help.Plain("");

      SP.Help.Plain("    ... other results ...");
      SP.Help.Plain("");

      SP.Help.Plain("    Matching contexts:  1063");
      SP.Help.Plain("    Matching files: 118");
      SP.Help.Plain("");

      SP.Help.Block("I don't want Unbounded strings, so exclude those from results.  ");

      SP.Help.Plain("    > exclude-like Unbounded");
      SP.Help.Plain("    > match-contexts");
      SP.Help.Plain("");

      SP.Help.Plain("    D:/dev/ada/septum/src/linux/sp-platform.adb");
      SP.Help.Plain("        11              if Env.Exists (""XDG_CONFIG_HOME"") then");
      SP.Help.Plain("    ->     12                  S := SP.Strings.String_Holders.To_Holder (Ada.Directories.Full_Name (Env.Value (""XDG_CONFIG_HOME"")));");
      SP.Help.Plain("        13              elsif Env.Exists (""HOME"") then");
      SP.Help.Plain("    ->     14                  S := SP.Strings.String_Holders.To_Holder (Ada.Directories.Full_Name (Env.Value (""HOME"") & ""/.config""));");
      SP.Help.Plain("        15              end if;");
      SP.Help.Plain("        16          end return;");
      SP.Help.Plain("        17      end Global_Config_Dir;");
      SP.Help.Plain("");

      SP.Help.Plain("    ... a bunch of other results ...");
      SP.Help.Plain("");

      SP.Help.Plain("    Matching contexts:  669");
      SP.Help.Plain("    Matching files: 111");
      SP.Help.Plain("");

      SP.Help.Block("I also got a bunch of results related to String_Holders, SP.Strings,  Ada.Strings, String_Vectors and functions which return Strings, and string  parameters to subprograms but I don't want those either.  ");

      SP.Help.Block("Also, it looks like the project has a linux specific folder and some other  projects I don't want, so ignore those in the results.  ");

      SP.Help.Plain("    > exclude-like Holder SP.Strings String_Vectors "": String"" ""Ada.Strings"" ""return string"" : String");
      SP.Help.Plain("    > exclude-path linux ada/trendy_test ada/dir_iterators ada/trendy_terminal obj/");
      SP.Help.Plain("");

      SP.Help.Plain("    Files:      895              Extensions:   Any");
      SP.Help.Plain("    Path Filters: None");
      SP.Help.Plain("");

      SP.Help.Plain("    Distance:   3                Max Results:  50");
      SP.Help.Plain("    Filters:");
      SP.Help.Plain("    1      KEEP : Case Insensitive Match ""STRING""");
      SP.Help.Plain("    2          EXCLUDE : Case Insensitive Match ""UNBOUNDED""");
      SP.Help.Plain("    3              EXCLUDE : Case Insensitive Match ""HOLDER""");
      SP.Help.Plain("    4                  EXCLUDE : Case Insensitive Match ""SP.STRINGS""");
      SP.Help.Plain("    5                      EXCLUDE : Case Insensitive Match ""STRING_VECTORS""");
      SP.Help.Plain("    6                          EXCLUDE : Case Insensitive Match ""RETURN STRING""");
      SP.Help.Plain("    7                              EXCLUDE : Case Insensitive Match ""ADA.STRINGS""");
      SP.Help.Plain("    8                                  EXCLUDE : Case Insensitive Match "":""");
      SP.Help.Plain("    9                                      EXCLUDE : Case Insensitive Match ""STRING""");
      SP.Help.Plain("");

      SP.Help.Plain("");

      SP.Help.Block("Oops looks like I forgot to quote "": String"" for parameters, let's redo that.  ");

      SP.Help.Plain("    > drop 8 9");
      SP.Help.Plain("    > exclude-like "": String""");
      SP.Help.Plain("");

      SP.Help.Plain("    Matching contexts:  67");
      SP.Help.Plain("    Matching files: 14");
      SP.Help.Plain("");

      SP.Help.Block("That's more reasonable. What files is it in?  ");

      SP.Help.Plain("    > match-f");
      SP.Help.Plain("    Resolved to: match-files");
      SP.Help.Plain("");

      SP.Help.Plain("");

      SP.Help.Plain("    D:/dev/ada/septum/src/common/sp-cache.adb");
      SP.Help.Plain("    D:/dev/ada/septum/src/common/sp-cache.ads");
      SP.Help.Plain("    D:/dev/ada/septum/src/common/sp-commands.adb");
      SP.Help.Plain("    D:/dev/ada/septum/src/common/sp-config.adb");
      SP.Help.Plain("    D:/dev/ada/septum/src/common/sp-config.ads");
      SP.Help.Plain("    D:/dev/ada/septum/src/common/sp-file_system.adb");
      SP.Help.Plain("    D:/dev/ada/septum/src/common/sp-filters.adb");
      SP.Help.Plain("    D:/dev/ada/septum/src/common/sp-interactive.adb");
      SP.Help.Plain("    D:/dev/ada/septum/src/common/sp-output.adb");
      SP.Help.Plain("    D:/dev/ada/septum/src/common/sp-searches.adb");
      SP.Help.Plain("    D:/dev/ada/septum/src/common/sp-strings.adb");
      SP.Help.Plain("    D:/dev/ada/septum/src/common/sp-strings.ads");
      SP.Help.Plain("    D:/dev/ada/septum/src/common/sp.ads");
      SP.Help.Plain("    D:/dev/ada/septum/src/entry/make_septum_help.adb");
      SP.Help.Plain("");

      SP.Help.Plain("    Matching files: 14");
      SP.Help.Plain("");

      SP.Help.Block("We can abbreviate commands with unambiguous prefixes, so  let's shorten `match-contexts` to `match-c`.  ");

      SP.Help.Plain("    > match-c");
      SP.Help.Plain("");

      SP.Help.Plain("    Matching contexts:  653");
      SP.Help.Plain("    Matching files: 111");
      SP.Help.Plain("");

   end About;
   pragma Style_Checks(On);

   pragma Style_Checks(Off);
   procedure Usage is
   begin
      SP.Help.Plain("");

      SP.Help.Block("Septum is meant to stay open in a terminal or tmux tab while you iterate. On startup it runs command scripts from the project-local `.septum/config` and from the global septum config directory when present, unless you opt out with `--no-config`.  ");

      SP.Help.Block("Create a starter local config with `septum init`. In config and script files, blank lines and lines whose first non-empty character starts a `#` comment are ignored, so you can document shared setups.  ");

      SP.Help.Block("Partial command matching applies everywhere: type a unique prefix and Septum resolves it to the full command name before running. Ambiguous prefixes are rejected rather than guessed. That same resolution is what lets abbreviated match and filter commands feel short in daily use.  ");

      SP.Help.Block("An example session might look like this:  ");

      SP.Help.Plain("    find-t alloc");
      SP.Help.Plain("    exclude-t malloc alloca");
      SP.Help.Plain("    match-f");
      SP.Help.Plain("    match-c");
      SP.Help.Plain("");

      SP.Help.Block("`help` with no arguments prints a one-line summary of every command. `help` with a command name prints that command's longer description.  ");

      SP.Help.Plain("    help find-like");
      SP.Help.Plain("");

      SP.Help.Block("`run` reads one or more script files and executes each non-comment line as if typed interactively. Nested `run` of a file already on the script stack is refused to prevent recursion loops. From outside the REPL, septum run can drive the same scripts for batch jobs and pipelines; tool-oriented modes can emit structured results for match commands.  ");

      SP.Help.Plain("    run load_alt_project.septum");
      SP.Help.Plain("");

      SP.Help.Block("`source` remains as a deprecated alias for `run` so older configs keep working. Prefer `run` in new scripts.  ");

      SP.Help.Block("`quit` and `exit` end the interactive session. They do not write the cache or filters back to disk; put durable defaults in config files via `run` lines such as `enable-auto-search`, `set-max-results`, and `add-dirs` if you want them every launch.  ");

   end Usage;
   pragma Style_Checks(On);

   pragma Style_Checks(Off);
   procedure Line_Filters is
   begin
      SP.Help.Plain("");

      SP.Help.Block("Septum searches multi-line neighborhoods called contexts, not single isolated lines. You build a search by stacking filters: find filters require terms to appear somewhere in a context, and exclude filters drop any context that still contains an unwanted term. Filters are applied in order, and each new find filter only keeps contexts that also satisfy earlier find filters where their neighborhoods overlap. That lets terms appear in any order and across line breaks, which is hard to express in ordinary line-oriented tools. Exclude filters override find filters on the same line.  ");

      SP.Help.Block("Each space-separated argument becomes its own filter entry. You can add several terms at once and later remove or reorder just one of them. When auto-search is off, filter commands only update the filter stack; you run `match-contexts` or `match-files` to see the effect. With auto-search on, each filter change triggers a search and prints result counts.  ");

      SP.Help.Block("`find-text` adds a case-sensitive substring filter. Every argument is a separate keep filter that contexts must still satisfy after merging.  ");

      SP.Help.Plain("    > find-text malloc new unique_ptr");
      SP.Help.Plain("");

      SP.Help.Block("`find-like` works like `find-text` but matches case-insensitively, which is often enough when casing varies across the codebase.  ");

      SP.Help.Plain("    > find-like make");
      SP.Help.Plain("");

      SP.Help.Block("`exclude-text` adds a case-sensitive exclusion. Any context whose line range still contains a matching line is discarded entirely, even if that line is only in the surrounding neighborhood of another match.  ");

      SP.Help.Plain("    > exclude-text alloca alloc");
      SP.Help.Plain("");

      SP.Help.Block("`exclude-like` is the case-insensitive form of `exclude-text`. Use it to peel away overloaded names, generated helpers, or other contexts that share a term but are not what you want.  ");

      SP.Help.Plain("    > exclude-like Destroy");
      SP.Help.Plain("");

      SP.Help.Block("`find-regex` keeps contexts that match a regular expression. Invalid patterns fail that filter rather than being accepted silently.  ");

      SP.Help.Block("`exclude-regex` excludes contexts that match a regular expression, using the same full-context exclusion rule as the text excludes.  ");

      SP.Help.Block("`list-line-filters` prints the current line-filter stack in order, with indices matching what `drop` and `reorder` expect.  ");

      SP.Help.Block("`pop` removes only the most recently applied line filter. It is the quick undo when the last term was too aggressive or mistyped, as in the common fix-up cycle of apply, inspect counts, then `pop`.  ");

      SP.Help.Block("`drop` removes filters by 1-based index. With no arguments it behaves like `pop`. With several indices it drops those entries, processing from high to low so remaining indices stay valid.  ");

      SP.Help.Block("`reorder` rebuilds the stack from a full list of existing indices. You must list every current index exactly once; use `drop` to delete filters and `reorder` only to rearrange what remains so later `pop` operations hit the filter you intend.  ");

      SP.Help.Block("`clear-line-filters` removes every line filter at once. Path filters and extension filters are left unchanged, so you can start a new content search over the same file subset.  ");

      SP.Help.Block("`test` evaluates each current line filter against a sample line you provide. For every filter it shows whether that line would MATCH a keep filter, EXCLUDE, or neither, then summarizes whether the line would be MATCHED, EXCLUDED, or IGNORED. Use it when the stack is large and a result is mysterious.  ");

      SP.Help.Plain("    > find-like alloc");
      SP.Help.Plain("    > exclude-like alloca malloc");
      SP.Help.Plain("    > test malloc");
      SP.Help.Plain("");

      SP.Help.Plain("    malloc");
      SP.Help.Plain("    [  MATCH  ]    Case Insensitive Match ""ALLOC""");
      SP.Help.Plain("    [         ]    Case Insensitive Match ""ALLOCA""");
      SP.Help.Plain("    [ EXCLUDE ]    Case Insensitive Match ""MALLOC""");
      SP.Help.Plain("");

      SP.Help.Plain("    EXCLUDED");
      SP.Help.Plain("");

      SP.Help.Plain("    > test alloc");
      SP.Help.Plain("");

      SP.Help.Plain("    alloc");
      SP.Help.Plain("    [  MATCH  ]    Case Insensitive Match ""ALLOC""");
      SP.Help.Plain("    [         ]    Case Insensitive Match ""ALLOCA""");
      SP.Help.Plain("    [         ]    Case Insensitive Match ""MALLOC""");
      SP.Help.Plain("");

      SP.Help.Plain("    MATCHED");
      SP.Help.Plain("");

   end Line_Filters;
   pragma Style_Checks(On);

   pragma Style_Checks(Off);
   procedure File_Cache is
   begin
      SP.Help.Plain("");

      SP.Help.Block("Septum loads candidate text into an in-memory file cache and runs searches against that cache rather than rereading the disk for every query. Anecdotally this uses about 100 MB per million lines and can scan on the order of millions of lines per second on a modern laptop, which is why interactive refinement stays responsive on large trees.  ");

      SP.Help.Block("What you load and what you search are related but not identical. Directories and files define the cache. Path and extension filters decide which cached paths participate in the next match. Reloading refreshes content; it does not by itself change your filter stack.  ");

      SP.Help.Block("Septum currently doesn't track updates to files or loaded directories.  ");

      SP.Help.Block("`add-dirs` is the main way to grow the cache. It walks each directory recursively and loads files that look like text: common source extensions are accepted, known binary extensions are skipped, and unknown types are accepted only if the first 4 KiB contain no null byte. Load progress and failures are reported as each path is processed. Separate directories in the same command with spaces. Quote paths which contain spaces.  ");

      SP.Help.Plain("    > add-dirs D:\dev\ada\septum\src D:\dev\ada\trendy_terminal");
      SP.Help.Plain("    > add-dirs ""C:\Program Files\Example\bin""");
      SP.Help.Plain("");

      SP.Help.Block("`add-files` adds individual paths without treating their parent directory as a recursive search root. Prefer it for log files, single dumps, or a handful of targets you do not want to pull an entire tree for.  ");

      SP.Help.Block("`list-dirs` prints the directory roots currently registered for recursive loading.  ");

      SP.Help.Block("`list-files` prints paths currently in the search pool. By default output is capped so huge trees stay readable; pass a positive number to choose the cap, or full to print every path.  ");

      SP.Help.Block("`clear-dirs` removes all recursive roots and the files discovered through them. Files that entered the cache only via `add-files` remain.  ");

      SP.Help.Block("`clear-files` removes only those directly added files. Files discovered under `add-dirs` stay loaded.  ");

      SP.Help.Block("`stats` reports how many files, lines, and characters the cache currently holds. Check it after large loads or before `unload` when memory pressure is a concern.  ");

      SP.Help.Plain("    > stats");
      SP.Help.Plain("");

      SP.Help.Plain("    Files:       163128");
      SP.Help.Plain("    Lines:       40005234");
      SP.Help.Plain("    Characters:  1593398756");
      SP.Help.Plain("");

      SP.Help.Block("The cache does not watch the filesystem. After edits, rebases, or generated-file updates, run `reload` to reread every currently loaded path from disk while keeping the same roots and direct-file list.  ");

      SP.Help.Block("`unload` empties the in-memory text without exiting the session. Use it when a huge load is contending with other work, then `reload` when you need search again. Unloading does not forget your filter stack or settings.  ");

      SP.Help.Plain("    ... drop file cache to do something memory intensive");
      SP.Help.Plain("    > unload");
      SP.Help.Plain("    ... need to search again");
      SP.Help.Plain("    > reload");
      SP.Help.Plain("");

   end File_Cache;
   pragma Style_Checks(On);

   pragma Style_Checks(Off);
   procedure Path_Filters is
   begin
      SP.Help.Plain("");

      SP.Help.Block("Path filters shrink the set of cached files considered for matching. They do not unload files from memory; they only decide eligibility at search time. Extension filters work the same way and can be combined with path fragment filters.  ");

      SP.Help.Block("With no find-path filters active, every cached file whose extension is allowed is a candidate, and `exclude-path` is enough to carve out noise such as dependency or build trees. Once any `find-path` filter exists, the default flips: a file must match at least one find-path filter to stay in, then exclude-path filters can still drop it. Matching is against the full path string as stored in the cache, so directory names, separators, and file names all participate as ordinary text fragments.  ");

      SP.Help.Block("`find-path` keeps only paths containing the given fragments. Each argument becomes its own path keep filter. After the first find-path, files outside those fragments stop appearing in `match-contexts` and `match-files` even though they remain cached.  ");

      SP.Help.Block("`exclude-path` drops paths containing the given fragments. A single well-chosen segment often removes an entire subtree. When both keep and exclude path filters match, exclusion wins for that path.  ");

      SP.Help.Block("`list-path-filters` prints the path-filter stack currently in effect.  ");

      SP.Help.Block("`clear-path-filters` removes all path keep and exclude filters so every extension-eligible cached file is a candidate again.  ");

      SP.Help.Block("`only-exts` restricts candidates to files with the listed extensions. Extensions are compared without inventing a leading dot if you omit it the same way the path is stored; list what `list-exts` shows after you set them. Until you call `only-exts`, extension filtering is open and all extensions are eligible.  ");

      SP.Help.Block("`remove-exts` removes individual extensions from the allow-list built by `only-exts`.  ");

      SP.Help.Block("`list-exts` shows the current extension allow-list. An empty list means no extension restriction is active.  ");

      SP.Help.Block("`clear-exts` clears the extension allow-list entirely, restoring the default that every extension may participate.  ");

      SP.Help.Block("A practical workflow is to load broadly with `add-dirs`, confirm size with `stats` and `list-files`, then use `exclude-path` or `find-path` and `only-exts` until `match-files` shows a plausible set before refining line filters.  ");

   end Path_Filters;
   pragma Style_Checks(On);

   pragma Style_Checks(Off);
   procedure Results is
   begin
      SP.Help.Plain("");

      SP.Help.Block("A result is a context: a contiguous line range in one file that contains the merged matches of your find filters and none of your exclude hits. Context width controls how many lines above and below each matching line are pulled into that neighborhood. The default width is several lines so nearby declarations stay visible; width 0 collapses each hit toward a single line and approximates an interactive grep. Omitting the argument to `set-context-width` removes the width restriction so neighborhoods can grow to the whole file when needed.  ");

      SP.Help.Block("Multiple find filters do not require all terms on one line. Each keep filter produces candidate neighborhoods; only overlapping neighborhoods are merged into a smaller shared window that still contains every required match. Overlapping or nested contexts are then deduplicated, preferring the larger enclosing range so the list stays readable. Lines that actually matched a keep filter are marked in the interactive display with an arrow prefix.  ");

      SP.Help.Block("`match-contexts` runs the full search and prints contexts. With no arguments it prints up to the current max-results limit. One number N prints the first N contexts. Two numbers M and N print the inclusive slice from M through N, which is useful when paging through a long hit list. After printing, the interactive UI reports Matching contexts and Matching files totals so you can watch the search tighten as filters change.  ");

      SP.Help.Block("`match-files` lists each distinct file that still has at least one matching context, without printing the line bodies. Use it to judge whether path or extension filters should be applied before diving into large context dumps.  ");

      SP.Help.Block("Command names resolve by unique prefix, so short forms such as match-c often expand to `match-contexts` after a Resolved to message.  ");

      SP.Help.Block("`set-context-width` sets the neighborhood size in lines above and below each match. Larger widths find terms that are farther apart but also make exclusions more powerful, because any excluded line inside the wider window kills the whole context.  ");

      SP.Help.Block("`set-max-results` caps how many contexts are printed before the rest are omitted. The totals still reflect the full match set, so you can keep output short while watching counts fall. Omit the argument to remove the print cap.  ");

      SP.Help.Block("`enable-auto-search` reruns a context search after filter-changing commands and prints results using the current max-results setting. That matches the interactive style in the README, where each find or exclude immediately shows new Matching contexts counts.  ");

      SP.Help.Block("`disable-auto-search` restores manual mode, where you call `match-contexts` or `match-files` when you want output. Manual mode is quieter when building long filter stacks or running scripts.  ");

      SP.Help.Block("`enable-line-numbers` prefixes each printed line with its 1-based file line number.  ");

      SP.Help.Block("`disable-line-numbers` hides those numbers for denser output.  ");

      SP.Help.Block("`enable-line-colors` colorizes lines that contain matches when the terminal supports it.  ");

      SP.Help.Block("`disable-line-colors` prints match lines without that color emphasis. Match arrows still mark hit lines either way.  ");

      SP.Help.Block("`enable-timing` prints how long each executed command took, which helps when comparing large cache loads or heavy match runs.  ");

   end Results;
   pragma Style_Checks(On);

end SP.Help_Topics;
