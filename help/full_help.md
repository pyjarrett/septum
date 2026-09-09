# About

Septum provides interactive searching of a codebase for blocks of lines which contain the terms you want, and exclude the terms you don't want.

It's different from grep by interactively allowing filters to be added and removed, and focusing on multiple line groups. If you're doing a search looking for something on a specific line, you probably want to use grep (or ripgrep). If you're looking for a block of code with a bunch of terms in those lines and want to whittle it down since they appear in a lot of places, you probably want to use Septum. Using a context width of 0 approximates an interactive grep.

Limiting the search into blocks around search terms allows searching for elements in arbitrary order which may span across lines, in a way which can be difficult to express in other tools. Sometimes terms appear multiple times in a project and have names which change based on context. Septum allows exclusion of these contexts.

Filters get applied in turn with contexts being removed at every step. Similar contexts get deduplicated before presented to cut out more clutter.

## Example:

    > find-like String
    > match-contexts

    D:/dev/ada/dir_iterators/src/dir_iterators-recursive.ads
        57      end record;
        58
        59      -- The list of unprocessed directories needs to be stored.
    ->     60      package String_Vectors is new Ada.Containers.Vectors
        61         (Index_Type   => Positive,
    ->     62          Element_Type => Ada.Strings.Unbounded.Unbounded_String,
    ->     63          "="          => Ada.Strings.Unbounded."=");

    ... other results ...

    Matching contexts:  1063
    Matching files: 118

I don't want Unbounded strings, so exclude those from results.

    > exclude-like Unbounded
    > match-contexts

    D:/dev/ada/septum/src/linux/sp-platform.adb
        11              if Env.Exists ("XDG_CONFIG_HOME") then
    ->     12                  S := SP.Strings.String_Holders.To_Holder (Ada.Directories.Full_Name (Env.Value ("XDG_CONFIG_HOME")));
        13              elsif Env.Exists ("HOME") then
    ->     14                  S := SP.Strings.String_Holders.To_Holder (Ada.Directories.Full_Name (Env.Value ("HOME") & "/.config"));
        15              end if;
        16          end return;
        17      end Global_Config_Dir;

    ... a bunch of other results ...

    Matching contexts:  669
    Matching files: 111

I also got a bunch of results related to String_Holders, SP.Strings,
Ada.Strings, String_Vectors and functions which return Strings, and string
parameters to subprograms but I don't want those either.

Also, it looks like the project has a linux specific folder and some other
projects I don't want, so ignore those in the results.

    > exclude-like Holder SP.Strings String_Vectors ": String" "Ada.Strings" "return string" : String
    > exclude-path linux ada/trendy_test ada/dir_iterators ada/trendy_terminal obj/

    Files:      895              Extensions:   Any
    Path Filters: None

    Distance:   3                Max Results:  50
    Filters:
    1      KEEP : Case Insensitive Match "STRING"
    2          EXCLUDE : Case Insensitive Match "UNBOUNDED"
    3              EXCLUDE : Case Insensitive Match "HOLDER"
    4                  EXCLUDE : Case Insensitive Match "SP.STRINGS"
    5                      EXCLUDE : Case Insensitive Match "STRING_VECTORS"
    6                          EXCLUDE : Case Insensitive Match "RETURN STRING"
    7                              EXCLUDE : Case Insensitive Match "ADA.STRINGS"
    8                                  EXCLUDE : Case Insensitive Match ":"
    9                                      EXCLUDE : Case Insensitive Match "STRING"


Oops looks like I forgot to quote ": String" for parameters, let's redo that.

    > drop 8 9
    > exclude-like ": String"

    Matching contexts:  67
    Matching files: 14

That's more reasonable. What files is it in?

    > match-f
    Resolved to: match-files


    D:/dev/ada/septum/src/common/sp-cache.adb
    D:/dev/ada/septum/src/common/sp-cache.ads
    D:/dev/ada/septum/src/common/sp-commands.adb
    D:/dev/ada/septum/src/common/sp-config.adb
    D:/dev/ada/septum/src/common/sp-config.ads
    D:/dev/ada/septum/src/common/sp-file_system.adb
    D:/dev/ada/septum/src/common/sp-filters.adb
    D:/dev/ada/septum/src/common/sp-interactive.adb
    D:/dev/ada/septum/src/common/sp-output.adb
    D:/dev/ada/septum/src/common/sp-searches.adb
    D:/dev/ada/septum/src/common/sp-strings.adb
    D:/dev/ada/septum/src/common/sp-strings.ads
    D:/dev/ada/septum/src/common/sp.ads
    D:/dev/ada/septum/src/entry/make_septum_help.adb

    Matching files: 14

We can abbreviate commands with unambiguous prefixes, so
let's shorten `match-contexts` to `match-c`.

    > match-c

Matching contexts:  653
Matching files: 111

# Usage

Septum is meant to stay open in a terminal or tmux tab while you iterate. On startup it runs command scripts from the project-local `.septum/config` and from the global septum config directory when present, unless you opt out with `--no-config`.

Create a starter local config with `septum init`. In config and script files, blank lines and lines whose first non-empty character starts a `#` comment are ignored, so you can document shared setups.

Partial command matching applies everywhere: type a unique prefix and Septum resolves it to the full command name before running. Ambiguous prefixes are rejected rather than guessed. That same resolution is what lets abbreviated match and filter commands feel short in daily use.

An example session might look like this:

    find-t alloc
    exclude-t malloc alloca
    match-f
    match-c

`help` with no arguments prints a one-line summary of every command. `help` with a command name prints that command's longer description.

    help find-like

`run` reads one or more script files and executes each non-comment line as if typed interactively. Nested `run` of a file already on the script stack is refused to prevent recursion loops. From outside the REPL, septum run can drive the same scripts for batch jobs and pipelines; tool-oriented modes can emit structured results for match commands.

    run load_alt_project.septum

`source` remains as a deprecated alias for `run` so older configs keep working. Prefer `run` in new scripts.

`quit` and `exit` end the interactive session. They do not write the cache or filters back to disk; put durable defaults in config files via `run` lines such as `enable-auto-search`, `set-max-results`, and `add-dirs` if you want them every launch.

# Line Filters

Septum searches multi-line neighborhoods called contexts, not single isolated lines. You build a search by stacking filters: find filters require terms to appear somewhere in a context, and exclude filters drop any context that still contains an unwanted term. Filters are applied in order, and each new find filter only keeps contexts that also satisfy earlier find filters where their neighborhoods overlap. That lets terms appear in any order and across line breaks, which is hard to express in ordinary line-oriented tools.

Because each space-separated argument becomes its own filter entry, you can add several terms at once and later remove or reorder just one of them. When auto-search is off, filter commands only update the filter stack; you run `match-contexts` or `match-files` to see the effect. With auto-search on, each filter change triggers a search and prints result counts.

`find-text` adds a case-sensitive substring filter. Every argument is a separate keep filter that contexts must still satisfy after merging.

    > find-text malloc new unique_ptr

`find-like` works like `find-text` but matches case-insensitively, which is often enough when casing varies across the codebase.

    > find-like make

`exclude-text` adds a case-sensitive exclusion. Any context whose line range still contains a matching line is discarded entirely, even if that line is only in the surrounding neighborhood of another match.

    > exclude-text alloca alloc

`exclude-like` is the case-insensitive form of `exclude-text`. Use it to peel away overloaded names, generated helpers, or other contexts that share a term but are not what you want.

    > exclude-like Destroy

`find-regex` keeps contexts that match a regular expression. Invalid patterns fail that filter rather than being accepted silently.

`exclude-regex` excludes contexts that match a regular expression, using the same full-context exclusion rule as the text excludes.

`list-line-filters` prints the current line-filter stack in order, with indices matching what `drop` and `reorder` expect.

`pop` removes only the most recently applied line filter. It is the quick undo when the last term was too aggressive or mistyped, as in the common fix-up cycle of apply, inspect counts, then `pop`.

`drop` removes filters by 1-based index. With no arguments it behaves like `pop`. With several indices it drops those entries, processing from high to low so remaining indices stay valid.

`reorder` rebuilds the stack from a full list of existing indices. You must list every current index exactly once; use `drop` to delete filters and `reorder` only to rearrange what remains so later `pop` operations hit the filter you intend.

`clear-line-filters` removes every line filter at once. Path filters and extension filters are left unchanged, so you can start a new content search over the same file subset.

`test` evaluates each current line filter against a sample line you provide. For every filter it shows whether that line would MATCH a keep filter, EXCLUDE, or neither, then summarizes whether the line would be MATCHED, EXCLUDED, or IGNORED. Use it when the stack is large and a result is mysterious.

    > find-like alloc
    > exclude-like alloca malloc
    > test malloc

    malloc
    [  MATCH  ]    Case Insensitive Match "ALLOC"
    [         ]    Case Insensitive Match "ALLOCA"
    [ EXCLUDE ]    Case Insensitive Match "MALLOC"

    EXCLUDED

    > test alloc

    alloc
    [  MATCH  ]    Case Insensitive Match "ALLOC"
    [         ]    Case Insensitive Match "ALLOCA"
    [         ]    Case Insensitive Match "MALLOC"

    MATCHED


# File Cache

Septum loads candidate text into an in-memory file cache and runs searches against that cache rather than rereading the disk for every query. Anecdotally this uses about 100 MB per million lines and can scan on the order of millions of lines per second on a modern laptop, which is why interactive refinement stays responsive on large trees.

What you load and what you search are related but not identical. Directories and files define the cache. Path and extension filters decide which cached paths participate in the next match. Reloading refreshes content; it does not by itself change your filter stack.

`add-dirs` is the main way to grow the cache. It walks each directory recursively and loads files that look like text: common source extensions are accepted, known binary extensions are skipped, and unknown types are accepted only if the first 4 KiB contain no null byte. Load progress and failures are reported as each path is processed. Separate directories in the same command with spaces. Quote paths which contain spaces.

    add-dirs D:\dev\ada\septum\src D:\dev\ada\trendy_terminal
    add-dirs "C:\Program Files\Example\bin"

`add-files` adds individual paths without treating their parent directory as a recursive search root. Prefer it for log files, single dumps, or a handful of targets you do not want to pull an entire tree for.

`list-dirs` prints the directory roots currently registered for recursive loading.

`list-files` prints paths currently in the search pool. By default output is capped so huge trees stay readable; pass a positive number to choose the cap, or full to print every path.

`clear-dirs` removes all recursive roots and the files discovered through them. Files that entered the cache only via `add-files` remain.

`clear-files` removes only those directly added files. Files discovered under `add-dirs` stay loaded.

`stats` reports how many files, lines, and characters the cache currently holds. Check it after large loads or before `unload` when memory pressure is a concern.

    > stats

    Files:       163128
    Lines:       40005234
    Characters:  1593398756

The cache does not watch the filesystem. After edits, rebases, or generated-file updates, run `reload` to reread every currently loaded path from disk while keeping the same roots and direct-file list.

`unload` empties the in-memory text without exiting the session. Use it when a huge load is contending with other work, then `reload` when you need search again. Unloading does not forget your filter stack or settings.

    ... drop file cache to do something memory intensive
    > unload
    ... need to search again
    > reload

# Path Filters

Path filters shrink the set of cached files considered for matching. They do not unload files from memory; they only decide eligibility at search time. Extension filters work the same way and can be combined with path fragment filters.

With no find-path filters active, every cached file whose extension is allowed is a candidate, and `exclude-path` is enough to carve out noise such as dependency or build trees. Once any `find-path` filter exists, the default flips: a file must match at least one find-path filter to stay in, then exclude-path filters can still drop it. Matching is against the full path string as stored in the cache, so directory names, separators, and file names all participate as ordinary text fragments.

`find-path` keeps only paths containing the given fragments. Each argument becomes its own path keep filter. After the first find-path, files outside those fragments stop appearing in `match-contexts` and `match-files` even though they remain cached.

`exclude-path` drops paths containing the given fragments. A single well-chosen segment often removes an entire subtree. When both keep and exclude path filters match, exclusion wins for that path.

`list-path-filters` prints the path-filter stack currently in effect.

`clear-path-filters` removes all path keep and exclude filters so every extension-eligible cached file is a candidate again.

`only-exts` restricts candidates to files with the listed extensions. Extensions are compared without inventing a leading dot if you omit it the same way the path is stored; list what `list-exts` shows after you set them. Until you call `only-exts`, extension filtering is open and all extensions are eligible.

`remove-exts` removes individual extensions from the allow-list built by `only-exts`.

`list-exts` shows the current extension allow-list. An empty list means no extension restriction is active.

`clear-exts` clears the extension allow-list entirely, restoring the default that every extension may participate.

A practical workflow is to load broadly with `add-dirs`, confirm size with `stats` and `list-files`, then use `exclude-path` or `find-path` and `only-exts` until `match-files` shows a plausible set before refining line filters.

# Results

A result is a context: a contiguous line range in one file that contains the merged matches of your find filters and none of your exclude hits. Context width controls how many lines above and below each matching line are pulled into that neighborhood. The default width is several lines so nearby declarations stay visible; width 0 collapses each hit toward a single line and approximates an interactive grep. Omitting the argument to `set-context-width` removes the width restriction so neighborhoods can grow to the whole file when needed.

Multiple find filters do not require all terms on one line. Each keep filter produces candidate neighborhoods; only overlapping neighborhoods are merged into a smaller shared window that still contains every required match. Overlapping or nested contexts are then deduplicated, preferring the larger enclosing range so the list stays readable. Lines that actually matched a keep filter are marked in the interactive display with an arrow prefix.

`match-contexts` runs the full search and prints contexts. With no arguments it prints up to the current max-results limit. One number N prints the first N contexts. Two numbers M and N print the inclusive slice from M through N, which is useful when paging through a long hit list. After printing, the interactive UI reports Matching contexts and Matching files totals so you can watch the search tighten as filters change.

`match-files` lists each distinct file that still has at least one matching context, without printing the line bodies. Use it to judge whether path or extension filters should be applied before diving into large context dumps.

Command names resolve by unique prefix, so short forms such as match-c often expand to `match-contexts` after a Resolved to message.

`set-context-width` sets the neighborhood size in lines above and below each match. Larger widths find terms that are farther apart but also make exclusions more powerful, because any excluded line inside the wider window kills the whole context.

`set-max-results` caps how many contexts are printed before the rest are omitted. The totals still reflect the full match set, so you can keep output short while watching counts fall. Omit the argument to remove the print cap.

`enable-auto-search` reruns a context search after filter-changing commands and prints results using the current max-results setting. That matches the interactive style in the README, where each find or exclude immediately shows new Matching contexts counts.

`disable-auto-search` restores manual mode, where you call `match-contexts` or `match-files` when you want output. Manual mode is quieter when building long filter stacks or running scripts.

`enable-line-numbers` prefixes each printed line with its 1-based file line number.

`disable-line-numbers` hides those numbers for denser output.

`enable-line-colors` colorizes lines that contain matches when the terminal supports it.

`disable-line-colors` prints match lines without that color emphasis. Match arrows still mark hit lines either way.

`enable-timing` prints how long each executed command took, which helps when comparing large cache loads or heavy match runs.

`disable-timing` stops those duration lines.
