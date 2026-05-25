using System;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

// https://stackoverflow.com/questions/1788279/redirecting-stdin-and-stdout-in-net
// https://stackoverflow.com/questions/34485368/how-to-interact-with-a-process-through-stdout-and-stdin
string RunProcess(string fileName, params string[] args)
{
    var argsArray = string.Join(" ", args);
    Console.WriteLine();
    Console.ForegroundColor = ConsoleColor.Magenta;
    Console.WriteLine($" > {fileName} {argsArray}");
    Console.ResetColor();
    Console.WriteLine();
    using (var run = new Process())
    {
        run.StartInfo.FileName = fileName;
        run.StartInfo.Arguments = argsArray;
        run.StartInfo.RedirectStandardError = true;
        run.StartInfo.RedirectStandardOutput = true;
        run.StartInfo.UseShellExecute = false;

        var outputBuilder = new StringBuilder();
        var errorBuilder = new StringBuilder();
        var syncLock = new object();

        run.OutputDataReceived += (sender, e) =>
        {
            if (e.Data != null)
            {
                lock (syncLock)
                {
                    Console.WriteLine(e.Data);
                    outputBuilder.AppendLine(e.Data);
                }
            }
        };
        run.ErrorDataReceived += (sender, e) =>
        {
            if (e.Data != null)
            {
                lock (syncLock)
                {
                    Console.Error.WriteLine(e.Data);
                    errorBuilder.AppendLine(e.Data);
                }
            }
        };

        if (!run.Start())
        {
            throw new Exception($"Unable to run program \"{fileName}\"");
        }

        run.BeginOutputReadLine();
        run.BeginErrorReadLine();

        run.WaitForExit();

        if (run.ExitCode != 0)
        {
            throw new Exception($"Failed running {fileName}");
        }

        return outputBuilder.ToString();
    }
}

const string expectedBBTVersion = "0.2.1";

bool IsAlrToolInstalled(string toolName, string toolVersion)
{
    string toolNameVersion = toolName + "=" + toolVersion;
    string alrOutputRaw = RunProcess("alr", "install", "--info");

    var lines = alrOutputRaw.Split(new[] { "\n", "\r\n" }, StringSplitOptions.RemoveEmptyEntries);
    return lines.Any(line => line.Trim().StartsWith(toolNameVersion));
}

if (!File.Exists("septum.gpr") || !File.Exists("alire.toml"))
{
    Console.Error.WriteLine("Error: This script must be run from the root directory of the septum project.");
    Environment.Exit(1);
}

if (!IsAlrToolInstalled("bbt", expectedBBTVersion))
{
    Console.WriteLine("Installing BBT from Alire");
    RunProcess("alr", "--force", "install", "bbt^" + expectedBBTVersion);
}
else
{
    Console.WriteLine("BBT already installed");
}

RunProcess("alr", "build");

string septumPath = "bin/septum";
if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
{
    septumPath = "bin/septum.exe";
}

RunProcess(septumPath, "version");

RunProcess(
    "bbt",
    "--exec_dir",
    ".",
    "--recursive",
    "docs/tests",
    "--verbose",
    "--strict",
    "--keep_going",
    "--tmp_dir",
    "bbt_out",
    "--output",
    "integration_test_results.md"
);

Directory.SetCurrentDirectory ("tests");
RunProcess("alr", "run");
