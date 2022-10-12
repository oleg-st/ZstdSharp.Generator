using System.IO;
using System.Threading.Tasks;

namespace ZstdSharp.Generator.CodeGenerator;

internal static class FsHelper
{
    public static async Task CopyFileAsync(string sourceFile, string destinationFile)
    {
        await using var sourceStream = new FileStream(sourceFile, FileMode.Open, FileAccess.Read, FileShare.Read, 4096,
            FileOptions.Asynchronous | FileOptions.SequentialScan);
        await using var destinationStream = new FileStream(destinationFile, FileMode.Create, FileAccess.Write,
            FileShare.None, 4096, FileOptions.Asynchronous | FileOptions.SequentialScan);
        await sourceStream.CopyToAsync(destinationStream);
    }

    public static async Task CopyAll(string sourcePath, string destPath)
    {
        foreach (var fi in new DirectoryInfo(sourcePath).GetFiles())
            await CopyFileAsync(fi.FullName, Path.Combine(destPath, fi.Name));

        foreach (var fi in new DirectoryInfo(sourcePath).GetDirectories())
            await CopyAll(Path.Combine(sourcePath, fi.Name), Path.Combine(destPath, fi.Name));
    }
}
