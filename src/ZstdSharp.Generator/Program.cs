using System;
using System.IO;
using System.Threading.Tasks;
// ReSharper disable StringLiteralTypo

namespace ZstdSharp.Generator;

class Program
{
    static async Task Main(string[] args)
    {
        if (args.Length < 2)
        {
            Console.WriteLine("Usage: ZstdSharp.Generator.exe <ztd/lib location> <out directory>");
            Console.WriteLine("Out directory will be cleaned");
            return;
        }

        await Generate(Path.GetFullPath(args[0]), Path.GetFullPath(args[1]));
    }

    static async Task Generate(string inputLocation, string outputLocation)
    {
        try
        {
            Console.WriteLine($"{inputLocation} -> {outputLocation}");

            var generator = new Generator(inputLocation, outputLocation);
            await generator.Generate();
        }
        catch (Exception exception)
        {
            await Console.Error.WriteLineAsync($"Error: {exception}");
        }
    }
}
