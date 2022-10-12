using System;
using System.Threading.Tasks;
// ReSharper disable StringLiteralTypo

namespace ZstdSharp.Generator;

class Program
{
    static async Task Main(string[] args)
    {
        if (args.Length == 0)
        {
            Console.WriteLine("Usage: ZstdSharp.Generator.exe <ztd/lib location>");
            return;
        }

        await Generate(args[0]);
    }

    static async Task Generate(string inputLocation)
    {
        try
        {
            var generator = new Generator(inputLocation, "out");
            await generator.Generate();
        }
        catch (Exception exception)
        {
            await Console.Error.WriteLineAsync($"Error: {exception}");
        }
    }
}
