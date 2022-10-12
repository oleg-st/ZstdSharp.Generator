using System.Collections.Generic;

namespace ZstdSharp.Generator.CodeGenerator.Extensions;

internal static class ReadOnlyListExtensions
{
    public static int IndexOf<T>(this IReadOnlyList<T> self, T value)
    {
        var comparer = EqualityComparer<T>.Default;

        for (int i = 0; i < self.Count; i++)
        {
            if (comparer.Equals(self[i], value))
            {
                return i;
            }
        }

        return -1;
    }
}
