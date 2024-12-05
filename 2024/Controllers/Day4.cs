using _2024.Utilities;
using System.Net.Mime;
using Microsoft.AspNetCore.Mvc;
using System.Text.RegularExpressions;
using System.Text;

namespace _2024.Controllers
{
    [ApiController]
    [Route("[controller]")]
    public partial class Day4 : ControllerBase
    {
        private static readonly string[] newlines = ["\r\n", "\r", "\n"];

        [GeneratedRegex("XMAS")]
        private static partial Regex Xmas();

        public class Result
        {
            public int XMASMatches { get; set; }
            public int XedMASMatches { get; set; }
        }

        private static string GetVerticalLine(int size, int index, IEnumerable<string> hLines)
        {
            return new StringBuilder(size)
                       .Append(hLines.Select(hLine => hLine[index]).ToArray())
                       .ToString();
        }

        private static string GetDiagonalLine(int size, int index, IEnumerable<string> lines)
        {
            return new StringBuilder(size)
                       .Append(lines.Take(size).Select(line => line[index++]).ToArray())
                       .ToString();
        }

        private static string[] GetDiagonalLines(int shortLineLength, int longLineLength, string[] longLines)
        {
            int fullDiagonalsCount = longLineLength - shortLineLength + 1;
            string[] diagonalLines = new string[longLineLength + shortLineLength - 7];
            for (int startIndex = 0; startIndex < fullDiagonalsCount; startIndex++)
            {
                diagonalLines[startIndex + shortLineLength - 4] = GetDiagonalLine(shortLineLength, startIndex, longLines);
            }
            for (int startIndex = fullDiagonalsCount; startIndex < longLineLength - 3; startIndex++)
            {
                diagonalLines[startIndex + shortLineLength - 4] = GetDiagonalLine(longLineLength - startIndex, startIndex, longLines);
            }
            IEnumerable<string> remaining = longLines;
            for (int i = 0; i < shortLineLength - 4; i++)
            {
                remaining = remaining.Skip(1);
                diagonalLines[i] = GetDiagonalLine(shortLineLength - i + 1, 0, remaining);
            }
            return diagonalLines;
        }

        private int CountXMASs(string line) => Xmas().Matches(line).Count + Xmas().Matches(String.Concat(line.Reverse())).Count;

        private bool IsDiagonalMAS(int lineOffset, int charOffset, string[] lines, bool forward)
        {
            int slant = forward ? 1 : -1;
            return lines[lineOffset][charOffset] == 'A'
                && (
                    (
                        lines[lineOffset - 1][charOffset - slant] == 'M'
                     && lines[lineOffset + 1][charOffset + slant] == 'S'
                    )||(
                        lines[lineOffset - 1][charOffset - slant] == 'S'
                     && lines[lineOffset + 1][charOffset + slant] == 'M'
                    )
                   );
        }

        [Consumes(MediaTypeNames.Text.Plain)]
        [HttpPost("{part}")]
        public ActionResult<Result> Post([FromBody] string input, [FromRoute] Part part)
        {
            string[] horizontalLines = input.Split(newlines, StringSplitOptions.None).ToArray();
            Result result = new();

            if (part == Part.One || part == Part.None)
            {
                int matchCount = horizontalLines.Select(CountXMASs).Sum();

                int verticalLineLength = horizontalLines.Count();
                int horizontalLineLength = horizontalLines.First().Length;
                IEnumerable<string> verticalLines = Enumerable.Range(0, horizontalLineLength)
                                                    .Select((index) => GetVerticalLine(verticalLineLength, index, horizontalLines));
                string[] longLines;
                int shortLineLength;
                int longLineLength;
                if (verticalLineLength < horizontalLineLength)
                {
                    longLines = horizontalLines;
                    shortLineLength = verticalLineLength;
                    longLineLength = horizontalLineLength;
                    matchCount += verticalLines.Select(CountXMASs).Sum();
                }
                else
                {
                    longLines = verticalLines.ToArray();
                    shortLineLength = horizontalLineLength;
                    longLineLength = verticalLineLength;
                    matchCount += longLines.Select(CountXMASs).Sum();
                }

                shortLineLength = Math.Min(verticalLineLength, horizontalLineLength);
                longLineLength = Math.Max(verticalLineLength, horizontalLineLength);
                matchCount += GetDiagonalLines(shortLineLength, longLineLength, longLines).Select(CountXMASs).Sum();
                matchCount += GetDiagonalLines(shortLineLength, longLineLength, longLines.Reverse().ToArray()).Select(CountXMASs).Sum();

                result.XMASMatches = matchCount;
            }

            if (part == Part.Two || part == Part.None)
            {
                int xCount = 0;
                for (int l = 1; l < horizontalLines.Length - 1; l++) for (int c = 1; c < horizontalLines[0].Length - 1; c++)
                {
                    if (IsDiagonalMAS(l, c, horizontalLines, true) && IsDiagonalMAS(l, c, horizontalLines, false)) xCount++;
                }
                result.XedMASMatches = xCount;
            }
            return result;
        }
    }
}
