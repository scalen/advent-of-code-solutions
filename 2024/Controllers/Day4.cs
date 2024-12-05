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

        [Consumes(MediaTypeNames.Text.Plain)]
        [HttpPost("{part}")]
        public ActionResult<Result> Post([FromBody] string input, [FromRoute] Part part)
        {
            if (part == Part.One || part == Part.None)
            {
                string[] horizontalLines = input.Split(newlines, StringSplitOptions.None).ToArray();
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

                return new Result
                {
                    XMASMatches = matchCount;
                }
            }
            return NotFound();
        }
    }
}
