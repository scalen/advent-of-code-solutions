using System.Net.Mime;
using _2024.Utilities;
using Microsoft.AspNetCore.Mvc;

namespace _2024.Controllers
{
    [ApiController]
    [Route("[controller]")]
    public class Day5 : ControllerBase
    {
        public class ManualResult
        {
            public int SumOfValidUpdateMiddlePages { get; set; }
        }

        private static readonly string[] newlines = ["\r\n", "\r", "\n"];

        private static List<string> ValidUpdate(string[] update, List<string> preceding, Dictionary<string, HashSet<string>> mustPrecede, bool shouldFix, bool wasFixed = false)
        {
            if (!update.Any()) return wasFixed || !shouldFix ? preceding : [];
            string firstPage = update.First();
            string[] following = update.Skip(1).ToArray();
            IEnumerable<string> mustPrecedeFirst = mustPrecede.GetValueOrDefault(firstPage, []).Intersect(following);
            if (mustPrecedeFirst.Any())
            {
                if (shouldFix)
                {
                    int lastIndex = mustPrecedeFirst.Select((page) =>
                    {
                        for (int i = 1; i < update.Length; i++)
                        {
                            if (update[i] == page) return i;
                        }
                        return 0;
                    }).Max();
                    for (int i = 1; i <= lastIndex; i++) update[i-1] = update[i];
                    update[lastIndex] = firstPage;
                    return ValidUpdate(update, preceding, mustPrecede, shouldFix, true);
                } else return [];
            }
            preceding.Add(firstPage);
            return ValidUpdate(following, preceding, mustPrecede, shouldFix, wasFixed);
        }

        [Consumes(MediaTypeNames.Text.Plain)]
        [HttpPost("{part}")]
        public ActionResult<ManualResult> Post([FromBody] string input, [FromRoute] Part part)
        {
            if (part == Part.None) return NotFound();

            Dictionary<string, HashSet<string>> mustPrecede = new();
            var lines = input.Split(newlines, StringSplitOptions.TrimEntries).GetEnumerator();
            while (lines.MoveNext())
            {
                string line = (string)lines.Current;
                if (line == "") break;

                string[] pair = line.Split('|').ToArray();
                HashSet<string> preceding = mustPrecede.GetValueOrDefault(pair[1], []);
                preceding.Add(pair[0]);
                mustPrecede[pair[1]] = preceding;
            }
            int sumOfValidUpdateMiddlePages = 0;
            while (lines.MoveNext())
            {
                string[] line = ((string)lines.Current).Split(',', StringSplitOptions.TrimEntries).ToArray();

                List<string> update = ValidUpdate(line, [], mustPrecede, part == Part.Two);
                if (update.Any()) sumOfValidUpdateMiddlePages += int.Parse(update[update.Count / 2]);
            }
            return new ManualResult
            {
                SumOfValidUpdateMiddlePages = sumOfValidUpdateMiddlePages,
            };
        }
    }
}
