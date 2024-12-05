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

        private static List<string> ValidUpdate(string[] update, List<string> preceding, Dictionary<string, HashSet<string>> mustPrecede)
        {
            if (!update.Any()) return preceding;
            string first = update.First();
            string[] following = update.Skip(1).ToArray();
            if (mustPrecede.GetValueOrDefault(first, []).Intersect(following).Any()) return [];
            preceding.Add(first);
            return ValidUpdate(following, preceding, mustPrecede);
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
            if (part == Part.One)
            {
                while (lines.MoveNext())
                {
                    string[] line = ((string)lines.Current).Split(',', StringSplitOptions.TrimEntries).ToArray();

                    List<string> update = ValidUpdate(line, [], mustPrecede);
                    if (update.Any()) sumOfValidUpdateMiddlePages += int.Parse(update[update.Count / 2]);
                }
                return new ManualResult
                {
                    SumOfValidUpdateMiddlePages = sumOfValidUpdateMiddlePages,
                };
            }
            return NotFound();
        }
    }
}
