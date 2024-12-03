using System.Net.Mime;
using System.Reflection.Emit;
using _2024.Utilities;
using Microsoft.AspNetCore.Mvc;

namespace _2024.Controllers
{
    [ApiController]
    [Route("[controller]")]
    public class Day2 : ControllerBase
    {
        private readonly ILogger<Day2> _logger;

        public Day2(ILogger<Day2> logger)
        {
            _logger = logger;
        }

        private static readonly string[] newlines = ["\r\n", "\r", "\n"];

        private static int UnsafeAtIndex(int prev, IEnumerable<int> remaining, bool increasing, bool dampnerActive, int index = 1)
        {
            int current = remaining.FirstOrDefault(-1);
            if (current == -1) return -1;

            int difference = increasing ? current - prev : prev - current;
            if (3 < difference || difference < 1)
            {
                if (dampnerActive && UnsafeAtIndex(prev, remaining.Skip(1), increasing, false, index + 1) < 0) return -1;
                else return index;
            }
            return UnsafeAtIndex(current, remaining.Skip(1), increasing, dampnerActive, index + 1);
        }

        private static bool IsSafe(IEnumerable<int> levels, Part part = Part.One)
        {
            int first = levels.First();
            IEnumerable<int> remaining = levels.Skip(1);
            bool increasing = remaining.First() > first;
            bool dampnerActive = part == Part.Two;
            int unsafeAtIndex = UnsafeAtIndex(first, remaining, increasing, dampnerActive);

            if (unsafeAtIndex < 0) return true;
            if (!dampnerActive) return false;
            if (unsafeAtIndex < 3)
            {
                if (IsSafe(remaining)) return true;
                remaining = remaining.Skip(1);
                increasing = remaining.First() > first;
                return UnsafeAtIndex(first, remaining, increasing, false) < 0;
            }
            return false;
        }

        [Consumes(MediaTypeNames.Text.Plain)]
        [HttpPost("{part}")]
        public ActionResult<Safety> Post([FromBody] string reports, [FromRoute] Part part)
        {
            if (part == Part.None) return NotFound();

            var lines = reports.Split(newlines, StringSplitOptions.None);
            int safeReports = 0;
            foreach (var line in lines)
            {
                int[] levels = line
                               .Split()
                               .Where(static (string element) => element != "")
                               .Select(s => int.Parse(s))
                               .ToArray();
                if (IsSafe(levels, part)) safeReports++;
                else _logger.LogInformation("Unsafe: {line}", line);
            }
            return new Safety
            {
                Passed = safeReports,
            };
        }
    }

    public class Safety
    {
        public int Passed { get; set; }
    }
}
