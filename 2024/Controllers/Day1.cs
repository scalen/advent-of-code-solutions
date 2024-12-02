using System.Net.Mime;
using Microsoft.AspNetCore.Mvc;

namespace _2024.Controllers
{
    [ApiController]
    [Route("[controller]")]
    public class Day1 : ControllerBase
    {
        public enum Part
        {
            None = 0,
            One = 1,
        }

        private static string[] newlines = new string[] { "\r\n", "\r", "\n" };

        private readonly ILogger<Day1> _logger;

        public Day1(ILogger<Day1> logger)
        {
            _logger = logger;
        }

        [Consumes(MediaTypeNames.Text.Plain)]
        [HttpPost("{part}")]
        public ActionResult<Distance> Post([FromBody] string locationIds, [FromRoute] Part part)
        {
            if (part == Part.None) return NotFound();

            var lines = locationIds.Split(newlines, StringSplitOptions.None);
            IEnumerable<int> group1 = [], group2 = [];
            foreach (var line in lines)
            {
                string[] ids = line.Split().Where(static (string element) => element != "").ToArray();
                group1 = group1.Append(int.Parse(ids[0]));
                group2 = group2.Append(int.Parse(ids[1]));
            }
            if (part == Part.One) return new Distance
            {
                Total = group1
                        .Order()
                        .Zip(group2.Order(), static (int x, int y) => Math.Abs(x - y))
                        .Sum(),
            };
            else return NotFound();
        }
    }

    public class Distance
    {
        public int Total { get; set; }
    }
}
