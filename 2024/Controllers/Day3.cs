using _2024.Utilities;
using System.Net.Mime;
using Microsoft.AspNetCore.Mvc;
using System.Text.RegularExpressions;

namespace _2024.Controllers
{
    [ApiController]
    [Route("[controller]")]
    public partial class Day3 : ControllerBase
    {
        private int SumUncorruptedMultiplys(string program)
        {
            return MulApplication()
                   .Matches(program)
                   .Select(match => int.Parse(match.Groups["arg1"].Value) * int.Parse(match.Groups["arg2"].Value))
                   .Sum();
        }

        [Consumes(MediaTypeNames.Text.Plain)]
        [HttpPost("{part}")]
        public ActionResult<MultiplierProgramResult> Post([FromBody] string program, [FromRoute] Part part)
        {
            if (part == Part.None) return NotFound();

            if (part == Part.One) return new MultiplierProgramResult
            {
                UncorruptedTotal = SumUncorruptedMultiplys(program),
            };
            else return NotFound();
        }

        [GeneratedRegex("mul[(](?<arg1>0|[1-9][0-9]?[0-9]?),(?<arg2>0|[1-9][0-9]?[0-9]?)[)]", RegexOptions.Multiline & RegexOptions.ExplicitCapture)]
        private static partial Regex MulApplication();
    }

    public class MultiplierProgramResult
    {
        public int UncorruptedTotal { get; set; }
    }
}