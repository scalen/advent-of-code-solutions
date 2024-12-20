﻿using System.Net.Mime;
using System.Text;
using _2024.Utilities;
using Microsoft.AspNetCore.Mvc;

namespace _2024.Controllers
{
    [ApiController]
    [Route("[controller]")]
    public class Day6(ILogger<Day6> logger) : ControllerBase
    {
        private readonly ILogger<Day6> _logger = logger;

        public class MapResult
        {
            public int VisitedLocationCount { get; set; }
        }

        private static readonly string[] newlines = ["\r\n", "\r", "\n"];
        private static readonly char[] guardMarkers = ['^', '>', 'v', '<'];
        private static readonly char obstacle = '#';

        private class Guard(int x, int y, char symbol)
        {
            public Coordinates<bool?> Heading { get; private set; } = symbol == '^' ? new(null, true) :
                                                                      symbol == 'v' ? new(null, false) :
                                                                      symbol == '>' ? new(true, null) :
                                                                      new(false, null);
            public Coordinates<int> Location { get; private set; } = new(x, y);
            public char Symbol { get; private set; } = symbol;

            public int VisitedLocationCount { get; private set; } = 1;
            public bool Step(char[,] map, bool markPath)
            {
                var start = Location.Copy();
                if (Heading.X != null) Location.X += (bool)Heading.X ? -1 : 1;
                if (Heading.Y != null) Location.Y += (bool)Heading.Y ? -1 : 1;

                bool hitObstacle;
                try { hitObstacle = map[Location.X, Location.Y] == obstacle; }
                // Out of bounds
                catch (IndexOutOfRangeException) { return false; }
                if (hitObstacle)
                {
                    Location = start;
                    Heading = new Coordinates<bool?>(
                        Heading.X != null ? null : Heading.Y != true,
                        Heading.Y != null ? null : Heading.X == true
                    );
                    switch (Symbol)
                    {
                        case '^': Symbol = '>'; break;
                        case '>': Symbol = 'v'; break;
                        case 'v': Symbol = '<'; break;
                        default:  Symbol = '^'; break;
                    }
                    return true;
                }

                if (!guardMarkers.Contains(map[Location.X, Location.Y]))
                {
                    if (markPath) map[Location.X, Location.Y] = Symbol;
                    ++VisitedLocationCount;
                }
                return Step(map, markPath);
            }
        }

        [Consumes(MediaTypeNames.Text.Plain)]
        [HttpPost("{part}")]
        public ActionResult<MapResult> Post([FromBody] string input, [FromRoute] Part part)
        {
            if (part == Part.None) return NotFound();

            bool markPath = part == Part.One;

            string[] lines = input.Split(newlines, StringSplitOptions.TrimEntries).ToArray();
            char[,] map = new char[lines[0].Length, lines.Length];

            Guard? guard = null;
            for (int x = 0; x < map.GetLength(0); x++) for (int y = 0; y < map.GetLength(1); y++)
            {
                if (guardMarkers.Contains(map[x, y]))
                {
                    guard = new Guard(x, y, map[x, y]);
                    if (markPath) map[x, y] = lines[y][x];
                }
                else map[x, y] = lines[y][x];
            }
            if (guard == null) return NotFound();
            while (guard.Step(map, markPath)) ;

            var mapView = new StringBuilder(map.GetLength(0) * map.GetLength(1));
            for (int y = 0; y < map.GetLength(1); y++) mapView.Append(Enumerable.Range(0, map.GetLength(0)).Select(x => map[x, y]).ToArray()).AppendLine();
            _logger.LogInformation("{}", mapView.ToString());
            return new MapResult
            {
                VisitedLocationCount = guard == null ? 0 : guard.VisitedLocationCount,
            };
        }
    }
}
