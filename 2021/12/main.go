package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
	"unicode"
)
type void struct{}
type CaveSet map[*Cave]void
var member void

func (caves CaveSet) Add(cave *Cave) {
	caves[cave] = member
}

func (caves CaveSet) Union(other CaveSet) CaveSet {
	result := make(CaveSet)
	for cave := range caves {
		result[cave] = member
	}
	for cave := range other {
		result[cave] = member
	}
	return result
}

func (caves CaveSet) Subtract(other CaveSet) CaveSet {
	result := make(CaveSet)
	for cave := range caves {
		if _, present := other[cave]; ! present {
			result[cave] = member
		}
	}
	return result
}

type Cave struct {
	Label string
	EdgesTo CaveSet
}

func (cave *Cave) IsSmall() bool {
	return ! unicode.IsUpper([]rune(cave.Label)[0])
}

func (cave *Cave) FindPathCountTo(destLabel string, visited CaveSet) (pathsForward uint) {
	if cave.Label == destLabel { return 1 }
	nowVisited := visited
	if cave.IsSmall() { nowVisited = nowVisited.Union(CaveSet{cave: member}) }

	for next := range cave.EdgesTo.Subtract(nowVisited) {
		pathsForward += next.FindPathCountTo(destLabel, nowVisited)
	}
	return
}

func (cave *Cave) FindPathCountWithOneDoubleTo(destLabel, startLabel string, visited CaveSet, doubleSeen bool) (pathsForward uint) {
	if cave.Label == destLabel { return 1 }
	nowVisited := visited
	if cave.IsSmall() { nowVisited = nowVisited.Union(CaveSet{cave: member}) }

	for next := range cave.EdgesTo {
		if _, alreadyVisited := nowVisited[next]; next.Label != startLabel && ! (alreadyVisited && doubleSeen) {
			pathsForward += next.FindPathCountWithOneDoubleTo(
				destLabel,
				startLabel,
				nowVisited,
				alreadyVisited || doubleSeen,
			)
		}
	}
	return
}

func NewNetworkWithEntrance(edges <-chan string, startLabel string) *Cave {
	seen := make(map[string]*Cave)
	for edgeStr := range edges {
		labels := strings.Split(edgeStr, "-")
		var edge [2]*Cave
		for i, label := range labels {
			if edge[i] = seen[label]; edge[i] == nil {
				seen[label] = &Cave{
					Label: label,
					EdgesTo: make(CaveSet),
				}
				edge[i] = seen[label]
			}
		}
		edge[0].EdgesTo.Add(edge[1])
		edge[1].EdgesTo.Add(edge[0])
	}
	return seen[startLabel]
}

func partTwo(input <-chan string, solutions chan<- string) {
	defer close(solutions)

	start := NewNetworkWithEntrance(input, "start")
	pathCount := start.FindPathCountWithOneDoubleTo("end", "start", make(CaveSet), false)

	solutions <- fmt.Sprintf("Part 2 Paths: %d", pathCount)
}

func partOne(input <-chan string, solutions chan<- string) {
	defer close(solutions)

	start := NewNetworkWithEntrance(input, "start")
	pathCount := start.FindPathCountTo("end", make(CaveSet))

	solutions <- fmt.Sprintf("Part 1 Paths: %d", pathCount)
}

func main() {
	parts := []func(<-chan string, chan<- string) {
		partOne,
		partTwo,
	}

	instructionsChannels := make([]chan string, len(parts))
	for i := range instructionsChannels {
		instructionsChannels[i] = make(chan string, 100)
	}
	solutionChannels := make([]chan string, len(parts))
	for i := range instructionsChannels {
		solutionChannels[i] = make(chan string)
	}
	for i, part := range parts {
		go part(instructionsChannels[i], solutionChannels[i])
	}

	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		instruction := scanner.Text()
		for _, instructions := range instructionsChannels {
			instructions <- instruction
		}
	}
	for _, instructions := range instructionsChannels {
		close(instructions)
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	for _, solutions := range solutionChannels {
		for solution := range solutions {
			fmt.Printf("%s\n", solution)
		}
	}
}
