package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

func AsToIs(as string) (is []uint8) {
	is = make([]uint8, len(as))
	for x, a := range strings.Split(as, "") {
		i, _ := strconv.Atoi(a)
		is[x] = uint8(i)
	}
	return is
}

type RiskMap [][]uint8

func NewRiskMap(scanLines <-chan string, scaleFactor uint) RiskMap {
	var riskMap RiskMap
	for line := range scanLines {
		riskMap = append(riskMap, AsToIs(line))
	}
	if scaleFactor == 1 { return riskMap }

	scale := int(scaleFactor)
	biggerMap := make(RiskMap, len(riskMap)*scale)
	for x := 0; x < scale; x++ {
		baseX := len(riskMap) * x
		for i := range riskMap {
			biggerMap[baseX + i] = make([]uint8, len(riskMap[i])*scale)
		}
		for y := 0; y < scale; y ++ {
			baseY := len(riskMap[0]) * y
			for subX, line := range riskMap {
				for subY, risk := range line {
					biggerMap[baseX+subX][baseY+subY] = uint8((int(risk) + x + y - 1) % 9 + 1)
				}
			}
		}
	}
	return biggerMap
}

type Coord [2]uint
type Path struct {
	Risk uint
	Length uint
	End Coord
	PreviousStep *Path
}
type PathMap map[Coord]*Path
type PathRiskStack []*Path

func (coord Coord) Neighbours(max Coord) []Coord {
	neighbours := make([]Coord, 0, 4)
	if coord[0] > 0 {
		neighbours = append(neighbours, Coord{coord[0]-1, coord[1]})
	}
	if coord[0] < max[0] {
		neighbours = append(neighbours, Coord{coord[0]+1, coord[1]})
	}
	if coord[1] > 0 {
		neighbours = append(neighbours, Coord{coord[0], coord[1]-1})
	}
	if coord[1] < max[1] {
		neighbours = append(neighbours, Coord{coord[0], coord[1]+1})
	}
	return neighbours
}

func (stack PathRiskStack) Pop() (*Path, PathRiskStack) {
	if len(stack) == 0 { return nil, stack }
	return stack[len(stack)-1], stack[:len(stack)-1]
}

func (stack PathRiskStack) Push(paths ...*Path) PathRiskStack {
	sort.Slice(paths, func(i, j int) bool { return PathIsLess(paths[i], paths[j]) })
	insertAt := len(stack)
	for _, path := range paths {
		for insertAt > 0 && PathIsLess(stack[insertAt-1], path) {
			insertAt--
			if stack[insertAt].End == path.End {
				path = nil
				break
			}
		}
		if path == nil { continue }
		if insertAt == len(stack) {
			stack = append(stack, path)
		} else {
			stack = append(stack[:insertAt+1], stack[insertAt:]...)
			stack[insertAt] = path
		}
	}
	return stack
}

func PathIsLess(a, b *Path) bool {
	return a.Risk < b.Risk || (a.Risk == b.Risk && a.Length < b.Length)
}

func (path *Path) EvaluateNextSteps(pathStack PathRiskStack, pathMap PathMap, riskMap RiskMap, max Coord) (PathRiskStack, PathMap) {
	for _, neighbour := range path.End.Neighbours(max) {
		next := &Path{
			Risk: path.Risk + uint(riskMap[neighbour[0]][neighbour[1]]),
			Length: path.Length + 1,
			End: neighbour,
			PreviousStep: path,
		}
		if existing := pathMap[neighbour]; existing == nil || PathIsLess(next, existing) {
			pathStack = pathStack.Push(next)
			pathMap[neighbour] = next
		}
	}
	return pathStack, pathMap
}

func FindShortestPath(input <-chan string, scaleFactor uint) *Path {
	riskMap := NewRiskMap(input, scaleFactor)
	end := Coord{uint(len(riskMap)-1), uint(len(riskMap[len(riskMap)-1])-1)}
	pathStack := make(PathRiskStack, 0, len(riskMap)*len(riskMap[0]))
	path := &Path{}
	pathMap := PathMap{path.End: path}
	for path.End != end {
		if path == pathMap[path.End] {
			pathStack, pathMap = path.EvaluateNextSteps(pathStack, pathMap, riskMap, end)
		}
		path, pathStack = pathStack.Pop()
	}
	return path
}

func partTwo(input <-chan string, solutions chan<- string) {
	defer close(solutions)

	path := FindShortestPath(input, 5)

	solutions <- fmt.Sprintf("Part 2 Lowest Risk: %d", path.Risk)
}

func partOne(input <-chan string, solutions chan<- string) {
	defer close(solutions)

	path := FindShortestPath(input, 1)

	solutions <- fmt.Sprintf("Part 1 Lowest Risk: %d", path.Risk)
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
