package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

type void struct{}
var member void

func AsToIs(as string) []uint {
	is := make([]uint, len(as))
	for x, r := range as {
		number, _ := strconv.Atoi(string(r))
		is[x] = uint(number)
	}
	return is
}

type Basin struct {
	Size uint
	Edge [2]uint
}

func (basin Basin) Overlaps(other Basin) bool {
	for i := range basin.Edge {
		if (basin.Edge[i] | other.Edge[i]) > 0 && (basin.Edge[i] & other.Edge[i]) == 0 {
			return false
		}
	}
	return true
}

func (basin *Basin) Combine(other *Basin) {
	basin.Size += other.Size
	for i := range basin.Edge {
		basin.Edge[i] = basin.Edge[i] | other.Edge[i]
	}
}

func (basin *Basin) Include(cell uint) {
	basin.Size++
	edgeIndex := cell / 64
	cell = cell % 64
	basin.Edge[edgeIndex] = basin.Edge[edgeIndex] | (1 << cell)
}

func GenerateBasins(line string) (basins map[Basin]void) {
	var basin *Basin = &Basin{}
	basins = make(map[Basin]void)
	for i, digit := range AsToIs(line) {
		if digit == 9 {
			if basin.Size > 0 {
				basins[*basin] = member
				basin = &Basin{}
			}
		} else {
			basin.Include(uint(i))
		}
	}
	if basin.Size > 0 {
		basins[*basin] = member
	}
	return
}

func GenerateBasinRefs(line string) []*Basin {
	var basinRefs []*Basin
	for basin := range GenerateBasins(line) {
		b := basin
		basinRefs = append(basinRefs, &b)
	}
	return basinRefs
}

type BasinsScanState struct {
	Active, New map[Basin]void
	Extended map[Basin]*Basin
}

func (state *BasinsScanState) ProcessBasin(basin *Basin) {
	var overlappedBasins []Basin
	for active, extended := range state.Extended {
		if active.Overlaps(*basin) {
			overlappedBasins = append(overlappedBasins, active)
			if basin != extended {
				extended.Combine(basin)
				basin = extended
			}
		}
	}
	for active := range state.Active {
		if active.Overlaps(*basin) {
			overlappedBasins = append(overlappedBasins, active)
			basin.Size += active.Size
		}
	}
	if overlappedBasins == nil {
		state.New[*basin] = member
	} else {
		for _, active := range overlappedBasins {
			if _, present := state.Active[active]; present {
				delete(state.Active, active)
			}
			state.Extended[active] = basin
		}
	}
}

func GetBiggestBasins(bigBasins [3]uint, completeBasins map[Basin]void) [3]uint {
	for completeBasin := range completeBasins {
		switch size := completeBasin.Size; {
		case size > bigBasins[0]: bigBasins[0], bigBasins[1], bigBasins[2] = size, bigBasins[0], bigBasins[1]
		case size > bigBasins[1]: bigBasins[1], bigBasins[2] = size, bigBasins[1]
		case size > bigBasins[2]: bigBasins[2] = size
		}
	}
	return bigBasins
}

func partTwo(scanLines <-chan string, solutions chan<- string) {
	var (
		bigBasins [3]uint
		state *BasinsScanState
	)

	state = &BasinsScanState{
		Active: GenerateBasins(<-scanLines),
	}
	for scanLine := range scanLines {
		state.Extended = make(map[Basin]*Basin)
		state.New = make(map[Basin]void)
		for _, next := range GenerateBasinRefs(scanLine) {
			state.ProcessBasin(next)
		}
		bigBasins = GetBiggestBasins(bigBasins, state.Active)
		state.Active = state.New
		for _, extended := range state.Extended {
			state.Active[*extended] = member
		}
	}
	bigBasins = GetBiggestBasins(bigBasins, state.Active)

	solutions <- fmt.Sprintf(
		"Part Two 3 biggest basins: %v, Answer: %d",
		bigBasins, bigBasins[0]*bigBasins[1]*bigBasins[2],
	)
	close(solutions)
}

type ScanSlice [3][]uint

func TotalRiskLevelInMidLine(lines ScanSlice) (total uint) {
	for i, d := range lines[1] {
		if (i == 0 || d < lines[1][i-1]) &&
		(i == len(lines[1]) - 1 || d < lines[1][i+1]) &&
		(lines[2] == nil || d < lines[2][i]) &&
		(lines[0] == nil || d < lines[0][i]) {
			total += d + 1
		}
	}
	return
}

func partOne(scanLines <-chan string, solutions chan<- string) {
	var (
		lines ScanSlice
		total uint
	)
	lines[1] = AsToIs(<-scanLines)
	for scanLine := range scanLines {
		lines[0] = AsToIs(scanLine)
		total += TotalRiskLevelInMidLine(lines)
		lines[2], lines[1], lines[0] = lines[1], lines[0], nil
	}
	total += TotalRiskLevelInMidLine(lines)

	solutions <- fmt.Sprintf("Part One Total Risk Level: %d", total)
	close(solutions)
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
