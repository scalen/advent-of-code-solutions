package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Coord struct {
	X int
	Y int
}

func (coord Coord) Add(other Coord) Coord {
	return Coord{
		X: coord.X + other.X,
		Y: coord.Y + other.Y,
	}
}

func (coord Coord) Subtract(other Coord) Coord {
	return Coord{
		X: coord.X - other.X,
		Y: coord.Y - other.Y,
	}
}

func (coord Coord) Normalise() (unit Coord, magnitude uint) {
	// Lines will only ever be 0, 45 or 90 degrees, therefore a step along one axis will
	// either be impossible, result in no change on the other, or be matched by an equal
	// set on the other.
	var absX, absY uint
	if coord.X < 0 { absX = uint(-coord.X) } else { absX = uint(coord.X) }
	if coord.Y < 0 { absY = uint(-coord.Y) } else { absY = uint(coord.Y) }
	if absX > absY { magnitude = absX } else { magnitude = absY }
	unit = Coord{
		X: coord.X / int(magnitude),
		Y: coord.Y / int(magnitude),
	}
	return
}

func mapper(vectors <-chan string, solutions chan<- string, noDiag bool) {
	grid := make(map[Coord]uint)
	for vector := range vectors {
		ends := strings.Fields(vector)
		startParts, endParts := strings.Split(ends[0], ","), strings.Split(ends[2], ",")
		startX, _ := strconv.Atoi(startParts[0])
		startY, _ := strconv.Atoi(startParts[1])
		endX, _ := strconv.Atoi(endParts[0])
		endY, _ := strconv.Atoi(endParts[1])

		current := Coord{X: startX, Y: startY}
		difference := Coord{X: endX, Y: endY}.Subtract(current)
		if noDiag && difference.X != 0 && difference.Y != 0 { continue }

		grid[current] += 1
		for increment, length := difference.Normalise(); length > 0; length -= 1 {
			current = current.Add(increment)
			grid[current] += 1
		}
	}

	var overlapCount uint
	for _, count := range grid {
		if count > 1 {
			overlapCount += 1
		}
	}

	solutions <- fmt.Sprintf("Overlap points: %d\nIgnoring diagonals: %t\n", overlapCount, noDiag)
	close(solutions)
}

func partOne(vectors <-chan string, solutions chan<- string) {
	mapper(vectors, solutions, true)
}
func partTwo(vectors <-chan string, solutions chan<- string) {
	mapper(vectors, solutions, false)
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
