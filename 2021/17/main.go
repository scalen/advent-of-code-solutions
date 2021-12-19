package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Bounds [2][2]int
type void struct{}

const dimX, dimY uint = 0, 1

var member void

func NewBounds(spec string) Bounds {
	var area Bounds
	areaDef := strings.Split(spec, ": ")[1]
	for _, boundsDef := range strings.Split(areaDef, ", ") {
		boundsDefParts := strings.Split(boundsDef, "=")
		dimension := uint([]rune(boundsDefParts[0])[0] - 'x')
		for bound, boundDef := range strings.Split(boundsDefParts[1], "..") {
			boundValue, _ := strconv.Atoi(boundDef)
			area[dimension][bound] = boundValue
		}
	}
	return area
}

func partTwo(input <-chan string, solutions chan<- string) {
	defer close(solutions)
	var (
		successMap = make(map[[2]int]void)
		vCount uint
		tBounds = NewBounds(<-input)
		vBounds = Bounds{[2]int{0, tBounds[dimX][1]}}
		maxSteps, maxTriangle int
		triangles []int
	)

	// Find velocity bounds
	for vBounds[dimX][0] * (vBounds[dimX][0] + 1) / 2 < tBounds[dimX][0] { vBounds[dimX][0]++ }
	if tBounds[dimY][0] > 0 {
		vBounds[dimY][1] = tBounds[dimY][1]
		maxSteps = vBounds[dimY][1] * 2
		for vBounds[dimY][0] * (vBounds[dimY][0] + 1) / 2 < tBounds[dimY][0] { vBounds[dimY][0]++ }
	} else if tBounds[dimY][1] < 0 {
		vBounds[dimY] = [2]int{tBounds[dimY][0], - tBounds[dimY][0] - 1}
		maxSteps = vBounds[dimY][1] * 2 + 2
	} else {
		solutions <- "Infinite initial velocities possible."
		return
	}

	// Calculate all necessary triangle numbers
	for _, i := range [...]int{maxSteps-vBounds[dimY][0]-1, maxSteps-vBounds[dimY][1]-1, vBounds[dimY][1]} {
		if i > maxTriangle { maxTriangle = i }
	}
	triangles = make([]int, maxTriangle + 1)
	triangles[0] = 0
	for i := 1; i <= maxTriangle; i++ {
		triangles[i] = triangles[i-1] + i
	}

	// Iterate over all viable Xs, Ys and steps, counting the first instance of each valid velocity
	for x := vBounds[dimX][0]; x <= vBounds[dimX][1]; x++ {
		var (
			end [2]int
			step int
		)
		for ; step <= maxSteps && end[0] < tBounds[dimX][0]; step++ {
			end[0] += x - step
		}
		for ; step <= maxSteps && end[0] <= tBounds[dimX][1]; step++ {
			for y := vBounds[dimY][0]; y <= vBounds[dimY][1]; y++ {
				var peak, stepsFromPeak int
				if y < 0 {
					peak = triangles[-1 - y]
					stepsFromPeak = step - y - 1
				} else {
					peak = triangles[y]
					if step < y {
						stepsFromPeak = y - step - 1
					} else {
						stepsFromPeak = step - y - 1
					}
				}
				var drop int
				if stepsFromPeak > 0 { drop = triangles[stepsFromPeak] }
				end[1] = peak - drop

				if tBounds[dimY][0] <= end[1] && end[1] <= tBounds[dimY][1] {
					velocity := [2]int{x, y}
					if _, seen := successMap[velocity]; ! seen {
						successMap[velocity] = member
						vCount++
					} else {
					}
				}
			}
			if step < x { end[0] += x - step }
		}
	}

	solutions <- fmt.Sprintf("Distinct initial velocities: %d", vCount)
}

func partOne(input <-chan string, solutions chan<- string) {
	defer close(solutions)

	var (
		height int
		yBounds = NewBounds(<-input)[dimY]
	)
	if yBounds[0] > 0 {
		height = yBounds[1] * (1 + yBounds[1]) / 2
	} else if yBounds[1] < 0 {
		height = (1 + yBounds[0]) * yBounds[0] / 2
	} else {
		solutions <- "No upper bound on height."
		return
	}

	solutions <- fmt.Sprintf("Maximum height: %d", height)
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
