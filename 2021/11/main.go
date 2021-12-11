package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func AsToIs(as string) []uint {
	is := make([]uint, len(as))
	for x, r := range as {
		number, _ := strconv.Atoi(string(r))
		is[x] = uint(number)
	}
	return is
}

func MinUInt(a, b uint) uint {
	if a > b {
		return b
	}
	return a
}

func SetupOctopusGrid(octopusStrs <-chan string) (octopuses [][]uint) {
	for line := range octopusStrs {
		octopuses = append(octopuses, AsToIs(line))
	}
	return
}

func Trigger(t [2]uint, octopuses [][]uint, flashes chan<- [2]uint) {
	octopuses[t[0]][t[1]]++
	if octopuses[t[0]][t[1]] == 10 {
		flashes <- t
		for x := MinUInt(t[0], t[0]-1); x < MinUInt(t[0]+2, uint(len(octopuses))); x++ {
			for y := MinUInt(t[1], t[1]-1); y < MinUInt(t[1]+2, uint(len(octopuses[x]))); y++ {
				if x != t[0] || y != t[1] {
					Trigger([2]uint{x, y}, octopuses, flashes)
				}
			}
		}
	}
	return
}

func StepAndCountFlashes(octopuses [][]uint) (flashCount uint) {
	var octopusCount uint
	for _, line := range octopuses {octopusCount += uint(len(line))}
	flashes := make(chan [2]uint, octopusCount)
	for x := range octopuses {
		for y := range octopuses[x] {
			 Trigger([2]uint{uint(x), uint(y)}, octopuses, flashes)
		}
	}
	close(flashes)
	for f := range flashes {
		flashCount++
		octopuses[f[0]][f[1]] = 0
	}
	return
}

func partTwo(octopusStrs <-chan string, solutions chan<- string) {
	defer close(solutions)
	var step, flashCount, octopusCount uint

	octopuses := SetupOctopusGrid(octopusStrs)
	for _, line := range octopuses {octopusCount += uint(len(line))}
	for ; flashCount < octopusCount; step++ {
		flashCount = StepAndCountFlashes(octopuses)
	}

	solutions <- fmt.Sprintf("Part 2 Steps: %d", step)
}

func partOne(octopusStrs <-chan string, solutions chan<- string) {
	defer close(solutions)
	var flashCount uint

	octopuses := SetupOctopusGrid(octopusStrs)
	for step := 0; step < 100; step++ {
		flashCount += StepAndCountFlashes(octopuses)
	}

	solutions <- fmt.Sprintf("Part 1 Flashes: %d", flashCount)
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
