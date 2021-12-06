package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func findIncreases(depths <-chan string, distance uint) (count uint) {
	depthBuffer := make(chan int, distance)
	for ; distance > 0; distance-- {
		depth, _ := strconv.Atoi(<-depths)
		depthBuffer <- depth
	}
	for depthStr := range depths {
		depth, _ := strconv.Atoi(depthStr)
		if depth > <-depthBuffer {
			count++
		}
		depthBuffer <- depth
	}
	return
}

func partOne(depths <-chan string, solutions chan<- string) {
	solutions <- fmt.Sprintf("Part 1 Depth increases: %d\n", findIncreases(depths, 1))
	close(solutions)
}

func partTwo(depths <-chan string, solutions chan<- string) {
	solutions <- fmt.Sprintf("Part 2 Depth increases: %d\n", findIncreases(depths, 3))
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
