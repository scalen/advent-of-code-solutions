package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func partOne(instructions <-chan string, solutions chan<- string) {
	forward := 0
	depth := 0
	for instruction := range instructions {
		instructionParts := strings.Split(instruction, " ")
		distance, err := strconv.Atoi(instructionParts[1])
		if err != nil {
			log.Fatal(err)
		}
		switch instructionParts[0] {
		case "forward":
			forward += distance
		case "up":
			depth -= distance
		case "down":
			depth += distance
		}
	}

	solutions <- fmt.Sprintf(
		"Part 1:\nFoward: %d\nDepth: %d\nSolution: %d\n",
		forward,
		depth,
		forward * depth,
	)
	close(solutions)
}

func partTwo(instructions <-chan string, solutions chan<- string) {
	forward := 0
	aim := 0
	depth := 0
	for instruction := range instructions {
		instructionParts := strings.Split(instruction, " ")
		difference, err := strconv.Atoi(instructionParts[1])
		if err != nil {
			log.Fatal(err)
		}
		switch instructionParts[0] {
		case "forward":
			forward += difference
			depth += difference * aim
		case "up":
			aim -= difference
		case "down":
			aim += difference
		}
	}

	solutions <- fmt.Sprintf(
		"Part 2:\nFoward: %d\nDepth: %d\nSolution: %d\n",
		forward,
		depth,
		forward * depth,
	)
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
