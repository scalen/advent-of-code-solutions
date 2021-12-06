package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func simulate(fishAges string, days uint) (finalCount uint) {
	countByAge := make(map[uint]uint)
	for _, ageStr := range strings.Split(fishAges, ",") {
		age, _ := strconv.Atoi(ageStr)
		countByAge[uint(age)] += 1
	}
	for day := uint(0); day < days; day++ {
		newCountByAge := map[uint]uint{ 8: countByAge[0], 6: countByAge[0] }
		for age := uint(1); age <= 8; age ++ {
			newCountByAge[age-1] += countByAge[age]
		}
		countByAge = newCountByAge
	}
	for _, countForAge := range countByAge {
		finalCount += countForAge
	}
	return
}

func partOne(fishAges <-chan string, solutions chan<- string) {
	solutions <- fmt.Sprintf("Part One Fish Count: %d\n", simulate(<-fishAges, 80))
	close(solutions)
}

func partTwo(fishAges <-chan string, solutions chan<- string) {
	solutions <- fmt.Sprintf("Part Two Fish Count: %d\n", simulate(<-fishAges, 256))
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
