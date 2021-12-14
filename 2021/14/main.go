package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

func GetTemplateAndInsertionRules(input <-chan string) (template []string, rules map[string][2]string) {
	rules = make(map[string][2]string)

	templateChars := strings.Split(<-input, "")
	last := templateChars[0]
	for _, next := range templateChars[1:] {
		template = append(template, fmt.Sprintf("%s%s", last, next))
		last = next
	}

	if len(<-input) > 0 { panic("The second input line should be blank") }

	for ruleStr := range input {
		parts := strings.Split(ruleStr, " -> ")
		initialPair := parts[0]
		splitPair := strings.Split(initialPair, "")
		rules[initialPair] = [2]string{
			fmt.Sprintf("%s%s", splitPair[0], parts[1]),
			fmt.Sprintf("%s%s", parts[1], splitPair[1]),
		}
	}
	return
}

func GetElementCounts(input <-chan string, steps uint) (resultChars map[rune]uint) {
	template, rules := GetTemplateAndInsertionRules(input)
	resultChars = map[rune]uint{
		[]rune(template[0])[0]: 1,
		[]rune(template[len(template)-1])[1]: 1,
	}

	resultPairs := make(map[string]uint, len(rules))
	for _, pair := range template {
		resultPairs[pair]++
	}
	for step := uint(0); step < steps; step++ {
		newResultPairs := make(map[string]uint, len(resultPairs))
		for pair, count := range resultPairs {
			for _, result := range rules[pair] {
				newResultPairs[result] += count
			}
		}
		resultPairs = newResultPairs
	}

	for pair, count := range resultPairs {
		for _, char := range []rune(pair) {
			resultChars[char] += count
		}
	}

	for char, count := range resultChars {
		resultChars[char] = count / 2
	}
	return
}

func MostSubLeast(counts map[rune]uint) uint {
	var most, least uint
	least--
	for _, count := range counts {
		if count > most {
			most = count
		} else if count < least {
			least = count
		}
	}
	return most - least
}

func partTwo(input <-chan string, solutions chan<- string) {
	defer close(solutions)

	resultChars := GetElementCounts(input, 40)

	solutions <- fmt.Sprintf(
		"Part 1\n  Letter Frequencies: %v\n  Score: %d",
		resultChars, MostSubLeast(resultChars),
	)
}

func partOne(input <-chan string, solutions chan<- string) {
	defer close(solutions)

	resultChars := GetElementCounts(input, 10)

	solutions <- fmt.Sprintf(
		"Part 1\n  Letter Frequencies: %v\n  Score: %d",
		resultChars, MostSubLeast(resultChars),
	)
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
