package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
)

type BraceStack []rune

func (stack BraceStack) Add(r rune) (BraceStack, bool) {
	var offset rune
	switch r {
	case ']', '}', '>': offset = 2
	case ')': offset = 1
	case '(', '[', '{', '<':
		return append(stack, r), true
	}
	if stack[len(stack)-1] == r - offset {
		return stack[:len(stack)-1], true
	}
	return stack, false
}

func (stack BraceStack) Empty(out chan<- rune) {
	defer close(out)
	for i := len(stack) - 1; i > 0; i-- {
		out <- stack[i]
	}
	out <- stack[0]
}

func partTwo(code <-chan string, solutions chan<- string) {
	defer close(solutions)

	var lineTotals []int
	for line := range code {
		var stack BraceStack
		var legal bool
		for _, r := range line {
			stack, legal = stack.Add(r)
			if ! legal {
				break
			}
		}
		if ! legal { continue }
		var lineTotal int
		stackDrain := make(chan rune)
		go stack.Empty(stackDrain)
		for r := range stackDrain {
			var value int
			switch r {
			case '(': value = 1
			case '[': value = 2
			case '{': value = 3
			case '<': value = 4
			}
			lineTotal = lineTotal * 5 + value
		}
		lineTotals = append(lineTotals, lineTotal)
	}
	sort.Ints(lineTotals)
	middleIndex := len(lineTotals) / 2 + len(lineTotals) % 2 - 1

	solutions <- fmt.Sprintf("Autocorrect Score: %d", lineTotals[middleIndex])
}

func partOne(code <-chan string, solutions chan<- string) {
	defer close(solutions)

	illegal := make(map[rune]uint, 4)
	for line := range code {
		var stack BraceStack
		for _, r := range line {
			var legal bool
			stack, legal = stack.Add(r)
			if ! legal {
				illegal[r]++
				break
			}
		}
	}

	solutions <- fmt.Sprintf(
		"Illegal )s: %d, ]s: %d, }s: %d, >s: %d, Score: %d",
		illegal[')'], illegal[']'], illegal['}'], illegal['>'],
		illegal[')']*3+illegal[']']*57+illegal['}']*1197+illegal['>']*25137,
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
