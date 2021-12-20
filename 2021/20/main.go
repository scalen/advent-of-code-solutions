package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

type Scan [][]bool
type void struct{}
var member void

func NewEnhancementMap(description string) map[uint]void {
	enhancementMap := make(map[uint]void)
	for i, r := range []rune(description) {
		if r == '#' {
			enhancementMap[uint(i)] = member
		}
	}
	return enhancementMap
}

func ReadScan(coded <-chan string) Scan {
	var scan Scan
	for codedLine := range coded {
		line := make([]bool, len(codedLine) + 6)
		for i, r := range []rune(codedLine) {
			line[i+3] = r == '#'
		}
		scan = append(scan, line)
	}
	bufferLines := Scan{
		make([]bool, len(scan[0])),
		make([]bool, len(scan[0])),
		make([]bool, len(scan[0])),
	}
	scan = append(bufferLines, scan...)
	return append(scan, bufferLines...)
}

func (scan Scan) String() string {
	var output strings.Builder
	for _, line := range scan {
		for _, pixel := range line {
			if pixel { output.WriteString("#") } else { output.WriteString(".") }
		}
		output.WriteString("\n")
	}
	return output.String()
}

func (scan Scan) ApplyEnhancementAlgorithm(enhancementMap map[uint]void, rounds uint) Scan {
	for round := uint(0); round < rounds; round++ {
		enhanced := make(Scan, len(scan) + 2)
		for lineNo := 2; lineNo < len(scan); lineNo++ {
			enhanced[lineNo] = make([]bool, len(scan[0]) + 2)
			for colNo := 2; colNo < len(scan[0]); colNo++ {
				var pixelValue uint
				enhancementSpace := Scan{make([]bool,3), make([]bool,3), make([]bool,3)}
				for lOffset := 0; lOffset < 3; lOffset++{
					for cOffset := 0; cOffset < 3; cOffset++ {
						if scan[lineNo - lOffset][colNo - cOffset] {
							enhancementSpace[2-lOffset][2-cOffset] = true
							pixelValue += 1 << (lOffset * 3 + cOffset)
						}
					}
				}
				_, lit := enhancementMap[pixelValue]
				enhanced[lineNo][colNo] = lit
			}
			enhanced[lineNo][0] = enhanced[lineNo][2]
			enhanced[lineNo][1] = enhanced[lineNo][2]
			enhanced[lineNo][len(enhanced[lineNo])-2] = enhanced[lineNo][len(enhanced[lineNo])-3]
			enhanced[lineNo][len(enhanced[lineNo])-1] = enhanced[lineNo][len(enhanced[lineNo])-3]
		}
		enhanced[0], enhanced[1] = enhanced[2], enhanced[2]
		enhanced[len(enhanced)-2] = enhanced[len(enhanced)-3]
		enhanced[len(enhanced)-1] = enhanced[len(enhanced)-3]
		scan = enhanced
	}
	return scan
}

func partTwo(input <-chan string, solutions chan<- string) {
	defer close(solutions)

	enhancementMap := NewEnhancementMap(<-input)
	if "" != <-input { panic("Expecting empty second line in input") }
	scan := ReadScan(input)
	enhancedScan := scan.ApplyEnhancementAlgorithm(enhancementMap, 50)

	var lightCount uint
	for _, line := range enhancedScan {
		for _, pixel := range line {
			if pixel { lightCount++ }
		}
	}

	solutions <- fmt.Sprintf("Light Pixels after 50 Rounds: %d", lightCount)
}

func partOne(input <-chan string, solutions chan<- string) {
	defer close(solutions)

	enhancementMap := NewEnhancementMap(<-input)
	if "" != <-input { panic("Expecting empty second line in input") }
	scan := ReadScan(input)
	enhancedScan := scan.ApplyEnhancementAlgorithm(enhancementMap, 2)

	var lightCount uint
	for _, line := range enhancedScan {
		for _, pixel := range line {
			if pixel { lightCount++ }
		}
	}

	solutions <- fmt.Sprintf("Light Pixels after 2 Rounds: %d", lightCount)
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
