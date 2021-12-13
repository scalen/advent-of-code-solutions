package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

type void struct{}
type Dot [2]uint
type Paper map[Dot]void
var member void

func (dot Dot) Reflect(line Dot) (reflection Dot, reflected bool) {
	var lineDimension uint
	reflection = dot
	if line[0] > 0 { lineDimension = 0 } else { lineDimension = 1 }
	if dot[lineDimension] < line[lineDimension] { return }
	if dot[lineDimension] > 2*line[lineDimension] { return Dot{}, true }
	reflection[lineDimension] = 2*line[lineDimension] - dot[lineDimension]
	return reflection, true
}

func (dots Paper) Add(dot Dot) {
	dots[dot] = member
}

func (dots Paper) Fold(line Dot) {
	for dot := range dots {
		if reflection, reflected := dot.Reflect(line); reflected {
			delete(dots, dot)
			dots.Add(reflection)
		}
	}
}

func NewPaper(dotDescriptions <-chan string) (dots Paper) {
	dots = make(Paper)
	for dotDescription := range dotDescriptions {
		if len(strings.TrimSpace(dotDescription)) == 0 { return }
		xy := strings.Split(dotDescription, ",")
		x, _ := strconv.Atoi(xy[0])
		y, _ := strconv.Atoi(xy[1])
		dots.Add(Dot{uint(x), uint(y)})
	}
	return
}

func GenerateFoldLines(instructions <-chan string, lines chan<- Dot) {
	defer close(lines)
	for instruction := range instructions {
		parts := strings.Split(instruction, "=")
		extent, _ := strconv.Atoi(parts[1])
		switch []rune(parts[0])[len(parts[0])-1] {
		case 'x': lines <- Dot{uint(extent), 0}
		case 'y': lines <- Dot{0, uint(extent)}
		}
	}
}

func firstFold(input <-chan string, solutions chan<- string) {
	defer close(solutions)

	paper := NewPaper(input)
	lines := make(chan Dot)
	go GenerateFoldLines(input, lines)
	paper.Fold(<-lines)
	for range lines {}

	solutions <- fmt.Sprintf("Dot Count after First Fold: %d", len(paper))
}

func (paper Paper) String() string {
	var (
		index uint
		dots []Dot = make([]Dot, len(paper))
	)
	for dot := range paper {
		dots[index] = dot
		index++
	}
	sort.Slice(dots, func(i, j int) bool {
		return dots[i][1] < dots[j][1] || (dots[i][1] == dots[j][1] && dots[i][0] < dots[j][0])
	})

	var (
		last Dot
		offset uint
		output strings.Builder
	)
	for _, dot := range dots {
		if dot[1] > last[1] {
			for i := last[1]; i < dot[1]; i++ {
				output.WriteString("\n")
			}
			last, offset = Dot{0, dot[1]}, 0
		}
		for i := last[0]+offset; i < dot[0]; i++ {
			output.WriteString(" ")
		}
		output.WriteString("#")
		last, offset = dot, 1
	}
	return output.String()
}

func revealCode(input <-chan string, solutions chan<- string) {
	defer close(solutions)

	paper := NewPaper(input)
	lines := make(chan Dot)
	go GenerateFoldLines(input, lines)
	for line := range lines {
		paper.Fold(line)
	}

	solutions <- fmt.Sprintf("Code:\n%s", paper)
}

func main() {
	parts := []func(<-chan string, chan<- string) {
		firstFold,
		revealCode,
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
