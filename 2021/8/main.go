package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"math"
	"os"
	"strings"
)

type void struct{}
type RuneSet map[rune]void
var member void

func NewRuneSet(str string) (set RuneSet) {
	set = make(RuneSet, len(str))
	for _, r := range []rune(str) {
		set[r] = member
	}
	return
}

func (set RuneSet) IsSubset(other RuneSet) bool {
	if len(set) > len(other) { return false }
	for r, _ := range set {
		if _, present := other[r]; ! present {
			return false
		}
	}
	return true
}

func (set RuneSet) IsEqual(other RuneSet) bool {
	if len(set) != len(other) { return false }
	for r, _ := range set {
		if _, present := other[r]; ! present {
			return false
		}
	}
	return true
}

func (set RuneSet) Subtract(other RuneSet) (diff RuneSet) {
	diff = make(RuneSet)
	for r, _ := range set {
		if _, present := other[r]; ! present {
			diff[r] = member
		}
	}
	return
}

func (set RuneSet) Intersect(other RuneSet) (intersection RuneSet) {
	intersection = make(RuneSet)
	for r, _ := range set {
		if _, present := other[r]; present {
			intersection[r] = member
		}
	}
	return
}

func (set RuneSet) Union(other RuneSet) (union RuneSet) {
	union = make(RuneSet, len(set))
	for r, _ := range set {
		union[r] = member
	}
	for r, _ := range other {
		union[r] = member
	}
	return
}

func partTwoBenLomax(segmentReports <-chan string, solutions chan<- string) {
	var total uint
	for report := range segmentReports {
		parts := strings.Split(report, "|")
		allSegments := NewRuneSet(strings.Replace(parts[0], " ", "", -1))
		outputSegments := strings.Fields(parts[1])

		setsByNo := make(map[uint]RuneSet)
		i069, i235 := allSegments, allSegments
		for _, segments := range strings.Fields(parts[0]) {
			segmentMap := NewRuneSet(segments)
			switch len(segments) {
			case 2: setsByNo[1] = segmentMap
			case 3: setsByNo[7] = segmentMap
			case 4: setsByNo[4] = segmentMap
			case 5: i235 = i235.Intersect(segmentMap)
			case 6: i069 = i069.Intersect(segmentMap)
			case 7: setsByNo[8] = segmentMap
			}
		}
		setsByNo[0] = setsByNo[8].Subtract(i235).Union(i069)
		setsByNo[2] = setsByNo[8].Subtract(i069).Union(i235)
		setsByNo[3] = setsByNo[1].Union(i235)
		setsByNo[5] = i069.Union(i235)
		setsByNo[6] = setsByNo[8].Subtract(setsByNo[1]).Union(i069)
		setsByNo[9] = setsByNo[4].Union(i069)

		for i, multiplier := 0, uint(math.Pow(10, float64(len(outputSegments)-1))); i < len(outputSegments); i, multiplier = i+1, multiplier/10 {
			segmentSet := NewRuneSet(outputSegments[i])
			for digit, set := range setsByNo {
				if set.IsEqual(segmentSet) {
					total += digit * multiplier
					break
				}
			}
		}
	}

	solutions <- fmt.Sprintf("Total of Outputs, by Ben Lomax: %d", total)
	close(solutions)
}

func CategoriseSegments(segments string, segmentsInNumbers map[uint]RuneSet) (err error) {
	segmentSet := NewRuneSet(segments)
	switch len(segmentSet) {
	case 2:
		segmentsInNumbers[1] = segmentSet
	case 3:
		segmentsInNumbers[7] = segmentSet
	case 4:
		segmentsInNumbers[4] = segmentSet
	case 5:
		set1, present1 := segmentsInNumbers[1]
		set4, present4 := segmentsInNumbers[4]
		set7, present7 := segmentsInNumbers[7]
		if present1 && present4 && present7 {
			if set7.IsSubset(segmentSet) {
				segmentsInNumbers[3] = segmentSet
			} else if set4.Subtract(set1).IsSubset(segmentSet) {
				segmentsInNumbers[5] = segmentSet
			} else {
				segmentsInNumbers[2] = segmentSet
			}
		} else {
			err = errors.New("Unrecognised 5 segment digit")
		}
	case 6:
		set4, present4 := segmentsInNumbers[4]
		set7, present7 := segmentsInNumbers[7]
		if present4 && present7 {
			if set4.IsSubset(segmentSet) {
				segmentsInNumbers[9] = segmentSet
			} else if set7.IsSubset(segmentSet) {
				segmentsInNumbers[0] = segmentSet
			} else {
				segmentsInNumbers[6] = segmentSet
			}
		} else {
			err = errors.New("Unrecognised 6 segment digit")
		}
	case 7:
		segmentsInNumbers[8] = segmentSet
	}
	return
}

func partTwo(segmentReports <-chan string, solutions chan<- string) {
	var total uint
	for report := range segmentReports {
		parts := strings.Split(report, "|")
		outputSegments := strings.Fields(parts[1])
		unknownSegments := make(chan string, 10)
		for _, segments := range strings.Fields(parts[0]) {
			unknownSegments <- segments
		}

		segmentsInNumbers := make(map[uint]RuneSet)
		for len(segmentsInNumbers) < 10 {
			segments := <-unknownSegments
			if err := CategoriseSegments(segments, segmentsInNumbers); err != nil {
				unknownSegments <- segments
			}
		}
		close(unknownSegments)

		for i, multiplier := 0, uint(math.Pow(10, float64(len(outputSegments)-1))); i < len(outputSegments); i, multiplier = i+1, multiplier/10 {
			segmentSet := NewRuneSet(outputSegments[i])
			for digit, set := range segmentsInNumbers {
				if set.IsEqual(segmentSet) {
					total += digit * multiplier
					break
				}
			}
		}
	}

	solutions <- fmt.Sprintf("Total of Outputs: %d", total)
	close(solutions)
}

func partOne(segmentReports <-chan string, solutions chan<- string) {
	counts := make(map[uint]uint, 4)
	outputSegments := make(chan string)
	go func(reports <-chan string, outputs chan<- string) {
		for report := range reports {
			for _, output := range strings.Fields(strings.Split(report, "|")[1]) {
				outputs <- output
			}
		}
		close(outputs)
	}(segmentReports, outputSegments)

	var total uint
	for segments := range outputSegments {
		switch len(segments) {
		case 2: counts[1]++
		case 3: counts[7]++
		case 4: counts[4]++
		case 7: counts[8]++
		default: continue
		}
		total++
	}

	solutions <- fmt.Sprintf(
		"1s: %d, 4s: %d, 7s: %d, 8s: %d, Total %d",
		counts[1], counts[4], counts[7], counts[8], total,
	)
	close(solutions)
}

func main() {
	parts := []func(<-chan string, chan<- string) {
		partOne,
		partTwo,
		partTwoBenLomax,
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
