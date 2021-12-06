package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func partOne(report <-chan string, solutions chan<- string) {
	var (
		totalsPerBit []int
		totalReports int
		gamma uint
		epsilon uint
		bit uint
	)
	for binaryReportStr := range report {
		if totalsPerBit == nil {
			totalsPerBit = make([]int, len(binaryReportStr))
		}
		for i, c := range binaryReportStr {
			switch c {
			case '1':
				totalsPerBit[i] += 1
			}
		}
		totalReports += 1
	}
	threshold := totalReports / 2
	for i, count := range totalsPerBit {
		bit = (1 << (len(totalsPerBit) - i - 1))
		if count > threshold {
			gamma |= bit
		} else {
			epsilon |= bit
		}
	}
	solutions <- fmt.Sprintf(
		"Bit Counts: %v\nNumber of Reports: %d\nGamma: %d\nEpsilon: %d\nPower Consumption: %d",
		totalsPerBit,
		totalReports,
		gamma,
		epsilon,
		gamma * epsilon,
	)
	close(solutions)
}

func greaterThanOrEqual(a, b uint) bool {
	return a >= b
}

func lessThan(a, b uint) bool {
	return a < b
}

type TreeNode struct {
	Count uint
	Bits uint
	BitPosition uint
	HighBranch *TreeNode
	LowBranch *TreeNode
}

func (node *TreeNode) AddReportBit(c rune) (nextNode *TreeNode) {
	if node.BitPosition < 1 {
		panic("bit position cannot be less than 0")
	}
	newBitPosition := node.BitPosition - 1
	if c == '1' {
		if node.HighBranch == nil {
			node.HighBranch = &TreeNode{
				BitPosition: newBitPosition,
				Bits: node.Bits | (1 << newBitPosition),
			}
		}
		nextNode = node.HighBranch
	} else {
		if node.LowBranch == nil {
			node.LowBranch = &TreeNode{
				BitPosition: newBitPosition,
				Bits: node.Bits,
			}
		}
		nextNode = node.LowBranch
	}
	nextNode.Count += 1
	return
}

func (node *TreeNode) GetRatingByComparator(comparator func (uint, uint) bool) uint {
	if node.HighBranch == nil {
		if node.LowBranch == nil {
			return node.Bits
		} else {
			return node.LowBranch.GetRatingByComparator(comparator)
		}
	} else if node.LowBranch == nil || comparator(node.HighBranch.Count, node.LowBranch.Count) {
		return node.HighBranch.GetRatingByComparator(comparator)
	} else {
		return node.LowBranch.GetRatingByComparator(comparator)
	}
}

func partTwo(report <-chan string, solutions chan<- string) {
	var reportsRoot TreeNode
	for binaryReportStr := range report {
		if reportsRoot.BitPosition == 0 {
			reportsRoot.BitPosition = uint(len(binaryReportStr))
		}
		reportsNode := &reportsRoot
		for _, c := range binaryReportStr {
			reportsNode = reportsNode.AddReportBit(c)
		}
	}
	o2 := (&reportsRoot).GetRatingByComparator(greaterThanOrEqual)
	cO2 := (&reportsRoot).GetRatingByComparator(lessThan)

	solutions <- fmt.Sprintf(
		"Oxygen Generator: %d\nCO2 Scrubber: %d\nLife Support: %d",
		o2,
		cO2,
		o2 * cO2,
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
