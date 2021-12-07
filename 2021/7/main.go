package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Position struct {
	Position int
	TotalOffset int
	Count int
	Size int
	Lower *Position
	Higher *Position
}

func NewPosition(position int) *Position {
	return &Position{
		Position: position,
		Count: 1,
		Size: 1,
	}
}

func (root *Position) Balance() int {
	var imbalance int
	if root.Higher != nil {
		imbalance -= root.Higher.Size
	}
	if root.Lower != nil {
		imbalance += root.Lower.Size
	}
	return imbalance / 2
}

func (root *Position) TotalOffsetFrom(position int) int {
	return root.TotalOffset + (root.Size * (root.Position - position))
}

func (root *Position) Recalculate() {
	root.Size, root.TotalOffset = root.Count, 0
	if root.Higher != nil {
		root.Size += root.Higher.Size
		root.TotalOffset += root.Higher.TotalOffsetFrom(root.Position)
	}
	if root.Lower != nil {
		root.Size += root.Lower.Size
		root.TotalOffset += root.Lower.TotalOffsetFrom(root.Position)
	}
}

func (root *Position) Add(other *Position) {
	var subTreePointer **Position
	if other.Position < root.Position {
		subTreePointer = &root.Lower
	} else if other.Position > root.Position {
		subTreePointer = &root.Higher
	} else {
		root.Count++
		if other.Higher != nil {root.Add(other.Higher)}
		if other.Lower != nil {root.Add(other.Lower)}
	}

	if subTreePointer != nil {
		if *subTreePointer == nil {
			*subTreePointer = other
		} else {
			(*subTreePointer).Add(other)
		}
	}

	root.Recalculate()
}

func (root *Position) Cost(offset int) (cost int) {
	if root.Higher != nil {
		cost += root.Higher.TotalOffsetFrom(root.Position + offset)
	}
	if root.Lower != nil {
		cost -= root.Lower.TotalOffsetFrom(root.Position + offset)
	}
	return cost + offset
}

func partOne(positions <-chan string, solutions chan<- string) {
	var primePosition *Position
	for _, positionStr := range strings.Split(<-positions, ",") {
		position, _ := strconv.Atoi(positionStr)
		newPosition := NewPosition(position)
		if primePosition == nil {
			primePosition = newPosition
			continue
		}
		primePosition.Add(newPosition)
	}

	for -primePosition.Count > primePosition.Balance() || primePosition.Balance() > primePosition.Count {
		var newPrimePosition *Position
		if primePosition.Lower == nil || (primePosition.Higher != nil && primePosition.Balance() < 0) {
			newPrimePosition, primePosition.Higher = primePosition.Higher, nil
		} else {
			newPrimePosition, primePosition.Lower = primePosition.Lower, nil
		}
		primePosition.Recalculate()
		newPrimePosition.Add(primePosition)
		primePosition = newPrimePosition
	}

	solutions <- fmt.Sprintf(
		"Part One Target Position: %d, Fuel Cost: %d",
		primePosition.Position,
		primePosition.Cost(0),
	)
	close(solutions)
}

func (root *Position) TriangleCost(position int) (cost int) {
	if root.Higher != nil {
		cost += root.Higher.TriangleCost(position)
	}
	if root.Lower != nil {
		cost += root.Lower.TriangleCost(position)
	}
	var distance int
	if root.Position > position {
		distance = root.Position - position
	} else {
		distance = position - root.Position
	}
	return cost + root.Count * distance * (distance + 1) / 2
}

func partTwo(positions <-chan string, solutions chan<- string) {
	var (
		rootPosition *Position
		maxPosition, minPosition, primePosition, minCost int
	)
	for _, positionStr := range strings.Split(<-positions, ",") {
		position, _ := strconv.Atoi(positionStr)
		if position > maxPosition {maxPosition = position}
		if position < minPosition {minPosition = position}
		newPosition := NewPosition(position)
		if rootPosition == nil {
			rootPosition = newPosition
			continue
		}
		rootPosition.Add(newPosition)
	}

	for position := minPosition; position < maxPosition; position++ {
		if cost := rootPosition.TriangleCost(position); minCost == 0 || cost < minCost {
			minCost, primePosition = cost, position
		}
	}

	solutions <- fmt.Sprintf(
		"Part Two Target Position: %d, Fuel Cost: %d",
		primePosition,
		minCost,
	)
	close(solutions)
}

func main() {
	parts := []func(<-chan string, chan<- string){
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
