package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
)

type RegularNum uint8
type SnailNum interface{
	Magnitude() uint
	ExplodeAll(uint8, [2]*RegularNum) (SnailNum, *RegularNum)
	SplitFirst() (SnailNum, bool)
	Copy() SnailNum
}
type SnailPair [2]SnailNum

func ParseOneSnailNum(runes <-chan rune) SnailNum {
	firstChar := <-runes
	if firstChar != '[' {
		i, _ := strconv.Atoi(string(firstChar))
		regularNum := RegularNum(i)
		return &regularNum
	}
	first := ParseOneSnailNum(runes)
	if ',' != <-runes { panic("First of SnailPair not followed by ','.") }
	second := ParseOneSnailNum(runes)
	if ']' != <-runes { panic("Second of SnailPair not followed by ']'.") }
	return &SnailPair{first, second}
}

func NewSnailNum(input string) SnailNum {
	runes := make(chan rune, len(input))
	for _, r := range []rune(input) {
		runes <- r
	}
	close(runes)
	return ParseOneSnailNum(runes)
}

func (num *RegularNum) Magnitude() uint {
	return uint(*num)
}

func (num *RegularNum) ExplodeAll(_ uint8, _ [2]*RegularNum) (SnailNum, *RegularNum) {
	return nil, num
}

func (num *RegularNum) SplitFirst() (SnailNum, bool) {
	if *num >= 10 {
		first := RegularNum(math.Floor(float64(*num) / 2))
		second := RegularNum(math.Ceil(float64(*num) / 2))
		return &SnailPair{&first, &second}, true
	}
	return nil, false
}

func (num *RegularNum) Copy() SnailNum {
	n := *num
	return &n
}

func (num *RegularNum) String() string {
	return fmt.Sprintf("%d", *num)
}

func (num *SnailPair) Magnitude() uint {
	return num[0].Magnitude() * 3 + num[1].Magnitude() * 2
}

func (num *SnailPair) ExplodeAll(depth uint8, borders [2]*RegularNum) (SnailNum, *RegularNum) {
	depth++
	if depth > 4 {
		for i := range num { *borders[i] += *num[i].(*RegularNum) }
		var zero RegularNum
		return &zero, &zero
	}
	newSub, pointerToLastRegularNum := num[0].ExplodeAll(
		depth,
		[2]*RegularNum{
			borders[0],
			GetPointerToNextRegularNum(num[1]),
		},
	)
	if newSub != nil { num[0] = newSub }
	newSub, pointerToLastRegularNum = num[1].ExplodeAll(
		depth,
		[2]*RegularNum{
			pointerToLastRegularNum,
			borders[1],
		},
	)
	if newSub != nil { num[1] = newSub }
	return nil, pointerToLastRegularNum
}

func (num *SnailPair) SplitFirst() (SnailNum, bool) {
	for i := range num {
		if newSub, split := num[i].SplitFirst(); split {
			if newSub != nil { num[i] = newSub }
			return nil, true
		}
	}
	return nil, false
}

func (num *SnailPair) Copy() SnailNum {
	return &SnailPair{num[0].Copy(), num[1].Copy()}
}

func (num *SnailPair) String() string {
	return fmt.Sprintf("[%s,%s]", num[0], num[1])
}

func GetPointerToNextRegularNum(num SnailNum) *RegularNum {
	switch v := num.(type) {
	case *RegularNum:
		return v
	case *SnailPair:
		return GetPointerToNextRegularNum(v[0])
	}
	return nil
}

func Reduce(num SnailNum) (reduced SnailNum) {
	var (
		zero RegularNum
		reducing = true
	)
	reduced = num
	for reducing {
		num, _ = reduced.ExplodeAll(0, [2]*RegularNum{&zero, &zero})
		if num != nil { reduced = num }
		num, reducing = reduced.SplitFirst()
		if num != nil { reduced = num }
	}
	return
}

func Add(first, second SnailNum) SnailNum {
	return Reduce(&SnailPair{first, second})
}

func partTwo(input <-chan string, solutions chan<- string) {
	defer close(solutions)

	var (
		numbers []SnailNum
		max uint
	)
	for numStr := range input {
		numbers = append(numbers, NewSnailNum(numStr))
	}
	for i := range numbers {
		for o := range numbers {
			if i == o { continue }
			if magnitude := Add(numbers[i].Copy(), numbers[o].Copy()).Magnitude(); magnitude > max {
				max = magnitude
			}
		}
	}

	solutions <- fmt.Sprintf("Maximum Pair-added Magnitude: %d", max)
}

func partOne(input <-chan string, solutions chan<- string) {
	defer close(solutions)

	acc := NewSnailNum(<-input)
	for numStr := range input {
		next := NewSnailNum(numStr)
		acc = Add(acc, next)
	}

	solutions <- fmt.Sprintf("Added Magnitude: %d", acc.Magnitude())
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
