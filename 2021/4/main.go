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

type NumberSet struct {
	Numbers map[uint8]bool
	Boards []*BingoBoard
}

type BingoBoard struct {
	Sets []*NumberSet
}

func CreateNumberSetsInBoards(boardChannel <-chan string) (numberSetsByNumber map[uint8][]*NumberSet, boards map[*BingoBoard]bool) {
	var (
		numberSetsByHash map[string]*NumberSet = make(map[string]*NumberSet)
		rows [][]uint8
	)
	numberSetsByNumber = make(map[uint8][]*NumberSet)
	boards = make(map[*BingoBoard]bool)
	for line := range boardChannel {
		if strings.TrimSpace(line) == "" {
			if len(rows) == 0 {
				continue
			}

			boards[BuildBoard(rows, numberSetsByHash, numberSetsByNumber)] = false
			rows = [][]uint8{}
			continue
		}

		var row []uint8
		for _, space := range strings.Fields(line) {
			number, _ := strconv.Atoi(space)
			row = append(row, uint8(number))
		}
		rows = append(rows, row)
	}
	if len(rows) > 0 {
		boards[BuildBoard(rows, numberSetsByHash, numberSetsByNumber)] = false
	}
	return
}

func BuildBoard(rows [][]uint8, numberSetsByHash map[string]*NumberSet, numberSetsByNumber map[uint8][]*NumberSet) (board *BingoBoard) {
	board = &BingoBoard{}
	for _, row := range rows {
		board.RegisterNumberSet(row, numberSetsByHash, numberSetsByNumber)
	}
	for i := range rows[0] {
		var column []uint8
		for _, row := range rows {
			column = append(column, row[i])
		}
		board.RegisterNumberSet(column, numberSetsByHash, numberSetsByNumber)
	}
	return
}

func (board *BingoBoard) RegisterNumberSet(numbers []uint8, numberSetsByHash map[string]*NumberSet, numberSetsByNumber map[uint8][]*NumberSet) {
	sortedNumbers := make([]uint8, len(numbers))
	copy(sortedNumbers, numbers)
	sort.Slice(sortedNumbers, func(a, b int) bool { return numbers[a] < numbers[b] })
	hash := fmt.Sprintf("%v", sortedNumbers)
	numberSet, present := numberSetsByHash[hash]
	if !present {
		numberSet = &NumberSet{Numbers: make(map[uint8]bool, len(numbers))}
		for _, number := range numbers {
			numberSet.Numbers[number] = false
			numberSetsByNumber[number] = append(numberSetsByNumber[number], numberSet)
		}
		numberSetsByHash[hash] = numberSet
	}
	numberSet.Boards = append(numberSet.Boards, board)
	board.Sets = append(board.Sets, numberSet)
}

func (board *BingoBoard) Score(lastDraw uint8) uint {
	var unmarked uint
	var markedNumbers, unmarkedNumbers []uint8
	for _, numberSet := range board.Sets {
		for number, drawn := range numberSet.Numbers {
			if drawn {
				markedNumbers = append(markedNumbers, number)
			} else {
				unmarkedNumbers = append(unmarkedNumbers, number)
				unmarked += uint(number)
			}
		}
	}
	return (unmarked / 2) * uint(lastDraw)
}

func winBingo(setup <-chan string, solutions chan<- string) {
	drawsStr := <-setup
	numberSetsByNumber, _ := CreateNumberSetsInBoards(setup)

	var (
		bingoedBoards []*BingoBoard
		draw uint8
	)
	for _, drawStr := range strings.Split(drawsStr, ",") {
		bigDraw, _ := strconv.Atoi(drawStr)
		draw = uint8(bigDraw)
		for _, numberSet := range numberSetsByNumber[draw] {
			numberSet.Numbers[draw] = true
			bingo := true
			for _, drawn := range numberSet.Numbers {
				bingo = bingo && drawn
			}
			if bingo {
				bingoedBoards = append(bingoedBoards, numberSet.Boards...)
			}
		}
		if len(bingoedBoards) > 0 {
			break
		}
	}

	var winningScore uint
	for _, board := range bingoedBoards {
		if newScore := board.Score(draw); winningScore < newScore {
			winningScore = newScore
		}
	}
	solutions <- fmt.Sprintf(
		"Final Draw: %d\nWinning Score: %d",
		draw,
		winningScore,
	)
	close(solutions)
}

func loseBingo(setup <-chan string, solutions chan<- string) {
	drawsStr := <-setup
	numberSetsByNumber, boards := CreateNumberSetsInBoards(setup)

	var (
		bingoedBoards []*BingoBoard
		draw uint8
	)
	for _, drawStr := range strings.Split(drawsStr, ",") {
		bigDraw, _ := strconv.Atoi(drawStr)
		draw = uint8(bigDraw)

		bingoedBoards = []*BingoBoard{}
		for _, numberSet := range numberSetsByNumber[draw] {
			numberSet.Numbers[draw] = true
			bingo := true
			for _, drawn := range numberSet.Numbers {
				bingo = bingo && drawn
			}
			if bingo {
				for _, board := range numberSet.Boards {
					if bingoed, _ := boards[board]; bingoed {
						continue
					}
					boards[board] = true
					bingoedBoards = append(bingoedBoards, board)
				}
			}
		}
		var remainingBoards bool
		for _, bingoed := range boards {
			if !bingoed {
				remainingBoards = true
			}
		}
		if !remainingBoards {
			break
		}
	}

	var losingScore uint
	for _, board := range bingoedBoards {
		if newScore := board.Score(draw); losingScore == 0 || losingScore > newScore {
			losingScore = newScore
		}
	}
	solutions <- fmt.Sprintf(
		"Final Draw: %d\nLosing Score: %d",
		draw,
		losingScore,
	)
	close(solutions)
}

func main() {
	parts := []func(<-chan string, chan<- string) {
		winBingo,
		loseBingo,
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
