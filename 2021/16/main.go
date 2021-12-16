package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func Sum(values []uint) uint {
	var sum uint
	for _, value := range values {
		sum += value
	}
	return sum
}

func Product(values []uint) uint {
	var product uint = 1
	for _, value := range values {
		product *= value
	}
	return product
}

func Min(values []uint) uint {
	var min uint
	min -= 1
	for _, value := range values {
		if value < min {
			min = value
		}
	}
	return min
}

func Max(values []uint) uint {
	var max uint
	for _, value := range values {
		if value > max {
			max = value
		}
	}
	return max
}

func Greater(values []uint) uint {
	var min uint
	min -= 1
	for _, value := range values {
		if value < min {
			min = value
		} else {
			return 0
		}
	}
	return 1
}

func Lesser(values []uint) uint {
	var max uint
	for _, value := range values {
		if value > max {
			max = value
		} else {
			return 0
		}
	}
	return 1
}

func Equal(values []uint) uint {
	for _, value := range values {
		if value != values[0] {
			return 0
		}
	}
	return 1
}


var (
	HexMap = map[rune][4]bool{
		'0': [4]bool{false, false, false, false},
		'1': [4]bool{false, false, false, true},
		'2': [4]bool{false, false, true, false},
		'3': [4]bool{false, false, true, true},
		'4': [4]bool{false, true, false, false},
		'5': [4]bool{false, true, false, true},
		'6': [4]bool{false, true, true, false},
		'7': [4]bool{false, true, true, true},
		'8': [4]bool{true, false, false, false},
		'9': [4]bool{true, false, false, true},
		'A': [4]bool{true, false, true, false},
		'B': [4]bool{true, false, true, true},
		'C': [4]bool{true, true, false, false},
		'D': [4]bool{true, true, false, true},
		'E': [4]bool{true, true, true, false},
		'F': [4]bool{true, true, true, true},
	}
	PacketTypeMap = map[uint]func([]uint)uint{
		0: Sum,
		1: Product,
		2: Min,
		3: Max,
		5: Greater,
		6: Lesser,
		7: Equal,
	}

	lenVersion uint = 3
	lenType uint = 3
	lenLength uint = 15
	lenCount uint = 11
	lenLiteralPart uint = 4

	literalType uint8 = 4
)

func HexStringToBitStream(hex string, bits chan<- bool) {
	defer close(bits)
	for _, r := range []rune(hex) {
		for _, bit := range HexMap[r] {
			bits <- bit
		}
	}
}

type Packet struct {
	Version uint8
	Operator func([]uint)uint
	Size uint
	SubPackets []Packet
}

func ParseBlock(transmission <-chan bool, lenBlock uint) (value uint) {
	for i := lenBlock-1; i < lenBlock; i-- {
		if <-transmission {
			value += 1 << i
		}
	}
	return
}

func ParseLiteral(transmission <-chan bool) (literal uint, read uint) {
	var finished bool
	for ! finished {
		finished = ! <-transmission
		literal = (literal << lenLiteralPart) + ParseBlock(transmission, lenLiteralPart)
		read += lenLiteralPart + 1
	}
	return
}

func ParsePacket(transmission <-chan bool) (Packet, uint) {
	var (
		packet Packet
		read uint
		isFunction bool
	)

	packet.Version = uint8(ParseBlock(transmission, lenVersion))
	read += lenVersion
	packet.Operator, isFunction = PacketTypeMap[ParseBlock(transmission, lenType)]
	read += lenType

	if ! isFunction {
		var literalRead uint
		packet.Size, literalRead = ParseLiteral(transmission)
		return packet, read + literalRead
	}
	if read++; <-transmission {
		packet.Size = ParseBlock(transmission, lenCount)
		read += lenCount
		for i := uint(0); i < packet.Size; i++ {
			sub, subRead := ParsePacket(transmission)
			read += subRead
			packet.SubPackets = append(packet.SubPackets, sub)
		}
	} else {
		packet.Size = ParseBlock(transmission, lenLength)
		read += lenLength
		for end := read + packet.Size; read < end; {
			sub, subRead := ParsePacket(transmission)
			read += subRead
			packet.SubPackets = append(packet.SubPackets, sub)
		}
	}
	return packet, read
}

func (packet Packet) Interpret() uint {
	if len(packet.SubPackets) == 0 {
		return packet.Size
	}
	values := make([]uint, len(packet.SubPackets))
	for i, sub := range packet.SubPackets {
		values[i] = sub.Interpret()
	}
	return packet.Operator(values)
}

func partTwo(input <-chan string, solutions chan<- string) {
	defer close(solutions)

	for transmission := range input {
		bits := make(chan bool)
		go HexStringToBitStream(transmission, bits)
		packet, _ := ParsePacket(bits)
		solutions <- fmt.Sprintf("Interpretation: %d", packet.Interpret())
		for bit := range bits { if bit { panic("Non zero bits after end of packet: " + transmission) } }
	}
}

func (packet Packet) SumVersions() (total uint) {
	total += uint(packet.Version)
	for _, sub := range packet.SubPackets {
		total += sub.SumVersions()
	}
	return
}

func partOne(input <-chan string, solutions chan<- string) {
	defer close(solutions)

	for transmission := range input {
		bits := make(chan bool)
		go HexStringToBitStream(transmission, bits)
		packet, _ := ParsePacket(bits)
		solutions <- fmt.Sprintf("Sum of Versions: %d", packet.SumVersions())
		for bit := range bits { if bit { panic("Non zero bits after end of packet: " + transmission) } }
	}
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
