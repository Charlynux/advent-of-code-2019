package main

import (
        "bufio"
        "fmt"
        "io"
        "os"
        "strconv"
)

func fuelRequired(mass int) int {
        return ((mass / 3) - 2)
}

func fuelRequired2(total int, mass int) int {
        if (mass <= 0) {
                return total
        } else {
                return fuelRequired2(total + mass, fuelRequired(mass))
        }
}

func main() {
        file, err := os.Open("input")
        if err != nil {
                panic(err)
        }
        
        defer file.Close()
        
        reader := bufio.NewReader(file)
        
        result := 0
        result2 := 0
        for {
                line, _, err := reader.ReadLine()
                
                if err == io.EOF {
                        break
                }
                
                value, _ := strconv.Atoi(string(line))
                result += fuelRequired(value)
                result2 += fuelRequired2(0, fuelRequired(value));
        }
        fmt.Printf("%d \n", result)
        fmt.Printf("%d \n", result2)
}