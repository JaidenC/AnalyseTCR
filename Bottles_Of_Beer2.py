if __name__ == "__main__":
    beerCount = 99
    while beerCount > 0:
        print(str(beerCount) + " bottles of beer on the wall, " + str(beerCount) + " bottles of beer.")
        beerCount -= 1
        print("Take one down, pass it around, " + str(beerCount) + " bottles of beer on the wall...")
    print("No more bottles of beer on the wall, no more bottles of beer. Go to the store and buy some more, 99 bottles of beer on the wall...")