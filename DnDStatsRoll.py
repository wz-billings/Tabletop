import random

#This program will roll stats for 5th Edition Dungeons and Dragons,
#with the following parameters:
# 1. Stats are rolled in order down the list.
# 2. Each stat is rolled using 4d6 drop lowest.
# 3. Two stats must be 15 or higher.
# 4. All stats must sum to at least 85. (83 = sum of standard array.)

#Define a function that generates a random number for a "dice roll".
def d6():
    DieRoll = random.randint(1,6) #Random integer between 1 and 6.
    return DieRoll #return the value of the roll as output.

#Define a function that generates a list of 4 dice, equal to rolling 4d6.
#Add the list together, and subtract the lowest value = 4d6 drop lowest.
def StatRoll():
    DieRoll = [d6(), d6(), d6(), d6()] #Creates list of 4 d6 rolls.
    SumTotal = sum(DieRoll) - min(DieRoll) #Adds sum and subs the lowest.
    return SumTotal #return the sum value as the output.

#Define a function that runs StatRoll 6 times and returns a dictionary,
#Where the key is the stat name, and the value is the numerical value for
#that statistic.

def RollAttributes():
     return {
        "Str": StatRoll(),
        "Dex": StatRoll(),
        "Con": StatRoll(),
        "Int": StatRoll(),
        "Wis": StatRoll(),
        "Cha": StatRoll(),
    }

#Define parameters that make a set of stats "valid", i.e.
# They must sum to at least 83; the sum total of the Standard Array.
# And two of them must be above 15
# These parameters are easily adjusted below:

SumAttributesTotal = 85 #The number the stats must add up to.
NumberGoodStats = 2 #This many stats have to be above a certain value.
GoodStatValue = 15 #This is the value those stats must be >=.

#This function will take a dictionary of attributes as an input,
#and will determine if it is valid according to the above parameters:

def AttributesAreValid(attributes):
    #Use a filter lambda to determine if two values in the dictionary
    #"attributes" are >= GoodStatValue. It saves each stats
    #meeting the criterion into a list.
    GoodStats = filter(lambda x: x >= GoodStatValue, attributes.values())
    #If the length of the list of stats meeting the criterion is sufficient,
    #Set this criterion to true; otherwise set it to false.
    if len(list(GoodStats)) == NumberGoodStats:
        GoodStatCondition = 1
    else:
        GoodStatCondition = 0

    #Make a list of the dictionary values, and sum them together.
    #If they are == the sum criterion, make it true;
    #Else make it false.
    if sum(list(attributes.values())) == SumAttributesTotal:
        SumCondition = 1
    else:
        SumCondition = 0

    #Perform the logical and operator on the two criteria and return
    #The result, a logical value.
    return GoodStatCondition #and SumCondition

#Define a function that repeatedly calls the RollAttributes and
# AttributesAreValid functions until AttributesAreValid returns True;
# Then print all of the keys and values in the valid dictionary.
def GenerateStats():
    #Define a dictionary equal to the output of RollAttributes
    attributes = RollAttributes()

    #Continue to redine the dictionary using the same function until
    #AttributesAreValid called on the dictionary returns True.
    while not AttributesAreValid(attributes):
        attributes = RollAttributes()

    #Print "Attributes" dictionary in a nice pretty way.
    print('''\
        Str: {Str}
        Dex: {Dex}
        Con: {Con}
        Int: {Int}
        Wis: {Wis}
        Cha: {Cha}\
        '''.format(**attributes))

#Run the GenerateStats function.
GenerateStats()
