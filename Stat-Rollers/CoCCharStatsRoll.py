import random

def d6():
    dieroll = random.randint(1,6)
    return dieroll

def PrimaryStatRoll():
    dieroll = d6() + d6() + d6()
    return dieroll

def SecondaryStatRoll():
    dieroll = d6() + d6() + 6
    return dieroll

def RollAttributes():
     return {
        "str": PrimaryStatRoll()*5,
        "dex": PrimaryStatRoll()*5,
        "int": SecondaryStatRoll()*5,
        "con": PrimaryStatRoll()*5,
        "app": PrimaryStatRoll()*5,
        "pow": PrimaryStatRoll()*5,
        "siz": SecondaryStatRoll()*5,
        "edu": (PrimaryStatRoll() + 3)*5,
    }

#parameters for valid attribues to have
SumAttributesTotal = 460

def AttributesAreValid(attributes):
    if sum(list(dict.values(attributes))) == SumAttributesTotal:
        SumCondition = 1
    else:
        SumCondition = 0

    if attributes["int"] >= 40 and attributes["siz"] >= 40:
        IntSizCondition = 1
    else:
        IntSizCondition = 0

    for stat in attributes:
        if not(attributes.get(stat) > 90 and attributes.get(stat) < 15):
            BoundCondition = 1
        else:
            BoundCondition = 0
            break

    return (SumCondition and IntSizCondition and BoundCondition)

def GenerateStats():
    attributes = RollAttributes()
    while not AttributesAreValid(attributes):
        attributes = RollAttributes()
    print('''\
        str: {str}
        dex: {dex}
        int: {int}
        con: {con}
        app: {app}
        pow: {pow}
        siz: {siz}
        edu: {edu}\
        '''.format(**attributes))

GenerateStats()
