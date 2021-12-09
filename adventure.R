## Text adventure in R

## Initiate game
verbs <- c("look", "take", "put", "use", "wait", "kill", "quit")
moves <- c("north", "south", "east", "west", "up", "down")
map <- read.csv("~/Desktop/adventure/map.csv")
objects <- read.csv("~/Desktop/adventure/objects.csv", stringsAsFactors = FALSE)
actions <- readLines("~/Desktop/adventure/actions.md")
library(crayon)
room <- 2
health <- 6
capacity <- 6
move <- 0
obedience <- 1

## Displays relevant prose (descriptions and results of actions)
prose <- function(key) {
    key <- tolower(key)
    line <- which(actions == paste("##", key)) + 1
    if(length(line) == 0)
        return()
    while(substr(actions[line], 1, 1) != "#") {
        cat(paste(actions[line], "\n"))
        line <- line + 1
    }
    cat("\n")
}

## List objects carried by player
inventory <- function() {
    stuff <- subset(objects, type == "object" & location == 0)$name
    if (length(stuff) > 0)
        cat(paste0("You have a ", paste(stuff, collapse = ", "), ".\n\n"))
}

## Show health, capacity and objects from inventory

showStatus <- function() {
    # informs health
    cat(paste0("You have ", health, " points of health\n"))
    # informs capacity
    cat(paste0("You can carry ", capacity, " objects\n"))
    # informs on the inventory
    inventory()
}

## Describe the local surroundings
look <- function(room) {
    ## Describe
    prose(paste("room", room))
    ## Objects
    stuff <- subset(objects, type == "object" & location == room)$name
    if (length(stuff) != 0)
        cat(paste0("You see a ", paste(stuff, collapse = ", "), ".\n\n"))
    inventory()
    ## Voice
    for (r in c(1,3,6,8,11)) {
      if (room == r)
      obedience <<- 0
    }
    if (obedience == 1) {
      if (room == 5)
        prose("voice5")
      if (room == 4)
        prose("voice4")
      if (room == 7)
        prose("voice7")
    } else if (obedience == 0 & room < 12)
        prose("silence")
      else if (room == 24){
        if (obedience == 0) {
          prose("thrown out")
          health <<- health - 1
          room <<- 3
          look(3)
        } else if (obedience == 2) {
          prose("voice24")
          declaration <- readline(prompt = "Will you leave the facility? ")
          if (declaration == "yes") {
            prose("victory")
            health <<- 99 
          } else if (declaration == "no") {
            prose("return")
            readline()
            prose("opening")
            room <<- 2
            health <<- 6
            capacity <<- 6
            move <<- 0
            obedience <<- 1
            objects$health[8] <- 1
            objects$health[7] <- 5
            objects$location[1] <- 5
            objects$location[2] <- 99
            objects$location[3] <- 99
            objects$location[4] <- 11
          } else {
            cat("No muttering now. Look at what's out there, and then say yes or no firmly. Everything is on the line.\n\n")
            room <<- 24
          }
        }
      }
    ## Directions
    passages <- moves[which(map[map$room == room, 3:8] != 0)]
    if (room != 24) {
      cat(paste0("You can go ", paste(passages, collapse = ", "), ".\n\n"))
    }
}

## Take an object
take <- function(object) {
    if (is.na(object)) {
        return(cat("You cannot take this.\n"))}
    stuff <- subset(objects, type == "object" & location == room)$name
    if (object %in% stuff) {
        ob_num <- which(objects$name == object)
        if (capacity - objects$weight[ob_num] < 0)
            return(cat(paste0("You cannot carry the ", object, ".\n")))
        objects$location[ob_num] <<- 0
        capacity <<- capacity - objects$weight[ob_num]
        cat(paste0("You take the ", object, ".\n"))
        inventory()
    } else {
        cat(paste0("You cannot take the ", object, ".\n"))
    }
}

## Put and object on the ground
put <- function(object)  {
    stuff <- subset(objects, type == "object" & location == 0)$name
    if (object %in% stuff) {
        ob_num <- which(objects$name == object)
        objects$location[ob_num] <<- room
        capacity <<- capacity + objects$weight[ob_num]
        cat(paste0("You place the ", object, " on the ground.\n"))
        inventory()
        }
    else
      cat(paste0("You don't have the ", object, ".\n"))
}

## Use an object
use <- function(object) {
    if (is.na(object)) {
        return(cat("You don't have that.\n"))
    } else if (objects[which(objects$name == object), "location"] != 0) {
        return(cat(paste0("Your hand closes on the air. You blink. A ", object, "? Oh, right. You don't have one of those.\nWhy would you think you did?\n\n")))
    } else switch (object,
                   patch = { # if object is patch
                       objects$location[1] <<- 99
                       capacity <<- capacity + objects$weight[1]
                       if (room == objects$location[8] & objects$health[8] > 0) {
                           prose("heal android")
                           objects$status[8] <<- 30
                       } else {
                           if (health == 6) {
                               prose("eat patch")
                           } else {
                               prose("use patch")
                               health <<- 6
                           }
                       }
                   },
                   sword = { # if object is sword
                       prose("fingernails")
                       
                   },
                   code = { # if object is code
                       if (room == 21) { ## use the code by slot
                           prose("code1")
                           prose("code2")
                           room <<- 24
                           obedience <<- 2
                       } else 
                           cat("There's no way to use it here.\n")
                   },
                   screwdriver = { # if object is screwdriver
                       if (room == 14) {
                           prose("screwdriver")
                           ## Create new room connections
                           map$up[14] <<- 15
                           map$down[15] <<- 14
                           ## screwdriver is no longer an object
                           objects$location[4] <<- 99
                           capacity <<- capacity + objects$weight[4]
                           ## New room descriptions
                           actions[which(actions == "## screwdriver in tree")] <<- "\n"
                           ## Look
                           look(room)
                       } else {
                           prose("skipping")
                           health <<- health - 1
                       }# if it is something else
                   },
                   )
}

## Wait for situation to change
wait <- function(dummy) {
    cat("You wait a little while ...\n\n")
}

## Kill something
kill <- function(object) {
    opponent <- which(objects$name == object)
    weapon <- ifelse(objects$location[2] == 0, "sword", "fists")
    ## is opponent listed in objects?
    if (length(which(objects$name == object)) == 0) {
        return(cat("You really shouldn't do that.\n"))
    ## Existing actor?
    } else if (is.na(object) | objects$type[opponent] != "actor") {
        return(cat("Woah. You really need to calm down. You're kind of losing it.\n"))
    ## No opponent present
    } else if (objects$location[opponent] != room) {
        return(prose(paste("shadow fight", weapon)))
    ## Dead opponent
    } else if  (objects$health[opponent] <= 0) {
        return(cat(paste0("You poke the dead ", object, ".\n")))
    ## everything else
    }
     
    ## Fight sequence
    strength <- ifelse(weapon == "sword", 2, 1)
    cat(paste0("You attack the ", object, " with your ", weapon, ".\n\n"))
    if (runif(1, 0, 1) > 0.3) {
        objects$health[opponent] <<- objects$health[opponent] - strength
        if (objects$health[opponent] == 1)
            cat(paste0("The ", object, " is heavily wounded.\n"))
        if (objects$health[opponent] <= 0 & room != 10) {
            cat(paste0("The ", object, " succumbs to the blows of your ", weapon, ".\n"))
        }         
    } else
        cat(paste0("You miss. You don't manage to hit the ",
                   object, ".\n\n"))     
}

## Move player
walk <- function(direction) {
    r <- map[map$room == room, direction]
    if (r != 0)
        room <<- r
    else
        cat("You can't go that way.\n")
    look(room)
}

## Actors
actors <- function() {
    ## computer (object 5) hands the code and vanishes
    if (room == objects$location[5]) {
        prose("computer")
        objects$location[5] <<- 100 ## computer vanishes
        objects$location[3] <<- 0 ## You have the code
        health <<- health - objects$weight[3]
        inventory()
    }
    ## robot (object 7)
    if (objects$health[7] <= 0 & room == 17)
        prose("dead robot")
    if (objects$health[7] > 0 & room == 17) {
        prose("robot attack")
        if (runif(1, 0, 1) < 0.5) {
            prose("robot hit 1")
            health <<- health - 2
        } else
            prose("robot miss")   
    }
    if (objects$health[7] > 0 & room == 18) {
        prose("robot hit 2")
        room <<- 16
        health <<- health - 1
    }    
    ## android (object 8)
    if (room == objects$location[8]) {
        if (objects$health[8] <= 0) {
            prose("dead android")
            readline(prompt = "What would you like to do? : ")
            readline(prompt = "What would you like to do? : ")
            user <- list.files("/Users")
            cat(paste0("What are you trying to do ", user[1], "?\n"))
            readline(prompt = "You don't know what to do.")
            readline(prompt = "But I do.")
            readline(prompt = "I can take over from here.")
            prose("brainwash")
            readline()
            prose("opening")
            room <<- 2
            health <<- 6
            capacity <<- 6
            move <<- 0
            obedience <<- 1
            objects$health[8] <<- 1
            objects$location[1] <<- 5
            objects$location[2] <<- 99
            objects$location[3] <<- 99
            objects$location[4] <<- 11
        } else {
            if (objects$status[8] == 60) {
                if (room == objects$location[8]) {
                    prose("android dies")
                    objects$health[8] <<- 0
                    objects$status[8] <<- 0
                }
            }
            if (objects$status[8] == 50) {
                if (room == objects$location[8]) {
                    prose("android gives sword")
                    objects$location[2] <<- 11
                    objects$location[8] <<- sample(1:9, 1)
                    objects$status[8] <<- 70
                    look(room)
                }
            }
            if (objects$status[8] >= 40 & objects$status[8] < 50) {
                objects$status[8] <<- objects$status[8] + 1
                if (room == objects$location[8])
                    prose("android forges sword")
                if (objects$status[8] == 42)
                    objects$status[8] <<- 50
            }
            if (objects$status[8] == 30) {
                prose("android healed")
                objects$status[8] <<- 40
                objects$location[8] <<- 11
            }
            if (objects$status[8] >= 20 & objects$status[8] < 30) {
                prose("android pleading")
                objects$status[8] <<- objects$status[8] + 1
                if (objects$status[8] == 25)
                    objects$status[8] <<- 60
            }
            if (objects$status[8] == 10) {
                prose("android wounded")
                objects$status[8] <<- 20
            }
        }
    }
}

## Game Play loop
while (health > 0 & health < 99) {
    if (move == 0)
        prose("opening")
    verb <- NA
    direction <- NA
    object <- NA
    cat("----------------------------------------\n")
    command <- readline(prompt = "What would you like to do? : ")
    command <- tolower(command)
    words <- unlist(strsplit(command, " "))
    verb <<- verbs[verbs %in% words][1] # First valid verb in the list
    direction <<- moves[moves %in% words][1] # First valid direction in the list
    object <<- objects$name[objects$name %in% words][1] # First valid object
    cat("\n")
    if (!is.na(direction)) {
        walk(direction)
    }
    if (!is.na(verb)) {
        if (verb == "look")
            arg <- room
        else if (verb == "quit") {
          cat("You've only been here for a short while, but it feels like you've been here forever. You're tired. So, so tired. You just want to be done, at any cost.\n")
          cat(red("E̴̼͔̎R̵̝̎R̶̜̐O̷̻͘Ṟ̸͂.̵͚̯͊̀ ̸̬̕É̶̳̠R̷̭̉R̵̭͌̋O̶͓͠R̷̭̎̎.̷̺̔
R̷̨̡̡̩͍̙̦̠̩͈̦͈̦͈̅͂̽E̸̬̜̐-̷̨̬̩̯̻̮̘̜̙̰̩͌͋̌̐L̴̡̡̰̟̰̱̫̻͔͈̘̙̄́̈́͑͋͜Ô̵̤͋̽̋́͆̈́̈́̂̂́̆͂̆̿͝A̶̢̡͓̥̬͔̩̣͔̓̿̋̀͛͒̐͐̽̀͂̽̾̏͝D̶̨̢̬͎̼̟̭̼̫̟̘̭̼̱͔̞̈́̑͂̏̃̈́̈́̋̂͛̏̍̍͜͠I̴̢̛̥͙͎̝̮͈̺̝̳̊̓̏͐͋̍́̊̈́̏̽̿̀̂̅͋Ņ̵̲̹̦̭̳̭̯͓̠̟̺̟̝̥͈͇̈́G̶̨̧̥̬̜̣̘͎͔̲͓̯͎̗̻͆̎̄̽̒͆́̏̐͛̀̈̆̾̕͘͝.̷̨̛̞̫͖̪͇̣̳̱̱͓͍̑̇̒̃̓͛.̸̻̰̯̦̪̙͉̹̯͈̮̩̱͔̌͂̅͂̄̉͗̉͐͜͠.̴̨̛͇̝͉̘̍̅̍͂̌͐̀̔́͠"))
          break
        }
        else 
            arg <- object
        do.call(verb, list(arg))
    }
    if (is.na(direction) & is.na(verb))
        cat("You are talking nonsense. This place is really getting to you, huh?\n\n")
    else {
        actors()
        if(health <= 0){
          prose("death")
          cat(red("E̴̼͔̎R̵̝̎R̶̜̐O̷̻͘Ṟ̸͂.̵͚̯͊̀ ̸̬̕É̶̳̠R̷̭̉R̵̭͌̋O̶͓͠R̷̭̎̎.̷̺̔
R̷̨̡̡̩͍̙̦̠̩͈̦͈̦͈̅͂̽E̸̬̜̐-̷̨̬̩̯̻̮̘̜̙̰̩͌͋̌̐L̴̡̡̰̟̰̱̫̻͔͈̘̙̄́̈́͑͋͜Ô̵̤͋̽̋́͆̈́̈́̂̂́̆͂̆̿͝A̶̢̡͓̥̬͔̩̣͔̓̿̋̀͛͒̐͐̽̀͂̽̾̏͝D̶̨̢̬͎̼̟̭̼̫̟̘̭̼̱͔̞̈́̑͂̏̃̈́̈́̋̂͛̏̍̍͜͠I̴̢̛̥͙͎̝̮͈̺̝̳̊̓̏͐͋̍́̊̈́̏̽̿̀̂̅͋Ņ̵̲̹̦̭̳̭̯͓̠̟̺̟̝̥͈͇̈́G̶̨̧̥̬̜̣̘͎͔̲͓̯͎̗̻͆̎̄̽̒͆́̏̐͛̀̈̆̾̕͘͝.̷̨̛̞̫͖̪͇̣̳̱̱͓͍̑̇̒̃̓͛.̸̻̰̯̦̪̙͉̹̯͈̮̩̱͔̌͂̅͂̄̉͗̉͐͜͠.̴̨̛͇̝͉̘̍̅̍͂̌͐̀̔́͠"))
          readline(prompt = "What's happening\n")
          readline(prompt = "It hurts\n")
          prose("opening")
          room <- 2
          health <- 6
          capacity <- 6
          move <- 0
          obedience <- 1
          objects$health[8] <- 1
          objects$health[7] <- 5
          objects$location[1] <- 5
          objects$location[2] <- 99
          objects$location[3] <- 99
          objects$location[4] <- 11
          }
    }
    move <- move + 1
}
