#foraging society


#generate person
person_gen <- function(map) {
  posistion <- round(runif(1,min=1,max=length(map)))
  posistion_vector <- posistion
  caloric_requirement <- sample(c(1,2,3),1,prob=c(.6,.4,.2))
  max_food <- 10 + floor(runif(1,min=0,max=12))*caloric_requirement/1.67
  food <- sample(c(1:5),1)
  food_vector <- food
  return(list("posistion" = posistion,
              "posistion_vector" = posistion_vector,
              "caloric_requirement"=caloric_requirement,
              "food" = food,
              "max_food" = max_food,
              "food_vector" = food_vector))
  
}
#generate a plant
plant_gen <- function(map) {
  posistion <- sample(map,1)
  fruit <- sample(c(1:5),1)
  growth_rate <- runif(1,min=1,max=2.5)
  max_growth <- 100 + ceiling(runif(1,min=0,max=100))
  blight_rate <- runif(1,min=.1,max=.3)
  fruit_vector <- fruit
  return(list("posistion" = posistion,
              "growth_rate"=growth_rate,
              "max_fruit"=max_growth,
              "blight_rate" = blight_rate,
              "fruit" = fruit,
              "fruit_vector" = fruit_vector))
}
#simulate a period
simulate <- function(map,person1,plant1) {
  
  #person
  #eat
  if(person1$food > 0){
  person1$food <- person1$food - person1$caloric_requirement
  }
  #movement
  move <- round(runif(1))
  if(person1$posistion == plant1$posistion) move <- sample(c(0,1),1,prob=c(.8,.2))
  if(move == 1 & person1$posistion != min(map) & person1$posistion != max(map)) person1$posistion <- person1$posistion + sample(c(-1,1),1)
  if(move == 1 & person1$posistion == min(map)) person1$posistion <- person1$posistion + 1
  if(move == 1 & person1$posistion == max(map)) person1$posistion <- person1$posistion - 1
  person1$posistion_vector <- c(person1$posistion_vector,person1$posistion)
  #gathering
  if(person1$posistion == plant1$posistion){
    fruit_gathered <- sample(plant1$fruit,1)
    fruit_gathered <- min(person1$max_food-person1$food,fruit_gathered)
    if(fruit_gathered == plant1$fruit) fruit_gathered = sample(c(fruit_gathered, fruit_gathered-1),1)
    person1$food <- min(person1$max_food,person1$food + fruit_gathered)
    plant1$fruit <- plant1$fruit - fruit_gathered
  }
  person1$food_vector <- c(person1$food_vector,person1$food)
  #offspring
  
  #plant
  #generate fruit - one plant for now
  plant1$fruit  <- min(plant1$max_fruit,floor(plant1$fruit * plant1$growth_rate))
  plant1$fruit <- ceiling(plant1$fruit*sample(c(.5,1),1,prob=c(plant1$blight_rate,1-plant1$blight_rate)))
  plant1$fruit_vector  <- c(plant1$fruit_vector ,plant1$fruit)
  return(list(person1,plant1))
}

#generate world
gen_world <- function(){
max_rounds = 100
map <- 1:5
persons <- list()
#generate people
person1 <- person_gen(map);person1
#generate plants
plant1 <- plant_gen(map);plant1
plant2 <- plant_gen(map);plant2
plants <- list(plant1,plant2)
i<-0
while(person1$food >0 & i < max_rounds) {
  i <- i + 1
round <- simulate(map,person1,plants)
person1 <- round[[1]]
plant1 <- round[[2]]
}
person1$food_vector
plant1$fruit_vector
person1$posistion_vector
df <- as.data.frame(cbind(1:length(person1$food_vector),person1$food_vector,plant1$fruit_vector,person1$posistion_vector))
}
df <- gen_world()
names(df) <- c("period","food","fruit","posistion")
ggplot(data=df) +
  geom_line(aes(x=period,y=food)) +
  geom_line(aes(x=period,y=fruit),linetype="dashed") + 
  scale_x_continuous(breaks=c(1:nrow(df))) +
  scale_y_continuous(breaks=c(0:max(df$food,df$fruit)))

ggplot(data=df) +
  geom_line(aes(x=period,y=posistion)) +
  scale_x_continuous(breaks=c(1:nrow(df))) +
  scale_y_continuous(breaks=c(1:5))
