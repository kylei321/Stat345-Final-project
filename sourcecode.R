
# Set up links to reviews ----------------------------------------------------------


# 1094 pages with useful reviews
pre <- "https://www.winespectator.com/dailypicks/category/catid/1/page/"
post <- 1:200
links <- paste(pre, post, sep = "")
links <- as.data.frame(links)


# Scrape Reviews ----------------------------------------------------------


gather_reviews <- function(url) {
  page <- read_html(url)
  reviews <- html_nodes(page, ".mr-md-32")
  reviews <- html_text(reviews, trim = TRUE)
  
  # Splits the long string of all reviews into separate reviews (using the date as the splitter)
  dates <- str_extract_all(reviews, "(.){4}\\s(\\d{2}),\\s(\\d{4})")
  reviews <- str_split(reviews, "(.){4}\\s(\\d{2}),\\s(\\d{4})")
  
  # Adds date back to review and converts to vector
  dates <- unlist(dates)
  dates <- str_c(dates, " ")
  reviews <- unlist(reviews)
  reviews <- reviews[-1]
  reviews <- str_trim(reviews)
  reviews <- str_c(dates, reviews)
}


# Clean up reviews and separate into different variables ------------------


# Get reviews for every link and then convert to a data frame with one review per row
all_reviews <- apply(links, 1, gather_reviews) %>% as.data.frame()
all_reviews <- pivot_longer(all_reviews, V1:V200, values_to = "Reviews")
all_reviews <- all_reviews[, -1]

# Convert to strings, remove extra garbage (\n and spaces, spaces > 4 replaced by '~')
all_reviews[] <- lapply(all_reviews, as.character)

for (i in 1:1000) {
  all_reviews[i,1] <- str_replace_all(all_reviews[i,1], "[\n]", "")
  all_reviews[i,1] <- str_replace_all(all_reviews[i,1], "\\s{4,}", "~")
}

# Seperate into columns by date/rating/name and type/review (seperation character is '~')
all_reviews <- all_reviews %>% separate(Reviews, c("Date", "Score", "Maker", "Name", "Price", "Review", "Reviewer"), sep = "~", extra = "merge")
all_reviews <- all_reviews %>% separate(Date, c("Date", "Score"), sep = -2, remove = FALSE)

# Creates a variable for the last 2 sentences
all_reviews <- all_reviews %>% mutate(last2 = " ")

# Loops for each wine review and extracts the last 2 sentences and puts it into the variable last2
for (i in 1:1000){
  x <- str_locate(all_reviews$Review[i], "Drink")
  all_reviews$last2[i] <- str_sub(all_reviews$Review[i], start = x[1])
}

# Removes the last two sentences from Review
all_reviews <- all_reviews %>% mutate(Review = str_remove(Review, last2))

# Removes the rows that contain NA values
all_reviews <- na.omit(all_reviews)

# Seperates the last2 variable into Drink, Cases and Extra
all_reviews <- all_reviews %>% separate(last2, c("Drink", "Cases", "Extra"), sep = "\\. ", fill = "right")

# Determines which rows have additional pieces in the last 2 sentences
extra <- which(complete.cases(all_reviews))

# Removes the rows that have additional pieces in the last 2 sentences 
all_reviews <- all_reviews[-extra, ]

# Removes the variable Extra which contains all NA values
all_reviews <- subset(all_reviews, select = -Extra)
# Removes the dollar sign from the price variable and makes it numeric.
all_reviews <- all_reviews %>% mutate(Price = as.numeric(str_remove_all(Price, "\\$")))

# Turns the date variable from a character into a Date
all_reviews <- all_reviews %>% mutate(Date = mdy(Date))

# Replaces the dashes in Names with a space
all_reviews <- all_reviews %>% mutate(Name = str_replace_all(Name, "-", " "))

# Removes the dashes in front of the Reviewer
all_reviews <- all_reviews %>% mutate(Reviewer = substring(Reviewer, 2))

# Seperates the punctuation under the Review variable by adding a space before and after the punctuation.
all_reviews <- all_reviews %>% mutate(Review = str_replace_all(Review, ",", " ,")) %>% mutate(Review = str_replace_all(Review, "\\.", " .")) %>% mutate(Review = str_replace_all(Review, "-", " - "))

# Seperates the punctuation under the Cases variable by adding a space before and after the punctuation.
all_reviews <- all_reviews %>% mutate(Cases = str_replace_all(Cases, ",", " , ")) %>% mutate(Cases = str_replace_all(Cases, "\\.", " .")) %>% mutate(Cases = str_replace_all(Cases, "-", " - "))


# Wine types dataframe ----------------------------------------------------

# Create dataframe of types of wine/color
reds <- c("Gamay", "Puglia", "Dão", "Alentejo", "Primitivo", "Sirah", "Ventoux", "Rosso", "Terre", "Chianti", "Beaujolais", "Shiraz", "Garnacha", "Nemea", "Pinot Noir", "Barbera", "Cabernet Franc", "Cabernet", "Alentejano", "Grenache", "Cabernet Sauvignon", "Malbec", "Merlot", "Nebbiolo", "Sangiovese", "Syrah", "Tempranillo", "Bordeaux", "Zinfandel", "Carignan", "Carménère", "Mencia", "Montepulciano", "Negroamaro", "Rhône", "Valpolicella", "Anglianico", "Mourvèdre", "Nero d'Avola", "Petit Verdot", "Petit Sirah", "Pinotage", "Touriga Nacional", "Red", "Maderia", "Marsala", "Port", "Sauternais", "Vin Santo")

whites <- c("Alberiño", "Blanc", "Moscato", "Verdejo", "Vinho", "Chardonnay", "Rioja", "Chenin Blanc", "Douro", "Gewürztraminer", "Pinot Grigio", "Orvieto", "Pinot Gris", "Riesling", "Sauvignon Blanc", "Viognier", "Muscadet", "Grüner Veltliner", "Soave", "Brut", "Vermentino", "Marsanne", "Sémillon", "Muscat Blanc", "Mosato", "Torrontés", "White", "Prosecco", "Nîmes", "Champagne", "Cava", "Lambrusco")

rose <- ("Rosé")

wine_typeR <- data.frame("Type" = reds, "Color" = "Red")
wine_typeW <- data.frame("Type" = whites, "Color" = "White")
wine_type_rose <- data.frame("Type" = rose, "Color" = "Rosé")
wine_types <- rbind2(wine_typeR, wine_typeW, by = "Type")
wine_types <- rbind2(wine_types, wine_type_rose, by = "Type")

# Changes all wine types to be characters
wine_types <- wine_types %>% mutate(Type = as.character(Type), Color = as.character(Color))
wine_typeR <- wine_typeR %>% mutate(Type = as.character(Type), Color = as.character(Color))
wine_typeW <- wine_typeW %>% mutate(Type = as.character(Type), Color = as.character(Color))
wine_type_rose <- wine_type_rose %>% mutate(Type = as.character(Type), Color = as.character(Color))


# Adds Color Variable ----------------------------------------------------

all_reviews <- all_reviews %>% mutate(Color = "")
for(i in 1:length(all_reviews$Name)){
  wine <- all_reviews$Name[i]
  if(any(str_detect(wine, wine_typeR$Type[1:length(wine_typeR$Type)]) == "TRUE")){
    all_reviews$Color[i] <- "Red"
  }
  else if(any(str_detect(wine, wine_typeW$Type[1:length(wine_typeW$Type)]) == "TRUE")){
    all_reviews$Color[i] <- "White"
  }
  else if(any(str_detect(wine, wine_type_rose$Type[1:length(wine_type_rose$Type)]) == "TRUE")){
    all_reviews$Color[i] <- "Rosé"
  }
  else{
    all_reviews$Color[i] <- "NA"
  }
}

# Set up review word pairs and unique word list --------------------------------------

# Filters all_reviews for the wine color that appears the most
if(length(which(all_reviews$Color == "White")) >= length(which(all_reviews$Color == "Red"))){
  all_reviews <- all_reviews %>% filter(Color == "White")
  type <- "White"
} else if(length(which(all_reviews$Color == "Red")) >= length(which(all_reviews$Color == "White"))){
  all_reviews <- all_reviews %>% filter(Color == "Red")
  type <- "Red"
} else{
  all_reviews <- all_reviews %>% filter(Color == "Rosé")
  type <- "Rosé"
}

# Split words into pairs
review_w <- all_reviews["Review"]
review_w <- review_w %>% unnest_tokens(words, Review, token = "words", strip_punct = FALSE)
word_2 <- review_w[-1,1]
rows <- nrow(word_2) + 1
word_2[rows,1] = " "
word_3 <- word_2[-1,1]
word_3[rows,1] = " "
review_w <- cbind(review_w, word_2)
colnames(review_w) <- c("Word 1", "Word 2")

pairs <- review_w

# Finds all unique words
test <- all_reviews["Review"]
test2 <- test %>% unnest_tokens(words, Review, token = "words", strip_punct = FALSE)
unique <- test2 %>% distinct()


# Build list function -----------------------------------------------------


# Build lists with probabilities initialized to 0
build_list <- function(word) {
  init <- replicate(nrow(unique), 0)
  probs <- data.frame(start = init)
  colnames(probs) <- word
  row.names(probs) <- unlist(unique)
  return(probs)
}


# Find first order probabilities ------------------------------------------


probabilities <- apply(unique, 1, build_list)
names(probabilities) <- unlist(unique)

# Counts each occurence of word pairs
for(i in 1:nrow(pairs)) {
  probabilities[[pairs$`Word 1`[i]]][pairs$`Word 2`[i], pairs$`Word 1`[i]] <- probabilities[[pairs$`Word 1`[i]]][pairs$`Word 2`[i], pairs$`Word 1`[i]] + 1
}

#Find total times first word appears
totals <- lapply(probabilities, function(x) { sum(x, na.rm=TRUE)})
names(totals) <- unlist(unique)

# Finds new probabilities (word pair count/first word count)
for(i in 1:length(probabilities)) {
  probabilities[[i]] <- probabilities[[i]] / totals[names(probabilities)[i]]
}

# Removes extra cell from "." probabiltiies--not sure why it's there yet
probabilities[["."]] <- probabilities[["."]][-nrow(probabilities[["."]]),]


# Finds average number of words per review --------------------------------

lengths <- sapply(all_reviews$Review, function(x) { lengths(strsplit(x, "\\W+")) })
average_words <- mean(lengths)
# Average number of words per review is around 22.5 roughly.
# Use 50 for a limit (our reviews won't perfectly emulate the originals), so 25 words per sentence

# Generate review and build sentence functions ----------------------------


# Builds a review
# Start word is the starting word for the review, num_sentences is the number of sentences the review should have, transition_probs is the probability table for the starting word
generate_review <- function(start_word, num_sentences, prob_list, chain) {
  # Set up variables for the review being generated and the total sentences generated
  review <- start_word
  # transition_probs <- prob_table[[start_word]]
  total_sent <- 0
  
  # While the number of sentences generated is less than number of sentences desiered, continue building sentences
  for(i in 1:num_sentences) {
    review <- build_sentence(start_word, prob_list, review, chain)
    if(chain==1) {
      start_word <- review[length(review)]
    }
    else {
      start_word <- str_c(review[length(review)-1], review[length(review)])
    }
    # transition_probs <- probabilities[["."]]
  }
  
  # Capitalize first word and words after periods
  review[1] <- str_to_title(review[1])
  ends <- which(review == ".") + c(1)
  ends <- ends[-length(ends)]
  review[ends] <- str_to_title(review[ends])
  
  # Join vector into one string
  review <- str_c(review, collapse = " ")
  return(review)
}

# Builds a single sentence for a review.
# `start_word` is the current state, transition_probs is the probability table for the start word, gen_review is the sentence currently being generated
# Helper to the generate_review function
build_sentence <- function(start_word, prob_list, gen_review, chain) {
  # Picks next word by sampling from all unique words with proability determind by our probability table
  transition_probs <- prob_list[[start_word]]
  next_word <- ""
  max_words <- 25
  
  # If we generate a period (end of sentence), return the sentence. If not, continue building the sentence
  while(!(next_word == "." | (length(gen_review) >= max_words)) ) {
  # while(!(next_word == ".")) {
    next_word <- sample(unlist(unique), 1, prob=unname(unlist(transition_probs)))
    
    if(chain==2) {
      next_two <- str_c(gen_review[length(gen_review)], next_word, sep=" ")
      transition_probs <- prob_list[[next_two]]
    }
    else {
      transition_probs <- prob_list[[next_word]]
    }
    gen_review <- c(gen_review, next_word)
  }
  
  if(!(gen_review[length(gen_review)] == ".")) {
    if(gen_review[length(gen_review)] == "," | gen_review[length(gen_review)] == "-"){
      gen_review <- gen_review[-length(gen_review)]
    }
    gen_review <- c(gen_review, ".")
  }
  return(gen_review)
}


# Generate first order reviews --------------------------------------------


#Adds a variable `First` that gets the first word or every review
all_reviews <- all_reviews %>% mutate(First = word(all_reviews$Review, 1))
# Start words are all available beginning words besides punctuation 
start_words <- paste(all_reviews$First)
start_words <- unique(start_words)

# Generates 100 reviews with a random start word
generated_reviews <- replicate(100, generate_review(sample(start_words, 1), 2, probabilities, 1))
generated_reviews <- as.data.frame(generated_reviews)
names(generated_reviews)[1] <- "Reviews"

# Removes extra spaces from reviews generated 
generated_reviews <- generated_reviews %>% mutate(Reviews = str_replace_all(Reviews, " ,", ",")) %>% mutate(Reviews = str_replace_all(Reviews, " \\.", ".")) %>% mutate(Reviews = str_replace_all(Reviews, " - ", "-"))

# Set up words and unique words for second order chains -------------------


# Creates dataframe for words in second order markov chain
review_second <- unite(review_w, "part_1", sep = " ")
review_second <- cbind(review_second, word_3)
colnames(review_second) <- c("Part 1", "Word 3")
pairs_2 <- review_second

# Finds the unique words of 'Part 1'
unique2 <- review_second %>% distinct(`Part 1`)


# Find second order transition probabilities ------------------------------


# Creates large list of probabilities for each combination of second order markov chain
probabilities_2 <- apply(unique2, 1, build_list )
names(probabilities_2) <- unlist(unique2)

# Counts each occurence of word pairs
for(i in 1:nrow(pairs_2)) {
  probabilities_2[[pairs_2$`Part 1`[i]]][pairs_2$`Word 3`[i], pairs_2$`Part 1`[i]] <- probabilities_2[[pairs_2$`Part 1`[i]]][pairs_2$`Word 3`[i], pairs_2$`Part 1`[i]] + 1
}

# Find total times first word appears
totals <- lapply(probabilities_2, function(y) { sum(y, na.rm=TRUE)})
names(totals) <- unlist(unique2)

# Finds new probabilities (word pair count/first word count)
for(i in 1:length(probabilities_2)) {
  probabilities_2[[i]] <- probabilities_2[[i]] / totals[names(probabilities_2)[i]]
}


# Generates second order reviews ------------------------------------------


# Creates 100 generated reviews based on the second order markov chain
all_reviews <- all_reviews %>% mutate(First_Sec = word(all_reviews$Review, 1, 2))

first_two <- paste(all_reviews$First_Sec)
first_two <- unique(first_two)

generated_reviews2 <- replicate(100, generate_review(sample(first_two, 1), 2, probabilities_2, 2))
generated_reviews2 <- as.data.frame(generated_reviews2)
names(generated_reviews2)[1] <- "Reviews"


# Spruce up first order reviews -------------------------------------------


# Greates additional variables for the first order generated review that gives each review a date, price, score, when to drink and how many cases were made 
generated_reviews <- generated_reviews %>% mutate(Price = "", Score = "", Drink = "", Cases = "", Date = "")

for(i in 1:length(generated_reviews$Reviews)){
  generated_reviews$Price[i] <- sample(all_reviews$Price, 1)
  generated_reviews$Score[i] <- sample(all_reviews$Score, 1)
  generated_reviews$Drink[i] <- sample(all_reviews$Drink, 1)
  generated_reviews$Cases[i] <- sample(all_reviews$Cases, 1)
  generated_reviews$Date[i] <- as.character(sample(all_reviews$Date, 1))
}

# Removes extra spaces from cases generated 
generated_reviews <- generated_reviews %>% mutate(Cases = str_replace_all(Cases, " , ", ",")) %>% mutate(Cases = str_replace_all(Cases, " \\.", ".")) %>% mutate(Cases = str_replace_all(Cases, " - ", "-"))

# Adds dollar sign to price 
generated_reviews <- generated_reviews %>% mutate(Price = paste("$", generated_reviews$Price, sep = ""))

# Adds when to drink and cases made to reviews
generated_reviews <- generated_reviews %>% mutate(Reviews = paste(Reviews, Drink, ".", Cases, sep = " "))
generated_reviews <- generated_reviews %>% mutate(Reviews = str_replace_all(Reviews, " \\. ", ". "))

# Adds date, score and price to the reviews to mimic original reviews
generated_reviews <- generated_reviews %>% mutate(Reviews = paste(Date, ", Score: ", Score, ", ", Price, ", ", Reviews, sep = ""))
generated_reviews <- generated_reviews %>% select(Reviews)

# Names generated reviews
names(generated_reviews)[1] <- str_c(type, "Wine Reviews", sep = " ")

# Spruce up second order reviews ------------------------------------------


# Greates additional variables for the second order generated review that gives each review a date, price, score, when to drink and how many cases were made
generated_reviews2 <- generated_reviews2 %>% mutate(Price = "", Score = "", Drink = "", Cases = "", Date = "")

for(i in 1:length(generated_reviews2$Reviews)){
  generated_reviews2$Price[i] <- sample(all_reviews$Price, 1)
  generated_reviews2$Score[i] <- sample(all_reviews$Score, 1)
  generated_reviews2$Drink[i] <- sample(all_reviews$Drink, 1)
  generated_reviews2$Cases[i] <- sample(all_reviews$Cases, 1)
  generated_reviews2$Date[i] <- as.character(sample(all_reviews$Date, 1))
}

# Removes extra spaces from cases generated 
generated_reviews2 <- generated_reviews2 %>% mutate(Cases = str_replace_all(Cases, " , ", ",")) %>% mutate(Cases = str_replace_all(Cases, " \\.", ".")) %>% mutate(Cases = str_replace_all(Cases, " - ", "-"))

# Adds dollar sign to price 
generated_reviews2 <- generated_reviews2 %>% mutate(Price = paste("$", generated_reviews2$Price, sep = ""))

# Adds when to drink and cases made to reviews
generated_reviews2 <- generated_reviews2 %>% mutate(Reviews = paste(Reviews, Drink, ".", Cases, sep = " "))
generated_reviews2 <- generated_reviews2 %>% mutate(Reviews = str_replace_all(Reviews, " \\. ", ". "))

# Adds date, score and price to the reviews to mimic original reviews
generated_reviews2 <- generated_reviews2 %>% mutate(Reviews = paste(Date, ", Score: ", Score, ", ", Price, ", ", Reviews, sep = ""))
generated_reviews2 <- generated_reviews2 %>% select(Reviews)

# Names generated reviews
names(generated_reviews2)[1] <- str_c(type, "Wine Reviews", sep = " ")
