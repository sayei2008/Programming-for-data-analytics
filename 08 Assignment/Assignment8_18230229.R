# Loading the required packages
library(readr)
library(stringr)
library(tidyverse)
library(ggplot2)
f_pre <- readLines("Chapter 02.txt",encoding = "UTF-8")
#loading the given text file and encoding it using UTF-8 inorder to read words from other languages like german which are present in the text
str(f_pre)
f_pre[1:10]#performing the steps as given in the assignment question

# 1. Write a function to convert the vector of lines to vector where each element is a word.
convert_to_words_vector <- function(x) {#creating a function as given in the assignment question
  split <- unlist(strsplit(x," ")) #splitting based on space
  split <- split[split!= ""]#leaving out empty ones
  return(split)#returning the processed words
}
f_pre_vec <- convert_to_words_vector(f_pre)#calling the function
str(f_pre_vec)
f_pre_vec[1:10]#desired outputs as given in the assignment question


# 2. Write a function to pre-process the data, where the function will do the following.
# Remove Invalid Characters
# convert each word to lower case
# Remove all stopwords
# Remove all empty strings 
invalid_characters <- c("--","\\?","\\!","\\.",",","\\.","'",":")#loading the invalid characters and stopwords as given
stopwords <- c("a", "about", "above", "above", "across", "after", "afterwards", "again", 
               "against", "all", "almost", "alone", "along", "already", "also","although",
               "always","am","among", "amongst", "amoungst", "amount",  "an", "and", "another", 
               "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as",  
               "at", "back","be","became", "because","become","becomes", "becoming", "been", 
               "before", "beforehand", "behind", "being", "below", "beside", "besides", 
               "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can", 
               "cannot", "cant", "co", "con", "could", "couldnt", "cry", "de", "describe", 
               "detail", "do", "done", "down", "due", "during", "each", "eg", "eight", "either", 
               "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", 
               "everyone", "everything", "everywhere", "except", "few", "fifteen", "fify", 
               "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty", 
               "found", "four", "from", "front", "full", "further", "get", "give", "go", "had", 
               "has", "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby", 
               "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however", 
               "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its", 
               "itself", "keep", "last", "latter", "latterly", "least", "less", "ltd", "made", 
               "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover", 
               "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", 
               "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", 
               "nor", "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", 
               "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", 
               "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re", 
               "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she", 
               "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow", 
               "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system",
               "take", "ten", "than", "that", "the", "their", "them", "themselves", "then", 
               "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", 
               "these", "they", "thickv", "thin", "third", "this", "those", "though", "three", 
               "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward",
               "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us", 
               "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence", 
               "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", 
               "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", 
               "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you", 
               "your", "yours", "yourself", "yourselves", "the")
preprocess<-function(x){
  f_post<-str_replace_all(f_pre_vec,str_c(invalid_characters,collapse = "|"),replacement = "")
  # joining all strings in invalid character using str_c and removing the mentioned punctuations from the f_pre_vec
  f_post <- str_to_lower(f_post)#converting all to lower case
  f_post<-str_replace_all(f_post,str_c("^",stopwords,"$",collapse = "|"),replacement = "")
  # joining all strings in stopwords using str_c and removing the mentioned stopwords from the f_post
  f_post<-f_post[f_post!=""]#removing the empty strings
  return(f_post)
}
f_post<-preprocess(f_pre_vec)
str(f_post)#to get the desired output as given in the assignment question

# 4. Create	a	tibble	with	3	columns.	The	first	is	for	each	word	is	the	processed	text,
#the	second	is	a	regex	search	pattern	(note	use	of	anchors)	that	can	be	used	to	search	the	text third is the word length
ans<-tibble(Words=unique(f_post),Pattern=str_c("^",regex(unique(f_post),"^ $",multiline = T),"$"),WLength=str_length(unique(f_post)))
#generating ans as a tibble with words containing the unique list of words from f_post
#Pattern containing the pattern search based on which the word could be found using the regex and anchors ^ and $ to ensure it is found as a full word always
#WLength is found as the string length of each unique word present in f_post
print(ans)#getting the desired output as given in the assignment question

# 5. Create	a	tibble	that	contains	the	frequency	of	word	length	occurrence	for	the	text
freq<-dplyr::summarise(group_by(ans,WLength),WFrequency=n())
#creating freq by summarising the grouping of ans by Wlength and getting the word frequency for each word length using n()
#mentioning dplyr inorder to avoid conflicts and masking issues while using n()
print(freq)#getting the desired output as given in the assignment question

#6. Plot result for the chapter text analysis
ggplot(data=freq, aes(x = WLength, y=WFrequency))+  geom_point(colour = "blue") + 
  # providing the data as frequency and giving x co ordinates as word length and y as word frequency with point colours as blue
  geom_line()+ # to get the required line plot
  ylab("Word Frequency") +  xlab("Word Length")#providing the x axis and y axis labels as given in the assignment question
