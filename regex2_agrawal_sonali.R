#Katherine Who?

#One regular expression for each of the lists below to capture every instance of the name
#1. Cathrin, Cathryn, Cathrinn, Cathrynn, Cathrine, Cathryne, Cathrinne, Cathrynne
reg_string1 = c("Cathrin", "Cathryn", "Cathrinn", "Cathrynn", "Cathrine", "Cathryne", "Cathrinne", "Cathrynne")
match1 = str_subset(reg_string1, "^(Cathr)[i|y](n){1,2}(e)?$")
match1

#2. Catherin, Catheryn, Catherinn, Catherynn, Catherine, Catheryne, Catherinne, Catherynne
reg_string2 = c("Catherin", "Catheryn", "Catherinn", "Catherynn", "Catherine", "Catheryne", "Catherinne", "Catherynne")
match2 = str_subset(reg_string2, "^(Cather)[i|y](n){1,2}(e)?$")
match2

#3. Kathrin, Kathryn, Kathrinn, Kathrynn, Kathrine, Kathryne, Kathrinne, Kathrynne
reg_string3 = c("Kathrin", "Kathryn", "Kathrinn", "Kathrynn", "Kathrine", "Kathryne", "Kathrinne", "Kathrynne")
match3 = str_subset(reg_string3, "^(Kathr)[i|y](n){1,2}(e)?$")
match3

#4. Katherin, Katheryn, Katherinn, Katherynn, Kattherine, Katheryne, Katherinne, Katherynne
reg_string4 = c("Katherin", "Katheryn", "Katherinn", "Katherynn", "Kattherine", "Katheryne", "Katherinne", "Katherynne")
match4 = str_subset(reg_string4, "^(Ka)(t){1,2}(her)[i|y](n){1,2}(e)?$")
match4

#Write a single regular expression that will capture every version of the name in above four lists
reg_string5 = c("Cathrin", "Cathryn", "Cathrinn", "Cathrynn", "Cathrine", "Cathryne", "Cathrinne", "Cathrynne",
                "Catherin", "Catheryn", "Catherinn", "Catherynn", "Catherine", "Catheryne", "Catherinne", "Catherynne",
                "Kathrin", "Kathryn", "Kathrinn", "Kathrynn", "Kathrine", "Kathryne", "Kathrinne", "Kathrynne",
                "Katherin", "Katheryn", "Katherinn", "Katherynn", "Kattherine", "Katheryne", "Katherinne", "Katherynne")
match5 = str_subset(reg_string5, "^(K|C)(a)(t){1,2}(h)(e)?(r)[i|y](n){1,2}(e)?$")
match5

#Adverbs and Adjectives

#Regular expression to capture adverbs ending in ly which proceeds a word it modifies in "sentences"
list1 = str_subset(sentences, "([^ ]+ly)")
list2 = str_extract(list1, "([^ ]+ly)")
list2

#For the adverbs identified, a regular expression to capture words modified by those adverbs
list3 = str_extract(list1, "([^ ]+)(?= [^ ]+ly)")
list4 = !grepl("(^the$|^a$|^was$|^The$|^and$|^by$)",list3,perl=TRUE)
list5 = list3[list4]
list5[!is.na(list5)]
