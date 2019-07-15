
###Spam email example

library(kernlab)
data(spam)

#How often does the word your appear in spam vs non-spam emails
plot(density(spam$your[spam$type == "nonspam"]), col = "blue", main ="", xlab = "Freq of 'your'")
lines(density(spam$your[spam$type == "spam"]), col = "red")

#Simple prediction based on a cutoff
prediction <- ifelse(spam$your > 0.5, "spam", "nonspam")
#So, what is my prediction accuracy?
table(prediction, spam$type)/length(spam$type)
#prediction   nonspam      spam
#     nonspam 0.4590306 0.1017170
#     spam    0.1469246 0.2923278

0.459+0.292
# 0.751 

tapply(spam$capitalAve, spam$type, summary)

plot(density(spam$money[spam$type == "nonspam"]), col = "blue", main ="", xlab = "Freq of 'money'")
lines(density(spam$money[spam$type == "spam"]), col = "red")

plot(density(spam$credit[spam$type == "nonspam"]), col = "blue", main ="", xlab = "Freq of 'money'")
lines(density(spam$credit[spam$type == "spam"]), col = "red")
