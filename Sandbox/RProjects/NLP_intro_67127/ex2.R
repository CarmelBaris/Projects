part_of_speech_tag2(L, Ct2):
	tags <- []
	i <- 2
	while i ≤ len(L):
		tags[i] <- argmax(Ct2{ L[1], ・} )