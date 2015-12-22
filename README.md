# thoughtWork-challenges
###########################################################################################################################
##																						  ##
##								Merchant's Guide to Galaxy								  ##
##																						  ##
###########################################################################################################################

java-version = 1.6
maven-version = 3.3.3
junit=version = 4.11

<dependency-version>
	gson-2.3.1			## for reading json file 
	logback-1.0.13		## for logging utility
	
main-class = com.thoughtworks.guide.to.galaxy.main.MerchantOutputerMain
argument-to-main-class = absolute path to the input file (optional)
default = input.txt

### Junit Test File
Two Test file = validInput-test.txt
				invalidInput-test.txt