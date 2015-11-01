#SparkR environment setup in RStudio

# Step1: Set the system environment variables

Sys.setenv(SPARK_HOME = "C:/spark-1.5.1-bin-hadoop2.6")

# Step2: Set the library path

.libPaths(c(file.path(Sys.getenv("SPARK_HOME"),"R","lib"),.libPaths()))

# Step3: Load SparkR library
library(SparkR)

# Step4: Create Spark context
sc = sparkR.init(master = "local")

# Step5: Create SQL Context
sqlContext = sparkRSQL.init(sc)


  #Code to check whether SC loads as expected

#create a sparkR DataFrame
DF <- createDataFrame(sqlContext, faithful)
head(DF)

# Create a simple local data.frame
localDF <- data.frame(name=c(1, 2, 3))


# Convert local data frame to a SparkR DataFrame
df <- createDataFrame(sqlContext, localDF)

# Print its schema
printSchema(df)
# root
#  |-- name: string (nullable = true)
#  |-- age: double (nullable = true)

# Create a DataFrame from a JSON file
path <- file.path(Sys.getenv("SPARK_HOME"), "examples/src/main/resources/people.json")
peopleDF <- jsonFile(sqlContext, path)
printSchema(peopleDF)

# Register this DataFrame as a table.
registerTempTable(peopleDF, "people")

# SQL statements can be run by using the sql methods provided by sqlContext
teenagers <- sql(sqlContext, "SELECT name FROM people WHERE age >= 13 AND age <= 19")

# Call collect to get a local data.frame
teenagersLocalDF <- collect(teenagers)

# Print the teenagers in our dataset 
print(teenagersLocalDF)

# Stop the SparkContext now
sparkR.stop()
