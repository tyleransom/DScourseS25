--Create the table since direct import wasn't working
CREATE TABLE Insurance ("var1" INTEGER, "Var2" CHAR, "Var3" CHAR, "Var4" CHAR, "Var5" DOUBLE, "Var6" DOUBLE, "Var7" DOUBLE, "Var8" DOUBLE, "Var9" DOUBLE, "Var10" DOUBLE, "Var11" DOUBLE, "Var12" DOUBLE, "Var13" DOUBLE, "Var14" DOUBLE, "Var15" DOUBLE, "Var16" DOUBLE, "Var17" DOUBLE, "Var18" DOUBLE);

-- Make sure everything is ready to read in a CSV
.mode csv

-- Import the CSV to Table Insurance
.import /home/ouecon002/DScourseS25/ProblemSets/PS3/FL_insurance_sample.csv Insurance

-- task for part C Print the first 10 rows of the table
SELECT * FROM Insurance LIMIT 10


-- task for part D List which counties are unique in the file
SELECT DISTINCT Var3 FROM Insurance;

-- task for part E compute the average property appreciation.
SELECT AVG(Var9-Var8) FROM Insurance


-- Count what buildings are made of which material
SELECT Var17, COUNT(*) FROM Insurance GROUP BY Var17;

-- Count the total number of buildings
SELECT COUNT(*) FROM Insurance