CREATE DATABASE IF NOT EXISTS covid_project;
USE covid_project;
Select * from covid_deaths
ORDER BY 3,4;
Select * from covid_vaccinations
ORDER BY 3,4;

-- our table contains some unwanted rows i.e. continent names not country names
-- fix this issue by adding WHERE continent != "" 
-- these are called as empty strings.
-- for NULL values or empty cells we can use WHERE continenet is not null

-- selecting the data that we will be using in the project
SELECT location, date, total_cases, new_cases, total_deaths, population
FROM covid_project.covid_deaths
WHERE continent != ""
ORDER BY 1,2;

-- total cases vs total deaths (% of people who died of covid in a country(india))
SELECT location, date, total_cases, total_deaths, (total_deaths/total_cases)*100 as death_percentage
FROM covid_project.covid_deaths
WHERE location LIKE"%india%"
AND continent != ""
ORDER BY 1,2;

-- total cases vs population (how many people get infected by covid in a country(US))
SELECT location, date, total_cases, population, (total_cases/population)*100 as infection_percentage
FROM covid_project.covid_deaths
WHERE location LIKE"%states%"
AND continent != ""
ORDER BY 1,2;

-- which countries have the highest infection percentage?
SELECT location, population, MAX(total_cases) as total_infect_count, MAX((total_cases/population))*100 as infection_percentage
FROM covid_project.covid_deaths
WHERE continent != ""
GROUP BY location, population
ORDER BY 4 DESC;

-- countries with the highest death count 
SELECT location, population, MAX(total_deaths) as death_count
FROM covid_project.covid_deaths
WHERE continent != ""
GROUP BY location, population
ORDER BY 3 DESC;
-- didn't give expected results as the total deaths data type is text not numerical
-- so we need to convert the data type of total deaths
SELECT location, population, MAX(cast(total_deaths as float)) as death_count
FROM covid_project.covid_deaths
WHERE continent != ""
GROUP BY location, population
ORDER BY death_count DESC;
-- couldn't convert to int due to presence of empty strings. if it was null values we can convert to int

-- EDA by continent
SELECT continent, MAX(cast(total_deaths as float)) as death_count
FROM covid_project.covid_deaths
WHERE continent != ""
GROUP BY continent
ORDER BY death_count DESC;

-- global numbers
SELECT SUM(new_cases) as total_cases,
SUM(cast(new_deaths as float)) as total_deaths, 
SUM(cast(new_deaths as float))/SUM(new_cases)*100 as death_percentage
FROM covid_project.covid_deaths
WHERE continent != ""
ORDER BY 1,2;

-- joining both tables
SELECT *
FROM covid_project.covid_deaths dea
JOIN covid_project.covid_vaccinations vac
	ON dea.location = vac.location
    AND dea.date = vac.date;

-- total amount of people in the world that have been vaccinated
-- using total population and new vaccinations
-- we dont use here total vaccinations rather we do rolling count on new vaccinations to find total vaccinations
-- use CTE
WITH PopvsVac (continent, location, date, population, new_vaccinations, rolling_people_vac)
as 
(SELECT dea.continent, dea.location, dea.date, dea.population,
vac.new_vaccinations,
SUM(convert(vac.new_vaccinations, float)) OVER (partition by dea.location order by dea.location, dea.date) as rolling_people_vac
FROM covid_project.covid_deaths dea
JOIN covid_project.covid_vaccinations vac
	ON dea.location = vac.location
    AND dea.date = vac.date
WHERE dea.continent != ""
)
SELECT *, (rolling_people_vac/population)*100
FROM PopvsVac;

-- same query but with temp table
DROP table if exists Percent_pop_vaccinated
CREATE table Percent_pop_vaccinated
(continent varchar(255),
location varchar(255),
date datetime, 
population numeric,
new_vaccinations varchar(255),
rolling_people_vac numeric
);

insert into Percent_pop_vaccinated
SELECT dea.continent, dea.location, dea.date, dea.population,
vac.new_vaccinations,
SUM(convert(vac.new_vaccinations, float)) OVER (partition by dea.location order by dea.location, dea.date) as rolling_people_vac
FROM covid_project.covid_deaths dea
JOIN covid_project.covid_vaccinations vac
	ON dea.location = vac.location
    AND dea.date = vac.date
WHERE dea.continent != ""
;

SELECT *, (rolling_people_vac/population)*100
FROM Percent_pop_vaccinated;

-- Creating view to store data for later visualizations
Create View PercentPopVaccinated as
SELECT dea.continent, dea.location, dea.date, dea.population,
vac.new_vaccinations,
SUM(convert(vac.new_vaccinations, float)) OVER (partition by dea.location order by dea.location, dea.date) as rolling_people_vac
FROM covid_project.covid_deaths dea
JOIN covid_project.covid_vaccinations vac
	ON dea.location = vac.location
    AND dea.date = vac.date
WHERE dea.continent != ""
;

