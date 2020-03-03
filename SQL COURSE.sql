create user TestDB identified by hillary123;
grant dba to TestDB



/* Create the schema for our tables */
create table Movie(
mID int,
title varchar(50),
year int,
director varchar(50)
);

create table Reviewer(
rID int,
name varchar(50)
); 


create table Rating(
rID int,
mID int,
stars int,
ratingDate varchar(50)
);


insert into Rating values(201, 101, 2, '2011-01-22')
insert into Rating values(201, 101, 4, '2011-01-27');
insert into Rating values(202, 106, 4, null);
insert into Rating values(203, 103, 2, '2011-01-20');
insert into Rating values(203, 108, 4, '2011-01-12');
insert into Rating values(203, 108, 2, '2011-01-30');
insert into Rating values(204, 101, 3, '2011-01-09');
insert into Rating values(205, 103, 3, '2011-01-27');
insert into Rating values(205, 104, 2, '2011-01-22');
insert into Rating values(205, 108, 4, null);
insert into Rating values(206, 107, 3, '2011-01-15');
insert into Rating values(206, 106, 5, '2011-01-19');
insert into Rating values(207, 107, 5, '2011-01-20');
insert into Rating values(208, 104, 3, '2011-01-02');

select * from rating
/* Populate the tables with our data */
insert into Movie values(101, 'Gone with the Wind', 1939, 'Victor Fleming');
insert into Movie values(102, 'Star Wars', 1977, 'George Lucas');
insert into Movie values(103, 'The Sound of Music', 1965, 'Robert Wise');
insert into Movie values(104, 'E.T.', 1982, 'Steven Spielberg');
insert into Movie values(105, 'Titanic', 1997, 'James Cameron');
insert into Movie values(106, 'Snow White', 1937, null);
insert into Movie values(107, 'Avatar', 2009, 'James Cameron');
insert into Movie values(108, 'Raiders of the Lost Ark', 1981, 'Steven Spielberg');

select * from movie

insert into Reviewer values(201, 'Sarah Martinez');
insert into Reviewer values(202, 'Daniel Lewis');
insert into Reviewer values(203, 'Brittany Harris');
insert into Reviewer values(204, 'Mike Anderson');
insert into Reviewer values(205, 'Chris Jackson');
insert into Reviewer values(206, 'Elizabeth Thomas');
insert into Reviewer values(207, 'James Cameron');
insert into Reviewer values(208, 'Ashley White');

select * from reviewer


---------------------------------------------------------------------------------------------------------------------------

EXERCISES
-------------------
--Q2---
select distinct MOVIE.year
from movie
inner join RATING
on movie.mID=RATING.mID
where stars=4
OR
stars=5
order by year ASC;


--Q3---
select movie.title 
from movie
left join rating
on movie.mID=rating.mID
where RATING.stars is null


---Q4--
select reviewer.name 
from reviewer
left join rating
on reviewer.rID=rating.rID
where ratingDate is null


--Q5-----
select Reviewer.name, Movie.title, Rating.stars, Rating.ratingDate
FROM Rating join Reviewer on Rating.rID = Reviewer.rID
 join Movie on Movie.mID=Rating.mID
order by Reviewer.name, Movie.title, stars

--Q6---
select Reviewer.name, Movie.title
from Reviewer, Movie, (select R1.rID, R1.mID from Rating R1, Rating R2 where R1.rID=R2.rID and R1.mID=R2.mID and R2.ratingDate>R1.ratingDate and R2.stars>R1.stars) as T
where Reviewer.rID=T.rID and Movie.mID=T.mID;

---For all cases where the same reviewer rated the same movie twice and gave it a higher rating the
--second time, return the reviewer's name and the title of the movie.
select Reviewer.name, Movie.title
from Reviewer, Movie, (select R1.rID, R1.mID from Rating R1, Rating R2 
where R1.rID=R2.rID and R1.mID=R2.mID and R2.ratingDate>R1.ratingDate and R2.stars>R1.stars) as T
where Reviewer.rID=T.rID and Movie.mID=T.mID;



--Q7---

select Movie.title, max(Rating.stars) as max_rating from Rating
inner join Movie on Movie.mID=Rating.mID group by Movie.title order by movie.title;

--Q8--
select * from (select title, max(stars)-min(stars) value
        from rating natural join movie group by title) 
order by value desc


--Q9--
select abs(avg(a)-(select avg(b) 
                    from (select avg(stars) b 
                    from rating natural join movie 
                    where year <1980 
                    group by title)))
from (select avg(stars) a
        from rating natural join movie 
        where year >1980 
        group by title) 

-----------
--PART 2
----------

--Q1--
--Find the names of all reviewers who rated Gone with the Wind.
select distinct name 
from reviewer inner join rating
on reviewer.rid=rating.rid
where mid=(select mid from movie
where title='Gone with the Wind')

--Q2
--For any rating where the reviewer is the same as the director of the movie, return the reviewer name, 
--movie title, and number of stars.

select * from movie

select * from rating

select director from movie
inner join rating
on movie.mid=rating.mid
where 



