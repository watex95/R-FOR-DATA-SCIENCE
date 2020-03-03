

--student information
create table STUDENT_Dim(
StudentID varchar2(50),
StudentName varchar2(50),
DateOfBirth date
);
alter table student_dim add constraint pk_student primary key(StudentID);


---course information
CREATE TABLE COURSE_Dim(
CourseCode varchar(50),
Version varchar(50), 
CourseName varchar(200),
SchoolName  varchar(200)
);
alter table course_dim add constraint pk_course primary key(CourseCode);


--unit information
CREATE TABLE UNIT_Dim(
UnitCode varchar(50),
UnitName varchar(250)
);
alter table unit_dim add constraint pk_unit primary key(unitcode);


CREATE TABLE UNIT_OFFERING_Dim (
OfferingNumber numeric,
Year date,
TeachingPeriod numeric
);
alter table UNIT_OFFERING_Dim add constraint pk_offering primary key(OfferingNumber)


CREATE TABLE OFFERING_COORDINATOR_Dim(
StaffID varchar(50),
UnitOfferingNumber numeric
);
alter table OFFERING_COORDINATOR_Dim add constraint pk_coordinator primary key(StaffID)



--HR information
CREATE TABLE STAFF_MEMBER_Dim(
StaffNumber numeric,
StaffName varchar(200)
);
alter table STAFF_MEMBER_Dim add constraint pk_staff primary key(StaffNumber)



CREATE TABLE SCHOOL_Dim(
SchoolCode varchar(50), 
SchoolTitle varchar(250)
);
alter table SCHOOL_Dim add constraint pk_school primary key(SchoolCode)


--previous institution information
CREATE TABLE PREVIOUS_Dim (
StudentNumber numeric,
Student_Name varchar(200),
Institution_Code varchar(50),
Institution_Name varchar(50),
Country varchar(50)
);
alter table PREVIOUS_Dim add constraint pk_previous primary key(StudentNumber)


CREATE TABLE ENROLLMENT_Dim(
EnrolNumber NUMERIC ,
UnitOfferingNumber numeric,
Grade varchar(50)
);
alter table enrollment_dim add constraint pk_enrollment primary key(EnrolNumber);


select * from perfomance_fact

---FACT TATBLE INFORMATION
CREATE TABLE Perfomance_Fact(
PerfomanceID numeric,
StudentID varchar2(50),
CourseCode varchar(50),
UnitCode varchar(50),
EnrolNumber NUMERIC ,
OfferingNumber numeric,
StaffNumber numeric,
SchoolCode varchar(50),
StaffID varchar(50),
StudentNumber numeric,

Count_students numeric,
Count_grade numeric,

constraint pk_performance primary key(PerfomanceID),

constraint fk_student foreign key(StudentID) references STUDENT_Dim(StudentID),
constraint fk_course foreign key(CourseCode) references COURSE_Dim(CourseCode),
constraint fk_unit foreign key(UnitCode) references UNIT_Dim(UnitCode),
constraint fk_enroll foreign key(EnrolNumber) references ENROLLMENT_Dim(EnrolNumber),
constraint fk_unit_offering foreign key(OfferingNumber) references UNIT_OFFERING_Dim(OfferingNumber),
constraint fk_coodinator foreign key(StaffID) references OFFERING_COORDINATOR_Dim(StaffID),
constraint fk_staff foreign key(StaffNumber) references STAFF_MEMBER_Dim(StaffNumber),
constraint fk_school foreign key(SchoolCode) references SCHOOL_Dim(SchoolCode),
constraint fk_previous foreign key (StudentNumber) references PREVIOUS_Dim(StudentNumber)
);



