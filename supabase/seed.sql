insert into
public.timelines (name,period)
values
('Jan''s Birthday','1980-06-6'),
('High School','1995 - 1999'),
('Working in Permutive','2019 - '),
('Queen Elizabeth II reign','1952-02-06 - 2022-09-08'),
('Moved to the UK','2005 - '),
('French Revolution','1789 - 1795'),
('Lived in Clapham','2014 - 2018'),
('Finished in 1985',' - 1985'),
('Left Permutive','2023-04-6'),
('Bad Year','2022'),
('Pride Month','2023-06'),
('Happy Year','2023'),
('Urodziny Kaśki','1982-09-15'),
('Met Alex','2023-07-08'),
('programming','1991-05-06'),
('programming3','1991-05-06 - '),
('Using Android',' - 2022'),
('test4','2023-04 - 2023-09'),
('Future','2022-03-06'),
('Future2','2024-07-03'),
('Edward VII''s reign','1901-01-22 - 1910-05-06'),
('Victoria''s reign','1837-06-20 - 1901-01-22'),
('George V''s reign','1910-05-06 - 1936-01-20'),
('Edward VII''s reign','1936-01-20 - 1936-12-11'),
('George VI''s reign','1936-12-11 - 1952-02-06'),
('Charles III''s reign','2022-09-08 - ');

insert into
public.eras (name,erastart,eraend)
values
('Wszystko','1789','2024-07-03'),
('Test','2020-03-13','2023');

insert into
public.tags (name)
values
('To study'),
('Tag2');

insert into
public.timeline_tag (timeline_id, tag_id)
values
(1,1),
(2,2),
(3,2),
(3,1);