CREATE TABLE books (
    title TEXT,
    price NUMERIC(10, 2),
    availability VARCHAR(50)
);

--Veryfing the imported records
SELECT * FROM books;
SELECT COUNT (*) AS total_books
FROM books;

--Investigation 1: Overall book price analysis
SELECT
	COUNT(*) AS total_books,
	ROUND(AVG(price), 2) AS average_price,
	MIN(price) AS lowest_price,
	MAX(price) AS highest_price
FROM books;
--Purpose: To investigate the overall pricing characteristics of the bookstores 20 products

--Investigation 2: Book distribution by price range
SELECT
	CASE
		WHEN price < 25 THEN 'Budget'
		WHEN price < 45 THEN 'Mid-Range'
		ELSE 'Premium'
	END AS price_category,
	COUNT (*) AS number_of_books
FROM books
GROUP BY price_category
ORDER BY MIN(price);
--Purpose: To investigate how the bookstores products are distributed across different price ranges