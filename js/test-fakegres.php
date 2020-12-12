<?php

// $pg = pg_connect ('postgresql://localhost:5433/mcarter?sslmode=disable');

// $res = pg_query('SELECT * FROM foo');

// var_dump (pg_fetch_row($res));

$pdo = new PDO('pgsql:dbname=mcarter;port=5432;host=localhost;sslmode=disable', 'mcarter');

// $stmt = $pdo->prepare('
// -- this is a comment
// SELECT * FROM basket');

$stmt = $pdo->prepare('select 1.5 as foo');

$res = $stmt->execute();
// var_dump($res);
// $row = $stmt->fetch();
$rows = $stmt->fetchAll();
// var_dump ($row);
var_dump ($rows);
