<?php

// $pg = pg_connect ('postgresql://localhost:5433/mcarter?sslmode=disable');

// $res = pg_query('SELECT * FROM foo');

// var_dump (pg_fetch_row($res));

$pdo = new PDO('pgsql:dbname=mcarter;port=5433;host=localhost;sslmode=disable', 'mcarter');

$stmt = $pdo->prepare('SELECT * FROM basket');

echo 'execute';
$res = $stmt->execute();
echo 'fetch';
var_dump($res);
$row = $stmt->fetch();
// $rows = $stmt->fetchAll();
var_dump ($row);
// var_dump ($rows); die;