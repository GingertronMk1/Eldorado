<!DOCTYPE html>
<html>

  <head>
    <meta charset="utf-8">
    <title></title>
    <meta name="author" content="">
    <meta name="description" content="">
    <meta name="viewport" content="width=device-width, initial-scale=1">
  </head>

  <body>

    <?php
$testStrings = ["£69.420", "£69s420", "£69.4200000000"];

$find = "/(\d\.\d{2})\d*/";
$repl = "\\1";

foreach ($testStrings as $ts) {
  echo $ts . "<br>";

  $tsReplaced = preg_replace($find, $repl, $ts);
  echo $tsReplaced . "<br>---<br>";

}
    ?>

  </body>

</html>
