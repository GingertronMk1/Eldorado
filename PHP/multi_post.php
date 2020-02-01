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

    <p>Hello, world!</p>

    <?php
      $rotated = array();
      if($_POST){
        foreach($_POST as $k1 => $v1) {   // For each thing in the POST
          foreach($v1 as $k2 => $v2) {    // Enter its array
            foreach($v2 as $k3 => $v3) {  // And the one in that
              $rotated[$k3][$k2] = $v3;
            }
          }
        }
      }
      echo "<pre>";
      print_r($rotated);
      echo "</pre>";
    ?>


    <form method="post" action="">
    <?php for($n = 0; $n < 10; $n++): ?>
    <fieldset name="othertext<?php echo $n; ?>">
    <input type="text" name="test[othertext][]" value="<?php echo $n; ?>othertext">
    </fieldset>
    <?php endfor; ?>
    <?php for($i = 0; $i < 10; $i++): ?>
    <fieldset name="standard<?php echo $i; ?>">
    <input type="text" name="test[text][]" value="<?php echo $i; ?>text">
    <input type="number" name="test[number][]" value="<?php echo $i; ?>">
    </fieldset>
    <?php endfor; ?>
    <input type="submit">
    </form>

  </body>

</html>

