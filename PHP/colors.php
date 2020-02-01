<!DOCTYPE html>
<html>

  <head>
    <meta charset="utf-8">
    <title></title>
    <meta name="author" content="">
    <meta name="description" content="">
    <meta name="viewport" content="width=device-width, initial-scale=1">

    <link href="css/normalize.css" rel="stylesheet">
    <link href="css/style.css" rel="stylesheet">
  </head>

  <body>

    <?php 
    
    $i = 0;
    $hexes = ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'];

    foreach ($hexes as $c1):
    foreach ($hexes as $c2):
    foreach ($hexes as $c3):
    foreach ($hexes as $c4):
    foreach ($hexes as $c5):
    foreach ($hexes as $c6):
    ?>

    <div style="width: 100%; min-height: 1px; background-color:#<?php echo $c1 . $c2 . $c3 . $c4 . $c5 . $c6; ?>;">
    </div>

    <div style="display: none;">
    <?php echo $i++; ?>
    </div>

    <?php
    endforeach;
    endforeach;
    endforeach;
    endforeach;
    endforeach;
    endforeach;
    ?>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js"></script>
    <script src="js/script.js"></script>
  </body>

</html>

