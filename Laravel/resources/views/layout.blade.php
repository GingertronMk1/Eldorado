<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" type="text/css" href="//fonts.googleapis.com/css?family=Open+Sans"/>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
          integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">

    <style>
        * {
            font-family: "Open Sans", sans-serif;
        }

    </style>

    <title>@yield('title', 'Datatank Tech Exercise')</title>
</head>
<body>
<div>
    <ul>
        <li>Home <a href="/">here</a></li>
        <li>Contact details <a href="/contact">here</a></li>
        <li>Inventory page <a href="/inventory">here</a></li>
    </ul>
    @yield('content')
</div>
</body>
</html>
