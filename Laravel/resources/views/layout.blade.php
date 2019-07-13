<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" type="text/css" href="//fonts.googleapis.com/css?family=Open+Sans" />

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
