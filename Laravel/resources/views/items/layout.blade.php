<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">

    <link rel="stylesheet" type="text/css" href="//fonts.googleapis.com/css?family=Open+Sans" />
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">

    <style>
        * {
            font-family: "Open Sans", sans-serif;
        }

    </style>

    <title>@yield('title', 'Inventory Management')</title>
</head>
<body>
<div class="align-content-center" style="height: 5%">
    <div style="text-align: center">
        <a href="/inventory">View all items</a>
        |
        <a href="/inventory/create">Create new item</a>
        |
        <a href="/">Go home</a>
    </div>
    <h1 style="text-align: center">
        @yield('header')
    </h1>
    <div class="container">
        @yield('content')
    </div>
</div>
</body>
</html>
