<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">

    <title>@yield('title', 'Datatank Tech Exercise')</title>
</head>
<body>
<div>
    <ul>
        <li>Home <a href="/">here</a></li>
        <li>Contact details <a href="/contact">here</a></li>
        <li>About page <a href="/about">here</a></li>
    </ul>
    @yield('content')
</div>
</body>
</html>
