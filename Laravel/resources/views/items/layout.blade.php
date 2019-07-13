<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">

    <title>@yield('title', 'Inventory Management')</title>
</head>
<body>
    <div>
        <ul>
            <li><a href="/inventory">View all items</a></li>
            <li><a href="/inventory/create">Create new item</a></li>
        </ul>
    </div>
    <div class="container">
        @yield('content')
    </div>
</body>
</html>