@extends('items.layout')

@section('title', 'Index')

@section('content')
<h1>Items In The Inventory</h1>

<ul>
    @foreach($items as $item)
        <li>
            <a href="inventory/{{ $item->id }}">
                {{ $item->name }}
            </a>
        </li>
    @endforeach
</ul>
@endsection
