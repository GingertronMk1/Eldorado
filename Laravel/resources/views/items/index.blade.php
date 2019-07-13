@extends('items.layout')

@section('title', 'Index')

@section('content')
<h1>Items In The Inventory</h1>

@if($items->isEmpty())
  The database is currently empty.
  Use the link above to add some data to it.
@else
<ul>
    @foreach($items as $item)
        <li>
            <a href="inventory/{{ $item->id }}">
                {{ $item->name }}
            </a>
        </li>
    @endforeach
</ul>
@endif
@endsection
