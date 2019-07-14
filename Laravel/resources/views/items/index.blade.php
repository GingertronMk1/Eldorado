@extends('items.layout')

@section('title', 'Index')

@section('content')
    <h1 style="text-align: center">Items In The Inventory</h1>

    <div style="padding-left: 1%">
        @if($inventory->isEmpty())
            The database is currently empty.
            Use the link above to add some data to it.
        @else
            <ul>
                @foreach($inventory as $item)
                    <li>
                        <a href="inventory/{{ $item->id }}">
                            {{ $item->name }}
                        </a>
                    </li>
                @endforeach
            </ul>
        @endif
    </div>
@endsection
