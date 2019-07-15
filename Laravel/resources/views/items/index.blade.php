@extends('items.layout')

@section('title', 'Index')

@section('header', 'Items In The Inventory')

@section('content')
    @if($inventory->isEmpty())
        <div style="text-align: center">
            The database is currently empty.
            <a href="/inventory/create">Click here</a> to add some data to it.
        </div>
    @else
        <div style="padding-left: 1%">

            <ul>
                @foreach($inventory as $item)
                    <li>
                        <a href="inventory/{{ $item->id }}">
                            {{ $item->name }}
                        </a>
                    </li>
                @endforeach
            </ul>
        </div>
    @endif
@endsection
