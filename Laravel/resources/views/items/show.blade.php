@extends('items.layout')

@section('Title', 'One Item')

@section('content')
    <h1 style="text-align: center">{{ $inventory->name }}</h1>

    <ul>
        <li>
            <h3>Description</h3>
            {{ $inventory->description }}
        </li>
        <li>
            <h3>Unit Cost Price</h3>
            {{ $inventory->unit_cost_price }}
        </li>
    </ul>
    <a href="/inventory/{{ $inventory->id }}/edit">Edit this item</a></li>
@endsection
