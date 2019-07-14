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
            £ {{ number_format($inventory->unit_cost_price, 2, '.', ',') }}
        </li>
    </ul>

    <a href="/inventory/{{ $inventory->id }}/edit" class="btn btn-primary">Edit item</a>

    <br>

    <form method="POST" action="/inventory/{{ $inventory->id }}">
        @method('DELETE')
        @csrf
        <button type="submit" class="btn btn-danger">Delete item</button>
    </form>
    <br>

@endsection
