@extends('items.layout')

@section('Title', 'One Item')

@section('content')
<ul>
    <li>{{ $inventory->name }}</li>
    <li>{{ $inventory->description }}</li>
    <li>{{ $inventory->unit_cost_price }}</li>
    <li><a href="/inventory/{{ $inventory->id }}/edit">Edit this item</a></li>
    </ul>
@endsection