@extends('items.layout')

@section('Title', 'One Item')

@section('content')
<ul>
    <li>
      <h2>Name:</h2>
      {{ $inventory->name }}
    </li>
    <li>
      <h2>Description:</h2>
      {{ $inventory->description }}
    </li>
    <li>
      <h2>Unit Cost Price:</h2>
      {{ $inventory->unit_cost_price }}
    </li>
    </ul>
    <a href="/inventory/{{ $inventory->id }}/edit">Edit this item</a></li>
@endsection
