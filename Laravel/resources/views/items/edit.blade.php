@extends('items.layout')

@section('Title', 'Editor')

@section('content')
    <h1>Edit Content</h1>
    <form method="POST" action="/inventory/{{ $inventory->id }}">
        @method('PATCH')
        @csrf


        <label for="Name">Item Name</label>
        <br>
        <input type="text" class="input" name="name" placeholder="Item Name" value="{{ $inventory->name }}" required>
        <br>
        <label for="description">Description</label>
        <br>
        <textarea name="description">{{ $inventory->description }}</textarea>
        <br>
        <label for="unit_cost_price">Unit Cost Price</label>
        <br>
        <input type="number" name="unit_cost_price" step="0.01" value="{{ $inventory->unit_cost_price }}">
        <br>
        <button type="submit">Update Item</button>
    </form>

    <form method="POST" action="/inventory/{{ $inventory->id }}">
        @method('DELETE')
        @csrf
        <button type="submit">Delete item</button>
    </form>

    @if($errors->any())
        <ul>
            @foreach($errors->all() as $error)
                <li>{{ $error }}</li>
            @endforeach
        </ul>
    @endif

    @endsection