@extends('items.layout')

@section('Title', 'Editor')

@section('content')
    <h1>Edit {{ $inventory->name }}</h1>
    <div class="form-group">
        <form method="POST" action="/inventory/{{ $inventory->id }}">
            @method('PATCH')
            @csrf

            <label for="name">Item Name</label>
            <br>
            <input type="text" class="form-control form-control-lg" name="name" placeholder="Item Name" value="{{ $inventory->name }}" required>
            <br>
            <label for="description">Description</label>
            <br>
            <textarea class="form-control" name="description">{{ $inventory->description }}</textarea>
            <br>
            <label for="unit_cost_price">Unit Cost Price</label>
            <br>
            <input type="number" class="form-control" name="unit_cost_price" step="0.01" value="{{ $inventory->unit_cost_price }}">
            <br>
            <button type="submit" class="btn btn-primary">Update Item</button>
        </form>
        <br>

        <form method="POST" action="/inventory/{{ $inventory->id }}">
            @method('DELETE')
            @csrf
            <button type="submit" class="btn btn-danger">Delete item</button>
        </form>
    </div>

    @if($errors->any())
        <div class="bg-danger rounded">
            <ul>
                @foreach($errors->all() as $error)
                    <li style="color:white">{{ $error }}</li>
                @endforeach
            </ul>
        </div>
    @endif

@endsection