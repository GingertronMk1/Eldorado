@extends('items.layout')

@section('title', 'Add Item')

@section('content')
<h1>Add an Item</h1>

<form method="POST" action="/inventory">

    {{ @csrf_field() }}
    <div>
        <input type="text" name="name" placeholder="Item Name" required>
    </div>
    <div>
        <textarea name="description" placeholder="Item Description"></textarea>
    </div>
    <div>
        <input type="number" name="unit_cost_price" step="0.01" placeholder="Item Unit Cost">
    </div>
    <div>
        <button type="submit">Add Item</button>
    </div>

    @if($errors->any())
    <ul>
        @foreach($errors->all() as $error)
            <li>{{ $error }}</li>
        @endforeach
    </ul>
        @endif
</form>
@endsection
