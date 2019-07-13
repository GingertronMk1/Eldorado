@extends('items.layout')

@section('title', 'Add Item')

@section('content')
    <h1>Add an Item</h1>
    <div class="form-group">

        <form method="POST" action="/inventory">
            @csrf

            <label for="name">Item Name</label>
            <br>
            <input type="text"
                   class="form-control form-control-lg {{ $errors->has('name') ? 'is-invalid' : '' }}"
                   name="name"
                   value="{{ old('name') }}"
                   >

            <br>

            <label for="description">Description</label>

            <br>

            <textarea class="form-control textarea {{ $errors->has('description') ? 'is-invalid' : '' }}" name="description">{{ old('description') }}</textarea>

            <br>

            <label for="unit_cost_price">Unit Cost Price</label>

            <br>

            <input type="number"
                   class="form-control {{ $errors->has('unit_cost_price') ? 'is-invalid' : '' }}"
                   name="unit_cost_price"
                   step="0.01"
                   value="{{ old('unit_cost_price') }}">

            <br>

            <button type="submit" class="btn btn-primary">Create Item</button>

            @if($errors->any())
                <div class="bg-danger">

                    <ul>
                        @foreach($errors->all() as $error)
                            <li>{{ $error }}</li>
                        @endforeach
                    </ul>
                </div>
            @endif
        </form>
    </div>

@endsection
