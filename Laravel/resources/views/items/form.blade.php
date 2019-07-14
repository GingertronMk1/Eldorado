@extends('items.layout')

@section('content')
    @yield('before_form')

    <form method="POST" action=@yield('form_action')>
        @yield('form_method')

        @csrf

        <label for="name">Item Name</label>
        <br>
        <input type="text"
               class="form-control form-control-lg {{ $errors->has('name') ? 'is-invalid' : '' }}"
               name="name"
               value="{{ ( ! empty($inventory) ? $inventory->name : old('name') ) }}">

        <br>

        <label for="description">Description</label>

        <br>

        <textarea class="form-control {{ $errors->has('description') ? 'is-invalid' : '' }}" name="description">{{ ( ! empty($inventory) ? $inventory->description : old('description') ) }}</textarea>

        <br>

        <label for="unit_cost_price">Unit Cost Price</label>

        <br>

        <input type="number"
               class="form-control {{ $errors->has('unit_cost_price') ? 'is-invalid' : '' }}"
               name="unit_cost_price"
               step="0.01"
               value="{{ ( ! empty($inventory) ? $inventory->unit_cost_price : old('unit_cost_price') ) }}">

        <br>

        <button type="submit" class="btn btn-primary">
            @yield('button_text')
        </button>
    </form>

        @if($errors->any())
            <div class="bg-danger rounded">
                <ul>
                    @foreach($errors->all() as $error)
                        <li style="color:white">{{ $error }}</li>
                    @endforeach
                </ul>
            </div>
        @endif

    @yield('after_form')

@endsection