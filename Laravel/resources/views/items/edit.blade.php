@extends('items.form')

@section('form_action')
    /inventory/{{ $inventory->id }}
@endsection

@section('form_method')
    @method('PATCH')
@endsection

@section('button_text', 'Update Item')