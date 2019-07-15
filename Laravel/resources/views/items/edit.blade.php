@extends('items.form')

@section('header', 'Edit Item')

@section('form_action')
    /inventory/{{ $inventory->id }}
@endsection

@section('form_method')
    @method('PATCH')
@endsection

@section('button_text', 'Save Item')