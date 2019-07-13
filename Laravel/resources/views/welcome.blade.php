@extends('layout')

@section('content')
<h1>Let's have a go then</h1>

<ul>
    @foreach($tasks as $task)
    <li>{{ $task }}</li>
    @endforeach
</ul>
@endsection
