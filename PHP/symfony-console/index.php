#!/usr/bin/env php
<?php

use Symfony\Component\Console\Helper\Table;
use Symfony\Component\Console\Helper\TableSeparator;
use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Output\ConsoleOutputInterface;
use Symfony\Component\Console\SingleCommandApplication;

require __DIR__ . '/vendor/autoload.php';



(new SingleCommandApplication())
  ->setCode(function (InputInterface $in, ConsoleOutputInterface $out) {
    $dannyBuderus = 'Danny Buderus';
    $nathanHindmarsh = 'Nathan Hindmarsh';
    $table = new Table($out);
    $table->setHeaders(
      ['Year', 'FB', 'WG', 'CE', 'FE', 'HB', 'LK', 'SR', 'PR', 'HK', 'INT']
    );
    $years = [
      [
        2004,
        'Anthony Minichiello',
        'Amos Roberts',
        'Willie Tonga',
        'Darren Lockyer',
        'Brett Finch',
        'Shaun Timmins',
        $nathanHindmarsh,
        'Paul Rauhihi',
        $dannyBuderus,
      ],
      [
        2005,
        'Brett Hodgson',
        'Eric Grothe Jr',
        'Mark Gasnier',
        'Benji Marhsall',
        'Johnathan Thurston',
        'Ben Kennedy',
        $nathanHindmarsh,
        'Luke Bailey',
        $dannyBuderus
      ],
      [
        2006,
        'Clinton Schifkofske',
        'Brian Carney',
        'Mark Gasnier',
        'Darren Lockyer',
        'Cooper Cronk',
        'Ben Kennedy',
        $nathanHindmarsh,
        'Roy Asotasi',
        'Cameron Smith'
      ],
      [
        2007,
        'Matt Bowen',
        'Jarryd Hayne',
        'Justin Hodges',
        'Darren Lockyer',
        'Johnathan Thurston',
        'Dallas Johnson',
        'Anthony Watmough',
        'Steve Price',
        'Robbie Farah',
      ],
      [
        2008,
        'Billy Slater',
        'Colin Best',
        'Israel Folau',
        'Greg Inglis',
        'Matt Orford',
        'Alan Tongue',
        'Glenn Stewart',
        'Petero Civoniceva',
        'Cameron Smith',
      ],
      [
        2009,
        'Jarryd Hayne',
        'Taniela Tuiaki',
        'Josh Morris',
        'Jamie Soward',
        'Johnathan Thurston',
        'David Stagg',
        'Anthony Watmough',
        'Ben Hannant',
        'Michael Ennis',
      ],
      [
        2010,
        'Darius Boyd',
        'Akuila Uate',
        'Jamie Lyon',
        'Todd Carney',
        'Scott Prince',
        'Luke Lewis',
        'Sam Thaiday',
        'David Shillington',
        'Robbie Farah',
      ],
      [
        2011,
        'Billy Slater',
        'Akuila Uate',
        'Jamie Lyon',
        'Benji Marshall',
        'Cooper Cronk',
        'Paul Gallen',
        'Sam Thaiday',
        'Matthew Scott',
        'Cameron Smith',
      ],
      [
        2012,
        'Ben Barba',
        'Akuila Uate',
        'Josh Morris',
        'Johnathan Thurston',
        'Cooper Cronk',
        'Paul Gallen',
        'Nate Myles',
        'Sam Kasiano',
        'Cameron Smith',
      ],
      [
        2013,
        'Greg Inglis',
        'Roger Tuivasa-Sheck',
        'Jamie Lyon',
        "Todd Carney\nJohnathan Thurston",
        'Cooper Cronk',
        'Corey Parker',
        'Boyd Cordner',
        'Andrew Fifita',
        'Cameron Smith',
      ],
      [
        2014,
        'Jarryd Hayne',
        'Semi Radradra',
        'Jamie Lyon',
        'Johnathan Thurston',
        'Daly Cherry-Evans',
        'Sam Burgess',
        'Beau Scott',
        'James Graham',
        'James Segeyaro',
      ],
      [
        2015,
        'Roger Tuivasa-Sheck',
        'Semi Radradra',
        'James Roberts',
        'Blake Austin',
        'Johnathan Thurston',
        'Jason Taumalolo',
        'Josh Jackson',
        'Aaron Woods',
        'Michael Ennis',
      ],
      [
        2016,
        'James Tedesco',
        'Josh Mansour',
        'Joseph Leilua',
        'James Maloney',
        'Cooper Cronk',
        'Jason Taumalolo',
        'Matt Gillett',
        'Jesse Bromwich',
        'Cameron Smith',
      ],
      [
        2017,
        'Billy Slater',
        'Jordan Rapana',
        'Dylan Walker',
        'Gareth Widdop',
        'Michael Morgan',
        'Paul Gallen',
        'Matt Gillett',
        'Aaron Woods',
        'Cameron Smith',
        'Reagan Campbell-Gillard',
      ],
      [
        2018,
        'Roger Tuivasa-Sheck',
        'Blake Ferguson',
        'Joseph Leilua',
        'Cameron Munster',
        'Luke Brooks',
        'Jason Taumalolo',
        'Josh Jackson',
        'Andrew Fifita',
        'Damien Cook',
        'Jazz Tevaga',
      ],
      [
        2019,
        'James Tedesco',
        'Ken Maumalo',
        'Latrell Mitchell',
        'Cameron Munster',
        'Mitchell Moses',
        'Cameron Murray',
        'John Bateman',
        'Payne Haas',
        'Cameron Smith',
        'Brandon Smith',
      ],
      [
        2020,
        'Clint Gutherson',
        "David Nofoaluma\nJosh Addo-Carr",
        "Kotoni Staggs\nStephen Crichton",
        'Jack Wighton',
        'Nathan Cleary',
        'Isaah Yeo',
        "Viliame Kikau\nTohu Harris",
        "Josh Papalii\nJames Fisher-Harris",
        'Cameron Smith',
      ],
      [
        2021,
        'Tom Trbojevic',
        "Reuben Garrick\nBrian To'o",
        "Justin Olam\nMatt Burton",
        'Cody Walker',
        'Nathan Cleary',
        'Isaah Yeo',
        "Viliame Kikau\nIsaiah Papali'i",
        "James Fisher-Harris\nPayne Haas",
        'Brandon Smith',
      ],
      [
        2022,
        'James Tedesco',
        "Joseph Suaalii\nAlex Johnston",
        "Joseph Manu\nValentine Holmes",
        'Cameron Munster',
        'Nicho Hynes',
        'Isaah Yeo',
        "Jeremiah Nanai\nViliame Kikau",
        "Payne Haas\nJoseph Tapine",
        'Apisai Koroisau',
      ],
    ];

    foreach ($years as $key => $year) {
      $table->addRow($year);
      if ($key < count($years) - 1) {
        $table->addRow(new TableSeparator());
      }
    }
    $table->render();
  })->run();
