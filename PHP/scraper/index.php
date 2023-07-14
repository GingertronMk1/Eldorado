#!/usr/bin/env php
<?php

require __DIR__ . '/vendor/autoload.php';

use Symfony\Component\Console\Helper\QuestionHelper;
use Symfony\Component\Console\Helper\Table;
use Symfony\Component\Console\Helper\TableSeparator;
use Symfony\Component\Console\Question\Question;
use Symfony\Component\DomCrawler\Crawler;
use Symfony\Component\HttpClient\HttpClient;
use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Output\OutputInterface;
use Symfony\Component\Console\SingleCommandApplication;
use Symfony\Component\Console\Command\Command;

function nicePrint(array $arr): void
{
  echo json_encode($arr, JSON_PRETTY_PRINT) . PHP_EOL;
}

function getFromUrl(string $url, string $method = 'GET'): string
{
  $client = HttpClient::create();
  $response = $client->request(
    $method,
    $url
  );

  $content = $response->getContent();
  return $content;
}


function getBaseData(): array
{
  return json_decode(getFromUrl('https://www.mut.gg/api/23/core-data/'), true)['data'];
}

function getAllPrograms(): array
{
  $defs = getBaseData()['programs'];
  $return = [];
  foreach($defs as $def)
  {
    $return[$def['id']] = $def['name'];
  }
  ksort($return);
  return $return;
}

function getChemistryDefsForTeams(): array
{
  $defs = getBaseData()['chemistryDefs'];
  $defs = array_filter(
    $defs,
    fn (array $def) => $def['chemistryType'] === 1
  );
  usort($defs, fn (array $def1, array $def2) => $def1['sortOrder'] - $def2['sortOrder']);
  return $defs;
}

function getProgramId(string $programName): int
{
  $defs = getBaseData()['programs'];
  $defs = array_filter(
    $defs,
    fn (array $def) => $def['name'] === $programName
  );
  $result = array_shift($defs);
  if ($result === null) {
    throw new \Exception('No program found with that name');
  }
  return $result['id'];
}

function getPlayerNamesFromPage(string $url): array
{
  $pageContent = getFromUrl($url);
  $crawler = new Crawler($pageContent);
  $crawler = $crawler
    ->filter('.player-list-item__name')
    ->each(fn (Crawler $node) => $node->text());

  return $crawler;
}

function getAllPlayersForTeamsForProgram(string $programName): Generator
{
  $programId = getProgramId($programName);
  foreach(getChemistryDefsForTeams() as $chemistryDef) {
    $playerNames = getPlayerNamesFromPage("https://www.mut.gg/players/?program_id={$programId}&market=2&team_chem={$chemistryDef['displaySlug']}");
    sort($playerNames);
    yield $chemistryDef['name'] => $playerNames;
  }
}

(new SingleCommandApplication())
    ->setCode(function (InputInterface $input, OutputInterface $output): int {
      $helper = new QuestionHelper();
      $question = new Question('Please select what program you want: ');
      $question->setAutocompleterValues(getAllPrograms());
      $programName = $helper->ask($input, $output, $question);
        $section = $output->section();
        $table = new Table($section);
        $allPlayersInTeams = getAllPlayersForTeamsForProgram($programName); 
        $table
          ->setHeaders(['Team Name', 'Players'])
          ->render();
        $addSeparator = false;
        foreach($allPlayersInTeams as $team => $players) {
          if ($addSeparator) {
            $table->appendRow(new TableSeparator());
          } else {
            $addSeparator = !$addSeparator;
          }
          $table->appendRow([$team, implode(', ', $players)]);
        }
        return Command::SUCCESS;
    })
    ->run();