#!/usr/bin/env php
<?php

require __DIR__ . '/vendor/autoload.php';

use Exception;
use Generator;
use Symfony\Component\Console\Command\Command;
use Symfony\Component\Console\Helper\QuestionHelper;
use Symfony\Component\Console\Helper\Table;
use Symfony\Component\Console\Helper\TableSeparator;
use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Output\OutputInterface;
use Symfony\Component\Console\Question\Question;
use Symfony\Component\Console\SingleCommandApplication;
use Symfony\Component\DomCrawler\Crawler;
use Symfony\Component\HttpClient\Exception\ClientException;
use Symfony\Component\HttpClient\HttpClient;

const BASEURL = 'https://www.mut.gg';

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


/**
 * @return array<string, string>
 */
function getBaseData(string $index): array
{
    /** @var array<string, array<string, string>> $baseData */
    $baseData = json_decode(getFromUrl(BASEURL . '/api/23/core-data/'), true);
    $data = $baseData['data'];
    return $data[$index];
}

function getAllPrograms(): array
{
    $defs = getBaseData('programs');
    $return = [];
    foreach ($defs as $def) {
        $return[$def['id']] = $def['name'];
    }
    ksort($return);
    return $return;
}

function getChemistryDefsForTeams(): array
{
    $defs = getBaseData('chemistryDefs');
    $defs = array_filter(
        $defs,
        fn (array $def) => $def['chemistryType'] === 1
    );
    usort($defs, fn (array $def1, array $def2) => $def1['sortOrder'] - $def2['sortOrder']);
    return $defs;
}

function getProgramId(string $programName): int
{
    $defs = getBaseData('programs');
    $defs = array_filter(
        $defs,
        fn (array $def) => $def['name'] === $programName
    );
    $result = end($defs);
    if ($result === null) {
        throw new Exception('No program found with that name');
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
    $chemistryDefs = getChemistryDefsForTeams();
    foreach ($chemistryDefs as $chemistryDef) {
        $playerNames = [];
        $break = false;
        $i = 1;
        while (! $break) {
            $query = http_build_query([
                'program_id' => $programId,
                'market' => 2,
                'team_chem' => $chemistryDef['displaySlug'],
                'page' => $i,
            ]);
            try {
                $newPlayers = getPlayerNamesFromPage(BASEURL . '/players/?' . $query);
                if (empty($newPlayers)) {
                    $break = true;
                    continue;
                }
                $playerNames = array_merge($playerNames, $newPlayers);
                sort($playerNames);
                $i++;
            } catch (ClientException $e) {
                $break = true;
            }
        }
        yield $chemistryDef['name'] => $playerNames;
    }
}

(new SingleCommandApplication())
    ->setCode(function (InputInterface $input, OutputInterface $output): int {
        $helper = new QuestionHelper();
        $allPrograms = array_filter(getAllPrograms());
        sort($allPrograms);
        $question = new Question(
            implode(
                "",
                array_map(
                    fn (string $str) => $str . PHP_EOL,
                    [
                        'Please select what program you want from the following list:',
                        implode(
                            PHP_EOL,
                            array_map(
                                fn (string $str) => "  - {$str}",
                                $allPrograms
                            )
                        ),
                    ]
                )
            )
        );
        $question->setAutocompleterValues($allPrograms);
        $programName = $helper->ask($input, $output, $question);
        $allPlayersInTeams = getAllPlayersForTeamsForProgram($programName);
        $section = $output->section();
        $table = new Table($section);
        $table
            ->setHeaders(['Team Name', 'Players'])
            ->render();
        $addSeparator = false;
        foreach ($allPlayersInTeams as $team => $players) {
            if ($addSeparator) {
                $table->appendRow(new TableSeparator());
            } else {
                $addSeparator = ! $addSeparator;
            }
            $numPlayers = count($players);
            $table->appendRow(["{$team} ({$numPlayers})", wordwrap(implode(', ', $players))]);
        }
        return Command::SUCCESS;
    })
    ->run();
