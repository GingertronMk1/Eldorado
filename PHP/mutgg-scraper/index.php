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
use Symfony\Component\Console\Output\ConsoleOutputInterface;
use Symfony\Component\Console\Question\Question;
use Symfony\Component\Console\SingleCommandApplication;
use Symfony\Component\Console\Style\SymfonyStyle;
use Symfony\Component\DomCrawler\Crawler;
use Symfony\Component\HttpClient\Exception\ClientException;
use Symfony\Component\HttpClient\HttpClient;

const BASEURL = 'https://www.mut.gg';

final class MutGGApiData
{
    /** @param array<int, mixed> $abilities
     *  @param array<int, mixed> $programs
     *  @param array<int, mixed> $teams
     *  @param array<int, mixed> $stats
     *  @param array<int, mixed> $featuredRatings
     *  @param array<int, mixed> $chemistryDefs
     */
    public function __construct(
        public readonly array $abilities = [],
        public readonly array $programs = [],
        public readonly array $teams = [],
        public readonly array $stats = [],
        public readonly array $featuredRatings = [],
        public readonly array $chemistryDefs = [],
    ) {
    }
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


function getBaseData(): MutGGApiData
{
    /** @var array<string, array<string, array<int, string>>> $baseData */
    $baseData = json_decode(getFromUrl(BASEURL . '/api/23/core-data/'), true);

    if (! isset($baseData['data'])) {
        throw new Exception('No data');
    }

    $data = $baseData['data'];

    return new MutGGApiData(
        $data['abilities'],
        $data['programs'],
        $data['teams'],
        $data['stats'],
        $data['featuredRatings'],
        $data['chemistryDefs'],
    );
}

/**
 * @return array<string, string>
 */
function getAllPrograms(): array
{
    $defs = getBaseData()->programs;
    $return = [];
    foreach ($defs as $def) {
        /** @var array<string, string> $def */
        $return[$def['id']] = $def['name'];
    }
    ksort($return);
    return $return;
}

/**
 * @return  array<int, mixed>
 */
function getChemistryDefsForTeams(): array
{
    $defs = getBaseData()->chemistryDefs;

    /** @var callable $filterFn */
    $filterFn = fn (array $def) => $def['chemistryType'] === 1;

    $defs = array_filter(
        $defs,
        $filterFn
    );
    /** @var callable $sortFn */
    $sortFn = fn (array $def1, array $def2) => intval($def1['sortOrder'] - $def2['sortOrder']);
    usort($defs, $sortFn);
    return $defs;
}

function getProgramId(string $programName): int
{
    $defs = getBaseData()->programs;

    /** @var callable $filterFn */
    $filterFn = fn (array $def) => $def['name'] === $programName;

    $defs = array_filter(
        $defs,
        $filterFn
    );
    /** @var null|false|array<string, int|string> $result */
    $result = end($defs);
    if (! $result) {
        throw new Exception('No program found with that name');
    }
    return intval($result['id']);
}

/**
 * @return array<string>
 */
function getPlayerNamesFromPage(string $url): array
{
    $pageContent = getFromUrl($url);
    $crawler = new Crawler($pageContent);
    $crawler = $crawler
        ->filter('.player-list-item__name')
        ->each(fn (Crawler $node) => $node->text());

    return $crawler;
}

/**
 * @return Generator<string, array<int, string>>
 */
function getAllPlayersForTeamsForProgram(string $programName): Generator
{
    $programId = getProgramId($programName);
    $chemistryDefs = getChemistryDefsForTeams();
    foreach ($chemistryDefs as $chemistryDef) {
        /** @var array<string, string> $chemistryDef */
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
    ->setCode(function (InputInterface $input, ConsoleOutputInterface $output): int {
        $io = new SymfonyStyle($input, $output);
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
        /** @var string $programName */
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
                $addSeparator = true;
            }
            $numPlayers = count($players);
            $table->appendRow(["{$team} ({$numPlayers})", wordwrap(implode(', ', $players))]);
        }
        return Command::SUCCESS;
    })
    ->run();
