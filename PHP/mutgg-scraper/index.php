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
use Symfony\Component\Console\Question\ChoiceQuestion;
use Symfony\Component\Console\Question\Question;
use Symfony\Component\Console\SingleCommandApplication;
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

final class MutGGPlayer
{
    public function __construct(
        public readonly int $ovr,
        public readonly string $firstName,
        public readonly string $lastName,
        public readonly string $position,
        public readonly string $teamName
    ) {
    }

    public function __toString(): string
    {
        return "{$this->ovr}OVR {$this->firstName} {$this->lastName}";
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

function getPageAsCrawler(string $url): Crawler
{
    $pageContent = getFromUrl($url);
    return new Crawler($pageContent);
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

function getPlayerDataFromLink(string $str): array
{
    $playerLinkExploded = array_filter(explode('/', $str));
    $playerId = end($playerLinkExploded);
    return getPlayerDataFromApi($playerId);
}

function getPlayerDataFromApi(string $str): array
{
    $jsonData = getFromUrl(BASEURL . "/api/mutdb/player-items/{$str}");
    $decodedData = json_decode($jsonData, true);
    return $decodedData['data'];
}

/**
 * @return Generator<MutGGPlayer>
 */
function getPlayerNamesFromPage(string $url): Generator
{
    $crawler = getPageAsCrawler($url);
    $crawler = $crawler
        ->filter('.player-list-item__link')
        ->each(fn (Crawler $node) => $node->attr('href'));

    foreach ($crawler as $link) {
        $playerData = getPlayerDataFromLink($link);
        yield new MutGGPlayer(
            $playerData['overall'],
            $playerData['firstName'],
            $playerData['lastName'],
            $playerData['position']['name'],
            $playerData['team']['name']
        );
    }
}

/**
 * @return Generator<MutGGPlayer>
 */
function getAllPlayersForProgram(string $programName): Generator
{
    $programId = getProgramId($programName);
    /** @var array<string, string> $chemistryDef */
    $break = false;
    $i = 1;
    while (! $break) {
        $query = http_build_query([
            'program_id' => $programId,
            'market' => 2,
            'page' => $i,
        ]);
        try {
            $newPlayers = getPlayerNamesFromPage(BASEURL . '/players/?' . $query);
            if (empty($newPlayers)) {
                $break = true;
                continue;
            }
            foreach ($newPlayers as $newPlayer) {
                yield $newPlayer;
            }
            $i++;
        } catch (ClientException $e) {
            break;
        }
    }
}

(new SingleCommandApplication())
    ->setCode(function (InputInterface $input, ConsoleOutputInterface $output): int {
        $programHelper = new QuestionHelper();
        $allPrograms = array_filter(getAllPrograms());
        asort($allPrograms);
        $programQuestion = new ChoiceQuestion(
          'What program do you want?',
          $allPrograms
        );
        /** @var string $programName */
        $programName = $programHelper->ask($input, $output, $programQuestion);

        $groupAttribute = 'teamName';

        $playerClassVars = get_class_vars(MutGGPlayer::class);
        $playerAttributes = array_keys($playerClassVars);

        $groupHelper = new QuestionHelper();
        $groupQuestion = new ChoiceQuestion(
          'What player attribute would you like to group by?',
          $playerAttributes
        );
        $groupAttribute = $groupHelper->ask($input, $output, $groupQuestion);

        if (!in_array($groupAttribute, $playerAttributes)) {
          throw new Exception();
        };

        $output->write(sprintf("\033\143"));
        $allPlayers = getAllPlayersForProgram($programName);
        $section = $output->section();
        $allPlayersArray = [];
        $playersProcessed = 0;
        foreach ($allPlayers as $player) {
            if (isset($allPlayersArray[$player->$groupAttribute])) {
                $allPlayersArray[$player->$groupAttribute][] = $player;
            } else {
                $allPlayersArray[$player->$groupAttribute] = [$player];
            }
            $playersProcessed++;
            $section->overwrite("Processed {$playersProcessed} players - currently on {$player}");
        }
        $table = new Table($section);
        $table->setHeaders(['Position', 'Players']);
        uasort(
            $allPlayersArray,
            fn (array $arr1, array $arr2) => count($arr1) - count($arr2)
        );
        $tableRows = [];
        foreach ($allPlayersArray as $position => $players) {
            $tableRows[] = [
              count($players) . ' ' . $position,
                wordwrap(implode(', ', $players), 120),
            ];
            $tableRows[] = new TableSeparator();
        }
        array_pop($tableRows);
        $table->setRows($tableRows);
        $section->clear();
        $table->render();
        return Command::SUCCESS;
    })
    ->run();
