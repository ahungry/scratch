<?php

// First attempt - not great
function getIp () {
    return json_decode(file_get_contents('http://httpbin.org/ip'));
}

// Add some more robust code/checks.

// f,g = f(g(x1, x2, xN...))
function compose ($f, $g) {
    return function () use ($f, $g) {
        return $f(call_user_func_array($g, func_get_args()));
    };
}

$assert = function (string $message = 'Bad Argument') {
    return function ($x) use ($message) {
        if (false === (bool) $x) throw new \InvalidArgumentException($message);
    };
};
$isIpFormat = function (string $s) { return preg_match('/^\d+\.\d+\.\d+\.\d+/', $s); };
$assertIpFormat = compose($assert('Invalid IP format'), $isIpFormat);

class IpModel {
    private $ip;

    // Builds a model from JSON.
    public function __construct(string $json) {
        $this->unserialize($json);
    }

    public function setIp (string $ip) {
        global $assertIpFormat;
        $assertIpFormat($ip);
        $this->ip = $ip;
    }

    public function unserialize(string $json) {
        $tmp = json_decode($json);
        $this->setIp($tmp->origin);
    }
}

$fakeResponse = function () { return json_encode(['origin' => 'xxx']); };
$getIp = function () { return @file_get_contents('http://httpbin.org/delay/10'); };
$getIpWithTimeout = function () {
    global $getIp, $fakeResponse;
    ini_set('default_socket_timeout', 1);
    $raw = $getIp();

    // Here, we re-join the model path to make use of it's property assertion logic.
    return false === $raw ? $fakeResponse() : $raw;
};
$makeIpModel = function (string $json) { return new IpModel($json); };
$getIpModel = compose($makeIpModel, $getIpWithTimeout);

$model = $getIpModel();
var_dump($model);
