addEventListener("fetch", event => {
  event.respondWith(handleRequest(event.request))
})

async function handleRequest(request) {
  const url = new URL(request.url);

    const proxyRequest = new Request(
        url, 
        request
    );
    const proxyResponse = await fetch(proxyRequest);
    return {
      ...proxyResponse,
      headers: {
        'Access-Control-Allow-Origin': '*'
      }
    };
}