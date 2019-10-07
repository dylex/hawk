namespace _HaWK__ {

  function uriDomain(u: string|undefined): string|undefined {
    if (!u)
      return;
    const a = document.createElement('a');
    a.href = u;
    return a.hostname;
  }

  type DomainMap<a> = {$?:a,[d:string]:DomainMap<a>|a|undefined}

  function lookupDomain<a>(map: DomainMap<a>|undefined, dom: string|undefined): a|undefined {
    if (!map)
      return;
    let v = map.$;
    if (!dom)
      return v;
    const d = dom.split('.').reverse();
    for (let c of d) {
      map = <DomainMap<a>>map[c];
      if (!map) break;
      if (map.$ != undefined)
        v = map.$;
    }
    return v;
  }

  type LoadSet = number
  export var loadSet: {[type:string]:LoadSet}
  export var allow: DomainMap<LoadSet>|undefined

  type LoadedElement = HTMLElement&{src?:string,href?:string}

  function loadAllow(type: string, src: string|undefined): boolean {
    const a = lookupDomain(allow, uriDomain(src));
    const b = !(<LoadSet>a & loadSet[type]);
    if (b || type === 'SCRIPT' || type === 'IFRAME' || type === 'OBJECT')
      console.log((b ? "-" : "+") + " " + (<any>type).padEnd(6) + " " + src);
    return !b;
  }

  function initDocument(doc: HTMLDocument): void {
    /* maybe should be click: */
    doc.addEventListener('focusin', function focusin(event) {
      if (event.target instanceof HTMLInputElement && (event.target.type === 'text' || event.target.type === 'password') || 
          event.target instanceof HTMLTextAreaElement) {
        (<any>window).webkit.messageHandlers.hawk.postMessage('input');
      }
    }, false);
    doc.addEventListener('beforeload', function beforeload(event) {
      const el = <LoadedElement>event.target;
      if (!(el instanceof HTMLElement))
        return;
      if (!loadAllow(el.tagName, el.src || el.href)) {
        event.preventDefault();
        if (el.parentNode)
          el.parentNode.removeChild(el);
      } else if (el instanceof HTMLIFrameElement) {
        if (el.contentDocument)
          /* this does not work, possibly because the document hasn't started loading yet */
          initDocument(el.contentDocument);
      }
    }, true);
  }

  initDocument(document);

  const links = document.getElementsByTagName("a");

  var linkSelected = -1;

  function linkFocus(n: number): void {
    linkSelected = n;
    links[n].focus();
  }

  export function linkSelect(link: string, match: RegExp): void {
    const el = document.querySelector('link[rel=' + link + ']');
    if (el instanceof HTMLLinkElement) {
      location.assign(el.href);
      return;
    }
    for (var i = linkSelected+1; i < links.length; i++) {
      if (match.test(links[i].text)) {
        linkFocus(i);
        break;
      }
    }
    linkSelected = -1;
  }

}
