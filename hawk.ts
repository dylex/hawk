namespace _HaWK__ {

  type Block = {
    def: boolean,
    exc?: RegExp
  };

  interface Blocks {
    def: Block;
    [type:string]: Block;
  }

  const blocks: Blocks = {
    def: {
      def: false
    },
    LINK: {
      def: true
    },
    IMG: {
      def: true
    },
  };

  export var blockSrc: RegExp|undefined;

  type LoadedElement = HTMLElement&{src?:string};

  function blockTest(type: string, src: string|undefined) {
    let block = blocks[type/*.toUpperCase()*/];
    if (!block) {
      type += "<def>";
      block = blocks.def;
    }
    let res = block.def;
    if (!block.exc && res && blockSrc)
      block.exc = blockSrc;
    if (src && block.exc && block.exc.test(src))
      res = !res;
    if (!res || res !== block.def)
      console.log((res ? "allowing" : "blocking") + " " + type + " " + src);
    return res;
  }

  function initDocument(doc: HTMLDocument) {
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
      if (!blockTest(el.tagName, el.src)) {
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

  function linkFocus(n: number) {
    linkSelected = n;
    links[n].focus();
  }

  export function linkSelect(link: string, match: RegExp) {
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

  console.log("hawk loaded");
}
