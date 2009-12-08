import mx.rpc.events.ResultEvent;
import mx.rpc.events.FaultEvent;
import mx.controls.Alert;
import mx.collections.ArrayCollection;
import mx.utils.ArrayUtil;

[Bindable]
private var Symbols:ArrayCollection;

private function symbolsHandler(event:ResultEvent):void
{
     Symbols = new ArrayCollection(ArrayUtil.toArray(event.result));
}

private function quoteHandler(event:ResultEvent):void
{
     stockPrice.text = String(event.result);
}

private function faultHandler(event:FaultEvent):void
{
     Alert.show(event.fault.faultString);
}
