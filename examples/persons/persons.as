import mx.rpc.events.ResultEvent;
import mx.rpc.events.FaultEvent;
import mx.controls.Alert;
import mx.utils.ArrayUtil;
import mx.collections.ArrayCollection;
import Person;
	
[Bindable] private var dp:ArrayCollection;
	
[Bindable] private var selectedPerson:Person;
	
private function resultHandler(event:ResultEvent):void {
     dp = new ArrayCollection(ArrayUtil.toArray(event.result));
}

private function faultHandler(event:FaultEvent):void {
     Alert.show(event.fault.faultString);
}
