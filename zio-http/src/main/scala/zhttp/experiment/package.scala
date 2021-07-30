package zhttp

package object experiment {
  type AnyRequest  = HRequest[Any, Nothing]
  type AnyResponse = HResponse[Any, Nothing]
}
