import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取全部采购单
const usePurchaseOrder = (loadedCallBack, reload = false) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.purchaseOrders)
  // 未加载则拉取
  if (!loaded.value || reload) {
    store.commit('config/SET_LOADED', { key: 'purchaseOrders', loaded: false })
    store.dispatch('config/fetchPurchaseOrder')
  }

  // 加载成功回调
  if (loadedCallBack) {
    const monitor = watch(
      loaded,
      (flag) => {
        if (flag) {
          setTimeout(() => {
            loadedCallBack()
            monitor()
          }, 0)
        }
      },
      { immediate: true }
    )
  }

  return {
    loaded,
    purchaseOrders: computed(() => store.state.config.purchaseOrders),
    purchaseOrderKV: computed(() => store.state.config.purchaseOrderKV)
  }
}

export default usePurchaseOrder
