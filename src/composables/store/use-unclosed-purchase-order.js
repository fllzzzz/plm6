import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取未关闭的采购合同
const useUnclosedRequisition = (loadedCallBack, reload = false) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.unclosedPurchaseOrder)
  // 未加载则拉取
  if (!loaded.value || reload) {
    store.commit('config/SET_LOADED', { key: 'unclosedPurchaseOrder', loaded: false })
    store.dispatch('config/fetchUnclosedPurchaseOrder')
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
    purchaseOrder: computed(() => store.state.config.unclosedPurchaseOrder)
  }
}

export default useUnclosedRequisition
