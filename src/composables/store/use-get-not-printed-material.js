import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取车间
const useGetNotPrintedMaterial = (loadedCallBack, reload = true) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.notPrintedMaterialNumber)

  // 未加载则拉取
  if (!loaded.value || reload) {
    store.dispatch('wms/fetchNotPrintedMaterialNumber')
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
    notPrintedMaterialNumber: computed(() => store.state.wms.notPrintedMaterialNumber),
    loaded: computed(() => store.state.wms.loaded.notPrintedMaterialNumber),
    refresh: () => {
      store.dispatch('wms/fetchNotPrintedMaterialNumber')
    }
  }
}

export default useGetNotPrintedMaterial
