import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取桥梁工序
const useBridgeProcess = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.bridgeProcess)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchBridgeProcess')
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
    bridgeProcess: computed(() => store.state.config.bridgeProcess),
    loaded
  }
}

export default useBridgeProcess
