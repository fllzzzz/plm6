import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取生产班组全部数据
const useBridgeCutConfig = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.bridgeCutConfigs)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchBridgeCutConfig')
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
    bridgeCutConfigs: computed(() => store.state.config.bridgeCutConfigs || []),
    bridgeCutConfigKV: computed(() => store.state.config.bridgeCutConfigKV),
    loaded
  }
}

export default useBridgeCutConfig
