import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取金额小数精度
const useSteelMinLengthConfig = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.steelMinLengthConfig)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchSteelMinLengthConfig')
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
    steelMinLengthConfig: computed(() => store.state.config.steelMinLengthConfig),
    loaded
  }
}

export default useSteelMinLengthConfig
