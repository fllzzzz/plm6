import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取签证原因
const useVisaReason = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.visaReason)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchVisaReason')
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
    visaReason: computed(() => store.state.config.visaReason),
    loaded
  }
}

export default useVisaReason
