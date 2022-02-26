import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取变更原因
const useChangeReason = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.changeReasonConfig)
  console.log(loaded)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchChangeReasonConfig')
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
    changeReasonConfig: computed(() => store.state.config.changeReasonConfig),
    loaded
  }
}

export default useChangeReason
