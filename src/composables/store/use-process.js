import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取工序
const useProcess = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.process)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchProcess')
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
    process: computed(() => store.state.config.process),
    loaded
  }
}

export default useProcess
