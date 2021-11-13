import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取字典值
const useUnit = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.unit)
  // 拉取未加载的字典值
  if (!loaded.value) {
    store.dispatch('config/fetchUnit')
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
    unit: computed(() => store.state.config.unit),
    loaded
  }
}

export default useUnit
