import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取工厂
const useFactory = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.factories)
  // 若工厂未加载则拉取工厂
  if (!loaded.value) {
    store.dispatch('config/fetchFactories')
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
    factoryKV: computed(() => store.state.config.factoryKV),
    factories: computed(() => store.state.config.factories),
    loaded
  }
}

export default useFactory
