import { computed } from 'vue'
import { useStore } from 'vuex'

// 获取字典值
const useFactory = () => {
  const store = useStore()
  // 拉取未加载的字典值
  if (!store.state.config.loaded.factories) {
    store.dispatch('config/fetchFactories')
  }
  return {
    factories: computed(() => store.state.config.factories),
    loaded: computed(() => store.state.config.loaded.factories)
  }
}

export default useFactory
