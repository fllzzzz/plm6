import { computed } from 'vue'
import { useStore } from 'vuex'

// 获取工厂
const useFactory = () => {
  const store = useStore()
  // 若工厂未加载则拉取工厂
  if (!store.state.config.loaded.factories) {
    store.dispatch('config/fetchFactories')
  }
  return {
    factoryKV: computed(() => store.state.config.factoryKV),
    factories: computed(() => store.state.config.factories),
    loaded: computed(() => store.state.config.loaded.factories)
  }
}

export default useFactory
