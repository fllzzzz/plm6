import { computed } from 'vue'
import { useStore } from 'vuex'

// 获取字典值
const useUnit = () => {
  const store = useStore()
  // 拉取未加载的字典值
  if (!store.state.config.loaded.unit) {
    store.dispatch('config/fetchUnit')
  }
  return {
    unit: computed(() => store.state.config.unit),
    loaded: computed(() => store.state.config.loaded.unit)
  }
}

export default useUnit
