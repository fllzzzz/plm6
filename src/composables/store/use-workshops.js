import { computed } from 'vue'
import { useStore } from 'vuex'

// 获取车间
const useWorkshop = () => {
  const store = useStore()
  // 未加载则拉取
  if (!store.state.config.loaded.workshops) {
    store.dispatch('config/fetchWorkshops')
  }
  return {
    workshops: computed(() => store.state.config.workshops),
    loaded: computed(() => store.state.config.loaded.workshops)
  }
}

export default useWorkshop
