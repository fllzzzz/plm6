import { computed } from 'vue'
import { useStore } from 'vuex'

// 获取生产线
const useEnclosureProductLines = () => {
  const store = useStore()
  // 拉取未加载的生产线
  if (!store.state.config.loaded.enclosureProductLines) {
    store.dispatch('config/fetchEnclosureProductLines')
  }
  return {
    enclosureProductLines: computed(() => store.state.config.enclosureProductLines),
    loaded: computed(() => store.state.config.loaded.enclosureProductLines)
  }
}

export default useEnclosureProductLines
