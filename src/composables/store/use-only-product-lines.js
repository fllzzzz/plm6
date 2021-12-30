import { computed } from 'vue'
import { useStore } from 'vuex'

// 获取生产线
const useOnlyProductLines = () => {
  const store = useStore()
  // 拉取未加载的生产线
  if (!store.state.config.loaded.onlyProductLines) {
    store.dispatch('config/fetchOnlyProductLines')
  }
  return {
    onlyProductLines: computed(() => store.state.config.onlyProductLines),
    loaded: computed(() => store.state.config.loaded.onlyProductLines)
  }
}

export default useOnlyProductLines
