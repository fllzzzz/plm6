import { computed } from 'vue'
import { useStore } from 'vuex'

// 获取工序
const useProcess = () => {
  const store = useStore()
  // 未加载则拉取
  if (!store.state.config.loaded.process) {
    store.dispatch('config/fetchProcess')
  }
  return {
    process: computed(() => store.state.config.process),
    loaded: computed(() => store.state.config.loaded.process)
  }
}

export default useProcess
