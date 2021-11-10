import { computed } from 'vue'
import { useStore } from 'vuex'

// 获取工序
const useProcess = () => {
  const store = useStore()
  // 拉取未加载的工序
  if (!store.state.config.loaded.process) {
    store.dispatch('config/fetchProcess')
  }
  return {
    process: computed(() => store.state.config.process),
    loaded: computed(() => store.state.config.loaded.process)
  }
}

export default useProcess
