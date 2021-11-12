import { computed } from 'vue'
import { useStore } from 'vuex'

// 获取部门列表
const useDept = () => {
  const store = useStore()
  // 未加载则拉取
  if (!store.state.config.loaded.dept) {
    store.dispatch('config/fetchDept')
  }
  return {
    dept: computed(() => store.state.config.dept),
    loaded: computed(() => store.state.config.loaded.dept)
  }
}

export default useDept
