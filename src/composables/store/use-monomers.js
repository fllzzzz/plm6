import { computed } from 'vue'
import { useStore } from 'vuex'
import { isBlank } from '@data-type/index'

// 获取字典值
const useMonomers = (projectId) => {
  const store = useStore()
  const stateMonomers = store.state.config.monomers
  const unload = isBlank(stateMonomers[projectId])
  // 拉取未加载项目的单体
  if (unload) {
    store.dispatch('config/fetchMonomer', projectId)
  }
  return computed(() => store.state.config.monomers)
}

export default useMonomers
