import { computed } from 'vue'
import { useStore } from 'vuex'

// 获取人员列表
const useUsers = () => {
  const store = useStore()
  // 拉取未加载的人员列表
  if (!store.state.config.loaded.users) {
    store.dispatch('config/fetchUsers')
  }
  return {
    users: computed(() => store.state.config.users),
    loaded: computed(() => store.state.config.loaded.users)
  }
}

export default useUsers
