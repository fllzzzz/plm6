import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取人员部门列表
const userDeptTree = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.userDeptTree)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchUserDeptTree')
  }

  // 加载成功回调
  if (loadedCallBack) {
    const monitor = watch(
      loaded,
      (flag) => {
        if (flag) {
          setTimeout(() => {
            loadedCallBack()
            monitor()
          }, 0)
        }
      },
      { immediate: true }
    )
  }

  return {
    userDeptTree: computed(() => store.state.config.userDeptTree),
    loaded
  }
}

export default userDeptTree
