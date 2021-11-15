import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取部门列表
const useDept = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.dept)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchDept')
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
    dept: computed(() => store.state.config.dept),
    loaded
  }
}

export default useDept
