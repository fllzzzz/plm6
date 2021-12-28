import { clear } from '@/api/mes/scheduling-manage/scheduling/common'
import { ref } from 'vue'
import { ElNotification } from 'element-plus'

export default function useSchedulingClear({ successHook }) {
  const clearPopVisible = ref(false) // 清空排产提示
  const clearLoading = ref(false) // 清空排产

  const handleClear = async (selects, productType) => {
    clearPopVisible.value = false
    clearLoading.value = true
    try {
      await clear({
        productType: productType,
        productId: selects.map(v => v.id)
      })
      ElNotification({ title: '清空成功', type: 'success', duration: 2500 })
    } catch (error) {
      console.log('reassignTasks', error)
    } finally {
      if (typeof successHook === 'function') successHook()
      clearLoading.value = false
    }
  }

  return {
    clearPopVisible,
    clearLoading,
    handleClear
  }
}
