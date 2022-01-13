import { reactive, ref } from 'vue'
import { mapGetters } from '@/store/lib'

export default function usePagination({ fetchHook }) {
  const { tablePageSize } = mapGetters('tablePageSize')

  const queryPage = reactive({
    pageNumber: 1,
    pageSize: tablePageSize
  })
  const total = ref(0)

  function handleSizeChange(val) {
    queryPage.pageNumber = 1
    queryPage.pageSize = val
    if (typeof fetchHook === 'function') {
      fetchHook()
    }
  }
  function handleCurrentChange(val) {
    queryPage.pageNumber = val
    if (typeof fetchHook === 'function') {
      fetchHook()
    }
  }

  function setTotalPage(val) {
    total.value = val
  }

  return {
    handleSizeChange,
    handleCurrentChange,
    total,
    setTotalPage,
    queryPage
  }
}
