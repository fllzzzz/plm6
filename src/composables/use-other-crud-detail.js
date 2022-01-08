import { ref } from 'vue'

// 使用其他crud的detail
export default function useOtherCrudDetail() {
  const detailRef = ref()

  // 打开详情
  function openDetail(params) {
    if (detailRef.value) detailRef.value.toDetail(params)
  }
  return {
    detailRef,
    openDetail
  }
}
