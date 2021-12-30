import { ref } from 'vue'

// 连续审核
export default function useContinuousReview({ detailMethod, passedMethod, returnedMethod, pendingListMethod, detailInitCallBack, closeDlg }) {
  const reviewConvenientRef = ref() // 连续审核组件ref
  const passedLoading = ref(false) // 提交loading
  const returnedLoading = ref(false) // 退回loading
  const operateRecordNumber = ref(0) // 操作记录条数
  const reviewNext = ref(false) // 当前记录审核完成后，直接审核下一条
  const pendingReviewIdList = ref([]) // 待审核列表
  const currentRecordId = ref() // 当前id

  // 处理连续审核
  function handleConvenientChange(params) {
    detailInit()
    detailMethod(params)
  }

  // 详情初始化
  function detailInit() {
    returnedLoading.value = false // 退回loading
    passedLoading.value = false // 通过loading
    if (typeof initCallBack === 'function') detailInitCallBack()
  }

  // 获取待审核入库单id列表
  async function reviewInit() {
    pendingReviewIdList.value = await pendingListMethod()
  }

  // 通过
  async function passed() {
    try {
      passedLoading.value = true
      await passedMethod()
      handleAfterSubmit()
    } catch (error) {
      console.log('通过提交', error)
    } finally {
      passedLoading.value = false
    }
  }

  // 拒绝
  async function returned() {
    try {
      returnedLoading.value = true
      await returnedMethod()
      handleAfterSubmit()
    } catch (error) {
      console.log('退回', error)
    } finally {
      returnedLoading.value = false
    }
  }

  // 提交后处理
  function handleAfterSubmit() {
    try {
      ++operateRecordNumber.value
      // 继续审核
      if (reviewNext.value && reviewConvenientRef.value) {
        reviewConvenientRef.value.removeCurrent()
      } else {
        handleClose()
      }
    } catch (error) {
      console.log('审核提交后', error)
      handleClose()
    }
  }

  // 关闭
  function handleClose() {
    if (typeof closeDlg === 'function') {
      closeDlg()
    }
  }

  return {
    reviewNext,
    currentRecordId,
    pendingReviewIdList,
    operateRecordNumber,
    returnedLoading,
    passedLoading,
    reviewConvenientRef,
    reviewInit,
    detailInit,
    passed,
    returned,
    handleAfterSubmit,
    handleConvenientChange
  }
}
