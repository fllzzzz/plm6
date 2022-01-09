import { reviewStatusEnum } from '@/utils/enum/modules/common'
import EO from '@/utils/enum/index'

// 物料状态
const materialStatusEnum = {
  UNSUBMITTED: { L: '未提交', V: -1, K: 'UNSUBMITTED', TAG: 'warning' },
  PENDING_REVIEW: { L: '待审核', V: reviewStatusEnum.UNREVIEWED.V, K: 'PENDING_REVIEW', TAG: 'info' },
  RETURNED: { L: '已退货', V: reviewStatusEnum.PASS.V, K: 'RETURNED', TAG: 'success' }
}
EO.constantize(materialStatusEnum)

export {
  materialStatusEnum
}
