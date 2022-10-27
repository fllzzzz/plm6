// 钉钉
import { constantize } from '../base'

// 钉钉审批类型
const ddApproveTypeEnum = {
  REQUISITIONS_APPROVAL: { L: '申购审批', K: 'CONTRACT_REVIEW', V: 1 }
}
constantize(ddApproveTypeEnum)

// 钉钉审批流程状态
const ddApprovalProcessStatusEnum = {
  START: { L: '发起审批', K: 'START', V: 0, COLOR: '#1890ff' },
  NEW: { L: '未启动', K: 'NEW', V: 1, COLOR: '#1890ff' },
  RUNNING: { L: '处理中', K: 'RUNNING', V: 2, COLOR: '#1890ff' },
  PAUSED: { L: '暂停', K: 'PAUSED', V: 3, COLOR: '#909399' }, // 暂未使用
  COMPLETED: { L: '完成', K: 'COMPLETED', V: 4, COLOR: '#13ce66' }, // 处理完
  CANCELED: { L: '取消', K: 'CANCELED', V: 5, COLOR: '#FFBA00' }, // 或签一个同意 另一人取消
  TERMINATED: { L: '被终止', K: 'TERMINATED', V: 6, COLOR: '#ff4949' } // 撤销审批实例
}
constantize(ddApprovalProcessStatusEnum)

// 钉钉审核状态
const ddReviewStatusEnum = {
  UNREVIEWED: { L: '待审核', K: 'UNREVIEWED', V: 1 << 0, TAG: '' },
  AUDITING: { L: '审核中', K: 'AUDITING', V: 1 << 4, TAG: 'warning' },
  PASS: { L: '已通过', K: 'PASS', V: 1 << 1, TAG: 'success' },
  REFUSE: { L: '已拒绝', K: 'REFUSE', V: 1 << 2, TAG: 'danger' },
  CANCEL: { L: '已撤销', K: 'REFUSE', V: 1 << 3, TAG: 'danger' }
}
constantize(ddReviewStatusEnum)

// 抄送节点
const ddApprovalPositionEnum = {
  START: { L: '开始', K: 'START', V: 1 },
  FINISH: { L: '结束', K: 'FINISH', V: 2 },
  START_FINISH: { L: '开始和结束', K: 'START_FINISH', V: 3 }
}
constantize(ddApprovalPositionEnum)

// 审批类型
const ddTaskActionTypeEnum = {
  AND: { L: '会签', K: 'AND', V: 1 },
  OR: { L: '或签', K: 'OR', V: 2 },
  NONE: { L: '单人', K: 'NONE', V: 3 }
}
constantize(ddTaskActionTypeEnum)

// 审批用户类型
const ddUserTypeEnum = {
  DEPT: { L: '部门', K: 'DEPT', V: 1 },
  USER: { L: '用户', K: 'USER', V: 2 }
}
constantize(ddUserTypeEnum)

export {
  ddApproveTypeEnum, // 钉钉审批类型
  ddReviewStatusEnum, // 钉钉审核状态
  ddApprovalPositionEnum, // 抄送节点
  ddTaskActionTypeEnum, // 审批类型
  ddApprovalProcessStatusEnum, // 钉钉审批流程状态
  ddUserTypeEnum // 审批用户类型
}

export default {
  ddApproveTypeEnum, // 钉钉审批类型
  ddReviewStatusEnum, // 钉钉审核状态
  ddApprovalPositionEnum, // 抄送节点
  ddTaskActionTypeEnum, // 审批类型
  ddApprovalProcessStatusEnum, // 钉钉审批流程状态
  ddUserTypeEnum // 审批用户类型
}
