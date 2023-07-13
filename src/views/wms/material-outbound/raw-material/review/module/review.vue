<template>
  <common-drawer
    ref="drawerRef"
    v-model="drawerVisible"
    :content-loading="detailLoading"
    :before-close="handleClose"
    :title="drawerTitle"
    :show-close="true"
    size="100%"
    custom-class="raw-mat-application-review-form"
  >
    <template #titleAfter>
      <el-tag v-if="form.applicant" type="success" effect="dark">
        {{ `申请人：${form.applicant.name} | ${form.applicant.deptName}` }}
      </el-tag>
      <el-tag effect="plain">{{ `出库申请时间：${parseTime(form.createTime)}` }}</el-tag>
    </template>
    <template #titleRight>
      <template v-if="form.reviewStatus === reviewStatusEnum.UNREVIEWED.V">
        <span class="batch-set-info child-mr-7">
          <el-date-picker v-model="batchOutboundTime" type="datetime" value-format="x" placeholder="批量设置出库时间" size="mini" />
          <common-button type="success" size="mini" @click="setOutboundTime">设置</common-button>
        </span>
      </template>
      <review-convenient-operate
        ref="reviewConvenientRef"
        v-if="pendingReviewIdList && pendingReviewIdList.length > 1"
        v-model="currentInboundId"
        v-model:consequent="reviewNext"
        :list="pendingReviewIdList"
        @change="handleConvenientChange"
        class="convenient-operation"
      />
      <template v-if="form.reviewStatus === reviewStatusEnum.UNREVIEWED.V">
        <!-- 审核按钮 -->
        <review-confirm-button
          :passed-loading="submitOptLoading"
          :returned-loading="submitOptLoading"
          :passed-fn="passed"
          :returned-fn="returned"
        />
      </template>
      <template v-if="form.reviewStatus === reviewStatusEnum.PASS.V">
        <print-table api-key="wmsRmOutboundReceipt" :params="form.id" size="mini" type="warning" class="filter-item" @success="nextRecord" />
      </template>
    </template>
    <template #content>
      <el-form class="form" :disabled="formDisabled">
        <common-table
          :data="form.list"
          :data-format="columnsDataFormat"
          :max-height="maxHeight"
          show-summary
          :summary-method="getSummaries"
          :expand-row-keys="expandRowKeys"
          row-key="id"
        >
          <el-expand-table-column :data="form.list" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
            <template #default="{ row }">
              <expand-secondary-info :basic-class="row.basicClass" :row="row" show-remark show-graphics>
                <p v-if="row.boolTransfer">
                  调拨：
                  <span>（来源）</span>
                  <span style="color: brown">{{ row.sourceProject }}</span>
                  <span> ▶ </span>
                  <span>（目的）</span>
                  <span style="color: #3a8ee6">{{ row.project }}</span>
                </p>
              </expand-secondary-info>
            </template>
          </el-expand-table-column>
          <!-- 基础信息 -->
          <material-base-info-columns :basic-class="form.basicClass" show-outbound-mode fixed="left" />
          <!-- 次要信息 -->
          <material-secondary-info-columns />
          <!-- 单位及其数量 -->
          <material-unit-quantity-columns />
          <!-- 仓库信息 -->
          <warehouse-info-columns show-project show-monomer show-area show-transfer showOutboundWorkshop />
          <el-table-column label="领用人" width="100px" align="center">
            <template #default="{ row }">
              <el-tooltip v-if="row.recipient" placement="top" effect="light" :content="`${row.recipient.deptName}`">
                <span>{{ row.recipient.name }}</span>
              </el-tooltip>
            </template>
          </el-table-column>
          <el-table-column label="出库时间" width="195px" align="center">
            <template #default="{ row }">
              <el-date-picker
                v-if="form.reviewStatus === reviewStatusEnum.UNREVIEWED.V"
                v-model="row.sourceRow.outboundTime"
                type="datetime"
                value-format="x"
                placeholder="出库时间"
              />
              <span v-else>{{ row.outboundTime }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="checkPermission(permission.edit) && form.reviewStatus === reviewStatusEnum.UNREVIEWED.V"
            label="操作"
            width="70px"
            align="left"
          >
            <template #default="{ row, $index }">
              <el-popconfirm
                confirm-button-text="确定"
                cancel-button-text="取消"
                icon="el-icon-info"
                icon-color="red"
                :title="`确认删除当前${row.classifyFullName}吗?`"
                @confirm="delItem(row, $index)"
              >
                <template #reference>
                  <common-button :loading="submitOptLoading" type="danger" icon="el-icon-delete" size="mini" />
                </template>
              </el-popconfirm>
            </template>
          </el-table-column>
        </common-table>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { getPendingReviewIdList, detail, reviewPassed, reviewReturned, delMaterial } from '@/api/wms/material-outbound/raw-material/review'
import { inject, computed, ref, defineEmits, defineProps, watch } from 'vue'
import { reviewStatusEnum } from '@/utils/enum/modules/common'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { parseTime } from '@/utils/date'
import { materialColumns } from '@/utils/columns-format/wms'
import checkPermission from '@/utils/system/check-permission'

import { regExtra } from '@/composables/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import expandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import reviewConvenientOperate from '@/components-system/common/review-convenient-operate.vue'
import ReviewConfirmButton from '@/components-system/common/review-confirm-button.vue'

const emit = defineEmits(['refresh', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
  },
  data: {
    type: Object,
    default: () => {
      return {}
    }
  }
})

const permission = inject('permission')
const reviewConvenientRef = ref() // 连续审核
const drawerRef = ref() // 当前drawer

const submitOptLoading = ref(false) // 操作loading
const detailLoading = ref(false) // 通过loading

const expandRowKeys = ref([]) // 展开
const form = ref({})
const operateRecordNumber = ref(0) // 操作记录条数
const reviewNext = ref(false) // 当前记录审核完成后，直接审核下一条
const pendingReviewIdList = ref([]) // 待审核列表
const currentInboundId = ref() // 当前id
const batchOutboundTime = ref() // 批量设置时间
// 表格列格式化
const columnsDataFormat = ref([
  ...materialColumns,
  ['remark', 'empty-text'],
  ['project', 'parse-project'],
  ['sourceProject', 'parse-project'],
  ['outboundTime', ['parse-time', '{y}-{m}-{d} {h}:{i}:{s}']]
])

// 表单禁止操作
const formDisabled = computed(() => submitOptLoading.value)
// 标题
const drawerTitle = computed(() => {
  if (form.value && form.value.applicationSN) {
    return `出库申请单：${form.value.applicationSN}`
  } else {
    return '出库申请单'
  }
})

const { crud } = regExtra()

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.raw-mat-application-review-form',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    minHeight: 300
  },
  () => computed(() => !detailLoading.value)
)

const { visible: drawerVisible, handleClose } = useVisible({
  emit,
  props,
  closeHook: closeHook
})

// 监听数据变化
watch(
  () => props.data,
  async (data) => {
    init()
    if (data && data.id) {
      currentInboundId.value = data.id
      detailLoading.value = true
      await fetchPendingReviewIdList()
      fetchDetail(data.id)
    }
  },
  { immediate: true, deep: true }
)

// 初始化
function init() {
  detailLoading.value = false // 详情loading
  submitOptLoading.value = false // 退回loading
  submitOptLoading.value = false // 通过loading
  expandRowKeys.value = [] // 展开
  form.value = {} // 表单重置
}

// 处理连续审核
function handleConvenientChange(id) {
  init()
  fetchDetail(id)
}

// 获取待审核入库单id列表
async function fetchPendingReviewIdList() {
  pendingReviewIdList.value = await getPendingReviewIdList(crud.query)
}

// 加载详情
async function fetchDetail(id) {
  if (!id) return
  try {
    detailLoading.value = true
    const data = await detail(id)
    form.value = await detailFormat(data)
  } catch (error) {
    console.log('加载失败', error)
  } finally {
    detailLoading.value = false
  }
}

// 详情格式转换
async function detailFormat(form) {
  await setSpecInfoToList(form.list)
  form.list = await numFmtByBasicClass(form.list, {
    toSmallest: false,
    toNum: false
  })
  return form
}

// 删除出库单中的物料，删除也需要更新页面，避免材料类型发生变化，或者，当前出库单已不存在物料被自动删除
async function delItem(row, index) {
  try {
    submitOptLoading.value = true
    await delMaterial({ listId: form.value.id, materialId: row.id })
    form.value.list.splice(index, 1)
    // 如果list被清空，等同于当前出库单审核完成
    if (form.value.list.length === 0) {
      nextRecord()
    } else {
      ++operateRecordNumber.value
    }
  } catch (error) {
    console.log('删除', error)
  } finally {
    submitOptLoading.value = false
  }
}

// 通过提交
async function passed() {
  try {
    submitOptLoading.value = true
    await reviewPassed(form.value)
    // 审核通过后设置审核状态
    form.value.reviewStatus = reviewStatusEnum.PASS.V
    ++operateRecordNumber.value
  } catch (error) {
    console.log('通过提交', error)
  } finally {
    submitOptLoading.value = false
  }
}

// 退回
async function returned() {
  try {
    submitOptLoading.value = true
    await reviewReturned(form.value.id)
    ++operateRecordNumber.value
    nextRecord()
  } catch (error) {
    console.log('退回', error)
  } finally {
    submitOptLoading.value = false
  }
}

// 提交后处理
function nextRecord() {
  try {
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

// 关闭钩子
function closeHook() {
  // 审核数量>0 则刷新
  if (operateRecordNumber.value) {
    emit('refresh')
  }
}

// 设置出库时间
function setOutboundTime() {
  form.value.list.forEach((v) => {
    v.outboundTime = batchOutboundTime.value
  })
}

// 合计
function getSummaries(param) {
  return tableSummary(param, { props: ['quantity', 'mete'] })
}
</script>

<style lang="scss" scoped>
.raw-mat-application-review-form {
  .el-drawer__header .el-tag {
    min-width: 120px;
    text-align: center;
  }

  .convenient-operation {
    margin-right: 10px;
  }
  .el-table {
    ::v-deep(td .cell) {
      min-height: 28px;
      line-height: 28px;
    }

    ::v-deep(.el-input--suffix .el-input__inner) {
      padding-right: 10px;
    }
  }
  .batch-set-info {
    display: inline-flex;
    align-items: center;
    padding-right: 10px;
    margin-right: 10px;
    border-right: 1px solid#dcdfe6;
  }
}
</style>
