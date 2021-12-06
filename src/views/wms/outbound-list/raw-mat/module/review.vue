<template>
  <common-drawer
    ref="drawerRef"
    v-model="drawerVisible"
    :content-loading="detailLoading"
    :before-close="handleClose"
    :title="drawerTitle"
    :show-close="true"
    size="100%"
    custom-class="raw-mat-outbound-list-review-form"
  >
    <template #titleAfter>
      <el-tag effect="plain">{{ `出库申请时间：${detail.userUpdateTime}` }}</el-tag>
      <el-tag v-if="detail.user" type="warning" effect="plain">{{ `领用人：${detail.user.name} | d${detail.user.deptName}` }}</el-tag>
    </template>
    <template #titleRight>
      <review-convenient-operate
        ref="reviewConvenientRef"
        v-if="pendingReviewIdList && pendingReviewIdList.length > 1"
        v-model="currentInboundId"
        v-model:consequent="reviewNext"
        :list="pendingReviewIdList"
        @change="handleConvenientChange"
        class="convenient-operation"
      />
      <el-popconfirm
        confirm-button-text="确定"
        cancel-button-text="取消"
        icon="el-icon-info"
        icon-color="green"
        title="确认通过？"
        @confirm="passed"
      >
        <template #reference>
          <common-button :loading="passedLoading" type="primary" size="mini" icon="el-icon-s-promotion"> 确认出库 </common-button>
        </template>
      </el-popconfirm>
      <el-popconfirm
        confirm-button-text="确定"
        cancel-button-text="取消"
        icon="el-icon-info"
        icon-color="red"
        title="确认删除？"
        @confirm="returned"
      >
        <template #reference>
          <common-button :loading="returnedLoading" size="mini" icon="el-icon-document-delete" type="danger"> 删 除 </common-button>
        </template>
      </el-popconfirm>
    </template>
    <template #content>
      <el-form class="form" :disabled="formDisabled">
        <common-table
          :data="form.list"
          :max-height="maxHeight"
          show-summary
          :cell-class-name="wrongCellMask"
          :summary-method="getSummaries"
          :expand-row-keys="expandRowKeys"
          row-key="id"
        >
          <!-- <el-expand-table-column :data="form.list" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
            <template #default="{ row }">
              <expand-secondary-info v-if="showAmount" :basic-class="form.basicClass" :row="row" />
            </template>
          </el-expand-table-column> -->
          <el-table-column label="序号" type="index" align="center" width="50" fixed="left" />
          <!-- 基础信息 -->
          <material-base-info-columns :basic-class="form.basicClass" :show-factory="showWarehouse" />
          <!-- 单位及其数量 -->
          <material-unit-quantity-columns :basic-class="form.basicClass" />
          <!-- 次要信息 -->
          <material-secondary-info-columns v-if="!showAmount" :basic-class="form.basicClass" show-project />
          <!-- 仓库设置 -->
          <warehouse-set-columns :form="form" />
          <warehouse-info-columns />
        </common-table>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { getPendingReviewIdList, detail, reviewPassed, reviewReturned } from '@/api/wms/outbound/raw-mat-outbound-list'
import { computed, ref, defineEmits, defineProps, watch } from 'vue'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import reviewConvenientOperate from '@/components-system/common/review-convenient-operate.vue'

import warehouseSetColumns from '@/views/wms/inbound-components/warehouse-set-columns.vue'

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

const reviewConvenientRef = ref() // 连续审核
const drawerRef = ref() // 当前drawer

const returnedLoading = ref(false) // 退回loading
const detailLoading = ref(false) // 通过loading
const passedLoading = ref(false) // 提交loading

const expandRowKeys = ref([]) // 展开
const amount = ref() // 金额变化
const form = ref({})
const operateRecordNumber = ref(0) // 操作记录条数
const reviewNext = ref(false) // 当前记录审核完成后，直接审核下一条
const pendingReviewIdList = ref([]) // 待审核列表
// const currentReviewIndex = ref(0) // 当前审核下标
const currentInboundId = ref() // 当前id

// 表单禁止操作
const formDisabled = computed(() => passedLoading.value || returnedLoading.value)

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.raw-mat-outbound-list-review-form',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    minHeight: 300,
    extraHeight: 10
  },
  () => computed(() => !detailLoading.value)
)

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, closeHook: closeHook })

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
  returnedLoading.value = false // 退回loading
  passedLoading.value = false // 通过loading
  expandRowKeys.value = [] // 展开
  amount.value = undefined // 金额变化
  form.value = {} // 表单重置
}

// 处理连续审核
function handleConvenientChange(id) {
  init()
  fetchDetail(id)
}

// 获取待审核入库单id列表
async function fetchPendingReviewIdList() {
  pendingReviewIdList.value = await getPendingReviewIdList()
}

// 加载详情
async function fetchDetail(id) {
  try {
    detailLoading.value = true
    const data = await detail(id)
    data.logistics = {}
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

// 通过提交
async function passed() {
  try {
    passedLoading.value = true
    // await reviewPassed(nForm)
    handleAfterSubmit()
  } catch (error) {
    console.log('通过提交', error)
  } finally {
    passedLoading.value = false
  }
}

// 退回
async function returned() {
  try {
    returnedLoading.value = true
    const data = {
      id: form.value.id,
      approvalComments: form.value.approvalComments
    }
    await reviewReturned(data)
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

// 关闭钩子
function closeHook() {
  // 审核数量>0 则刷新
  if (operateRecordNumber.value) {
    emit('refresh')
  }
}

// 合计
function getSummaries(param) {
  return tableSummary(param, { props: ['number', 'mete'] })
}
</script>

<style lang="scss" scoped>
.raw-mat-inbound-application-review-form {
  .el-drawer__header .el-tag {
    min-width: 120px;
    text-align: center;
  }

  .convenient-operation {
    margin-right: 10px;
  }
  .el-table {
    ::v-deep(.cell) {
      min-height: 28px;
      line-height: 28px;
    }
  }
}
</style>
