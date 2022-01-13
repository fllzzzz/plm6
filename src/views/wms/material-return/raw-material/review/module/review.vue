<template>
  <common-drawer
    ref="drawerRef"
    v-model="drawerVisible"
    :content-loading="detailLoading"
    :before-close="handleClose"
    :title="drawerTitle"
    :show-close="true"
    size="100%"
    custom-class="raw-mat-return-application-review-form"
  >
    <template #titleAfter>
      <title-after-info :detail="form" />
    </template>
    <template #titleRight>
      <review-convenient-operate
        ref="reviewConvenientRef"
        v-if="pendingReviewIdList && pendingReviewIdList.length > 1"
        v-model="currentRecordId"
        v-model:consequent="reviewNext"
        :list="pendingReviewIdList"
        @change="handleConvenientChange"
        class="convenient-operation"
      />
      <!-- 审核按钮 -->
      <review-confirm-button
        :passed-loading="passedLoading"
        :returned-loading="returnedLoading"
        :passed-fn="passed"
        :returned-fn="returned"
      />
    </template>
    <template #content>
      <el-form class="form" :disabled="formDisabled">
        <material-info class="material-info" :basic-class="form.basicClass" :material="currentSource" />
        <common-table
          ref="tableRef"
          :data="form.list"
          :max-height="maxHeight"
          show-summary
          :summary-method="getSummaries"
          :expand-row-keys="expandRowKeys"
          row-key="id"
          highlight-current-row
          @row-click="handleRowClick"
        >
          <el-expand-table-column :data="form.list" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
            <template #default="{ row }">
              <expand-secondary-info :basic-class="row.basicClass" :row="row" :show-batch-no="false" show-remark show-graphics />
            </template>
          </el-expand-table-column>
          <!-- 基础信息 -->
          <material-base-info-columns :basic-class="form.basicClass" fixed="left" />
          <!-- 次要信息 -->
          <material-secondary-info-columns :basic-class="form.basicClass" />
          <!-- 单位及其数量 -->
          <material-unit-quantity-columns :basic-class="form.basicClass" />
          <!-- 仓库信息 -->
          <warehouse-info-columns show-project />
        </common-table>
        <el-input
          class="approval-comments"
          v-model="form.approvalComments"
          :rows="2"
          maxLength="1000"
          type="textarea"
          show-word-limit
          placeholder="审核意见"
          style="margin-top: 5px"
        />
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { getPendingReviewIdList, detail, reviewPassed, reviewReturned } from '@/api/wms/material-return/raw-material/review'
import { computed, ref, defineEmits, defineProps, watch } from 'vue'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

import { regExtra } from '@/composables/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useContinuousReview from '@/composables/use-continuous-review'
import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import ExpandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import MaterialInfo from '@/views/wms/material-return/raw-material/application/components/material-info/index.vue'

import reviewConvenientOperate from '@/components-system/common/review-convenient-operate.vue'
import ReviewConfirmButton from '@/components-system/common/review-confirm-button.vue'
import titleAfterInfo from '@/views/wms/material-return/raw-material/components/title-after-info.vue'

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

const drawerRef = ref() // 当前drawer
const detailLoading = ref(false) // 详情loading
const expandRowKeys = ref([]) // 展开

// 表单禁止操作
const formDisabled = computed(() => passedLoading.value || returnedLoading.value)

// 当前源数据
const currentSource = ref()
const form = ref()

const { crud } = regExtra()

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.raw-mat-return-application-review-form',
    extraBox: ['.el-drawer__header', '.approval-comments'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    minHeight: 300
  },
  () => computed(() => !detailLoading.value)
)

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, closeHook: closeHook })
const { baseUnit } = useMatBaseUnit() // 当前分类基础单位

// 连续审核
const {
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
  handleConvenientChange
} = useContinuousReview({
  detailMethod: fetchDetail,
  passedMethod: () => reviewPassed(form.value),
  returnedMethod: () => reviewReturned(form.value),
  pendingListMethod: () => getPendingReviewIdList(crud.query),
  detailInitCallBack: detailInitCallBack,
  closeDlg: handleClose
})

// 标题
const drawerTitle = computed(() => {
  if (form.value && form.value.serialNumber) {
    return `退库单：${form.value.serialNumber}`
  } else {
    return '退库单'
  }
})

// 监听数据变化
watch(
  () => props.data,
  async (data) => {
    detailInit()
    if (data && data.id) {
      currentRecordId.value = data.id
      detailLoading.value = true
      reviewInit()
      fetchDetail(data.id)
    }
  },
  { immediate: true, deep: true }
)

// 初始化
function detailInitCallBack() {
  detailLoading.value = false // 详情loading
  expandRowKeys.value = [] // 展开
}

// 行选中
function handleRowClick(row, column, event) {
  currentSource.value = row.source
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
  // 当前数据
  currentSource.value = undefined
  await setSpecInfoToList(form.list)
  await numFmtByBasicClass(form.list, { toNum: true })
  const sourceList = form.list.map((v) => v.source)
  await setSpecInfoToList(sourceList)
  await numFmtByBasicClass(
    sourceList,
    { toNum: true },
    {
      mete: ['mete', 'returnableMete', 'singleMete', 'singleReturnableMete']
    }
  )
  return form
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
  // 获取单位精度
  const dp =
    form.value.basicClass && baseUnit.value && baseUnit.value[form.value.basicClass]
      ? baseUnit.value[form.value.basicClass].measure.precision
      : 0
  return tableSummary(param, { props: [['quantity', dp], 'mete'] })
}
</script>

<style lang="scss" scoped>
.raw-mat-return-application-review-form {
  .material-info {
    margin-bottom: 10px;
  }
  .el-drawer__header .el-tag {
    min-width: 70px;
    text-align: center;
  }
  .el-table {
    ::v-deep(.cell) {
      height: 28px;
      line-height: 28px;
    }

    ::v-deep(.current-row > td.el-table__cell) {
      --el-table-current-row-background-color: #d7ffef;
    }
  }
}
</style>
