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
          :data-format="columnsDataFormat"
          :max-height="maxHeight"
          show-summary
          :summary-method="getSummaries"
          default-expand-all
          :expand-row-keys="expandRowKeys"
          row-key="id"
          highlight-current-row
          @row-click="handleRowClick"
        >
          <el-expand-table-column :data="form.list" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
            <template #default="{ row }">
              <template v-if="form.basicClass===rawMatClsEnum.STEEL_PLATE.V && row.boolReturns">
                <common-table :class="currentRow.uid===row.uid?'child-table':''" :key="row.id" :data="row.list" style="margin:10px 0;" :stripe="false">
                  <material-base-info-columns :basic-class="row.basicClass" fixed="left" />
                  <!-- 次要信息 -->
                  <material-secondary-info-columns :basic-class="row.basicClass" />
                  <!-- 单位及其数量 -->
                <el-table-column prop="singleQuantity" align="center" width="110px" :label="`数量 (${baseUnit[form.basicClass].measure.unit})`" />
                <el-table-column key="singleReturnMete" prop="singleReturnMete" align="center" :label="`总重 (${baseUnit[form.basicClass].weight.unit})`" width="120px" />
                  <!-- <material-unit-quantity-columns :basic-class="row.basicClass" /> -->
                  <!-- 仓库信息 -->
                  <warehouse-info-columns />
                </common-table>
              </template>
              <expand-secondary-info :basic-class="row.basicClass" :row="row" :show-batch-no="false" show-remark show-graphics />
            </template>
          </el-expand-table-column>
          <!-- 基础信息 -->
          <material-base-info-columns :basic-class="form.basicClass" :showTip="form.basicClass===rawMatClsEnum.STEEL_PLATE.V" fixed="left" />
          <!-- 次要信息 -->
          <material-secondary-info-columns :basic-class="form.basicClass" />
          <!-- 单位及其数量 -->
          <material-unit-quantity-columns :showTip="form.basicClass===rawMatClsEnum.STEEL_PLATE.V" :basic-class="form.basicClass" />
          <!-- 仓库信息 -->
          <warehouse-info-columns show-project show-monomer show-area />
        </common-table>
        <el-input
          class="approval-comments"
          v-model="form.approvalComments"
          :rows="2"
          maxlength="1000"
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
import { materialColumns } from '@/utils/columns-format/wms'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { isNotBlank, toPrecision } from '@/utils/data-type'

import { regExtra } from '@/composables/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useContinuousReview from '@/composables/use-continuous-review'
import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from './material-unit-quantity-columns/index.vue'
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
const currentRow = ref({})
const form = ref()
// 表格列数据格式转换
const columnsDataFormat = ref([...materialColumns, ['remark', 'empty-text']])

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
  currentRow.value = row
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
  currentRow.value = {}
  const allArr = []
  const allArr1 = []
  if (form.basicClass === rawMatClsEnum.STEEL_PLATE.V) {
    form.list.forEach(async (v) => {
      if (v.boolReturns && isNotBlank(v.list)) {
        await setSpecInfoToList(v.list)
        const ps = await numFmtByBasicClass(v.list, { toNum: true },
          {
            mete: ['mete', 'returnableMete', 'singleMete', 'singleReturnableMete', 'singleReturnMete']
          }
        )
        // source 原出库信息转换
        const childSourceList = v.list.map((row) => row.source)
        await setSpecInfoToList(childSourceList)
        const ps1 = await numFmtByBasicClass(
          childSourceList,
          { toNum: true },
          {
            mete: ['mete', 'returnableMete', 'singleMete', 'singleReturnableMete', 'singleReturnMete']
          }
        )
        allArr.push(ps)
        allArr1.push(ps1)
      }
    })
  }
  await Promise.all(allArr1)
  await Promise.all(allArr)
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
  form.list.forEach(v => {
    v.actualMete = v.mete
  })
  if (form.basicClass === rawMatClsEnum.STEEL_PLATE.V) {
    form.list.forEach(async (v) => {
      v.uid = v.id
      if (v.boolReturns && isNotBlank(v.list)) {
        let detailMete = 0
        v.list.forEach(k => {
          k.pid = v.id
          k.uid = k.id
          if (k.singleReturnMete) {
            detailMete += k.singleReturnMete
          }
        })
        v.detailMete = toPrecision(detailMete, baseUnit.value[form.basicClass].weight.precision) * (v.quantity || 0)
        v.actualMete = v.detailMete
      }
    })
  }
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
      ? baseUnit.value[form.value.basicClass]?.measure.precision
      : 0
  return tableSummary(param, { props: [['quantity', dp], 'actualMete'] })
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
    ::v-deep(td .cell) {
      min-height: 28px;
      line-height: 28px;
    }
    ::v-deep(.current-row > td.el-table__cell) {
      --el-table-current-row-background-color: #d7ffef;
    }
  }
  .child-table{
  ::v-deep(th.el-table__cell.is-leaf) {
      background: #e5f9f1 !important;
    }
    ::v-deep(.el-table__empty-block){
      background: #e5f9f1 !important;
    }
    ::v-deep(td.el-table__cell){
      background: #e5f9f1 !important;
    }
  }
}
</style>
