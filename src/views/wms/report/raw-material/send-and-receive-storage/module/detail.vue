<template>
  <common-dialog
    ref="drawerRef"
    :visible="dialogVisible"
    :before-close="handleClose"
    :title="drawerTitle"
    show-close
    fullscreen
    custom-class="raw-mat-send-and-receive-storage-detail"
  >
    <div class="head-container query-header filter-container">
      <div class="filter-left-box">
        <common-radio-button
          v-model="filter.formType"
          :options="formTypeEnum"
          type="enum"
          size="small"
          class="filter-item"
          @change="fetchDetail"
        />
        <el-input v-model.trim="filter.specification" clearable size="small" placeholder="规格" class="filter-item" style="width: 250px" />
        <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="fetchDetail">搜索</common-button>
        <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery()">
          重置
        </common-button>
      </div>
      <div class="filter-right-box">
        <export-button
          v-permission="permission.get"
          :params="printParams"
          :fn="exportSendAndReceiveStorageDetailExcel"
          response-header-result
        >
          下载材料报表（根据查询条件）
        </export-button>
      </div>
    </div>
    <common-table
      ref="tableRef"
      v-loading="detailLoading"
      :data="filterList"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      show-summary
      :summary-method="getSummaries"
      :expand-row-keys="expandRowKeys"
      row-key="id"
      highlight-current-row
    >
      <el-table-column label="序号" type="index" align="center" width="55" fixed="left">
        <template #default="{ row, $index }">
          <!-- 是否调整单 -->
          <table-cell-tag
            :show="row.sourceRow?.receipt?.receiptType === receiptTypeEnum.SUPPLEMENT.V"
            :name="receiptTypeEnum.SUPPLEMENT.L"
            type="supplement"
          />
          <span>{{ $index + 1 }}</span>
        </template>
      </el-table-column>
      <!-- 基础信息 -->
      <material-base-info-columns
        :basic-class="materialInfo.basicClass"
        :show-index="false"
        spec-merge
        :show-outbound-mode="Boolean(filter.formType === formTypeEnum.INBOUND.V)"
        fixed="left"
        show-oddment-by-half-out
      />
      <!-- 次要信息 -->
      <material-secondary-info-columns :basic-class="materialInfo.basicClass" />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :basic-class="materialInfo.basicClass & rawMatClsEnum.STEEL_COIL.V ? '' : materialInfo.basicClass" />
      <!-- 价格信息 -->
      <template v-if="showAmount">
        <amount-info-columns />
      </template>
      <!-- 仓库信息 -->
      <warehouse-info-columns show-project />
      <el-table-column key="receipt.receiptType" prop="receipt.receiptType" show-overflow-tooltip label="类型" align="center" width="70" />
      <el-table-column key="receipt" prop="receipt" show-overflow-tooltip label="单据编号" align="center" min-width="120">
        <template #default="{ row: { sourceRow: row } }">
          <receipt-sn-clickable
            :receipt-types="['INBOUND', 'OUTBOUND', 'TRANSFER', 'RETURN', 'REJECTED', 'SUPPLEMENT']"
            :receipt="row?.receipt"
          />
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { getSendAndReceiveStorageDetail, exportSendAndReceiveStorageDetailExcel } from '@/api/wms/report/raw-material/statistics'
import { computed, inject, ref, defineEmits, defineProps } from 'vue'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { materialColumns } from '@/utils/columns-format/wms'
import { specFormat } from '@/utils/wms/spec-format'
import { isNotBlank } from '@/utils/data-type'
import { formTypeEnum } from '../enum'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

import { invoiceTypeEnum } from '@/utils/enum/modules/finance'
import { DP } from '@/settings/config'

import useMaxHeight from '@compos/use-max-height'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useVisible from '@/composables/use-visible'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import amountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
import ExportButton from '@comp-common/export-button/index.vue'
import ReceiptSnClickable from '@/components-system/wms/receipt-sn-clickable'
import { receiptTypeEnum } from '@/utils/enum/modules/wms'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const emit = defineEmits(['update:visible'])

const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  date: {
    type: [Number, String],
    required: true
  },
  materialInfo: {
    type: Object
  },
  showAmount: {
    type: Boolean
  }
})

// 表格列数据格式转换
const columnsDataFormat = computed(() => {
  return [
    ...materialColumns,
    // 金额相关
    ['invoiceType', ['parse-enum', invoiceTypeEnum, { f: 'SL' }]],
    ['taxRate', ['suffix', '%']],
    ['unitPrice', ['to-thousand', decimalPrecision.value.wms]],
    ['unitPriceExcludingVAT', ['to-thousand', decimalPrecision.value.wms]],
    ['amount', ['to-thousand', DP.YUAN]],
    ['amountExcludingVAT', ['to-thousand', DP.YUAN]],
    ['inputVAT', ['to-thousand', DP.YUAN]],
    ['receipt.receiptType', ['parse-enum', receiptTypeEnum]]
  ]
})
// const columnsDataFormat = ref([...materialHasAmountColumns, ['receipt.receiptType', ['parse-enum', receiptTypeEnum]]])

const permission = inject('permission')
const detailLoading = ref(false)
const drawerRef = ref()
const tableRef = ref()
const expandRowKeys = ref([])
// 详情
const materialInfo = ref()
const detail = ref({})
const list = ref([])
const filter = ref({
  formType: formTypeEnum.BEGIN_PERIOD.V,
  specification: undefined
})

const { baseUnit } = useMatBaseUnit() // 当前分类基础单位

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.raw-mat-send-and-receive-storage-detail',
    extraBox: ['.el-drawer__header', '.material-info'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    minHeight: 300,
    extraHeight: 10
  },
  detailLoading
)

// 因为报表变动可能比较频繁,每次显示重新加载详情
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchDetail })

// 标题
const drawerTitle = computed(() => {
  if (detail.value && detail.value.serialNumber) {
    return `收发存详情：${detail.value.serialNumber}`
  } else {
    return '收发存详情'
  }
})

// 过滤后的list
const filterList = computed(() => {
  if (list.value && filter.value.specification) {
    return list.value.filter((row) => isNotBlank(row.specMerge) && row.specMerge.indexOf(filter.value.specification) > -1)
  } else {
    return list.value
  }
})

// 查询参数
const printParams = computed(() => {
  return {
    id: props.materialInfo.id,
    ids: props.materialInfo.ids,
    statId: props.materialInfo.statId,
    statIds: props.materialInfo.statIds,
    date: props.date,
    formType: filter.value.formType
  }
})

// 初始化
function init() {
  detail.value = {}
  list.value = []
  materialInfo.value = props.materialInfo
  expandRowKeys.value.length = 0
}

// 重置查询参数
function resetQuery() {
  filter.value.specification = undefined
}

// 加载详情
async function fetchDetail() {
  init()
  // TODO:查询条件可能变更，目前使用id。若id不存在，不加载
  if (!props.materialInfo.id) return
  // 查询参数
  const params = {
    ...printParams.value
  }
  try {
    detailLoading.value = true
    const { content } = await getSendAndReceiveStorageDetail(params)
    list.value = await dataFormat(content)
  } catch (error) {
    console.error(error)
  } finally {
    detailLoading.value = false
  }
}

// 格式转换
async function dataFormat(data) {
  await setSpecInfoToList(data)
  await numFmtByBasicClass(data)
  data.forEach((row) => {
    row.specMerge = specFormat(row)
  })
  return data
}

// 合计
function getSummaries(param) {
  // 获取单位精度
  const dp =
    materialInfo.value.basicClass && baseUnit.value && baseUnit.value[materialInfo.value.basicClass]
      ? baseUnit.value[materialInfo.value.basicClass].measure.precision
      : 0
  return tableSummary(param, {
    props: [['quantity', dp], 'mete', ['amount', DP.YUAN], ['amountExcludingVAT', DP.YUAN], ['inputVAT', DP.YUAN]],
    toThousandFields: ['amount', 'amountExcludingVAT', 'inputVAT']
  })
}
</script>

<style lang="scss" scoped>
.raw-mat-send-and-receive-storage-detail {
  .query-header {
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
}
</style>
