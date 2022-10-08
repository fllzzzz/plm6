<template>
  <div class="app-container" :class="{ container: props.isComponent }">
    <!--工具栏-->
    <m-header />
    <!--表格渲染-->
    <common-table
      :key="`returnable_list_${basicClass}`"
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
      @sort-change="crud.handleSortChange"
    >
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="basicClass" fixed="left" />
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="basicClass" />
      <!-- 单位及其数量 -->
      <material-unit-operate-quantity-columns
        :columns="columns"
        :basic-class="basicClass"
        single-mete-mode
        label-prefix="可退库"
        mete-field="singleMete"
        operable-mete-field="singleReturnableMete"
        :show-operable-quantity="false"
      />
      <template v-if="basicClass === rawMatClsEnum.SECTION_STEEL.V && curMatBaseUnit">
        <el-table-column
          v-if="columns.visible('singleReturnableLength')"
          key="singleReturnableLength"
          :show-overflow-tooltip="true"
          prop="singleReturnableLength"
          :label="`可退库 / 长(${curMatBaseUnit.length.unit})`"
          align="right"
          width="150"
        >
          <template #default="{ row }">
            <span class="color-green">{{ toFixed(row.singleReturnableLength, curMatBaseUnit.length.precision) }}</span>
            /
            {{ toFixed(row.length, curMatBaseUnit.length.precision) }}
          </template>
        </el-table-column>
      </template>
      <!-- 仓库信息 -->
      <warehouse-info-columns :columns="columns" show-project show-monomer show-area />
      <el-table-column
        v-if="columns.visible('outboundSN')"
        key="outboundSN"
        :show-overflow-tooltip="true"
        prop="outboundSN"
        label="出库单号"
        align="center"
        min-width="120"
      >
        <template #default="{ row }">
          <receipt-sn-clickable :receipt-types="['OUTBOUND']" :receipt="row.outbound" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('recipientName')"
        key="recipientName"
        :show-overflow-tooltip="true"
        prop="recipientName"
        label="领用人"
        align="center"
        width="90"
      />
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="出库日期"
        align="center"
        width="100"
        sortable="custom"
      />
      <el-table-column class="return-btn-column" v-if="props.isComponent" label="退库" align="center" width="100" sortable="custom" fixed="right">
        <template #default="{ row: { sourceRow: row } }">
          <el-badge :value="returnNumber[row.id]" :hidden="returnNumber[row.id] === 0" class="badge-item">
            <!-- 编辑状态下， -->
            <common-button :disabled="row.showReviewPending" type="warning" size="mini" @click="handleAddReturn(row)"> 退库 </common-button>
          </el-badge>
          <table-cell-tag v-if="row.showReviewPending" name="退库中" color="#909399" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/material-return/raw-material/returnable-list'
import { rawMaterialReturnableListPM as permission } from '@/page-permission/wms'
import { computed, defineEmits, defineProps, defineExpose, provide, reactive, ref, watchEffect } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { calcTheoryWeight } from '@/utils/wms/measurement-calc'
import { createUniqueString } from '@/utils/data-type/string'
import { specFormat } from '@/utils/wms/spec-format'
import { toFixed } from '@/utils/data-type'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import Pagination from '@crud/Pagination'
import MHeader from './module/header'

import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import MaterialUnitOperateQuantityColumns from '@/components-system/wms/table-columns/material-unit-operate-quantity-columns/index.vue'
import ReceiptSnClickable from '@/components-system/wms/receipt-sn-clickable'
import { ElMessage } from 'element-plus'

const emit = defineEmits(['add'])

const props = defineProps({
  edit: {
    type: Boolean,
    default: false
  },
  sourceReturnIds: {
    type: Array,
    default: () => []
  },
  basicClass: {
    // 基础类型
    type: Number,
    default: rawMatClsEnum.STEEL_PLATE.V
  },
  selectList: {
    // 选中列表
    type: Array,
    default: () => []
  },
  isComponent: {
    // 是否组件
    type: Boolean,
    default: false
  }
})

provide('isComponent', props.isComponent)
provide('basicClass', props.basicClass)

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

// 展开行
const expandRowKeys = ref([])
const returnNumber = ref({})
// 表格ref
const tableRef = ref()
// 表格列格式化
const columnsDataFormat = [
  ['quantity', ['to-fixed-field', 'measurePrecision']],
  ['singleMete', ['to-fixed-field', 'accountingPrecision']],
  ['singleReturnableMete', ['to-fixed-field', 'accountingPrecision']],
  ['project', ['parse-project', { onlyShortName: true }]],
  ['createTime', ['parse-time', '{y}-{m}-{d}']]
]
const { CRUD, crud, columns } = useCRUD(
  {
    title: '可退库列表',
    sort: ['id.desc'],
    invisibleColumns: ['outboundSN'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight(
  props.isComponent
    ? {
      mainBox: '.returnable-list-drawer',
      extraBox: ['.el-drawer__header', '.head-container'],
      wrapperBox: ['.el-drawer__body'],
      paginate: true
    }
    : {
      paginate: true
    }
)

// 当前分类基础单位
const { baseUnit } = useMatBaseUnit()

const curMatBaseUnit = computed(() => {
  if (baseUnit.value) {
    return baseUnit.value[props.basicClass]
  } else {
    return {}
  }
})

const basicClass = computed(() => {
  if (crud.query) return crud.query.basicClass
  return null
})

// 实时计算归还信息
watchEffect(() => calcReturnInfo())

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(
    data.content,
    {
      toSmallest: false,
      toNum: false
    },
    {
      unitNetCalcMete: 'returnableMete',
      unitNetCalcQuantity: 'quantity',
      length: ['length', 'singleReturnableLength'],
      mete: ['mete', 'returnableMete', 'singleMete', 'singleReturnableMete']
    }
  )
  // 计算理论重量
  await calcTheoryWeight(data.content)
  data.content.forEach((row) => {
    row.sourceReturnableMete = row.returnableMete
    if (props.basicClass === rawMatClsEnum.SECTION_STEEL.V) {
      row.returnableLength = row.singleReturnableLength * row.quantity
      row.totalLength = row.length * row.quantity
      row.sourceReturnableLength = row.returnableLength
    }
    // 编辑模式，不是当前退库单的在退库中的物料 “显示退库中”
    row.showReviewPending = row.boolReviewPending && (!props.edit || (props.edit && !props.sourceReturnIds.includes(row.id)))
  })
}

// 添加退库信息
function handleAddReturn(row) {
  const selectList = props.selectList
  const newData = reactive({
    uid: createUniqueString(), // 当前退库记录唯一id
    // id: row.id, // 物料id
    source: row,
    basicClass: row.basicClass, // 基础类型
    measureUnit: row.measureUnit, // 计量单位
    accountingUnit: row.accountingUnit, // 核算单位
    accountingPrecision: row.accountingPrecision, // 核算单位小数精度
    outboundUnitType: row.outboundUnitType, // 出库单位类型
    measurePrecision: row.measurePrecision // 计量单位小数精度
  })
  if (selectList.length > 0) {
    newData.factoryId = -1 // 工厂 同上
    newData.warehouseId = -1 // 仓库 同上
  }
  // setBasicInfoForData(row, newData)
  const index = selectList.findLastIndex((v) => v.source.id === row.id)

  if (index > -1) {
    selectList.splice(index + 1, 0, newData)
  } else {
    selectList.push(newData)
  }
  const specInfo = specFormat(row)
  const message = `${row.classifyFullName}${specInfo ? ' - ' + specInfo : ''} 加入退库列表`
  ElMessage.success(message)
  emit('add', newData)
}

// 计算退库信息
function calcReturnInfo() {
  const number = {}
  const mete = {}
  // 遍历退库列表，计算相同物料的可退库量及可退库数
  for (const scRow of props.selectList) {
    const source = scRow.source
    const sourceId = source ? source.id : undefined
    if (!sourceId) continue
    if (number[sourceId]) {
      number[sourceId]++
      mete[sourceId] += scRow.mete || 0
    } else {
      number[sourceId] = 1
      mete[sourceId] = scRow.mete || 0
    }
  }
  // 遍历当前列表，设置可退库量
  crud.data.forEach((row) => {
    row.returnableMete = row.sourceReturnableMete - (mete[row.id] || 0)
  })
  returnNumber.value = number
}

// 刷新
function refresh() {
  crud.refresh()
}

defineExpose({
  refresh
})
</script>

<style lang="scss" scoped>
.container {
  padding: 0;
}

::v-deep(.el-table__row td) {
  padding: 0;
  .cell {
    padding-top: 10px;
    padding-bottom: 10px;
  }
}
</style>
