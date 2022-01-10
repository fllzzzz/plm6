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
            <span class="color-green" v-empty-text v-to-fixed="{ val: row.singleReturnableLength, dp: curMatBaseUnit.length.precision }" />
            /
            <span v-empty-text v-to-fixed="{ val: row.length, dp: curMatBaseUnit.length.precision }" />
          </template>
        </el-table-column>
      </template>
      <!-- 仓库信息 -->
      <warehouse-info-columns :columns="columns" show-project />
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
          <clickable-permission-span
            v-if="row.outbound"
            :permission="permission.outboundDetail"
            @click="openOutboundDetailView(row.outbound.id)"
            :text="row.outbound.serialNumber"
          />
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
      >
        <template #default="{ row }">
          <span v-parse-time="'{y}-{m}-{d}'">{{ row.createTime }}</span>
        </template>
      </el-table-column>
      <el-table-column class="return-btn-column" v-if="props.isComponent" label="退库" align="center" width="100" sortable="custom">
        <template #default="{ row }">
          <el-badge :value="returnNumber[row.id]" :hidden="returnNumber[row.id] === 0" class="badge-item">
            <common-button :disabled="row.boolReviewPending" type="warning" size="mini" @click="handleAddReturn(row)">退库</common-button>
          </el-badge>
          <table-cell-tag v-if="row.boolReviewPending" name="退库中" color="#909399" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 调拨详情 -->
    <detail-wrapper ref="outboundDetailRef" :api="getOutboundDetail">
      <outbound-detail />
    </detail-wrapper>
    <!-- -->
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/material-return/raw-material/returnable-list'
import { detail as getOutboundDetail } from '@/api/wms/material-outbound/raw-material/review'

import { computed, defineEmits, defineProps, provide, reactive, ref, watchEffect } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { calcTheoryWeight } from '@/utils/wms/measurement-calc'
import { createUniqueString } from '@/utils/data-type/string'
import { specFormat } from '@/utils/wms/spec-format'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import Pagination from '@crud/Pagination'
import DetailWrapper from '@crud/detail-wrapper.vue'
import MHeader from './module/header'

import tableCellTag from '@comp-common/table-cell-tag/index.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import OutboundDetail from '@/views/wms/material-outbound/raw-material/review/module/detail.vue'
import ClickablePermissionSpan from '@/components-system/common/clickable-permission-span.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import MaterialUnitOperateQuantityColumns from '@/components-system/wms/table-columns/material-unit-operate-quantity-columns/index.vue'
import { ElMessage } from 'element-plus'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'

const emit = defineEmits(['add'])

const props = defineProps({
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

const permission = {
  // get: ['wms_partyABorrow:get'],
  // return: ['wms_partyABorrow:return'],
  outboundDetail: ['wms_outboundApplication_review:detail']
}

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

// 展开行
const expandRowKeys = ref([])
// 出库详情ref
const outboundDetailRef = ref()
const returnNumber = ref({})
// 表格ref
const tableRef = ref()
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

const { maxHeight } = useMaxHeight({ paginate: true })

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
  })
}

// 添加退库信息
function handleAddReturn(row) {
  const selectList = props.selectList
  const newData = reactive({
    uid: createUniqueString(), // 当前退库记录唯一id
    id: row.id, // 物料id
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
  const index = selectList.findLastIndex((v) => v.id === row.id)

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
  props.selectList.forEach((scRow) => {
    if (number[scRow.id]) {
      number[scRow.id]++
      mete[scRow.id] += scRow.mete || 0
    } else {
      number[scRow.id] = 1
      mete[scRow.id] = scRow.mete || 0
    }
  })
  crud.data.forEach((row) => {
    row.returnableMete = row.sourceReturnableMete - (mete[row.id] || 0)
  })
  returnNumber.value = number
}

// 打开出库详情窗口
function openOutboundDetailView(outboundId) {
  outboundDetailRef.value.toDetail(outboundId)
}
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